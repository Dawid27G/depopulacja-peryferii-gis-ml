# =============================================================================
#  Determinanty depopulacji i peryferyjności powiatów w Polsce (2011-2024)
#  Wersja R: czyszczenie danych -> klasyfikacja peryferyjności (rok bazowy)
#  -> regresja łączna i FE/RE na opóźnionych predyktorach -> XGBoost
#  (prognostyczny, determinantów, GW-XGBoost) -> diagnostyka przestrzenna
#  -> prognoza do 2030 -> scenariusze interwencji -> mapy GIS
# -----------------------------------------------------------------------------
#  Wejście:  dataset.xlsx, dataset_demografia_depopulacja.xlsx, dml_panel_ml.csv,
#            centroidy_powiatow.csv (kod_powiatu, lon, lat),
#            powiaty.geojson + powiaty_mapowanie_kodow.csv (granice powiatów)
#  Wyjście:  outputs/ (CSV, PNG, mapa HTML, wyniki_modeli.txt)
#
#  install.packages(c("readxl","dplyr","tidyr","ggplot2","fixest","plm",
#                     "xgboost","leaflet","htmlwidgets","sf"))
# =============================================================================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(fixest)
  library(plm)
  library(xgboost)
})

# Ścieżki względne: skrypt uruchamiamy z folderu projektu
# (Rscript analiza_peryferyjnosc.R albo setwd() na folder repozytorium).
UPLOADS <- "."
OUT     <- file.path(UPLOADS, "outputs")
KEY     <- c("kod_powiatu", "rok")
NTHREAD <- max(1L, parallel::detectCores() - 1L)

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# 1. WCZYTANIE I CZYSZCZENIE
# ---------------------------------------------------------------------------
# dataset.xlsx zawiera sztuczne duplikaty (błędne scalenie many-to-many):
# pary (powiat, rok) powtarzają się 16 lub 32 razy. Zamiast keep='first'
# agregujemy medianą w obrębie pary klucza (odporna na pojedyncze błędne rekordy).
wczytaj_i_oczysc <- function() {
  eko  <- read_excel(file.path(UPLOADS, "dataset.xlsx")) %>%
    mutate(kod_powiatu = trimws(as.character(kod_powiatu)))
  demo <- read_excel(file.path(UPLOADS, "dataset_demografia_depopulacja.xlsx")) %>%
    mutate(kod_powiatu = trimws(as.character(kod_powiatu)))

  czyste <- eko %>%
    group_by(across(all_of(KEY))) %>%
    summarise(
      across(where(is.numeric) & !any_of(c("powiat", "miasto_na_prawach")),
             ~ {
               x <- .x[is.finite(.x)]
               if (length(x) == 0) NA_real_ else median(x)
             }),
      powiat            = first(powiat),
      miasto_na_prawach = first(miasto_na_prawach),
      .groups = "drop"
    )

  demo <- demo %>% select(-any_of(c("powiat", "miasto_na_prawach")))
  panel <- left_join(czyste, demo, by = KEY)
  panel %>%
    mutate(
      across(where(is.numeric), ~ replace(.x, !is.finite(.x), NA_real_)),
      ludnosc_tys = if_else(ludnosc_tys > 0, ludnosc_tys, NA_real_),
      kanalizacja_pct = if_else(
        !is.na(kanalizacja_pct) &
          kanalizacja_pct >= 0 &
          kanalizacja_pct <= 100,
        kanalizacja_pct, NA_real_),
      gestosc_zaludnienia = if_else(
        gestosc_zaludnienia >= 0, gestosc_zaludnienia, NA_real_),
      lekarze_10tys = if_else(lekarze_10tys >= 0, lekarze_10tys, NA_real_),
      bezrobocie_rejestrowane = if_else(
        bezrobocie_rejestrowane >= 0, bezrobocie_rejestrowane, NA_real_)
    )
}

raport_jakosci_danych <- function(panel) {
  numeryczne <- names(panel)[vapply(panel, is.numeric, logical(1))]
  podsumowanie <- bind_rows(lapply(numeryczne, function(nm) {
    x <- panel[[nm]]
    med <- median(x, na.rm = TRUE)
    mad_x <- mad(x, constant = 1, na.rm = TRUE)
    out <- is.finite(x) & is.finite(mad_x) & mad_x > 0 &
      abs(x - med) > 6 * mad_x
    data.frame(
      zmienna = nm,
      n = sum(!is.na(x)),
      brakujace = sum(is.na(x)),
      min = suppressWarnings(min(x, na.rm = TRUE)),
      mediana = med,
      max = suppressWarnings(max(x, na.rm = TRUE)),
      skrajne_mad_6 = sum(out),
      stringsAsFactors = FALSE
    )
  }))
  write.csv(podsumowanie, file.path(OUT, "kontrola_jakosci_zmiennych.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  podsumowanie
}

wczytaj_panel_ml <- function() {
  plik <- file.path(UPLOADS, "dml_panel_ml.csv")
  if (!file.exists(plik)) {
    stop("Brak pliku dml_panel_ml.csv z dodatkowymi zmiennymi demograficznymi.")
  }
  read.csv(plik, stringsAsFactors = FALSE, check.names = FALSE) %>%
    transmute(
      kod_powiatu = trimws(as.character(kod_powiatu)),
      rok = as.integer(rok),
      przyrost_naturalny_1000 = as.numeric(przyrost_naturalny_1000),
      saldo_migracji_1000 = as.numeric(saldo_migracji_1000)
    ) %>%
    distinct(kod_powiatu, rok, .keep_all = TRUE)
}

wzbogac_panel_ml <- function(panel) {
  potrzebne <- names(PREDYKTORY_DEMOGRAFICZNE)
  dodatkowe <- wczytaj_panel_ml()

  for (nm in potrzebne) {
    kandydaci <- c(nm, paste0(nm, ".x"), paste0(nm, ".y"))
    znalezione <- kandydaci[kandydaci %in% names(panel)]
    if (length(znalezione) == 0) next
    if (nm != znalezione[1]) {
      panel[[nm]] <- panel[[znalezione[1]]]
    }
  }

  brakujace <- setdiff(potrzebne, names(panel))
  if (length(brakujace) > 0) {
    panel <- panel %>%
      select(-any_of(c(paste0(brakujace, ".x"), paste0(brakujace, ".y")))) %>%
      left_join(dodatkowe, by = KEY)
  }

  brakujace <- setdiff(potrzebne, names(panel))
  if (length(brakujace) > 0) {
    stop("Brak zmiennych demograficznych w panelu: ",
         paste(brakujace, collapse = ", "))
  }
  panel %>% select(-any_of(c(paste0(potrzebne, ".x"),
                              paste0(potrzebne, ".y"))))
}


# ---------------------------------------------------------------------------
# 2. ZMIENNE POCHODNE
# ---------------------------------------------------------------------------
# Próg rozbieżności bilansu ludności [‰]: 99% obserwacji ma |różnicę| < 3,7‰.
PROG_ARTEFAKTU <- 5

dodaj_zmienne <- function(df) {
  regon <- "podmioty_gospodarki_narodowej_wpisane_do_rejestru_regon_razem"
  df <- df %>%
    mutate(
      podmioty_1000 = .data[[regon]] / ludnosc_tys,
      wage_rel      = .data[["wynagrodzenie_brutto_relacja_do_sredniej_krajowej.x"]],
      miasto        = as.integer(as.logical(miasto_na_prawach))
    )

  # Zmienna zależna: roczna zmiana ludności na 1000 mieszk.
  # Artefakty zmian granic wykrywamy z BILANSU LUDNOŚCI: zmiana ludności
  # powinna być równa przyrostowi naturalnemu + saldu migracji. Gdy różnica
  # przekracza PROG_ARTEFAKTU, ludność zmieniła się przez przesunięcie granic,
  # a nie przez zjawiska demograficzne; wtedy zmianę ludności zastępujemy sumą
  # składników. Wcześniejsza winsoryzacja 1/99 percentyla (a) przycinała
  # prawdziwe skrajne spadki najbardziej wyludniających się powiatów,
  # (b) pomijała część artefaktów, (c) liczyła granice z całego okresu, czyli
  # także z lat testowych. Reguła bilansowa działa na pojedynczej obserwacji,
  # więc nie korzysta z informacji z innych lat.
  y <- "zmiana_ludnosci_1000"
  sk <- df$przyrost_naturalny_1000 + df$saldo_migracji_1000
  artefakt <- !is.na(sk) & !is.na(df[[y]]) & abs(df[[y]] - sk) > PROG_ARTEFAKTU
  df %>% mutate(
    dep_surowe    = .data[[y]],
    zmiana_granic = artefakt,
    dep           = ifelse(artefakt, sk, .data[[y]])
  )
}


# ---------------------------------------------------------------------------
# 3. KLASYFIKACJA PERYFERYJNOŚCI (rdzeń / półperyferie / peryferie)
# ---------------------------------------------------------------------------
# Indeks rdzeniowości = średnia ze standaryzowanych: log gęstości zaludnienia,
# relacji płac, gęstości firm i dochodów własnych pc; tercyle -> 3 klasy.
# Liczony na OKRESIE BAZOWYM (średnia 2011-2013), a nie na ostatnim roku:
# klasa z 2024 r. odzwierciedlałaby już skutki depopulacji, którą potem
# modelujemy i prognozujemy (selekcja próby na podstawie wyniku).
LATA_BAZOWE <- 2011:2013

sklasyfikuj_peryferyjnosc <- function(df, lata = LATA_BAZOWE) {
  z    <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  comp <- c("gestosc_zaludnienia", "wage_rel", "podmioty_1000", "doch_wlasne_pc")

  df %>%
    filter(rok %in% lata) %>%
    group_by(kod_powiatu) %>%
    summarise(across(all_of(comp), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(across(all_of(comp), ~ replace(.x, !is.finite(.x), NA_real_)),
           gestosc_zaludnienia = log(gestosc_zaludnienia)) %>%
    mutate(
      indeks_rdzeniowosci = rowMeans(across(all_of(comp), z), na.rm = TRUE),
      klasa = factor(ntile(indeks_rdzeniowosci, 3),
                     labels = c("peryferyjne", "polperyferyjne", "rdzeniowe"))
    ) %>%
    select(kod_powiatu, klasa, indeks_rdzeniowosci)
}


# ---------------------------------------------------------------------------
# 4. MODELE (pooled OLS, FE, Hausman)
# ---------------------------------------------------------------------------
PREDYKTORY <- c(
  doch_wlasne_pc          = "dochody własne pc",
  udzial_wyd_inwest       = "udział wyd. inwest.",
  bezrobocie_rejestrowane = "bezrobocie rejestr.",
  wage_rel                = "płace vs kraj",
  podmioty_1000           = "firmy / 1000 mieszk.",
  kanalizacja_pct         = "kanalizacja %",
  lekarze_10tys           = "lekarze / 10 tys.",
  udzial_65_plus          = "udział 65+",
  gestosc_zaludnienia     = "gęstość zaludn."
)

PREDYKTORY_DEMOGRAFICZNE <- c(
  przyrost_naturalny_1000 = "przyrost naturalny / 1000",
  saldo_migracji_1000    = "saldo migracji / 1000"
)

# Przyrost naturalny i saldo migracji są składnikami zmiany ludności.
# Nie używamy ich jako predyktorów, aby model nie wyjaśniał wyniku jego
# własnymi składnikami. Pozostają dostępne do opisowej dekompozycji.
PREDYKTORY_PANEL <- PREDYKTORY
PREDYKTORY_STRUKTURALNE <- PREDYKTORY

# Predyktory wchodzą do regresji z OPÓŹNIENIEM t-1. Wersja jednoczesna była
# obciążona mechanicznie: gęstość zaludnienia = ludność / powierzchnia, a firmy,
# dochody i lekarze są liczone per capita, więc zmiana ludności w roku t
# zmienia je w tym samym roku "z definicji" (stąd beta gęstości +6 w FE).
# Gęstość wchodzi w logarytmie (silna prawoskośność).
przygotuj_macierz <- function(df) {
  d <- df %>%
    arrange(kod_powiatu, rok) %>%
    group_by(kod_powiatu) %>%
    mutate(gestosc_zaludnienia = log(gestosc_zaludnienia),
           across(all_of(names(PREDYKTORY_PANEL)), ~ dplyr::lag(.x, 1))) %>%
    ungroup() %>%
    select(all_of(c(names(PREDYKTORY_PANEL), "dep", "kod_powiatu", "rok", "miasto"))) %>%
    drop_na()
  # standaryzacja predyktorów -> współczynniki porównywalne
  for (nm in names(PREDYKTORY_PANEL)) d[[paste0(nm, "_z")]] <- as.numeric(scale(d[[nm]]))
  list(d = d, zpreds = paste0(names(PREDYKTORY_PANEL), "_z"))
}

# Pooled OLS z efektami stałymi lat i SE klastrowanymi po powiecie
regresja_laczna <- function(d, zpreds) {
  # "miasto" jest stałe w próbie peryferyjnej (same powiaty ziemskie) - wtedy
  # nie wchodzi do modelu zamiast dawać współczynnik NA.
  rhs <- if (n_distinct(d$miasto) > 1) c(zpreds, "miasto") else zpreds
  f <- as.formula(paste("dep ~", paste(rhs, collapse = " + "), "| rok"))
  feols(f, data = d, cluster = ~kod_powiatu)
}

modele_panelowe <- function(d, zpreds) {
  rhs <- paste(zpreds, collapse = " + ")

  # FE dwukierunkowe (powiat + rok), SE klastrowane po powiecie
  fe <- feols(as.formula(paste("dep ~", rhs, "| kod_powiatu + rok")),
              data = d, cluster = ~kod_powiatu)

  # Hausman FE vs RE (plm): identyczna specyfikacja w obu modelach,
  # efekty czasu jako dummies, efekt powiatu: within vs random.
  pd   <- pdata.frame(as.data.frame(d), index = c("kod_powiatu", "rok"))
  f_pl <- as.formula(paste("dep ~", rhs, "+ factor(rok)"))
  fe_p <- plm(f_pl, data = pd, model = "within")
  re_p <- plm(f_pl, data = pd, model = "random")
  list(fe = fe, haus = phtest(fe_p, re_p))
}


# ---------------------------------------------------------------------------
# 5. WYKRES TRENDU
# ---------------------------------------------------------------------------
wykres_trendu <- function(df) {
  tr <- df %>%
    group_by(rok) %>%
    summarise(udzial  = mean(zmiana_ludnosci_1000 < 0, na.rm = TRUE) * 100,
              mediana = median(zmiana_ludnosci_1000, na.rm = TRUE),
              .groups = "drop")

  # druga oś: liniowe przeskalowanie mediany na zakres 0-100
  lo <- min(tr$mediana); rng <- diff(range(tr$mediana))
  to100   <- function(m) (m - lo) / rng * 100
  from100 <- function(p) p / 100 * rng + lo

  p <- ggplot(tr, aes(rok)) +
    geom_col(aes(y = udzial), fill = "#cc4444", alpha = .35) +
    geom_line(aes(y = to100(mediana)), colour = "#222255", linewidth = 1) +
    geom_point(aes(y = to100(mediana)), colour = "#222255") +
    geom_hline(yintercept = to100(0), linetype = "dashed", colour = "grey50") +
    scale_y_continuous(
      "% powiatów ze spadkiem ludności", limits = c(0, 100),
      sec.axis = sec_axis(~ from100(.), name = "mediana zmiany ludności /1000")
    ) +
    labs(x = NULL, title = "Postępująca depopulacja powiatów peryferyjnych, 2011–2024") +
    theme_minimal() +
    theme(axis.title.y.left  = element_text(colour = "#cc4444"),
          axis.title.y.right = element_text(colour = "#222255"))
  ggsave(file.path(OUT, "rys_trend_depopulacji.png"), p,
         width = 9, height = 4.5, dpi = 130)
}


# ---------------------------------------------------------------------------
# 6. XGBOOST WAŻONY GEOGRAFICZNIE (GW-XGBoost)
# ---------------------------------------------------------------------------
# Idea: dla każdego powiatu i budujemy OSOBNY model XGBoost na wszystkich
# powiato-latach, ale każda obserwacja dostaje wagę z jądra przestrzennego
# zależną od odległości jej powiatu od powiatu i (adaptacyjne pasmo = k
# najbliższych powiatów, jądro bisquare). Powiaty dalsze niż pasmo mają wagę 0.
# Z lokalnego modelu liczymy SHAP dla obserwacji powiatu i -> lokalne ważności
# i kierunki wpływu cech (analogia do GWR, ale nieliniowo).
# Szerokość pasma k wybieramy przez leave-location-out CV (powiat testowy
# jest wykluczony z treningu).
# Cechy GW-XGBoost: predyktory opóźnione o rok (te same nazwy kolumn, wartości
# z t-1), trend roczny i statyczna odległość od miasta wojewódzkiego (GIS).
# "miasto" pominięte - w próbie peryferyjnej jest stałe.
CECHY_GW <- c(names(PREDYKTORY), "odl_stolica_km", "rok")

XGB_PARAMS <- list(
  objective = "reg:squarederror", eta = 0.05, max_depth = 3,
  subsample = 0.8, colsample_bytree = 0.8, min_child_weight = 1,
  nthread = NTHREAD, seed = 1
)
XGB_NROUNDS <- 200
XGB_MODEL_CONFIG <- list(
  params = modifyList(XGB_PARAMS, list(
    max_depth = 2, eta = 0.03, subsample = 0.85,
    colsample_bytree = 0.85, objective = "reg:squarederror"
  )),
  nrounds = 250
)
# Model determinantów (bez opóźnionej zmiennej zależnej) ma osobne strojenie
# (random search w main). Bez bezwładności zmiany ludności optymalne drzewa są
# głębsze niż w modelu prognostycznym. NULL = konfiguracja prognostyczna.
XGB_DET_CONFIG <- NULL
cfg_det <- function() if (is.null(XGB_DET_CONFIG)) XGB_MODEL_CONFIG else XGB_DET_CONFIG
# Progi z ALE trafiają do modelu ADDYTYWNEGO (hybryda), więc wyznaczamy je
# z addytywnego XGBoost (drzewa o głębokości 1, czyli wzmacniany GAM):
# najlepsza konfiguracja z max_depth = 1 z tego samego random searcha.
# Krzywe ALE z głębszych drzew mieszają efekt cechy z jej interakcjami;
# po strojeniu (głębokość 4) progi były wyraźnie mniej stabilne między oknami.
XGB_DET_ADD_CONFIG <- NULL
cfg_det_add <- function() {
  if (!is.null(XGB_DET_ADD_CONFIG)) return(XGB_DET_ADD_CONFIG)
  list(params = modifyList(cfg_det()$params, list(max_depth = 1)), nrounds = cfg_det()$nrounds)
}

wczytaj_wspolrzedne <- function() {
  plik <- file.path(UPLOADS, "centroidy_powiatow.csv")
  if (!file.exists(plik)) return(NULL)
  w <- read.csv(plik, stringsAsFactors = FALSE)
  # prosta projekcja równoprostokątna (km) - wystarczająca dla obszaru Polski
  lat0 <- mean(w$lat) * pi / 180
  data.frame(kod = as.character(w$kod_powiatu),
             lon = w$lon,
             lat = w$lat,
             x   = w$lon * cos(lat0) * 111.32,
             y   = w$lat * 110.57)
}

# Miasta wojewódzkie (TERYT powiatu, 4 cyfry) - siedziby wojewodów i sejmików.
STOLICE_WOJ <- c("0264", "0461", "0463", "0663", "0861", "0862", "1061",
                 "1261", "1465", "1661", "1863", "2061", "2261", "2469", "2661",
                 "2862", "3064", "3262")

# Odległość (km, po prostej) centroidu powiatu od centroidu najbliższego miasta
# wojewódzkiego. Prosta miara dostępności - nie uwzględnia sieci drogowej.
odleglosc_od_stolic <- function(coords) {
  mapowanie <- read.csv(file.path(UPLOADS, "powiaty_mapowanie_kodow.csv"),
                        colClasses = "character", fileEncoding = "UTF-8")
  mapowanie$JPT_KOD_JE <- sprintf("%04d", as.integer(mapowanie$JPT_KOD_JE))
  kody_stolic <- mapowanie$kod_powiatu[mapowanie$JPT_KOD_JE %in% STOLICE_WOJ]
  stolice <- coords[coords$kod %in% kody_stolic, ]
  if (nrow(stolice) < 15) {
    stop("Nie znaleziono centroidów miast wojewódzkich (", nrow(stolice), ").")
  }
  D <- outer(coords$x, stolice$x, "-")^2 + outer(coords$y, stolice$y, "-")^2
  data.frame(kod_powiatu = coords$kod,
             odl_stolica_km = sqrt(apply(D, 1, min)))
}

przygotuj_gw <- function(panel, coords) {
  d <- panel %>%
    arrange(kod_powiatu, rok) %>%
    group_by(kod_powiatu) %>%
    mutate(across(all_of(names(PREDYKTORY)), ~ dplyr::lag(.x, 1))) %>%
    ungroup() %>%
    select(all_of(c("kod_powiatu", CECHY_GW, "dep"))) %>%
    drop_na() %>%
    mutate(kod = as.character(kod_powiatu)) %>%
    inner_join(coords, by = "kod")

  cent <- distinct(d, kod, x, y)
  D    <- as.matrix(dist(cent[, c("x", "y")]))
  d$pidx <- match(d$kod, cent$kod)
  list(d = d, cent = cent, D = D)
}

# wagi powiatów wokół powiatu i: bisquare, adaptacyjne pasmo (k sąsiadów)
wagi_jadra <- function(odl, k) {
  if (is.infinite(k)) return(rep(1, length(odl)))        # model globalny
  h <- sort(odl)[min(k + 1, length(odl))]
  ifelse(odl < h, (1 - (odl / h)^2)^2, 0)
}

dopasuj_lokalnie <- function(G, i, k, wylacz_focal = FALSE) {
  w_pow <- wagi_jadra(G$D[i, ], k)
  if (wylacz_focal) w_pow[i] <- 0
  w   <- w_pow[G$d$pidx]
  keep <- w > 0
  dtr <- xgb.DMatrix(as.matrix(G$d[keep, CECHY_GW]),
                     label = G$d$dep[keep], weight = w[keep])
  xgb.train(params = XGB_PARAMS, data = dtr, nrounds = XGB_NROUNDS, verbose = 0)
}

# RMSE leave-location-out: model dla powiatu i uczony BEZ jego obserwacji
cv_przestrzenne <- function(G, k, n_focal = 40, seed = 1) {
  set.seed(seed)                                   # te same powiaty dla każdego k
  idx <- sample(nrow(G$cent), min(n_focal, nrow(G$cent)))
  bl <- lapply(idx, function(i) {
    m    <- dopasuj_lokalnie(G, i, k, wylacz_focal = TRUE)
    test <- G$d$pidx == i
    data.frame(y = G$d$dep[test],
               p = as.numeric(predict(m, as.matrix(G$d[test, CECHY_GW]))))
  })
  bl <- bind_rows(bl)
  sqrt(mean((bl$y - bl$p)^2))
}

wybierz_pasmo <- function(G, siatka = c(30, 60, 100, 150, Inf)) {
  wyn <- data.frame(k = siatka,
                    rmse_cv = vapply(siatka, function(k) cv_przestrzenne(G, k),
                                     numeric(1)))
  write.csv(wyn, file.path(OUT, "gwxgb_cv_pasmo.csv"), row.names = FALSE)
  wyn
}

# Cechy stałe w czasie dostępne w panelu (obecnie: odległość od stolicy woj.).
cechy_statyczne <- function(d) intersect("odl_stolica_km", names(d))

# Panelowy XGBoost prognozujący przyszły rok. Wszystkie zmienne ekonomiczne
# są opóźnione, aby uniknąć używania informacji z prognozowanego roku.
przygotuj_dane_xgb_panel <- function(panel, coords = NULL) {
  panel <- wzbogac_panel_ml(panel)

  d <- panel %>%
    arrange(kod_powiatu, rok) %>%
    group_by(kod_powiatu) %>%
    mutate(
      dep_lag1 = dplyr::lag(dep, 1),
      dep_lag2 = dplyr::lag(dep, 2),
      across(all_of(names(PREDYKTORY_PANEL)),
             list(lag1 = ~dplyr::lag(.x, 1), lag2 = ~dplyr::lag(.x, 2)))
    ) %>%
    ungroup()

  if (!is.null(coords)) {
    d <- d %>% left_join(coords %>% select(kod, x, y),
                          by = c("kod_powiatu" = "kod"))
  } else {
    d$x <- 0
    d$y <- 0
  }

  cechy <- c(
    "rok", "x", "y", cechy_statyczne(d), "dep_lag1", "dep_lag2",
    as.vector(outer(names(PREDYKTORY_PANEL), c("_lag1", "_lag2"), paste0))
  )
  d <- d %>%
    select(kod_powiatu, rok, dep, klasa,
           all_of(names(PREDYKTORY_PANEL)), all_of(cechy)) %>%
    filter(if_all(all_of(c("dep", cechy)), ~ !is.na(.x)))
  list(d = d, cechy = cechy)
}

# Specyfikacja determinantów nie zawiera opóźnionej zmiennej zależnej.
# Dzięki temu jej ważność nie jest miarą samej bezwładności depopulacji.
przygotuj_dane_xgb_determinanty <- function(panel, coords = NULL) {
  panel <- wzbogac_panel_ml(panel)
  d <- panel %>%
    arrange(kod_powiatu, rok) %>%
    group_by(kod_powiatu) %>%
    mutate(across(all_of(names(PREDYKTORY_PANEL)),
                  list(lag1 = ~ dplyr::lag(.x, 1),
                       lag2 = ~ dplyr::lag(.x, 2)))) %>%
    ungroup()

  if (!is.null(coords)) {
    d <- d %>% left_join(coords %>% select(kod, x, y),
                          by = c("kod_powiatu" = "kod"))
  } else {
    d$x <- 0
    d$y <- 0
  }

  cechy <- c(
    "rok", "x", "y", cechy_statyczne(d),
    as.vector(outer(names(PREDYKTORY_PANEL), c("_lag1", "_lag2"), paste0))
  )
  d <- d %>%
    select(kod_powiatu, rok, dep, klasa, all_of(cechy)) %>%
    filter(if_all(all_of(c("dep", cechy)), ~ !is.na(.x)))
  list(d = d, cechy = cechy)
}

rmse <- function(y, p) sqrt(mean((y - p)^2))

mae <- function(y, p) mean(abs(y - p))

auc_rank <- function(y, score) {
  y <- as.integer(y)
  ok <- is.finite(score) & !is.na(y)
  y <- y[ok]
  score <- score[ok]
  n1 <- sum(y == 1L)
  n0 <- sum(y == 0L)
  if (n1 == 0L || n0 == 0L) return(NA_real_)
  r <- rank(score, ties.method = "average")
  (sum(r[y == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

metryki_klasyfikacji_depopulacji <- function(y, p, prog = 0) {
  y_bin <- as.integer(y < 0)
  pred_bin <- as.integer(p < prog)
  tp <- sum(y_bin == 1 & pred_bin == 1)
  tn <- sum(y_bin == 0 & pred_bin == 0)
  fp <- sum(y_bin == 0 & pred_bin == 1)
  fn <- sum(y_bin == 1 & pred_bin == 0)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  f1 <- if (is.finite(precision) && is.finite(recall) &&
            precision + recall > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
  data.frame(
    auc = auc_rank(y_bin, -p),
    accuracy = mean(y_bin == pred_bin),
    # przy ~95% obserwacji ze spadkiem accuracy jest mało informatywna
    balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE),
    sensitivity = recall,
    specificity = specificity,
    precision = precision,
    f1 = f1,
    prevalence = mean(y_bin),
    stringsAsFactors = FALSE
  )
}

predykcja_xgb <- function(train, test, cechy) {
  dtr <- xgb.DMatrix(as.matrix(train[, cechy]), label = train$dep)
  model <- xgb.train(
    params = XGB_MODEL_CONFIG$params,
    data = dtr,
    nrounds = XGB_MODEL_CONFIG$nrounds,
    verbose = 0
  )
  # xgboost 3.x zwraca wektor ALTREP, którego dplyr nie umie grupować
  as.numeric(predict(model, as.matrix(test[, cechy])))
}

# Porządny random search XGBoost. Walidacja krocząca na latach 2017-2018
# (przed pierwszym rokiem testowym 2019; dane zaczynają się w 2015 r.).
# Dla każdej konfiguracji liczba drzew jest wybierana z krzywej błędu
# walidacyjnego (średnia z lat walidacji), do XGB_RS_MAX_ROUNDS; trening
# kończy się wcześniej, gdy błąd walidacyjny nie spada przez 150 drzew.
# Zakres colsample_bytree od 0,1: przy dolnej granicy 0,3 najlepsze
# konfiguracje leżały na krawędzi przestrzeni.
XGB_RS_N <- 200
XGB_RS_MAX_ROUNDS <- 2000
XGB_RS_EARLY_STOP <- 150
xgb_random_search <- function(d, cechy, lata = LATA_STROJENIA, n = XGB_RS_N, seed = 2026,
                              plik = NULL) {
  set.seed(seed)
  lu <- function(a, b) exp(runif(n, log(a), log(b)))
  siatka <- data.frame(
    max_depth = sample(1:6, n, replace = TRUE),
    eta = signif(lu(0.005, 0.1), 3),
    min_child_weight = signif(lu(1, 50), 3),
    subsample = round(runif(n, 0.5, 1), 2),
    colsample_bytree = round(runif(n, 0.1, 1), 2),
    lambda = signif(lu(0.01, 50), 3),
    alpha = ifelse(runif(n) < 0.5, 0, signif(lu(0.01, 10), 3)),
    gamma = ifelse(runif(n) < 0.5, 0, round(runif(n, 0, 2), 2))
  )
  hp <- names(siatka)
  dm <- lapply(lata, function(r) {
    tr <- d[d$rok < r, ]; te <- d[d$rok == r, ]
    list(tr = xgb.DMatrix(as.matrix(tr[, cechy]), label = tr$dep),
         te = xgb.DMatrix(as.matrix(te[, cechy]), label = te$dep))
  })
  wyn <- t(vapply(seq_len(n), function(i) {
    if (i %% 20 == 0) message("random search XGBoost: ", i, "/", n, "  ", format(Sys.time(), "%H:%M:%S"))
    params <- modifyList(XGB_PARAMS, as.list(siatka[i, hp]))
    krzywe <- vapply(dm, function(z) {
      m <- xgb.train(params = params, data = z$tr, nrounds = XGB_RS_MAX_ROUNDS,
                     evals = list(valid = z$te), early_stopping_rounds = XGB_RS_EARLY_STOP,
                     verbose = 0)
      lg <- attributes(m)$evaluation_log; if (is.null(lg)) lg <- m$evaluation_log
      r <- as.numeric(lg$valid_rmse)
      c(r, rep(NA_real_, XGB_RS_MAX_ROUNDS - length(r)))
    }, numeric(XGB_RS_MAX_ROUNDS))
    # średnia z lat tylko tam, gdzie wszystkie lata mają krzywą (przed zatrzymaniem)
    sr <- rowMeans(krzywe)
    c(nrounds = which.min(sr), rmse_walidacja = min(sr, na.rm = TRUE))
  }, numeric(2)))
  siatka <- cbind(siatka, wyn)
  siatka <- siatka[order(siatka$rmse_walidacja), ]
  if (!is.null(plik)) write.csv(siatka, file.path(OUT, plik), row.names = FALSE, fileEncoding = "UTF-8")
  best <- siatka[1, ]
  list(params = modifyList(XGB_PARAMS, as.list(best[hp])), nrounds = as.integer(best$nrounds),
       siatka = siatka)
}

strojenie_xgb_panel <- function(panel, coords = NULL) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  cechy <- P$cechy
  # Okno strojenia kończy się PRZED pierwszym rokiem walidacji kroczącej
  # (2019), żeby wybór hiperparametrów nie widział lat testowych.
  train <- d %>% filter(rok <= 2016)
  valid <- d %>% filter(rok %in% 2017:2018)
  if (nrow(train) == 0 || nrow(valid) == 0) {
    stop("Brak danych do strojenia XGBoost.")
  }
  kandydaci <- list(
    bazowy = list(max_depth = 2, eta = 0.03,
                  min_child_weight = 1, subsample = 0.85,
                  colsample_bytree = 0.85, nrounds = 250),
    plytszy = list(max_depth = 1, eta = 0.03,
                   min_child_weight = 1, subsample = 0.9,
                   colsample_bytree = 0.9, nrounds = 300),
    regularized = list(max_depth = 2, eta = 0.02,
                       min_child_weight = 5, subsample = 0.9,
                       colsample_bytree = 0.9, nrounds = 350),
    wolniejszy = list(max_depth = 3, eta = 0.02,
                      min_child_weight = 5, subsample = 0.8,
                      colsample_bytree = 0.8, nrounds = 350)
  )
  wyniki <- lapply(names(kandydaci), function(nazwa) {
    cfg <- kandydaci[[nazwa]]
    model <- xgb.train(
      params = modifyList(XGB_PARAMS, cfg[setdiff(names(cfg), "nrounds")]),
      data = xgb.DMatrix(as.matrix(train[, cechy]), label = train$dep),
      nrounds = cfg$nrounds,
      verbose = 0
    )
    pred <- as.numeric(predict(model, as.matrix(valid[, cechy])))
    data.frame(
      wariant = nazwa,
      rmse_walidacja = rmse(valid$dep, pred),
      mae_walidacja = mae(valid$dep, pred),
      max_depth = cfg$max_depth,
      eta = cfg$eta,
      min_child_weight = cfg$min_child_weight,
      subsample = cfg$subsample,
      colsample_bytree = cfg$colsample_bytree,
      nrounds = cfg$nrounds
    )
  }) %>% bind_rows() %>% arrange(rmse_walidacja)
  wybrany <- kandydaci[[wyniki$wariant[1]]]
  list(
    wyniki = wyniki,
    config = list(
      params = modifyList(XGB_PARAMS,
                          wybrany[setdiff(names(wybrany), "nrounds")]),
      nrounds = wybrany$nrounds
    )
  )
}

walidacja_antyprzeuczeniowa_xgb <- function(panel, coords = NULL,
                                             lata_testowe = 2019:2024) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  cechy <- P$cechy
  wyniki <- list()

  for (rok_test in lata_testowe) {
    train <- d %>% filter(rok < rok_test)
    test <- d %>% filter(rok == rok_test)
    if (nrow(train) == 0 || nrow(test) == 0) next

    pred_train <- as.numeric(predykcja_xgb(train, train, cechy))
    pred_test <- as.numeric(predykcja_xgb(train, test, cechy))
    klasy <- metryki_klasyfikacji_depopulacji(test$dep, pred_test)
    wyniki[[length(wyniki) + 1L]] <- data.frame(
      rok_test = rok_test,
      n_train = nrow(train),
      n_test = nrow(test),
      rmse_train = rmse(train$dep, pred_train),
      rmse_test = rmse(test$dep, pred_test),
      mae_train = mae(train$dep, pred_train),
      mae_test = mae(test$dep, pred_test),
      bias_test = mean(pred_test - test$dep),
      luka_rmse = rmse(test$dep, pred_test) -
        rmse(train$dep, pred_train),
      klasy,
      stringsAsFactors = FALSE
    )
  }

  foldy <- bind_rows(wyniki)
  if (nrow(foldy) == 0) {
    stop("Brak foldów do walidacji antyprzeuczeniowej XGBoost.")
  }
  podsumowanie <- foldy %>%
    summarise(
      foldy = n(),
      rmse_train_srednie = mean(rmse_train, na.rm = TRUE),
      rmse_test_srednie = mean(rmse_test, na.rm = TRUE),
      mae_train_srednie = mean(mae_train, na.rm = TRUE),
      mae_test_srednie = mean(mae_test, na.rm = TRUE),
      luka_rmse_srednia = mean(luka_rmse, na.rm = TRUE),
      bias_test_sredni = mean(bias_test, na.rm = TRUE),
      auc_srednie = mean(auc, na.rm = TRUE),
      accuracy_srednia = mean(accuracy, na.rm = TRUE),
      balanced_accuracy_srednia = mean(balanced_accuracy, na.rm = TRUE),
      prevalence_srednia = mean(prevalence, na.rm = TRUE),
      sensitivity_srednia = mean(sensitivity, na.rm = TRUE),
      specificity_srednia = mean(specificity, na.rm = TRUE),
      f1_srednie = mean(f1, na.rm = TRUE),
      .groups = "drop"
    )
  list(foldy = foldy, podsumowanie = podsumowanie)
}

predykcja_fe <- function(train, test, cechy) {
  # cechy stałe w czasie są pochłaniane przez efekt stały powiatu
  cechy_fe <- setdiff(cechy, c("x", "y", "odl_stolica_km"))
  formula_fe <- as.formula(paste(
    "dep ~", paste(cechy_fe, collapse = " + "), "| kod_powiatu"
  ))
  model <- fixest::feols(formula_fe, data = train)
  as.numeric(predict(model, newdata = test))
}

CELE_PROGNOZ <- c("dep", names(PREDYKTORY_PANEL))

trenuj_xgb_cel <- function(d, cel, cechy) {
  wymagane <- c(cel, cechy)
  dane <- d %>%
    filter(if_all(all_of(wymagane), ~ is.finite(.x)))
  if (nrow(dane) < 100) {
    stop("Za malo kompletnych obserwacji dla celu: ", cel,
         " (n = ", nrow(dane), ").")
  }
  xgb.train(
    params = XGB_MODEL_CONFIG$params,
    data = xgb.DMatrix(as.matrix(dane[, cechy]), label = dane[[cel]]),
    nrounds = XGB_MODEL_CONFIG$nrounds,
    verbose = 0
  )
}

ocena_xgb_wszystkich_cech <- function(panel, coords = NULL,
                                      lata_testowe = 2019:2024) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  cechy <- P$cechy
  wyniki <- list()

  for (rok_test in lata_testowe) {
    train <- d %>% filter(rok < rok_test)
    test <- d %>% filter(rok == rok_test)
    if (nrow(train) == 0 || nrow(test) == 0) next

    for (cel in CELE_PROGNOZ) {
      dane_test <- test %>%
        filter(is.finite(.data[[cel]]),
               if_all(all_of(cechy), ~ is.finite(.x)))
      if (nrow(dane_test) == 0) next
      model <- trenuj_xgb_cel(train, cel, cechy)
      pred <- as.numeric(predict(model, as.matrix(dane_test[, cechy])))
      wyniki[[length(wyniki) + 1L]] <- data.frame(
        rok = rok_test,
        cel = cel,
        n_test = nrow(dane_test),
        rmse = rmse(dane_test[[cel]], pred),
        mae = mean(abs(dane_test[[cel]] - pred)),
        bias = mean(pred - dane_test[[cel]])
      )
    }
  }

  surowe <- bind_rows(wyniki)
  podsumowanie <- surowe %>%
    group_by(cel) %>%
    summarise(
      rmse_srednie = mean(rmse, na.rm = TRUE),
      mae_srednie = mean(mae, na.rm = TRUE),
      bias_sredni = mean(bias, na.rm = TRUE),
      lata_testowe = n(),
      .groups = "drop"
    ) %>%
    arrange(rmse_srednie)
  attr(podsumowanie, "wyniki_roczne") <- surowe
  podsumowanie
}

prognoza_cech_do_2030 <- function(panel, coords = NULL, rok_koncowy = 2030) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  cechy <- P$cechy
  rok_ostatni <- max(panel$rok, na.rm = TRUE)
  if (rok_koncowy <= rok_ostatni) {
    stop("Rok koncowy prognozy musi byc pozniejszy niz ostatni rok danych.")
  }

  modele <- setNames(lapply(CELE_PROGNOZ, function(cel)
    trenuj_xgb_cel(d, cel, cechy)), CELE_PROGNOZ)
  stany <- panel %>%
    filter(rok == rok_ostatni) %>%
    select(kod_powiatu, klasa, all_of(CELE_PROGNOZ),
           all_of(cechy_statyczne(panel))) %>%
    filter(if_all(all_of(CELE_PROGNOZ), ~ is.finite(.x)))
  if (nrow(stany) == 0) {
    stop("Brak kompletnych obserwacji dla ostatniego roku prognozy.")
  }

  poprzednie <- panel %>%
    filter(rok == rok_ostatni - 1,
           kod_powiatu %in% stany$kod_powiatu) %>%
    select(kod_powiatu, all_of(CELE_PROGNOZ))
  poprzednie <- stany %>%
    select(kod_powiatu) %>%
    left_join(poprzednie, by = "kod_powiatu")
  poprzednie[CELE_PROGNOZ] <- lapply(poprzednie[CELE_PROGNOZ],
                                     function(x) ifelse(is.finite(x), x, NA_real_))
  poprzednie <- poprzednie %>%
    mutate(across(all_of(CELE_PROGNOZ),
                  ~ ifelse(is.na(.x), stany[[cur_column()]], .x)))

  if (!is.null(coords)) {
    stany <- stany %>%
      left_join(coords %>% select(kod, x, y),
                by = c("kod_powiatu" = "kod"))
    bez_wspolrzednych <- !is.finite(stany$x) | !is.finite(stany$y)
    if (any(bez_wspolrzednych)) {
      message("Pomijam ", sum(bez_wspolrzednych),
              " powiatow bez kompletnych wspolrzednych w prognozie cech.")
      stany <- stany[!bez_wspolrzednych, , drop = FALSE]
    }
  } else {
    stany$x <- 0
    stany$y <- 0
  }
  if (nrow(stany) == 0) {
    stop("Brak powiatow z kompletnymi danymi i wspolrzednymi do prognozy.")
  }

  wyniki <- list()
  for (rok in seq.int(rok_ostatni + 1, rok_koncowy)) {
    cechy_prog <- data.frame(
      rok = rok,
      x = stany$x,
      y = stany$y,
      dep_lag1 = stany$dep,
      dep_lag2 = poprzednie$dep
    )
    for (nm in cechy_statyczne(stany)) cechy_prog[[nm]] <- stany[[nm]]
    for (nm in names(PREDYKTORY_PANEL)) {
      cechy_prog[[paste0(nm, "_lag1")]] <- stany[[nm]]
      cechy_prog[[paste0(nm, "_lag2")]] <- poprzednie[[nm]]
    }
    cechy_prog <- cechy_prog[, cechy, drop = FALSE]

    pred <- setNames(lapply(CELE_PROGNOZ, function(cel)
      as.numeric(predict(modele[[cel]], as.matrix(cechy_prog)))),
      CELE_PROGNOZ)
    rok_wynik <- stany %>%
      select(kod_powiatu, klasa) %>%
      mutate(rok = rok)
    for (cel in CELE_PROGNOZ) {
      rok_wynik[[paste0("prognoza_", cel)]] <- pred[[cel]]
    }
    wyniki[[length(wyniki) + 1L]] <- rok_wynik
    poprzednie <- stany %>% select(kod_powiatu, all_of(CELE_PROGNOZ))
    for (cel in CELE_PROGNOZ) stany[[cel]] <- pred[[cel]]
  }
  bind_rows(wyniki)
}

ocena_modeli_panelowych <- function(panel, coords = NULL,
                                    lata_testowe = 2019:2024) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  cechy <- P$cechy
  if (is.null(coords)) {
    d$x <- 0
    d$y <- 0
  }

  wyniki <- list()
  for (rok_test in lata_testowe) {
    train <- d %>% filter(rok < rok_test)
    test  <- d %>% filter(rok == rok_test)
    if (nrow(train) == 0 || nrow(test) == 0) next

    p_naive <- test$dep_lag1
    p_fe <- predykcja_fe(train, test, cechy)
    p_xgb <- predykcja_xgb(train, test, cechy)

    # Korekta lokalna: do prognozy dodajemy średnią resztę z ostatniego roku
    # treningowego u 8 najbliższych powiatów (bez samego powiatu). Reszty są
    # agregowane do poziomu powiatu, żeby indeksy sąsiadów odpowiadały
    # wierszom tabeli reszt (wcześniej mieszano indeksy powiatów i obserwacji).
    p_hyb <- p_xgb
    if (!is.null(coords)) {
      ostatni_tr <- max(train$rok)
      tr_ost <- train %>% filter(rok == ostatni_tr)
      reszty_pow <- tr_ost %>%
        mutate(reszta = dep - predykcja_xgb(train %>% filter(rok < ostatni_tr),
                                             tr_ost, cechy)) %>%
        select(kod_powiatu, x, y, reszta)
      for (i in seq_len(nrow(test))) {
        inne <- reszty_pow$kod_powiatu != test$kod_powiatu[i]
        d2 <- (reszty_pow$x[inne] - test$x[i])^2 +
          (reszty_pow$y[inne] - test$y[i])^2
        naj <- order(d2)[seq_len(min(8L, length(d2)))]
        p_hyb[i] <- p_xgb[i] + mean(reszty_pow$reszta[inne][naj])
      }
    }

    peryferyjne <- !is.na(test$klasa) & test$klasa == "peryferyjne"
    predykcje <- list(
      `XGBoost globalny` = p_xgb,
      `FE panelowy` = p_fe,
      `poprzedni rok` = p_naive,
      `XGBoost + korekta lokalna` = p_hyb
    )
    for (nazwa in names(predykcje)) {
      p <- predykcje[[nazwa]]
      wyniki[[length(wyniki) + 1L]] <- data.frame(
        rok = rok_test,
        model = nazwa,
        rmse = rmse(test$dep, p),
        rmse_peryferyjne = if (any(peryferyjne))
          rmse(test$dep[peryferyjne], p[peryferyjne]) else NA_real_,
        n_test = nrow(test),
        n_peryferyjne = sum(peryferyjne)
      )
    }
  }

  surowe <- bind_rows(wyniki)
  tabela <- surowe %>%
    group_by(model) %>%
    summarise(
      rmse_srednie = mean(rmse, na.rm = TRUE),
      rmse_sd = sd(rmse, na.rm = TRUE),
      rmse_peryferyjne_srednie = mean(rmse_peryferyjne, na.rm = TRUE),
      lata_testowe = n(),
      .groups = "drop"
    ) %>%
    arrange(rmse_srednie)
  attr(tabela, "wyniki_roczne") <- surowe
  tabela
}

# Model determinantów (bez opóźnionej zmiennej zależnej) w dwóch wariantach:
# XGBoost (nieliniowy) i elastic net (liniowy, ten sam typ co model prognozy).
# Zwraca funkcję przewidującą na ramce danych z kolumnami `cechy`.
MODELE_DET <- c(xgb = "XGBoost determinantów (bez dep_lag)",
                enet = "Elastic net determinantów (bez dep_lag)",
                enet_progi = "Hybryda: elastic net + progi z ALE (XGBoost)")

# Progi z ALE wyznaczane WYŁĄCZNIE na danych uczących: XGBoost determinantów
# -> krzywe ALE -> model łamany. Zostają cechy wyraźnie nieliniowe
# (R² liniowe < 0,9) o niepomijalnym efekcie (rozpiętość ALE > 0,5‰).
progi_ale <- function(train, cechy) {
  m <- xgb.train(params = cfg_det_add()$params,
                 data = xgb.DMatrix(as.matrix(train[, cechy]), label = train$dep),
                 nrounds = cfg_det_add()$nrounds, verbose = 0)
  X <- as.data.frame(train[, cechy])
  zmienne <- c(names(PREDYKTORY_PANEL), cechy_statyczne(train))
  bind_rows(lapply(zmienne, function(v) ocena_nieliniowosci(ale_cecha(m, X, v)))) %>%
    filter(r2_liniowe < 0.9, zakres_efektu > 0.5)
}

# Cecha "łamana" (hinge): max(x - próg, 0); razem z x daje w modelu liniowym
# inne nachylenie przed i po progu.
dodaj_progi <- function(D, progi) {
  for (i in seq_len(nrow(progi))) {
    v <- progi$cecha[i]
    k <- if (v %in% names(D)) v else paste0(v, "_lag1")
    D[[paste0("prog_", v)]] <- pmax(D[[k]] - progi$prog[i], 0)
  }
  D
}

dopasuj_det <- function(train, cechy, typ = c("xgb", "enet", "enet_progi")) {
  typ <- match.arg(typ)
  if (typ == "enet") {
    fit <- enet_dopasuj(train, cechy)
    return(function(X) enet_przewidz(fit, X))
  }
  if (typ == "enet_progi") {
    pr <- progi_ale(train, cechy)
    fit <- enet_dopasuj(dodaj_progi(train, pr), c(cechy, paste0("prog_", pr$cecha)))
    f <- function(X) enet_przewidz(fit, dodaj_progi(X, pr))
    attr(f, "progi") <- pr
    attr(f, "fit") <- fit
    return(f)
  }
  m <- xgb.train(params = cfg_det()$params,
                 data = xgb.DMatrix(as.matrix(train[, cechy]), label = train$dep),
                 nrounds = cfg_det()$nrounds, verbose = 0)
  function(X) as.numeric(predict(m, as.matrix(X[, cechy, drop = FALSE])))
}

# Interakcje cech: nastrojony XGBoost determinantów (jeśli strojenie dałoby
# drzewa o głębokości 1, czyli model addytywny, używamy głębokości 3)
# i wartości interakcji SHAP;
# wersje t-1 i t-2 cechy sumujemy w jedną grupę. Najsilniejsze pary (wybrane
# tylko na latach uczących) dodajemy do hybrydy jako iloczyny wycentrowanych
# cech i sprawdzamy w walidacji kroczącej, czy poprawiają prognozę.
xgb_cfg_interakcje <- function() {
  if (cfg_det()$params$max_depth >= 2) return(cfg_det())
  list(params = modifyList(cfg_det()$params, list(max_depth = 3, eta = 0.02,
         min_child_weight = 5, subsample = 0.8, colsample_bytree = 0.8)), nrounds = 350)
}

ranking_interakcji <- function(train, cechy) {
  cfg <- xgb_cfg_interakcje()
  m <- xgb.train(params = cfg$params,
                 data = xgb.DMatrix(as.matrix(train[, cechy]), label = train$dep),
                 nrounds = cfg$nrounds, verbose = 0)
  I <- predict(m, xgb.DMatrix(as.matrix(train[, cechy])), predinteraction = TRUE)
  I <- I[, cechy, cechy, drop = FALSE]
  g <- sub("_lag[12]$", "", cechy); G <- unique(g[!g %in% c("rok", "x", "y")])
  wiersze <- list()
  for (i in seq_along(G)) for (j in seq_along(G)) if (i < j) {
    a <- which(g == G[i]); b <- which(g == G[j])
    s <- apply(I[, a, b, drop = FALSE], 1, sum) * 2   # phi_ab + phi_ba
    wiersze[[length(wiersze) + 1]] <- data.frame(cecha_a = G[i], cecha_b = G[j], sila = mean(abs(s)))
  }
  glowne <- vapply(G, function(v) { a <- which(g == v); mean(abs(apply(I[, a, a, drop = FALSE], 1, sum))) }, 0)
  r <- bind_rows(wiersze) %>% arrange(desc(sila))
  attr(r, "udzial_interakcji") <- sum(r$sila) / (sum(r$sila) + sum(glowne))
  r
}

dopasuj_enet_interakcje <- function(train, cechy, K = 3) {
  kol <- function(D, v) if (v %in% names(D)) v else paste0(v, "_lag1")
  pr <- progi_ale(train, cechy)
  pary <- head(ranking_interakcji(train, cechy), K)
  uzyte <- unique(c(vapply(pary$cecha_a, function(v) kol(train, v), ""),
                    vapply(pary$cecha_b, function(v) kol(train, v), "")))
  sr <- vapply(uzyte, function(v) mean(train[[v]]), 0)
  prz <- function(D) {
    D <- dodaj_progi(D, pr)
    for (k in seq_len(nrow(pary))) {
      a <- kol(D, pary$cecha_a[k]); b <- kol(D, pary$cecha_b[k])
      D[[paste0("int_", k)]] <- (D[[a]] - sr[[a]]) * (D[[b]] - sr[[b]])
    }
    D
  }
  fit <- enet_dopasuj(prz(train), c(cechy, paste0("prog_", pr$cecha), paste0("int_", seq_len(nrow(pary)))))
  f <- function(X) enet_przewidz(fit, prz(X))
  attr(f, "pary") <- pary
  f
}

ocena_interakcji <- function(panel, coords = NULL, lata_testowe = 2019:2024, K = c(1, 3, 5)) {
  P <- przygotuj_dane_xgb_determinanty(panel, coords)
  ranking <- ranking_interakcji(P$d, P$cechy)
  wyn <- list(); bl <- list(); pary <- list()
  for (rok_test in lata_testowe) {
    train <- P$d %>% filter(rok < rok_test); test <- P$d %>% filter(rok == rok_test)
    modele <- list(enet_progi = dopasuj_det(train, P$cechy, "enet_progi"))
    for (k in K) modele[[paste0("enet_progi_int", k)]] <- dopasuj_enet_interakcje(train, P$cechy, k)
    pr3 <- modele[[paste0("enet_progi_int", max(K))]]
    pary[[length(pary) + 1]] <- attr(pr3, "pary") %>% mutate(rok_testowy = rok_test)
    for (nm in names(modele)) {
      p <- modele[[nm]](test)
      wyn[[length(wyn) + 1]] <- data.frame(rok = rok_test, model = nm, rmse = rmse(test$dep, p))
      bl[[length(bl) + 1]] <- data.frame(kod_powiatu = test$kod_powiatu, rok = rok_test, model = nm,
                                          e2 = (test$dep - p)^2)
    }
  }
  W <- bind_rows(wyn)
  E <- bind_rows(bl) %>% tidyr::pivot_wider(names_from = model, values_from = e2)
  porown <- setNames(rep("enet_progi", length(K)), paste0("enet_progi_int", K))
  dm <- bind_rows(lapply(names(porown), function(a) {
    E$r <- E[[a]] - E[[porown[[a]]]]
    m <- fixest::feols(r ~ 1, data = E, cluster = ~kod_powiatu)
    data.frame(model_a = a, model_b = porown[[a]], srednia_roznica = unname(coef(m)[1]),
               p = unname(fixest::pvalue(m)[1]), lat_lepszy = sum(tapply(E$r, E$rok, mean) < 0))
  }))
  tab <- W %>% group_by(model) %>% summarise(rmse_srednie = mean(rmse), .groups = "drop") %>%
    left_join(dm %>% select(model = model_a, wzgledem = model_b, srednia_roznica, p, lat_lepszy), by = "model")
  list(ranking = ranking, udzial = attr(ranking, "udzial_interakcji"), walidacja = tab,
       pary_okna = bind_rows(pary), roczne = W)
}

# Ocena modeli determinantów: walidacja krocząca + panelowe testy DM parami.
ocena_xgb_determinantow <- function(panel, coords = NULL,
                                    lata_testowe = 2019:2024) {
  P <- przygotuj_dane_xgb_determinanty(panel, coords)
  d <- P$d
  wyniki <- list(); bledy <- list(); progi <- list()

  for (rok_test in lata_testowe) {
    train <- d %>% filter(rok < rok_test)
    test <- d %>% filter(rok == rok_test)
    if (nrow(train) == 0 || nrow(test) == 0) next
    for (typ in names(MODELE_DET)) {
      f <- dopasuj_det(train, P$cechy, typ)
      pred <- f(test)
      if (!is.null(attr(f, "progi")))
        progi[[length(progi) + 1L]] <- attr(f, "progi") %>% mutate(rok_testowy = rok_test)
      wyniki[[length(wyniki) + 1L]] <- data.frame(
        rok = rok_test, model = MODELE_DET[[typ]],
        rmse = rmse(test$dep, pred), n_test = nrow(test))
      bledy[[length(bledy) + 1L]] <- data.frame(
        kod_powiatu = test$kod_powiatu, rok = rok_test, typ = typ,
        e2 = (test$dep - pred)^2)
    }
  }

  surowe <- bind_rows(wyniki)
  tabela <- surowe %>%
    group_by(model) %>%
    summarise(
      rmse_srednie = mean(rmse, na.rm = TRUE),
      rmse_sd = sd(rmse, na.rm = TRUE),
      lata_testowe = n(),
      .groups = "drop"
    ) %>%
    arrange(rmse_srednie)
  # panelowy test Diebolda-Mariano: różnica kwadratów błędów A - B
  E <- bind_rows(bledy) %>% tidyr::pivot_wider(names_from = typ, values_from = e2)
  pary <- list(c("enet", "xgb"), c("enet_progi", "enet"), c("enet_progi", "xgb"))
  attr(tabela, "dm") <- bind_rows(lapply(pary, function(pr) {
    E$roznica <- E[[pr[1]]] - E[[pr[2]]]
    m_dm <- fixest::feols(roznica ~ 1, data = E, cluster = ~kod_powiatu)
    data.frame(model_a = MODELE_DET[[pr[1]]], model_b = MODELE_DET[[pr[2]]],
               srednia_roznica = unname(coef(m_dm)[1]),
               p = unname(fixest::pvalue(m_dm)[1]),
               lat_lepszy = sum(tapply(E$roznica, E$rok, mean) < 0))
  }))
  attr(tabela, "progi") <- bind_rows(progi)
  attr(tabela, "wyniki_roczne") <- surowe
  tabela
}

# Porównanie ważności cech EN vs XGB (model determinantów).
# Permutacja GRUPOWA: obie wersje opóźnienia cechy (t-1, t-2) permutowane
# razem, bo są silnie skorelowane i model liniowy może dzielić między nie wagę.
# Ważność = wzrost RMSE po permutacji, średnio po latach testowych 2019-2024.
porownanie_waznosci_modeli <- function(panel, coords = NULL,
                                       lata_testowe = 2019:2024,
                                       n_permutacji = 5) {
  P <- przygotuj_dane_xgb_determinanty(panel, coords)
  d <- P$d
  preds <- names(PREDYKTORY_PANEL)
  grupy <- c(setNames(lapply(preds, function(nm) paste0(nm, c("_lag1", "_lag2"))), preds),
             setNames(as.list(cechy_statyczne(d)), cechy_statyczne(d)))
  set.seed(20261104)
  wyniki <- list()
  for (typ in c("xgb", "enet")) for (rok_test in lata_testowe) {
    train <- d %>% filter(rok < rok_test)
    test <- d %>% filter(rok == rok_test)
    if (nrow(train) == 0 || nrow(test) == 0) next
    przewidz <- dopasuj_det(train, P$cechy, typ)
    base <- rmse(test$dep, przewidz(test))
    for (g in names(grupy)) {
      delty <- replicate(n_permutacji, {
        idx <- sample(nrow(test)); tp <- test
        for (k in grupy[[g]]) tp[[k]] <- test[[k]][idx]
        rmse(test$dep, przewidz(tp)) - base
      })
      wyniki[[length(wyniki) + 1L]] <- data.frame(
        typ = typ, rok = rok_test, cecha = g, delta_rmse = mean(delty))
    }
  }
  tab <- bind_rows(wyniki) %>%
    group_by(typ, cecha) %>%
    summarise(delta = mean(delta_rmse), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = typ, values_from = delta) %>%
    rename(waznosc_xgb = xgb, waznosc_enet = enet) %>%
    mutate(udzial_xgb = pmax(waznosc_xgb, 0) / sum(pmax(waznosc_xgb, 0)),
           udzial_enet = pmax(waznosc_enet, 0) / sum(pmax(waznosc_enet, 0)),
           ranga_xgb = rank(-waznosc_xgb), ranga_enet = rank(-waznosc_enet)) %>%
    arrange(ranga_enet)
  sp <- suppressWarnings(cor.test(tab$waznosc_xgb, tab$waznosc_enet,
                                  method = "spearman"))
  attr(tab, "spearman") <- data.frame(rho = unname(sp$estimate), p = sp$p.value,
                                      n_cech = nrow(tab))
  tab
}

wykres_waznosci_porownanie <- function(tab) {
  dane <- tab %>%
    transmute(cecha = etykieta(cecha), `XGBoost` = udzial_xgb,
              `Elastic net` = udzial_enet) %>%
    tidyr::pivot_longer(-cecha, names_to = "model", values_to = "udzial") %>%
    mutate(cecha = factor(cecha, levels = rev(etykieta(tab$cecha))))
  rho <- attr(tab, "spearman")$rho
  g <- ggplot(dane, aes(udzial, cecha, fill = model)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_x_continuous(labels = function(v) paste0(round(100 * v), "%")) +
    scale_fill_manual(values = c(`Elastic net` = "#2b6cb0", XGBoost = "#dd6b20")) +
    labs(title = "Ważność cech: elastic net vs XGBoost",
         subtitle = sprintf("Model determinantów, permutacja grupowa, lata testowe 2019-2024; Spearman = %.2f",
                            rho),
         x = "udział w łącznym wzroście RMSE", y = NULL, fill = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(OUT, "rys_waznosc_enet_vs_xgb.png"), g, width = 8, height = 5, dpi = 150)
}

# Permutation importance determinantów liczona bez opóźnionej zmiennej zależnej.
ocena_permutacyjna_determinantow <- function(panel, coords = NULL,
                                             lata_testowe = 2019:2024,
                                             n_permutacji = 3) {
  P <- przygotuj_dane_xgb_determinanty(panel, coords)
  d <- P$d
  wyniki <- list()
  set.seed(20261024)

  for (rok_test in lata_testowe) {
    train <- d %>% filter(rok < rok_test)
    test <- d %>% filter(rok == rok_test)
    if (nrow(train) == 0 || nrow(test) == 0) next
    {
      nazwa <- "XGBoost determinantów (bez dep_lag)"
      cechy <- P$cechy
      model <- xgb.train(
        params = cfg_det()$params,
        data = xgb.DMatrix(as.matrix(train[, cechy]), label = train$dep),
        nrounds = cfg_det()$nrounds,
        verbose = 0
      )
      x_test <- as.matrix(test[, cechy])
      pred_base <- as.numeric(predict(model, x_test))
      rmse_base <- rmse(test$dep, pred_base)
      rmse_base_per <- NA_real_

      for (cecha in cechy) {
        delty <- numeric(n_permutacji)
        delty_per <- numeric(n_permutacji)
        for (j in seq_len(n_permutacji)) {
          x_perm <- x_test
          x_perm[, cecha] <- sample(x_perm[, cecha], replace = FALSE)
          pred_perm <- as.numeric(predict(model, x_perm))
          delty[j] <- rmse(test$dep, pred_perm) - rmse_base
          delty_per[j] <- NA_real_
        }
        wyniki[[length(wyniki) + 1L]] <- data.frame(
          rok = rok_test,
          model = nazwa,
          cecha = cecha,
          delta_rmse = mean(delty),
          delta_rmse_peryferyjne = mean(delty_per),
          rmse_bazowe = rmse_base,
          rmse_bazowe_peryferyjne = rmse_base_per,
          n_permutacji = n_permutacji
        )
      }
    }
  }

  surowe <- bind_rows(wyniki)
  surowe %>%
    group_by(model, cecha) %>%
    summarise(
      delta_rmse_srednia = mean(delta_rmse, na.rm = TRUE),
      delta_rmse_sd = sd(delta_rmse, na.rm = TRUE),
      delta_rmse_peryferyjne_srednia =
        mean(delta_rmse_peryferyjne, na.rm = TRUE),
      lata_testowe = n(),
      .groups = "drop"
    ) %>%
    arrange(model, desc(delta_rmse_srednia)) %>%
    structure(wyniki_roczne = surowe)
}

wykres_waznosci_xgb <- function(importance) {
  dane <- importance %>%
    filter(!cecha %in% c("rok", "x", "y")) %>%
    mutate(
      cecha_bazowa = sub("_(lag1|lag2)$", "", cecha),
      etykieta = dplyr::recode(
        cecha_bazowa,
        dep = "zmiana ludności",
        doch_wlasne_pc = "dochody własne pc",
        udzial_wyd_inwest = "udział wyd. inwest.",
        bezrobocie_rejestrowane = "bezrobocie rejestrowane",
        wage_rel = "płace względem kraju",
        podmioty_1000 = "firmy / 1000 mieszk.",
        kanalizacja_pct = "kanalizacja %",
        lekarze_10tys = "lekarze / 10 tys.",
        udzial_65_plus = "udział 65+",
        gestosc_zaludnienia = "gęstość zaludnienia",
        odl_stolica_km = "odległość od miasta woj. (GIS)",
        .default = cecha_bazowa
      ),
      etykieta = paste0(
        etykieta,
        ifelse(grepl("_lag1$", cecha), " — lag 1",
               ifelse(grepl("_lag2$", cecha), " — lag 2", ""))
      )
    ) %>%
    arrange(delta_rmse_srednia) %>%
    mutate(etykieta = factor(etykieta, levels = etykieta))

  p <- ggplot(dane, aes(x = delta_rmse_srednia, y = etykieta,
                        fill = FALSE)) +
    geom_col() +
    scale_fill_manual(
      values = c(`FALSE` = "#4472C4", `TRUE` = "#C00000"),
      guide = "none"
    ) +
    labs(
      title = "Permutation importance — XGBoost strukturalny",
      subtitle = "Wzrost RMSE po przetasowaniu cechy",
      x = "Średnia zmiana RMSE",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank())

  ggsave(
    file.path(OUT, "rys_xgb_permutation_importance.png"),
    p, width = 11, height = 8, dpi = 130
  )
  invisible(p)
}

bootstrap_istotnosci_importance <- function(importance_roczna,
                                            n_boot = 1000,
                                            seed = 20261024) {
  wymagane <- c("rok", "model", "cecha", "delta_rmse")
  brakujace <- setdiff(wymagane, names(importance_roczna))
  if (length(brakujace) > 0) {
    stop("Brak kolumn bootstrapu importance: ",
         paste(brakujace, collapse = ", "))
  }
  bloki <- importance_roczna %>%
    group_by(rok, model, cecha) %>%
    summarise(delta = mean(delta_rmse, na.rm = TRUE), .groups = "drop")
  set.seed(seed)
  wyniki <- lapply(split(bloki, interaction(bloki$model, bloki$cecha,
                                             drop = TRUE)), function(d) {
    lata <- unique(d$rok)
    est <- mean(d$delta, na.rm = TRUE)
    boot <- replicate(n_boot, {
      probka <- sample(lata, length(lata), replace = TRUE)
      mean(d$delta[match(probka, d$rok)], na.rm = TRUE)
    })
    p_emp <- 2 * min(mean(boot <= 0), mean(boot >= 0))
    data.frame(
      model = d$model[1],
      cecha = d$cecha[1],
      delta_rmse_srednia = est,
      ci95_dolna = unname(quantile(boot, 0.025, na.rm = TRUE)),
      ci95_gorna = unname(quantile(boot, 0.975, na.rm = TRUE)),
      p_bootstrap = min(1, p_emp),
      n_blokow = length(lata),
      n_boot = n_boot
    )
  })
  bind_rows(wyniki) %>%
    group_by(model) %>%
    mutate(p_fdr = p.adjust(p_bootstrap, method = "BH")) %>%
    ungroup() %>%
    arrange(model, p_fdr, desc(delta_rmse_srednia))
}

przygotuj_dane_xgb_przestrzenny <- function(panel, coords, k = 8) {
  if (is.null(coords)) {
    stop("Przestrzenny XGBoost wymaga centroidow_powiatow.csv.")
  }
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  coords_d <- d %>%
    distinct(kod_powiatu, x, y) %>%
    arrange(kod_powiatu)
  W <- macierz_knn(coords_d, k)
  cechy_bazowe <- P$cechy
  cechy_lagowane <- setdiff(cechy_bazowe, c("rok", "x", "y", "odl_stolica_km"))
  cechy_przestrzenne <- paste0("sp_lag_", cechy_lagowane)
  for (cecha in cechy_przestrzenne) d[[cecha]] <- NA_real_

  for (j in seq_along(cechy_lagowane)) {
    cecha <- cechy_lagowane[j]
    wartosci <- split(d[[cecha]], d$rok)
    for (rok in names(wartosci)) {
      idx <- which(d$rok == as.numeric(rok))
      kolejnosc <- match(d$kod_powiatu[idx], coords_d$kod_powiatu)
      vals <- rep(NA_real_, nrow(coords_d))
      vals[kolejnosc] <- d[[cecha]][idx]
      if (any(!is.finite(vals))) {
        stop("Niekompletne dane do przestrzennego laga: ", cecha,
             ", rok ", rok)
      }
      d[[cechy_przestrzenne[j]]][idx] <- as.numeric(W %*% vals)[kolejnosc]
    }
  }
  d <- d %>% filter(if_all(all_of(cechy_przestrzenne), is.finite))
  list(
    d = d,
    cechy = c(cechy_bazowe, cechy_przestrzenne),
    W = W,
    coords = coords_d
  )
}

ocena_xgb_przestrzennego <- function(panel, coords,
                                     lata_testowe = 2019:2024,
                                     k = 8) {
  if (is.null(coords)) {
    stop("Ocena przestrzennego XGBoost wymaga centroidow_powiatow.csv.")
  }
  P0 <- przygotuj_dane_xgb_panel(panel, coords)
  P1 <- przygotuj_dane_xgb_przestrzenny(panel, coords, k)
  wyniki <- list()
  predykcje <- list()
  for (rok_test in lata_testowe) {
    train0 <- P0$d %>% filter(rok < rok_test)
    test0 <- P0$d %>% filter(rok == rok_test)
    train1 <- P1$d %>% filter(rok < rok_test)
    test1 <- P1$d %>% filter(rok == rok_test)
    if (nrow(train0) == 0 || nrow(test0) == 0 ||
        nrow(train1) == 0 || nrow(test1) == 0) next

    pred0 <- predykcja_xgb(train0, test0, P0$cechy)
    pred1 <- predykcja_xgb(train1, test1, P1$cechy)
    peryferyjne <- !is.na(test0$klasa) &
      test0$klasa == "peryferyjne"
    for (spec in c("XGBoost bazowy", "XGBoost przestrzenny")) {
      pred <- if (spec == "XGBoost bazowy") pred0 else pred1
      wyniki[[length(wyniki) + 1L]] <- data.frame(
        rok = rok_test,
        model = spec,
        rmse = rmse(test0$dep, pred),
        rmse_peryferyjne = if (any(peryferyjne))
          rmse(test0$dep[peryferyjne], pred[peryferyjne]) else NA_real_,
        n_test = nrow(test0),
        n_peryferyjne = sum(peryferyjne)
      )
      predykcje[[length(predykcje) + 1L]] <- data.frame(
        kod_powiatu = test0$kod_powiatu,
        rok = rok_test,
        dep = test0$dep,
        klasa = test0$klasa,
        model = spec,
        pred = pred
      )
    }
  }
  surowe <- bind_rows(wyniki)
  tabela <- surowe %>%
    group_by(model) %>%
    summarise(
      rmse_srednie = mean(rmse, na.rm = TRUE),
      rmse_sd = sd(rmse, na.rm = TRUE),
      rmse_peryferyjne_srednie =
        mean(rmse_peryferyjne, na.rm = TRUE),
      lata_testowe = n(),
      .groups = "drop"
    ) %>%
    arrange(rmse_srednie)
  attr(tabela, "wyniki_roczne") <- surowe
  attr(tabela, "predykcje") <- bind_rows(predykcje)
  tabela
}

kalibracja_przedzialow_xgb <- function(panel, coords = NULL,
                                       lata_testowe = 2019:2024,
                                       poziom = 0.90) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  bledy <- list()
  for (rok_test in lata_testowe) {
    train <- d %>% filter(rok < rok_test)
    test <- d %>% filter(rok == rok_test)
    if (nrow(train) == 0 || nrow(test) == 0) next
    pred <- predykcja_xgb(train, test, P$cechy)
    bledy[[length(bledy) + 1L]] <- data.frame(
      kod_powiatu = test$kod_powiatu,
      rok = rok_test,
      klasa = test$klasa,
      dep = test$dep,
      pred = pred,
      blad_abs = abs(test$dep - pred)
    )
  }
  bledy <- bind_rows(bledy)
  if (nrow(bledy) == 0) stop("Brak bledow out-of-time do kalibracji.")
  q <- function(x) unname(quantile(x, probs = poziom, na.rm = TRUE,
                                    type = 8))
  kalibratory <- bind_rows(
    data.frame(grupa = "wszystkie", q90 = q(bledy$blad_abs),
               n = nrow(bledy)),
    bledy %>%
      group_by(klasa) %>%
      summarise(q90 = q(blad_abs), n = n(), .groups = "drop") %>%
      rename(grupa = klasa)
  )
  bledy <- bledy %>%
    mutate(
      dolna = pred - q(blad_abs),
      gorna = pred + q(blad_abs),
      pokrycie = dep >= dolna & dep <= gorna
    )
  pokrycie <- bind_rows(
    bledy %>%
      summarise(grupa = "wszystkie", pokrycie = mean(pokrycie), n = n()),
    bledy %>%
      group_by(klasa) %>%
      summarise(pokrycie = mean(pokrycie), n = n(), .groups = "drop") %>%
      rename(grupa = klasa)
  ) %>%
    left_join(kalibratory, by = c("grupa", "n"))
  list(
    kalibratory = kalibratory,
    pokrycie = pokrycie,
    bledy = bledy
  )
}

kalibracja_horyzontow_xgb <- function(panel, coords = NULL,
                                      lata_kalibracyjne = 2016:2018,
                                      lata_oceny = 2019:2021,
                                      horyzonty = 1:3,
                                      poziom = 0.90) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  wyniki <- list()
  lata_poczatkowe <- sort(unique(c(lata_kalibracyjne, lata_oceny)))
  for (rok_start in lata_poczatkowe) {
    train <- d %>% filter(rok <= rok_start)
    if (nrow(train) == 0) next
    model <- xgb.train(
      params = XGB_MODEL_CONFIG$params,
      data = xgb.DMatrix(as.matrix(train[, P$cechy]), label = train$dep),
      nrounds = XGB_MODEL_CONFIG$nrounds,
      verbose = 0
    )
    for (h in horyzonty) {
      test <- d %>% filter(rok == rok_start + h)
      if (nrow(test) == 0) next
      pred <- as.numeric(predict(model, as.matrix(test[, P$cechy])))
      wyniki[[length(wyniki) + 1L]] <- data.frame(
        rok_start = rok_start,
        horyzont = h,
        zbior = if (rok_start %in% lata_kalibracyjne)
          "kalibracja" else "ocena",
        kod_powiatu = test$kod_powiatu,
        klasa = test$klasa,
        dep = test$dep,
        pred = pred,
        blad_abs = abs(test$dep - pred)
      )
    }
  }
  bledy <- bind_rows(wyniki)
  if (nrow(bledy) == 0) {
    stop("Brak danych do kalibracji wielohoryzontowej.")
  }
  q <- function(x) unname(quantile(x, probs = poziom, na.rm = TRUE,
                                    type = 8))
  kalibratory <- bledy %>%
    filter(zbior == "kalibracja") %>%
    group_by(horyzont, klasa) %>%
    summarise(q90 = q(blad_abs), n = n(), .groups = "drop")
  pokrycie <- bledy %>%
    filter(zbior == "ocena") %>%
    left_join(kalibratory, by = c("horyzont", "klasa")) %>%
    mutate(pokrycie = abs(dep - pred) <= q90) %>%
    group_by(horyzont, klasa) %>%
    summarise(
      pokrycie = mean(pokrycie),
      q90 = first(q90),
      n = n(),
      .groups = "drop"
    )
  list(kalibratory = kalibratory, pokrycie = pokrycie,
       bledy = bledy)
}

moran_i <- function(z, W, n_permutacji = 199, seed = 1) {
  z <- as.numeric(z)
  ok <- is.finite(z)
  z <- z[ok]
  W <- W[ok, ok, drop = FALSE]
  if (length(z) < 3 || sum(W) == 0) {
    return(data.frame(moran_i = NA_real_, p_permutacyjne = NA_real_,
                      n = length(z)))
  }
  zc <- z - mean(z)
  den <- sum(zc^2)
  observed <- length(z) / sum(W) * sum(W * outer(zc, zc)) / den
  set.seed(seed)
  simulated <- replicate(n_permutacji, {
    zp <- sample(z)
    zpc <- zp - mean(zp)
    length(zp) / sum(W) * sum(W * outer(zpc, zpc)) / sum(zpc^2)
  })
  data.frame(
    moran_i = observed,
    p_permutacyjne = (1 + sum(abs(simulated) >= abs(observed))) /
      (n_permutacji + 1),
    n = length(z)
  )
}

macierz_knn <- function(coords, k = 8) {
  xy <- as.matrix(coords[, c("x", "y")])
  D <- as.matrix(dist(xy))
  W <- matrix(0, nrow = nrow(coords), ncol = nrow(coords))
  for (i in seq_len(nrow(W))) {
    sasiedzi <- order(D[i, ])[seq_len(min(k + 1L, nrow(W)))]
    sasiedzi <- sasiedzi[sasiedzi != i]
    if (length(sasiedzi) > 0) W[i, sasiedzi] <- 1 / length(sasiedzi)
  }
  W
}

ocena_autokorelacji_przestrzennej <- function(panel, coords = NULL,
                                               lata_testowe = 2019:2024,
                                               k = 8,
                                               n_permutacji = 199) {
  if (is.null(coords)) {
    stop("Diagnostyka przestrzenna wymaga centroidy_powiatow.csv.")
  }
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  warianty <- list("XGBoost prognostyczny" = P$cechy)
  coords_d <- d %>%
    distinct(kod_powiatu, x, y) %>%
    arrange(kod_powiatu)
  W <- macierz_knn(coords_d, k)
  wyniki <- list()

  for (rok_test in lata_testowe) {
    train <- d %>% filter(rok < rok_test)
    test <- d %>%
      filter(rok == rok_test) %>%
      right_join(coords_d %>% select(kod_powiatu), by = "kod_powiatu") %>%
      arrange(kod_powiatu)
    if (nrow(train) == 0 || nrow(test) == 0 ||
        anyNA(test$dep)) next

    for (nazwa in names(warianty)) {
      cechy <- warianty[[nazwa]]
      pred <- predykcja_xgb(train, test, cechy)
      diagnostyka_celu <- moran_i(test$dep, W, n_permutacji,
                                  seed = 1000 + rok_test)
      diagnostyka_reszt <- moran_i(test$dep - pred, W, n_permutacji,
                                   seed = 2000 + rok_test)
      wyniki[[length(wyniki) + 1L]] <- data.frame(
        rok = rok_test,
        model = nazwa,
        k_sasiadow = k,
        moran_cel = diagnostyka_celu$moran_i,
        p_cel = diagnostyka_celu$p_permutacyjne,
        moran_reszty = diagnostyka_reszt$moran_i,
        p_reszty = diagnostyka_reszt$p_permutacyjne,
        n = diagnostyka_reszt$n
      )
    }
  }
  bind_rows(wyniki)
}

lokalny_moran <- function(z, coords, W, n_permutacji = 99, seed = 1) {
  z <- as.numeric(z)
  if (length(z) != nrow(W) || any(!is.finite(z))) {
    stop("Wektor lokalnego Moran's I nie pasuje do macierzy sasiedztwa.")
  }
  zstd <- (z - mean(z)) / sd(z)
  lag_z <- as.numeric(W %*% zstd)
  set.seed(seed)
  wyniki <- lapply(seq_along(z), function(i) {
    observed <- zstd[i] * lag_z[i]
    # permutacja warunkowa: wartość w i stała, pozostałe tasowane
    simulated <- replicate(n_permutacji, {
      zp <- zstd
      zp[-i] <- sample(zstd[-i])
      zstd[i] * sum(W[i, ] * zp)
    })
    p <- (1 + sum(abs(simulated) >= abs(observed))) /
      (n_permutacji + 1)
    klaster <- if (p >= 0.05) {
      "NS"
    } else if (zstd[i] >= 0 && lag_z[i] >= 0) {
      "High-High"
    } else if (zstd[i] < 0 && lag_z[i] < 0) {
      "Low-Low"
    } else if (zstd[i] >= 0) {
      "High-Low"
    } else {
      "Low-High"
    }
    data.frame(
      kod_powiatu = coords$kod_powiatu[i],
      x = coords$x[i],
      y = coords$y[i],
      wartosc = z[i],
      moran_lokalny = observed,
      p_permutacyjne = p,
      klaster = klaster
    )
  })
  bind_rows(wyniki)
}

diagnostyka_klastrow_lokalnych <- function(panel, coords,
                                           predykcje,
                                           n_permutacji = 99) {
  if (is.null(coords)) {
    stop("Diagnostyka lokalna wymaga centroidow_powiatow.csv.")
  }
  coords_d <- coords %>%
    transmute(kod_powiatu = as.character(kod), x, y) %>%
    distinct(kod_powiatu, .keep_all = TRUE) %>%
    filter(kod_powiatu %in% predykcje$kod_powiatu) %>%
    arrange(kod_powiatu)
  W <- macierz_knn(coords_d, k = 8)
  wyniki <- list()
  for (rok in sort(unique(predykcje$rok))) {
    for (model in unique(predykcje$model)) {
      d <- predykcje %>%
        filter(rok == !!rok, model == !!model) %>%
        right_join(coords_d, by = "kod_powiatu") %>%
        arrange(kod_powiatu)
      if (any(!is.finite(d$dep)) || any(!is.finite(d$pred))) next
      dla_celu <- lokalny_moran(
        d$dep, coords_d, W, n_permutacji,
        seed = 3000 + rok
      ) %>% mutate(rok = rok, model = model, obiekt = "dep")
      dla_reszt <- lokalny_moran(
        d$dep - d$pred, coords_d, W, n_permutacji,
        seed = 4000 + rok
      ) %>% mutate(rok = rok, model = model, obiekt = "reszty")
      wyniki[[length(wyniki) + 1L]] <- bind_rows(dla_celu, dla_reszt)
    }
  }
  bind_rows(wyniki)
}

# ---------------------------------------------------------------------------
# ELASTIC NET: dobór hiperparametrów na osi czasu + prognoza do 2030
# ---------------------------------------------------------------------------
# Elastic net (Zou i Hastie 2005; pakiet glmnet) = regresja liniowa z karą
# L1 (lasso, selekcja cech) + L2 (ridge, stabilność przy skorelowanych
# cechach). W teście odporności był jedynym modelem istotnie lepszym od FE,
# dlatego to on generuje główną prognozę do 2030 r.
# Dobór lambda i alpha bez mieszania lat: model uczony na latach uczących bez
# ostatniego, wybór (alpha, lambda) minimalizujący RMSE na ostatnim roku
# uczącym, a następnie ponowne dopasowanie na wszystkich latach uczących.
ENET_ALPHA <- c(0.2, 0.5, 0.8, 1)   # gęstsza siatka seq(0,1,0.1) testowana: brak poprawy RMSE

enet_dopasuj <- function(train, cechy) {
  X <- as.matrix(train[, cechy]); y <- train$dep
  ostatni <- max(train$rok)
  wew <- train$rok < ostatni
  najlepszy <- list(rmse = Inf)
  if (sum(wew) > 50 && length(unique(train$rok)) >= 3) {
    for (a in ENET_ALPHA) {
      m <- glmnet::glmnet(X[wew, , drop = FALSE], y[wew], alpha = a)
      P <- predict(m, X[!wew, , drop = FALSE])
      r <- sqrt(colMeans((y[!wew] - P)^2))
      if (min(r) < najlepszy$rmse)
        najlepszy <- list(rmse = min(r), alpha = a, lambda = m$lambda[which.min(r)])
    }
  } else {
    najlepszy <- list(alpha = 0.5, lambda = NULL)
  }
  m <- glmnet::glmnet(X, y, alpha = najlepszy$alpha)
  s <- if (is.null(najlepszy$lambda)) min(m$lambda) else najlepszy$lambda
  list(model = m, s = s, alpha = najlepszy$alpha, cechy = cechy)
}

enet_przewidz <- function(fit, dane) {
  as.numeric(predict(fit$model, as.matrix(dane[, fit$cechy]), s = fit$s))
}

# średnia zmiana ludności powiatu z lat WCZEŚNIEJSZYCH (bez wycieku)
dodaj_sr_dep_hist <- function(d, panel) {
  hist <- panel %>% arrange(kod_powiatu, rok) %>% group_by(kod_powiatu) %>%
    mutate(sr_dep_hist = dplyr::lag(cummean(dep))) %>% ungroup() %>%
    select(kod_powiatu, rok, sr_dep_hist)
  d %>% select(-any_of("sr_dep_hist")) %>%
    left_join(hist, by = c("kod_powiatu", "rok")) %>%
    filter(is.finite(sr_dep_hist))
}

# Prognoza rekurencyjna 2025-2030 dla modelu "enet" (główna) lub "xgb"
# (porównawcza). Ścieżki predyktorów: trend z ostatnich 3 lat w granicach
# zakresu obserwowanego. Średnia historyczna powiatu jest aktualizowana
# o kolejne prognozy. Przedział 90%: q90 |błędu| z walidacji kroczącej
# danego modelu, poszerzany o sqrt(horyzontu).
prognoza_do_2030 <- function(panel, coords = NULL, rok_koncowy = 2030,
                             q90 = NA_real_, model_typ = c("enet", "xgb")) {
  model_typ <- match.arg(model_typ)
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- dodaj_sr_dep_hist(P$d, panel)
  cechy <- if (model_typ == "enet") c(P$cechy, "sr_dep_hist") else P$cechy
  preds <- names(PREDYKTORY_PANEL)
  rok_ostatni <- max(panel$rok, na.rm = TRUE)
  if (rok_koncowy <= rok_ostatni) {
    stop("Rok końcowy prognozy musi być późniejszy niż ostatni rok danych.")
  }

  if (model_typ == "enet") {
    fit <- enet_dopasuj(d, cechy)
    przewidz <- function(X) enet_przewidz(fit, X)
  } else {
    fit <- xgb.train(params = XGB_MODEL_CONFIG$params,
                     data = xgb.DMatrix(as.matrix(d[, cechy]), label = d$dep),
                     nrounds = XGB_MODEL_CONFIG$nrounds, verbose = 0)
    przewidz <- function(X) as.numeric(predict(fit, as.matrix(X[, cechy, drop = FALSE])))
  }

  zakres <- lapply(setNames(preds, preds), function(nm)
    range(panel[[nm]], na.rm = TRUE))
  przytnij <- function(df) {
    for (nm in preds) df[[nm]] <- pmin(pmax(df[[nm]], zakres[[nm]][1]),
                                       zakres[[nm]][2])
    df
  }

  statyczne <- cechy_statyczne(panel)
  stan_t <- panel %>%
    filter(rok == rok_ostatni) %>%
    select(kod_powiatu, klasa, dep, all_of(preds), all_of(statyczne))
  stan_t1 <- stan_t %>% select(kod_powiatu) %>%
    left_join(panel %>% filter(rok == rok_ostatni - 1) %>%
                select(kod_powiatu, dep, all_of(preds)), by = "kod_powiatu")
  hist_dep <- panel %>% filter(is.finite(dep)) %>% group_by(kod_powiatu) %>%
    summarise(suma = sum(dep), n = n(), .groups = "drop")
  hist_dep <- stan_t %>% select(kod_powiatu) %>% left_join(hist_dep, by = "kod_powiatu")

  trendy <- panel %>%
    filter(rok >= rok_ostatni - 2) %>%
    group_by(kod_powiatu) %>%
    summarise(across(all_of(preds), ~ {
      ok <- is.finite(.x)
      if (sum(ok) >= 2) unname(coef(lm(.x[ok] ~ rok[ok]))[2]) else 0
    }, .names = "{.col}_trend"), .groups = "drop")
  trendy <- stan_t %>% select(kod_powiatu) %>% left_join(trendy, by = "kod_powiatu")

  xy <- if (!is.null(coords)) {
    stan_t %>% select(kod_powiatu) %>%
      left_join(coords %>% select(kod, x, y), by = c("kod_powiatu" = "kod"))
  } else data.frame(kod_powiatu = stan_t$kod_powiatu, x = 0, y = 0)

  wyniki <- list()
  for (rok in seq.int(rok_ostatni + 1, rok_koncowy)) {
    X <- data.frame(rok = rok, x = xy$x, y = xy$y,
                    dep_lag1 = stan_t$dep, dep_lag2 = stan_t1$dep,
                    sr_dep_hist = hist_dep$suma / hist_dep$n)
    for (nm in statyczne) X[[nm]] <- stan_t[[nm]]
    for (nm in preds) {
      X[[paste0(nm, "_lag1")]] <- stan_t[[nm]]
      X[[paste0(nm, "_lag2")]] <- stan_t1[[nm]]
    }
    pred <- przewidz(X)
    h <- rok - rok_ostatni
    wyniki[[length(wyniki) + 1L]] <- data.frame(
      kod_powiatu = stan_t$kod_powiatu, rok = rok, klasa = stan_t$klasa,
      prognoza_zmiany_ludnosci_1000 = pred,
      prognoza_dolna_90 = pred - q90 * sqrt(h),
      prognoza_gorna_90 = pred + q90 * sqrt(h))
    stan_t1 <- stan_t
    stan_t$dep <- pred
    hist_dep$suma <- hist_dep$suma + pred
    hist_dep$n <- hist_dep$n + 1
    for (nm in preds) stan_t[[nm]] <- stan_t[[nm]] + trendy[[paste0(nm, "_trend")]]
    stan_t <- przytnij(stan_t)
  }
  list(prognoza = bind_rows(wyniki), model = fit, model_typ = model_typ)
}


# Scenariusze interwencji (polityka regionalna).
# Liczone na MODELU DETERMINANTÓW (bez dep_lag): model prognostyczny opiera się
# niemal wyłącznie na bezwładności dep_lag, więc zmiana cech strukturalnych
# prawie nie zmienia jego prognozy (sprawdzone: efekty ~0).
# Interwencja = trwała zmiana poziomu cechy w latach 2023-2024 (lag1 i lag2);
# efekt = różnica prognozowanej zmiany ludności w 2025 r. [promile/rok].
# To symulacja na modelu predykcyjnym, NIE oszacowanie efektu przyczynowego.
SCENARIUSZE <- list(
  `lekarze +20%` = function(df) mutate(df, lekarze_10tys = lekarze_10tys * 1.2),
  `bezrobocie -2 pp` = function(df) mutate(df, bezrobocie_rejestrowane =
                                             pmax(bezrobocie_rejestrowane - 2, 0)),
  `inwestycje +5 pp` = function(df) mutate(df, udzial_wyd_inwest =
                                             udzial_wyd_inwest + 0.05),
  `kanalizacja +10 pp` = function(df) mutate(df, kanalizacja_pct =
                                               pmin(kanalizacja_pct + 10, 100)),
  # dostępność transportowa: odległość od miasta wojewódzkiego jest jedyną
  # miarą dostępności w danych; -20% to przybliżenie skrócenia czasu dojazdu
  # (np. nowa droga ekspresowa / szybsze połączenie kolejowe)
  `dostępność: odległość −20%` = function(df) {
    if ("odl_stolica_km" %in% names(df)) df$odl_stolica_km <- df$odl_stolica_km * 0.8
    df
  },
  `pakiet łączny` = function(df) {
    df <- df %>%
      mutate(lekarze_10tys = lekarze_10tys * 1.2,
             bezrobocie_rejestrowane = pmax(bezrobocie_rejestrowane - 2, 0),
             udzial_wyd_inwest = udzial_wyd_inwest + 0.05,
             kanalizacja_pct = pmin(kanalizacja_pct + 10, 100))
    if ("odl_stolica_km" %in% names(df)) df$odl_stolica_km <- df$odl_stolica_km * 0.8
    df
  }
)

analiza_scenariuszy <- function(panel, coords, typ = c("enet_progi", "xgb", "enet")) {
  typ <- match.arg(typ)
  P <- przygotuj_dane_xgb_determinanty(panel, coords)
  preds <- names(PREDYKTORY_PANEL)
  przewidz <- dopasuj_det(P$d, P$cechy, typ)
  rok_ostatni <- max(panel$rok)
  stan_t <- panel %>% filter(rok == rok_ostatni) %>%
    select(kod_powiatu, all_of(preds), all_of(cechy_statyczne(panel))) %>%
    left_join(coords %>% select(kod, x, y), by = c("kod_powiatu" = "kod"))
  stan_t1 <- stan_t %>% select(kod_powiatu) %>%
    left_join(panel %>% filter(rok == rok_ostatni - 1) %>%
                select(kod_powiatu, all_of(preds)), by = "kod_powiatu")

  zakres <- lapply(setNames(preds, preds), function(nm)
    range(panel[[nm]], na.rm = TRUE))
  predykcja <- function(s_t, s_t1) {
    X <- data.frame(rok = rok_ostatni + 1, x = stan_t$x, y = stan_t$y)
    # cechy statyczne ze stanu po scenariuszu (np. odległość w scenariuszu dostępności)
    for (nm in cechy_statyczne(s_t)) X[[nm]] <- s_t[[nm]]
    for (nm in preds) {
      X[[paste0(nm, "_lag1")]] <- pmin(pmax(s_t[[nm]], zakres[[nm]][1]), zakres[[nm]][2])
      X[[paste0(nm, "_lag2")]] <- pmin(pmax(s_t1[[nm]], zakres[[nm]][1]), zakres[[nm]][2])
    }
    przewidz(X)
  }
  baza <- predykcja(stan_t, stan_t1)
  per_powiat <- bind_rows(lapply(names(SCENARIUSZE), function(nazwa) {
    f <- SCENARIUSZE[[nazwa]]
    data.frame(kod_powiatu = stan_t$kod_powiatu, scenariusz = nazwa,
               model = MODELE_DET[[typ]], prognoza_bazowa = baza,
               prognoza_scenariusz = predykcja(f(stan_t), f(stan_t1))) %>%
      mutate(efekt_1000 = prognoza_scenariusz - prognoza_bazowa)
  })) %>% filter(is.finite(efekt_1000))
  podsumowanie <- per_powiat %>%
    group_by(scenariusz, model) %>%
    summarise(
      sredni_efekt_1000_rocznie = mean(efekt_1000),
      mediana_efektu = median(efekt_1000),
      min_efekt = min(efekt_1000),
      max_efekt = max(efekt_1000),
      odsetek_powiatow_z_poprawa = mean(efekt_1000 > 0.05),
      .groups = "drop"
    ) %>%
    arrange(desc(sredni_efekt_1000_rocznie))
  # Eksport do interaktywnego symulatora w raporcie (tylko model liniowy:
  # prognozę można wtedy policzyć w przeglądarce dokładnie tak jak w R).
  symulator <- NULL
  if (typ == "enet_progi") {
    fit <- attr(przewidz, "fit")
    b <- as.matrix(coef(fit$model, s = fit$s))
    ludnosc <- panel %>% filter(rok == rok_ostatni) %>% select(kod_powiatu, ludnosc)
    symulator <- list(
      rok = rok_ostatni + 1,
      wyraz_wolny = unname(b[1, 1]),
      wspolczynniki = as.list(setNames(b[-1, 1], rownames(b)[-1])),
      progi = attr(przewidz, "progi") %>% select(cecha, prog),
      zakres = lapply(zakres, as.numeric),
      powiaty = stan_t %>% left_join(ludnosc, by = "kod_powiatu") %>%
        mutate(kod_powiatu = as.character(kod_powiatu), prognoza_bazowa = baza),
      powiaty_t1 = stan_t1 %>% mutate(kod_powiatu = as.character(kod_powiatu)))
  }
  list(per_powiat = per_powiat, podsumowanie = podsumowanie, symulator = symulator)
}

# Istotność dźwigni w elastic net: glmnet nie daje p-wartości (kara ściąga
# współczynniki do zera), więc stosujemy bootstrap blokowy po powiatach:
# losujemy powiaty ze zwracaniem (z całą historią), dopasowujemy hybrydę od
# nowa (z doborem alfa/lambda) i liczymy efekt scenariusza na tych samych
# danych z 2024 r. Progi z ALE są ustalone z pełnej próby (wariant szybki);
# wariant z progami szacowanymi w każdej replikacji: progi_stale = FALSE.
SCENARIUSZE_DODATKOWE <- list(
  `płace +10 pp` = function(df) mutate(df, wage_rel = wage_rel + 10),
  `firmy +30%` = function(df) mutate(df, podmioty_1000 = podmioty_1000 * 1.3),
  `dochody własne +50%` = function(df) mutate(df, doch_wlasne_pc = doch_wlasne_pc * 1.5),
  `pakiet bez dostępności` = function(df) mutate(df,
    lekarze_10tys = lekarze_10tys * 1.2,
    bezrobocie_rejestrowane = pmax(bezrobocie_rejestrowane - 2, 0),
    udzial_wyd_inwest = udzial_wyd_inwest + 0.05,
    kanalizacja_pct = pmin(kanalizacja_pct + 10, 100))
)

bootstrap_scenariuszy <- function(panel, coords, B = 500, progi_stale = TRUE, seed = 1) {
  P <- przygotuj_dane_xgb_determinanty(panel, coords)
  preds <- names(PREDYKTORY_PANEL)
  rok_ostatni <- max(panel$rok)
  stan_t <- panel %>% filter(rok == rok_ostatni) %>%
    select(kod_powiatu, all_of(preds), all_of(cechy_statyczne(panel))) %>%
    left_join(coords %>% select(kod, x, y), by = c("kod_powiatu" = "kod"))
  stan_t1 <- stan_t %>% select(kod_powiatu) %>%
    left_join(panel %>% filter(rok == rok_ostatni - 1) %>%
                select(kod_powiatu, all_of(preds)), by = "kod_powiatu")
  zakres <- lapply(setNames(preds, preds), function(nm) range(panel[[nm]], na.rm = TRUE))
  X_z <- function(s_t, s_t1) {
    X <- data.frame(rok = rok_ostatni + 1, x = stan_t$x, y = stan_t$y)
    for (nm in cechy_statyczne(s_t)) X[[nm]] <- s_t[[nm]]
    for (nm in preds) {
      X[[paste0(nm, "_lag1")]] <- pmin(pmax(s_t[[nm]], zakres[[nm]][1]), zakres[[nm]][2])
      X[[paste0(nm, "_lag2")]] <- pmin(pmax(s_t1[[nm]], zakres[[nm]][1]), zakres[[nm]][2])
    }
    X
  }
  scen <- c(SCENARIUSZE, SCENARIUSZE_DODATKOWE)
  X0 <- X_z(stan_t, stan_t1)
  Xs <- lapply(scen, function(f) X_z(f(stan_t), f(stan_t1)))
  pr_pelne <- progi_ale(P$d, P$cechy)
  efekty <- function(d) {
    pr <- if (progi_stale) pr_pelne else progi_ale(d, P$cechy)
    fit <- enet_dopasuj(dodaj_progi(d, pr), c(P$cechy, paste0("prog_", pr$cecha)))
    b0 <- enet_przewidz(fit, dodaj_progi(X0, pr))
    b <- as.matrix(coef(fit$model, s = fit$s))[-1, 1]
    list(efekt = vapply(Xs, function(X) mean(enet_przewidz(fit, dodaj_progi(X, pr)) - b0, na.rm = TRUE), 0),
         wsp = b)
  }
  punkt <- efekty(P$d)
  powiaty <- unique(P$d$kod_powiatu)
  set.seed(seed)
  rep <- lapply(seq_len(B), function(i) {
    los <- sample(powiaty, replace = TRUE)
    d <- bind_rows(lapply(seq_along(los), function(j)
      P$d[P$d$kod_powiatu == los[j], ] %>% mutate(blok = j)))
    efekty(d)
  })
  E <- do.call(rbind, lapply(rep, `[[`, "efekt"))
  efekty_tab <- data.frame(scenariusz = names(scen), efekt = punkt$efekt,
    dolna_95 = apply(E, 2, quantile, 0.025), gorna_95 = apply(E, 2, quantile, 0.975),
    odsetek_dodatnich = colMeans(E > 0), row.names = NULL) %>%
    mutate(p_bootstrap = pmin(1, 2 * pmin(odsetek_dodatnich, 1 - odsetek_dodatnich)),
           istotny_5 = dolna_95 > 0 | gorna_95 < 0)
  W <- do.call(rbind, lapply(rep, function(r) r$wsp[names(punkt$wsp)]))
  W[is.na(W)] <- 0
  wsp_tab <- data.frame(zmienna = names(punkt$wsp), wspolczynnik = punkt$wsp,
    dolna_95 = apply(W, 2, quantile, 0.025), gorna_95 = apply(W, 2, quantile, 0.975),
    odsetek_niezerowych = colMeans(W != 0), odsetek_dodatnich = colMeans(W > 0),
    odsetek_ujemnych = colMeans(W < 0), row.names = NULL)
  list(efekty = efekty_tab, wspolczynniki = wsp_tab, B = B, progi_stale = progi_stale)
}

ranking_ryzyka_depopulacji <- function(prognoza) {
  wymagane <- c(
    "kod_powiatu", "klasa", "rok",
    "prognoza_zmiany_ludnosci_1000",
    "prognoza_dolna_90", "prognoza_gorna_90"
  )
  brakujace <- setdiff(wymagane, names(prognoza))
  if (length(brakujace) > 0) {
    stop("Brak kolumn do rankingu ryzyka: ",
         paste(brakujace, collapse = ", "))
  }
  if (nrow(prognoza) == 0) {
    stop("Brak prognoz do utworzenia rankingu ryzyka.")
  }

  # Ranking według skumulowanej zmiany ludności 2025-2030 (w % ludności 2024).
  # Sortowanie po dolnej granicy przedziału nic nie wnosiło - szerokość
  # przedziału jest taka sama dla wszystkich powiatów.
  prognoza %>%
    group_by(kod_powiatu, klasa) %>%
    summarise(
      skumulowana_zmiana_2025_2030_pct =
        100 * (prod(1 + prognoza_zmiany_ludnosci_1000 / 1000) - 1),
      prognoza_srednia_2025_2030 =
        mean(prognoza_zmiany_ludnosci_1000, na.rm = TRUE),
      prognoza_2030 = prognoza_zmiany_ludnosci_1000[rok == max(rok)][1],
      dolna_granica_90_2030 = prognoza_dolna_90[rok == max(rok)][1],
      gorna_granica_90_2030 = prognoza_gorna_90[rok == max(rok)][1],
      lata_z_prognoza_ujemna =
        sum(prognoza_zmiany_ludnosci_1000 < 0, na.rm = TRUE),
      najgorsza_prognoza =
        min(prognoza_zmiany_ludnosci_1000, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(skumulowana_zmiana_2025_2030_pct) %>%
    mutate(ranking_ryzyka = row_number())
}

dolacz_nazwy_powiatow <- function(ranking) {
  plik <- file.path(UPLOADS, "powiaty_mapowanie_kodow.csv")
  if (!file.exists(plik)) {
    stop("Brak pliku powiaty_mapowanie_kodow.csv.")
  }
  mapowanie <- read.csv(
    plik, stringsAsFactors = FALSE, check.names = FALSE,
    fileEncoding = "UTF-8"
  ) %>%
    transmute(
      kod_powiatu = trimws(as.character(kod_powiatu)),
      nazwa_powiatu = trimws(as.character(`JPT_NAZWA_`))
    ) %>%
    filter(nzchar(kod_powiatu), nzchar(nazwa_powiatu)) %>%
    distinct(kod_powiatu, .keep_all = TRUE)

  ranking %>%
    left_join(mapowanie, by = "kod_powiatu") %>%
    mutate(
      nazwa_powiatu = if_else(
        is.na(nazwa_powiatu) | !nzchar(nazwa_powiatu),
        paste0("kod ", kod_powiatu),
        nazwa_powiatu
      )
    ) %>%
    select(ranking_ryzyka, kod_powiatu, nazwa_powiatu, everything())
}

wykres_top_ryzyka <- function(ranking, n = 20) {
  top <- ranking %>%
    slice_head(n = n) %>%
    mutate(
      etykieta = paste0(ranking_ryzyka, ". ", nazwa_powiatu),
      etykieta = reorder(etykieta, -ranking_ryzyka)
    )
  p <- ggplot(
    top,
    aes(x = etykieta, y = skumulowana_zmiana_2025_2030_pct)
  ) +
    geom_col(width = 0.72, fill = "#b2182b") +
    geom_hline(yintercept = 0, colour = "grey35") +
    coord_flip() +
    labs(
      title = "20 powiatów o największym prognozowanym ubytku ludności",
      subtitle = "Skumulowana zmiana ludności 2025–2030 (elastic net), % stanu z 2024 r.",
      x = NULL, y = "Zmiana ludności 2025–2030 [%]"
    ) +
    theme_minimal(base_size = 10)
  ggsave(
    file.path(OUT, "top20_ryzyka_depopulacji_2030.png"), p,
    width = 10, height = 8, dpi = 150
  )
  invisible(p)
}

# ---------------------------------------------------------------------------
# MAPY GIS (granice powiatów z powiaty.geojson, PRG/geoportal)
# ---------------------------------------------------------------------------
wczytaj_granice <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Mapy wymagają pakietu 'sf': install.packages('sf').")
  }
  mapowanie <- read.csv(file.path(UPLOADS, "powiaty_mapowanie_kodow.csv"),
                        colClasses = "character", fileEncoding = "UTF-8") %>%
    transmute(JPT_KOD_JE = sprintf("%04d", as.integer(JPT_KOD_JE)),
              kod_powiatu = trimws(kod_powiatu))
  g <- sf::st_read(file.path(UPLOADS, "powiaty.geojson"), quiet = TRUE)
  g$JPT_KOD_JE <- sprintf("%04d", as.integer(g$JPT_KOD_JE))
  g <- merge(g, mapowanie, by = "JPT_KOD_JE", all.x = TRUE)
  g$nazwa <- g$JPT_NAZWA_
  sf::st_make_valid(g)
}

mapy_statyczne <- function(granice, klasy, ranking, scen) {
  motyw <- theme_void(base_size = 11) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", colour = NA))

  # (1) klasy peryferyjności - wszystkie 380 powiatów
  g1 <- merge(granice, klasy, by = "kod_powiatu", all.x = TRUE)
  p1 <- ggplot(g1) +
    geom_sf(aes(fill = klasa), colour = "white", linewidth = 0.1) +
    scale_fill_manual(values = c(peryferyjne = "#b2182b",
                                 polperyferyjne = "#f4a582",
                                 rdzeniowe = "#4393c3"),
                      labels = c(peryferyjne = "peryferie",
                                 polperyferyjne = "półperyferie",
                                 rdzeniowe = "rdzeń"),
                      na.value = "grey85", name = NULL) +
    labs(title = "Klasy peryferyjności powiatów",
         subtitle = "Indeks rdzeniowości, średnia 2011–2013, tercyle") +
    motyw
  ggsave(file.path(OUT, "mapa_klasy_peryferyjnosci.png"), p1,
         width = 8, height = 6.5, dpi = 200)

  # (2) prognoza: skumulowana zmiana ludności 2025-2030
  g2 <- merge(granice, ranking %>% select(kod_powiatu,
                                          skumulowana_zmiana_2025_2030_pct),
              by = "kod_powiatu", all.x = TRUE)
  p2 <- ggplot(g2) +
    geom_sf(aes(fill = skumulowana_zmiana_2025_2030_pct),
            colour = "white", linewidth = 0.1) +
    scale_fill_distiller(palette = "Reds", direction = -1,
                         na.value = "grey90",
                         name = "Zmiana\nludności\n2025–2030\n[%]") +
    labs(title = "Mapa przyszłości: prognoza depopulacji peryferii do 2030",
         subtitle = "Elastic net, prognoza rekurencyjna; szare = poza próbą peryferyjną") +
    motyw
  ggsave(file.path(OUT, "mapa_prognoza_2030.png"), p2,
         width = 8, height = 6.5, dpi = 200)

  # (3) scenariusz łączny: różnica względem prognozy bazowej
  if (!is.null(scen)) {
    g3 <- merge(granice,
                scen$per_powiat %>% filter(scenariusz == "pakiet łączny") %>%
                  select(kod_powiatu, efekt_1000),
                by = "kod_powiatu", all.x = TRUE)
    lim <- max(abs(g3$efekt_1000), na.rm = TRUE)
    p3 <- ggplot(g3) +
      geom_sf(aes(fill = efekt_1000), colour = "white", linewidth = 0.1) +
      scale_fill_distiller(palette = "RdBu", direction = 1,
                           limits = c(-lim, lim), na.value = "grey90",
                           name = "Różnica\nzmiany\nludności\n[‰/rok]") +
      labs(title = "Scenariusz „pakiet łączny” – symulowany efekt (elastic net + progi ALE)",
           subtitle = "lekarze +20%, bezrobocie −2 pp, inwestycje +5 pp, kanalizacja +10 pp, odległość −20%") +
      motyw
    ggsave(file.path(OUT, "mapa_scenariusz_pakiet.png"), p3,
           width = 8, height = 6.5, dpi = 200)
  }
}

mapa_interaktywna_prognoz <- function(prognoza, ranking, granice) {
  if (!requireNamespace("leaflet", quietly = TRUE) ||
      !requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("Mapa interaktywna wymaga pakietów 'leaflet' i 'htmlwidgets'.")
  }
  g <- sf::st_transform(granice, 4326)
  szer <- prognoza %>%
    select(kod_powiatu, rok, prognoza_zmiany_ludnosci_1000) %>%
    pivot_wider(names_from = rok, values_from = prognoza_zmiany_ludnosci_1000,
                names_prefix = "p_")
  g <- merge(g, ranking %>% select(kod_powiatu, ranking_ryzyka,
                                   skumulowana_zmiana_2025_2030_pct),
             by = "kod_powiatu", all.x = TRUE)
  g <- merge(g, szer, by = "kod_powiatu", all.x = TRUE)
  lata <- sort(unique(prognoza$rok))

  pal_rok <- leaflet::colorNumeric("RdBu", prognoza$prognoza_zmiany_ludnosci_1000,
                                   na.color = "#e0e0e0")
  pal_sk <- leaflet::colorNumeric("Reds", g$skumulowana_zmiana_2025_2030_pct,
                                  reverse = TRUE, na.color = "#e0e0e0")
  popup <- ifelse(
    is.na(g$ranking_ryzyka),
    paste0("<b>", g$nazwa, "</b><br>poza próbą peryferyjną"),
    paste0("<b>", g$nazwa, "</b><br>Miejsce w rankingu ryzyka: ",
           g$ranking_ryzyka,
           "<br>Zmiana ludności 2025–2030: ",
           sprintf("%.1f%%", g$skumulowana_zmiana_2025_2030_pct),
           "<br>", vapply(seq_len(nrow(g)), function(i) paste(
             sprintf("%d: %.1f‰", lata,
                     unlist(sf::st_drop_geometry(g)[i, paste0("p_", lata)])),
             collapse = "<br>"), character(1))))

  m <- leaflet::leaflet(g) %>%
    leaflet::addTiles() %>%   # OpenStreetMap (CartoDB wymaga dziś klucza API)
    leaflet::addPolygons(
      fillColor = ~pal_sk(skumulowana_zmiana_2025_2030_pct),
      fillOpacity = 0.8, color = "white", weight = 0.5,
      popup = popup, group = "2025–2030 łącznie [%]")
  for (r in lata) {
    m <- m %>% leaflet::addPolygons(
      fillColor = pal_rok(g[[paste0("p_", r)]]),
      fillOpacity = 0.8, color = "white", weight = 0.5,
      popup = popup, group = paste0(r, " [‰]"))
  }
  m <- m %>%
    leaflet::addLayersControl(
      baseGroups = c("2025–2030 łącznie [%]", paste0(lata, " [‰]")),
      options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::addLegend(pal = pal_sk, values = ~skumulowana_zmiana_2025_2030_pct,
                       title = "Zmiana 2025–2030 [%]", position = "bottomleft") %>%
    leaflet::addLegend(pal = pal_rok, values = prognoza$prognoza_zmiany_ludnosci_1000,
                       title = "Zmiana roczna [‰]", position = "bottomright")

  htmlwidgets::saveWidget(
    m, file.path(OUT, "mapa_interaktywna_prognoz_2025_2030.html"),
    selfcontained = TRUE
  )
  invisible(m)
}

uruchom_xgb_panel <- function(panel, coords = NULL) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d
  cechy <- P$cechy

  # Walidacja odpowiada rzeczywistemu zadaniu prognozowania: lata późniejsze
  # nie mogą trafić do zbioru uczącego.
  train <- d$rok <= 2021
  valid <- d$rok %in% 2022:2023
  test  <- d$rok >= 2024
  if (!any(train) || !any(valid) || !any(test)) {
    stop("Brak danych dla podziału czasowego 2021/2022-2023/2024+.")
  }

  x_train <- xgb.DMatrix(as.matrix(d[train, cechy]), label = d$dep[train])
  x_valid <- xgb.DMatrix(as.matrix(d[valid, cechy]), label = d$dep[valid])
  model <- xgb.train(
    params = XGB_MODEL_CONFIG$params,
    data = x_train,
    nrounds = max(1000L, XGB_MODEL_CONFIG$nrounds),
    evals = list(train = x_train, valid = x_valid),
    early_stopping_rounds = 50,
    verbose = 0
  )

  pred <- as.numeric(predict(model, as.matrix(d[test, cechy])))
  wynik <- d[test, c("kod_powiatu", "rok", "dep", "klasa", "dep_lag1")]
  wynik$pred_xgb <- pred
  wynik$pred_persistence <- wynik$dep_lag1

  # Uczciwy model odniesienia: liniowy model panelowy z efektami powiatów.
  # Ma dokładnie te same opóźnione informacje co XGBoost i ten sam test czasowy.
  wynik$pred_fe <- predykcja_fe(d[train, ], d[test, ], cechy)

  peryferyjne <- !is.na(wynik$klasa) & wynik$klasa == "peryferyjne"
  ocena <- data.frame(
    model = c("XGBoost panelowy", "FE panelowy", "poprzedni rok"),
    rmse = c(rmse(wynik$dep, wynik$pred_xgb),
             rmse(wynik$dep, wynik$pred_fe),
             rmse(wynik$dep, wynik$pred_persistence)),
    rmse_peryferyjne = c(
      if (any(peryferyjne)) rmse(wynik$dep[peryferyjne],
                                 wynik$pred_xgb[peryferyjne]) else NA_real_,
      if (any(peryferyjne)) rmse(wynik$dep[peryferyjne],
                                 wynik$pred_fe[peryferyjne]) else NA_real_,
      if (any(peryferyjne)) rmse(wynik$dep[peryferyjne],
                                 wynik$pred_persistence[peryferyjne]) else NA_real_
    )
  )
  xgb_rmse <- ocena$rmse[ocena$model == "XGBoost panelowy"]
  poprawa_fe <- 100 * (ocena$rmse[ocena$model == "FE panelowy"] -
                         xgb_rmse) /
    ocena$rmse[ocena$model == "FE panelowy"]
  poprawa_naive <- 100 *
    (ocena$rmse[ocena$model == "poprzedni rok"] - xgb_rmse) /
    ocena$rmse[ocena$model == "poprzedni rok"]
  ocena$poprawa_vs_fe_pct <- ifelse(
    ocena$model == "XGBoost panelowy", poprawa_fe, NA_real_
  )
  ocena$poprawa_vs_poprzedni_rok_pct <- ifelse(
    ocena$model == "XGBoost panelowy", poprawa_naive, NA_real_
  )
  q90 <- unname(quantile(abs(wynik$dep - wynik$pred_xgb),
                         probs = 0.90, na.rm = TRUE))
  list(model = model, ocena = ocena, predykcje = wynik, q90 = q90)
}

# lokalne modele + SHAP dla obserwacji danego powiatu
gwxgb_lokalne <- function(G, k) {
  bind_rows(lapply(seq_len(nrow(G$cent)), function(i) {
    m    <- dopasuj_lokalnie(G, i, k)
    test <- G$d$pidx == i
    sh   <- predict(m, as.matrix(G$d[test, CECHY_GW]), predcontrib = TRUE)
    sh   <- sh[, CECHY_GW, drop = FALSE]
    tibble(kod = G$cent$kod[i], x = G$cent$x[i], y = G$cent$y[i],
           cecha = CECHY_GW,
           mean_abs_shap = colMeans(abs(sh)),
           mean_shap     = colMeans(sh))
  }))
}

gwxgb_lokalne_permutacje <- function(G, k, n_permutacji = 99,
                                     seed = 20261024) {
  set.seed(seed)
  bind_rows(lapply(seq_len(nrow(G$cent)), function(i) {
    m <- dopasuj_lokalnie(G, i, k)
    test <- G$d$pidx == i
    dtest <- G$d[test, CECHY_GW, drop = FALSE]
    y <- G$d$dep[test]
    if (nrow(dtest) < 3) return(NULL)
    pred_base <- as.numeric(predict(m, as.matrix(dtest)))
    rmse_base <- rmse(y, pred_base)
    bind_rows(lapply(CECHY_GW, function(cecha) {
      delty <- replicate(n_permutacji, {
        x_perm <- dtest
        x_perm[[cecha]] <- sample(x_perm[[cecha]], replace = FALSE)
        rmse(y, as.numeric(predict(m, as.matrix(x_perm)))) - rmse_base
      })
      data.frame(
        kod = G$cent$kod[i],
        x = G$cent$x[i],
        y = G$cent$y[i],
        cecha = cecha,
        delta_rmse_lokalne = mean(delty),
        ci95_dolna = unname(quantile(delty, 0.025)),
        ci95_gorna = unname(quantile(delty, 0.975)),
        p_permutacyjne = (1 + sum(delty <= 0)) / (n_permutacji + 1),
        n_permutacji = n_permutacji
      )
    }))
  })) %>%
    mutate(p_fdr = p.adjust(p_permutacyjne, method = "BH"))
}

etykieta <- function(x) {
  lab <- c(PREDYKTORY, PREDYKTORY_DEMOGRAFICZNE,
           miasto = "miasto na prawach pow.", rok = "rok (trend)",
           odl_stolica_km = "odległość od miasta woj.")
  unname(lab[x])
}

# Mapy GW-XGBoost na granicach powiatów (szare = poza próbą peryferyjną).
wykresy_gw <- function(loc, granice) {
  loc <- loc %>% mutate(cecha_et = etykieta(cecha))
  tlo <- granice[, "geometry"]
  motyw <- theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold", margin = margin(b = 12)),
          plot.background = element_rect(fill = "white", colour = NA))

  # (1) lokalna ważność każdej cechy (bez trendu rocznego)
  g1 <- merge(granice, loc %>% filter(cecha != "rok") %>%
                select(kod_powiatu = kod, cecha_et, mean_abs_shap),
              by = "kod_powiatu")
  p1 <- ggplot() +
    geom_sf(data = tlo, fill = "grey92", colour = NA) +
    geom_sf(data = g1, aes(fill = mean_abs_shap), colour = NA) +
    scale_fill_viridis_c(option = "magma", direction = -1, trans = "sqrt",
                         name = "|SHAP|") +
    facet_wrap(~cecha_et, ncol = 4) +
    labs(title = "GW-XGBoost: lokalna ważność cech (średnie |SHAP|, skala pierwiastkowa)") +
    motyw
  ggsave(file.path(OUT, "rys_gwxgb_waznosc.png"), p1, width = 12, height = 9, dpi = 150)

  # (2) dominujący czynnik oraz (3) najważniejszy czynnik poza starzeniem
  mapa_dominanty <- function(dane, tytul, plik) {
    dom <- dane %>% group_by(kod) %>%
      slice_max(mean_abs_shap, n = 1, with_ties = FALSE) %>% ungroup()
    g <- merge(granice, dom %>% select(kod_powiatu = kod, cecha_et),
               by = "kod_powiatu")
    p <- ggplot() +
      geom_sf(data = tlo, fill = "grey92", colour = "white", linewidth = 0.05) +
      geom_sf(data = g, aes(fill = cecha_et), colour = "white", linewidth = 0.1) +
      scale_fill_brewer(palette = "Set2", name = NULL) +
      labs(title = tytul) + motyw
    ggsave(file.path(OUT, plik), p, width = 8.5, height = 6.5, dpi = 200)
  }
  mapa_dominanty(loc %>% filter(cecha != "rok"),
                 "GW-XGBoost: dominujący czynnik w powiecie",
                 "rys_gwxgb_dominanta.png")
  mapa_dominanty(loc %>% filter(!cecha %in% c("rok", "udzial_65_plus")),
                 "GW-XGBoost: najważniejszy czynnik poza starzeniem (65+)",
                 "rys_gwxgb_dominanta_bez65.png")
}

wykresy_artefaktow <- function(df) {
  wymagane <- c("rok", "dep_surowe", "dep", "zmiana_granic")
  brakujace <- setdiff(wymagane, names(df))
  if (length(brakujace) > 0) {
    stop("Brak kolumn do wykresów artefaktów: ",
         paste(brakujace, collapse = ", "))
  }

  rozklad <- bind_rows(
    df %>% transmute(wartosc = dep_surowe, wersja = "Przed korektą"),
    df %>% transmute(wartosc = dep, wersja = "Po korekcie")
  ) %>%
    filter(is.finite(wartosc))

  p_rozklad <- ggplot(rozklad, aes(wartosc, fill = wersja)) +
    geom_histogram(bins = 60, alpha = 0.65, colour = "white") +
    facet_wrap(~wersja, ncol = 1, scales = "free_y") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey35") +
    labs(
      x = "Zmiana ludności na 1000 mieszkańców",
      y = "Liczba obserwacji",
      title = "Rozkład zmiany ludności: przed i po ograniczeniu artefaktów"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  ggsave(file.path(OUT, "wykres_artefakty_rozkład.png"), p_rozklad,
         width = 9, height = 7, dpi = 130)

  trend <- df %>%
    group_by(rok) %>%
    summarise(
      surowa = median(dep_surowe, na.rm = TRUE),
      oczyszczona = median(dep, na.rm = TRUE),
      n_artefaktow = sum(zmiana_granic, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(c(surowa, oczyszczona),
                 names_to = "wersja", values_to = "mediana") %>%
    mutate(wersja = recode(
      wersja,
      surowa = "Przed korektą",
      oczyszczona = "Po korekcie"
    ))

  p_trend <- ggplot(trend, aes(rok, mediana, colour = wersja)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey35") +
    labs(
      x = NULL,
      y = "Mediana zmiany ludności / 1000",
      colour = NULL,
      title = "Wpływ korekty artefaktów na trend mediany"
    ) +
    theme_minimal()
  ggsave(file.path(OUT, "wykres_artefakty_trend.png"), p_trend,
         width = 9, height = 4.5, dpi = 130)

  write.csv(
    trend %>% distinct(rok, n_artefaktow),
    file.path(OUT, "artefakty_rocznie.csv"),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
}

# ---------------------------------------------------------------------------
# 7. PORÓWNANIE KLAS: RDZEŃ / PÓŁPERYFERIE / PERYFERIE (wszystkie 380 powiatów)
# ---------------------------------------------------------------------------
# Główna analiza dotyczy peryferii; ten moduł powtarza kluczowe kroki na pełnym
# panelu, żeby porównać trajektorie i determinanty między typami jednostek.
KOLORY_KLAS <- c(peryferyjne = "#b2182b", polperyferyjne = "#f4a582",
                 rdzeniowe = "#4393c3")
NAZWY_KLAS <- c(peryferyjne = "peryferie", polperyferyjne = "półperyferie",
                rdzeniowe = "rdzeń")

porownanie_klas <- function(panel_all, coords, granice) {
  # (a) trajektorie depopulacji
  trend <- panel_all %>%
    group_by(rok, klasa) %>%
    summarise(mediana_dep = median(dep, na.rm = TRUE),
              udzial_spadkow = mean(dep < 0, na.rm = TRUE),
              .groups = "drop")
  write.csv(trend, file.path(OUT, "klasy_trend.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  p <- ggplot(trend, aes(rok, mediana_dep, colour = klasa)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_line(linewidth = 1.1) + geom_point() +
    scale_colour_manual(values = KOLORY_KLAS, labels = NAZWY_KLAS, name = NULL) +
    labs(title = "Mediana rocznej zmiany ludności według klasy peryferyjności",
         x = NULL, y = "zmiana ludności / 1000 mieszk.") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(OUT, "rys_klasy_trend.png"), p, width = 9, height = 4.8, dpi = 150)

  # (b) panel FE osobno dla każdej klasy; standaryzacja na wspólnej próbie,
  # więc współczynniki są porównywalne między klasami
  m <- przygotuj_macierz(panel_all)
  m$d$klasa <- panel_all$klasa[match(m$d$kod_powiatu, panel_all$kod_powiatu)]
  rhs <- paste(m$zpreds, collapse = " + ")
  fe_klasy <- bind_rows(lapply(levels(m$d$klasa), function(k) {
    mod <- feols(as.formula(paste("dep ~", rhs, "| kod_powiatu + rok")),
                 data = m$d[m$d$klasa == k, ], cluster = ~kod_powiatu)
    data.frame(klasa = k, zmienna = sub("_z$", "", names(coef(mod))),
               beta = unname(coef(mod)), se = unname(se(mod)),
               p = unname(pvalue(mod)), r2_within = r2(mod, "wr2"),
               n = nobs(mod))
  }))
  write.csv(fe_klasy, file.path(OUT, "klasy_fe_wspolczynniki.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  # (c) walidacja krocząca na pełnym panelu, błąd raportowany wg klas
  P <- przygotuj_dane_xgb_panel(panel_all, coords)
  d_k <- dodaj_sr_dep_hist(P$d, panel_all)
  cechy_en <- c(P$cechy, "sr_dep_hist")
  bledy <- bind_rows(lapply(2019:2024, function(rok_test) {
    train <- d_k %>% filter(rok < rok_test)
    test <- d_k %>% filter(rok == rok_test)
    data.frame(kod_powiatu = test$kod_powiatu, rok = rok_test,
               klasa = test$klasa, dep = test$dep,
               `XGBoost` = predykcja_xgb(train, test, P$cechy),
               `Elastic net` = enet_przewidz(enet_dopasuj(train, cechy_en), test),
               `FE panelowy` = predykcja_fe(train, test, P$cechy),
               `poprzedni rok` = test$dep_lag1, check.names = FALSE)
  }))
  ocena <- bledy %>%
    pivot_longer(c(`XGBoost`, `Elastic net`, `FE panelowy`, `poprzedni rok`),
                 names_to = "model", values_to = "pred") %>%
    group_by(klasa, model) %>%
    summarise(rmse = rmse(dep, pred), mae = mae(dep, pred), n = n(),
              .groups = "drop") %>%
    arrange(klasa, rmse)
  write.csv(ocena, file.path(OUT, "klasy_ocena_modeli.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  # (d) prognoza 2025-2030 dla wszystkich powiatów (model uczony na całości).
  # Model wybierany według łącznego RMSE na pełnym panelu: XGBoost vs elastic net.
  rmse_xgb <- rmse(bledy$dep, bledy$XGBoost)
  rmse_en <- rmse(bledy$dep, bledy$`Elastic net`)
  typ_all <- if (rmse_en <= rmse_xgb) "enet" else "xgb"
  kol <- if (typ_all == "enet") "Elastic net" else "XGBoost"
  q90 <- unname(quantile(abs(bledy$dep - bledy[[kol]]), 0.9, type = 8))
  prog <- prognoza_do_2030(panel_all, coords, q90 = q90, model_typ = typ_all)$prognoza
  message(sprintf("Prognoza 380 powiatów: %s (RMSE XGBoost %.3f, elastic net %.3f)",
                  kol, rmse_xgb, rmse_en))
  rank_all <- dolacz_nazwy_powiatow(ranking_ryzyka_depopulacji(prog))
  write.csv(rank_all, file.path(OUT, "prognoza_wszystkie_powiaty_2030.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  podsum <- rank_all %>%
    group_by(klasa) %>%
    summarise(powiaty = n(),
              srednia_zmiana_2025_2030_pct = mean(skumulowana_zmiana_2025_2030_pct),
              mediana_zmiana_2025_2030_pct = median(skumulowana_zmiana_2025_2030_pct),
              odsetek_ubytek_ponad_5pct = mean(skumulowana_zmiana_2025_2030_pct < -5),
              odsetek_wzrost = mean(skumulowana_zmiana_2025_2030_pct > 0),
              .groups = "drop")
  write.csv(podsum, file.path(OUT, "klasy_prognoza_podsumowanie.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  g <- merge(granice, rank_all %>% select(kod_powiatu, skumulowana_zmiana_2025_2030_pct),
             by = "kod_powiatu", all.x = TRUE)
  lim <- max(abs(g$skumulowana_zmiana_2025_2030_pct), na.rm = TRUE)
  pm <- ggplot(g) +
    geom_sf(aes(fill = skumulowana_zmiana_2025_2030_pct), colour = "white",
            linewidth = 0.1) +
    scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-lim, lim),
                         na.value = "grey85",
                         name = "Zmiana\nludności\n2025–2030\n[%]") +
    labs(title = "Mapa przyszłości: prognoza zmiany ludności do 2030 – wszystkie powiaty",
         subtitle = paste0(kol, " uczony na 380 powiatach; czerwony = ubytek, niebieski = wzrost")) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(OUT, "mapa_prognoza_2030_polska.png"), pm,
         width = 8.5, height = 6.5, dpi = 200)

  list(trend = trend, fe = fe_klasy, ocena = ocena, podsumowanie = podsum, model = kol,
       prognoza = rank_all)
}

# ---------------------------------------------------------------------------
# 7a. TEST ODPORNOŚCI: PORÓWNANIE MODELI ML (peryferie, walidacja 2019-2024)
# ---------------------------------------------------------------------------
# FE ma przewagę konstrukcyjną: osobną stałą dla każdego powiatu. Warianty
# poniżej sprawdzają, czy jakikolwiek model ML istotnie ją pokonuje:
# - XGBoost dostrojony: losowe przeszukanie 200 konfiguracji, walidacja
#   krocząca na latach 2017-2018 (przed okresem testowym),
# - XGBoost "within" (hybryda FE): cel i cechy pomniejszone o średnie powiatu
#   z lat uczących, XGBoost modeluje odchylenia, prognoza = średnia + odchylenie,
# - sieć neuronowa MLP (nnet), strojona na tym samym oknie, średnia z 5 startów,
# - Geographical Random Forest (Georganos i in. 2019): las lokalny na 30
#   najbliższych powiatach zmieszany 50/50 z lasem globalnym,
# - Random Forest, elastic net, ensemble FE + XGBoost.
# Istotność względem FE: panelowy test Diebolda-Mariano (regresja różnicy
# kwadratów błędów na stałą, SE klastrowane po powiecie) + korekta Holma
# na wielokrotne porównania.
LATA_STROJENIA <- 2017:2018

strojenie_xgb_losowe <- function(d, cechy) {
  xgb_random_search(d, cechy, plik = "strojenie_xgb_losowe.csv")
}

mlp_dopasuj <- function(train, test, cechy, size, decay, n_startow = 5) {
  mu <- colMeans(train[, cechy]); sdv <- apply(train[, cechy], 2, sd)
  sdv[!is.finite(sdv) | sdv == 0] <- 1
  sk <- function(df) scale(as.matrix(df[, cechy]), center = mu, scale = sdv)
  my <- mean(train$dep); sy <- sd(train$dep)
  xtr <- sk(train); xte <- sk(test)
  rowMeans(sapply(seq_len(n_startow), function(s) {
    set.seed(s)
    m <- nnet::nnet(xtr, (train$dep - my) / sy, size = size, decay = decay,
                    linout = TRUE, maxit = 1000, trace = FALSE, MaxNWts = 5000)
    as.numeric(predict(m, xte)) * sy + my
  }))
}

strojenie_mlp <- function(d, cechy, n_konfiguracji = 20, seed = 2026) {
  set.seed(seed)
  siatka <- data.frame(size = sample(1:12, n_konfiguracji, replace = TRUE),
                       decay = signif(exp(runif(n_konfiguracji, log(0.001), log(10))), 2))
  siatka$rmse_walidacja <- vapply(seq_len(nrow(siatka)), function(i) {
    mean(vapply(LATA_STROJENIA, function(r) {
      tr <- d[d$rok < r, ]; te <- d[d$rok == r, ]
      rmse(te$dep, mlp_dopasuj(tr, te, cechy, siatka$size[i], siatka$decay[i], 3))
    }, numeric(1)))
  }, numeric(1))
  siatka[which.min(siatka$rmse_walidacja), ]
}

# Random Forest: random search (mtry, min.node.size, sample.fraction)
rf_dopasuj <- function(train, test, cechy, cfg, n_drzew = 500) {
  m <- ranger::ranger(x = as.data.frame(train[, cechy]), y = train$dep,
                      num.trees = n_drzew, mtry = min(cfg$mtry, length(cechy)),
                      min.node.size = cfg$min_node_size,
                      sample.fraction = cfg$sample_fraction,
                      seed = 1, num.threads = NTHREAD)
  as.numeric(predict(m, as.data.frame(test[, cechy]))$predictions)
}

strojenie_rf <- function(d, cechy, n_konfiguracji = 20, seed = 2026) {
  set.seed(seed)
  p <- length(cechy)
  siatka <- data.frame(
    mtry = sample(seq(2, p), n_konfiguracji, replace = TRUE),
    min_node_size = sample(c(1, 3, 5, 10, 20, 40), n_konfiguracji, replace = TRUE),
    sample_fraction = round(runif(n_konfiguracji, 0.5, 1), 2))
  siatka$rmse_walidacja <- vapply(seq_len(nrow(siatka)), function(i) {
    mean(vapply(LATA_STROJENIA, function(r) {
      tr <- d[d$rok < r, ]; te <- d[d$rok == r, ]
      rmse(te$dep, rf_dopasuj(tr, te, cechy, siatka[i, ], n_drzew = 300))
    }, numeric(1)))
  }, numeric(1))
  siatka[which.min(siatka$rmse_walidacja), ]
}

# Geographical RF: random search (liczba sąsiadów k, waga modelu lokalnego,
# min.node.size lasów)
strojenie_grf <- function(d, cechy, n_konfiguracji = 10, seed = 2026) {
  set.seed(seed)
  siatka <- data.frame(
    k = sample(c(15, 20, 30, 50, 80), n_konfiguracji, replace = TRUE),
    waga_lokalna = sample(c(0.25, 0.5, 0.75), n_konfiguracji, replace = TRUE),
    min_node_size = sample(c(3, 5, 10, 20), n_konfiguracji, replace = TRUE))
  siatka$rmse_walidacja <- vapply(seq_len(nrow(siatka)), function(i) {
    mean(vapply(LATA_STROJENIA, function(r) {
      tr <- d[d$rok < r, ]; te <- d[d$rok == r, ]
      rmse(te$dep, grf_predykcja(tr, te, cechy, k = siatka$k[i],
                                 waga_lokalna = siatka$waga_lokalna[i],
                                 min_node_size = siatka$min_node_size[i]))
    }, numeric(1)))
  }, numeric(1))
  siatka[which.min(siatka$rmse_walidacja), ]
}

xgb_within <- function(train, test, cechy, cfg) {
  zmienne <- setdiff(cechy, c("rok", "x", "y", "odl_stolica_km", "sr_dep_hist"))
  srednie <- train %>% group_by(kod_powiatu) %>%
    summarise(across(all_of(c("dep", zmienne)), mean, .names = "m_{.col}"),
              .groups = "drop")
  odchyl <- function(df) {
    df <- left_join(df, srednie, by = "kod_powiatu")
    for (v in zmienne) df[[v]] <- df[[v]] - df[[paste0("m_", v)]]
    df
  }
  tr <- odchyl(train); te <- odchyl(test)
  tr$dep <- tr$dep - tr$m_dep
  cechy_w <- c("rok", zmienne)
  m <- xgb.train(params = cfg$params, nrounds = cfg$nrounds, verbose = 0,
                 data = xgb.DMatrix(as.matrix(tr[, cechy_w]), label = tr$dep))
  te$m_dep + as.numeric(predict(m, as.matrix(te[, cechy_w])))
}

grf_predykcja <- function(train, test, cechy, k = 30, waga_lokalna = 0.5,
                          min_node_size = 5) {
  rf <- function(df, n) ranger::ranger(x = as.data.frame(df[, cechy]), y = df$dep,
                                       num.trees = n, min.node.size = min_node_size,
                                       seed = 1, num.threads = NTHREAD)
  glob <- rf(train, 500)
  p_glob <- as.numeric(predict(glob, as.data.frame(test[, cechy]))$predictions)
  cent <- train %>% distinct(kod_powiatu, x, y)
  p_lok <- p_glob
  for (kod in unique(test$kod_powiatu)) {
    ti <- which(test$kod_powiatu == kod)
    d2 <- (cent$x - test$x[ti[1]])^2 + (cent$y - test$y[ti[1]])^2
    sasiedzi <- cent$kod_powiatu[order(d2)[seq_len(min(k, nrow(cent)))]]
    lok <- rf(train[train$kod_powiatu %in% sasiedzi, ], 200)
    p_lok[ti] <- as.numeric(predict(lok, as.data.frame(test[ti, cechy]))$predictions)
  }
  waga_lokalna * p_lok + (1 - waga_lokalna) * p_glob
}

# Odporność wyboru modelu prognozy. Lata testowe 2019-2024 posłużyły też do
# wyboru modelu, więc jego błąd na tych latach jest lekko optymistyczny.
# (1) wybór zagnieżdżony: w roku r wybieramy model o najniższym średnim RMSE
#     z lat testowych < r i oceniamy go w roku r (błąd całej procedury);
# (2) zbiór odłożony: wybór na latach 2019-2022, ocena na 2023-2024.
ocena_wyboru_modelu <- function(pred) {
  pred <- pred %>% mutate(kod_powiatu = as.character(kod_powiatu), e2 = (dep - pred)^2)
  roczne <- pred %>% group_by(model, rok) %>% summarise(rmse = sqrt(mean(e2)), .groups = "drop")
  lata <- sort(unique(pred$rok))
  wybor <- bind_rows(lapply(lata[-1], function(r) {
    h <- roczne %>% filter(rok < r) %>% group_by(model) %>% summarise(rmse_hist = mean(rmse), .groups = "drop") %>%
      arrange(rmse_hist)
    data.frame(rok = r, wybrany = h$model[1], rmse_lat_wczesniejszych = h$rmse_hist[1],
               rmse_w_roku = roczne$rmse[roczne$model == h$model[1] & roczne$rok == r])
  }))
  proc <- bind_rows(lapply(seq_len(nrow(wybor)), function(i)
    pred %>% filter(rok == wybor$rok[i], model == wybor$wybrany[i]))) %>%
    mutate(model = "Procedura: wybór zagnieżdżony")
  E <- bind_rows(pred %>% filter(rok %in% wybor$rok), proc)
  W <- E %>% select(kod_powiatu, rok, model, e2) %>% tidyr::pivot_wider(names_from = model, values_from = e2)
  dm <- function(a, b, dane) {
    dane$r <- dane[[a]] - dane[[b]]
    m <- fixest::feols(r ~ 1, data = dane, cluster = ~kod_powiatu)
    c(roznica = unname(coef(m)[1]), p = unname(fixest::pvalue(m)[1]))
  }
  tab_z <- E %>% group_by(model, rok) %>% summarise(r = sqrt(mean(e2)), .groups = "drop") %>%
    group_by(model) %>% summarise(rmse = mean(r), .groups = "drop") %>% arrange(rmse)
  d_fe <- dm("Procedura: wybór zagnieżdżony", "FE panelowy", W)
  d_en <- dm("Procedura: wybór zagnieżdżony", "Elastic net", W)
  attr_z <- data.frame(porownanie = c("procedura vs FE panelowy", "procedura vs Elastic net"),
                       srednia_roznica = c(d_fe[1], d_en[1]), p = c(d_fe[2], d_en[2]))
  sel <- roczne %>% filter(rok <= 2022) %>% group_by(model) %>% summarise(rmse_2019_2022 = mean(rmse), .groups = "drop")
  ho <- roczne %>% filter(rok >= 2023) %>% group_by(model) %>% summarise(rmse_2023_2024 = mean(rmse), .groups = "drop")
  holdout <- left_join(sel, ho, by = "model") %>% arrange(rmse_2019_2022)
  W2 <- pred %>% filter(rok >= 2023) %>% select(kod_powiatu, rok, model, e2) %>%
    tidyr::pivot_wider(names_from = model, values_from = e2)
  d_ho <- dm(holdout$model[1], "FE panelowy", W2)
  write.csv(wybor, file.path(OUT, "wybor_modelu_zagniezdzony.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(tab_z, file.path(OUT, "wybor_modelu_zagniezdzony_rmse.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(rbind(attr_z, data.frame(porownanie = paste(holdout$model[1], "vs FE panelowy (2023-2024)"),
                                     srednia_roznica = d_ho[1], p = d_ho[2])),
            file.path(OUT, "wybor_modelu_testy.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(holdout, file.path(OUT, "wybor_modelu_holdout_2023_2024.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(list(wybor = wybor, zagniezdzony = tab_z, holdout = holdout))
}

porownanie_modeli_ml <- function(panel, coords, lata_testowe = 2019:2024,
                                 sdm = NULL) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  # średnia zmiany ludności powiatu z lat WCZEŚNIEJSZYCH (bez wycieku)
  hist <- panel %>% arrange(kod_powiatu, rok) %>% group_by(kod_powiatu) %>%
    mutate(sr_dep_hist = dplyr::lag(cummean(dep))) %>% ungroup() %>%
    select(kod_powiatu, rok, sr_dep_hist)
  d <- P$d %>% left_join(hist, by = c("kod_powiatu", "rok")) %>%
    filter(is.finite(sr_dep_hist))
  cechy <- P$cechy
  cechy_sr <- c(cechy, "sr_dep_hist")
  # cechy SDM (W·X, W·y z t-1, sąsiedztwo graniczne) - połączenie XGBoost i SDM
  cechy_w <- character(0)
  if (!is.null(sdm)) {
    cechy_w <- setdiff(names(sdm), c("kod_powiatu", "rok"))
    d <- d %>% left_join(sdm, by = c("kod_powiatu", "rok")) %>%
      filter(if_all(all_of(cechy_w), is.finite))
  }

  cfg_xgb <- strojenie_xgb_losowe(d, cechy)
  cfg_mlp <- strojenie_mlp(d, cechy_sr)
  message(sprintf("MLP: size = %d, decay = %g", cfg_mlp$size, cfg_mlp$decay))
  cfg_rf <- strojenie_rf(d, cechy_sr)
  cfg_grf <- strojenie_grf(d, cechy_sr)
  # alpha elastic net wybrana w oknie strojenia (tylko do raportu; w każdym
  # oknie walidacji alpha i lambda dobierane są na ostatnim roku uczącym)
  en_s <- enet_dopasuj(d %>% filter(rok <= max(LATA_STROJENIA)), cechy_sr)
  strojenie_tab <- data.frame(
    model = c("XGBoost dostrojony", "Random Forest", "Geographical Random Forest",
              "Sieć neuronowa MLP", "Elastic net"),
    metoda = c("random search", "random search", "random search", "random search",
               "siatka α × pełna ścieżka λ"),
    konfiguracje = c(nrow(cfg_xgb$siatka), 20, 10, 20,
                     paste(length(ENET_ALPHA), "× ok. 100")),
    wybrane = c(
      with(cfg_xgb$siatka[1, ], sprintf("max_depth=%d, eta=%.4f, min_child_weight=%.2f, subsample=%.2f, colsample=%.2f, lambda=%.3f, alpha=%.3f, gamma=%.2f, nrounds=%d",
                                        as.integer(max_depth), eta, min_child_weight, subsample, colsample_bytree, lambda, alpha, gamma, as.integer(nrounds))),
      with(cfg_rf, sprintf("mtry=%d, min.node.size=%d, sample.fraction=%.2f", mtry, min_node_size, sample_fraction)),
      with(cfg_grf, sprintf("k=%d sąsiadów, waga lokalna=%.2f, min.node.size=%d", k, waga_lokalna, min_node_size)),
      with(cfg_mlp, sprintf("neurony=%d, decay=%g", size, decay)),
      sprintf("alpha=%.1f, lambda=%.4f", en_s$alpha, en_s$s)),
    rmse_walidacja_2017_2018 = c(cfg_xgb$siatka$rmse_walidacja[1], cfg_rf$rmse_walidacja,
                                 cfg_grf$rmse_walidacja, cfg_mlp$rmse_walidacja, NA))
  write.csv(strojenie_tab, file.path(OUT, "strojenie_modeli_ml.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  xgb_cfg <- function(train, test, cechy, cfg) {
    m <- xgb.train(params = cfg$params, nrounds = cfg$nrounds, verbose = 0,
                   data = xgb.DMatrix(as.matrix(train[, cechy]), label = train$dep))
    as.numeric(predict(m, as.matrix(test[, cechy])))
  }
  pred_rf <- function(train, test, cechy) rf_dopasuj(train, test, cechy, cfg_rf)
  # alpha i lambda dobierane na ostatnim roku uczącym (bez mieszania lat)
  pred_enet <- function(train, test, cechy) enet_przewidz(enet_dopasuj(train, cechy), test)
  # Hybryda: progi z ALE wyznaczane na modelu determinantów (XGBoost) WYŁĄCZNIE
  # z lat uczących danego okna, dodane do elastic net jako cechy łamane.
  d_det <- przygotuj_dane_xgb_determinanty(panel, coords)
  pred_hybryda <- function(train, test, cechy, rok_test) {
    pr <- progi_ale(d_det$d %>% filter(rok < rok_test), d_det$cechy)
    pred_enet(dodaj_progi(train, pr), dodaj_progi(test, pr),
              c(cechy, paste0("prog_", pr$cecha)))
  }

  pred <- bind_rows(lapply(lata_testowe, function(rok_test) {
    train <- d %>% filter(rok < rok_test)
    test <- d %>% filter(rok == rok_test)
    p_fe <- predykcja_fe(train, test, cechy)
    p_xgb_sr <- predykcja_xgb(train, test, cechy_sr)
    p <- list(
      `FE panelowy` = p_fe,
      `XGBoost` = predykcja_xgb(train, test, cechy),
      `XGBoost dostrojony (random search)` = xgb_cfg(train, test, cechy, cfg_xgb),
      `XGBoost + średnia powiatu` = p_xgb_sr,
      `XGBoost within (hybryda FE)` = xgb_within(train, test, cechy, cfg_xgb),
      `Sieć neuronowa MLP` = mlp_dopasuj(train, test, cechy_sr,
                                         cfg_mlp$size, cfg_mlp$decay),
      `Random Forest` = pred_rf(train, test, cechy_sr),
      `Geographical Random Forest` = grf_predykcja(train, test, cechy_sr, k = cfg_grf$k,
                                                   waga_lokalna = cfg_grf$waga_lokalna,
                                                   min_node_size = cfg_grf$min_node_size),
      `Elastic net` = pred_enet(train, test, cechy_sr),
      `Hybryda: elastic net + progi z ALE` = pred_hybryda(train, test, cechy_sr, rok_test),
      `Ensemble (FE + XGBoost)/2` = (p_fe + p_xgb_sr) / 2,
      `poprzedni rok` = test$dep_lag1
    )
    if (length(cechy_w) > 0) {
      p[["XGBoost + cechy SDM (W·X, W·y)"]] <-
        predykcja_xgb(train, test, c(cechy_sr, cechy_w))
      p[["Elastic net + cechy SDM (W·X, W·y)"]] <-
        pred_enet(train, test, c(cechy_sr, cechy_w))
    }
    bind_rows(lapply(names(p), function(nm)
      data.frame(kod_powiatu = test$kod_powiatu, rok = rok_test,
                 dep = test$dep, model = nm, pred = p[[nm]])))
  }))
  write.csv(pred, file.path(OUT, "porownanie_modeli_ml_predykcje.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  ref <- pred %>% filter(model == "FE panelowy") %>%
    select(kod_powiatu, rok, p_fe = pred)
  dm <- pred %>% filter(model != "FE panelowy") %>%
    left_join(ref, by = c("kod_powiatu", "rok")) %>%
    mutate(roznica = (dep - pred)^2 - (dep - p_fe)^2) %>%
    group_by(model) %>%
    group_modify(~ {
      t <- fixest::feols(roznica ~ 1, data = .x, cluster = ~kod_powiatu)
      data.frame(dm_srednia_roznica = unname(coef(t)), dm_p = unname(pvalue(t)))
    }) %>% ungroup() %>%
    mutate(dm_p_holm = p.adjust(dm_p, method = "holm"))
  # bezpośrednie porównanie hybrydy z czystym elastic net (bez korekty - jedno
  # z góry zaplanowane porównanie)
  hyb <- pred %>% filter(model %in% c("Elastic net", "Hybryda: elastic net + progi z ALE")) %>%
    select(kod_powiatu, rok, dep, model, pred) %>%
    tidyr::pivot_wider(names_from = model, values_from = pred)
  hyb$roznica <- (hyb$dep - hyb[["Hybryda: elastic net + progi z ALE"]])^2 -
    (hyb$dep - hyb[["Elastic net"]])^2
  t_h <- fixest::feols(roznica ~ 1, data = hyb, cluster = ~kod_powiatu)
  write.csv(data.frame(porownanie = "Hybryda vs elastic net (prognoza)",
                       srednia_roznica = unname(coef(t_h)), p = unname(pvalue(t_h)),
                       lat_lepsza = sum(tapply(hyb$roznica, hyb$rok, mean) < 0)),
            file.path(OUT, "prognoza_dm_hybryda_vs_enet.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  roczne <- pred %>% group_by(model, rok) %>%
    summarise(r = rmse(dep, pred), .groups = "drop")
  r_fe <- roczne %>% filter(model == "FE panelowy") %>% select(rok, r_fe = r)
  tabela <- roczne %>% left_join(r_fe, by = "rok") %>%
    group_by(model) %>%
    summarise(rmse_srednie = mean(r), rmse_sd = sd(r),
              lat_lepszy_od_FE = sum(r < r_fe), .groups = "drop") %>%
    left_join(dm, by = "model") %>%
    arrange(rmse_srednie)
  write.csv(tabela, file.path(OUT, "porownanie_modeli_ml.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  ocena_wyboru_modelu(pred)
  attr(tabela, "mlp") <- cfg_mlp
  attr(tabela, "predykcje") <- pred
  tabela
}

# ---------------------------------------------------------------------------
# 7b. EKONOMETRIA PRZESTRZENNA: testy LM, panelowe SAR / SEM / SARAR / SDM / SDEM
# ---------------------------------------------------------------------------
# Pełny panel 380 powiatów (zbalansowany), sąsiedztwo graniczne (queen)
# z powiaty.geojson, macierz W standaryzowana wierszami. Ta sama specyfikacja
# co FE: predyktory z t-1, standaryzowane, efekty stałe powiatu i roku
# (wszystkie modele przestrzenne są wersjami "FE": model = "within", twoways).
# Nazewnictwo splm: "lambda" = opóźnienie przestrzenne y (rho w podręcznikach),
# "rho" = autokorelacja błędu (lambda w podręcznikach) - w wynikach ujednolicone.
# Sąsiedztwo graniczne (queen) dla podanych kodów, w kolejności `kody`.
# Układ PL-1992 (płaski): bez geometrii sferycznej s2, która odrzuca drobne
# błędy topologii w granicach z geoportalu. Powiat bez sąsiada (wyspa)
# dostaje najbliższy powiat jako sąsiada.
sasiedztwo_graniczne <- function(kody, granice) {
  g <- sf::st_make_valid(sf::st_transform(
    granice[match(kody, granice$kod_powiatu), ], 2180))
  nb <- spdep::poly2nb(g, queen = TRUE, snap = 10)
  bez_sasiadow <- which(spdep::card(nb) == 0)
  if (length(bez_sasiadow) > 0) {
    knn <- spdep::knn2nb(spdep::knearneigh(sf::st_coordinates(
      sf::st_centroid(sf::st_geometry(g))), k = 1))
    for (i in bez_sasiadow) nb[[i]] <- knn[[i]]
  }
  lw <- spdep::nb2listw(nb, style = "W")
  list(nb = nb, lw = lw, Wm = spdep::listw2mat(lw))
}

# Cechy w stylu SDM dla modeli ML: średnie z SĄSIADÓW GRANICZNYCH (wszyscy
# sąsiedzi, także spoza peryferii) z roku t-1 dla predyktorów (W·X) i zmiany
# ludności (W·y). Wartości z t-1 są znane w chwili prognozy - brak wycieku.
# Średnia liczona po sąsiadach z dostępnymi danymi.
cechy_sdm <- function(panel_all, granice) {
  kody <- sort(intersect(unique(panel_all$kod_powiatu), granice$kod_powiatu))
  A <- (sasiedztwo_graniczne(kody, granice)$Wm > 0) * 1
  zmienne <- c(names(PREDYKTORY_PANEL), "dep")
  lata <- sort(unique(panel_all$rok))
  wynik <- bind_rows(lapply(lata, function(r) {
    dr <- panel_all %>% filter(rok == r)
    out <- data.frame(kod_powiatu = kody, rok = r + 1)   # wartość z r -> cecha dla r+1
    for (v in zmienne) {
      x <- dr[[v]][match(kody, dr$kod_powiatu)]
      ok <- is.finite(x)
      x0 <- ifelse(ok, x, 0)
      out[[paste0("W_", v, "_lag1")]] <- as.numeric(A %*% x0) / pmax(as.numeric(A %*% ok), 1)
      out[[paste0("W_", v, "_lag1")]][as.numeric(A %*% ok) == 0] <- NA_real_
    }
    out
  }))
  wynik
}

ekonometria_przestrzenna <- function(panel_all, granice) {
  m <- przygotuj_macierz(panel_all)
  d <- m$d
  lata <- sort(unique(d$rok))
  pelne <- d %>% count(kod_powiatu) %>% filter(n == length(lata)) %>% pull(kod_powiatu)
  kody <- sort(intersect(pelne, granice$kod_powiatu))
  d <- d %>% filter(kod_powiatu %in% kody) %>% arrange(rok, kod_powiatu) %>%
    as.data.frame()

  S <- sasiedztwo_graniczne(kody, granice)
  nb <- S$nb; lw <- S$lw; Wm <- S$Wm

  # przestrzenne opóźnienia predyktorów WX (rok po roku; kolejność = kody)
  wx <- paste0("W_", m$zpreds)
  for (j in seq_along(m$zpreds)) {
    d[[wx[j]]] <- NA_real_
    for (r in lata) {
      i <- which(d$rok == r)
      d[[wx[j]]][i] <- as.numeric(Wm %*% d[[m$zpreds[j]]][i])
    }
  }

  f <- as.formula(paste("dep ~", paste(m$zpreds, collapse = " + ")))
  f_wx <- as.formula(paste("dep ~", paste(c(m$zpreds, wx), collapse = " + ")))
  idx <- c("kod_powiatu", "rok")

  # (1) Moran I reszt FE (powiat + rok) w każdym roku - test dwustronny,
  # bo zależność może być ujemna
  fe <- feols(as.formula(paste("dep ~", paste(m$zpreds, collapse = " + "),
                               "| kod_powiatu + rok")), data = d)
  d$reszta_fe <- resid(fe)
  moran_fe <- bind_rows(lapply(lata, function(r) {
    t <- spdep::moran.test(d$reszta_fe[d$rok == r], lw, alternative = "two.sided")
    data.frame(rok = r, moran_i = unname(t$estimate[1]), p = t$p.value)
  }))

  # (2) testy LM dla panelu z efektami stałymi
  lm_testy <- bind_rows(lapply(c("lml", "lme", "rlml", "rlme"), function(tt) {
    r <- tryCatch(splm::slmtest(f, data = d, listw = lw, test = tt,
                                model = "within", effect = "twoways",
                                index = idx),
                  error = function(e) NULL)
    if (is.null(r)) return(data.frame(test = tt, statystyka = NA, p = NA))
    data.frame(test = tt, statystyka = unname(r$statistic), p = r$p.value)
  })) %>%
    mutate(opis = c(lml = "LM-lag (SAR)", lme = "LM-error (SEM)",
                    rlml = "odporny LM-lag", rlme = "odporny LM-error")[test])

  # (3) estymacja ML panelowych modeli przestrzennych z efektami stałymi
  spec <- list(
    SAR   = list(f = f,    lag = TRUE,  err = "none"),
    SEM   = list(f = f,    lag = FALSE, err = "b"),
    SARAR = list(f = f,    lag = TRUE,  err = "b"),
    SDM   = list(f = f_wx, lag = TRUE,  err = "none"),
    SDEM  = list(f = f_wx, lag = FALSE, err = "b"))
  modele <- lapply(spec, function(s)
    splm::spml(s$f, data = d, index = idx, listw = lw, model = "within",
               effect = "twoways", lag = s$lag, spatial.error = s$err))
  wsp <- bind_rows(lapply(names(modele), function(nm) {
    ct <- summary(modele[[nm]])$CoefTable
    data.frame(model = nm, zmienna = sub("_z$", "", rownames(ct)),
               beta = ct[, 1], se = ct[, 2], p = ct[, 4], row.names = NULL)
  })) %>%
    mutate(zmienna = case_when(
      zmienna == "lambda" ~ "rho_W_y (opóźnienie przestrzenne)",
      zmienna == "rho" ~ "lambda_W_e (błąd przestrzenny)",
      TRUE ~ sub("^W_(.*)_z$", "W_\\1", zmienna)))
  wsp <- bind_rows(
    data.frame(model = "FE", zmienna = sub("_z$", "", names(coef(fe))),
               beta = unname(coef(fe)), se = unname(se(fe)), p = unname(pvalue(fe))),
    wsp)

  # (4) test Walda: czy SDM redukuje się do SAR (wszystkie theta = 0)?
  wald <- tryCatch({
    mod <- modele$SDM
    b <- mod$coefficients; V <- as.matrix(mod$vcov)
    # splm nie nadaje nazw macierzy kowariancji; bez parametru lambda,
    # jeśli ten jest przechowywany osobno (arcoef)
    nm_v <- if (nrow(V) == length(b)) names(b) else setdiff(names(b), "lambda")
    dimnames(V) <- list(nm_v, nm_v)
    th <- intersect(wx, nm_v)
    stat <- as.numeric(t(b[th]) %*% solve(V[th, th]) %*% b[th])
    data.frame(test = "Wald: theta (WX) = 0, SDM -> SAR", chi2 = stat,
               df = length(th), p = pchisq(stat, length(th), lower.tail = FALSE))
  }, error = function(e) { message("Wald SDM: ", conditionMessage(e)); NULL })

  # (5) efekty bezpośrednie / pośrednie / całkowite (LeSage & Pace 2009)
  # SAR: S = (I - rho W)^-1, bezpośredni = beta * śr(diag S), całkowity = beta/(1-rho)
  # SDM: bezpośredni = beta * śr(diag S) + theta * śr(diag S W),
  #      całkowity = (beta + theta)/(1-rho)
  # Istotność: Monte Carlo, 500 losowań z niezależnych rozkładów normalnych
  # oszacowań (przybliżenie; spatialreg::impacts nie obsługuje obiektów splm).
  efekty <- function(nazwa) {
    ct <- summary(modele[[nazwa]])$CoefTable
    i_rho <- which(rownames(ct) == "lambda")
    i_b <- match(m$zpreds, rownames(ct))
    i_t <- match(wx, rownames(ct))
    ma_theta <- all(!is.na(i_t))
    licz <- function(rho, beta, theta) {
      S <- solve(diag(nrow(Wm)) - rho * Wm)
      dS <- mean(diag(S)); dSW <- mean(diag(S %*% Wm))
      if (is.null(theta)) theta <- 0 * beta
      bezp <- beta * dS + theta * dSW
      tot <- (beta + theta) / (1 - rho)
      cbind(bezposredni = bezp, posredni = tot - bezp, calkowity = tot)
    }
    los <- function(i) rnorm(length(i), ct[i, 1], ct[i, 2])
    est <- licz(ct[i_rho, 1], ct[i_b, 1], if (ma_theta) ct[i_t, 1] else NULL)
    set.seed(1)
    sym <- replicate(500, licz(los(i_rho), los(i_b), if (ma_theta) los(i_t) else NULL))
    p_mc <- apply(sym, c(1, 2), function(x) min(1, 2 * min(mean(x > 0), mean(x < 0))))
    data.frame(model = nazwa, zmienna = sub("_z$", "", m$zpreds), est,
               p_bezposredni = p_mc[, 1], p_posredni = p_mc[, 2],
               p_calkowity = p_mc[, 3], row.names = NULL)
  }
  imp <- tryCatch(bind_rows(efekty("SAR"), efekty("SDM")),
                  error = function(e) { message("Impacts: ", conditionMessage(e)); NULL })

  write.csv(moran_fe, file.path(OUT, "przestrzenne_moran_reszt_FE.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(lm_testy, file.path(OUT, "przestrzenne_testy_LM.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(wsp, file.path(OUT, "przestrzenne_modele_SAR_SEM.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(wald)) write.csv(wald, file.path(OUT, "przestrzenne_test_Walda_SDM.csv"),
                                row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(imp)) write.csv(imp, file.path(OUT, "przestrzenne_impacts.csv"),
                               row.names = FALSE, fileEncoding = "UTF-8")
  list(moran = moran_fe, lm = lm_testy, wsp = wsp, wald = wald, imp = imp,
       n_powiatow = length(kody), sasiedzi_sr = mean(spdep::card(nb)))
}

# ---------------------------------------------------------------------------
# 7c. NIELINIOWOŚCI: ALE (Accumulated Local Effects) dla modelu determinantów
# ---------------------------------------------------------------------------
# ALE (Apley i Zhu 2020) zamiast PDP: przy skorelowanych cechach (np. udział
# 65+ i gęstość) PDP uśrednia po nierealistycznych kombinacjach wartości.
# Zmiana cechy przesuwa jednocześnie jej lag1 i lag2 (poziom zmiennej), żeby
# efekt nie rozkładał się sztucznie między dwa opóźnienia.
# Nieliniowość: R² dopasowania liniowego do krzywej ALE oraz model łamany
# z jednym punktem załamania (próg) - nachylenie przed i po progu.
ale_cecha <- function(model, X, zmienna, n_bins = 10) {
  v1 <- if (paste0(zmienna, "_lag1") %in% names(X)) paste0(zmienna, "_lag1") else zmienna
  v2 <- paste0(zmienna, "_lag2")
  ma_lag2 <- v2 %in% names(X)
  krawedzie <- unique(unname(quantile(X[[v1]], seq(0, 1, length.out = n_bins + 1))))
  kosz <- cut(X[[v1]], krawedzie, include.lowest = TRUE, labels = FALSE)
  przewidz <- function(D) as.numeric(predict(model, as.matrix(D)))
  K <- length(krawedzie) - 1
  delta <- numeric(K); n <- integer(K)
  for (k in seq_len(K)) {
    i <- which(kosz == k); n[k] <- length(i)
    if (n[k] == 0) next
    ustaw <- function(wartosc) {
      D <- X[i, , drop = FALSE]
      przes <- wartosc - D[[v1]]
      D[[v1]] <- wartosc
      if (ma_lag2) D[[v2]] <- D[[v2]] + przes
      D
    }
    delta[k] <- mean(przewidz(ustaw(krawedzie[k + 1])) - przewidz(ustaw(krawedzie[k])))
  }
  ale <- c(0, cumsum(delta))
  srodki <- (ale[-1] + ale[-length(ale)]) / 2
  ale <- ale - sum(n * srodki) / sum(n)
  data.frame(cecha = zmienna, x = krawedzie, ale = ale, n_w_koszu = c(n, NA))
}

ocena_nieliniowosci <- function(krzywa) {
  x <- krzywa$x; y <- krzywa$ale
  lin <- lm(y ~ x)
  r2_lin <- if (var(y) > 0) summary(lin)$r.squared else 1
  kandydaci <- x[-c(1, 2, length(x) - 1, length(x))]
  najlepszy <- NULL
  for (b in kandydaci) {
    m <- lm(y ~ x + pmax(x - b, 0))
    if (is.null(najlepszy) || deviance(m) < deviance(najlepszy$m)) najlepszy <- list(m = m, b = b)
  }
  cf <- coef(najlepszy$m)
  data.frame(cecha = krzywa$cecha[1],
             zakres_efektu = diff(range(y)),
             r2_liniowe = r2_lin,
             prog = najlepszy$b,
             nachylenie_przed = cf[2], nachylenie_po = cf[2] + cf[3],
             r2_lamane = if (var(y) > 0) summary(najlepszy$m)$r.squared else 1,
             row.names = NULL)
}

analiza_nieliniowosci <- function(panel, coords) {
  P <- przygotuj_dane_xgb_determinanty(panel, coords)
  model <- xgb.train(params = cfg_det_add()$params, nrounds = cfg_det_add()$nrounds,
                     verbose = 0,
                     data = xgb.DMatrix(as.matrix(P$d[, P$cechy]), label = P$d$dep))
  X <- as.data.frame(P$d[, P$cechy])
  zmienne <- c(names(PREDYKTORY_PANEL), cechy_statyczne(P$d))
  krzywe <- bind_rows(lapply(zmienne, function(v) ale_cecha(model, X, v)))
  ocena <- bind_rows(lapply(split(krzywe, krzywe$cecha), ocena_nieliniowosci)) %>%
    arrange(desc(zakres_efektu))
  write.csv(krzywe, file.path(OUT, "nieliniowosci_ale.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(ocena, file.path(OUT, "nieliniowosci_ocena.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  kol <- krzywe %>% mutate(etykieta = factor(etykieta(cecha),
                                             levels = etykieta(ocena$cecha)))
  progi <- ocena %>% filter(r2_liniowe < 0.9) %>%
    mutate(etykieta = factor(etykieta(cecha), levels = levels(kol$etykieta)))
  p <- ggplot(kol, aes(x, ale)) +
    geom_hline(yintercept = 0, colour = "grey60") +
    geom_vline(data = progi, aes(xintercept = prog), linetype = "dashed",
               colour = "#b2182b") +
    geom_line(linewidth = 1, colour = "#2166ac") + geom_point(size = 1.3, colour = "#2166ac") +
    facet_wrap(~etykieta, scales = "free_x", ncol = 4) +
    labs(title = "Nieliniowości: efekty ALE w modelu determinantów (XGBoost, peryferie)",
         subtitle = "Oś Y: wpływ na roczną zmianę ludności [‰]; czerwona linia = wykryty próg (gdy R² liniowe < 0,9)",
         x = "wartość cechy (rok t−1)", y = "efekt ALE [‰]") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(OUT, "rys_nieliniowosci_ale.png"), p, width = 12, height = 7.5, dpi = 150)
  ocena
}

# ---------------------------------------------------------------------------
# 7d. WALIDACJA PRZESTRZENNO-CZASOWA (bloki przestrzenne)
# ---------------------------------------------------------------------------
# Walidacja losowa zawyża jakość modelu, gdy obserwacje sąsiednich powiatów
# i kolejnych lat są do siebie podobne. Porównujemy trzy schematy:
#  - losowy (10 foldów po obserwacjach) - punkt odniesienia, optymistyczny,
#  - przestrzenny blokowy: 10 zwartych regionów (k-means na centroidach),
#    model nie widzi żadnej obserwacji z testowanego regionu,
#  - przestrzenno-czasowy: uczenie na pozostałych regionach i latach < 2019,
#    test na wyłączonym regionie w latach 2019-2024.
# FE nie uczestniczy - nie może prognozować powiatów nieobecnych w uczeniu.
walidacja_przestrzenna <- function(panel, coords, granice, n_blokow = 10) {
  P <- przygotuj_dane_xgb_panel(panel, coords)
  d <- P$d; cechy <- P$cechy
  set.seed(1)
  cent <- d %>% distinct(kod_powiatu, x, y)
  cent$blok <- kmeans(cent[, c("x", "y")], n_blokow, nstart = 25)$cluster
  d$blok <- cent$blok[match(d$kod_powiatu, cent$kod_powiatu)]
  d$fold_losowy <- sample(rep(seq_len(n_blokow), length.out = nrow(d)))

  modele <- list(
    `XGBoost` = function(tr, te) predykcja_xgb(tr, te, cechy),
    `Elastic net` = function(tr, te) enet_przewidz(enet_dopasuj(tr, cechy), te),
    `OLS (pooled)` = function(tr, te) {
      m <- lm(as.formula(paste("dep ~", paste(cechy, collapse = " + "))), data = tr)
      as.numeric(predict(m, te))
    },
    `poprzedni rok` = function(tr, te) te$dep_lag1
  )
  schematy <- list(
    `losowy (obserwacje)` = function(k) list(tr = d$fold_losowy != k, te = d$fold_losowy == k),
    `przestrzenny blokowy` = function(k) list(tr = d$blok != k, te = d$blok == k),
    `przestrzenno-czasowy` = function(k) list(tr = d$blok != k & d$rok < 2019,
                                              te = d$blok == k & d$rok >= 2019)
  )
  pred <- bind_rows(lapply(names(schematy), function(s) {
    bind_rows(lapply(seq_len(n_blokow), function(k) {
      idx <- schematy[[s]](k)
      tr <- d[idx$tr, ]; te <- d[idx$te, ]
      if (nrow(te) == 0) return(NULL)
      bind_rows(lapply(names(modele), function(mn)
        data.frame(schemat = s, model = mn, dep = te$dep, pred = modele[[mn]](tr, te))))
    }))
  }))
  wynik <- pred %>% group_by(schemat, model) %>%
    summarise(rmse = rmse(dep, pred), mae = mae(dep, pred), n = n(), .groups = "drop")
  write.csv(wynik, file.path(OUT, "walidacja_przestrzenna.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  g <- merge(granice, cent %>% select(kod_powiatu, blok), by = "kod_powiatu")
  p <- ggplot() +
    geom_sf(data = granice[, "geometry"], fill = "grey92", colour = NA) +
    geom_sf(data = g, aes(fill = factor(blok)), colour = "white", linewidth = 0.1) +
    scale_fill_brewer(palette = "Paired", name = "blok") +
    labs(title = "Bloki przestrzenne walidacji (k-means na centroidach powiatów)") +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(OUT, "mapa_bloki_walidacji.png"), p, width = 7.5, height = 6, dpi = 150)
  wynik
}

# ---------------------------------------------------------------------------
# 7e. POTENCJAŁ ROZWOJOWY: typologia trajektorii (wszystkie 380 powiatów)
# ---------------------------------------------------------------------------
# Dwa wymiary:
#  - demograficzny: prognozowana skumulowana zmiana ludności 2025-2030
#    (XGBoost na pełnym panelu); "szybka depopulacja" = poniżej mediany kraju,
#  - gospodarczy: trend RELATYWNEJ pozycji powiatu 2014-2024 - średnie
#    nachylenie z-score'ów (liczonych w każdym roku względem wszystkich
#    powiatów) płac względem kraju, dochodów własnych pc, firm na 1000
#    mieszk. i bezrobocia (ze znakiem minus). >0 = powiat dogania resztę kraju.
# Wymiar gospodarczy opiera się na obserwowanej trajektorii, a nie na prognozie:
# zmienne nominalne (dochody) rosną w czasie, a drzewa nie ekstrapolują trendu.
typologia_rozwoju <- function(panel_all, prognoza_all, granice) {
  z <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)
  rel <- panel_all %>%
    filter(rok >= 2014) %>%
    group_by(rok) %>%
    mutate(z_place = z(wage_rel), z_dochody = z(log(doch_wlasne_pc)),
           z_firmy = z(podmioty_1000), z_bezrob = -z(bezrobocie_rejestrowane)) %>%
    ungroup()
  nachylenie <- function(y, t) {
    ok <- is.finite(y)
    if (sum(ok) < 4) NA_real_ else unname(coef(lm(y[ok] ~ t[ok]))[2])
  }
  trend <- rel %>% group_by(kod_powiatu, klasa) %>%
    summarise(across(c(z_place, z_dochody, z_firmy, z_bezrob),
                     ~ nachylenie(.x, rok), .names = "tr_{.col}"),
              .groups = "drop") %>%
    mutate(trend_gospodarczy = rowMeans(across(starts_with("tr_")), na.rm = TRUE))
  typ <- trend %>%
    inner_join(prognoza_all %>% select(kod_powiatu, nazwa_powiatu,
                                       skumulowana_zmiana_2025_2030_pct),
               by = "kod_powiatu") %>%
    mutate(
      mediana_dep = median(skumulowana_zmiana_2025_2030_pct),
      szybka_depop = skumulowana_zmiana_2025_2030_pct < mediana_dep,
      poprawa = trend_gospodarczy > 0,
      typ = case_when(
        szybka_depop & !poprawa ~ "spirala marginalizacji",
        szybka_depop & poprawa ~ "kurczenie z poprawą gospodarczą",
        !szybka_depop & !poprawa ~ "ryzyko stagnacji",
        TRUE ~ "potencjał rozwojowy"),
      typ = factor(typ, levels = c("spirala marginalizacji", "ryzyko stagnacji",
                                   "kurczenie z poprawą gospodarczą",
                                   "potencjał rozwojowy")))
  write.csv(typ, file.path(OUT, "typologia_rozwoju.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  tab <- typ %>% count(klasa, typ) %>% group_by(klasa) %>%
    mutate(udzial = n / sum(n)) %>% ungroup()
  write.csv(tab, file.path(OUT, "typologia_rozwoju_klasy.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  g <- merge(granice, typ %>% select(kod_powiatu, typ), by = "kod_powiatu", all.x = TRUE)
  p <- ggplot(g) +
    geom_sf(aes(fill = typ), colour = "white", linewidth = 0.1) +
    scale_fill_manual(values = c(`spirala marginalizacji` = "#b2182b",
                                 `ryzyko stagnacji` = "#f4a582",
                                 `kurczenie z poprawą gospodarczą` = "#92c5de",
                                 `potencjał rozwojowy` = "#2166ac"),
                      na.value = "grey85", name = NULL, drop = FALSE) +
    labs(title = "Typologia trajektorii rozwoju powiatów",
         subtitle = "Prognoza depopulacji 2025–2030 (vs mediana kraju) × trend pozycji gospodarczej 2014–2024") +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(OUT, "mapa_typologia_rozwoju.png"), p, width = 9.8, height = 6.5, dpi = 200)
  list(typ = typ, tab = tab)
}

# ---------------------------------------------------------------------------
# 8. AUDYT BRAKÓW I ANALIZA WRAŻLIWOŚCI
# ---------------------------------------------------------------------------
audyt_pokrycia_zrodel <- function() {
  eko <- read_excel(file.path(UPLOADS, "dataset.xlsx")) %>%
    transmute(kod_powiatu = trimws(as.character(kod_powiatu)),
              rok = as.integer(rok))
  demo <- read_excel(file.path(UPLOADS, "dataset_demografia_depopulacja.xlsx")) %>%
    transmute(kod_powiatu = trimws(as.character(kod_powiatu)),
              rok = as.integer(rok))
  ml <- wczytaj_panel_ml()

  klucze_eko <- distinct(eko)
  klucze_demo <- distinct(demo)
  klucze_ml <- select(ml, all_of(KEY))
  wszystkie <- full_join(
    distinct(klucze_eko, across(all_of(KEY)), obecne_eko = TRUE),
    distinct(klucze_demo, across(all_of(KEY)), obecne_demo = TRUE),
    by = KEY
  ) %>%
    full_join(
      distinct(klucze_ml, across(all_of(KEY)), obecne_ml = TRUE),
      by = KEY
    ) %>%
    mutate(across(starts_with("obecne_"), ~ replace_na(.x, FALSE)),
           status = case_when(
             !obecne_eko & obecne_demo ~ "tylko_demografia",
             obecne_eko & !obecne_demo ~ "tylko_ekonomia",
             obecne_eko & obecne_demo & !obecne_ml ~ "brak_w_panelu_ml",
             TRUE ~ "wspolny_klucz"
           ))

  podsumowanie <- bind_rows(
    data.frame(zrodlo = "dataset.xlsx",
               wiersze = nrow(eko),
               unikalne_klucze = nrow(klucze_eko),
               duplikaty_klucza = nrow(eko) - nrow(klucze_eko)),
    data.frame(zrodlo = "dataset_demografia_depopulacja.xlsx",
               wiersze = nrow(demo),
               unikalne_klucze = nrow(klucze_demo),
               duplikaty_klucza = nrow(demo) - nrow(klucze_demo)),
    data.frame(zrodlo = "dml_panel_ml.csv",
               wiersze = nrow(ml),
               unikalne_klucze = nrow(klucze_ml),
               duplikaty_klucza = nrow(ml) - nrow(klucze_ml))
  )
  write.csv(podsumowanie, file.path(OUT, "audyt_pokrycia_zrodel.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(filter(wszystkie, status != "wspolny_klucz"),
            file.path(OUT, "brakujace_klucze_zrodel.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  invisible(podsumowanie)
}

imputuj_lekarzy_interpolacja <- function(panel) {
  panel %>%
    arrange(kod_powiatu, rok) %>%
    group_by(kod_powiatu) %>%
    mutate(
      lekarze_10tys_brak = is.na(lekarze_10tys),
      lekarze_10tys = {
        ok <- !is.na(lekarze_10tys)
        if (sum(ok) >= 2) {
          wartosc <- lekarze_10tys
          # Tylko pojedyncza luka pomiędzy dwiema obserwacjami
          # tego samego powiatu; braków na krańcach szeregu nie uzupełniamy.
          wewnatrz <- is.na(wartosc) &
            !is.na(dplyr::lag(wartosc)) &
            !is.na(dplyr::lead(wartosc))
          if (any(wewnatrz)) {
            wartosc[wewnatrz] <- approx(
              x = rok[ok], y = wartosc[ok], xout = rok[wewnatrz],
              rule = 1
            )$y
          }
          wartosc
        } else {
          lekarze_10tys
        }
      },
      lekarze_10tys_imputed = lekarze_10tys_brak &
        !is.na(lekarze_10tys)
    ) %>%
    ungroup() %>%
    select(-lekarze_10tys_brak)
}

raportuj_zakresy_modeli <- function(panel, d, d_panel) {
  data.frame(
    zbior = c("panel_clean", "regresja_FE", "modele_panelowe_XGBoost"),
    obserwacje = c(nrow(panel), nrow(d), nrow(d_panel)),
    powiaty = c(n_distinct(panel$kod_powiatu),
                n_distinct(d$kod_powiatu),
                n_distinct(d_panel$kod_powiatu)),
    rok_min = c(min(panel$rok), min(d$rok), min(d_panel$rok)),
    rok_max = c(max(panel$rok), max(d$rok), max(d_panel$rok)),
    uwaga = c(
      "Po agregacji duplikatów; bez imputacji.",
      "Complete-case dla zmiennych regresji; główny wynik.",
      "Complete-case po utworzeniu opóźnień; pierwsze dwa lata odpadają."
    )
  ) %>%
    write.csv(file.path(OUT, "zakresy_modeli.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
}

analiza_wrazliwosci_lekarze <- function(panel) {
  panel_imp <- imputuj_lekarzy_interpolacja(panel)
  m0 <- przygotuj_macierz(panel)
  m1 <- przygotuj_macierz(panel_imp)
  model0 <- feols(
    as.formula(paste("dep ~", paste(m0$zpreds, collapse = " + "),
                     "| kod_powiatu + rok")),
    data = m0$d, cluster = ~kod_powiatu
  )
  model1 <- feols(
    as.formula(paste("dep ~", paste(m1$zpreds, collapse = " + "),
                     "| kod_powiatu + rok")),
    data = m1$d, cluster = ~kod_powiatu
  )
  porownanie <- data.frame(
    wariant = c("bez imputacji", "interpolacja lekarzy"),
    obserwacje = c(nrow(m0$d), nrow(m1$d)),
    rok_min = c(min(m0$d$rok), min(m1$d$rok)),
    rok_max = c(max(m0$d$rok), max(m1$d$rok)),
    lekarze_imputowane = c(0L,
                           sum(panel_imp$lekarze_10tys_imputed, na.rm = TRUE)),
    beta_lekarze = c(coef(model0)[["lekarze_10tys_z"]],
                     coef(model1)[["lekarze_10tys_z"]]),
    p_lekarze = c(pvalue(model0)[["lekarze_10tys_z"]],
                  pvalue(model1)[["lekarze_10tys_z"]])
  )
  write.csv(porownanie, file.path(OUT, "analiza_wrazliwosci_lekarze.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  invisible(porownanie)
}


# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------
main <- function() {
  panel <- dodaj_zmienne(wczytaj_i_oczysc())
  panel <- wzbogac_panel_ml(panel)
  coords <- wczytaj_wspolrzedne()
  if (!is.null(coords)) {
    panel <- left_join(panel, odleglosc_od_stolic(coords), by = "kod_powiatu")
  }
  # Klasyfikację wyznaczamy na pełnym panelu (380 powiatów, lata bazowe),
  # a dalsze analizy prowadzimy dla najniższego tercyla rdzeniowości.
  klasy <- sklasyfikuj_peryferyjnosc(panel)
  write.csv(klasy, file.path(OUT, "klasy_peryferyjnosci.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  # Wartość zmiany ludności dla 2010 r. jest niespójna z szeregiem
  # ludności, dlatego rok bazowy wyłączamy przed każdą analizą.
  panel_all <- left_join(panel, klasy, by = "kod_powiatu") %>%
    filter(rok >= 2011)
  panel <- panel_all %>%
    filter(klasa == "peryferyjne") %>%
    droplevels()
  granice <- wczytaj_granice()
  audyt_pokrycia_zrodel()
  raport_jakosci_danych(panel)
  wykresy_artefaktow(panel)
  write.csv(panel, file.path(OUT, "panel_clean.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  m      <- przygotuj_macierz(panel)
  d      <- m$d; zpreds <- m$zpreds
  d_panel <- przygotuj_dane_xgb_panel(panel, coords = NULL)$d
  raportuj_zakresy_modeli(panel, d, d_panel)
  analiza_wrazliwosci_lekarze(panel)
  strojenie <- strojenie_xgb_panel(panel, coords = NULL)
  XGB_MODEL_CONFIG <<- strojenie$config
  P_det <- przygotuj_dane_xgb_determinanty(panel, coords)
  rs_det <- xgb_random_search(P_det$d, P_det$cechy, plik = "strojenie_xgb_determinanty.csv")
  XGB_DET_CONFIG <<- rs_det[c("params", "nrounds")]
  hp <- c("max_depth", "eta", "min_child_weight", "subsample", "colsample_bytree", "lambda", "alpha", "gamma")
  s1 <- rs_det$siatka[rs_det$siatka$max_depth == 1, ][1, ]
  XGB_DET_ADD_CONFIG <<- list(params = modifyList(XGB_PARAMS, as.list(s1[hp])), nrounds = as.integer(s1$nrounds))
  write.csv(s1, file.path(OUT, "strojenie_xgb_determinanty_addytywny.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(strojenie$wyniki,
            file.path(OUT, "strojenie_xgb_peryferie.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  pooled <- regresja_laczna(d, zpreds)
  pan    <- modele_panelowe(d, zpreds)
  fe     <- pan$fe; h <- pan$haus
  wykres_trendu(panel)
  wsp_tab <- function(model, nazwa, r2) data.frame(
    model = nazwa, zmienna = sub("_z$", "", names(coef(model))),
    beta = unname(coef(model)), se = unname(se(model)),
    p = unname(pvalue(model)), r2 = r2)
  write.csv(rbind(wsp_tab(pooled, "pooled OLS", r2(pooled, "r2")),
                  wsp_tab(fe, "FE (powiat + rok)", r2(fe, "wr2"))),
            file.path(OUT, "wspolczynniki_regresji.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  nazwa <- function(cc) if (cc == "miasto") "miasto na prawach pow." else {
    lab <- c(PREDYKTORY, PREDYKTORY_DEMOGRAFICZNE)
    unname(lab[sub("_z$", "", cc)])
  }
  tab <- function(model, vars) {
    beta <- coef(model)
    pv <- pvalue(model)
    vapply(vars, function(cc) {
      if (!(cc %in% names(beta))) {
        return(sprintf("%-22s%9s%8s", nazwa(cc), "NA", "NA"))
      }
      sprintf("%-22s%+9.3f%8.3f", nazwa(cc), beta[[cc]], pv[[cc]])
    }, character(1))
  }
  hdr <- sprintf("%-22s%9s%8s", "predyktor", "beta", "p")

  linie <- c(
    sprintf("Panel po czyszczeniu: %d obserwacji | powiatów: %d | lata: %d-%d",
            nrow(panel), n_distinct(panel$kod_powiatu),
            min(panel$rok), max(panel$rok)),
    sprintf("Klasyfikacja: indeks rdzeniowości ze średniej %d-%d, tercyle z %d powiatów",
            min(LATA_BAZOWE), max(LATA_BAZOWE), nrow(klasy)),
    sprintf("Regresja główna: %d obserwacji | powiatów: %d | lata: %d-%d | bez imputacji",
            nrow(d), n_distinct(d$kod_powiatu), min(d$rok), max(d$rok)),
    "Predyktory w regresji: wartości z roku t-1, standaryzowane (beta = efekt 1 SD).",
    "=== 1. REGRESJA ŁĄCZNA (pooled OLS + FE lat, SE klastr. po powiecie) ===",
    sprintf("R^2 = %.3f\n", r2(pooled, "r2")),
    hdr, tab(pooled, zpreds),
    "\n=== 2. PANEL FIXED EFFECTS (within: powiat + rok) ===",
    sprintf("R^2 (within) = %.3f\n", r2(fe, "wr2")),
    hdr, tab(fe, zpreds),
    "\n=== 3. TEST HAUSMANA (FE vs RE) ===",
    sprintf("chi2 = %.1f | df = %d | p = %.4f",
            h$statistic, as.integer(h$parameter), h$p.value),
    paste("Wniosek:", if (h$p.value < .05)
      "p<0.05 -> wybieramy FE (model RE obciążony)." else
        "brak podstaw do odrzucenia RE.")
  )

  # ---- GW-XGBoost (wymaga współrzędnych centroidów powiatów) ----
  gw_importance_test <- NULL
  if (is.null(coords)) {
    message("Brak centroidy_powiatow.csv - pomijam GW-XGBoost.")
  } else {
    G <- przygotuj_gw(panel, coords)
    message(sprintf("GW-XGBoost: %d powiatów, %d obserwacji", nrow(G$cent), nrow(G$d)))

    cv   <- wybierz_pasmo(G)
    k_op <- with(cv[is.finite(cv$k), ], k[which.min(rmse_cv)])   # najlepsze skończone k
    loc  <- gwxgb_lokalne(G, k_op)
    write.csv(loc, file.path(OUT, "gwxgb_lokalne_shap.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    # 99 permutacji: przy 19 minimalne p = 0,05, więc po korekcie FDR
    # żaden test nie mógł wyjść istotny niezależnie od danych.
    gw_importance_test <- gwxgb_lokalne_permutacje(
      G, k_op, n_permutacji = 99)
    write.csv(gw_importance_test,
              file.path(OUT, "gwxgb_lokalne_testy_permutacyjne.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    wykresy_gw(loc, granice)

    glob <- loc %>% group_by(cecha) %>%
      summarise(m = mean(mean_abs_shap), .groups = "drop") %>% arrange(desc(m))
    linie <- c(linie,
      "\n=== 4. GW-XGBOOST (leave-location-out CV) ===",
      sprintf("%-10s%10s", "k sąsiadów", "RMSE CV"),
      sprintf("%-10s%10.4f", ifelse(is.finite(cv$k), cv$k, "global"), cv$rmse_cv),
      sprintf("Wybrane pasmo: k = %d", k_op),
      "\nŚrednia lokalna ważność cech (|SHAP|):",
      sprintf("%-26s%8.4f", etykieta(glob$cecha), glob$m),
      "\nTesty lokalne: zapisano delta RMSE, CI 95%, p permutacyjne i p FDR.")
  }

  # ---- Panelowy XGBoost: prognoza przyszłych lat ----
  xgb_panel <- uruchom_xgb_panel(panel, coords)
  write.csv(xgb_panel$ocena, file.path(OUT, "xgb_panel_ocena.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(xgb_panel$predykcje, file.path(OUT, "xgb_panel_predykcje.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  ocena_xgb <- xgb_panel$ocena
  linie <- c(linie,
    "\n=== 5. PANELOWY XGBOOST (test czasowy: 2024+) ===",
    "Model korzysta wyłącznie z opóźnień predyktorów i zmiennej zależnej.",
    sprintf("%-20s RMSE: %.4f", ocena_xgb$model, ocena_xgb$rmse),
    "Uwaga: pojedynczy rok testowy - miarodajne jest porównanie kroczące w sekcji 6.")

  # ---- Porównanie modeli na kolejnych, niewidzianych latach ----
  benchmark <- ocena_modeli_panelowych(panel, coords, lata_testowe = 2019:2024)
  write.csv(benchmark, file.path(OUT, "porownanie_modeli_panelowych.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(attr(benchmark, "wyniki_roczne"),
            file.path(OUT, "porownanie_modeli_panelowych_lata.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  warianty_xgb <- ocena_xgb_determinantow(panel, coords,
                                          lata_testowe = 2019:2024)
  write.csv(warianty_xgb, file.path(OUT, "porownanie_xgb_demograficzny_strukturalny.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(attr(warianty_xgb, "wyniki_roczne"),
            file.path(OUT, "porownanie_xgb_demograficzny_strukturalny_lata.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(attr(warianty_xgb, "dm"), file.path(OUT, "determinanty_dm.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(attr(warianty_xgb, "progi"), file.path(OUT, "progi_ale_stabilnosc.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  inter <- ocena_interakcji(panel, coords)
  write.csv(inter$ranking %>% mutate(udzial_interakcji_ogolem = inter$udzial),
            file.path(OUT, "interakcje_ranking.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(inter$walidacja, file.path(OUT, "interakcje_walidacja.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(inter$pary_okna, file.path(OUT, "interakcje_pary_okna.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  waznosc_por <- porownanie_waznosci_modeli(panel, coords)
  write.csv(waznosc_por, file.path(OUT, "waznosc_cech_enet_vs_xgb.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(attr(waznosc_por, "spearman"),
            file.path(OUT, "waznosc_cech_spearman.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  wykres_waznosci_porownanie(waznosc_por)
  importance_xgb <- ocena_permutacyjna_determinantow(
    panel, coords, lata_testowe = 2019:2024, n_permutacji = 3)
  write.csv(importance_xgb,
            file.path(OUT, "xgb_permutation_importance.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(attr(importance_xgb, "wyniki_roczne"),
            file.path(OUT, "xgb_permutation_importance_lata.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  importance_bootstrap <- bootstrap_istotnosci_importance(
    attr(importance_xgb, "wyniki_roczne"), n_boot = 1000)
  write.csv(importance_bootstrap,
            file.path(OUT, "xgb_importance_bootstrap.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  wykres_waznosci_xgb(importance_xgb)
  antyprzeuczenie <- walidacja_antyprzeuczeniowa_xgb(
    panel, coords, lata_testowe = 2019:2024)
  write.csv(
    antyprzeuczenie$podsumowanie,
    file.path(OUT, "walidacja_antyprzeuczeniowa_xgb.csv"),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  write.csv(
    antyprzeuczenie$foldy,
    file.path(OUT, "walidacja_antyprzeuczeniowa_xgb_lata.csv"),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  kalibracja <- kalibracja_przedzialow_xgb(
    panel, coords, lata_testowe = 2019:2024, poziom = 0.90)
  write.csv(kalibracja$kalibratory,
            file.path(OUT, "kalibracja_przedzialow_xgb.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(kalibracja$pokrycie,
            file.path(OUT, "pokrycie_przedzialow_xgb.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(kalibracja$bledy,
            file.path(OUT, "bledy_kalibracji_xgb.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  kalibracja_h <- kalibracja_horyzontow_xgb(
    panel, coords, lata_kalibracyjne = 2016:2018,
    lata_oceny = 2019:2021,
    horyzonty = 1:3, poziom = 0.90)
  write.csv(kalibracja_h$kalibratory,
            file.path(OUT, "kalibracja_horyzontow_xgb.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(kalibracja_h$pokrycie,
            file.path(OUT, "pokrycie_horyzontow_xgb.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(kalibracja_h$bledy,
            file.path(OUT, "bledy_horyzontow_xgb.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  if (is.null(coords)) {
    message("Brak centroidow - pomijam diagnostyke autokorelacji przestrzennej.")
    przestrzenna <- NULL
    przestrzenny_xgb <- NULL
    klastry_lokalne <- NULL
  } else {
    przestrzenna <- ocena_autokorelacji_przestrzennej(
      panel, coords, lata_testowe = 2019:2024, k = 8,
      n_permutacji = 199)
    write.csv(przestrzenna,
              file.path(OUT, "autokorelacja_przestrzenna_xgb.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    przestrzenny_xgb <- ocena_xgb_przestrzennego(
      panel, coords, lata_testowe = 2019:2024, k = 8)
    write.csv(przestrzenny_xgb,
              file.path(OUT, "porownanie_xgb_przestrzenny.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    write.csv(attr(przestrzenny_xgb, "wyniki_roczne"),
              file.path(OUT, "porownanie_xgb_przestrzenny_lata.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
    klastry_lokalne <- diagnostyka_klastrow_lokalnych(
      panel, coords, attr(przestrzenny_xgb, "predykcje"),
      n_permutacji = 99)
    write.csv(klastry_lokalne,
              file.path(OUT, "klastry_lokalne_moran_xgb.csv"),
              row.names = FALSE, fileEncoding = "UTF-8")
  }
  # ---- Test odporności: inne modele ML vs FE (przed prognozą - wybór modelu) ----
  por_ml <- porownanie_modeli_ml(panel, coords, sdm = cechy_sdm(panel_all, granice))
  pred_ml <- attr(por_ml, "predykcje")

  # ---- Prognoza do 2030: elastic net (najlepszy w teście odporności) ----
  # q90 z błędów out-of-time danego modelu w walidacji kroczącej 2019-2024
  q90_prognozy <- unname(quantile(
    abs(pred_ml$dep - pred_ml$pred)[pred_ml$model == "Elastic net"], 0.9, type = 8))
  q90_xgb <- kalibracja$kalibratory$q90[kalibracja$kalibratory$grupa == "wszystkie"]
  wynik_prognozy <- prognoza_do_2030(panel, coords, rok_koncowy = 2030,
                                     q90 = q90_prognozy, model_typ = "enet")
  prognoza <- wynik_prognozy$prognoza
  write.csv(prognoza, file.path(OUT, "prognoza_do_2030.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  # prognoza porównawcza XGBoost i zgodność obu prognoz
  prognoza_xgb <- prognoza_do_2030(panel, coords, rok_koncowy = 2030,
                                   q90 = q90_xgb, model_typ = "xgb")$prognoza
  write.csv(prognoza_xgb, file.path(OUT, "prognoza_xgb_porownawcza.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  rk_en <- ranking_ryzyka_depopulacji(prognoza)
  rk_xg <- ranking_ryzyka_depopulacji(prognoza_xgb)
  zg <- inner_join(rk_en %>% select(kod_powiatu, en = skumulowana_zmiana_2025_2030_pct,
                                    r_en = ranking_ryzyka),
                   rk_xg %>% select(kod_powiatu, xg = skumulowana_zmiana_2025_2030_pct,
                                    r_xg = ranking_ryzyka), by = "kod_powiatu")
  porownanie_prognoz <- data.frame(
    model = c("Elastic net (główna)", "XGBoost (porównawcza)"),
    q90 = c(q90_prognozy, q90_xgb),
    srednia_zmiana_2025_2030_pct = c(mean(zg$en), mean(zg$xg)),
    min_zmiana_pct = c(min(zg$en), min(zg$xg)),
    max_zmiana_pct = c(max(zg$en), max(zg$xg)),
    najnizsza_prognoza_roczna = c(min(prognoza$prognoza_zmiany_ludnosci_1000),
                                  min(prognoza_xgb$prognoza_zmiany_ludnosci_1000)),
    korelacja_spearmana_rankingow = cor(zg$r_en, zg$r_xg, method = "spearman"),
    wspolne_w_top20 = sum(zg$r_en <= 20 & zg$r_xg <= 20))
  write.csv(porownanie_prognoz, file.path(OUT, "prognoza_porownanie_modeli.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  ranking_ryzyka <- ranking_ryzyka_depopulacji(prognoza)
  ranking_ryzyka <- dolacz_nazwy_powiatow(ranking_ryzyka)
  write.csv(ranking_ryzyka,
            file.path(OUT, "ranking_ryzyka_depopulacji_2030.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(
    head(ranking_ryzyka, 20),
    file.path(OUT, "top20_ryzyka_depopulacji_2030.csv"),
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  wykres_top_ryzyka(ranking_ryzyka)

  # ---- Scenariusze interwencji ----
  # Model główny scenariuszy: hybryda elastic net + progi z ALE (najdokładniejszy
  # model determinantów, a jednocześnie odwzorowuje progi, np. dostępności).
  # XGBoost i czysty elastic net służą jako kontrola odporności.
  scen <- analiza_scenariuszy(panel, coords, "enet_progi")
  scen_x <- analiza_scenariuszy(panel, coords, "xgb")
  scen_en <- analiza_scenariuszy(panel, coords, "enet")
  write.csv(scen$podsumowanie, file.path(OUT, "scenariusze_podsumowanie.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(bind_rows(scen$podsumowanie, scen_x$podsumowanie, scen_en$podsumowanie),
            file.path(OUT, "scenariusze_porownanie_modeli.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  jsonlite::write_json(scen$symulator, file.path(OUT, "symulator_scenariuszy.json"),
                       digits = NA, auto_unbox = TRUE, dataframe = "rows")
  write.csv(scen$per_powiat, file.path(OUT, "scenariusze_powiaty.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  # istotność efektów scenariuszy: bootstrap blokowy po powiatach
  bt <- bootstrap_scenariuszy(panel, coords, B = 1000, progi_stale = TRUE)
  bt_p <- bootstrap_scenariuszy(panel, coords, B = 200, progi_stale = FALSE)
  write.csv(bind_rows(bt$efekty %>% mutate(wariant = "progi stałe, B = 1000"),
                      bt_p$efekty %>% mutate(wariant = "progi szacowane w replikacji, B = 200")),
            file.path(OUT, "scenariusze_bootstrap.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(bt$wspolczynniki, file.path(OUT, "enet_bootstrap_wspolczynniki.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")

  # ---- Mapy GIS ----
  mapy_statyczne(granice, klasy, ranking_ryzyka, scen)
  mapa_interaktywna_prognoz(prognoza, ranking_ryzyka, granice)

  # ---- Porównanie klas na wszystkich 380 powiatach ----
  por_klas <- porownanie_klas(panel_all, coords, granice)

  # ---- Ekonometria przestrzenna ----
  sp <- ekonometria_przestrzenna(panel_all, granice)

  # ---- Nieliniowości, walidacja przestrzenna, typologia rozwoju ----
  nielin <- analiza_nieliniowosci(panel, coords)
  wal_sp <- walidacja_przestrzenna(panel, coords, granice)
  typ_rozw <- typologia_rozwoju(panel_all, por_klas$prognoza, granice)

  # ---- Osobne modele i prognozy dla każdej cechy ----
  ocena_cech <- ocena_xgb_wszystkich_cech(panel, coords,
                                          lata_testowe = 2019:2024)
  write.csv(ocena_cech, file.path(OUT, "xgb_ocena_wszystkich_cech.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(attr(ocena_cech, "wyniki_roczne"),
            file.path(OUT, "xgb_ocena_wszystkich_cech_lata.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  prognoza_cech <- prognoza_cech_do_2030(panel, coords, rok_koncowy = 2030)
  write.csv(prognoza_cech, file.path(OUT, "prognoza_wszystkich_cech_do_2030.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  linie <- c(linie,
    "\n=== 6. PORÓWNANIE MODELI (walidacja krocząca 2019-2024) ===",
    "Niższy RMSE oznacza lepszą prognozę; ranking dotyczy średniej z lat testowych.",
    sprintf("%-30s RMSE średnie | SD między latami", "model"),
    sprintf("%-30s %12.4f | %8.4f",
            benchmark$model, benchmark$rmse_srednie, benchmark$rmse_sd),
    sprintf("Strojenie XGBoost: wybrany wariant = %s | RMSE walidacji strojenia = %.4f",
            strojenie$wyniki$wariant[1],
            strojenie$wyniki$rmse_walidacja[1]),
    "\n=== 7. XGBOOST: MODEL DETERMINANTÓW ===",
    "Model determinantów nie uzywa dep_lag ani salda migracji ani przyrostu naturalnego.",
    sprintf("%-30s RMSE średnie (bez dep_lag)",
            "model"),
    sprintf("%-30s %11.4f",
            warianty_xgb$model, warianty_xgb$rmse_srednie),
    "\n=== 8. PERMUTATION IMPORTANCE XGBOOST ===",
    "Delta RMSE jest liczona na testach 2019-2024 po przetasowaniu jednej cechy.",
    "Wartości dodatnie oznaczają pogorszenie predykcji po usunięciu informacji cechy.",
    sprintf(    "Najwazniejsza cecha strukturalna: %s (delta RMSE = %.4f)",
            importance_xgb$cecha[which.max(importance_xgb$delta_rmse_srednia)],
            max(importance_xgb$delta_rmse_srednia)),
    "\n=== 9. WALIDACJA ANTYPRZEUCZENIOWA I AUC ===",
    "Walidacja jest kroczaca ex ante: dla kazdego roku testowego model uczy sie tylko na latach wcześniejszych.",
    "AUC dotyczy pomocniczej klasyfikacji: czy zmiana ludnosci w roku testowym bedzie ujemna.",
    sprintf("RMSE train: %.4f | RMSE test: %.4f | luka RMSE: %.4f",
            antyprzeuczenie$podsumowanie$rmse_train_srednie,
            antyprzeuczenie$podsumowanie$rmse_test_srednie,
            antyprzeuczenie$podsumowanie$luka_rmse_srednia),
    sprintf("MAE train: %.4f | MAE test: %.4f | bias test: %.4f",
            antyprzeuczenie$podsumowanie$mae_train_srednie,
            antyprzeuczenie$podsumowanie$mae_test_srednie,
            antyprzeuczenie$podsumowanie$bias_test_sredni),
    sprintf("Odsetek obserwacji ze spadkiem ludności: %.1f%% (stąd wysoka accuracy 'za darmo')",
            100 * antyprzeuczenie$podsumowanie$prevalence_srednia),
    sprintf("AUC: %.4f | accuracy: %.4f | balanced acc.: %.4f | sensitivity: %.4f | specificity: %.4f | F1: %.4f",
            antyprzeuczenie$podsumowanie$auc_srednie,
            antyprzeuczenie$podsumowanie$accuracy_srednia,
            antyprzeuczenie$podsumowanie$balanced_accuracy_srednia,
            antyprzeuczenie$podsumowanie$sensitivity_srednia,
            antyprzeuczenie$podsumowanie$specificity_srednia,
            antyprzeuczenie$podsumowanie$f1_srednie),
    "\n=== 10. KALIBRACJA PRZEDZIALOW PROGNOZY XGBOOST ===",
    "Pokrycie jest sprawdzane na prognozach out-of-time 2019-2024.",
    sprintf("Pokrycie globalne przedzialu 90%%: %.1f%%",
            kalibracja$pokrycie$pokrycie[
              kalibracja$pokrycie$grupa == "wszystkie"] * 100),
    "Pokrycie wielohoryzontowe jest warunkowe na znanych opóźnionych cechach.",
    sprintf("Pokrycie h=1: %.1f%% | h=2: %.1f%% | h=3: %.1f%%",
            100 * mean(kalibracja_h$pokrycie$pokrycie[
              kalibracja_h$pokrycie$horyzont == 1]),
            100 * mean(kalibracja_h$pokrycie$pokrycie[
              kalibracja_h$pokrycie$horyzont == 2]),
            100 * mean(kalibracja_h$pokrycie$pokrycie[
              kalibracja_h$pokrycie$horyzont == 3])),
    "\n=== 11. AUTOKORELACJA PRZESTRZENNA RESZT XGBOOST ===",
    "Moran I liczony dla k=8 najblizszych powiatow, test permutacyjny.",
    "Istotne dodatnie Moran I reszt oznacza niewykorzystana strukture przestrzenna.",
    if (is.null(przestrzenna)) {
      "Brak centroidow: diagnostyka przestrzenna pominieta."
    } else {
      sprintf("Sredni Moran I reszt: %.4f | odsetek p<0.05: %.1f%%",
              mean(przestrzenna$moran_reszty, na.rm = TRUE),
              100 * mean(przestrzenna$p_reszty < 0.05, na.rm = TRUE))
    },
    "\n=== 12. XGBOOST PRZESTRZENNY: LAGI SASIEDZTWA ===",
    "Model rozszerza cechy o wazone srednie opoznionych wartosci 8 sasiadow.",
    if (is.null(przestrzenny_xgb)) {
      "Brak centroidow: przestrzenny XGBoost pominiety."
    } else {
      paste(sprintf("%-30s %11.4f",
                    przestrzenny_xgb$model,
                    przestrzenny_xgb$rmse_srednie),
            collapse = "\n")
    },
    "\n=== 13. LOKALNE KLASTRY MORAN'S I ===",
    "Dla dep: HH = skupiska wysokiej zmiany/wzrostu, LL = skupiska niskiej zmiany (depopulacji).",
    "HL/LH oznaczaja lokalne obserwacje odstajace; klasyfikacja przy p<0.05.",
    if (is.null(klastry_lokalne)) {
      "Brak centroidow: klastry lokalne pominiete."
    } else {
      paste(capture.output(print(
        # klastry zmiennej dep są identyczne dla obu modeli - pokazujemy raz
        klastry_lokalne %>%
          filter(obiekt == "dep", p_permutacyjne < 0.05,
                 model == "XGBoost bazowy") %>%
          count(rok, klaster) %>%
          pivot_wider(names_from = klaster, values_from = n, values_fill = 0)
      )), collapse = "\n")
    },
    "\n=== 14. PROGNOZA DO 2030 (elastic net; XGBoost porównawczo) ===",
    sprintf("Model główny: elastic net (alpha = %.1f), najlepszy w teście odporności (sekcja 15c).",
            wynik_prognozy$model$alpha),
    "Prognoza rekurencyjna: predyktory wg trendu z lat 2022-2024 (w granicach zakresu obserwowanego).",
    "Porównanie z prognozą XGBoost:",
    paste(capture.output(print(porownanie_prognoz, digits = 3)), collapse = "\n"),
    sprintf("Przedziały 90%%: +/- %.2f promila x sqrt(horyzont), q90 z walidacji kroczącej.",
            q90_prognozy),
    sprintf("Liczba powiatów: %d | lata prognozy: %d-%d",
            n_distinct(prognoza$kod_powiatu),
            min(prognoza$rok), max(prognoza$rok)),
    paste(capture.output(print(as.data.frame(
      prognoza %>% group_by(rok) %>%
        summarise(srednia = mean(prognoza_zmiany_ludnosci_1000),
                  min = min(prognoza_zmiany_ludnosci_1000),
                  max = max(prognoza_zmiany_ludnosci_1000), .groups = "drop")),
      digits = 3)), collapse = "\n"),
    sprintf("Średnia skumulowana zmiana ludności 2025-2030: %.2f%%",
            mean(ranking_ryzyka$skumulowana_zmiana_2025_2030_pct)),
    "\n=== 15. RANKING RYZYKA DEPOPULACJI DO 2030 ===",
    "Ranking rosnąco według skumulowanej zmiany ludności 2025-2030 (%).",
    sprintf("Powiatów w rankingu: %d", nrow(ranking_ryzyka)),
    paste(capture.output(print(as.data.frame(
      head(ranking_ryzyka %>%
        select(ranking_ryzyka, nazwa_powiatu,
               skumulowana_zmiana_2025_2030_pct, prognoza_2030), 10)),
      digits = 3)), collapse = "\n"),
    "\n=== 15a. SCENARIUSZE INTERWENCJI (model determinantów, bez dep_lag) ===",
    "Efekt = różnica prognozowanej rocznej zmiany ludności [promile]; >0 = mniejsza depopulacja.",
    "Odsetek z poprawą = powiaty z efektem > 0,05 promila.",
    "Symulacja na modelu predykcyjnym - nie jest oszacowaniem efektu przyczynowego.",
    paste(capture.output(print(as.data.frame(scen$podsumowanie), digits = 3)),
          collapse = "\n"),
    "\n=== 15b. PORÓWNANIE KLAS (wszystkie 380 powiatów) ===",
    "Walidacja krocząca 2019-2024 na pełnym panelu, RMSE według klas:",
    paste(capture.output(print(as.data.frame(
      por_klas$ocena %>% select(klasa, model, rmse) %>%
        pivot_wider(names_from = model, values_from = rmse)), digits = 3)),
      collapse = "\n"),
    "Prognoza 2025-2030 według klas:",
    paste(capture.output(print(as.data.frame(por_klas$podsumowanie), digits = 3)),
          collapse = "\n"),
    "\n=== 15c. INNE MODELE ML vs FE (peryferie, walidacja krocząca 2019-2024) ===",
    "DM = panelowy test Diebolda-Mariano vs FE (śr. różnica kwadratów błędów; <0 = lepszy od FE).",
    "dm_p_holm = p po korekcie Holma na wielokrotne porównania (10 modeli vs FE).",
    paste(capture.output(print(as.data.frame(por_ml), digits = 3)), collapse = "\n"),
    "\n=== 15d. EKONOMETRIA PRZESTRZENNA (380 powiatów, sąsiedztwo graniczne) ===",
    sprintf("Powiatów w zbalansowanym panelu: %d | średnio sąsiadów: %.2f",
            sp$n_powiatow, sp$sasiedzi_sr),
    "Testy LM (FE powiat + rok):",
    paste(capture.output(print(as.data.frame(sp$lm), digits = 3)), collapse = "\n"),
    "Moran I reszt FE (dwustronny):",
    paste(capture.output(print(as.data.frame(sp$moran), digits = 3)), collapse = "\n"),
    "Współczynniki FE / SAR / SEM:",
    paste(capture.output(print(as.data.frame(
      sp$wsp %>% mutate(b = sprintf("%+.3f%s", beta, ifelse(p < .05, "*", ""))) %>%
        select(zmienna, model, b) %>%
        pivot_wider(names_from = model, values_from = b)))), collapse = "\n"),
    "Test Walda (czy SDM redukuje się do SAR):",
    if (is.null(sp$wald)) "nie policzono" else
      paste(capture.output(print(sp$wald, digits = 3)), collapse = "\n"),
    "Efekty SAR i SDM (bezpośredni / pośredni-spillover / całkowity):",
    if (is.null(sp$imp)) "nie policzono" else
      paste(capture.output(print(as.data.frame(sp$imp), digits = 3)), collapse = "\n"),
    "\n=== 15f. NIELINIOWOŚCI (ALE, model determinantów) ===",
    "zakres_efektu = rozpiętość krzywej ALE [promile]; r2_liniowe < 0,9 = wyraźna nieliniowość.",
    paste(capture.output(print(as.data.frame(nielin), digits = 3)), collapse = "\n"),
    "\n=== 15g. WALIDACJA PRZESTRZENNO-CZASOWA (10 bloków przestrzennych) ===",
    paste(capture.output(print(as.data.frame(
      wal_sp %>% select(schemat, model, rmse) %>%
        pivot_wider(names_from = schemat, values_from = rmse)), digits = 3)), collapse = "\n"),
    "\n=== 15h. TYPOLOGIA ROZWOJU (380 powiatów) ===",
    paste(capture.output(print(as.data.frame(
      typ_rozw$tab %>% select(klasa, typ, n) %>%
        pivot_wider(names_from = klasa, values_from = n)))), collapse = "\n"),
    "\n=== 15e. PORÓWNANIE KLAS: FE według klas (beta standaryzowane, * p<0.05) ===",
    paste(capture.output(print(as.data.frame(
      por_klas$fe %>%
        mutate(b = sprintf("%+.2f%s", beta, ifelse(p < .05, "*", ""))) %>%
        select(zmienna, klasa, b) %>%
        pivot_wider(names_from = klasa, values_from = b)))), collapse = "\n"),
    "\n=== 16. OSOBNE PROGNOZY DLA KAŻDEJ CECHY ===",
    "Każdy cel ma osobny model XGBoost i te same opóźnione informacje wejściowe.",
    "RMSE/MAE są liczone na kolejnych latach niewidzianych podczas uczenia.",
    sprintf("Najlepszy cel wg średniego RMSE: %s",
            ocena_cech$cel[which.min(ocena_cech$rmse_srednie)]),
    sprintf("Liczba celów: %d | plik: prognoza_wszystkich_cech_do_2030.csv",
            nrow(ocena_cech)))

  raport <- paste(unlist(linie), collapse = "\n")
  cat(raport, "\n")
  con <- file(file.path(OUT, "wyniki_modeli.txt"), open = "w", encoding = "UTF-8")
  writeLines(raport, con)
  close(con)
}

if (sys.nframe() == 0) main()
