# CAPITOLO 8 - DIAGNOSTICA DEL MODELLO
# PULIZIA DELL'AMBIENTE
rm(list = ls())
# IMPOSTAZIONE DELLA CARTELLA DI LAVORO
setwd("/Users/vince/Desktop/Progetto-Statistica-Appl-2025-26/Gruppo19")

# CONTROLLO E CARICAMENTO DEI PACCHETTI
pacchetti <- c(
  "ggplot2",
  "dplyr",
  "openxlsx",
  "lmtest",
  "car"
)

pacchetti_mancanti <- pacchetti[
  !(pacchetti %in% installed.packages()[, "Package"])
]

if (length(pacchetti_mancanti) > 0) {
  install.packages(pacchetti_mancanti)
}

invisible(
  lapply(
    pacchetti,
    library,
    character.only = TRUE
  )
)


# CREAZIONE DELLA CARTELLA DEI GRAFICI
cartella_grafici <- file.path(
  "grafici",
  "capitolo 8"
)

if (!dir.exists(cartella_grafici)) {
  dir.create(
    cartella_grafici,
    recursive = TRUE
  )
}

cat(
  "\nCartella dei grafici:",
  normalizePath(cartella_grafici),
  "\n"
)

# IMPORTAZIONE DEL DATASET


dataset <- read.csv(
  "Dataset_N19.csv",
  header = TRUE,
  sep = ","
)

str(dataset)
summary(dataset)


# DEFINIZIONE DELLE VARIABILI
y_var <- "y_IQ"

x_vars <- c(
  "x1_ISO",
  "x2_T",
  "x3_MP",
  "x4_CF",
  "x5_F",
  "x6_GSI",
  "x7_UA"
)

nomi_variabili <- c(
  x1_ISO = "ISO",
  x2_T   = "Tempo di esposizione",
  x3_MP  = "Megapixel",
  x4_CF  = "Crop Factor",
  x5_F   = "Focale",
  x6_GSI = "Ground Sampling Interval",
  x7_UA  = "Altitudine UAV"
)


# CONTROLLO DELLE VARIABILI NECESSARIE
variabili_richieste <- c(
  y_var,
  x_vars
)

variabili_mancanti <- setdiff(
  variabili_richieste,
  names(dataset)
)

if (length(variabili_mancanti) > 0) {
  stop(
    paste(
      "Nel dataset mancano le seguenti variabili:",
      paste(variabili_mancanti, collapse = ", ")
    )
  )
}

if (anyNA(dataset[, variabili_richieste])) {
  stop(
    paste(
      "Sono presenti valori mancanti nelle variabili",
      "utilizzate dal modello."
    )
  )
}


# COSTRUZIONE DEL MODELLO MULTIPLO COMPLETO
formula_multipla <- as.formula(
  paste(
    y_var,
    "~",
    paste(x_vars, collapse = " + ")
  )
)

modello_multiplo <- lm(
  formula_multipla,
  data = dataset
)

print(
  summary(modello_multiplo)
)


# GRANDEZZE GENERALI DEL MODELLO
n <- nrow(dataset)

# Numero di regressori
p <- length(x_vars)

# Numero complessivo di parametri, inclusa l'intercetta
numero_parametri <- p + 1

valori_stimati <- fitted(
  modello_multiplo
)

residui <- residuals(
  modello_multiplo
)

residui_standardizzati <- rstandard(
  modello_multiplo
)

residui_studentizzati <- rstudent(
  modello_multiplo
)

distanza_cook <- cooks.distance(
  modello_multiplo
)

leverage <- hatvalues(
  modello_multiplo
)


# ANALISI DEI RESIDUI


cat(
  "\nMedia dei residui:",
  round(mean(residui), 10),
  "\n"
)

cat(
  "Minimo dei residui:",
  round(min(residui), 6),
  "\n"
)

cat(
  "Massimo dei residui:",
  round(max(residui), 6),
  "\n"
)


# GRAFICO RESIDUI VS VALORI STIMATI
dati_residui <- data.frame(
  Osservazione = seq_len(n),
  Valore_stimato = valori_stimati,
  Residuo = residui,
  Residuo_standardizzato = residui_standardizzati
)

grafico_residui <- ggplot(
  dati_residui,
  aes(
    x = Valore_stimato,
    y = Residuo
  )
) +
  geom_point(
    size = 2.5,
    alpha = 0.75
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.9
  ) +
  labs(
    title = "Residui e valori stimati",
    subtitle = "Modello di regressione multipla completo",
    x = "Valori stimati della qualità dell'immagine",
    y = "Residui"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

print(grafico_residui)

ggsave(
  filename = file.path(
    cartella_grafici,
    "Figura_8_1_Residui_Valori_Stimati.png"
  ),
  plot = grafico_residui,
  width = 9,
  height = 6,
  dpi = 300
)


# 8.2 VERIFICA DELLA NORMALITÀ DEI RESIDUI
# TEST DI SHAPIRO-WILK

test_shapiro <- shapiro.test(
  residui
)

W_shapiro <- unname(
  test_shapiro$statistic
)

p_value_shapiro <- test_shapiro$p.value

decisione_shapiro <- ifelse(
  p_value_shapiro < 0.05,
  "Rifiuto H0: evidenza di non normalità",
  "Non rifiuto H0: normalità compatibile con i dati"
)

cat(
  "\nTEST DI SHAPIRO-WILK\n"
)

cat(
  "W:",
  round(W_shapiro, 6),
  "\n"
)

cat(
  "p-value:",
  format.pval(
    p_value_shapiro,
    digits = 6,
    eps = 1e-10
  ),
  "\n"
)

cat(
  "Decisione:",
  decisione_shapiro,
  "\n"
)



# Q-Q PLOT DEI RESIDUI STANDARDIZZATI

dati_qq <- data.frame(
  Residuo_standardizzato = residui_standardizzati
)

grafico_qq <- ggplot(
  dati_qq,
  aes(
    sample = Residuo_standardizzato
  )
) +
  stat_qq(
    size = 2.5,
    alpha = 0.75
  ) +
  stat_qq_line(
    linetype = "dashed",
    linewidth = 0.9
  ) +
  labs(
    title = "Q-Q plot dei residui standardizzati",
    subtitle = "Verifica grafica dell'ipotesi di normalità",
    x = "Quantili teorici della distribuzione normale",
    y = "Quantili osservati dei residui"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

print(grafico_qq)

ggsave(
  filename = file.path(
    cartella_grafici,
    "Figura_8_2_QQ_Plot_Residui.png"
  ),
  plot = grafico_qq,
  width = 8,
  height = 7,
  dpi = 300
)


# VERIFICA DELL'OMOSCHEDASTICITÀ
# TEST DI BREUSCH-PAGAN
test_bp <- bptest(
  modello_multiplo
)

statistica_bp <- unname(
  test_bp$statistic
)

gradi_liberta_bp <- unname(
  test_bp$parameter
)

p_value_bp <- test_bp$p.value

decisione_bp <- ifelse(
  p_value_bp < 0.05,
  "Rifiuto H0: evidenza di eteroschedasticità",
  "Non rifiuto H0: omoschedasticità compatibile con i dati"
)

cat(
  "\nTEST DI BREUSCH-PAGAN\n"
)

cat(
  "Statistica BP:",
  round(statistica_bp, 6),
  "\n"
)

cat(
  "Gradi di libertà:",
  gradi_liberta_bp,
  "\n"
)

cat(
  "p-value:",
  format.pval(
    p_value_bp,
    digits = 6,
    eps = 1e-10
  ),
  "\n"
)

cat(
  "Decisione:",
  decisione_bp,
  "\n"
)

# INDIVIDUAZIONE DI OSSERVAZIONI INFLUENTI
soglia_residui <- 2
soglia_residui_elevati <- 3
soglia_cook <- 4 / n
soglia_leverage <- (
  2 * numero_parametri
) / n


# CLASSIFICAZIONE DELLE OSSERVAZIONI
tabella_diagnostica_osservazioni <- data.frame(
  Osservazione = seq_len(n),
  Valore_osservato = dataset[[y_var]],
  Valore_stimato = valori_stimati,
  Residuo = residui,
  Residuo_standardizzato = residui_standardizzati,
  Residuo_studentizzato = residui_studentizzati,
  Distanza_Cook = distanza_cook,
  Leverage = leverage,
  stringsAsFactors = FALSE
)

tabella_diagnostica_osservazioni <- (
  tabella_diagnostica_osservazioni %>%
    mutate(
      Residuo_atipico = abs(
        Residuo_standardizzato
      ) > soglia_residui,
      
      Residuo_molto_elevato = abs(
        Residuo_standardizzato
      ) > soglia_residui_elevati,
      
      Cook_elevata = Distanza_Cook > soglia_cook,
      
      Leverage_elevato = Leverage > soglia_leverage,
      
      Osservazione_segnalata = (
        Residuo_atipico |
          Cook_elevata |
          Leverage_elevato
      )
    )
)

# OSSERVAZIONI DA APPROFONDIRE
osservazioni_segnalate <- (
  tabella_diagnostica_osservazioni %>%
    filter(
      Osservazione_segnalata
    ) %>%
    arrange(
      desc(Distanza_Cook)
    )
)

numero_residui_atipici <- sum(
  tabella_diagnostica_osservazioni$Residuo_atipico
)

numero_residui_molto_elevati <- sum(
  tabella_diagnostica_osservazioni$Residuo_molto_elevato
)

numero_cook_elevata <- sum(
  tabella_diagnostica_osservazioni$Cook_elevata
)

numero_leverage_elevato <- sum(
  tabella_diagnostica_osservazioni$Leverage_elevato
)

numero_osservazioni_segnalate <- nrow(
  osservazioni_segnalate
)

cat(
  "\nOSSERVAZIONI DIAGNOSTICHE\n"
)

cat(
  "Soglia |residuo standardizzato|:",
  soglia_residui,
  "\n"
)

cat(
  "Soglia distanza di Cook:",
  round(soglia_cook, 6),
  "\n"
)

cat(
  "Soglia leverage:",
  round(soglia_leverage, 6),
  "\n"
)

cat(
  "Residui standardizzati con valore assoluto > 2:",
  numero_residui_atipici,
  "\n"
)

cat(
  "Residui standardizzati con valore assoluto > 3:",
  numero_residui_molto_elevati,
  "\n"
)

cat(
  "Osservazioni oltre la soglia di Cook:",
  numero_cook_elevata,
  "\n"
)

cat(
  "Osservazioni con leverage elevato:",
  numero_leverage_elevato,
  "\n"
)

cat(
  "Osservazioni complessivamente segnalate:",
  numero_osservazioni_segnalate,
  "\n"
)

# GRAFICO DELLA DISTANZA DI COOK
dati_cook <- data.frame(
  Osservazione = seq_len(n),
  Distanza_Cook = distanza_cook,
  Oltre_soglia = distanza_cook > soglia_cook
)

grafico_cook <- ggplot(
  dati_cook,
  aes(
    x = Osservazione,
    y = Distanza_Cook
  )
) +
  geom_col(
    width = 0.70
  ) +
  geom_hline(
    yintercept = soglia_cook,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_text(
    data = subset(
      dati_cook,
      Oltre_soglia
    ),
    aes(
      label = Osservazione
    ),
    vjust = -0.40,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      n,
      by = 10
    )
  ) +
  labs(
    title = "Distanza di Cook",
    subtitle = paste0(
      "Soglia orientativa: 4/n = ",
      round(soglia_cook, 3)
    ),
    x = "Numero dell'osservazione",
    y = "Distanza di Cook"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    panel.grid.major.x = element_blank()
  )

print(grafico_cook)

ggsave(
  filename = file.path(
    cartella_grafici,
    "Figura_8_3_Distanza_Cook.png"
  ),
  plot = grafico_cook,
  width = 10,
  height = 6,
  dpi = 300
)


# GRAFICO LEVERAGE E RESIDUI STUDENTIZZATI
dati_influenza <- data.frame(
  Osservazione = seq_len(n),
  Leverage = leverage,
  Residuo_studentizzato = residui_studentizzati,
  Segnalata = (
    abs(residui_standardizzati) > soglia_residui |
      leverage > soglia_leverage |
      distanza_cook > soglia_cook
  )
)

grafico_influenza <- ggplot(
  dati_influenza,
  aes(
    x = Leverage,
    y = Residuo_studentizzato
  )
) +
  geom_point(
    size = 2.5,
    alpha = 0.75
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.7
  ) +
  geom_hline(
    yintercept = c(-2, 2),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = soglia_leverage,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_text(
    data = subset(
      dati_influenza,
      Segnalata
    ),
    aes(
      label = Osservazione
    ),
    nudge_y = 0.15,
    size = 3.3
  ) +
  labs(
    title = "Leverage e residui studentizzati",
    subtitle = paste0(
      "Soglia leverage = ",
      round(soglia_leverage, 3),
      "; soglie dei residui = ±2"
    ),
    x = "Leverage",
    y = "Residui studentizzati"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    )
  )

print(grafico_influenza)

ggsave(
  filename = file.path(
    cartella_grafici,
    "Figura_8_4_Leverage_Residui_Studentizzati.png"
  ),
  plot = grafico_influenza,
  width = 9,
  height = 7,
  dpi = 300
)



# ANALISI DELLA MULTICOLLINEARITÀ
# CALCOLO DEI VIF
valori_vif <- vif(
  modello_multiplo
)

tabella_vif <- data.frame(
  Variabile = names(valori_vif),
  Descrizione = unname(
    nomi_variabili[names(valori_vif)]
  ),
  VIF = as.numeric(valori_vif),
  stringsAsFactors = FALSE
)

tabella_vif <- (
  tabella_vif %>%
    mutate(
      Tolleranza = 1 / VIF,
      
      Valutazione = case_when(
        VIF < 5 ~ "Nessuna criticità rilevante",
        VIF < 10 ~ "Multicollinearità da approfondire",
        TRUE ~ "Multicollinearità elevata"
      )
    ) %>%
    arrange(
      desc(VIF)
    )
)

cat(
  "\nVARIANCE INFLATION FACTOR\n"
)

print(tabella_vif)


#TABELLA DI SINTESI DEI TEST E DELLA DIAGNOSTICA
tabella_sintesi <- data.frame(
  
  Sezione = c(
    "Analisi dei residui",
    "Normalità",
    "Omoschedasticità",
    "Osservazioni influenti",
    "Osservazioni influenti",
    "Osservazioni influenti",
    "Multicollinearità"
  ),
  
  Indicatore = c(
    "Media dei residui",
    "Shapiro-Wilk",
    "Breusch-Pagan",
    "Residui standardizzati |r| > 2",
    "Distanza di Cook > 4/n",
    "Leverage > 2(p+1)/n",
    "VIF massimo"
  ),
  
  Statistica = c(
    mean(residui),
    W_shapiro,
    statistica_bp,
    numero_residui_atipici,
    numero_cook_elevata,
    numero_leverage_elevato,
    max(tabella_vif$VIF)
  ),
  
  p_value = c(
    NA,
    p_value_shapiro,
    p_value_bp,
    NA,
    NA,
    NA,
    NA
  ),
  
  Soglia_o_riferimento = c(
    "Valore atteso prossimo a 0",
    "α = 0,05",
    "α = 0,05",
    "|r| > 2",
    paste0(
      "4/n = ",
      round(soglia_cook, 4)
    ),
    paste0(
      "2(p+1)/n = ",
      round(soglia_leverage, 4)
    ),
    "Valori inferiori a 5 generalmente non critici"
  ),
  
  Esito = c(
    "Residui centrati intorno a zero",
    decisione_shapiro,
    decisione_bp,
    paste(
      numero_residui_atipici,
      "osservazioni segnalate"
    ),
    paste(
      numero_cook_elevata,
      "osservazioni segnalate"
    ),
    paste(
      numero_leverage_elevato,
      "osservazioni segnalate"
    ),
    ifelse(
      max(tabella_vif$VIF) < 5,
      "Nessuna criticità rilevante",
      ifelse(
        max(tabella_vif$VIF) < 10,
        "Multicollinearità da approfondire",
        "Multicollinearità elevata"
      )
    )
  ),
  
  stringsAsFactors = FALSE
)
  

  # ARROTONDAMENTO DELLE TABELLE
  tabella_sintesi <- tabella_sintesi %>%
    mutate(
      Statistica = round(
        Statistica,
        6
      ),
      p_value = round(
        p_value,
        6
      )
    )
  
  osservazioni_segnalate <- osservazioni_segnalate %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 6)
      )
    )
  
  tabella_vif <- tabella_vif %>%
    mutate(
      VIF = round(
        VIF,
        6
      ),
      Tolleranza = round(
        Tolleranza,
        6
      )
    )
  

  # CREAZIONE UNICO FILE EXCEL
  workbook_capitolo8 <- createWorkbook()
  
  # Foglio 1: sintesi diagnostica
  addWorksheet(
    workbook_capitolo8,
    "Sintesi diagnostica"
  )
  
  writeData(
    workbook_capitolo8,
    sheet = "Sintesi diagnostica",
    x = tabella_sintesi
  )
  
  # Foglio 2: osservazioni influenti
  addWorksheet(
    workbook_capitolo8,
    "Osservazioni influenti"
  )
  
  if (nrow(osservazioni_segnalate) > 0) {
    
    writeData(
      workbook_capitolo8,
      sheet = "Osservazioni influenti",
      x = osservazioni_segnalate
    )
    
  } else {
    
    writeData(
      workbook_capitolo8,
      sheet = "Osservazioni influenti",
      x = data.frame(
        Esito = paste(
          "Nessuna osservazione supera le soglie",
          "diagnostiche adottate."
        )
      )
    )
  }
  
  # Foglio 3: multicollinearità
  addWorksheet(
    workbook_capitolo8,
    "Multicollinearita"
  )

  writeData(
    workbook_capitolo8,
    sheet = "Multicollinearita",
    x = tabella_vif
  )
  
  stile_intestazione <- createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "Bottom"
  )
  
  for (foglio in names(workbook_capitolo8)) {
    
    dati_foglio <- readWorkbook(
      workbook_capitolo8,
      sheet = foglio
    )
    
    addStyle(
      workbook_capitolo8,
      sheet = foglio,
      style = stile_intestazione,
      rows = 1,
      cols = seq_len(
        ncol(dati_foglio)
      ),
      gridExpand = TRUE
    )
    
    setColWidths(
      workbook_capitolo8,
      sheet = foglio,
      cols = seq_len(
        ncol(dati_foglio)
      ),
      widths = "auto"
    )
    
    freezePane(
      workbook_capitolo8,
      sheet = foglio,
      firstRow = TRUE
    )
  }
  
  saveWorkbook(
    workbook_capitolo8,
    file = "Capitolo8_Risultati_Diagnostica.xlsx",
    overwrite = TRUE
  )
  
  
  cat(
    "\nShapiro-Wilk, p-value:",
    round(p_value_shapiro, 6)
  )
  
  cat(
    "\nBreusch-Pagan, p-value:",
    round(p_value_bp, 6)
  )
  
  cat(
    "\nNumero di residui standardizzati con |r| > 2:",
    numero_residui_atipici
  )
  
  cat(
    "\nNumero di osservazioni oltre la soglia di Cook:",
    numero_cook_elevata
  )
  
  cat(
    "\nNumero di osservazioni con leverage elevato:",
    numero_leverage_elevato
  )
  
  cat(
    "\nVIF massimo:",
    round(
      max(tabella_vif$VIF),
      6
    ),
    "\n"
  )
  
  cat(
    "\nFile Excel creato:\n",
    normalizePath(
      "Capitolo8_Risultati_Diagnostica.xlsx"
    ),
    "\n"
  )