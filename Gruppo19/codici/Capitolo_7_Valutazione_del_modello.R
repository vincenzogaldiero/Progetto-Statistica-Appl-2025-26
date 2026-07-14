# CAPITOLO 7 - VALUTAZIONE DEL MODELLO

# 1. PULIZIA DELL'AMBIENTE

rm(list = ls())

# 2. IMPOSTAZIONE DELLA CARTELLA DI LAVORO
setwd("/Users/vince/Desktop/Progetto-Statistica-Appl-2025-26/Gruppo19")

# 3. CONTROLLO E CARICAMENTO DEI PACCHETTI

pacchetti <- c(
  "ggplot2",
  "dplyr",
  "broom",
  "readr",
  "openxlsx"
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

# 4. CREAZIONE DELLA CARTELLA DEI GRAFICI

cartella_grafici <- file.path(
  "grafici",
  "capitolo 7"
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

# 5. IMPORTAZIONE DEL DATASET

dataset <- read.csv(
  "Dataset_N19.csv",
  header = TRUE,
  sep = ","
)

# Controllo preliminare
str(dataset)
summary(dataset)

# 6. DEFINIZIONE DELLE VARIABILI

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

# 7. CONTROLLO DELLE COLONNE NECESSARIE

variabili_richieste <- c(y_var, x_vars)

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

# 8. COSTRUZIONE DEL MODELLO MULTIPLO COMPLETO

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

summary_modello <- summary(modello_multiplo)

print(summary_modello)

# 7.1 COEFFICIENTE DI DETERMINAZIONE R²

R2_multiplo <- summary_modello$r.squared

cat(
  "\nR² del modello multiplo:",
  round(R2_multiplo, 6),
  "\n"
)

cat(
  "Percentuale di variabilità spiegata:",
  round(R2_multiplo * 100, 2),
  "%\n"
)

# 9. MODELLI DI REGRESSIONE SEMPLICE

lista_modelli_semplici <- lapply(
  x_vars,
  function(variabile) {
    
    formula_semplice <- as.formula(
      paste(y_var, "~", variabile)
    )
    
    lm(
      formula_semplice,
      data = dataset
    )
  }
)

names(lista_modelli_semplici) <- x_vars

# 10. ESTRAZIONE R² DEI MODELLI SEMPLICI

tabella_modelli_semplici <- data.frame(
  Variabile = x_vars,
  Modello = unname(nomi_variabili[x_vars]),
  R2 = sapply(
    lista_modelli_semplici,
    function(modello) {
      summary(modello)$r.squared
    }
  ),
  R2_corretto = sapply(
    lista_modelli_semplici,
    function(modello) {
      summary(modello)$adj.r.squared
    }
  ),
  stringsAsFactors = FALSE
)

tabella_modelli_semplici$Percentuale_spiegata <- (
  tabella_modelli_semplici$R2 * 100
)

# 11. AGGIUNTA DEL MODELLO MULTIPLO

riga_modello_multiplo <- data.frame(
  Variabile = "Modello_completo",
  Modello = "Modello multiplo completo",
  R2 = R2_multiplo,
  R2_corretto = summary_modello$adj.r.squared,
  Percentuale_spiegata = R2_multiplo * 100,
  stringsAsFactors = FALSE
)

tabella_confronto_R2 <- rbind(
  tabella_modelli_semplici,
  riga_modello_multiplo
)

tabella_confronto_R2 <- tabella_confronto_R2 %>%
  mutate(
    R2 = round(R2, 6),
    R2_corretto = round(R2_corretto, 6),
    Percentuale_spiegata = round(
      Percentuale_spiegata,
      2
    )
  ) %>%
  arrange(desc(R2))

print(tabella_confronto_R2)

# 12. ESPORTAZIONE DEL CONFRONTO R² IN EXCEL

write.xlsx(
  tabella_confronto_R2,
  file = "Capitolo7_Confronto_R2_Modelli.xlsx",
  overwrite = TRUE,
  rowNames = FALSE
)

# 13. GRAFICO DI CONFRONTO DEI COEFFICIENTI R²

dati_grafico_R2 <- tabella_confronto_R2 %>%
  mutate(
    Modello = factor(
      Modello,
      levels = Modello[order(R2)]
    )
  )

grafico_R2 <- ggplot(
  dati_grafico_R2,
  aes(
    x = Modello,
    y = R2
  )
) +
  geom_col(
    width = 0.70
  ) +
  geom_text(
    aes(
      label = sprintf("%.3f", R2)
    ),
    hjust = -0.10,
    size = 3.8
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(
      0,
      max(dati_grafico_R2$R2) * 1.15
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  labs(
    title = "Confronto del coefficiente di determinazione",
    subtitle = paste(
      "Modelli semplici e modello di regressione multipla"
    ),
    x = NULL,
    y = expression(R^2)
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
    panel.grid.major.y = element_blank()
  )

print(grafico_R2)

ggsave(
  filename = file.path(
    cartella_grafici,
    "Figura_7_1_Confronto_R2_Modelli.png"
  ),
  plot = grafico_R2,
  width = 10,
  height = 6,
  dpi = 300
)

# 7.2 COEFFICIENTE DI DETERMINAZIONE CORRETTO

R2_corretto_multiplo <- summary_modello$adj.r.squared

cat(
  "\nR² corretto del modello multiplo:",
  round(R2_corretto_multiplo, 6),
  "\n"
)

cat(
  "Differenza tra R² e R² corretto:",
  round(
    R2_multiplo - R2_corretto_multiplo,
    6
  ),
  "\n"
)

# 7.3 ERRORE QUADRATICO MEDIO

# Valori osservati
valori_osservati <- dataset[[y_var]]

# Valori stimati dal modello
valori_stimati <- fitted(modello_multiplo)

# Residui
residui_modello <- residuals(modello_multiplo)

# Numero di osservazioni
n <- nrow(dataset)

# Numero di regressori
p <- length(x_vars)

# 14. MSE CALCOLATO SULLE OSSERVAZIONI

MSE_training <- mean(
  (valori_osservati - valori_stimati)^2
)

RMSE_training <- sqrt(MSE_training)

MAE_training <- mean(
  abs(valori_osservati - valori_stimati)
)

# 15. STIMA DELLA VARIANZA RESIDUA
# Questa quantità utilizza n - p - 1 al denominatore
# e coincide con il quadrato dell'errore standard residuo.

varianza_residua <- sum(residui_modello^2) / (
  n - p - 1
)

errore_standard_residuo <- sqrt(
  varianza_residua
)


cat(
  "\nMSE sui dati di stima:",
  round(MSE_training, 6),
  "\n"
)

cat(
  "RMSE sui dati di stima:",
  round(RMSE_training, 6),
  "\n"
)

cat(
  "MAE sui dati di stima:",
  round(MAE_training, 6),
  "\n"
)

cat(
  "Stima della varianza residua:",
  round(varianza_residua, 6),
  "\n"
)

cat(
  "Errore standard residuo:",
  round(errore_standard_residuo, 6),
  "\n"
)

# 7.4 CAPACITÀ PREDITTIVA DEL MODELLO

# La capacità predittiva viene valutata mediante
# cross-validation a 10 blocchi.
#
# Ogni osservazione viene prevista da un modello che non
# utilizza quella stessa osservazione nella fase di stima.

# 16. CROSS-VALIDATION A 10 BLOCCHI

set.seed(19)

numero_blocchi <- 10

blocchi <- sample(
  rep(
    1:numero_blocchi,
    length.out = n
  )
)

predizioni_cv <- rep(
  NA_real_,
  n
)

for (blocco in 1:numero_blocchi) {
  
  indici_training <- which(
    blocchi != blocco
  )
  
  indici_test <- which(
    blocchi == blocco
  )
  
  modello_cv <- lm(
    formula_multipla,
    data = dataset[indici_training, ]
  )
  
  predizioni_cv[indici_test] <- predict(
    modello_cv,
    newdata = dataset[indici_test, ]
  )
}

# 17. CONTROLLO DELLE PREDIZIONI

if (anyNA(predizioni_cv)) {
  stop(
    paste(
      "La cross-validation non ha prodotto una previsione",
      "per tutte le osservazioni."
    )
  )
}

# 18. INDICATORI PREDITTIVI CROSS-VALIDATI

errori_cv <- valori_osservati - predizioni_cv

MSE_cv <- mean(errori_cv^2)

RMSE_cv <- sqrt(MSE_cv)

MAE_cv <- mean(abs(errori_cv))

SST <- sum(
  (valori_osservati - mean(valori_osservati))^2
)

SSE_cv <- sum(
  (valori_osservati - predizioni_cv)^2
)

R2_predittivo_cv <- 1 - (SSE_cv / SST)


cat(
  "\nRISULTATI DELLA CROSS-VALIDATION\n"
)

cat(
  "MSE cross-validato:",
  round(MSE_cv, 6),
  "\n"
)

cat(
  "RMSE cross-validato:",
  round(RMSE_cv, 6),
  "\n"
)

cat(
  "MAE cross-validato:",
  round(MAE_cv, 6),
  "\n"
)

cat(
  "R² predittivo cross-validato:",
  round(R2_predittivo_cv, 6),
  "\n"
)

# 19. TABELLA COMPLESSIVA DEGLI INDICATORI

tabella_indicatori <- data.frame(
  Indicatore = c(
    "Numero di osservazioni",
    "Numero di regressori",
    "R²",
    "R² corretto",
    "Variabilità spiegata (%)",
    "MSE sui dati di stima",
    "RMSE sui dati di stima",
    "MAE sui dati di stima",
    "Varianza residua corretta",
    "Errore standard residuo",
    "MSE cross-validato",
    "RMSE cross-validato",
    "MAE cross-validato",
    "R² predittivo cross-validato"
  ),
  Valore = c(
    n,
    p,
    R2_multiplo,
    R2_corretto_multiplo,
    R2_multiplo * 100,
    MSE_training,
    RMSE_training,
    MAE_training,
    varianza_residua,
    errore_standard_residuo,
    MSE_cv,
    RMSE_cv,
    MAE_cv,
    R2_predittivo_cv
  ),
  stringsAsFactors = FALSE
)

tabella_indicatori$Valore <- round(
  tabella_indicatori$Valore,
  6
)

print(tabella_indicatori)

# 20. ESPORTAZIONE DEGLI INDICATORI IN EXCEL

write.xlsx(
  tabella_indicatori,
  file = "Capitolo7_Indicatori_Valutazione_Modello.xlsx",
  overwrite = TRUE,
  rowNames = FALSE
)

# 21. TABELLA VALORI OSSERVATI, STIMATI E PREDETTI

tabella_predizioni <- data.frame(
  Osservazione = seq_len(n),
  Valore_osservato = valori_osservati,
  Valore_stimato = valori_stimati,
  Residuo = residui_modello,
  Blocco_cross_validation = blocchi,
  Valore_predetto_CV = predizioni_cv,
  Errore_predittivo_CV = errori_cv,
  Errore_assoluto_CV = abs(errori_cv),
  Errore_quadratico_CV = errori_cv^2
)

tabella_predizioni <- tabella_predizioni %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 6)
    )
  )

write.xlsx(
  tabella_predizioni,
  file = "Capitolo7_Valori_Osservati_Stimati_Predetti.xlsx",
  overwrite = TRUE,
  rowNames = FALSE
)

# 22. GRAFICO VALORI OSSERVATI VS VALORI STIMATI

dati_adattamento <- data.frame(
  Osservato = valori_osservati,
  Stimato = valori_stimati
)

limiti_adattamento <- range(
  c(
    dati_adattamento$Osservato,
    dati_adattamento$Stimato
  ),
  na.rm = TRUE
)

grafico_adattamento <- ggplot(
  dati_adattamento,
  aes(
    x = Osservato,
    y = Stimato
  )
) +
  geom_point(
    size = 2.5,
    alpha = 0.75
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  coord_equal(
    xlim = limiti_adattamento,
    ylim = limiti_adattamento
  ) +
  labs(
    title = "Valori osservati e valori stimati",
    subtitle = paste0(
      "Modello multiplo completo - RMSE = ",
      round(RMSE_training, 3)
    ),
    x = "Qualità dell'immagine osservata",
    y = "Qualità dell'immagine stimata"
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

print(grafico_adattamento)

ggsave(
  filename = file.path(
    cartella_grafici,
    "Figura_7_2_Valori_Osservati_Stimati.png"
  ),
  plot = grafico_adattamento,
  width = 8,
  height = 7,
  dpi = 300
)

# 23. GRAFICO VALORI OSSERVATI VS PREDIZIONI CROSS-VALIDATE

dati_previsione_cv <- data.frame(
  Osservato = valori_osservati,
  Predetto_CV = predizioni_cv
)

limiti_cv <- range(
  c(
    dati_previsione_cv$Osservato,
    dati_previsione_cv$Predetto_CV
  ),
  na.rm = TRUE
)

grafico_previsione_cv <- ggplot(
  dati_previsione_cv,
  aes(
    x = Osservato,
    y = Predetto_CV
  )
) +
  geom_point(
    size = 2.5,
    alpha = 0.75
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  coord_equal(
    xlim = limiti_cv,
    ylim = limiti_cv
  ) +
  labs(
    title = "Valori osservati e predizioni cross-validate",
    subtitle = paste0(
      "Cross-validation a 10 blocchi - RMSE = ",
      round(RMSE_cv, 3),
      " - R² predittivo = ",
      round(R2_predittivo_cv, 3)
    ),
    x = "Qualità dell'immagine osservata",
    y = "Qualità dell'immagine predetta"
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

print(grafico_previsione_cv)

ggsave(
  filename = file.path(
    cartella_grafici,
    "Figura_7_3_Valori_Osservati_Predetti_CV.png"
  ),
  plot = grafico_previsione_cv,
  width = 8,
  height = 7,
  dpi = 300
)

# 24. TABELLA DI CONFRONTO TRA ADATTAMENTO E PREVISIONE

tabella_confronto_errori <- data.frame(
  Valutazione = c(
    "Adattamento sui dati utilizzati per la stima",
    "Predizione mediante cross-validation a 10 blocchi"
  ),
  MSE = c(
    MSE_training,
    MSE_cv
  ),
  RMSE = c(
    RMSE_training,
    RMSE_cv
  ),
  MAE = c(
    MAE_training,
    MAE_cv
  ),
  R2 = c(
    R2_multiplo,
    R2_predittivo_cv
  )
)

tabella_confronto_errori <- tabella_confronto_errori %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 6)
    )
  )

print(tabella_confronto_errori)

write.xlsx(
  tabella_confronto_errori,
  file = "Capitolo7_Confronto_Adattamento_Previsione.xlsx",
  overwrite = TRUE,
  rowNames = FALSE
)

# 25. CREAZIONE DI UN UNICO FILE EXCEL CON TUTTI I RISULTATI

workbook_capitolo7 <- createWorkbook()

addWorksheet(
  workbook_capitolo7,
  "Indicatori modello"
)

writeData(
  workbook_capitolo7,
  sheet = "Indicatori modello",
  x = tabella_indicatori
)

addWorksheet(
  workbook_capitolo7,
  "Confronto R2"
)

writeData(
  workbook_capitolo7,
  sheet = "Confronto R2",
  x = tabella_confronto_R2
)

addWorksheet(
  workbook_capitolo7,
  "Adattamento e previsione"
)

writeData(
  workbook_capitolo7,
  sheet = "Adattamento e previsione",
  x = tabella_confronto_errori
)

addWorksheet(
  workbook_capitolo7,
  "Predizioni"
)

writeData(
  workbook_capitolo7,
  sheet = "Predizioni",
  x = tabella_predizioni
)


# Formattazione delle intestazioni
stile_intestazione <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

for (foglio in names(workbook_capitolo7)) {
  
  addStyle(
    workbook_capitolo7,
    sheet = foglio,
    style = stile_intestazione,
    rows = 1,
    cols = 1:ncol(
      readWorkbook(
        workbook_capitolo7,
        sheet = foglio
      )
    ),
    gridExpand = TRUE
  )
  
  setColWidths(
    workbook_capitolo7,
    sheet = foglio,
    cols = 1:20,
    widths = "auto"
  )
  
  freezePane(
    workbook_capitolo7,
    sheet = foglio,
    firstRow = TRUE
  )
}

saveWorkbook(
  workbook_capitolo7,
  file = "Capitolo7_Risultati_Completi.xlsx",
  overwrite = TRUE
)

# 26. RIEPILOGO FINALE

cat("\n")
cat("ANALISI DEL CAPITOLO 7 COMPLETATA\n")

cat(
  "\nR²:",
  round(R2_multiplo, 4)
)

cat(
  "\nR² corretto:",
  round(R2_corretto_multiplo, 4)
)

cat(
  "\nMSE sui dati di stima:",
  round(MSE_training, 4)
)

cat(
  "\nRMSE sui dati di stima:",
  round(RMSE_training, 4)
)

cat(
  "\nMSE cross-validato:",
  round(MSE_cv, 4)
)

cat(
  "\nRMSE cross-validato:",
  round(RMSE_cv, 4)
)

cat(
  "\nR² predittivo cross-validato:",
  round(R2_predittivo_cv, 4),
  "\n"
)

cat(
  "\nI grafici sono stati salvati nella cartella:\n",
  normalizePath(cartella_grafici),
  "\n"
)

cat(
  "\nI file Excel sono stati salvati nella cartella",
  "principale del progetto.\n"
)

