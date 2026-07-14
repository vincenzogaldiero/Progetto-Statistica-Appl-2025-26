# ANALISI DI REGRESSIONE SEMPLICE

# Pulizia ambiente
rm(list = ls())

# Impostazione cartella di lavoro
setwd("/Users/vince/Desktop/Progetto-Statistica-Appl-2025-26/Gruppo19")

# Caricamento librerie
library(ggplot2)
library(dplyr)
library(broom)
library(readr)

# Importazione dataset
dataset <- read.csv("Dataset_N19.csv", header = TRUE, sep = ",")

# Controllo struttura
str(dataset)
summary(dataset)

# Variabile dipendente
y_var <- "y_IQ"

# Variabili indipendenti
x_vars <- c("x1_ISO", "x2_T", "x3_MP", "x4_CF", "x5_F", "x6_GSI", "x7_UA")

# Nomi descrittivi per tabelle e grafici
nomi_variabili <- c(
  x1_ISO = "ISO",
  x2_T   = "Tempo di esposizione",
  x3_MP  = "Megapixel",
  x4_CF  = "Crop Factor",
  x5_F   = "Focale",
  x6_GSI = "Ground Sampling Interval",
  x7_UA  = "Altitudine UAV"
)

# Cartella principale
dir.create("Output_Capitolo5", showWarnings = FALSE)

# Cartella Modelli
dir.create("Output_Capitolo5/Modelli", showWarnings = FALSE)

# Cartella Grafici (se non esiste)
dir.create("Grafici", showWarnings = FALSE)

# Sottocartella del Capitolo 5
dir.create("Grafici/Capitolo5", showWarnings = FALSE)

# STIMA DEI MODELLI DI REGRESSIONE LINEARE SEMPLICE

modelli_semplici <- list()

for (x in x_vars) {
  formula_modello <- as.formula(paste(y_var, "~", x))
  modello <- lm(formula_modello, data = dataset)
  modelli_semplici[[x]] <- modello
  
  cat("\n")
  cat("MODELLO:", y_var, "~", x, "-", nomi_variabili[x], "\n")
  print(summary(modello))
}


# TABELLA RIASSUNTIVA DEI MODELLI SEMPLICI

tabella_modelli <- data.frame()

for (x in x_vars) {
  
  modello <- modelli_semplici[[x]]
  sommario <- summary(modello)
  coeff <- coef(sommario)
  
  r2 <- sommario$r.squared
  r2_adj <- sommario$adj.r.squared
  errore_residuo <- sommario$sigma
  f_stat <- sommario$fstatistic[1]
  df1 <- sommario$fstatistic[2]
  df2 <- sommario$fstatistic[3]
  p_value_f <- pf(f_stat, df1, df2, lower.tail = FALSE)
  
  riga <- data.frame(
    Variabile = nomi_variabili[x],
    Nome_R = x,
    Intercetta = coeff[1, 1],
    Beta1 = coeff[2, 1],
    Errore_std_Beta1 = coeff[2, 2],
    t_value = coeff[2, 3],
    p_value_Beta1 = coeff[2, 4],
    R2 = r2,
    R2_corretto = r2_adj,
    Errore_standard_residuo = errore_residuo,
    F_statistic = f_stat,
    p_value_F = p_value_f,
    Significativa_5percento = ifelse(coeff[2, 4] < 0.05, "Sì", "No")
  )
  
  tabella_modelli <- rbind(tabella_modelli, riga)
}

# Ordinamento per R2 decrescente
tabella_modelli <- tabella_modelli %>%
  arrange(desc(R2))

# Visualizzazione tabella
print(tabella_modelli)

# Salvataggio tabella
write.csv(tabella_modelli,
          "Output_Capitolo5/Tabella_confronto_modelli_semplici.csv",
          row.names = FALSE)

# Versione arrotondata per Word
tabella_modelli_word <- tabella_modelli %>%
  mutate(
    Intercetta = round(Intercetta, 4),
    Beta1 = round(Beta1, 4),
    Errore_std_Beta1 = round(Errore_std_Beta1, 4),
    t_value = round(t_value, 4),
    p_value_Beta1 = round(p_value_Beta1, 6),
    R2 = round(R2, 4),
    R2_corretto = round(R2_corretto, 4),
    Errore_standard_residuo = round(Errore_standard_residuo, 4),
    F_statistic = round(F_statistic, 4),
    p_value_F = round(p_value_F, 6)
  )

write.csv(tabella_modelli_word,
          "Output_Capitolo5/Tabella_confronto_modelli_semplici_WORD.csv",
          row.names = FALSE)

print(tabella_modelli_word)


#SALVATAGGIO OUTPUT COMPLETI DEI SINGOLI MODELLI

sink("Output_Capitolo5/Modelli/Output_completo_modelli_semplici.txt")

for (x in x_vars) {
  cat("\n\n")
  cat("MODELLO:", y_var, "~", x, "-", nomi_variabili[x], "\n")
  cat("\n")
  print(summary(modelli_semplici[[x]]))
}

sink()


# EQUAZIONI STIMATE DEI MODELLI


equazioni_modelli <- data.frame()

for (x in x_vars) {
  
  modello <- modelli_semplici[[x]]
  b0 <- coef(modello)[1]
  b1 <- coef(modello)[2]
  
  equazione <- paste0(
    "y_IQ = ",
    round(b0, 4),
    ifelse(b1 >= 0, " + ", " - "),
    abs(round(b1, 4)),
    " * ",
    x
  )
  
  riga <- data.frame(
    Variabile = nomi_variabili[x],
    Nome_R = x,
    Equazione_stimata = equazione
  )
  
  equazioni_modelli <- rbind(equazioni_modelli, riga)
}

print(equazioni_modelli)

write.csv(equazioni_modelli,
          "Output_Capitolo5/Equazioni_modelli_semplici.csv",
          row.names = FALSE)


# GRAFICO DI CONFRONTO TRA R2 DEI MODELLI SEMPLICI

grafico_r2 <- ggplot(tabella_modelli_word,
                     aes(x = reorder(Variabile, R2), y = R2)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Confronto tra i modelli di regressione semplice",
    subtitle = "Modelli ordinati in base al coefficiente di determinazione R²",
    x = "Variabile indipendente",
    y = "R²"
  ) +
  theme_minimal(base_size = 13)

print(grafico_r2)

ggsave(
  filename = "Grafici/Capitolo5/confronto_R2_modelli_semplici.png",
  plot = grafico_r2,
  width = 8,
  height = 5,
  dpi = 300
)

# GRAFICO DI CONFRONTO TRA P-VALUE DEI COEFFICIENTI

grafico_pvalue <- ggplot(tabella_modelli_word,
                         aes(x = reorder(Variabile, p_value_Beta1),
                             y = p_value_Beta1)) +
  geom_col() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Confronto dei p-value nei modelli semplici",
    subtitle = "La linea tratteggiata indica il livello di significatività 0.05",
    x = "Variabile indipendente",
    y = "p-value del coefficiente β1"
  ) +
  theme_minimal(base_size = 13)

print(grafico_pvalue)

ggsave(
  filename = "Grafici/Capitolo5/confronto_pvalue_log_modelli_semplici.png",
  plot = grafico_pvalue,
  width = 8,
  height = 5,
  dpi = 300
)


# RESIDUI DEI MODELLI SEMPLICI

for (x in x_vars) {
  
  modello <- modelli_semplici[[x]]
  
  png(
    filename = paste0("Grafici/Capitolo5/residui_", x, ".png"),
    width = 900,
    height = 700
  )
  
  plot(
    fitted(modello),
    residuals(modello),
    main = paste("Residui vs valori stimati -", nomi_variabili[x]),
    xlab = "Valori stimati",
    ylab = "Residui",
    pch = 19
  )
  abline(h = 0, lty = 2)
  
  dev.off()
}

# 5.9 - MODELLO MIGLIORE TRA LE REGRESSIONI SEMPLICI

modello_migliore <- tabella_modelli[1, ]

cat("\n\n")
cat("MODELLO SEMPLICE MIGLIORE IN BASE A R2\n")
cat("\n")
print(modello_migliore)

write.csv(modello_migliore,
          "Output_Capitolo5/Modello_semplice_migliore.csv",
          row.names = FALSE)

