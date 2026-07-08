# Pulizia dell'ambiente di lavoro
rm(list = ls())

# Impostazione della cartella di lavoro
setwd("/Users/danaiannaccone/Desktop/Gruppo19_SA")

# Importazione del dataset
dataset <- read.csv("Dataset_N19.csv", header = TRUE)

# Prime osservazioni
head(dataset)

# Dimensioni del dataset
dim(dataset)

# Nomi delle variabili
names(dataset)

# Struttura del dataset
str(dataset)

# Controllo dei valori mancanti
colSums(is.na(dataset))

# Statistiche descrittive di base
summary(dataset)

# Deviazione standard
sapply(dataset, sd)

# Tabella con le statistiche descrittive
stat_descrittive <- data.frame(
  Variabile = names(dataset),
  Media = sapply(dataset, mean),
  Mediana = sapply(dataset, median),
  DeviazioneStandard = sapply(dataset, sd),
  Minimo = sapply(dataset, min),
  PrimoQuartile = sapply(dataset, quantile, probs = 0.25),
  TerzoQuartile = sapply(dataset, quantile, probs = 0.75),
  Massimo = sapply(dataset, max)
)

# Arrotondamento a due decimali
stat_descrittive[, -1] <- round(stat_descrittive[, -1], 2)

# Visualizzazione della tabella
stat_descrittive

# Salvataggio della tabella delle statistiche descrittive
write.csv2(
  stat_descrittive,
  "Statistiche_Descrittive.csv",
  row.names = FALSE
)

# Calcolo dell'Interquartile Range e dei limiti per gli outlier
analisi_outlier <- data.frame(
  Variabile = names(dataset),
  PrimoQuartile = sapply(dataset, quantile, probs = 0.25),
  TerzoQuartile = sapply(dataset, quantile, probs = 0.75)
)

analisi_outlier$IQR <- analisi_outlier$TerzoQuartile - analisi_outlier$PrimoQuartile

analisi_outlier$LimiteInferiore <- analisi_outlier$PrimoQuartile - 1.5 * analisi_outlier$IQR
analisi_outlier$LimiteSuperiore <- analisi_outlier$TerzoQuartile + 1.5 * analisi_outlier$IQR

# Numero di outlier per ciascuna variabile
analisi_outlier$NumeroOutlier <- sapply(names(dataset), function(i) {
  Q1 <- quantile(dataset[[i]], 0.25)
  Q3 <- quantile(dataset[[i]], 0.75)
  IQR <- Q3 - Q1
  LI <- Q1 - 1.5 * IQR
  LS <- Q3 + 1.5 * IQR
  
  sum(dataset[[i]] < LI | dataset[[i]] > LS)
})

# Arrotondamento a due decimali
analisi_outlier[, 2:6] <- round(analisi_outlier[, 2:6], 2)

# Visualizzazione della tabella degli outlier
analisi_outlier

# Salvataggio della tabella degli outlier
write.csv2(
  analisi_outlier,
  "Analisi_Outlier_IQR.csv",
  row.names = FALSE
)

# Esempio dettagliato per la variabile dipendente y_IQ
Q1_y <- quantile(dataset$y_IQ, 0.25)
Q3_y <- quantile(dataset$y_IQ, 0.75)

IQR_y <- Q3_y - Q1_y

limite_inferiore_y <- Q1_y - 1.5 * IQR_y
limite_superiore_y <- Q3_y + 1.5 * IQR_y

min_y <- min(dataset$y_IQ)
max_y <- max(dataset$y_IQ)

# Visualizzazione dei valori per y_IQ
Q1_y
Q3_y
IQR_y
limite_inferiore_y
limite_superiore_y
min_y
max_y