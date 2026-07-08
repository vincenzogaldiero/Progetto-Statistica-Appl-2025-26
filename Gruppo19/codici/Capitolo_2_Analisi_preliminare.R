# Pulizia dell'ambiente di lavoro
rm(list = ls())

# Impostazione della cartella di lavoro
setwd("C:/Users/vince/Desktop/Gruppo19")

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

# Statistiche descrittive
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

stat_descrittive

# Arrotondamento a due decimali
stat_descrittive[, -1] <- round(stat_descrittive[, -1], 2)

# Salvataggio della tabella
write.csv2(stat_descrittive,
           "Statistiche_Descrittive.csv",
           row.names = FALSE)

# Creazione della cartella per i grafici
if(!dir.exists("grafici")){
  dir.create("grafici")
}

# Boxplot delle variabili
png("grafici/Boxplot_variabili.png",
    width = 1800,
    height = 1200)

par(mfrow = c(2,4))

for(i in names(dataset)){
  boxplot(dataset[[i]],
          main = i,
          col = "lightblue")
}

dev.off()

# Ricerca degli outlier
outlier <- lapply(dataset, function(x) boxplot.stats(x)$out)

outlier

# Numero di outlier
sapply(outlier, length)

# Istogrammi
png("grafici/Istogrammi_variabili.png",
    width = 1800,
    height = 1200)

par(mfrow = c(2,4))

for(i in names(dataset)){
  hist(dataset[[i]],
       main = i,
       xlab = i,
       col = "lightblue",
       border = "white")
}

dev.off()