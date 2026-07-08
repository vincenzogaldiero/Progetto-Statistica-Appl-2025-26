# Capitolo 3 - Analisi grafica dei dati

# Pulizia ambiente
rm(list = ls())

# Impostazione cartella di lavoro
setwd("/Users/danaiannaccone/Desktop/Gruppo19_SA")

# Importazione dataset
dataset <- read.csv("Dataset_N19.csv", header = TRUE)

# Controllo rapido
head(dataset)
names(dataset)
dim(dataset)

# Creazione cartelle per i grafici

if(!dir.exists("Grafici")){
  dir.create("Grafici")
}

if(!dir.exists("Grafici/Istogrammi")){
  dir.create("Grafici/Istogrammi")
}

if(!dir.exists("Grafici/BoxPlot")){
  dir.create("Grafici/BoxPlot")
}

if(!dir.exists("Grafici/ScatterPlot")){
  dir.create("Grafici/ScatterPlot")
}

if(!dir.exists("Grafici/Correlazioni")){
  dir.create("Grafici/Correlazioni")
}

# 3.1 Istogrammi - un grafico per ogni variabile

for(i in names(dataset)){
  
  png(
    filename = paste0("Grafici/Istogrammi/Istogramma_", i, ".png"),
    width = 1200,
    height = 900
  )
  
  hist(
    dataset[[i]],
    main = paste("Istogramma di", i),
    xlab = i,
    ylab = "Frequenza",
    col = "lightblue",
    border = "white"
  )
  
  dev.off()
}

# 3.1.2 Istogrammi di tutte le variabili 
png(
  filename = "Grafici/Istogrammi/Figura_3_1_Istogrammi_tutte_variabili.png",
  width = 2400,
  height = 1600,
  res = 200
)

par(
  mfrow = c(2, 4),
  mar = c(4, 4, 3, 1),
  cex.main = 1,
  cex.lab = 0.9,
  cex.axis = 0.8
)

for(i in names(dataset)){
  
  hist(
    dataset[[i]],
    main = paste("Istogramma di", i),
    xlab = i,
    ylab = "Frequenza",
    col = "lightblue",
    border = "white"
  )
}

par(mfrow = c(1, 1))
dev.off()

# 3.2 Box Plot - un grafico per ogni variabile

for(i in names(dataset)){
  
  png(
    filename = paste0("Grafici/BoxPlot/Boxplot_", i, ".png"),
    width = 900,
    height = 900
  )
  
  boxplot(
    dataset[[i]],
    main = paste("Box plot di", i),
    ylab = i,
    col = "lightblue"
  )
  
  dev.off()
}

# Figura 3.2.2 - Box plot di tutte le variabili

png(
  filename = "Grafici/BoxPlot/Figura_3_2_BoxPlot_tutte_variabili.png",
  width = 2400,
  height = 1600,
  res = 200
)

par(
  mfrow = c(2, 4),
  mar = c(4, 4, 3, 1),
  cex.main = 1,
  cex.lab = 0.9,
  cex.axis = 0.8
)

for(i in names(dataset)){
  
  boxplot(
    dataset[[i]],
    main = paste("Box plot di", i),
    ylab = "",
    col = "lightblue",
    border = "darkblue",
    outline = TRUE
  )
}

par(mfrow = c(1, 1))
dev.off()

# 3.3 Scatter Plot
# Variabile dipendente y_IQ rispetto a ciascun regressore

variabili_indipendenti <- names(dataset)[names(dataset) != "y_IQ"]

for(i in variabili_indipendenti){
  
  png(
    filename = paste0("Grafici/ScatterPlot/Scatter_y_IQ_", i, ".png"),
    width = 1200,
    height = 900
  )
  
  plot(
    dataset[[i]],
    dataset$y_IQ,
    main = paste("Scatter plot: y_IQ e", i),
    xlab = i,
    ylab = "y_IQ",
    pch = 19,
    col = "blue"
  )
  
  abline(
    lm(dataset$y_IQ ~ dataset[[i]]),
    col = "red",
    lwd = 2
  )
  
  dev.off()
}

# Figura 3.3.2 - Scatter plot y_IQ vs regressori

variabili_indipendenti <- names(dataset)[names(dataset) != "y_IQ"]

png(
  filename = "Grafici/ScatterPlot/Figura_3_3_ScatterPlot_y_IQ_regressori.png",
  width = 2400,
  height = 1600,
  res = 200
)

par(
  mfrow = c(2, 4),
  mar = c(4, 4, 3, 1),
  cex.main = 1,
  cex.lab = 0.9,
  cex.axis = 0.8
)

for(i in variabili_indipendenti){
  
  plot(
    dataset[[i]],
    dataset$y_IQ,
    main = paste("y_IQ vs", i),
    xlab = i,
    ylab = "y_IQ",
    pch = 19,
    col = "blue"
  )
  
  abline(
    lm(dataset$y_IQ ~ dataset[[i]]),
    col = "red",
    lwd = 2
  )
}

# Ottavo riquadro vuoto
plot.new()

par(mfrow = c(1, 1))
dev.off()


# 3.4 Matrice di correlazione

matrice_correlazione <- cor(dataset)

matrice_correlazione

# Salvataggio matrice di correlazione in CSV
write.csv2(
  round(matrice_correlazione, 2),
  "Grafici/Correlazioni/Matrice_Correlazione.csv"
)

# 3.5 Heatmap delle correlazioni

png(
  filename = "Grafici/Correlazioni/Heatmap_correlazioni.png",
  width = 1200,
  height = 1000
)

heatmap(
  matrice_correlazione,
  main = "Heatmap delle correlazioni",
  symm = TRUE
)

dev.off()

# 3.4 alternativa grafica: matrice di correlazione visuale

png(
  filename = "Grafici/Correlazioni/Matrice_correlazione_grafica.png",
  width = 1200,
  height = 1000
)

pairs(
  dataset,
  main = "Matrice grafica delle relazioni tra variabili",
  pch = 19,
  col = "blue"
)

dev.off()