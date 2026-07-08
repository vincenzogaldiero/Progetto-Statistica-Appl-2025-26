# Capitolo 3 - Analisi grafica dei dati

# Pulizia ambiente
rm(list = ls())

# Impostazione cartella di lavoro
setwd("/Users/vince/Desktop/Progetto-Statistica-Appl-2025-26/Gruppo19")

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

#Box plot

y <- dataset$y_IQ

# Calcolo del criterio IQR per y_IQ
Q1 <- quantile(y, 0.25)
Q3 <- quantile(y, 0.75)
IQR_y <- IQR(y)

limite_inferiore <- Q1 - 1.5 * IQR_y
limite_superiore <- Q3 + 1.5 * IQR_y

outlier_y <- y[y < limite_inferiore | y > limite_superiore]

print(data.frame(
  Q1 = Q1,
  Q3 = Q3,
  IQR = IQR_y,
  Limite_inferiore = limite_inferiore,
  Limite_superiore = limite_superiore,
  Osservazione_IQR = outlier_y
))

variabili <- c("x1_ISO", "x2_T", "x3_MP",
               "x4_CF", "x5_F", "x6_GSI", "x7_UA")

#Boxplot di y_IQ

png("Grafici/BoxPlot/BoxPlot_y_IQ.png",
    width = 1800,
    height = 1400,
    res = 300)

par(mar = c(5,5,2,2))

boxplot(y,
        ylab = "y_IQ",
        col = "lightblue",
        border = "black",
        outline = FALSE,
        boxwex = 0.45,
        lwd = 2,
        cex.axis = 1.1,
        cex.lab = 1.2)

# Evidenzia manualmente l'outlier
if(length(outlier_y) > 0){
  
  points(1,
         outlier_y,
         pch = 16,
         col = "red3",
         cex = 0.8)
  
}

dev.off()

# Box plot complessivo dei regressori

png("Grafici/BoxPlot/BoxPlot_Regressori.png",
    width = 1400,
    height = 900)

par(mar = c(8, 5, 2, 2))

boxplot(dataset[, variabili],
        ylab = "Valori standardizzati",
        col = "lightblue",
        border = "black",
        las = 2,
        outline = TRUE,
        main = "",
        boxwex = 0.65,
        cex.axis = 1.1,
        cex.lab = 1.2)

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

# Heatmap delle correlazioni

if (!require(corrplot)) {
  install.packages("corrplot")
  library(corrplot)
} else {
  library(corrplot)
}

variabili_corr <- dataset[, c("y_IQ", "x1_ISO", "x2_T", "x3_MP",
                              "x4_CF", "x5_F", "x6_GSI", "x7_UA")]

matrice_corr <- cor(variabili_corr)

print(round(matrice_corr, 3))

png("Grafici/Correlazioni/Heatmap_Correlazioni.png",
    width = 1600,
    height = 1400,
    res = 300)

corrplot(matrice_corr,
         method = "color",
         type = "upper",
         order = "original",
         addCoef.col = "black",
         number.cex = 0.65,
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         cl.cex = 0.8,
         diag = TRUE)

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
