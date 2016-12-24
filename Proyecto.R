#Instalar estos paquetes antes de iniciar
install.packages("devtools")
devtools::install_github("mariytu/RegressionLibs",force=TRUE)
#esta libreria contiene las otras librerias
library(RegressionLibs)
#library(roxygen2)
#library(devtools)
#library(roxygen2)
#library(plyr)
#library(dplyr)
#modiciacion del enlace para que quede en forma de descarga directa csv
#carga los datos
datatraining <- read.csv(url
("https://docs.google.com/spreadsheets/d/1ZGasUIAGzU0nEQsUfeTMwgB-IBHOPbK5WoQe5kRgwBk/export?format=csv"))
#Las primeras tres columnas aparecen en color amarillo (A,B y C) contienen informacion de
#las muestras, y para efectos estadisticos deb en ser descartados.
datatraining$A<-NULL
datatraining$B<-NULL
datatraining$C<-NULL
#Eliminar las variables dependientes de los modelos de predicción
#Y guardo las variables en otro data
datossalida<-data.frame(datatraining$E,datatraining$G,datatraining$I)
datatraining$D<-NULL
datatraining$E<-NULL
datatraining$F<-NULL
datatraining$G<-NULL
datatraining$H<-NULL
datatraining$I<-NULL
#Funcion que permite normalizar datos
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
normalData <- as.data.frame(lapply(datatraining,normalize)) # In range [0,1]
#---------------------------------code------------------------------------
calculateDiff <- function(dataSet, inf, sup) {
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  if (missing(inf) || missing(sup)) {
    inf <- 1
    sup <- ncol(dataSet)
  }
  if (inf > sup) {
    stop("inf must be less than sup!")
  }
  #All parameters are OK!
  resp <- data.frame(matrix(ncol = ncol(dataSet), nrow = nrow(dataSet)))
  for (i in 1:nrow(dataSet)) {
    before <- dataSet[i,1]
    for (j in 1:ncol(dataSet)) {
      if (j >= inf && j <= sup) {
        resp[i,j] <- abs(dataSet[i,j] - before) / before
        if (is.nan(resp[i,j])) {
          resp[i,j] <- 0
        }
      }
      before <- dataSet[i,j]
    }
  }
  return (resp)
}
#' examples
#' iris.x <- iris[,1:4]
#' diffValues <- calculateDiff(iris.x)
#' limit = 0.15
#' columnsNoise <- getColumnsNoise(diffValues, limit)
getColumnsNoise <- function(data, limit, inf, sup) {
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (missing(limit)) {
    stop("Need to specify limit!")
  }
  if (missing(inf) || missing(sup)) {
    inf <- 1
    sup <- ncol(data)
  }
  if (inf > sup) {
    stop("inf must be less than sup!")
  }
  #All parameters are OK!
  noise <- data.frame(j=integer(),stringsAsFactors=FALSE)
  for (j in 1:ncol(data)) {
    if (j >= inf && j <= sup) {
      x <- data[,j]
      aux <- count(x >= limit)
      
      for (i in 1:nrow(aux)) {
        if (aux[i,1]==TRUE) {
          noise[nrow(noise)+1,1] <- j
        }
      }
    }
  }
  return (noise)
}
noiseDetection <- function(data, limit, cuttingTolerantCount, inf, sup) {
  CutZone <- data.frame(matrix(0, ncol = ncol(data), nrow = nrow(data)))
  FinalCutZone <- data.frame(matrix(0, ncol = ncol(data), nrow = 1))
  
  rows <- nrow(data)
  columns <- ncol(data)
  
  for (i in 1:rows) {
    before <- data[i,1]
    for (j in 1:columns) {
      val <- data[i,j]
      if (j >= inf && j <= sup) {
        if ((abs(val - before) / before) >= limit) {
          CutZone[i,j] <- 1
        }
      }
      before <- val
    }
  }
  
  for (i in 1:rows) {
    for (j in 1:columns) {
      if (j >= inf && j <= sup) {
        if (CutZone[i,j] == 1) {
          FinalCutZone[1,j] <- 1
        }
      }
    }
  }
  
  for (j in 1:columns) {
    if (FinalCutZone[1,j] == 1) {
      i <- 1
      cut <- 1
      while ((i+j) < columns && i <= cuttingTolerantCount) {
        if (FinalCutZone[1,(i+j)] != 1) {
          cut <- 0
        }
        i <- i + 1
      }
      if (cut == 1) {
        FinalCutZone[1,j] <- 1
      }
      else {
        FinalCutZone[1,j] <- 0
      }
    }
  }
  
  return (FinalCutZone)
}
#--------------------------code--------------------------------------------------
#A partir del código anterior calcula los Ruidos
diffValues <- calculateDiff(normalData)
#ParallelPlot me permite visualizar los datos para ir variando el parametro limit de tal manera que no queden cumulos en la gráfica
ParallelPlot(normalData,seq(1,nrow(normalData),1),seq(1,ncol(normalData),1),datossalida$datatraining.E,"salida",1,0.5,TRUE)
#despues de hacer variar este parámetro y mirar como variaba la gráfica anterior con la siguente se
#estima que un 0.03 es un parámetro aceptable 
limit = 0.03
columnsNoise <- getColumnsNoise(diffValues, limit)
#Elimina los ruidos de los datos
Dataunoiseless<-normalData[,-columnsNoise$j]
ParallelPlot(Dataunoiseless,seq(1,nrow(Dataunoiseless),1),seq(1,ncol(Dataunoiseless),1),datossalida$datatraining.E,"salida",1,0.5,TRUE)
DataBase<-datatraining[,-columnsNoise$j]
#Detección de Outlier
#library(DMwR)
library(Rlof)
#Detecto los outlier para diferentes k
outlier.scores1 <- lof(Dataunoiseless, k=5)
outlier.scores2 <- lof(Dataunoiseless, k=6)
outlier.scores3 <- lof(Dataunoiseless, k=7)
outlier.scores4 <- lof(Dataunoiseless, k=8)
outlier.scores5 <- lof(Dataunoiseless, k=9)
outlier.scores6 <- lof(Dataunoiseless, k=10)
#Los ordeno de manera decreciente para ver cuales son los más significativos
outliers1 <- order(outlier.scores1, decreasing=T)[1:10]
outliers2 <- order(outlier.scores2, decreasing=T)[1:10]
outliers3 <- order(outlier.scores3, decreasing=T)[1:10]
outliers4 <- order(outlier.scores4, decreasing=T)[1:10]
outliers5 <- order(outlier.scores5, decreasing=T)[1:10]
outliers6 <- order(outlier.scores6, decreasing=T)[1:10]
# try with different number of neighbors (k = 5,6,7,8,9 and 10)
#veo los 10 instancias con outlier más significativas para cada k y selecciono las 2 más importantes
print(outliers1)
print(outliers2)
print(outliers3)
print(outliers4)
print(outliers5)
print(outliers6)
#Eliminación de outlier de manera manual, por temas de que solamente existen 64 instancias 
#se eliminarán las dos mas significativas de acuerdo a los resultados anteriores
Data<-Dataunoiseless[-63,]
Data<-Data[-36,]
DataBase<-DataBase[-63,]
DataBase<-DataBase[-36,]
datossalida<-datossalida[-63,]
datossalida<-datossalida[-36,]
#vuelvo a normalizar el data set original
DataBase <- as.data.frame(lapply(DataBase,normalize)) # In range [0,1]
#Agrego las variables dependientes a la base de datos
DataBase<-cbind(datossalida,DataBase)
#Cambio en encabezado de las columnas para que coincida con el data set original
names (DataBase)[1] = "E"
names (DataBase)[2] = "G"
names (DataBase)[3] = "I"
#Genero un csv con el Daata Base para utilizar el PCA en wEKA
write.csv(DataBase, 'DataBase.csv')
#Una vez realizado el PCA para genero 3 diferentes data set uno para cada columna E, G e I
#Divido los data set en dos,los data de entrenamiento ylosl data de prueba o testing
#utiliza la regla comun de dejar 2/3 de los datos para entrenamiento y "1/3 para prueba
#esto con el objetivo de hacer un trade-off entre estimar
#un buen modelo y tener una buena estimacion del error.
I <- read.table("DataBaseI.csv",header=TRUE,sep=",")
G <- read.table("DataBaseG.csv",header=TRUE,sep=",")
E <- read.table("DataBaseE.csv",header=TRUE,sep=",")
#Al dividirlos de esta forma se busca una homogeneidad entre las dos clases
DataEntrenamientoI<-rbind(I[1:28,],I[30:31,],I[33,],I[35:43,],I[61:62,])
DataPruebaI<-rbind(I[29,],I[32,],I[34,],I[44:60,])
DataEntrenamientoG<-rbind(G[1:28,],G[30:31,],G[33,],G[35:43,],G[61:62,])
DataPruebaG<-rbind(G[29,],G[32,],G[34,],G[44:60,])
DataEntrenamientoE<-rbind(E[1:28,],E[30:31,],E[33,],E[35:43,],E[61:62,])
DataPruebaE<-rbind(E[29,],E[32,],E[34,],E[44:60,])
#DataEntrenamiento<-rbind(DataBase[1:28,],DataBase[30:31,],DataBase[33,],DataBase[35,],DataBase[36:44,],DataBase[62,])
#DataPrueba<-rbind(DataBase[29,],DataBase[32,],DataBase[34,],DataBase[45:61,])
#ahora volvemos a normalizar tanto los datos de entrenamiento como los datos de prueba
#esto debido a que existen nuevos maximos y minimos en cada data
DataPruebaE <- as.data.frame(lapply(DataPruebaE,normalize)) # In range [0,1]
DataEntrenamientoE <- as.data.frame(lapply(DataEntrenamientoE,normalize)) # In range [0,1]
DataPruebaG <- as.data.frame(lapply(DataPruebaG,normalize)) # In range [0,1]
DataEntrenamientoG <- as.data.frame(lapply(DataEntrenamientoG,normalize)) # In range [0,1]
DataPruebaI <- as.data.frame(lapply(DataPruebaI,normalize)) # In range [0,1]
DataEntrenamientoI <- as.data.frame(lapply(DataEntrenamientoI,normalize)) # In range [0,1]
#Exportar los datos para transformarlos de numericos a nominales en Weka y realizar
#las clasificaciones
#write.csv(DataEntrenamiento, 'DataEntrenamiento.csv')
#write.csv(DataPrueba, 'DataPrueba.csv')
write.csv(DataEntrenamientoI, 'DataEntrenamientoI.csv')
write.csv(DataPruebaI, 'DataPruebaI.csv')
write.csv(DataEntrenamientoG, 'DataEntrenamientoG.csv')
write.csv(DataPruebaG, 'DataPruebaG.csv')
write.csv(DataEntrenamientoE, 'DataEntrenamientoE.csv')
write.csv(DataPruebaE, 'DataPruebaE.csv')