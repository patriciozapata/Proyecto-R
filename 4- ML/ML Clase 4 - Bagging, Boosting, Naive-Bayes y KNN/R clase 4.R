#################### LECTURA DE LOS DATOS ####################
# Datos
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

# URL de los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv"

# Cargar datos
data <- read.csv(url, header=TRUE, sep=",")
data$Classification <- as.factor(data$Classification-1)
##############################################################


################# NORMALIZACI�N DE LOS DATOS #################
# normalize <- function(x){
#   return( (x-min(x))/(max(x)-min(x)) )
# }
# data[,-10] <- as.data.frame(lapply(data[,-10],normalize))

# Z-score = (x-mean(x))/sd(x)
data[,-10] <- as.data.frame(scale(data[,-10]))
##############################################################


############ MUESTRA DE ENTRENAMINETO Y DE PRUEBA ############
set.seed(2020)
position <- sample(1:nrow(data), 0.8*nrow(data))
training <- data[position, ]
test <- data[-position, ]
##############################################################


############ K-VECINOS M�S PR�XIMOS/CERCANOS - KNN ###########
require(class)

# B�squeda del valor k basado en el error de clasificaci�n 
# de la muestra de entrenamiento/prueba
ec <- numeric()
for(i in 1:50){
  predict <- knn(train=training[,-10], test=test[,-10], cl=training[,10], k=i)
  ec <- c(ec, mean(as.numeric(predict)!=as.numeric(test[,10])))
}
plot(ec, type="l", ylab="Error de clasificaci�n", xlab="K")

# Generalmente, se recomienda la ra�z cuadrada del n�mero
# de observaciones en el conjunto de entrenamiento
# k <- round(sqrt(nrow(training)))


# B�squeda del valor k basado en el promedio del error de 
# clasificaci�n de 100 muestras de entrenamiento/prueba
rate <- numeric(50)
N <- numeric(50)

for(j in 1:100){
  pos <- sample(1:nrow(data), 0.8*nrow(data))
  train.aux <- data[pos,]
  test.aux <- data[-pos,]
  test.size <- nrow(test.aux)
  for(i in 1:50){
    predict <- knn(train=train.aux[,-10], test=test.aux[,-10], cl=train.aux[,10], k=i)
    rate[i] <- rate[i] + sum(as.numeric(predict)!=as.numeric(test.aux[,10]))
    N[i] <- N[i] + test.size
  }
}

plot(rate/N, type="l", ylab="Error de clasificaci�n (100 muestras)", xlab="K")
k <- which.min(rate)


model <- knn(train=training[,-10], test=training[,-10], cl=training[,10], k=7)
##############################################################

##################### CALIDAD DEL AJUSTE #####################
# Matriz de confusi�n
table(training$Classification, model)
# % de clasificaci�n correcta
mean(training$Classification == model)
##############################################################

##################### VALIDACI�N CRUZADA #####################
library(caret)
set.seed(2020)
( cv.knn <- train(Classification ~., data=training, method="knn", 
            trControl=trainControl(method="cv", number=10, p=0.8)) )
##############################################################
