#################### LECTURA DE LOS DATOS ####################
# Datos
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

# URL de los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv"

# Cargar datos
data <- read.csv(url, header=TRUE, sep=",")
data$Classification <- as.factor(data$Classification-1)
##############################################################


################# NORMALIZACIÓN DE LOS DATOS #################
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


############ K-VECINOS MÁS PRÓXIMOS/CERCANOS - KNN ###########
require(class)

# Búsqueda del valor k basado en el error de clasificación 
# de la muestra de entrenamiento/prueba
ec <- numeric()
for(i in 1:50){
  predict <- knn(train=training[,-10], test=test[,-10], cl=training[,10], k=i)
  ec <- c(ec, mean(as.numeric(predict)!=as.numeric(test[,10])))
}
plot(ec, type="l", ylab="Error de clasificación", xlab="K")

# Generalmente, se recomienda la raíz cuadrada del número
# de observaciones en el conjunto de entrenamiento
# k <- round(sqrt(nrow(training)))


# Búsqueda del valor k basado en el promedio del error de 
# clasificación de 100 muestras de entrenamiento/prueba
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

plot(rate/N, type="l", ylab="Error de clasificación (100 muestras)", xlab="K")
k <- which.min(rate)


model <- knn(train=training[,-10], test=training[,-10], cl=training[,10], k=7)
##############################################################

##################### CALIDAD DEL AJUSTE #####################
# Matriz de confusión
table(training$Classification, model)
# % de clasificación correcta
mean(training$Classification == model)
##############################################################

##################### VALIDACIÓN CRUZADA #####################
library(caret)
set.seed(2020)
( cv.knn <- train(Classification ~., data=training, method="knn", 
            trControl=trainControl(method="cv", number=10, p=0.8)) )
##############################################################
