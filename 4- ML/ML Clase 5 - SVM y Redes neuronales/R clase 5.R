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


############## SUPPORT VECTOR MACHINES (SVM) #################
require(e1071)
model.svm <- svm(Classification ~ ., data=training,
             kernel="radial") # kernel="linear","polynomial"
##############################################################

##################### CALIDAD DEL AJUSTE #####################
# Valores predictivos
predval.svm <- predict(model.svm, training)
# Matriz de confusi�n
table(training$Classification, predval.svm)
# % de clasificaci�n correcta
mean(training$Classification == predval.svm)
##############################################################

##################### VALIDACI�N CRUZADA #####################
library(caret)
set.seed(2020)
( cv.svm <- train(Classification ~., data=training, method="svmRadial", # method="svmLinear","svmPoly"
                  trControl=trainControl(method="cv", number=10, p=0.8)) )
##############################################################



####################### REDES NEURONALES #####################
require(neuralnet) 
formula <- Classification ~ Age+BMI+Glucose+Insulin+HOMA+Leptin+Adiponectin+Resistin+MCP.1
layer <- c(2,1) # 0 = Perceptr�n
model.rn <- neuralnet(formula, data=training, hidden=layer, linear.output=FALSE)
plot(model.rn)
##############################################################

##################### CALIDAD DEL AJUSTE #####################
# Valores predictivos
predval.rn <- as.numeric(predict(model.rn, training)[,2] > 0.5)
# Matriz de confusi�n
table(training$Classification, predval.rn)
# % de clasificaci�n correcta
mean(training$Classification == predval.rn)
##############################################################

##################### VALIDACI�N CRUZADA #####################
set.seed(2020)
# N�mero de "folds"
k <- 10
# Proporci�n de la muestra de entrenamiento para la validaci�n cruzada
prop <- 0.8
# Resultado de la validaci�n cruzada
outs <- NULL

for(i in 1:k){
  # Muestra de entrenamiento y de prueba para la validaci�n cruzada
  pos <- sample(1:nrow(training), prop*nrow(training))
  train_cv <- training[pos, ]
  test_cv <- training[-pos, ]
  
  # Modelo de redes neuronales
  model.rn_cv <- neuralnet(formula, data=train_cv, hidden=layer, linear.output=FALSE)
  
  # Valores predictivos
  predval.rn_cv <- as.numeric(predict(model.rn, test_cv)[,2] > 0.5)
  # % de clasificaci�n correcta
  outs[i] <- mean(test_cv$Classification == predval.rn_cv)
}

# Exactitud (Accuracy)
mean(outs)
##############################################################