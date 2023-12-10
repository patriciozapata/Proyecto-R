#################### LECTURA DE LOS DATOS ####################
# Datos
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

# URL de los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv"

# Cargar datos
data <- read.csv(url, header=TRUE, sep=",")
data$Classification <- as.factor(data$Classification-1)
##############################################################


############ MUESTRA DE ENTRENAMINETO Y DE PRUEBA ############
set.seed(2020)
position <- sample(1:nrow(data), 0.8*nrow(data))
training <- data[position, ]
test <- data[-position, ]
##############################################################


################## ÁRBOL DE DECISIÓN - CART ##################
require(rpart)
require(rattle)
require(rpart.plot)
model.cart <- rpart(Classification ~ ., data=training)
fancyRpartPlot(model.cart)
rpart.rules(model.cart, cover=TRUE)
##############################################################

##################### CALIDAD DEL AJUSTE #####################
# Valores predictivos
predval.cart1 <- as.numeric(predict(model.cart, training)[,2] > 0.5)
# Matriz de confusión
table(training$Classification, predval.cart1)
# % de clasificación correcta
mean(training$Classification == predval.cart1)
##############################################################

##################### VALIDACIÓN CRUZADA #####################
library(caret)
set.seed(2020)
( cv.cart <- train(Classification ~., data=training, method="rpart", 
             trControl=trainControl(method="cv", number=10, p=0.8)) )
##############################################################

########################### PRUEBA ###########################
# Valores predictivos
predval.cart2 <- as.numeric(predict(model.cart, test)[,2] > 0.5)
# Matriz de confusión
table(test$Classification, predval.cart2)
# % de clasificación correcta
mean(test$Classification == predval.cart2)
##############################################################



###################### RANDOM FOREST #########################
require(randomForest)
# mtry = floor(sqrt(ncol(x)))
set.seed(2020)
model.rf <- randomForest(Classification ~ ., data=training, 
            ntree=10, mtry=3, replace=TRUE, importance=T)
varImpPlot(model.rf)
##############################################################

##################### CALIDAD DEL AJUSTE #####################
# Valores predictivos
predval.rf1 <- predict(model.rf, training)
# Matriz de confusión
table(training$Classification, predval.rf1)
# % de clasificación correcta
mean(training$Classification == predval.rf1)
##############################################################

##################### VALIDACIÓN CRUZADA #####################
set.seed(2020)
( cv_rf <- train(Classification ~., data=training, method="rf", 
           trControl=trainControl(method="cv", number=10, p=0.8), 
           tuneGrid=expand.grid(.mtry=3)) )
##############################################################


########################### PRUEBA ###########################
# Valores predictivos
predval.rf2 <- predict(model.rf, test)
# Matriz de confusión
table(test$Classification, predval.rf2)
# % de clasificación correcta
mean(test$Classification == predval.rf2)
##############################################################
