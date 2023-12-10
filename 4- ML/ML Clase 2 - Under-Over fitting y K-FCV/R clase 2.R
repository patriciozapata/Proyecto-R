#################### LECTURA DE LOS DATOS ####################
# Datos
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

# URL de los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv"

# Cargar datos
data <- read.csv(url, header=TRUE, sep=",")
##############################################################


#################### RESUMEN DE LOS DATOS ####################
str(data)
summary(data)
require(corrplot)
corrplot(cor(data), method="number")
data$Classification <- as.factor(data$Classification-1)
##############################################################


############ MUESTRA DE ENTRENAMINETO Y DE PRUEBA ############
set.seed(2020)
position <- sample(1:nrow(data), 0.8*nrow(data))
training <- data[position, ]
test <- data[-position, ]
##############################################################


#################### REGRESIÓN LOGÍSTICA #####################
model <- glm(Classification ~ ., data=training, family=binomial)
summary(model)
##############################################################


##################### CALIDAD DEL AJUSTE #####################
# Valores predictivos
predval.train <- as.numeric(predict(model, training, type="response") > 0.5)

# Matriz de confusión
table(training$Classification, predval.train)

# % de error de clasificación
mean(training$Classification != predval.train)
##############################################################


######################### VALIDACIÓN #########################
# Valores predictivos
predval.test <- as.numeric(predict(model, test, type="response") > 0.5)

# Matriz de confusión
table(test$Classification, predval.test)

# % de error de clasificación
mean(test$Classification != predval.test)
##############################################################


##################### VALIDACIÓN CRUZADA #####################
# Subconjunto 1
train1 <- training[c(1:69), ]
test1 <- training[-c(1:69), ]
# Subconjunto 2
train2 <- training[c(1:46,70:92), ]
test2 <- training[-c(1:46,70:92), ]
# Subconjunto 3
train3 <- training[c(1:23,47:92), ]
test3 <- training[-c(1:23,47:92), ]
# Subconjunto 4
train4 <- training[c(24:92), ]
test4 <- training[-c(24:92), ]

# REGRESIÓN LOGÍSTICA
model1 <- glm(Classification ~ ., data=train1, family=binomial)
model2 <- glm(Classification ~ ., data=train2, family=binomial)
model3 <- glm(Classification ~ ., data=train3, family=binomial)
model4 <- glm(Classification ~ ., data=train4, family=binomial)

# CALIDAD DEL AJUSTE
# Valores predictivos
predval.train1 <- as.numeric(predict(model1, train1, type="response") > 0.5)
predval.train2 <- as.numeric(predict(model2, train2, type="response") > 0.5)
predval.train3 <- as.numeric(predict(model3, train3, type="response") > 0.5)
predval.train4 <- as.numeric(predict(model4, train4, type="response") > 0.5)

# % de error de clasificación
error.train1 <- mean(train1$Classification != predval.train1)
error.train2 <- mean(train2$Classification != predval.train2)
error.train3 <- mean(train3$Classification != predval.train3)
error.train4 <- mean(train4$Classification != predval.train4)
( error.train <- (error.train1+error.train2+error.train3+error.train4)/4 )


# VALIDACIÓN
# Valores predictivos
predval.test1 <- as.numeric(predict(model1, test1, type="response") > 0.5)
predval.test2 <- as.numeric(predict(model2, test2, type="response") > 0.5)
predval.test3 <- as.numeric(predict(model3, test3, type="response") > 0.5)
predval.test4 <- as.numeric(predict(model4, test4, type="response") > 0.5)

# % de error de clasificación
error.test1 <- mean(test1$Classification != predval.test1)
error.test2 <- mean(test2$Classification != predval.test2)
error.test3 <- mean(test3$Classification != predval.test3)
error.test4 <- mean(test4$Classification != predval.test4)
( error.test <- (error.test1+error.test2+error.test3+error.test4)/4 )
##############################################################
