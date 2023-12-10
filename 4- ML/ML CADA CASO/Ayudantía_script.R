library(tidyverse);library(caret);library(rpart)
library(rpart.plot);library(rattle);library(MLmetrics)
library(readr)
Cancer <- read_csv("Cancer.csv")
##Cancer <- read_csv("G:/Mi unidad/Universidad/Ayudant铆as/Diplomado Data Science/Ayudant铆as/Ayudant铆a 4 2020/Cancer.csv", 
                   ##col_types = cols(X33 = col_skip()))

## Quitamos la columna id y transformamos diagnosis
Cancer <- Cancer %>% 
  select(-id) %>% 
  mutate(diagnosis = factor(ifelse(diagnosis == "M", "0", "1"))) %>% 
  drop_na() %>% 
  rename("concavepointsmean" = `concave points_mean`)

## Observemos correlaci贸n entre variables
library(corrplot)
Cancer %>% 
  select(-diagnosis) %>% 
  cor() %>% 
  corrplot()

## Quitaremos todas las columnas worst y se
Cancer <- Cancer %>% 
  select(diagnosis, contains("mean"))

## Generamos base de datos de entrenamiento y testeo
set.seed(2020)
train <- slice_sample(Cancer, prop = 0.8)
test <- anti_join(Cancer,train)

# Regresion logistica  ----------------------------------------------------


# Ajustamos regresi贸n log铆stica
model_log <- glm(diagnosis ~ ., data = train,
                 family = binomial(link = "logit"))
model_log <- step(model_log)
summary(model_log)

## Calidad de ajuste
pred_log <- as.numeric(predict(model_log, train, type = "response") > 0.5)
error_log <- mean(pred_log != train$diagnosis)
F_score <- F1_Score(train$diagnosis, pred_log, positive = 1)

### Validacion cruzada
library(caret)
traincontrol <- trainControl(method="cv", number=10, p=0.8)
model_log_cv <- train(diagnosis ~ ., data = train,
                      method = "glmStepAIC",
                      trControl= traincontrol)
summary(model_log_cv)
predcv_log <- predict(model_log_cv, newdata = test)
errorcv_log <- mean(predcv_log != test$diagnosis)
F_score_log <- F1_Score(test$diagnosis, predcv_log, positive = 1)

## Validaci贸n del modelo
predval_log <- as.numeric(predict(model_log, newdata = test, type = "response") > 0.5)
errorval_log <- mean(predval_log != test$diagnosis)
Fval_log <- F1_Score(test$diagnosis, predval_log, positive = 1)

# Arbol de decision -----------------------------------------------------


# Arb贸l de decisi贸n
model_tree <- rpart(diagnosis ~ ., data = train)
fancyRpartPlot(model_tree)
rpart.rules(model_tree, cover=TRUE)

## Calidad de ajuste
pred_tree <- as.numeric(predict(model_tree, train)[,2] > 0.5)
error_tree <- mean(pred_tree!= train$diagnosis)

### Validaci贸n cruzada
model_tree_cv <- train(diagnosis ~ ., data = train,
                       method = "rpart",
                       trControl= traincontrol)

## Validaci贸n del modelo
predval_tree <- as.numeric(predict(model_tree, newdata = test)[,2] > 0.5)
errorval_tree <- mean(predval_tree != test$diagnosis)
F1_score_tree <- F1_Score(test$diagnosis,predval_tree, positive = 1)

# Random Forest -----------------------------------------------------------


# Random Forest
library(randomForest)
set.seed(2020)
model_rf <- randomForest(diagnosis ~ ., data = train,
                         ntree = 1000)
varImpPlot(model_rf)

## Calidad de ajuste
model_tree_rf <- train(diagnosis ~ ., data = train,
                              method = "rf",
                              trControl= traincontrol) # mtry = 10, ntree = 500
grilla_rf <- expand.grid(mtry = 1:20)
set.seed(2020)
model_tree_rf_grilla <- train(diagnosis ~ ., data = train,
                       method = "rf",
                       trControl= traincontrol,
                       tuneGrid = grilla_rf) #mtry = 4
model_tree_rf_grilla

## Validaci贸n del modelo
pred_rf_test <- predict(model_tree_rf_grilla, newdata = test)
error_rf_test <- Precision(test$diagnosis, pred_rf_test, positive = 1)
F1_score_tree <- F1_Score(test$diagnosis, pred_rf_test, positive = 1)

# KNN ---------------------------------------------------------------------


# KNN

### Validaci贸n cruzada
grilla_knn <- expand.grid(k = 1:40)
set.seed(2020)
model_knn_cv <- train(diagnosis ~., data=train, method="knn", 
                      trControl=trainControl(method="cv", number=5, p=0.8),
                      tuneGrid = grilla_knn) ## Sugiere usar K=19
plot(model_knn_cv)
library(class)
model_knn <- knn(train[,-1], train[,-1], train$diagnosis, k = 19)

## Calidad del ajuste

### Matriz de confusion
table(train$diagnosis, model_knn)
mean(train$diagnosis == model_knn)

### Validaci贸n
model_knn_test <- knn(train[,-1], test[,-1], train$diagnosis, k = 5)
error_knn <- mean(model_knn_test != test$diagnosis)
F1_score_tree <- F1_Score(test$diagnosis, model_knn_test, positive = 1)

# Naive Bayes -------------------------------------------------------------


# Naive Bayes
library(e1071)
model_NB <- naiveBayes(diagnosis ~., data=train)

## Calidad de ajuste
predval_NB <- predict(model_NB, train)
error_NB <- mean( predval_NB != train$diagnosis)
ConfusionMatrix(predval_NB,train$diagnosis)

## Validaci贸n cruzada
model_NB_cv <- train(diagnosis ~., data=train, method="nb", 
                     trControl=trainControl(method="cv", number=5, p=0.8))

## Validaci贸n
pred_NB <- predict(model_NB,test)
error_NB <- Precision(test$diagnosis, pred_NB, positive = 1)
F_score_NB <- F1_Score(test$diagnosis, pred_NB, positive = 1)

#  SVM --------------------------------------------------------------------


## SVM
library(e1071)
model_svm <- svm(diagnosis ~ ., data = train)
grilla_svm <- expand.grid(kernel = c("radial","linear", "polynomial","sigmoid"))
set.seed(2020)
train(diagnosis ~., data=train, method ="svmRadial", 
      trControl=trainControl(method="cv", number=5, p=0.8))
set.seed(2020)
train(diagnosis ~., data=train, method ="svmPoly", 
      trControl=trainControl(method="cv", number=5, p=0.8))
set.seed(2020)
train(diagnosis ~., data=train, method ="svmLinear", 
      trControl=trainControl(method="cv", number=5, p=0.8))

# El mejor kernel es el polynomial!
model_svm_cv <- train(diagnosis ~., data=train, method ="svmPoly", 
      trControl=trainControl(method="cv", number=5, p=0.8))

# Validaci贸n del modelo
pred_svm <- predict(model_svm_cv, newdata = test)
score_svm <- Precision(test$diagnosis, pred_svm, positive = 1)
f1_svm <- F1_Score(test$diagnosis, pred_svm, positive = 1)

# Red neuronal ------------------------------------------------------------


## Red neuronal
library(neuralnet)
model_nr <- neuralnet(factor(diagnosis) ~ ., data=train, hidden = c(4,5,2), 
                      linear.output=FALSE)
plot(model_nr)

# Calidad de ajuste
train_pred <- as.numeric(predict(model_nr, train)[,2] > 0.5)
train_score <- Precision(train$diagnosis, train_pred)

# Validaci贸n
pred_svm <- as.numeric(predict(model_nr, test)[,2] > 0.5)
score_svm <- Precision(test$diagnosis, pred_svm)


#     ***** ML Clase 6 - K-means, K-mode y Clustering  ---------------------------------------------------



# Datos
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

# URL de los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv"

# Cargar datos
data <- read.csv(url, header=TRUE, sep=",")
data$Classification <- as.factor(data$Classification-1)
data.orig <- data



################# NORMALIZACI覰 DE LOS DATOS #################
# normalize <- function(x){
#   return( (x-min(x))/(max(x)-min(x)) )
# }
# data[,-10] <- as.data.frame(lapply(data[,-10],normalize))

# Z-score = (x-mean(x))/sd(x)
data[,-10] <- as.data.frame(scale(data[,-10]))



######################### K-MEANS ############################
# K-MODES: funci髇 kmodes en la librer韆 klaR

# Determinar el mejor n鷐ero de clusters basado en 30 韓dices
require(NbClust)
set.seed(2020)
k.nbclust <- NbClust(data=data[,-10], distance="euclidean", max.nc=15, method="kmeans", index="alllong")
fviz_nbclust(k.nbclust) + theme_minimal()

# centers=n鷐ero de clusters; nstart=n鷐ero de veces que se va a repetir el proceso
set.seed(2020)
model.km <- kmeans(data[,-10], centers=2, nstart=100)
model.km

# Valores medianos de cada variable en cada cluster
aggregate(data.orig[,-10], by=list(model.km$cluster), median)





######################## CLUSTERING ##########################
require(factoextra)

# Dendrograma
fviz_dend(x=hcut(data[,-10], k=1, hc_method="ward.D2"))

# Determina el mejor n鷐ero de clusters basado en 30 韓dices
require(NbClust)
set.seed(2020)
k.nbclust <- NbClust(data[,-10], distance="euclidean", max.nc=15, method="ward.D2", index="alllong") 
fviz_nbclust(k.nbclust) + theme_minimal()

# Dendrograma est醤dar
fviz_dend(hcut(data[,-10], k=3, hc_method="ward.D2"), rect=TRUE)

# Dendrograma en forma circular
fviz_dend(hcut(data[,-10], k=3, hc_method="ward.D2"), type="circular")

# Dendrograma en forma de 醨bol filogen閠ico
require(igraph)
fviz_dend(hcut(data[,-10], k=3), type="phylogenic", repel=TRUE)

# Valores medianos de cada variable en cada cluster
aggregate(data.orig[,-10], by=list(hcut(data[,-10],k=3)$cluster), median)


