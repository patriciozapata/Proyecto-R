library(tidyverse);library(caret);library(rpart)
library(rpart.plot);library(rattle);library(MLmetrics)
library(readr)


# NOMBRE: 

####### PREGUNTA 1 #######
# 0.75pt
# Respuesta:
# Justificación: 
# repuesta: d
# ya que el modelo esta sobre ajustado  esto se debe a que la muestra no tiene mismo valores 
# para el entrasnmiento 


####### PREGUNTA 2 #######
# 0.75pt
# Respuesta:
# Justificación: 
#Verdadero
#ya que tiene poca cantidad de datos y este no podra generalizar un sobreajuste de los datos.


####### PREGUNTA 3 #######
# (i) 0.25pt
library(e1071)
library(caret)

library(knitr)
library(ggplot2)
library(tidyr)
library(ROCR)

mi_df <- data.frame(

  "Edad" = c("18-40", "18-40", "18-40", "18-40", "18-40", "18-40", "18-40", "41-60", "41-60" , "41-60" , "41-60" , "41-60" , "41-60" , "41-60" , ">60" , ">60", ">60", ">60", ">60", ">60", ">60" ), 
  "Combo" = c("Fiesta", "Fiesta", "Fiesta", "Fiesta", "Supremo", "Mediano", "Fiesta", "Fiesta", "Fiesta" , "Supremo" , "Mediano" , "Fiesta" , "Mediano" , "Supremo" , "Mediano" , "Supremo", "Supremo", "Fiesta", "Mediano", "Supremo", "Fiesta" ) 
)



# (i) [0.25pt] Convierta el conjunto de datos en una tabla de frecuencia.

glimpse(mi_df)
new <-  table(mi_df)
datos <- prop.table(new)

# (ii) [0.25pt] Obtenga las probabilidades marginales por rango de Edad y
# Combo.
mayor_60 <-  sum(datos[1,])
mayor_60

mayor_18_40  <-  sum(datos[2,])
mayor_18_40

mayor_41_60  <- sum(datos[3,])
mayor_41_60


Fiesta  <- sum(datos[,1])
Fiesta

Mediano  <- sum(datos[,2])
Mediano    

Supremo  <- sum(datos[,3])
Supremo

# (iii) [1.00pt] Calcule la probabilidad de recomendar cada Combo dado
# el rango de Edad, es decir, la probabilidad posterior de recomendación.
# p(fiesta| mayor a 60 ) 
#    A           B
# (mayor_60 | Fiesta * Fiesta)  / mayor_60
mayor_60_Fiesta <-  sum(datos[1,1])
mayor_60_Fiesta
(mayor_60_Fiesta * Fiesta)  / mayor_60


# (iv) [0.25pt] Si un cliente de 70 años (es decir, rango de edad: ">60") se
# conecta al sistema, ¿qué Combo recomendaría el algoritmo?
    
# p(Mediano  | mayor a 60 ) 
#    A           B
# (mayor_60 | Fiesta * Fiesta)  / mayor_60
mayor_60_Mediano <-  sum(datos[,2])
mayor_60_Mediano
(mayor_60_Mediano * Mediano)  / mayor_60


mayor_60_Supremo <-  sum(datos[,3])
mayor_60_Supremo
(mayor_60_Supremo * Supremo)  / mayor_60

# se recomendara el combo supremo con 24%


####### PREGUNTA 4 #######
# Cargando los datos
# https://archive.ics.uci.edu/ml/datasets/Wine
# install.packages("rattle.data")
library(rattle.data)
data(wine); head(wine)
str(wine)

# (i) 0.25pt


# (ii) 0.25pt


# (iii) 0.25pt


# (iv) 0.25pt


# (v) 0.25pt


# (vi) 0.25pt


######### PREGUNTA 4  ##############



# install.packages("rattle.data")
# install.packages("dplyr")
library(rattle.data)
library(dplyr)

data <- wine
glimpse(data)
summary(data)
head(data)

summary


# PREGUNTA 4: (i) Estandarice variables continuas. ¿tomamos proline? ------------------


data[,-1] <- as.data.frame(scale(data[,-1]))

# ------------------------------------------------------------------------------------------------------------------- # 

# # PREGUNTA 4 : (ii) Divida la base de datos en 70% para entrenami --------


set.seed(69)
posTraining <- sample(1:nrow(data), 0.8*nrow(data))
data_training <- data[posTraining, ]
data_test <- data[-posTraining, ]
nrow(data_training) + nrow(data_test) # Corroboramos 178 filas


# ------------------------------------------------------------------------------------------------------------------- # 


# PREGUNTA 4 : iii) Implemente los algoritmos de Árbol de Decisión --------



# Arbol de decision
library(rpart)
library(rattle)
library(rpart.plot)
modelo.dt <- rpart(Type ~ . , data=data_training)
x11()
fancyRpartPlot(modelo.dt)
rpart.rules(modelo.dt, cover=TRUE)


# SVM
library(e1071)
modelo.svm <- svm(Type ~ ., data=data_training, kernel="radial")


# ------------------------------------------------------------------------------------------------------------------- # 


# PREGUNTA 4 :(iv) ¿Cuál es el porcentaje de clasificaciones correc --------



# Hint: para Árbol de Decisión, extraiga los valores predichos usando el siguiente código:  
# pred <- predict(model.cart, training) 
# predval.cart <- as.numeric(colnames(pred)[apply(pred,1,which.max)]

# DT predicciones
pred <- predict(modelo.dt, data_training)
predval.dt <- as.numeric(colnames(pred)[apply(pred,1,which.max)])
mean(data_training$Type == predval.dt)

# --> El % de predicción respecto a la data de entrenamiento con modelo decision tree es de 97%



# SVM Predicciones
predval.svm <- predict(modelo.svm, data_training)
# Matriz de confusión
table(data_training$Type, predval.svm)
# % de clasificación correcta
mean(data_training$Type == predval.svm)

# --> El % de predicción respecto a la data de prueba con modelo Support Vector Machine es de 
# 100% (es claramente un sobreajuste puesto que estamos usando la misma base de datos de entrenamiento).



# ------------------------------------------------------------------------------------------------------------------- # 


#  PREGUNTA 4 :  (v) Aplique una vc para ambos algoritmos usando 3 --------


# Basado en la validación cruzada, elija uno de los métodos como el más apropiado.

# DT
library(caret)
set.seed(69)
( cv.dt <- train(Type ~., data=data_training, method="rpart", 
                 trControl=trainControl(method="cv", number=30, p=0.7)) )
# con un CP de 0, el accuracy da 0.911


# SVM
( cv.svm <- train(Type ~., data=data_training, method="svmRadial",
                  trControl=trainControl(method="cv", number=30, p=0.7)) )
# Con sigma = 0.067 y C=0.25, me da un accuracy de 0.993


# --> Por tanto el mejor modelo con VC es el de SVM


# ------------------------------------------------------------------------------------------------------------------- # 


# PREGUNTA 4 : (vi) Aplique ambos modelos ajustados a la base de pr --------



modelo.dt.ajustado <- rpart(Type ~ ., data=data_training, cp=0) ## COMO MIERDA HACEMOS EL MODELO AJUSTADO??? Internet me dijo que carepalo el CP=0...

# DT predicciones
pred <- predict(modelo.dt.ajustado, data_test)
predval.dt <- as.numeric(colnames(pred)[apply(pred,1,which.max)])
mean(data_test$Type == predval.dt)

# El % de predicción respecto a la data de prueba con modelo decision tree es de 94%



modelo.svm.ajustado <- svm(Type ~ ., data=data_training, kernel="radial",C=0.25, sigma=0.06735516 )

# SVM Predicciones
predval.svm <- predict(modelo.svm.ajustado, data_test)
# Matriz de confusión
table(data_test$Type, predval.svm)
# % de clasificación correcta
mean(data_test$Type == predval.svm)

# El % de predicción respecto a la data de prueba con modelo Support Vector Machine es de 97%



# --> Con los modelos ajustados y usando la base de datos de prueba, el modelo que predice mejor es el : SVM con un 97%





####### PREGUNTA 5 #######
# PREGUNTA 5: (i) 0.25pt


# PREGUNTA 5: (ii) 0.25pt


# PREGUNTA 5: (iii) 0.25pt


# PREGUNTA 5: (iv) 0.50pt


########################   PREGUNTA 5: CLUSTERING ##########################

# PREGUNTA 5: (i) 0.25pt --------------------------------------------------


pregunta_5 <-  wine
head(pregunta_5)
require(factoextra)

# Normalizacion  de los datos.
pregunta_5[,-1] <- as.data.frame(scale(pregunta_5[,-1]))

# Determina el mejor número de clusters basado en 30 índices
require(NbClust)
set.seed(2020)
# Dendrograma estándar

# (i) [0.25pt] Aplique el Clustering Jerárquico con k=3 y linkage "ward.D". Visualice el
# resultado final con un gráfico dendrograma.

fviz_dend(hcut(pregunta_5, k=3, hc_method="ward.D"), rect=TRUE)

# Valores medianos de cada variable en cada cluster
# aggregate(pregunta_5, by=list(hcut(pregunta_5,k=3)$cluster), median)

# (ii) [0.25pt] Haga un Análisis Factorial con 2 factores y guarde los scores de ambos
# factores como nuevas variables.

require(psy)
scree.plot(cor(pregunta_5))

# Análisis factorial
model.fa <- factanal(pregunta_5, factors=2, scores=c("regression"))
print(model.fa)

# Relación
load <- model.fa$loadings[,1:2] 
km_res <- kmeans(load, centers = 2, nstart = 20)
fviz_cluster(km_res, load, ellipse.type = "norm")

# # PREGUNTA 5: (ii) 0.25pt -----------------------------------------------


# Se guardan los scores más relevantes del FA para usarlo posteriormente
Xnew.fa <- model.fa$scores[,1:2]
summary(Xnew.fa)

# Creando variables "índices"
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

Xnew.fa <- apply(Xnew.fa,2,normalize)
summary(Xnew.fa)


# PREGUNTA 5: (iii) 0.25pt ------------------------------------------------



clust.jer <- hclust(dist(Xnew.fa))

plot(clust.jer, labels = pregunta_5$labs, cex = 0.5, 
     main = "Clust. jerárquico sobre componentes principales")



