
# NOMBRE: 
#Ramon Ya�ez
#Patricio Zapata

####### PREGUNTA 1 #######
# 0.75pt
# Respuesta: (b) Underfitting
# Justificaci�n: Tenemos underfitting cuando los datos son pocos y no se puede generalizar el conocimiento.


####### PREGUNTA 2 #######
# 0.75pt
# Respuesta: Verdadero
# Justificaci�n: Al tener mas profundidad, el ajuste con la data de entrenamiento calza 
# plenamente pero cuando queremos generalizar fallar� asi mismo como hacer una 
# predicci�n con otra data o validaci�n cruzada


####### PREGUNTA 3 #######

mi_df <- data.frame(
  "Edad" = c("18-40", "18-40", "18-40", "18-40", "18-40", "18-40", "18-40", "41-60", "41-60" , "41-60" , "41-60" , "41-60" , "41-60" , "41-60" , ">60" , ">60", ">60", ">60", ">60", ">60", ">60" ), 
  "Combo" = c("Fiesta", "Supremo", "Supremo", "Fiesta", "Supremo", "Mediano", "Supremo", "Mediano", "Mediano" , "Fiesta" , "Supremo" , "Fiesta" , "Supremo" , "Supremo" , "Fiesta" , "Mediano", "Mediano", "Fiesta", "Fiesta", "Supremo", "Supremo" ) 
)

# (i) 0.25pt
glimpse(mi_df)
new <-  table(mi_df)
datos <- prop.table(new)

# (ii) 0.25pt
mayor_60 <-  sum(datos[1,])
mayor_60

entre_18_40  <-  sum(datos[2,])
entre_18_40

entre_41_60  <- sum(datos[3,])
entre_41_60


Fiesta  <- sum(datos[,1])
Fiesta

Mediano  <- sum(datos[,2])
Mediano

Supremo  <- sum(datos[,3])
Supremo


# (iii) 1.00pt


# p(fiesta | mayor a 60 ) 
#    A           B
# = 
# ( p(mayor_60 | Fiesta) * P(fiesta) )  / P(mayor_60)
mayor_60_Fiesta <- datos[1,1]
(mayor_60_Fiesta * Fiesta) / mayor_60


# p(mediano | mayor a 60 ) 
#    A           B
# = 
# ( p(mayor_60 | Mediano) * P(Mediano) )  / P(mayor_60)
mayor_60_Mediano <- datos[1,2]
(mayor_60_Mediano * Mediano) / mayor_60


# p(supremo | mayor a 60 ) 
#    A           B
# = 
# ( p(mayor_60 | supremo) * P(supremo) )  / P(mayor_60)
mayor_60_Supremo <- datos[1,3]
(mayor_60_Supremo * Supremo) / mayor_60



# p(fiesta | 18_40 ) 
#    A           B
# = 
# ( p(18_40 | Fiesta) * P(fiesta) )  / P(18_40)
entre_18_40_Fiesta <- datos[2,1]
(entre_18_40_Fiesta * Fiesta) / entre_18_40


# p(mediano | 18_40 ) 
#    A           B
# = 
# ( p(18_40 | mediano) * P(mediano) )  / P(18_40)
entre_18_40_mediano <- datos[2,2]
(entre_18_40_mediano * Mediano) / entre_18_40


# p(supremo | 18_40 ) 
#    A           B
# = 
# ( p(18_40 | supremo) * P(supremo) )  / P(18_40)
entre_18_40_supremo <- datos[2,3]
(entre_18_40_supremo * Supremo) / entre_18_40




# p(fiesta | 41_60 ) 
#    A           B
# = 
# ( p(41_60 | Fiesta) * P(fiesta) )  / P(41_60)
entre_41_60_Fiesta <- datos[3,1]
(entre_41_60_Fiesta * Fiesta) / entre_41_60


# p(mediano | 41_60 ) 
#    A           B
# = 
# ( p(41_60 | mediano) * P(mediano) )  / P(41_60)
entre_41_60_mediano <- datos[3,2]
(entre_41_60_mediano * Mediano) / entre_41_60


# p(supremo | 41_60 ) 
#    A           B
# = 
# ( p(41_60 | supremo) * P(supremo) )  / P(41_60)
entre_41_60_supremo <- datos[3,3]
(entre_41_60_supremo * Supremo) / entre_41_60


# (iv) 0.25pt
# Le recomendar�a el combo supremo ya que nos da un 24% de probabilidad que lo consuma



####### PREGUNTA 4 #######
# Cargando los datos
# https://archive.ics.uci.edu/ml/datasets/Wine
# install.packages("rattle.data")
library(rattle.data)
data <- wine
glimpse(data)
summary(data)

# (i) 0.25pt
data[,-1] <- as.data.frame(scale(data[,-1]))


# (ii) 0.25pt
set.seed(2020)
posTraining <- sample(1:nrow(data), 0.7*nrow(data))
data_training <- data[posTraining, ]
data_test <- data[-posTraining, ]
nrow(data_training) + nrow(data_test) # Corroboramos 178 filas


# (iii) 0.25pt
library(rpart)
library(rattle)
library(rpart.plot)
library(randomForest)

#set.seed(2020)
modelo.rf <- randomForest(Type ~ ., data=data_training, 
                         ntree=20, mtry=4, replace=TRUE, importance=T)
varImpPlot(modelo.rf)


# SVM
library(e1071)
# set.seed(2020)
modelo.svm <- svm(Type ~ ., data=data_training, kernel="radial")


# (iv) 0.25pt

# RANDOM FOREST Predicciones
# Valores predictivos
predval.rf1 <- predict(modelo.rf, data_training)
# Matriz de confusi�n
table(data_training$Type, predval.rf1)
# % de clasificaci�n correcta
mean(data_training$Type == predval.rf1)

# --> El % de predicci�n respecto a la data de entrenamiento con modelo Random Forest es de 100%


# SVM Predicciones
predval.svm <- predict(modelo.svm, data_training)
# Matriz de confusi�n
table(data_training$Type, predval.svm)
# % de clasificaci�n correcta
mean(data_training$Type == predval.svm)

# --> El % de predicci�n respecto a la data de prueba con modelo Support Vector Machine es de 
# 100% (es claramente un sobreajuste puesto que estamos usando la misma base de datos de entrenamiento).


# (v) 0.25pt
library(caret)
set.seed(2020)

# CV con Random Forest
( cv_rf <- train(Type ~., data=data_training, method="rf", 
                 trControl=trainControl(method="cv", number=30, p=0.7), 
                 tuneGrid=expand.grid(.mtry=3)) )
# La validacion cruzada da un accuracy de 0.993 para Random Forest



# CV con SVM
set.seed(2020)
( cv.svm <- train(Type ~., data=data_training, method="svmRadial",
                  trControl=trainControl(method="cv", number=30, p=0.7)) )
# Con sigma = 0.0830777 and C = 1, me da un accuracy de 0.991



# Se elige algoritmo Random Forest, ya que entrega un accuracy superior al SVM.


# (vi) 0.25pt

# Predicciones con BD de prueba para RANDOM FOREST

# Valores predictivos
predval.rf2 <- predict(modelo.rf, data_test)
# Matriz de confusi�n
table(data_test$Type, predval.rf2)
# % de clasificaci�n correcta
mean(data_test$Type == predval.rf2)

# El % de predicci�n respecto a la data de prueba con modelo Random Forest es de 96%


# Predicciones con BD de prueba para SVM

# Ajustamos el modelo con parametros de Validacion Cruzada.
modelo.svm.ajustado <- svm(Type ~ ., data=data_training, kernel="radial",C=1, sigma=0.0830777 )


# SVM Predicciones
predval.svm2 <- predict(modelo.svm.ajustado, data_test)
# Matriz de confusi�n
table(data_test$Type, predval.svm2)
# % de clasificaci�n correcta
mean(data_test$Type == predval.svm2)

# El % de predicci�n respecto a la data de prueba con modelo Support Vector Machine es de 98% 


# En la pregunta 5, bas�ndonos en accuracy, el Random Forest fue mejor predictor. Pero en la
# pr�ctica haciendo predicciones con base de datos de test, la mejor matriz de confusion la 
# entrega el algoritmo de SVM.



####### PREGUNTA 5 #######

######################## CLUSTERING ##########################
require(factoextra)
require(NbClust)
library(cluster)

# Pregunta 5: Tambi�n usando la base de datos wine, desarrolle los siguientes �tems:

#   (i) [0.25pt] Aplique el Clustering Jer�rquico con k=3 y linkage "ward.D". Visualice el
# resultado final con un gr�fico dendrograma.


data_5 <-  data[,-1]

# Matriz de distancias eucl�deas
d <- dist(data_5, method="euclidean")

# Dendrograma est�ndar
fviz_dend(hcut(data_5, k=3, hc_method="ward.D"), rect=TRUE)

# (ii) [0.25pt] Haga un PCA con 2 componentes principales y guarde los scores de ambos
# componentes como nuevas variables.

# PCA
model_pca <- prcomp(data_5)
model_pca$rotation[,1]

## Seleccion de componentes
# Escojo 2 componentes principales
fviz_eig(model_pca) 
summary(model_pca) 


## Grafico de contribucicion  de las variables en PC1 y PC2
fviz_pca_var(model_pca)
fviz_pca_ind(model_pca)


# contribucicion de cada variable en las componentes principales
fviz_contrib(model_pca, choice = "var" , axes = 1)
fviz_contrib(model_pca, choice = "var" , axes = 2)



# Obtener componentes principales para cada observacion
componentes_principales <- as_tibble(model_pca$x[,1:2]) 

 
# (iii) [0.25pt] Utilice los scores del �tem anterior para graficar los datos y pinte cada punto
# seg�n la agrupaci�n calculada con el clustering jer�rquico del �tem (i).

# Grafico de visualizacion de clusters
grafico_cluster <- fviz_cluster(model_hcut, data = data_5)

# (iv) [0.50pt] Haga el mismo gr�fico del �tem anterior pero ahora pinte los puntos seg�n el
# tipo de vino (variable Type). Compare este gr�fico con el del �tem (iii) y concluya si la
# agrupaci�n del clustering jer�rquico podr�a ser un buen clasificador de vinos
library(dplyr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

componentes_principales['type'] = data$Type

graficos_componente_type <- ggplot(data = componentes_principales, aes(x=PC1, y=PC2, color=type )) +
  geom_point()



# comparacion del grafico_cluster y graficos_componente_type 

grid.arrange(grafico_cluster,graficos_componente_type,  ncol=2)


# se puede apreciar que ambos graficos son muy similares, pero comparando las clasificaciones de tipo vs  las clasificaciones de cluster se puede  ver una peque�as diferencias.
# se podria decir que el cluster de vinos clasifica bien

