#################### LECTURA DE LOS DATOS ####################
# Datos
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra
# install.packages("cluster")
#library(cluster)

# URL de los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv"

# Cargar datos
data <- read.csv(url, header=TRUE, sep=",")
data$Classification <- as.factor(data$Classification-1)
data.orig <- data
##############################################################


################# NORMALIZACI�N DE LOS DATOS #################
# normalize <- function(x){
#   return( (x-min(x))/(max(x)-min(x)) )
# }
# data[,-10] <- as.data.frame(lapply(data[,-10],normalize))

# Z-score = (x-mean(x))/sd(x) # ESTANDARIZAR...
data[,-10] <- as.data.frame(scale(data[,-10]))
##############################################################


######################### K-MEANS ############################
# K-MODES: funci�n kmodes en la librer�a klaR

# Determinar el mejor n�mero de clusters basado en 30 �ndices
require(NbClust)
set.seed(2020)
x11()
k.nbclust <- NbClust(data=data[,-10], distance="euclidean", max.nc=15, method="kmeans", index="alllong")
require(factoextra)
fviz_nbclust(k.nbclust) + theme_minimal()

# centers=n�mero de clusters; nstart=n�mero de veces que se va a repetir el proceso
set.seed(2020)
model.km <- kmeans(data[,-10], centers=2, nstart=100)
model.km
# summary(model.km)

# Valores medianos de cada variable en cada cluster
aggregate(data.orig[,-10], by=list(model.km$cluster), median)
##############################################################




######################## CLUSTERING ##########################
require(factoextra)
# Matriz de distancias eucl�deas
d <- dist(data[,-10], method="euclidean")

# Dendograma con dos m�todos
model.cl.w <- hclust(d, method="ward.D") 

model.cl.c <- hclust(d, method="complete") 

# Comparaci�n de m�todos a trav�s de la similitud de observaciones
cor(d,cophenetic(model.cl.w))
cor(d,cophenetic(model.cl.c))

# Dendrograma
fviz_dend(x=hcut(data[,-10],k=1))

# Determina el mejor n�mero de clusters basado en 30 �ndices
require(NbClust)
set.seed(2020)
k.nbclust <- NbClust(data[,-10], distance="euclidean", max.nc=15, method="complete", index="alllong") 
fviz_nbclust(k.nbclust) + theme_minimal()

# Dendrograma est�ndar
fviz_dend(hcut(data[,-10],k=2), rect=TRUE)

# Dendrograma en forma circular
fviz_dend(hcut(data[,-10],k=2), type="circular")

# Dendrograma en forma de �rbol filogen�tico
require(igraph)
fviz_dend(hcut(data[,-10],k=2), type="phylogenic", repel=TRUE)

# Valores medianos de cada variable en cada cluster
aggregate(data.orig[,-10], by=list(hcut(data[,-10],k=2)$cluster), median)
##############################################################
