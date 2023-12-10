#################### LECTURA DE LOS DATOS ####################
# Datos
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

# URL de los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv"

# Cargar datos
data <- read.csv(url, header=TRUE, sep=",")
data$Classification <- as.factor(data$Classification-1)
data.orig <- data
##############################################################


################# NORMALIZACIÓN DE LOS DATOS #################
# normalize <- function(x){
#   return( (x-min(x))/(max(x)-min(x)) )
# }
# data[,-10] <- as.data.frame(lapply(data[,-10],normalize))

# Z-score = (x-mean(x))/sd(x)
data[,-10] <- as.data.frame(scale(data[,-10]))
##############################################################


######################### K-MEANS ############################
# K-MODES: función kmodes en la librería klaR

# Determinar el mejor número de clusters basado en 30 índices
require(NbClust)
set.seed(2020)
k.nbclust <- NbClust(data=data[,-10], distance="euclidean", max.nc=15, method="kmeans", index="alllong")
fviz_nbclust(k.nbclust) + theme_minimal()

# centers=número de clusters; nstart=número de veces que se va a repetir el proceso
set.seed(2020)
model.km <- kmeans(data[,-10], centers=2, nstart=100)
model.km

# Valores medianos de cada variable en cada cluster
aggregate(data.orig[,-10], by=list(model.km$cluster), median)
##############################################################




######################## CLUSTERING ##########################
require(factoextra)

# Dendrograma
fviz_dend(x=hcut(data[,-10], k=1, hc_method="ward.D2"))

# Determina el mejor número de clusters basado en 30 índices
require(NbClust)
set.seed(2020)
k.nbclust <- NbClust(data[,-10], distance="euclidean", max.nc=15, method="ward.D2", index="alllong") 
fviz_nbclust(k.nbclust) + theme_minimal()

# Dendrograma estándar
fviz_dend(hcut(data[,-10], k=3, hc_method="ward.D2"), rect=TRUE)

# Dendrograma en forma circular
fviz_dend(hcut(data[,-10], k=3, hc_method="ward.D2"), type="circular")

# Dendrograma en forma de árbol filogenético
require(igraph)
fviz_dend(hcut(data[,-10], k=3), type="phylogenic", repel=TRUE)

# Valores medianos de cada variable en cada cluster
aggregate(data.orig[,-10], by=list(hcut(data[,-10],k=3)$cluster), median)
##############################################################
