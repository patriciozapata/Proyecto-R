#################### LECTURA DE LOS DATOS ####################
# Datos
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

# URL de los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00451/dataR2.csv"

# Cargar datos
data <- read.csv(url, header=TRUE, sep=",")
##############################################################


################# NORMALIZACIÓN DE LOS DATOS #################
# normalize <- function(x){
#   return( (x-min(x))/(max(x)-min(x)) )
# }
# data[,-10] <- as.data.frame(lapply(data[,-10],normalize))

# Z-score = (x-mean(x))/sd(x)
data[,-10] <- as.data.frame(scale(data[,-10]))
##############################################################


########## ANÁLISIS DE COMPONENTES PRINCIPALES (PCA) #########
model.pca <- prcomp(data[,-10])
summary(model.pca)

# Dibujar la varianza que explica cada componente (Scree plot)
require(factoextra)
fviz_eig(model.pca, choice="eigenvalue")

# Gráfico de la contribución de cada variable en los ejes PC1 y PC2
fviz_pca_var(model.pca, col.var="contrib", gradient.cols = c("#00AFBB","#E7B800","#FC4E07"), repel=TRUE)

# Gráfico de cada individuo en los ejes PC1 y PC2
fviz_pca_ind(model.pca)

# Se guardan los componentes más relevantes del PCA para usarlo posteriormente
Xnew.pca <- model.pca$x[,1:2]
##############################################################


################### ANÁLISIS FACTORIAL (FA) ##################
# Dibujar la varianza que explica cada factor (Scree plot)
require(psy)
scree.plot(cor(data[,-10]))

# Análisis factorial
model.fa <- factanal(data[,-10], factors=4, scores=c("regression"))
print(model.fa)

# Relación
load <- model.fa$loadings[,1:2] 
plot(load, type="n", xlim=c(-0.1,1)) 
text(load, labels=names(data[,-10]), cex=0.7)

# Se guardan los scores más relevantes del FA para usarlo posteriormente
Xnew.fa <- model.fa$scores[,1:2]
summary(Xnew.fa)

# Creando variables "índices"
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

Xnew.fa <- apply(Xnew.fa,2,normalize)
summary(Xnew.fa)
##############################################################
