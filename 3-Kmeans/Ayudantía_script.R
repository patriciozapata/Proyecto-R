library(tidyverse)
library(readxl)

# Carga de bases de datos
World <- read_excel("C:/Users/patri/OneDrive/Escritorio/Ayudantía 7 2020/World.xlsx")
mushrooms <- read_excel("C:/Users/patri/OneDrive/Escritorio/Ayudantía 7 2020/mushrooms.csv")

str(World)

# Manipular la base de datos

## Seleccionar solo variables continuas y escalar
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

World_1 <- World %>% 
  select(-País, -`Intervención ONU`) %>% 
  scale()

World_2 <- World %>% 
  select(-País, -`Intervención ONU`) %>% 
  apply(2,normalize)



# K-means -----------------------------------------------------------------


library(factoextra)
library(cluster)
library(NbClust)

# World_1

## Buscar el k Optimo
n_cluster1 <- NbClust(World_1, method = "kmeans") # Sugiere el valor 3
set.seed(2020)
model_kmeans <- kmeans(World_1, centers = 3)


## VisualizaciÃ³n
World_1 <- World_1 %>% 
  as_tibble()

str(World)

# graficar los cluster 
World_1 %>% 
  mutate(etiquetas = model_kmeans$cluster) %>% 
  ggplot() + geom_point(aes(x = `Tasa de mortalidad infantil (???)`, 
                            y =`Tasa de natalidad (??? hab.)`,
                            col = as_factor(etiquetas))) +
  theme_light()

# World_2
n_cluster1 <- NbClust(World_2, method = "kmeans") # Sugiere el valor 3
set.seed(2020)
model_kmeans <- kmeans(World_2, centers = 3)

## VisualizaciÃ³n
World_2 <- World_2 %>% 
  as_tibble()

World_2 %>% 
  mutate(etiquetas = model_kmeans$cluster) %>% 
  ggplot() + geom_point(aes(x = `Tasa de mortalidad infantil (???)`, 
                            y =`Tasa de natalidad (??? hab.)`,
                            col = as_factor(etiquetas))) +
  theme_light()



# Clustering Jeraquico ----------------------------------------------------


World_11 <- as.matrix(World_1)
rownames(World_11) <- World$País

## Escogemos el K Optimo
n_cluster1 <- NbClust(World_1, distance = "euclidean", 
                     max.nc = 15, method = "complete", index="alllong") # k = 3

## Ajustamos clustering
d <- daisy(World_11, metric = "euclidean")
model_hclust <- hclust(d, method = "complete")
model_hcut <- hcut(d, k = 3, hc_method = "complete")
table(model_hcut$cluster)

## Visualzacion ³n
fviz_dend(model_hclust, k = 3)
fviz_dend(model_hcut, k = 3) # Recomendado este !!

# PCA
model_pca <- prcomp(World_11)
model_pca$rotation[,1]

## Seleccion de componentes
fviz_eig(model_pca) # Escojo 2 componentes principales
summary(model_pca) # Si fijo % en 80, entonces escojo 3 componentes

## Grafico de contribucicion  de las variables en PC1 y PC2
fviz_pca_var(model_pca)
fviz_pca_ind(model_pca)

# contribucicion de cada variable en las componentes principales
fviz_contrib(model_pca, choice = "var" , axes = 1)
fviz_contrib(model_pca, choice = "var" , axes = 2)

# Grafico de visualizacion de clusters
fviz_cluster(model_hcut, data = World_11)

# Obtener componentes principales para cada observacion
New_data <- as_tibble(model_pca$x[,1:2]) %>% 
  mutate(etiquetas = model_kmeans$cluster)

ggplot(New_data) +
  geom_point(aes(x = PC1, y = PC2, col = as_factor(etiquetas)))
