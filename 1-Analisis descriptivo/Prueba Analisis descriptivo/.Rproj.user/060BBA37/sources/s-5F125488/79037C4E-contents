#install.packages("nortest")
library(tidyverse)
library(dplyr)
library(ggplot)
library(readr)
library(corrplot)
library(lmtest)
library(nortest)
library(GGally)

#install.packages("GGally")
peli <- read_csv("peli.csv")
View(peli)

### 1 Use las herramientas de dplyr para crear un nuevo data frame (df):

# Con las siguientes variables: nombre de la película, nombre del director,
# nombre primer actor, número de críticas positivas, duración, cantidad
# bruta recaudada, lenguaje, presupuesto, año, puntuación IMDB, likes de la
# película y género predominante.
# - Elimine del df todas las observaciones que tengan alguna casilla vacía.
# - Ordénelo por año, del más reciente al más antiguo.


data <-  data.frame(peli$Nombre_Pelicula,peli$Nombre_Director,peli$Nombre_Primer_Actor,peli$Criticas_Positivas,peli$Duracion,peli$Cantidad_Bruta_Recaudada,peli$Lenguaje,peli$Presupuesto,peli$Año,peli$Puntuacion_IMDB,peli$Likes_Pelicula,peli$Genero_Predominante)
str(data)
sapply(data, function(x) sum(is.na(x)))
datos <- na.omit(data)
sapply(datos, function(x) sum(is.na(x)))
view(datos)

Muestra <- datos %>% 
  dplyr::arrange(datos$peli.Año) 

# Pregunta 2: Como interesa estudiar la puntuación de la película, debe
# transformar la variable Puntuacion_IMDB, ya que esta cuenta con un formato
# no numérico. Cree una variable llamada Puntuacion que tome solo la
# puntuación en formato numérico a partir de Puntuacion_IMDB. Elimine la
# variable Puntuacion_IMDB de su df.
# Finalmente, construya el objeto prom_puntuacion que contenga la
# puntuación promedio para cada tipo de género predominante.

str(Muestra)
#view(Muestra)


DOS <- Muestra %>% 
    dplyr::mutate(peli.Puntuacion_IMDB   = gsub(" , ", "", peli.Puntuacion_IMDB ),
                  peli.Puntuacion_IMDB   = gsub(" puntos", "", peli.Puntuacion_IMDB ),
                Puntuacion = as.numeric(peli.Puntuacion_IMDB ))
muestra2 <- select (DOS,-peli.Puntuacion_IMDB)

str(muestra2)

ptt_promedio <- muestra2 %>% 
  dplyr::group_by(peli.Genero_Predominante ) %>% 
  dplyr::summarise(Puntuacion = round(mean(Puntuacion, na.rm = TRUE), 0))

DT::datatable(ptt_promedio)



########################################
# Pregunta 3: Use la librería ggplot2 para construir un gráfico de la 
# distribución de la variable Puntuacion y un gráfico que permita comparar la 
# distribución de la puntuación por género predominante. Comente.

ggplot(data = muestra2, aes(x = Puntuacion)) +
  geom_histogram(bins = 50, alpha = 0.8, color = "white", fill = "turquoise") +
  xlab("Puntuacion") +
  ylab("Frecuencias") +
  ggtitle("Distribución de Puntuacion") +
  theme(plot.title = element_text(hjust = 0.5))


#la categoria mas heterogeneas a los datos documenary y  y las mas homoe4nea  thriller
ggplot(data = muestra2, aes(x = Puntuacion, y = peli.Genero_Predominante, fill = Puntuacion)) +
  geom_boxplot() +
  coord_flip()+
  theme(legend.position="none")


ggplot(data = muestra2, aes(y = Puntuacion, fill = factor(peli.Genero_Predominante))) +
  geom_boxplot(outlier.shape = NA) 



##################################
# Pregunta 4: Analice de manera gráfica la correlación de Puntuacion con 
tres = na.omit(muestra2[, -c(1,2,3,7,11)])
cor_tres = cor(tres)
cor_tres
corrplot(cor_tres)

# el resto de las variables cuantitativas. Además, analice a través de una 
# matriz gráfica, la dispersión y distribución de las variables. Comente.
str(muestra2)




ggpairs(tres,
        axisLabels = "none")
#Cantidad bruta hay tendencia lineales,Peli presupuesto tambien .Peli. Año.


#g2 <- ggplot(data = viv, aes(x = longitud, y = latitud,  col = prox_oceano))
#g2 + geom_point()


#######################################################
# Pregunta 5: Ajuste un modelo de regresión lineal múltiple para predecir 
# la puntuación en función de las variables Duración, Número de críticas
# positivas y género predominante. Entregue un análisis simple de los residuos
# y comente si es un modelo adecuado.
str(muestra2)
df <- muestra2

modelo <- step(lm(Puntuacion ~peli.Duracion +peli.Criticas_Positivas+peli.Genero_Predominante, data = df), direction = "both")

summary(modelo)
#Encontramos un modelo que utiliza 1 variables, de las cuales 1 son continuas y dos categorica
#shapiro
shapiro.test(modelo)
#lillie.test
nortest::lillie.test(modelo)
#El test de Breusche-Pagan tiene por hipotesis:
bptest(modelo)
#El test si rechaza la homocedasticidad de los residuos.
#Independencia de los residuos
dwtest(modelo)
#peli.Criticas_Positivas  0.67282  , es el valor de la pendiente e indica el cambio en puntuaron por el cambio en peli.Criticas_Positivas 
#El test rechaza rechaza la ausencia de autocorrelación
#En conclusión, el modelo no cumple los supuestos estadísticos básicos, por lo que no seria apropiado su uso y debe modificar.



#########################################################
#Pregunta 6: Cree una función que reciba el nombre del primer actor y retorne:
# - Un listado de las películas donde participó como primer actor ordenadas 
#    por puntuación más alta.
# - La cantidad de películas donde participó por género.
# - Un gráfico de la frecuencia de los géneros donde participa.
#
### Pruebe su función con el actor Leonardo DiCaprio.
#########################################################
str(muestra2)

search_ <- function(palabra) {
  indice <- grep(palabra, muestra2$peli.Nombre_Primer_Actor)
  tabla <- data %>% 
    dplyr::slice(indice) %>% 
    dplyr::count(peli.Genero_Predominante   ) %>% 
    dplyr::rename(Clasificacion = peli.Genero_Predominante   ,
                  Numero = n)
  #tabla$Numero ,tabla$Clasificacion
  str(tabla)
  g1 <- ggplot(data = tabla, aes(x = tabla$Numero ) ) +
    geom_histogram(bins = 30, color = "white", fill = "turquoise") +
    xlab("géneros donde participa") + ylab("cantidad de películas") +
    ggtitle(paste("frecuencia  de la géneros", tabla$Clasificacion)) +
    theme(plot.title = element_text(hjust = 0.5))
    print(g1)
    return(tabla)
  

}

knitr::kable(search_("Leonardo DiCaprio"))



