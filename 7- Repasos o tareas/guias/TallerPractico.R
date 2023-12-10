library(tidyverse)
library(readxl)
playstore <- read_excel("~/DiplomadoDataScience/Clase-20200707/playstore.xlsx", 
                        sheet = "APPS")

nrow(playstore)

#Pregunta 1 # ----

set.seed(772020)
id <- sample(1:nrow(playstore), 200, replace = FALSE) 
id

playstore_muestra <- playstore[id,]
playstore_muestra

# Forma tidyverse #
set.seed(772020)
playstore_muestra2 <- sample_n(playstore,200, replace = FALSE)
playstore_muestra2


#Pregunta 2 # -----

View(playstore_muestra)
# a) # 
# Utilizamos tipo y clasificacion
tabla1 <- table(playstore_muestra$Tipo, playstore_muestra$Clasificación)
prop.table(tabla1,1)
proportions(tabla1,1) #El 84% de las obs. son gratis para todo publico

#forma tidyverse #

playstore_muestra %>% 
  select(Tipo, Clasificación) %>% 
  table() %>% 
  proportions(margin = 1) #El 84% de las apps que  son gratis son  para todo publico

## b) ###
playstore_muestra$indicador <- ifelse(playstore_muestra$Valoración > 4,1,0)

tabla2 <- table(playstore_muestra$Tipo, playstore_muestra$indicador)
tabla2_NA <- table(playstore_muestra$Tipo, playstore_muestra$indicador,
                   useNA = "always")
prop.table(tabla2) # 70% de las apps son gratis y tienen valoración > 4
prop.table(tabla2_NA) # 58% de las apps son gratis y tienen valoración > 4

# Sin utilizar el indicador
table(playstore_muestra$Tipo, playstore_muestra$Valoración > 4)

# Tidyverse #
playstore_muestra %>% 
  mutate(indicador = ifelse(Valoración > 4, 1, 0)) %>% 
  select(Tipo, indicador) %>% 
  table() %>% 
  prop.table()

## c) ###
tabla3 <- table(playstore_muestra$Clasificación != "Todo público")
## TRUE = No son todo público
proportions(tabla3) ## 15.5% de las apps no son de todo público

## Tidyverse
playstore_muestra %>% 
  group_by(Clasificación != "Todo público") %>% 
  summarise(conteo = n()) %>% 
  select(conteo) %>% 
  proportions()


### d) ###

tabla4 <- table(playstore_muestra$Categoría == "COMPRAS", 
      playstore_muestra$`Tamaño(M)` > 5 & playstore_muestra$`Tamaño(M)` < 10)
proportions(tabla4,1) # El 0% de las compras tienen un peso entre 5 y 10 megas

tabla5 <- table(playstore$Categoría == "COMPRAS", 
                playstore$`Tamaño(M)` > 5 & playstore$`Tamaño(M)` < 10)
proportions(tabla5,1) # El 0% de las compras tienen un peso entre 5 y 10 megas

### e) ###

playstore_muestra %>% 
  select(Tipo, Clasificación) %>% 
  table() %>% 
  proportions(margin = 1) #El 84% de las apps que  son gratis son  para todo publico

playstore %>% 
  select(Tipo, Clasificación) %>% 
  table() %>% 
  proportions(margin = 1) #El 80% de las apps que  son gratis son  para todo publico



## Problema 3 -----
## a) #
t.test(playstore_muestra$Valoración, conf.level = 0.95)$conf.int[1:2]
# Un 95 de las veces el promedio de valoración se encontrará en el intervalo [4.1, 4.26]

## b) #
t.test(playstore_muestra$`Tamaño(M)`[playstore_muestra$Tipo == "Gratis"], conf.level = 0.95)$conf.int
# Un 95% de las veces el promedio del tamaño de las apps gratis se encontrará en el intervalo [23.13, 49.07]

t.test(playstore_muestra$`Tamaño(M)`[playstore_muestra$Tipo == "Pago"], conf.level = 0.95)$conf.int
# Un 95% de las veces el promedio del tamaño de las apps gratis se encontrará en el intervalo [-39.29, 317.71]

## c) ##

prop.test(sum(playstore_muestra$Clasificación == "Todo público"), 
          nrow(playstore_muestra), conf.level = 0.95, correct = FALSE)$conf.int[1:2]
#Un 95% de las veces la proporción de apps de Todo publico se encontrarán en el intervalo [0.78, 0.88]

## Otra forma
prop.test(table(playstore_muestra$Clasificación !="Todo público"), 
           conf.level = 0.95, correct = FALSE)
#Un 95% de las veces la proporción de apps de Todo publico se encontrarán en el intervalo [0.78, 0.88]


## d) ##
# Estimador puntual de la media de valoración
mean(playstore$Valoración, na.rm = TRUE) # Si está en el intervalo de confianza

# Estimador puntual de la media del tamaño según tipo
aggregate(playstore$`Tamaño(M)` ~ playstore$Tipo, FUN = mean) # Ambos están !

playstore %>% 
  group_by(Tipo) %>% 
  summarise(promedio = mean(`Tamaño(M)`))

# Estimador puntual de la proporción de apps de todo público
mean(playstore$Clasificación == "Todo público") # Si esta en el intervalo !

playstore %>% 
  group_by(Clasificación) %>% 
  summarise(conteo = n()) %>% 
  mutate(prob = conteo/sum(conteo))

## Problema 4 ----

## a) ##

# H0: Promedio tamaño <= 27 vs H1: Promedio tamaño > 27
t.test(playstore_muestra$`Tamaño(M)`, mu = 27, alternative = "greater", conf.level = 0.9)

t.test(playstore_muestra$`Tamaño(M)`, mu = 27, alternative = "greater", conf.level = 0.9)$p.value < 0.1

#Hay evidencia estadística suficiente para decir que el promedio del tamaño es mayor a 27
# sujeto a 10% de significancia

## b) ##
# H0: Promedio tamaño gratis = 50 vs H1: Promedio tamaño gratis != 50
t.test(playstore_muestra$`Tamaño(M)`[playstore_muestra$Tipo == "Gratis"], 
       mu = 50, alternative = "two.sided", conf.level = 0.9)$p.value < 0.1
#Hay evidencia estadística suficiente para decir que el promedio del tamaño de las apps gratis es distinto a 50,
# sujeto a 10% de significancia

## c) ##
# H0: Promedio tamaños adolescentes >= 30 vs H1: Promedio tamaños adolescentes < 30
t.test(playstore_muestra$`Tamaño(M)`[playstore_muestra$Clasificación == "Adolescentes"], 
       mu = 30, alternative = "less", conf.level = 0.9)$p.value < 0.1
# NO Hay evidencia estadística suficiente para decir que el promedio del tamaño de las apps 
# para adolscentes es menor a 30
# sujeto a 10% de significancia


## d) ##
# H0: Proporción de comentarios20 = 0.7 vs H1: Proporcion de comentarios20 != 0.7 
prop.test(sum(playstore_muestra$Comentarios > 20), nrow(playstore_muestra),
          p = 0.7, alternative = "two.sided", conf.level = 0.9)$p.value < 0.1
# No Hay evidencia estadística suficiente para decir que la proporción de las apps con más de 20 comentarios
# es distinta de 0.7 sujeto al 10% de significancia


## e) ##
# H0: Promedio Valoracion social = promedio valoracion juego vs
# H1: Promedio Valoracion social != promedio valoracion juego 

# Comparacion de varianza
var.test(playstore$Valoración[playstore$Categoría == "SOCIAL"],
         playstore$Valoración[playstore$Categoría == "JUEGO"], 
         conf.level = 0.9)$p.value < 0.1

# Hay evidencia estadística para decir que las varianzas son diferentes en
# ambas categorías

# Comparación de medias 
t.test(playstore$Valoración[playstore$Categoría == "SOCIAL"],
       playstore$Valoración[playstore$Categoría == "JUEGO"], 
       conf.level = 0.9, alternative = "two.sided", var.equal = FALSE)$p.value < 0.1

# Hay evidencia estadística para decir que el promedio de 
# la valoración de las apps sociales es igual al del de las 
# apps de Juego, sujeto a un 10% de significancia.
