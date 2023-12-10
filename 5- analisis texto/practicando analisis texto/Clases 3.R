## Diplomado en Data Science 2020
## Módulo: Análisis de textos con R
## Prof: Riva Quiroga
## Script sesión 3 (10 de noviembre)

## Los paquetes que vamos a utilizar ----

library(rvest)
library(robotstxt)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(readr)
library(tidytext)
library(topicmodels)
library(ggplot2)
#install.packages("get_robotstxt")
library(get_robotstxt)


## Obtener los datos ----
#get_robotstxt("https://www.amazon.com")
get_robotstxt("https://www.latercera.com/etiqueta/tecnologia/page/1/")


enlace_p1 <- read_html("https://www.latercera.com/etiqueta/tecnologia/page/1/")

texto_titulares <- enlace_p1 %>% 
  html_nodes("h3") %>% 
  html_text(trim = TRUE) %>% 
  .[-1]

enlace_titulares <- enlace_p1 %>% 
  html_nodes("h3 > a") %>% 
  html_attr("href")

tibble(titular = texto_titulares,
       enlace_noticia = enlace_titulares) %>% 
  mutate(enlace_noticia = paste0("https://www.latercera.com", enlace_noticia)) %>% 
  separate(col = titular,
           into = c("seccion", "titular"),
           sep = "  ",
           extra = "merge") 


## vamos a convertir este código en una función

obtener_titulares <- function(numero_pagina) {
  
  Sys.sleep(5)
  
  enlace <- paste0("https://www.latercera.com/etiqueta/tecnologia/page/", numero_pagina)
  
  html <- read_html(enlace)
  
  texto_titulares <- html %>% 
    html_nodes("h3") %>% 
    html_text(trim = TRUE) %>% 
    .[-1]
  
  enlace_titulares <- html %>% 
    html_nodes("h3 > a") %>% 
    html_attr("href")
  
  tibble(titular = texto_titulares,
         enlace_noticia = enlace_titulares) %>% 
    mutate(enlace_noticia = paste0("https://www.latercera.com", enlace_noticia)) %>% 
    separate(col = titular,
             into = c("seccion", "titular"),
             sep = "  ",
             extra = "merge") 
  
}

# probar con un titular
obtener_titulares(17)

### identificar el número total de páginas (PENDIENTE)

# iterar

map(1:3, obtener_titulares)

map_df(1:3, obtener_titulares)


# Si tuviésemos tiempo, ejecutaríamos lo siguiente:

# titulares_tecnologia <- map_df(1:152, obtener_titulares)

titulares_tecnologia <- read_csv("https://bit.ly/titulares-tecnologia-latercera")

# read.csv(url, encoding = "UTF-8")
# read.csv("https://raw.githubusercontent.com/rivaquiroga/datos-clases/main/dipDS_2020/2020-11-10_titulares-tecnologia.csv?token=AHPXJMFJEKM3BPGY6Q5VZ4K7WQZQI",fileEncoding="utf-8") # por si tienen problemas con la codificación de caracteres

head(titulares_tecnologia)

# veamos cuáles son los bigramas más frecuentes -----

# importar una lista de stopwords 

algunas_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

mas_stopwords <- tibble(palabra = c("va", "hace", "hacer", "ser", "puede", "tiene", "dice"))

las_stopwords <- bind_rows(algunas_stopwords, mas_stopwords)


titulares_tecnologia %>% 
  unnest_tokens(input = titular,
                output = palabra,
                token = "ngrams",
                n = 2) %>% 
  filter(!is.na(palabra)) %>% 
  count(palabra, sort = TRUE) %>% 
  separate(col = palabra,
           into = c("palabra_1", "palabra_2"),
           sep = " ") %>% 
  filter(!palabra_1 %in% las_stopwords$palabra) %>% 
  filter(!palabra_2 %in% las_stopwords$palabra) 


# argumento de encoding fileEncoding / iconv() / str_conv()

noticias <- read_csv("http://bit.ly/500-noticias-tecnologia")

head(noticias)
View(noticias)

frecuencias_noticias <- noticias %>% 
  mutate(documento = 1:nrow(noticias), .before = fecha) %>% 
  unnest_tokens(input = texto_noticia,
                output = palabra) %>% 
  anti_join(las_stopwords) %>% 
  count(documento, palabra, sort = TRUE)

frecuencias_noticias %>% 
  filter(palabra == "henry")

noticias_dtm <- frecuencias_noticias %>% 
  cast_dtm(documento, palabra, n)

noticias_lda <- LDA(noticias_dtm, k = 6, control = list(seed = 1234))


noticias_tema <- tidy(noticias_lda, matrix = "beta")

noticias_tema %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 5) %>% 
  arrange(topic, -beta) %>% 
  ggplot(aes(y = term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free")
#https://vas3k.com/blog/machine_learning/ 

# anaklisis supervissado  https://vas3k.com/blog/machine_learning/https://smltar.com/ 
#  https://quanteda.io/ quiero entender el concepto de una palabra 

  


