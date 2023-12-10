## Diplomado en Data Science 2020
## Módulo: Análisis de textos con R
## Clase 1
## Profesora: Riva Quiroga

# Los paquetes que vamos a utilizar ----
library(pdftools)
library(dplyr)
library(stringr)
library(readr)
library(tidytext)
library(ggplot2)
library(patchwork)
library(topicmodels)


# Descargar el discurso de 2020 de la página de la BCN

download.file("https://obtienearchivo.bcn.cl/obtienearchivo?id=recursoslegales/10221.3/46689/6/20200731.pdf", destfile = "discurso_2020.pdf", mode = "wb") # Si trabajan en Windows agregar el argumento: mode = "wb".

# Convertir este archivo en pdf en un objeto de tipo caracter
discurso_2020 <- pdf_text("discurso_2020.pdf")
discurso_2020

# unir todas las páginas en una sola cadena de caracteres
discurso_2020 <- paste(discurso_2020, collapse = " ")

# primero que vamos a hacer es abstraer nuestro patrón
#   para espacio: forma 1: [:space:] forma 2: \\s
# uno o más de cualquier elemento: +
# también puedo decirle que busque una cantidad específica o un rango: que busque exactamente 14: {14}
# Si yo quiero decirle que busque dos o más espacios {2,}
# para buscar dígitos: [:digit:]
# {1,2}

str_count(discurso_2020, "\n[:space:]{2,}[:digit:]+\n")

# Ahora vamos a eliminar del texto ese patrón

discurso_2020 <- str_remove_all(discurso_2020, "\n[:space:]{2,}[:digit:]+\n")
View(discurso_2020)


#str_replace_all(discurso_2020, "\n", " ") # W: \r\n


frecuencias_2020 <- tibble(discurso = discurso_2020) %>% 
  unnest_tokens(input = discurso, output = palabra, strip_numeric = TRUE) %>% 
  slice(-1:-30) %>% 
  count(palabra, sort = TRUE)

# vamos a sacar las stopwords. 
# algunos stopword (lista palabras que se pude sacar):
# https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt


unas_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

# stopworfd personalizado 
mis_stopwords <- tibble(palabra = c("va", "tenemos"))


frecuencias_2020 %>% 
  anti_join(unas_stopwords) %>%  # saca los valores que conincide
  anti_join(mis_stopwords) %>% 
  filter(!str_detect(palabra, regex("chile", ignore_case = TRUE)))# regenx para sacar mayuscula 





