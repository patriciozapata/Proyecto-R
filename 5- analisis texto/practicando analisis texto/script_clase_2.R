# install.packages("tidyverse")
# install.packages("pdftools")
# install.packages("tidytext")
# install.packages("patchwork")


# install.packages("wordcloud")
# install.packages("wordcloud2")
# install.packages("SnowballC")
# install.packages("udpipe")
library(wordcloud)
library(wordcloud2)
library(SnowballC)
library(udpipe) # lematizar (?)



library(tidyverse)



library(pdftools) # me permite leer PDF que tienen OCR o texto que el pdf lo reconoce per se...
library(dplyr)
library(stringr)
library(readr)
library(tidytext)
library(ggplot2)
library(patchwork)

###--- Descargar discurso

download.file("https://obtienearchivo.bcn.cl/obtienearchivo?id=recursoslegales/10221.3/46689/6/20200731.pdf",
              destfile = "discurso2020.pdf", 
              mode = "wb")
# si trabajan en windows, agregar el wb...

# Convertir archivo en objeto tipo caracter..
discurso_2020 <- pdf_text("discurso2020.pdf")


# Unir todas las paginas
discurso_2020 <- paste(discurso_2020, collapse = " ")


# ahora eliminar los \r\n
# abstraer patron y usar expresiones regulares
str_count(discurso_2020, "\r\n                                                                     39\r\n")

# expresiones regulares para los espacios
# forma1 : [:space:]
# forma2 : \\s

# uno o mas elementos: +
# para cantidad especifica (o rango):  ej buscar 14   {14}
# para buscar 2 o mas : {2,}

str_count(discurso_2020, "\r\n[:space:]{2,}39\r\n")


# ahora con digitos
str_count(discurso_2020,"\r\n[:space:]{2,}[:digit:]+\r\n")

# ahi nos aseguramos que tenemos 39 lineas... ahora hay que sacar eso del texto usando la expresion regular


# Eliminar del texto ese patron

discurso_2020 <- str_remove_all(discurso_2020,"\r\n[:space:]{2,}[:digit:]+\r\n")

View(discurso_2020)


## nuevo : Guardar archivo ##
writeLines(discurso_2020,"discurso2020.txt") 

# unnest_tokens() nos separa en la medida que le demos

tibble(discurso = discurso_2020) %>% 
  unnest_tokens(input = discurso, output = palabra) 

# output = palabra : me crea la columna palabra y lo deja ahi. la unidad por defecto es la palabra...
# mas info aca : https://www.rdocumentation.org/packages/tidytext/versions/0.2.6/topics/unnest_tokens 


# ahora tenemos que sacar la primera parte, para eso hacemos un View y vemos donde empieza el discurso
tibble(discurso = discurso_2020) %>% 
  unnest_tokens(input = discurso, output = palabra) %>% 
  View()
# parte en fila 30..

frecuencias_2020 <- tibble(discurso = discurso_2020) %>% 
  unnest_tokens(input = discurso, output = palabra) %>% 
  slice(-1:-30) %>% 
  count(palabra, sort = TRUE)

# me arroja muchisimos articulos, tipo de y la a que en....


# Vamos a sacar los stopwords, que son palabras que sirven para parte gramatical y aparecen mucho. Realmente no me interesan

# ahh riva nos dijo que hay diccionarios con stopwords: palabras vac?as... intedesante. Lo saco de una universidad de Valladolid

# a continuacion se ven 3 formas distintas de sacar palabras:

unas_stopwords <- read.csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt
")

mis_stopwords <- tibble(palabra = c('m?s','tenemos','va'))

# vamos a usar un anti join, que es para separar

frecuencias_2020 %>% 
  anti_join(unas_stopwords) %>% 
  anti_join(mis_stopwords)


# ahora podemos filtrar por patrones y separar los gentilicios y cosas asi

frecuencias_2020 %>% 
  anti_join(unas_stopwords) %>% 
  anti_join(mis_stopwords) %>% 
  filter(str_detect(palabra,"chile"))

# 1 chile       51
# 2 chilenos    40
# 3 chilenas    14
# 4 chileno      2
# 5 chilena      1



palabras_2020 <- frecuencias_2020 %>% 
  anti_join(unas_stopwords) %>% 
  anti_join(mis_stopwords) %>% 
  filter(!str_detect(palabra,"chile"))


# -----------------------------------------------------------------------------------

# tf_idf  -----------------------------------------------------------------



### Nuevo: Visualizaci?n de palabras (nube de palabras o graficos de barra)

palabras_2020 %>% 
  slice_head(n=15) %>% 
  ggplot(aes(y= reorder(palabra,n),n)) +
  geom_col(fill = "#0057e7") + 
  geom_text(aes(label=n)) +
  labs(y=NULL,
       x="Frecuencia",
       title = "Palabras mas frecuentes mensaje 2020",
       subtitle = "Presidente Sebasti?n Pi?era",
       caption = "Fuente: analisis propio a partir de documento BCN") +
  theme_minimal()

# labs deja etiquetas
# mas colores se buscan en color hex las paletas mas usadas  https://www.color-hex.com/

# nube version 1:

wordcloud2(palabras_2020)

# version 2
x11()
wordcloud(words=palabras_2020$palabra,
          freq= palabras_2020$n,
          min.freq=5,
          max.words = 200,
          random.order = F,
          colors = brewer.pal(6,'Dark2'))


# colors = brewer.pal para ver todas las paletas de colores buscar en  brewer.pal.info (comando de R)
# mas info en nube de palabras: https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html


# vamos a importar otro texto para poder comparar palabras frecuentes y que lo que destaca un documento del otro.

# Palabras 2019

frecuencias_2019 <- read.csv(file="http://bit.ly/frecuencias_2019",fileEncoding="utf-8")
palabras_2019 <- frecuencias_2019 %>% 
  anti_join(unas_stopwords) %>% 
  anti_join(mis_stopwords) %>% 
  filter(!str_detect(palabra,"chile"))

# graficamos..
palabras_2019 %>% 
  slice_head(n=15) %>% 
  ggplot(aes(y= reorder(palabra,n),n)) +
  geom_col(fill = "#0057e7") + 
  geom_text(aes(label=n)) +
  labs(y=NULL,
       x="Frecuencia",
       title = "Palabras mas frecuentes mensaje 2019",
       subtitle = "Presidente Sebasti?n Pi?era",
       caption = "Fuente: analisis propio a partir de documento BCN") +
  theme_minimal()


frecuencias_2020 <- frecuencias_2020 %>% 
  mutate(discurso = 2020, .before = palabra)


frecuencias_2019 <- frecuencias_2019 %>% 
  mutate(discurso = 2019, .before = palabra)



discursos <- bind_rows(frecuencias_2019, frecuencias_2020)
head(discursos)
tail(discursos)


discursos_tfidf <- bind_tf_idf(discursos,
                               term = palabra,
                               document = discurso,
                               n= n)

head(discursos_tfidf)
?tail
tail(discursos_tfidf)


x11()
discursos_tfidf %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(discurso) %>% 
  slice_head(n=15) %>% 
  ggplot(aes(y=reorder(palabra,tf_idf),x= tf_idf, fill= discurso)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~discurso, scales = "free") +
  labs(y=NULL) +
  scale_x_continuous(labels = scales::comma)


# usaremos los paquetes de arriba para lematizar: no perder la categor?a gramatical, si no quedarnos con ...

# Vamos a hacer etiquetado sint?ctico del discurso.

udpipe_download_model(language = "spanish") # bajamos el paquete espa?ol

modelo_es <- udpipe_load_model(file = "spanish-gsd-ud-2.5-191206.udpipe")
# ya tengo el modelo cargado, ahora lo tenemos que usar.

discurso_2020_anotado <- udpipe_annotate(modelo_es,
                                         x=discurso_2020) 
# ojo de usar discurso  y NO dataframe

# ahora que ya hizo el annotate, lo pasamos a dataframe para an?lisis.

discurso_2020_anotado <- as_tibble(discurso_2020_anotado)
head(discurso_2020_anotado)
View(discurso_2020_anotado)


# verbos mas buscados
discurso_2020_anotado %>% 
  slice(-1:-36) %>% 
  filter(upos=="VERB") %>% 
  count(lemma, sort = TRUE )

# adjetivos mas buscados
discurso_2020_anotado %>% 
  slice(-1:-36) %>% 
  filter(upos=="ADJ") %>% 
  count(lemma, sort = TRUE )
