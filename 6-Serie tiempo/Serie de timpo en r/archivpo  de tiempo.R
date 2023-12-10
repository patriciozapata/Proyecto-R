#install.packages("readxl")
#install.packages("forecast")
#install.packages("tidyverse")
#install.packages("zoo")
#install.packages("tseries")
#install.packages("lmtest")


library(readxl)    # Leer bases de datos
library(forecast)  # Comandos para series de Tiempo
library(tidyverse) # Manipulación de datos, dplyr y ggplot2
library(zoo)       # Series de Tiempo y fechas
library(tseries)   # Para ocupar la funcion adf.test()
library(lmtest)    # Para validar los coeficientes


library(readxl)


# Actividad I -------------------------------------------------------------
### a.-  Cargue la base de datos AirCanada.xlxs e inspeccionela

AirCanada <- read_excel("air canada.xlsx")
names(AirCanada)
summary(AirCanada)
head(AirCanada)

### b.- Utilizando el comando ts() transforme los datos a un formato "serie de
### tiempo“ que considere el período desde Enero de 2006 a Junio de 2018

AirCanada =  AirCanada[-c(1:6),]

Pasajeros = ts(data = AirCanada$`Total pasajeros periodo`,
               start=c(2006,1),
               frequency=12)
str(Pasajeros)

b = AirCanada$`Total pasajeros periodo`

b[10]=NA

View(b)

test = ts(data = na.omit(b),
          start=c(2006,1),
          frequency=12)
test
### c.- Utilizando el comando plot.ts() grafique la serie de tiempo resultante
### Comente

plot.ts(Pasajeros)
plot.ts(test)

### d.- Utilizando la función as Date transforme la variable de tiempo a
### formato "date" Realice un gráfico Y vs t para visualizar la serie

# zoo 

# as.Date() # me transforma en formato date
# as.yearm

# Para usar la transformación de la fecha, tiene que ser tipo "chr"

AirCanada$Periodo = as.character(AirCanada$Periodo)
AirCanada$Periodo = as.Date(as.yearmon(AirCanada$Periodo,"%Y%m"))


# YYYYMM -> "%Y%m"
# MM-YYYY -> "%m-%Y"

ggplot(data = AirCanada, aes(x=Periodo, y =`Total pasajeros periodo`)) +
  geom_line()


### e.- Utilice el comando decompose() y comente sobre la tendencia y
### estacionalidad de la serie

Descomposicion = decompose(Pasajeros)

plot(Descomposicion)
# observed  = los 3 sumaods ramdon+ seasonal + trend
# se divide en 3 parte trend =   tendencia +,  seasonal   forescasting
#, random = aleatorio  es marge de error que tiene entre tendencia y seasonal


acf(air_canada$`Total pasajeros periodo`)
# correlacion entre frecuencia 

