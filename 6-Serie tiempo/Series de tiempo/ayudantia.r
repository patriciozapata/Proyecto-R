#################################################################################
######################## SAV MODELOS DE REGRESION ###############################
#################################################################################

#primero carguemos algunos packages para ahorrar tiempo:

library(readxl)    #para leer bases
library(tidyverse) #para manipular bases
library(lmtest)    #tenemos tests para regresion
library(CARS)      #Acá hay una base que quizás utilicemos
#Si no funciona CARS, prueben con:
library(car)

play = read_excel(file.choose())
View(play)
str(play)

#chr: palabras.
#factor: categorías.

play$Categoría = as.factor(play$Categoría)
str(play)
play$`Tamaño(M)`

#cuidado con la normalidad:
hist(play$`Tamaño(M)`)
shapiro.test(play$`Tamaño(M)`)

#Para aplicar test de Kolmogorov-Smirnov vamos a utilizar otro comando que
#no necesita comparar:
install.packages("nortest")
library(nortest)

lillie.test(play$`Tamaño(M)`)
#Bajo este valor.p; rechazamos que distribuya normal

#Vamos a hacer un anova, para comparar con TIPO

play$Tipo = as.factor(play$Tipo)

modeloanova = aov(data = play,
                  `Tamaño(M)` ~ Tipo)

summary(modeloanova)

modeloanova2 = aov(data=play,
                   `Tamaño(M)` ~ Categoría)
summary(modeloanova2)

### Modelo de regresión simple

View(iris)

modeloregresion1 = lm(data = iris,
                      Petal.Length ~ Petal.Width)

summary(modeloregresion1)

estimaciones = modeloregresion1$fitted.values
residuos = modeloregresion1$residuals

##Analisis de supuestos.
plot(residuos)
residuos = rstandard(modeloregresion1)

#Normalidad:
qqnorm(residuos)
qqline(residuos, col= "red")

shapiro.test(residuos)
lillie.test(residuos)
#con nivel de significancia 0.05 aceptamos normalidad

ad.test(residuos)  #test de Anderson Darling
JarqueBeraTest(residuos) #test Jarque Bera

#Existen varios tests de Normalidad, pero todos con la misma estructura:
#H0: SI distribuye Normal
#H1: NO distribuye Normal


#Homocedasticidad: Varianza constante
#Linealidad

plot(residuos)  #nos hacemos una visión de la homocedasticidad
abline(h=1.5, col="red")
abline(h=-1.5, col="red")

plot(residuos ~ estimaciones) #Acá vemos homocedasticidad y si se comportan lineal
abline(h=1.5, col="red")
abline(h=-1.5, col="red")

bptest(modeloregresion1)
#con 0.05 rechazamos homocedasticidad

#Independencia

dwtest(modeloregresion1)

#rechazamos independencia

###Modelo con más variables predictoras

modeloregresion2 = lm(data=iris,
                      Petal.Length ~ Petal.Width + Sepal.Length + Sepal.Width)

summary(modeloregresion2)


modelo0 = lm(data = iris,
             Sepal.Length ~ 1)

modelosaturado = lm(data = iris,
                    Sepal.Length ~ .)

step(modelo0, direction = c("both"))
step(modelosaturado, direction = c("both"))

### GLM

Salud = read_excel(file.choose())

View(Salud)

modeloglm = glm(data = Salud,
                Diabetes ~ Peso + Sexo,
                family = binomial(link = "logit"))
summary(modeloglm)

#ODD RATIO / Chances

coef(modeloglm)
exp(coef(modeloglm))

#Predecir:

prob = predict(modeloglm, Salud, type = "response")
prob

#veamos punto de corte optimo:
library(broom)
library(InformationValue)
library(pROC)
library(ROCit)

ksplot(rocit(prob,Salud$Diabetes))


#Curva ROC:

plotROC(Salud$Diabetes, prob)


#test para saber si ajustamos bien

install.packages("DescTools")
library(DescTools)
HosmerLemeshowTest(prob,Salud$Diabetes)$C   #Ojo, queremos el C.

#Hip. nula: El modelo SE AJUSTA a los datos observados.
#Hip. alternativa: El modelo NO SE AJUSTA a los datos.

#En ese caso, guíandonos por el valor-p; el modelo ajusta bien con 0.05 de significancia


#El comando anterior nos lanza dos estadísticos. Para no confundirse existe otro
#comando, el cual lanza directamente el que nos importa:

install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(Salud$Diabetes, prob)

#Podemos ver que es el mismo que utilizamos antes.



