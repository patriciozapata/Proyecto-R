


library(readxl)
library(tidyverse)
library(lmtest)
library(cars)
library(haven)

Playstore <- read_excel("Playstore.xlsx")

#correlacion es siempre para variables numericas.












# anova se utiliza mas quimica o farmeceutico 
# ejemplo : depende distintos niveles de dosisi para ver como reacciona el distinto elementos se da (comparar niveles de dosis )
# Anova se utiliza mas quimica o farmeceutico  --------------------------
# la media la esta comparando con 0, es probable que todas las medias sea 0 y  no.-


str(Playstore)

Playstore$Categoría = as.factor(Playstore$Categoría)

Playstore$Tipo = as.factor(Playstore$Tipo)

str(Playstore)

Playstore$`Tamaño(M)` 

#cuiaddo con la normalizada

hist(Playstore$`Tamaño(M)`, rnorm(mean( Playstore$`Tamaño(M)`)))

# Algunos test de sacar la normalidad  en la variable
# se convierte en factor para que sea categoria. o indexada.

#test 1
#  tiene limnite de cantidad de datos
shapiro.test(Playstore$`Tamaño(M)`)

#test 2 no tiene limites de cantidad de datos
ks.test(Playstore$`Tamaño(M)`, rnorm(mean(Playstore$`Tamaño(M)`)))

# los test normalizada tiene como hipotesis nula , si es normal o no 
# el valor p como es muy chico  esta ifnoramcion sirve para rechazar la hipotesis
#  en este caso yo esoty rechazando  la distribucion normal.
# D = 0.98742, p-value < 2.2e-16


modelo_anova = aov(
              data= Playstore,
              `Tamaño(M)`~ Tipo
)

#compara  la variable de tipo  vs tamaño , y si p es muy chico entoces la variable si es significativa 

summary(modelo_anova)

# que esta diciendo : esta variable que esoty considerando tiene test significatica 
# hipotesis nula   la variable no importa o no es significativa
# como el p es muy chico se rewchaza por ende si tiene significancia 


# 2- fin ------------ ------------------------------------------------------------




# modelo regresion simple -------------------------------------------------
# coeficciente  intercept es distintio de sero 
# leido p es menor a sero por ende es distinto de 0 
# lo mismo en el caso de petal.letgh

 view(iris)


modeloregiona1 = lm(data = iris,
                    Petal.Length ~ Petal.Width)

summary(modeloregiona1)
# aca  entra la idea de los residuos o los errores que tenemos 
# Adjusted R-squared:  es  el porcentaje si esta bien  entre amyor mejor

estimacion <-  modeloregiona1$fitted.values
# son los betas 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.08356    0.07297   14.85   <2e-16 ***
  #Petal.Width  2.22994    0.05140   43.39   <2e-16 ***

#  son las estimaciones del largo del petalo 
residuos <-  modeloregiona1$residuals
# son la diferencia con los datos observado vs con los datos que lo comparo

# modelo de regresion intenta predecir ekl larego petalo segun el ancho del petalo 





# analisis de supuesto ----------------------------------------------------

plot(residuos)

residuos = rstandard(modeloregiona1)
# Normalidad:
qqnorm(residuos) # me saca los cualquiles 
qqline(residuos , col  = "red")
shapiro.test(residuos)
# aceptariamos normalidad 
#ahi varisos de test normalidad
#ks.test(residuos,rnorm(mean(residuos)))


# Homocedasticidad: varianza constante :
# se puede ver de 2 forma 
#Linealidad 

plot(residuos)
abline(h = 1.5 , col= "red")
abline(h = -1.5 , col= "red")
# lo que se quiere es que la dispercion s emantenga segun la brnaja
# test 2 
bptest(modeloregiona1) # se test sobre el modelo 
# no se cumple jhomocedasticidad porque valor p es muy chico
# se recjhaza hipotezio nula si es chico el p
# pero si el valor p es mnuiuy gran se acepta  homestacidad 
#hipoteniznula si es  homecedasticidiad
# Independencia:
dwtest(modeloregiona1)
# si se cumple la independencia


# la  jhipoteni nula si dsitrubuye 

#regresio n lineal 
# no nesecariamente sea niveles  o categorica . pero puede ser continuo.
# ejemplo: puede tener variable continua  que singiifca algo para nuestra variable que deseamos sacar}
# regresio n lineal  -----------------------------------------------------

modelo0  <-  lm(data = iris ,
                Sepal.Length ~ 1)

modelosaturado  <-  lm(data = iris ,
                Sepal.Length ~ .)



step(modelo0, direction = c("both"))
step(modelosaturado, direction = c("both"))
# aic se busca que se el valor mas chico 

#GLM


encuesta_salud <- read_excel("encuesta_salud.xlsx")


modeloglm <-  glm( data =  encuesta_salud,
                   Diabetes ~ Peso  + Sexo,
                   family = binomial(link = "logit")
                   )


#sexo y mujer  no tiene tanta significacia

summary(modeloglm)

#ODD RATIO / chances
coef(modeloglm)
exp(coef(modeloglm))

#Preedecir:

prob <-  predict(modeloglm , encuesta_salud , type = "response")
prob
# veamos el punto de corte optimo :

# curva roc surve para ver si se esta ajustando el modelo bien 

#install.packages("DescTools")
library(ROCit)
ksplot(rocit(prob,encuesta_salud$Diabetes))


# PUNTO DE CORTE DE LA DESCRIMINNACION DE SI O NO 
# CUAL ES EL PUNTO DONDE SE CONSIDERE SI TIENE DEABETE O SI NO TIENE

# test para saber si ajustamos ien 
library(DescTools)

HosmerLemeshowTest(prob,encuesta_salud$Diabetes)$H
#   nivel de significacncia, sobre grafico importa el punto de corte para poder descriminiar  y asi escogerlo.




# 2- fin ---------- -------------------------------------------------------



# #Regresion Logistica  ----------------------------------------------------
#regresion ligistica  nuestra variable y no es variable normal, si no que vemos algo si o no o muy separado.
#ejemplo con una base sonora  y se utilizo una variable logistica.
# algunas de la varble son como frecuenicia , la voz ,etc.



