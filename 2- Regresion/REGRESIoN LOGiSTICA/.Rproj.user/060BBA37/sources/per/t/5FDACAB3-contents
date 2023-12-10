##################################
## Parte 2: Regresi?n Log?stica ##
##################################

library(readxl)
cardiacas <- read_excel("C:/Users/patri/OneDrive/Escritorio/TALLER EVALUADO 1HEF/REGRESIoN LOGiSTICA/cardiacas.xlsx")
View(cardiacas)

#### Pregunta 1: ####
##  Ajuste como factor las variables sex ,cp,fbs,restecg,ca .



glimpse(cardiacas)

cardiacas$sex       <- factor(cardiacas$sex)
cardiacas$cp <-  factor(cardiacas$cp, levels = c(0,1,2,3), labels = c("leve","mediano", "alto","muy alto"))
cardiacas$fbs      <-  factor(cardiacas$fbs )
cardiacas$restecg <- factor(df$Hipertención, levels = c(0,1), labels = c("Sin hipertencion","Con hipetencion"))
cardiacas$ca <- factor(df$`Nivel educacional`, levels = c(1,2,3), labels = c("Menos de 8", "Entre 8 y 12","Mayor a 12"))
#### Pregunta 2: ####
## Ajuste un modelo de regresi?n log?stica utilizando solo como predictor el
## sexo ?Es el sexo un factor influyente en los problemas cardiacos?
## Interprete el odd del sexo

#### Pregunta 3 ####
## Ajuste un modelo de regresi?n log?stica utilizando solo como
## predictor la edad ?Es la edad un factor influyente en los problemas
## cardiacos? Interprete el odd de la edad

#### Pregunta 4: ####
## Utilizando un m?todo automatizado, ajuste un modelo de regresi?n log?stica,
## utilizando la metodolog?a de direcci?n both (forward y backward a la vez).
## Interprete los coeficientes.


#### Pregunta 5: ####
## Ajuste la curva ROC y KS asociada al modelo, ?Qu? puede concluir sobre la
## discriminaci?n del modelo?


#### Pregunta 6: ####
## Es muy importante saber si un paciente tendr? problemas cardiacos, es por
## ello que se desea optimizar la sensibilidad del modelo, busque un punto de
## corte que tenga una sensibilidad de 0.9

#### Pregunta 7: ####
## Utilizando el punto de corte anterior, clasifique a un paciente con los 
## siguientes factores:

# age = 55
# sex = "Mujer"
# cp = "2"
# trestbps = 132
# chol = 240
# fbs = "Mayor 120"
# restecg = 1
# thalach = 150
# oldpeak = 1
# ca = "2"
