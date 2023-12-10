


library(readxl)
library(GGally)
library(tidyverse)
library(lmtest)  
library(InformationValue)
library(tseries)   
library(psych)
library(corrplot)
library(faraway)
library(plotROC)
library(ROCit)
#install.packages("plotROC")
#install.packages("plotROC")
library(nortest)
library(pROC)
library(ggplot2)
library(gridExtra)
library(car)
pokemon <- read_excel("C:/Users/patri/OneDrive/Escritorio/TALLER EVALUADO 1HEF/REGRESIÓN LINEAL/pokemon.xlsx")


#Regresión Lineal - PREGUNTA 2: HAY QUE AJUSTAR UN MODELO CON TODAS LAS VARIABLES, EN ESTE CONTEXTO FACTORES SE REFIERE A VARIABLES.
#Mi mail para dudas en cualquier momento: caalarcon1@uc.cl

### -A- Regresion Lineal: ####
#### Pregunta 1: ####
## Haga un análisis exploratorio con las variables entregadas. 
## Genere  gráficos de dispersión y calcule la correlación de las variables continuas 
## con respecto a la variable attack

#NOTAS: 
## continua numero entero men 
## discreta numero flotat 
View(pokemon)

str(pokemon)

colnames(pokemon) <- c("ataque","defensa","altura_metro","vida","velocidad_ataque","velocidad_defensa","speed","peso_kg","Generacion","legendario")

round(cor(x = pokemon , method = "pearson"), 2)

# valorees continuos 
# se quita columna 8 y 3 ya que son valores discretos
x <- cor(pokemon[-8 ][-3])
corrplot(x, type="upper", order="hclust")

ggpairs(pokemon, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

pairs.panels(x = pokemon[-8 ][-3], ellipses = FALSE, lm = TRUE, method = "pearson")


plot(pokemon)


#### Pregunta 2: ####
##  Ajuste un modelo de regresión lineal todos los factores disponibles,
## ¿Son todos estos significativos?, ¿Qué porcentaje de la variabilidad de 
## ataque es explicada?, ¿Cómo interpretaría el factor speed?


regresion_l <- lm(pokemon$ataque ~ .,pokemon)
step(regresion_l)
summary(regresion_l)

#1.- legendario no es significativo.
#2.- El valor de R2 indica que el modelo calculado explica el 38,21% de la variabilidad presente en la variable respuesta (ataque) mediante la variable independientes.
#3.- es significativo ya que se acerca al valor 0.

#multiple R-squared:  0.3821,	Adjusted R-squared:  0.3729 


# step recibe un objeto  ajusta el modelo.
# foward es como ajustar el modelo , partir modelo de nad ay agrega una variable

# bowarck modelo completo y eliminar la varibale que menos se ajusta , que no son sifingicativas . 

# como elegir el modelo mejor :  la mejor preccidio con menos variable r ajustado  debe ser mayor. 
# con criteria AIC. 
# LAS VARIABLE SE TIENE QUE TOMAR LOS FACTORES. 

#REGRESION LGISTICA 


#### Pregunta 3 ####
## Ajuste un modelo de regresión lineal con la metodología forward,
## ¿Cuál es el factor más influyente? Hint: comando step().

forward <- lm( data = pokemon ,pokemon$ataque ~ .)
# ajuste al foward 
step(forward, direction = "forward")


summary(forward)
# factor mas influyente es si es legendario. 
# ademas de la altura por metro
#Multiple R-squared:  0.3821,	Adjusted R-squared:  0.3729 



#### Pregunta 4: ####
## Ajuste un modelo de regresión lineal con la metodología backward ,
## ¿Es este modelo diferente al ajustado por la metodología forward?, ¿Cuál de
## los 3 modelos ajustados utilizaría?

backward <- lm( data = pokemon ,pokemon$ataque ~ .)

# ajuste al backward 
step(backward, direction = "backward")
summary(backward)

#Multiple R-squared:  0.3821,	Adjusted R-squared:  0.3729 

# backward me ajusta automatico los datos que no significativcos.

#### Pregunta 5: ####
## Con el modelo elegido, realice un estudio de los residuos con los
## principales supuestos del modelo de regresión lineal. ¿Es un modelo
## adecuado?


# Relacion lineal entre los predictores numericos y la variable respuesta

plot1 <- ggplot(data = pokemon, aes(defensa, backward$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = pokemon, aes(vida , backward$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = pokemon, aes(velocidad_defensa, backward$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = pokemon, aes(speed, backward$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)

# se púede apreciar la regreesion lineal y sus dispercion

# Distribucion normal de los residuos
# dispersion de los residuos
qqnorm(backward$residuals) 
#dispersion residua y la lineal..-
qqline(backward$residuals)

#Variabilidad constante de los residuos (homocedasticidad)


ggplot(data = pokemon, aes(backward$fitted.values, backward$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()
# si es modelo adecuado backward


#### Pregunta 6: ####
## Realice una predicción de un Pokémon que tiene las siguientes 
##caracter�sticas:


queda_poco = data.frame (defense = 110, height_m = 3, 
                       hp = 80,
                       sp_attack = 110,
                       sp_defense = 75, 
                       speed = 120,
                       weight_kg= 77,
                       generation = 1, 
                       is_legendary = 0 )

predict.lm(backward, data = queda_poco, interval = "prediction")

summary(queda_poco)


#### Pregunta 7: ####
## Realice un estudio de puntos influyentes y ajuste el modelo
## con la metodolog�a both sin estas observaciones.
#¿Tiene mayor porcentaje  de variabilidad explicada que los otros modelos?
                                             
siete  <- lm( data = pokemon,
              pokemon$ataque ~  
                pokemon$defensa  +   
                pokemon$vida  + 
                pokemon$velocidad_ataque  +
                pokemon$speed  )


step(siete, direction = "both")
summary(influence.measures(model = siete))
influencePlot(model = siete)
summary(siete)
# tiene menor porcentaje varibialidabidad
#El valor de R2 indica que el modelo calculado explica el 32,17% de la variabilidad presente en la variable respuesta (ataque ) mediante la variable independientes.
# tiene menor variabilidad.
#punto de influyente son  beta  libreria car:: podrias busca rmen 

help(predict.glm)

### -B- Regresion Logistica: ####


## Parte 2: Regresion Logistica: ##


library(readxl)
cardiacas <-  read_excel("C:/Users/patri/OneDrive/Escritorio/TALLER EVALUADO 1HEF/REGRESIÓN LINEAL/cardiacas.xlsx")
View(cardiacas)

#### Pregunta 1: ####
##  Ajuste como factor las variables sex ,cp,fbs,restecg,ca .



cardiacas$sex  <- factor(cardiacas$sex)
cardiacas$cp <- factor(cardiacas$cp)
cardiacas$fbs <-  factor(cardiacas$fbs)
cardiacas$restecg <- factor(cardiacas$restecg)
cardiacas$ca <- factor(cardiacas$ca)

str(cardiacas)




#### Pregunta 2: ####
## Ajuste un modelo de regresion logistica utilizando solo como predictor el
## sexo ?Es el sexo un factor influyente en los problemas cardiacos?
## Interprete el odd del sexo

modelo <- glm(target ~ sex, data = cardiacas, family = binomial(link = "logit"))
help(summary.glm)

summary(modelo)

coef(modelo)
exp(coef(modelo))

#sexo es un factor influyente


#### Pregunta 3 ####
## Ajuste un modelo de regresion logistica utilizando solo como
## predictor la edad ?Es la edad un factor influyente en los problemas
## cardiacos? Interprete el odd de la edad

modelo2 <- glm(target ~ age, data = cardiacas, family = binomial(link = "logit"))

summary(modelo2)
# edad es significativa 

coef(modelo2)
exp(coef(modelo2))


#La edad es un factor influyente ya que avanza la edad aumenta la posibilidad en un  0.94


#### Pregunta 4: ####
## Utilizando un metodo automatizado, ajuste un modelo de regresion logistica,
## utilizando la metodologia de direcciionn both (forward y backward a la vez).
## Interprete los coeficientes.
str(cardiacas)

cardiacas$target <-  factor(cardiacas$target)

cuatro <- step(glm(target ~ age + 
                      sex+
                      cp+
                      trestbps+
                      chol+
                      fbs+
                      restecg+
                      thalach+
                      oldpeak, 
                    data = cardiacas, family = binomial(link = "logit")),
                direction = "both")
summary(cuatro)


confint(object = cuatro, level = 0.95 )




#### Pregunta 5: ####
## Ajuste la curva ROC y KS asociada al modelo, Que puede concluir sobre la
## discriminacion del modelo?

prob = predict(cuatro, cardiacas, type = "response")

ksplot(rocit(prob,cardiacas$target))$`KS stat`

#El punto de discriminacion  maximo es 0.6259, se aprecia buena discriminacion

plotROC(cardiacas$target, prob)


#### Pregunta 6: ####
## Es muy importante saber si un paciente tendra problemas cardiacos, es por
## ello que se desea optimizar la sensibilidad del modelo, busque un punto de
## corte que tenga una sensibilidad de 0.9




plotROC(cardiacas$target,prob, returnSensitivityMat  = TRUE)


# sensibilidad 0.90303030  punto de corte   0.36



#### Pregunta 7: ####
## Utilizando el punto de corte anterior, clasifique a un paciente con los 
## siguientes factores:




data = data.frame (age = 55 
                       ,sex = "Mujer"
                       ,cp = "2"
                       ,trestbps = 132
                       ,chol =240
                       ,fbs= "Mayor 120"
                       ,restecg = "1"
                       ,thalach = 150
                       ,oldpeak = "1"
                       ,ca ="2")
mol <- glm(target ~ ., data = cardiacas, family = binomial(link = "logit"))

matriz <- as.vector(predict(mol,newdata = data, type = "response")) 

specificity(cardiacas$target,matriz, 0.36)

sensitivity(cardiacas$target,matriz, 0.36)



