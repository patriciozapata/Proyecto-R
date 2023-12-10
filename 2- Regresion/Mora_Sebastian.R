library(readxl)   
library(tidyverse) 
library(lmtest)   
library(tseries)   
library(nortest)   
library(pROC)
#install.packages("tseries")
library(ROCit)
library(ggplot2)
library(broom)
library(InformationValue)

#REGRESIÓN LINEAL-----------------------------------------------
   #La base de datos pokémon.xlsx contiene información de 611 Pokémon de las 
   #primeras 7 generaciones. Además, incluye información sobre el poder de ataque, 
   #defensa, velocidad, cantidad de vida
   #(hp), peso y estatura, etc. Se desea estudiar el poder de ataque de un Pokémon,
   #detectando los factores que tienen mayor incidencia. Por ello, utilizando técnicas de
   #machine learning, en particular
   #modelos de regresión lineal, debe estudiar la importancia de los distintos factores,
   #además, de ajustar un modelo capaz de predecir el poder de ataque de un Pokémon.
   #Cargue la base de datos y desarrolle las siguientes preguntas.
                  
                  
                  #Regresi?n Lineal. Pregunta 1:----------------------
                  #Haga un análisis exploratorio con las variables entregadas. Genere
                  #gráficos de dispersión y calcule la correlación de las variables continuas con
                  #respecto a la variable attack.
                  
                  pkmn <- read_excel("C:/Users/patri/OneDrive/Escritorio/TALLER EVALUADO 1HEF/pokemon.xlsx")
                  
                  head(pkmn)
                  
                  
                  mod <- lm(attack ~ defense +
                              height_m + 
                              hp + 
                              sp_attack 
                            + sp_defense
                            + speed + 
                              weight_kg +
                              generation,
                            data = pkmn)
                  summary(mod)
                  
                  #Se rechaza H0, p-value: < 2.2e-16
                  #Outliers
                  
                  rstudent(mod)
                  
                  
                  # Normalidad
                  lillie.test(mod$residuals)
                  qqnorm(mod$residuals)
                  qqline(mod$residuals, col = "red")
                  
                  # Independencia
                  dwtest(mod)
                  
                  
                  # Homocedasticidad 
                  bptest( mod )
                  ggplot(data = pkmn, aes(mod$fitted.values, mod$residuals)) +
                    geom_point() +
                    geom_smooth(color = "firebrick", se = FALSE) +
                    geom_hline(yintercept = 0) +
                    theme_bw()
                  
                  
                  
                  #Regresi?n Lineal. Pregunta 2:--------------------
                  #Ajuste un modelo de regresión lineal todos los factores disponibles,
                  #·¿Son todos estos significativos?, ¿Qué porcentaje de la variabilidad de ataque
                  #es explicada?, ¿Cómo interpretar??a el factor speed?
                  mod2 <- lm(attack ~ .,
                            data = pkmn)
                  summary(mod2)
                  
                  #1.-No todos los factores son significativos, Height, sp_attack, is_legendary no lo son.
                  #2.-37.2%
                  #3.-Se rechaza H0 por lo que el ataque explicar??a las diferencias en speed.
                  
                  
                  #Regresi?n Lineal. Pregunta 3: ----------------------
                  #Ajuste un modelo de regresión lineal con la metodolog??a forward,
                  #¿Cuál es el factor más influyente? Hint: comando step().
                  
                  mod3 <- lm( data = pkmn,attack ~ .)
                  
                  
                  step(mod3, direction = "forward")
                  #El factor más influyente es generation, seeguido por height_m
                  
                  #Regresi?n Lineal. Pregunta 4: ---------------------
                  #Ajuste un modelo de regresión lineal con la metodolog??a backward,
                  #¿Es este modelo diferente al ajustado por la metodolog??a forward?, ¿Cuál de
                  #los 3 modelos ajustados utilizar??a?
                  
                  mod4 <- lm( data = pkmn,attack ~ .)
                  
                  
                  step(mod4, direction = "backward")
                  
                  #Difiere ya que no solo indica que generation es el más influyente, también sp attack.
                  #Utilizar??a backward ya que me parece más detallado.
                  
                  
                  #Regresi?n Lineal. Pregunta 5:---------------
                  #Con el modelo elegido, realice un estudio de los residuos con los
                  #principales supuestos del modelo de regresión lineal. ¿Es un modelo
                  #adecuado?
                  
                  mod5 <- step(lm(attack ~., data = pkmn), direction = "backward")
                  
                  summary(mod5)
                  #Se rechaza H0 p-value: < 2.2e-16
                  
                  #Outliers
                  
                  pkmn$studentized_residual <- rstudent(mod5)
                  ggplot(data = pkmn, aes(x = predict(mod5), y = abs(studentized_residual))) +
                    geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
                    # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
                    geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
                    scale_color_identity() +
                    labs(title = "Distribución de los residuos studentized",
                         x = "predicción modelo", y = "Residuos estandarizados") + 
                    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
                  
                  
                  
                  # Normalidad
                  lillie.test(mod5$residuals)
                  qqnorm(mod5$residuals)
                  qqline(mod5$residuals, col = "red")
                  #0.7202 > 0.05, no se rechaza Ho, aceptamos normalidad.
                  
                  # Independencia
                  dwtest(mod5)
                  #p-value = 1.575e-08 < 0.05, se rechaza Ho
                  
                  # Homocedasticidad 
                  bptest( mod5)
                  #p-value = 3.326e-05< 0.05, se rechaza Ho
                  
                  plot(mod5$residuals)  
                  abline(h=10, col="red")
                  abline(h=-10, col="red")
                  
                  #En conclusión el modelo no cumpliria todos los supuestos necesarios para ser valido. 
                  
                  
                  #Regresi?n Lineal. Pregunta 6:--------------- 
                  #Realice una predicción de un Pokémon que tiene las siguientes caracter??sticas:
                  #• defense = 110
                  #• height_m = 3
                  #• hp = 80
                  #• sp_attack = 110
                  #• sp_defense = 75
                  #• speed = 120
                  #• weight_kg = 77
                  #• generation = 1
                  #• Is legendary = 0 (no es legendario)
                  
                  head(pkmn)
                  pregunta6= data.frame (defense = 110, height_m = 3, 
                                         hp = 80,
                                         sp_attack = 110,
                                         sp_defense = 75, 
                                         speed = 120,
                                         weight_kg= 77,
                                         generation = 1, 
                                         is_legendary = 0 )
                  predict.lm(mod2, newdata = pregunta6, interval = "prediction")
                  predict.glm()
                  #Regresi?n Lineal. Pregunta 7 ---------------------------
                  #[BONUS 0.5 puntos]: Realice un estudio de puntos influyentes y ajuste el modelo
                  #con la metodolog??a both sin estas observaciones. ¿Tiene mayor porcentaje de variabilidad
                  #explicada que los otros modelos? 
                  
                  
                  mod7 <- lm( data = pkmn,attack ~ weight_kg 
                              + hp + 
                                sp_defense
                              + speed + 
                                defense)
                  
                  
                  step(mod7, direction = "both")
                  #Si, cambia-
                  
                  



#REGRESIÓN LOGÍSTICA----------------------------------
          #Usted es contratado como especialista en Data Science para un estudio de enfermedades 
          #cardiacas, para ello dispone de la base de datos heart.xlsx que contiene información de 303 internados en
          #la UCI. La idea es lograr encontrar las variables que tienen una incidencia en 
          #el padecimiento de enfermedades cardiacas. La base contiene lo siguiente:
          
          
          #age: Edad
          #sex: Género
          #cp: Tipo de dolor de pecho
          #trestps: Presión arterial en reposo
          #chol: Suero colesterol en mg/dl
          #fbs: Azúcar en la sangre en ayuna > 120mh/dl
          #restecg: Resultado electrocardiográficos en reposo (valores 0,1,2)
          #thalach: Presión arterial máxima alcanzada
          #oldpeack: Presión del ST inducida por el ejercicio relativo al descanso
          #ca: Número de vasos principales (0-3) coloreados por fluorosop??a.
          #target: 0 = No presenta enfermedades cardiacas, 1 = presenta enfermedades
          #cardiacas.
            
          ec <-  read_excel("C:/Users/patri/OneDrive/Escritorio/TALLER EVALUADO 1HEF/cardiacas.xlsx")
          head(ec)
          
              #Regresi?n Log?stica. Pregunta 1: Ajuste como factor las variables sex,cp,fbs,restecg,ca. 
              
              ec$sex  <- factor(ec$sex)
              ec$cp <- factor(ec$cp)
              ec$fbs <-  factor(ec$fbs)
              ec$restecg <- factor(ec$restecg)
              ec$ca <- factor(ec$ca)
              
              head(ec)
              
              
              ##Regresi?n Log?stica. Pregunta 2:--------------
              #Ajuste un modelo de regresión log??stica utilizando solo como
              #predictor el sexo, ¿Es el sexo un factor influyente en los problemas cardiacos?
              #  Interprete el odd del sexo.
              modelo <- glm(target ~ sex, data = ec, family = binomial(link = "logit"))
              summary(modelo)
              
              coef(modelo)
              exp(coef(modelo))
              #Sexo hombre se usa como base
              #El sexo es un factor influyente
              
              
              #El odd de sexMujer es de exp(1.3022) = 3.67, eso quiere decir que las chances
              #de padecer una enfermedad cardiaca es de 3.677 veces la de un hombre.
              
              #Regresi?n Log?stica. Pregunta 3:-----------------
              #Ajuste un modelo de regresión log??stica utilizando solo como
              #predictor la edad, ¿Es la edad un factor influyente en los problemas
              #cardiacos? Interprete el odd de la edad.
              
              modelo2 <- glm(target ~ age, data = ec, family = binomial(link = "logit"))
              summary(modelo2)
              coef(modelo2)
              exp(coef(modelo2))
              
              #La edad es un factor influyente, a medida que avanza la edad aumenta la 
              #posibilidad en 0.94 veces.
              
              #Regresi?n Log?stica. Pregunta 4:-------------
              #Utilizando un método automatizado, ajuste un modelo de regresión
              #log??stica, utilizando la metodolog??a de dirección both (forward y backward a la
              # vez). Interprete los coeficientes. 
              head(ec)
              ec$target <-  factor(ec$target)
              modelo3 <- step(glm(target ~ age + 
                                    sex+
                                    cp+
                                    trestbps+
                                    chol+
                                    fbs+
                                    restecg+
                                    thalach+
                                    oldpeak, 
                                  data = ec, family = binomial(link = "logit")),
                              direction = "both")
              
              #Las mujeres presentan mayor riesgo de enfermedades cardiacas.
              
              #Regresi?n Log?stica. Pregunta 5: ----------------
              #Ajuste la curva ROC y KS asociada al modelo, ¿Qué puede
              #concluir sobre la discriminación del modelo?
              prob = predict(modelo3, ec, type = "response")
              ksplot(rocit(prob,ec$target))$`KS stat`
              #El punto de discriminación máximo es 0.62, se aprecia buena discriminación.
              
              plotROC(ec$target, prob)
              
              
              #Regresi?n Log?stica. Pregunta 6: ------------
              #Es muy importante saber si un paciente tendrá problemas cardiacos, es por ello
              #que se desea optimizar la sensibilidad del modelo, busque un punto de corte que tenga una
              #sensibilidad de 0.9
              
              plotROC(ec$target,prob, returnSensitivityMat  = TRUE)
              
              #Al punto de corte 0.36 tenemos una sensibilidad de 0.90
              
              
              #Regresi?n Log?stica. Pregunta 7: Utilizando el punto de corte anterior, clasifique a un paciente con los siguientes
              #factores:
               # • age = 55
              #• sex = "Mujer"
              #• cp = "2"
              #• trestbps = 132
              #• chol = 240
              #• fbs = "Mayor 120"
              #• restecg = 1
              #• thalach = 150
              #• oldpeak = 1
              #• ca = "2"
              
              
              
              pregunta7= data.frame (age = 55 
                                     ,sex = "Mujer"
                                     ,cp = "2"
                                     ,trestbps = 132
                                  ,chol =240
                                     ,fbs= "Mayor 120"
                                     ,restecg = "1"
                                     ,thalach = 150
                                     ,oldpeak = "1"
                                     ,ca ="2")
              modelo9 <- glm(target ~ ., data = ec, family = binomial(link = "logit"))
              
              prob1 <- as.vector(predict(modelo9,newdata = pregunta7, type = "response")) 
              
              
              sensitivity(ec$target,prob1, 0.36)
              specificity(ec$target,prob1, 0.36)
