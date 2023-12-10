library(tidyverse)

# Lectura de Base de Datos ------------------------------------------------

data <- read.csv("seguros.csv", sep = ";", header = TRUE, dec = ",")
data <- data %>% 
  dplyr::mutate(Chile = factor(Chile),
                Sexo = factor(Sexo),
                EstCiv = factor(EstCiv),
                Salud = factor(Salud),
                Jubil = factor(Jubil),
                IngresoQuintil = factor(IngresoQuintil)) 

# Test Chi Cuadrado -------------------------------------------------------

chisq.test(data$Seguro, data$Chile)$p.value
chisq.test(data$Seguro, data$Sexo)$p.value
chisq.test(data$Seguro, data$EstCiv)$p.value
chisq.test(data$Seguro, data$Salud)$p.value
chisq.test(data$Seguro, data$Jubil)$p.value
chisq.test(data$Seguro, data$IngresoQuintil)$p.value

boxplot(data$Edad ~ data$Seguro, outline = FALSE)
anova(aov(data$Edad ~ data$Seguro))
boxplot(data$Ingreso ~ data$Seguro, outline = FALSE)
anova(aov(data$Ingreso ~ data$Seguro))
boxplot(data$Educ ~ data$Seguro, outline = FALSE)
anova(aov(data$Educ ~ data$Seguro))

# Analisis Exploratorio ---------------------------------------------------

table(data$Seguro)

## La base está desbalanceada. Se sugiere balancearla.
data_exitos <- data %>% dplyr::filter(Seguro == 1)

set.seed(6082020)
data_fracasos <- data %>% 
  dplyr::filter(Seguro == 0) %>% 
  dplyr::slice(sample(1:n(), size = nrow(data_exitos), replace = FALSE))

data_balanceada <- dplyr::bind_rows(data_exitos, data_fracasos)

# Data de Entrenamiento y de Testeo ---------------------------------------

id <- sample(1:nrow(data_balanceada), size = 0.8*nrow(data_balanceada), replace = FALSE)
data_train <- data_balanceada %>% 
  dplyr::slice(id)

data_test <- data_balanceada %>% 
  dplyr::slice(-id)

# Modelo ------------------------------------------------------------------

modelo <- glm(Seguro ~ Edad + Chile + Sexo +
              Educ + EstCiv + Salud + Jubil +
              Ingreso, 
              data = data_train, family = binomial(link = "logit"))

modelo2 <- glm(Seguro ~ Edad + Chile + Sexo +
               Educ + EstCiv + Salud + Jubil +
               IngresoQuintil, 
               data = data_train, family = binomial(link = "logit"))

summary(modelo); summary(modelo2)
AIC(modelo, modelo2)

### Se escoge el modelo con el ingreso categorizado.
### Eliminamos las variables que no entregan informacion.

modelo_step <- step(modelo2, birection = "backward", trace = FALSE)
summary(modelo_step)
modelo <- modelo_step
AIC(modelo2, modelo)

### Interpretacion de los coeficientes
exp(modelo$coefficients)
exp(confint(modelo, level = 0.95))

### La chance de tener un seguro complementario
### disminuye en un 100*(1 - 0.9636028)% cuando la edad aumenta en un año.
### La chance de tener un seguro complementario
### disminuye en un 100*(1 - 0.5102768)% si la persona vivio fuera de Chile.

### Se escoge el modelo reducido.

# modelo$coefficients[1] <- modelo$coefficients[1] -
#   log(sum(data$Seguro)/sum(data$Seguro == 0))

predict1 <- plogis(predict(modelo, data_train))
predict2 <- plogis(predict(modelo, data_test))

# Corte -------------------------------------------------------------------

library(InformationValue)
corte <- optimalCutoff(data_test$Seguro, predict2)
misClassError(data_test$Seguro, predict2, threshold = corte)

# Multicolinealidad -------------------------------------------------------

library(car)
vif(modelo)
### Valores pequeños, en general, si vif es mayor a 4, hay colinealidad.

# Bondad de Ajuste --------------------------------------------------------

plotROC(data_train$Seguro, predict1)
plotROC(data_test$Seguro, predict2)

Concordance(data_test$Seguro, predict2)
sensitivity(data_test$Seguro, predict2, threshold = corte)
specificity(data_test$Seguro, predict2, threshold = corte)
confusionMatrix(data_test$Seguro, predict2, threshold = corte)
precision(data_test$Seguro, predict2, threshold = corte)

# Test de Hosmer Lemeshow -------------------------------------------------

library(ResourceSelection)
hoslem.test(data_test$Seguro, predict2)$p.value

### El modelo se ajusta bien a los datos.
