#leemos los datos
datos <- read.csv("Tayko.csv")
head(datos)
#inciso a

#inciso b

#inspección de los datos
datos1 <- within(datos, rm(sequence_number,source_a,source_b,source_c,source_d,source_e,source_m,source_o,source_h,source_r,source_s,source_t,source_u,source_p,source_x,source_w,X1st_update_days_ago,Purchase))
fix(datos1)
names(datos1)
write.table(datos1)

#valores faltantes
sum(is.na(datos1))

#terminar de rellenar los datos faltantes
colSums(is.na(datos1))

#impresion de las variables
str(datos1)

library (ggplot2)

#Spending vs Freq y Spending vs last_update_days_ago.
plot(datos1$Freq, datos1$Spending)
plot(datos1$last_update_days_ago, datos1$Spending)

#inciso c
#1 particionar los datos
library(pastecs)
res <- stat.desc(datos1)
round(res, 2)

library("Hmisc")
res2 <- rcorr(as.matrix(datos1))
res2

#2 obtener la ecuacion
f1 <- lm(Spending~Freq, data = datos1)
summary(f1)
confint(f1)
plot(f1)
confint(f1)

#3
f2 <- lm(Spending~last_update_days_ago, data = datos1)
summary(f2)
confint(f2)
plot(f2)


#4 eliminación
lm.f=lm(Spending ~ Freq + last_update_days_ago + US + Web.order + Gender.male + Address_is_res, data = datos1)
summary(lm.f)

#5 error de predicción
library(car)
vif(lm.f)
Spending_pred <- predict(lm.f)
head(Spending_pred)
Spending_resid <- residuals(lm.f)
head(Spending_resid)

#6 
set.seed(1)
train.index <- sample(c(1:2000), 1600)
datos1_train <- datos1[train.index, ]
datos1_test <- datos1[-train.index, ]
model_train <- lm(Spending ~ Freq + last_update_days_ago + US + Web.order + Gender.male + Address_is_res, data = datos1_train)
model_train_summ <- summary(model_train)
model_train_summ$r.squared
y_test <- datos1_test$Spending
yhat_test <- predict(model_train, newdata = datos1_test)
n_test <- length(datos1_test$Spending)
rmse <- sqrt(sum((y_test - yhat_test)^2) / n_test)
rmse

7#histograma
hist(lm.f$residuals)

