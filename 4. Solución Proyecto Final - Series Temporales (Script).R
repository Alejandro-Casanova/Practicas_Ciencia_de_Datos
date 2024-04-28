# 2.	Series Temporales
# Tenemos los datos del precio de venta del excedente de sistemas fotovoltaicos a la red 
# eléctrica nacional, para distintos periodos de tiempo y con distinta resolución temporal.

#install.packages("forecast") # install, if necessary
library(forecast)
#install.packages("readxl") 
library("readxl")

# 2.1. Datos Semanales
# a)	Cargue el archivo de datos 'precios_excedente_peninsula_semana.xlsx' y guárdelo en 
#     un dataframe.
script_dir <- "C:/Users/alex/Desktop/Máster Software Embebido/2 Segundo Semestre/2 Ciencia de Datos/Ejercicios" # Actualizar con el directorio correcto
setwd(script_dir); getwd()

precios_semana_input <- as.data.frame(read_excel("Ficheros/precios_excedente_peninsula_semana.xlsx"))
head(precios_semana_input)
#plot(precios_semana_input[,2], type="l")
# b)	¿Cuántos días de datos hay disponibles? ¿Con qué resolución? Guarde un 20% de los mismos 
#     para test, y el resto como datos de entrenamiento.

# Hay 5 días de datos en el dataset, con una resolución de una hora, es decir, 24 datos por día.
# Un 20% de los datos supone reservar para test el último de los 5 días.
precios_semana_input_train <- precios_semana_input[1:96,] # El 80% de los datos -> 96/120
precios_semana_input_test <- precios_semana_input[97:120,] # El 20% restante
precios_semana <- ts(as.numeric(precios_semana_input_train[,2]), start=8, frequency = 24)
precios_semana_test <- ts(as.numeric(precios_semana_input_test[,2]),start=12, frequency = 24)

# c)	Ajuste un modelo ARIMA posiblemente estacional (¿periodo?) de forma manual, justificando 
#     los parámetros escogidos. Realice también un ajuste con autoarima sin aproximación.

plot(precios_semana, xlab = "Tiempo (días)",
     ylab = "Precios Excedente Eléctrico (€)")

# Comprobamos las condiciones para serie estacionaria
plot(diff(precios_semana,differences=1), main="d=1, D=0", ylab="")
abline(a=0, b=0)
var(diff(precios_semana,differences=1))

plot(diff(precios_semana,differences=2), main="d=2, D=0", ylab="")
abline(a=0, b=0)
var(diff(precios_semana,differences=2))

plot(diff(precios_semana,lag=24,differences=1), main="d=0, D=1, s=24", ylab="")
abline(a=0, b=0)
var(diff(precios_semana,lag=24,differences=1))

plot(diff(precios_semana,lag=24,differences=2), main="d=0, D=2, s=24", ylab="")
abline(a=0, b=0)
var(diff(precios_semana,lag=24,differences=2))

plot(diff(diff(precios_semana,differences=1),lag=24,differences=1), main="d=1, D=1, s=24", ylab="")
abline(a=0, b=0)
var(diff(diff(precios_semana,differences=1),lag=24,differences=1))

plot(diff(diff(precios_semana,differences=2),lag=24,differences=1), main="d=2, D=1, s=24", ylab="")
abline(a=0, b=0)
var(diff(diff(precios_semana,differences=2),lag=24,differences=1))

# Escogemos d=1, D=1 y s=24

# Analizamos las gráficas ACF y PACF para obtener p, q, P y Q
acf(diff(diff(precios_semana,differences=1),lag=24,differences=1), lag.max=96, main="")
pacf(diff(diff(precios_semana,differences=1),lag=24,differences=1), lag.max=96, main="")

# Componente estacional
# Ajustamos un modelo ARIMA (0,1,0)x(0,1,1)24
arima_semana_1 <- arima(precios_semana, order=c(0,1,0),
                        seasonal = list(order=c(0,1,1), period=24))
arima_semana_1
AIC(arima_semana_1, k = log(length(precios_semana))) # BIC

# Comprobamos el ACF y PACF de los residuos
acf(arima_semana_1$residuals, lag.max=96, main="")
pacf(arima_semana_1$residuals, lag.max=96, main="")

# Ajustamos un modelo ARIMA (2,1,3)x(0,1,1)24 aic=474.43, bic=492.38
arima_semana_2 <- arima (precios_semana, order=c(3,1,2),
                         seasonal = list(order=c(0,1,1), period=24))
arima_semana_2
AIC(arima_semana_2, k = log(length(precios_semana))) # BIC

# Ajustmos con autoarima
arima_semana_3 = auto.arima(precios_semana, d=1, D=1, max.order=5, 
                           trace=TRUE, approx=FALSE,
                           allowdrift=FALSE, stepwise=FALSE)
arima_semana_3
# Obtenido un modelo ARIMA (2,1,2)x(0,1,1)24 aic=472.5 bic=486.08
# Obtenido un modelo ARIMA (0,1,3)x(1,2,0)24 ? aic=363.74 bic=373
# Obtenido un modelo ARIMA (1,0,0)x(1,2,0)24 ? aic=365.1 bic=370.7

# d)	Analice el ACF, el PACF y la normalidad de los residuos para ambos modelos. Comente si 
#     hay diferencias significativas.

# ACF y PACF
## Modelo manual
acf(arima_semana_2$residuals, lag.max=96, main="Residuos (Modelo Manual)")
pacf(arima_semana_2$residuals, lag.max=96, main="Residuos (Modelo Manual)")
# Modelo autoarima
acf(arima_semana_3$residuals, lag.max=96, main="Residuos (Modelo Autoarima)")
pacf(arima_semana_3$residuals, lag.max=96, main="Residuos (Modelo Autoarima)")

# Normalidad de los residuos
## Modelo manual
plot(arima_semana_2$residuals, ylab = "Residuals", main="Ajuste Manual")
abline(a=0, b=0)
hist(arima_semana_2$residuals, xlab="Residuals", xlim=c(-30,30), main="Ajuste Manual")
qqnorm(arima_semana_2$residuals, main="Ajuste Manual")
qqline(arima_semana_2$residuals)
# Modelo autoarima
plot(arima_semana_3$residuals, ylab = "Residuals", main="Autoarima")
abline(a=0, b=0)
hist(arima_semana_3$residuals, xlab="Residuals", xlim=c(-30,30), main="Autoarima")
qqnorm(arima_semana_3$residuals, main="Autoarima")
qqline(arima_semana_3$residuals)

# e)	Utilice ambos modelos para predecir un día de datos y compárelos con los datos guardados 
#     para test. ¿Qué modelo es más preciso? ¿Cuál tiene más incertidumbre en las predicciones?

arima_semana_2.predict <- predict(arima_semana_2, n.ahead=24)
plot(precios_semana, xlab = "Tiempo (días)",
     ylab = "Precios Excedente Eléctrico (€)",
     xlim = c(8, 13))
lines(arima_semana_2.predict$pred, col=2)
lines(arima_semana_2.predict$pred+1.96*arima_semana_2.predict$se, col=3, lty=2)
lines(arima_semana_2.predict$pred-1.96*arima_semana_2.predict$se, col=3, lty=2)
lines(precios_semana_test, col=4)

arima_semana_3.predict <- predict(arima_semana_3, n.ahead=24)
plot(precios_semana, xlab = "Tiempo (días)",
     ylab = "Precios Excedente Eléctrico (€)",
     #ylim = c(1, 50),
     xlim = c(8, 13))
lines(arima_semana_3.predict$pred, col=2)
lines(arima_semana_3.predict$pred+1.96*arima_semana_3.predict$se, col=3, lty=2)
lines(arima_semana_3.predict$pred-1.96*arima_semana_3.predict$se, col=3, lty=2)
lines(precios_semana_test, col=4)
  
# 2.2. Datos mensuales

# a)	Cargue el archivo de datos 'precios_excedente_peninsula_mes.xls' y guárdelo en un dataframe.
precios_mes_input <- as.data.frame(read_excel("Ficheros/precios_excedente_peninsula_mes.xlsx"))

# b)	¿Cuántos días de datos hay disponibles? ¿Con qué resolución? Guarde cuatro semanas de 
#     datos para entrenamiento, y el resto como datos de test

# Hay 31 días de datos en el dataset, con una resolución de una hora, es decir, 24 datos por día.
# Un 20% de los datos supone reservar para test el último de los 5 días.
precios_mes_input_train <- precios_mes_input[1:672,] # 4 semanas para entrenamiento -> 4*7*24=672
precios_mes_input_test <- precios_mes_input[673:744,] # Los 3 días restantes -> 3*24=72
precios_mes <- ts(as.numeric(precios_mes_input_train[,2]), start=1, frequency = 24)
precios_mes_test <- ts(as.numeric(precios_mes_input_test[,2]),start=29, frequency = 24)
precios_mes_por_semanas <- ts(as.numeric(precios_mes_input_train[,2]), start=1, frequency = 168)
precios_mes_test_por_semanas <- ts(as.numeric(precios_mes_input_test[,2]),start=5, frequency = 168)

# c)	Tras una primera diferenciación sin estacionalidad, compare la serie resultante de aplicar 
#     una segunda diferenciación con estacionalidad si se considera un patrón diario o un patrón 
#     semanal. ¿Cuál es más adecuado? Comente qué ocurre con los precios el fin de semana, y 
#     por qué esto hace más conveniente un patrón u otro.

plot(precios_mes, xlab = "Tiempo (días)",
     ylab = "Precios Excedente Eléctrico (€)")

# Primera diferenciación
plot(diff(precios_mes,differences=1), main="d=1, D=0", ylab="")
abline(a=0, b=0)
var(diff(precios_mes,differences=1))

# Patrón diario
plot(diff(diff(precios_mes,differences=1),lag=24,differences=1), main="d=1, D=1, s=24", ylab="")
abline(a=0, b=0)
var(diff(diff(precios_mes,differences=1),lag=24,differences=1))

# Patrón Semanal
plot(diff(diff(precios_mes,differences=1),lag=168,differences=1), main="d=1, D=1, s=128", ylab="")
abline(a=0, b=0)
var(diff(diff(precios_mes,differences=1),lag=168,differences=1))

# d)	Realice un ajuste con autoarima con aproximación, y utilice el modelo resultante para 
#     predecir 3 días de datos. Comente los resultados.
arima_mes_1 = auto.arima(precios_mes, d=1, D=1, max.order=4, 
                            trace=TRUE, approx=TRUE,
                            allowdrift=FALSE, stepwise=FALSE)
arima_mes_1

arima_mes_2 = auto.arima(precios_mes_por_semanas, d=1, D=1, max.order=4, 
                         trace=TRUE, approx=TRUE,
                         allowdrift=FALSE, stepwise=FALSE)
arima_mes_2

arima_mes_1.predict <- predict(arima_mes_1, n.ahead=72)
plot(precios_mes, xlab = "Tiempo (días)",
     ylab = "Precios Excedente Eléctrico (€)",
     ylim = c(0, 370),
     xlim = c(1, 31))
lines(arima_mes_1.predict$pred, col=2)
lines(arima_mes_1.predict$pred+1.96*arima_mes_1.predict$se, col=3, lty=2)
lines(arima_mes_1.predict$pred-1.96*arima_mes_1.predict$se, col=3, lty=2)
lines(precios_mes_test, col=4)

arima_mes_2.predict <- predict(arima_mes_2, n.ahead=72)
plot(precios_mes_por_semanas, xlab = "Tiempo (días)",
     ylab = "Precios Excedente Eléctrico (€)",
     ylim = c(20, 340),
     xlim = c(1, 5.4))
lines(arima_mes_2.predict$pred, col=2)
lines(arima_mes_2.predict$pred+1.96*arima_mes_2.predict$se, col=3, lty=2)
lines(arima_mes_2.predict$pred-1.96*arima_mes_2.predict$se, col=3, lty=2)
lines(precios_mes_test_por_semanas, col=4)

