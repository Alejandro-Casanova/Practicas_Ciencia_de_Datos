# 1. Preparación de los Datos

# Para cada uno de los tres archivos real-daily-wages-in-pounds-engla.csv, monthly-milk-production-pounds-p.csv 
# y highest-mean-monthly-level-lake-.csv:

#install.packages("forecast")       # install, if necessary
library(forecast)

# a)	Cargarlos en un dataframe.
script_dir <- "C:/Users/alex/Desktop/Máster Software Embebido/2 Segundo Semestre/2 Ciencia de Datos/Ejercicios" # Actualizar con el directorio correcto
setwd(script_dir); getwd()

# Dataset de salarios diarios por año en libras
wages_input <- as.data.frame(read.csv("Ficheros/real-daily-wages-in-pounds-engla.csv", 
                                      header=TRUE))
wages_input <- wages_input[-736,] # Descartamos el último elemento, ya que no es útil
head(wages_input)

# Dataset de producción mensual de leche en libras
milk_input <- as.data.frame(read.csv("Ficheros/monthly-milk-production-pounds-p.csv"))
names(milk_input) <- c("date", "Monthly.milk.production.in.pounds") # Nombramos las columnas
head(milk_input)

# Dataset de nivel de agua medio por mes
lake_input <- as.data.frame(read.csv("Ficheros/highest-mean-monthly-level-lake-.csv", 
                                      header=TRUE))
lake_input <- lake_input[-c(97,98),] # Descartamos el último elemento, ya que no es útil
head(lake_input)

# b)	Separar los últimos valores (un 10% aproximadamente es suficiente) para usarlos como test 
# del modelo que entrenaremos con el resto de los valores.

### Para el dataset de salarios ##################################
n_wages <- nrow(wages_input) # Tamaño del dataset
n_wages_train <- floor(n_wages*0.9) # Tamaño del dataset para el modelado
n_wages_test <- n_wages - n_wages_train # Tamaño del dataset para testing
start_year_wages <- as.numeric(wages_input[1,1]) # Año inicial del dataset
start_year_wages_test <- start_year_wages + n_wages_train # Año inicial del dataset de testing

wages_input_train <- wages_input[1:n_wages_train,]
wages_input_test <- wages_input[(n_wages_train+1):n_wages,]

### Para el dataset de producción de leche ##################################
start_year_milk <- 1962 # Año inicial del dataset
start_offset_milk <- 1
n_milk <- nrow(milk_input) # Tamaño del dataset
n_milk_train <- floor(n_milk*0.9) # # Tamaño del dataset para el modelado (143)
n_milk_test <- n_milk - n_milk_train # Tamaño del dataset para testing
# Redondea al próximo múltiplo de 12 y resta 1 (empieza en febrero) 
n_milk_train <- n_milk_train + (12 - n_milk_train %% 12) - start_offset_milk 
start_year_milk_test <- start_year_milk + round(n_milk_train / 12) # Año inicial del dataset de testing (1974)

milk_input_train <- milk_input[1:n_milk_train,]
milk_input_test <- milk_input[(n_milk_train+1):n_milk,]

### Para el dataset de niveles de agua del lago Michigan ##################################
n_lake <- nrow(lake_input) # Tamaño del dataset
n_lake_train <- floor(n_lake*0.9) # Tamaño del dataset para el modelado
n_lake_test <- n_lake - n_lake_train # Tamaño del dataset para testing
start_year_lake <- as.numeric(lake_input[1,1]) # Año inicial del dataset
start_year_lake_test <- start_year_lake + n_lake_train # Año inicial del dataset de testing

lake_input_train <- lake_input[1:n_lake_train,]
lake_input_test <- lake_input[(n_lake_train+1):n_lake,]

# c)	Crear series temporales con los primeros valores (para entrenar el modelo) y los últimos valores 
# (para test), fijando el inicio adecuado para cada serie temporal y, en su caso, la frecuencia.

### Para el dataset de salarios ##################################
wages <- ts(as.numeric(wages_input_train[,2]), start=start_year_wages)
wages_test <- ts(as.numeric(wages_input_test[,2]), start=start_year_wages_test)

### Para el dataset de producción de leche ##################################
milk <- ts(as.numeric(milk_input_train[,2]), start=c(start_year_milk, 2), frequency = 12)
milk_test <- ts(as.numeric(milk_input_test[,2]),start=start_year_milk_test, frequency = 12)

### Para el dataset de niveles de agua del lago Michigan ##################################
lake <- ts(as.numeric(lake_input_train[,2]),start=start_year_lake)
lake_test <- ts(as.numeric(lake_input_test[,2]),start=start_year_lake_test)

# 2.	Búsqueda y evaluación del modelo ARIMA

# a)	Analizar los valores apropiados de d (y en su caso D y s) para el ajuste de un modelo 
#     ARIMA o ARIMA estacional, para asegurar que la serie temporal sea estacionaria.

### Para el dataset de salarios ##################################
plot(wages, xlab = "Time (years)",
     ylab = "daily wages (£)",
     main = "Dataset de Salarios")

# Comprobamos las condiciones para serie estacionaria
plot(diff(wages,differences=1), main= "Salarios, d=1")
abline(a=0, b=0)
var(diff(wages,differences=1))

plot(diff(wages,differences=2), main= "Salarios, d=2")
abline(a=0, b=0)
var(diff(wages,differences=2))

plot(diff(wages,differences=3), main= "Salarios, d=3")
abline(a=0, b=0)
var(diff(wages,differences=3))
# Demasiado, no es necesario. d=1 o d=2??

### Para el dataset de producción de leche ##################################
plot(milk, xlab = "Time (years)",
     ylab = "milk production (pounds)",
     main = "Dataset de Producción de Leche")

# Comprobamos las condiciones para serie estacionaria
plot(diff(milk,differences=1), main="Producción de Leche, d=1", ylab="")
abline(a=0, b=0)
var(diff(milk,differences=1))

plot(diff(milk,differences=2), main="Producción de Leche, d=2", ylab="")
abline(a=0, b=0)
var(diff(milk,differences=2))

plot(diff(diff(milk,differences=1),lag=12,differences=1), 
     main="Producción de Leche, d=1, D=1, s=12", ylab="")
abline(a=0, b=0)
var(diff(diff(milk,differences=1),lag=12,differences=1))

plot(diff(diff(milk,differences=2),lag=12,differences=1), 
     main="Producción de Leche, d=2, D=1, s=12",
     ylab="")
abline(a=0, b=0)
var(diff(diff(milk,differences=2),lag=12,differences=1))
#D=1, d=1, s=12

### Para el dataset de niveles de agua del lago Michigan ##################################
plot(lake, xlab = "Time (years)",
     ylab = "Highest average monthly water level",
     main = "Lake Michigan Water Level")

plot(diff(lake,differences=1), main="Nivel de Agua, d=1", ylab="")
abline(a=0, b=0)
var(diff(lake,differences=1))

plot(diff(lake,differences=2), main="Nivel de Agua, d=2", ylab="")
abline(a=0, b=0)
var(diff(lake,differences=2))

plot(diff(lake,differences=3), main="Nivel de Agua, d=3", ylab="")
abline(a=0, b=0)
var(diff(lake,differences=3))
# Demasiado
# d=1 o d=2 ??

# b)	Con dichos valores de d (y en su caso, D y S), obtener las gráficas de ACF y PACF 
#     para evaluar los valores apropiados para p, q (y, en su caso, P y Q).

### Para el dataset de salarios ##################################
# Examinamos las gráficas ACF y PACF
acf(diff(wages,differences=1), lag.max=48, main="Salarios, d=1")
pacf(diff(wages,differences=1), lag.max=48, main="Salarios, d=1")
acf(diff(wages,differences=2), lag.max=48, main="Salarios, d=2") 
pacf(diff(wages,differences=2), lag.max=48, main="Salarios, d=2")
# Para d=2 el comportamiento es más claro

# Ajustamos un modelo arima (0,2,2)
arima_wages_1 <- arima(wages, order=c(0,2,2))
arima_wages_1
AIC(arima_wages_1, k = log(length(wages))) # BIC

# Examinamos el ACF y PACF de los residuos
acf(arima_wages_1$residuals, lag.max=48, main="Salarios, Residuos Modelo ARIMA(0,2,2)")
pacf(arima_wages_1$residuals, lag.max=48, main="Salarios, Residuos Modelo ARIMA(0,2,2)")
# No son satisfactorios

# Ajustamos un modelo arima (2,2,2)
arima_wages_2 <- arima(wages, order=c(2,2,2))
arima_wages_2
AIC(arima_wages_2, k = log(length(wages))) # BIC

# Examinamos el ACF y PACF de los residuos
acf(arima_wages_2$residuals, lag.max=48, main="Salarios, Residuos Modelo ARIMA(2,2,2)")
pacf(arima_wages_2$residuals, lag.max=48, main="Salarios, Residuos Modelo ARIMA(2,2,2)")
# Nos quedamos con el modelo ARIMA(2,2,2)

### Para el dataset de producción de leche ##################################
acf(diff(diff(milk,differences=1),lag=12,differences=1), lag.max=48, 
    main="Leche, d=1, D=1, S=12")
pacf(diff(diff(milk,differences=1),lag=12,differences=1),  lag.max=48, 
    main="Leche, d=1, D=1, S=12")

# Componente estacional
# Ajustamos un modelo ARIMA (0,1,0)x(0,1,1)12
arima_milk_1 <- arima (milk, order=c(0,1,0),
                  seasonal = list(order=c(0,1,1), period=12))
arima_milk_1
AIC(arima_milk_1, k = log(length(milk))) # BIC

# Comprobamos el ACF y PACF de los residuos
acf(arima_milk_1$residuals, lag.max=48, main="Leche, Residuos Modelo ARIMA (0,1,0)(0,1,1)[12]")
pacf(arima_milk_1$residuals, lag.max=48, main="Leche, Residuos Modelo ARIMA (0,1,0)(0,1,1)[12]")
# No es necesario añadir más parámetros
# Nos quedamos con el modelo ARIMA(0,1,0)(0,1,1)[12] 

### Para el dataset de niveles de agua del lago Michigan ##################################
# Examinamos las gráficas ACF y PACF
acf(diff(lake,differences=1), lag.max=48, main="Lago d=1")
pacf(diff(lake,differences=1), lag.max=48, main="Lago d=1")
acf(diff(lake,differences=2), lag.max=48, main="Lago d=2") 
pacf(diff(lake,differences=2), lag.max=48, main="Lago d=2")
# d=2 da un comportamiento más claro

# Ajustamos un modelo arima (0,2,1)
arima_lake_1 <- arima(lake, order=c(0,2,1))
arima_lake_1
AIC(arima_lake_1, k = log(length(lake))) # BIC

# Examinamos el ACF y PACF de los residuos
acf(arima_lake_1$residuals, lag.max=48, main="Lago, residuos ARIMA(0,2,1)")
pacf(arima_lake_1$residuals, lag.max=48, main="Lago, residuos ARIMA(0,2,1)")
# Mejorable

# Ajustamos un modelo arima (1,2,1)
arima_lake_2 <- arima(lake, order=c(1,2,1))
arima_lake_2
AIC(arima_lake_2, k = log(length(lake))) # BIC

# Examinamos el ACF y PACF de los residuos
acf(arima_lake_2$residuals, lag.max=48, main="Lago, residuos ARIMA(1,2,1)")
pacf(arima_lake_2$residuals, lag.max=48, main="Lago, residuos ARIMA(1,2,1)")
# Nos quedamos con el modelo ARIMA(1,2,1)

# c)	Comparar los resultados obtenidos con la solución proporcionada por el comando 
#     auto.arima de R.

### Para el dataset de salarios ##################################
arima_wages_3=auto.arima(wages, d=2, max.order=4, 
                         trace=TRUE, approx=FALSE,
                         allowdrift=FALSE, stepwise=FALSE)
arima_wages_3 #  Best model: ARIMA(2,2,2)
# Coincide con nuestro modelo

### Para el dataset de producción de leche ##################################
arima_milk_2=auto.arima(milk, d=1, D=1, max.order=4, 
                         trace=TRUE, approx=FALSE,
                         allowdrift=FALSE, stepwise=FALSE)
arima_milk_2 #  Best model: ARIMA(0,1,0)(0,1,1)[12] 
# Coincide con nuestro modelo

### Para el dataset de niveles de agua del lago Michigan ##################################
arima_lake_3=auto.arima(lake, d=1, max.order=4, 
                        trace=TRUE, approx=FALSE,
                        allowdrift=FALSE, stepwise=FALSE)
arima_lake_3
# Nos quedamos con el modelo ARIMA(2,1,1)

# d)	Analizar la normalidad de los residuos.
# Normality and Constant Variance

### Para el dataset de salarios ##################################
plot(arima_wages_2$residuals, ylab = "Residuals", main="Residuos del Dataset de Salarios")
abline(a=0, b=0)

hist(arima_wages_1$residuals, xlab="Residuals", xlim=c(-5,5),
     main="Histograma de los Residuos del Dataset de Salarios")

qqnorm(arima_wages_1$residuals, main="Gráfica QQ de los Residuos del Dataset de Salarios")
qqline(arima_wages_1$residuals)

### Para el dataset de producción de leche ##################################
plot(arima_milk_1$residuals, ylab = "Residuals", main="Residuos del Dataset de Producción de Leche")
abline(a=0, b=0)

hist(arima_milk_1$residuals, xlab="Residuals", xlim=c(-50,50),
     main="Histograma de los Residuos del Dataset de Producción de Leche")

qqnorm(arima_milk_1$residuals, main="Gráfica QQ de los Residuos del Dataset de Producción de Leche")
qqline(arima_milk_1$residuals)

### Para el dataset de niveles de agua del lago Michigan ##################################
plot(arima_lake_3$residuals, ylab = "Residuals", main="Residuos del Dataset del Lago Michigan")
abline(a=0, b=0)

hist(arima_lake_3$residuals, xlab="Residuals", xlim=c(-3,3),
     main="Histograma de los Residuos del Dataset del Lago Michigan")

qqnorm(arima_lake_3$residuals, main="Gráfica QQ de los Residuos del Dataset del Lago Michigan")
qqline(arima_lake_3$residuals)

# e)	Representar gráficamente las predicciones del modelo, con sus intervalos de 
#     confianza, y compararlos con los datos reales que se reservaron para test.

### Para el dataset de salarios ##################################
# Predecimos los valores de los siguientes 74 años
arima_wages_3.predict <- predict(arima_wages_3, n.ahead=n_wages_test)
plot(wages, xlab = "Time (years)",
     ylab = "daily wages (£)",
     ylim = c(1, 50),
     xlim = c(1250, 2000),
     main = "Predicción del modelo ajustado para el dataset de salarios")
lines(arima_wages_3.predict$pred, col=2)
lines(arima_wages_3.predict$pred+1.96*arima_wages_3.predict$se, col=3, lty=2)
lines(arima_wages_3.predict$pred-1.96*arima_wages_3.predict$se, col=3, lty=2)
lines(wages_test, col=4)

### Para el dataset de producción de leche ##################################
arima_milk.predict <- predict(arima_milk_1, n.ahead=n_milk_test)
plot(milk, xlab = "Time (years)",
     ylab = "daily production (pounds)",
     ylim = c(550, 970),
     xlim = c(1962, 1975),
     main = "Predicción del modelo ajustado para el dataset de producción de leche")
lines(arima_milk.predict$pred, col=2)
lines(arima_milk.predict$pred+1.96*arima_milk.predict$se, col=3, lty=2)
lines(arima_milk.predict$pred-1.96*arima_milk.predict$se, col=3, lty=2)
lines(milk_test, col=4)

### Para el dataset de niveles de agua del lago Michigan ##################################
arima_lake.predict <- predict(arima_lake_3, n.ahead=n_lake_test)
plot(lake, xlab = "Time (years)",
     ylab = "highest monthly average water level",
     ylim = c(78, 84),
     xlim = c(1860, 1955),
     main = "Predicción del modelo ajustado para el dataset del lago Michigan")
lines(arima_lake.predict$pred, col=2)
lines(arima_lake.predict$pred+1.96*arima_lake.predict$se, col=3, lty=2)
lines(arima_lake.predict$pred-1.96*arima_lake.predict$se, col=3, lty=2)
lines(lake_test, col=4)
