# 1. Preparación de los Datos

# Para cada uno de los tres archivos real-daily-wages-in-pounds-engla.csv, monthly-milk-production-pounds-p.csv 
# y highest-mean-monthly-level-lake-.csv:

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

# Para el dataset de salarios
n_wages <- nrow(wages_input) # Tamaño del dataset
n_wages_train <- floor(n_wages*0.9) # Tamaño del dataset para el modelado
start_year_wages <- as.numeric(wages_input[1,1]) # Año inicial del dataset
start_year_wages_test <- start_year_wages + n_wages_train # Año inicial del dataset de testing

wages_input_train <- wages_input[1:n_wages_train,]
wages_input_test <- wages_input[(n_wages_train+1):n_wages,]

# Para el dataset de producción de leche
start_year_milk <- 1962 # Año inicial del dataset
start_offset_milk <- 1
n_milk <- nrow(milk_input) # Tamaño del dataset
n_milk_train <- floor(n_milk*0.9) # # Tamaño del dataset para el modelado (143)
# Redondea al próximo múltiplo de 12 y resta 1 (empieza en febrero) 
n_milk_train <- n_milk_train + (12 - n_milk_train %% 12) - start_offset_milk 
start_year_milk_test <- start_year_milk + round(n_milk_train / 12) # Año inicial del dataset de testing (1974)

milk_input_train <- milk_input[1:n_milk_train,]
milk_input_test <- milk_input[(n_milk_train+1):n_milk,]

# Para el dataset de niveles de agua del lago Michigan
n_lake <- nrow(lake_input) # Tamaño del dataset
n_lake_train <- floor(n_lake*0.9) # Tamaño del dataset para el modelado
start_year_lake <- as.numeric(lake_input[1,1]) # Año inicial del dataset
start_year_lake_test <- start_year_lake + n_lake_train # Año inicial del dataset de testing

lake_input_train <- lake_input[1:n_lake_train,]
lake_input_test <- lake_input[(n_lake_train+1):n_lake,]

# c)	Crear series temporales con los primeros valores (para entrenar el modelo) y los últimos valores 
# (para test), fijando el inicio adecuado para cada serie temporal y, en su caso, la frecuencia.

# Para el dataset de salarios
wages <- ts(as.numeric(wages_input_train[,2]), start=start_year_wages)
wages_test <- ts(as.numeric(wages_input_test[,2]), start=start_year_wages_test)

# Para el dataset de producción de leche
milk <- ts(as.numeric(milk_input_train[,2]), start=c(start_year_milk, 2), frequency = 12)
milk_test <- ts(as.numeric(milk_input_test[,2]),start=start_year_milk_test, frequency = 12)

# Para el dataset de niveles de agua del lago Michigan
lake <- ts(as.numeric(lake_input_train[,2]),start=start_year_lake)
lake_test <- ts(as.numeric(lake_input_test[,2]),start=start_year_lake_test)



