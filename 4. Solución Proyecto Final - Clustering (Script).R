# Soluciones del Proyecto Final

# Tenemos los datos de vinos de España de distintas denominaciones de origen, se
# requiere hacer una segmentación de vinos españoles en función de precio, calidad
# y acidez del vino.

# 1. Clustering

# (a) Instalar el paquete Tidyverse.
# install.packages("tidyverse") # install if necessary
library(tidyverse)

# (b) Cargar el fichero en un dataframe, considerando el tipo de separador adecuado.
script_dir <- "C:/Users/alex/Desktop/Máster Software Embebido/2 Segundo Semestre/2 Ciencia de Datos/Ejercicios" # Actualizar con el directorio correcto
setwd(script_dir); getwd()
Frame0 <- as.data.frame(read.csv("Ficheros/wines_SPA_1.csv", header=TRUE, 
                                 sep=';', dec ='.'))
head(Frame0)

# (c) Identificar la estructura de los datos y realizar la coerción de los datos que no se
#     encuentren en formato numérico y que sean necesarios para la segmentación.
# Mantendremos la denominación, dado que tendremos que hacer un filtrado más adelante.
SubFrame0 <-subset.data.frame(Frame0, select=c('price', 'rating', 'acidity', 'region'))
head(SubFrame0)

sapply(SubFrame0[1,], class) # Comprobar el tipo de dato de las variables

# Mapeamos a números las distintas regiones
region_names <- unique(SubFrame0$region)
head(region_names)
region_to_num_mapping <- setNames(1:length(region_names)*1e-8, region_names)

# Creamos un nuevo frame y comenzamos la conversión de los tipos de las diferentes variables
SubFrame1 <- SubFrame0
SubFrame1$region <- sapply(SubFrame1$region, function(x) region_to_num_mapping[as.character(x)])
SubFrame1$price <- as.numeric(SubFrame1$price)
SubFrame1$acidity <- as.numeric(SubFrame1$acidity)
sapply(SubFrame1[1,], class) # Comprobar el tipo de dato de las variables
apply(SubFrame1, 2, range) # Comprobar el rango de las variables

# (d) Eliminar aquellas filas que no contengan datos o que no estén disponibles.
SubFrame2 <- subset.data.frame(SubFrame1, !is.na(price) & !is.na(rating) & !is.na(acidity) & !is.na(region))
apply(SubFrame2, 2, range) # Comprobar el rango de las variables
apply(SubFrame2[1:3], 2, mean)
apply(SubFrame2[1:3], 2, median)

# Transform into matrix for kmeans
kmdata_orig = as.matrix(SubFrame2[,1:4])
mode(kmdata_orig) = "numeric"
head(kmdata_orig)

# (e) Valorar si es necesario realizar un escalado de algunos de los datos: price, rating, acidity.
# Será importante realizar un escalado de la variable precio, debido a la gran magnitud de su rango
# de valores. De todos modos,las tres variables tienen unidades distintas, y aunque no sea tan 
# crítico para las variables 'rating' y 'acidity', también normalizaremos éstas.
NUM_VAL <- 3
kmdata <- kmdata_orig
#kmdata[,1] <- sapply(kmdata[,1], sqrt) # Apply sqrt to ponderate price
#kmdata[,1] <- sapply(kmdata_orig[,1], log) # Apply log to ponderate price
max_val = numeric(NUM_VAL)
min_val = numeric(NUM_VAL)
for (k in 1:NUM_VAL) max_val[k] <- max(as.numeric(kmdata[,k]))
for (k in 1:NUM_VAL) min_val[k] <- min(as.numeric(kmdata[,k]))
kmdata[,1:NUM_VAL] <- scale(kmdata[,1:NUM_VAL], center=min_val, scale=(max_val-min_val))
head(kmdata)
apply(kmdata, 2, range)

# (f) Haz un clustering de todos los vinos. Considera si puedes ponderar o escalar los
#     datos de alguna manera razonable. Usa el método elbow para determinar el
#     valor más apropiado de k.
wss <- numeric(8) 
for (k in 1:8) wss[k] <- (sum(kmeans(kmdata, centers=k, nstart=25)$withinss))
#wss <- log(wss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

km2 = kmeans(kmdata,2, nstart=25)
km2

km3 = kmeans(kmdata,3, nstart=25)
km3

# Seleccionamos k=3
NUM_CLUSTERS <- 3
km_selected <- km3
df <- as.data.frame(kmdata) 
df$cluster <- factor(km_selected$cluster)
centers <- as.data.frame(km_selected$centers)

# Devolvemos los datos a sus magnitudes originales
df$region <- sapply(df$region, function(x) names(region_to_num_mapping[as.numeric(round(x*1e08))]))
for (k in 1:NUM_VAL) df[,k] <- df[,k] * (max_val[k] - min_val[k]) + min_val[k] # Unscale
for (k in 1:NUM_VAL) centers[,k] <- centers[,k] * (max_val[k] - min_val[k]) + min_val[k] # Unscale centers
#df$price <- sapply(df$price, exp) # Apply exp to reverse log
#centers$price <- sapply(centers$price, exp) # Apply exp to reverse log
apply(df[,1:3], 2, range) # Comprobar el rango de las variables
centers

# Dibujamos las gráficas calidad-precio, acidez-calidad y acidez-precio
library(ggplot2)
library(grid) #gráficos en cuadrícula
library(gridExtra)

plotGraphs <- function(df, centers){

  g1 <- ggplot(data=df, aes(x=rating, y=price, color=cluster )) + 
    geom_point() +
    geom_point(data=centers, aes(x=rating, y=price, color=as.factor(1:NUM_CLUSTERS)), 
               size=10, alpha=.6, show.legend=FALSE)
  
  g2 <- ggplot(data=df, aes(x=acidity, y=rating, color=cluster )) + 
    geom_point() +
    geom_point(data=centers, aes(x=acidity, y=rating, color=as.factor(1:NUM_CLUSTERS)), 
               size=10, alpha=.6, show.legend=FALSE)
  
  g3 <- ggplot(data=df, aes(x=acidity, y=price, color=cluster )) + 
    geom_point() +
    geom_point(data=centers, aes(x=acidity, y=price, color=as.factor(1:NUM_CLUSTERS)), 
               size=10, alpha=.6, show.legend=FALSE)
  
  g4 <- ggplot(data=df, aes(x=region, y=price, color=cluster )) + 
    geom_point() + theme(axis.text.x=element_blank())
    #geom_point(data=centers, aes(x=region, y=price, color=as.factor(1:NUM_CLUSTERS)), 
    #           size=10, alpha=.6, show.legend=FALSE)
  
  g5 <- ggplot(data=df, aes(x=region, y=rating, color=cluster )) + 
    geom_point() + theme(axis.text.x=element_blank())
    #geom_point(data=centers, aes(x=region, y=rating, color=as.factor(1:NUM_CLUSTERS)), 
    #           size=10, alpha=.6, show.legend=FALSE)
  
  grid.arrange(
    arrangeGrob(g1 + theme(
      legend.box.background = element_rect(),
      legend.box.margin = margin(6, 6, 6, 6))),
    arrangeGrob(g2 + theme(legend.position="none"), 
                g3 + theme(legend.position="none"), 
                ncol = 2)
  )
  
  return(list(g1,g2,g3,g4,g5))
}

plotSingleGraph <- function(gg_element){
  plot(gg_element + theme(
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6)))
}

g_i <- plotGraphs(df, centers)
plotSingleGraph(g_i[[1]])
plotSingleGraph(g_i[[2]])
plotSingleGraph(g_i[[3]])
plotSingleGraph(g_i[[4]])
plotSingleGraph(g_i[[5]])

# (g) Repite el proceso solo para los vinos de denominación "Rioja".
SubFrame_rioja <- subset.data.frame(SubFrame2, names(region_to_num_mapping[as.numeric(round(region*1e08))]) == 'Rioja')

# Transform into matrix for kmeans
kmdata_rioja = as.matrix(SubFrame_rioja[,1:4])
mode(kmdata_rioja) = "numeric"
head(kmdata_rioja)
apply(kmdata_rioja, 2, range)

# Scale the data
NUM_VAL <- 3
max_val_rioja = numeric(NUM_VAL)
min_val_rioja = numeric(NUM_VAL)
for (k in 1:NUM_VAL) max_val_rioja[k] <- max(as.numeric(kmdata_rioja[,k]))
for (k in 1:NUM_VAL) min_val_rioja[k] <- min(as.numeric(kmdata_rioja[,k]))
kmdata_rioja[,1:NUM_VAL] <- scale(kmdata_rioja[,1:NUM_VAL], 
                                  center=min_val_rioja, 
                                  scale=(max_val_rioja-min_val_rioja))
head(kmdata_rioja)
apply(kmdata_rioja, 2, range)

# Calculamos k mediante el método 'elbow'
wss_r <- numeric(8) 
for (k in 1:8) wss_r[k] <- (sum(kmeans(kmdata_rioja, centers=k, nstart=25)$withinss))
#wss_r <- log(wss_r)
plot(1:8, wss_r, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

km2_rioja = kmeans(kmdata_rioja, 2, nstart=25)
km2_rioja

km3_rioja = kmeans(kmdata_rioja, 3, nstart=25)
km3_rioja

# Seleccionamos k=3
NUM_CLUSTERS <- 3
km_selected_r <- km3_rioja
df_rioja <- as.data.frame(kmdata_rioja)
df_rioja$cluster <- factor(km_selected_r$cluster)
centers_rioja <- as.data.frame(km_selected_r$centers)

# Devolvemos los datos a sus magnitudes originales
df_rioja$region <- sapply(df_rioja$region, function(x) names(region_to_num_mapping[as.numeric(round(x*1e08))]))
for (k in 1:NUM_VAL) df_rioja[,k] <- df_rioja[,k] * (max_val_rioja[k] - min_val_rioja[k]) + min_val_rioja[k] # Unscale
for (k in 1:NUM_VAL) centers_rioja[,k] <- centers_rioja[,k] * (max_val_rioja[k] - min_val_rioja[k]) + min_val_rioja[k] # Unscale centers
apply(df_rioja[,1:3], 2, range) # Comprobar el rango de las variables
apply(df_rioja[,1:3], 2, mean) # Comprobar el rango de las variables
apply(df_rioja[,1:3], 2, median) # Comprobar el rango de las variables
centers_rioja

g_i_rioja <- plotGraphs(df_rioja, centers_rioja)
plotSingleGraph(g_i_rioja[[1]])
plotSingleGraph(g_i_rioja[[2]])
plotSingleGraph(g_i_rioja[[3]])

# (h) Estudia las características de los clústeres obtenidos y comenta las
#     observaciones y conclusiones que se pueden extraer.

# Cálculo de las proporciones de la variable 'region' para el dataset completo
region_frame <- sapply(SubFrame2$region, function(x) names(region_to_num_mapping[as.numeric(round(x*1e08))]))
my_table <- table(region_frame) 
my_table_sorted <- my_table[order(my_table, decreasing=TRUE)]
head(my_table_sorted)
barplot(my_table_sorted[1:5])
my_prop_table <- round(prop.table(my_table_sorted)*100, 2) # Overall
head(my_prop_table)
sum(my_prop_table[1:5])
barplot(my_prop_table[1:5], 
        main = "Proporciones de los 5 vinos más frecuentes",
        ylab = "porcentaje del total (%)",
        xlab= "denominación de origen",
        ylim = c(0, 35))

# Cálculo de las proporciones de la variable 'region' los vinos más caros (>500€)
df2 <- SubFrame2
df2$region <- sapply(SubFrame2$region, function(x) names(region_to_num_mapping[as.numeric(round(x*1e08))]))
df3 <- subset.data.frame(df2, price <= 600)
df4 <- subset.data.frame(df2, price <= 100)
df5 <- subset.data.frame(df2, price >= 100)
sapply(df5[1:3], mean)
my_table_2 <- table(df5$region)
my_table_sorted_2 <- my_table_2[order(my_table_2, decreasing=TRUE)]
#my_prob_table_2 <- round(prop.table(my_table_sorted_2)*100, 2)
my_table_sorted_2
par(mar=c(7,3,3,1)+.1)
barplot(my_table_sorted_2, cex.names = 0.8, 
        las=2, main = "Tabla de Frecuencias de los Vinos más Caros (>500€)")

hist(df4$price,
     main="Precios de los vinos del dataset completo",
     xlab="Euros (€)")

# Histograma de precios
hist_info <- hist(SubFrame2$price,
     main="Histograma de precios de los vinos",
     xlab="Euros (€)",
     xlim=c(0,500),
     breaks=c(seq(0,500,10),5000),
     #breaks=c(0, 100, 200, 500, 5000),
     plot = TRUE,
     freq = FALSE)

hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100
hist_info$density <- cumsum(hist_info$density) 
hist_info

hist(df_rioja$price)

      