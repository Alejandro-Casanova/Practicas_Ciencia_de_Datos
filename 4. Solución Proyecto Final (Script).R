# Soluciones del Proyecto Final

# Tenemos los datos de vinos de España de distintas denominaciones de origen, se
# requiere hacer una segmentación de vinos españoles en función de precio, calidad
# y acidez del vino.

# 1. Clustering

# (a) Instalar el paquete Tidyverse.
# install.packages("tidyverse")
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
region_names
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

# Transform into matrix for kmeans
kmdata = as.matrix(SubFrame2[,1:4])
mode(kmdata) = "numeric"
head(kmdata)

# (e) Valorar si es necesario realizar un escalado de algunos de los datos: price, rating, acidity.
# Será importante realizar un escalado de la variable precio, debido a la gran magnitud de su rango
# de valores. De todos modos,las tres variables tienen unidades distintas, y aunque no sea tan 
# crítico para las variables 'rating' y 'acidity', también normalizaremos éstas.
NUM_VAL <- 3
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
apply(df, 2, range) # Comprobar el rango de las variables
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
  
  grid.arrange(
    arrangeGrob(g1 + theme(
      legend.box.background = element_rect(),
      legend.box.margin = margin(6, 6, 6, 6))),
    arrangeGrob(g2 + theme(legend.position="none"), 
                g3 + theme(legend.position="none"), 
                ncol = 2)
  )
  
  return(list(g1,g2,g3))
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
apply(df_rioja, 2, range) # Comprobar el rango de las variables
centers_rioja

g_i_rioja <- plotGraphs(df_rioja, centers_rioja)
plotSingleGraph(g_i_rioja[[1]])
plotSingleGraph(g_i_rioja[[2]])
plotSingleGraph(g_i_rioja[[3]])

# (h) Estudia las características de los clústeres obtenidos y comenta las
#     observaciones y conclusiones que se pueden extraer.
region_frame <- sapply(SubFrame2$region, function(x) names(region_to_num_mapping[as.numeric(round(x*1e08))]))
my_table <- table(region_frame) 
my_table_sorted <- my_table[order(my_table, decreasing=TRUE)]
head(my_table_sorted)
my_prop_table <- round(prop.table(my_table_sorted)*100, 2) # Overall
head(my_prop_table)
