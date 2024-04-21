# Soluciones del Ejercicio 2

# 1. Preparación de datos

# (a) Cargar en R los datos del archivo Adult-data.csv en el frame Frame0. Los datos proceden de la página
# https://archive.ics.uci.edu/ml/datasets.php (UCI Machine Learning Repository).
script_dir <- "C:/Users/alex/Desktop/Máster Software Embebido/2 Segundo Semestre/2 Ciencia de Datos/Ejercicios" # Actualizar con el directorio correcto
setwd(script_dir); getwd()
Frame0 <- as.data.frame(read.csv("Ficheros/Adult-data.csv", header=TRUE, sep=',', encoding='latin1'))
head(Frame0)

# (b) Extraer en el subframe SubFrame0 las variables 'Edad', 'Educacion-num años', 'Raza' y 'Nivel de ingresos'.
SubFrame0 <-subset.data.frame(Frame0, select=c('Edad', 'Educacion.num.años', 'Raza', 'Nivel.de.ingresos'))
head(SubFrame0)

# (c) Determinar si en SubFrame0 existen campos no definidos (con contenido ?).
subset.data.frame(SubFrame0, Edad == "?" | Educacion.num.años == "?" | Raza == "?" | Nivel.de.ingresos == "?")
# (No se encontró ningún campo no definido)

# (d) Obtener el SubFrame1 eliminando los registros de SubFrame0 con algún campo no definido.
# (En este caso no es necesario, pero la operación se realizaría como sigue)
SubFrame1 <- subset.data.frame(SubFrame0, Edad != "?" & Educacion.num.años != "?" & Raza != "?" & Nivel.de.ingresos != "?")

# (e) Obtener los rangos de variación de las variables 'Edad' y 'Educacion-num años'.
range(SubFrame1$Edad); range(SubFrame1$Educacion.num.años)

# (f) Obtener los valores que pueden tomar las variables 'Raza' y 'Nivel de ingresos'.
race_names <- unique(SubFrame1$Raza)
income_classes <- unique(SubFrame1$Nivel.de.ingresos)
race_names; income_classes
# (g) Obtener la tabla de frecuencias de la variable 'Raza'.
table(SubFrame1$Raza)

# (h) Queremos hacer un clustering de las variables 'Edad', 'Educacion-num años' y 'Nivel de ingresos', pero no
# queremos perder la información de la variable 'Raza'. Obtener el SubFrame2 redefiniendo numéricamente
# la variable 'Nivel de ingresos' para que sea representativa en el clustering y la variable 'Raza' para que
# apenas influya en el clustering.
race_to_num_mapping <- setNames(1:length(race_names)*1e-8, race_names)
income_to_num_mapping <- setNames(1:length(income_classes)*0.5, income_classes)

SubFrame2 <- SubFrame1

SubFrame2$Raza <- sapply(SubFrame2$Raza, function(x) race_to_num_mapping[as.character(x)])
range(SubFrame2$Raza)

SubFrame2$Nivel.de.ingresos <- sapply(SubFrame2$Nivel.de.ingresos, function(x) income_to_num_mapping[as.character(x)])

apply(SubFrame2, 2, range)
head(SubFrame2)

# Transform into matrix for kmeans
kmdata_orig = as.matrix(SubFrame2[,1:4])
kmdata <- kmdata_orig[,1:4]
mode(kmdata) = "numeric"
kmdata[1:10,]

# Scale data
# We'll scale all variables so they range from 0 to 1
max_val = numeric(2)
min_val = numeric(2)
for (k in 1:2) max_val[k] <- max(as.numeric(kmdata[,k]))
for (k in 1:2) min_val[k] <- min(as.numeric(kmdata[,k]))
kmdata[,1:2] <- scale(kmdata[,1:2], center=min_val, scale=(max_val-min_val))
kmdata[1:10,]
apply(kmdata, 2, range)

# 2. Clustering y análisis de los resultados

# (a) Calcular mediante el criterio elbow el valor adecuado del núumero de clusters k.
# Decide number of clusters
wss <- numeric(8) 
for (k in 1:8) wss[k] <- (sum(kmeans(kmdata, centers=k, nstart=25)$withinss))
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

# (b) Obtener con dicho valor de k el clustering correspondiente.
km2 = kmeans(kmdata,2, nstart=25)
km2 # Nos quedaremos con este

km3 = kmeans(kmdata,3, nstart=25)
km3

# (c) Obtener las gráficas bidimensionales del clustering 'Edad'-'Educacion-num años', 
# 'Edad'-'Nivel de ingresos' y 'Educacion-num años'-'Nivel de ingresos'.
NUM_CLUSTERS <- 2
km_selected <- km2
df <- as.data.frame(kmdata)
df$cluster <- factor(km_selected$cluster)
centers <- as.data.frame(km_selected$centers)

library(ggplot2)
library(grid) #gráficos en cuadrícula
library(gridExtra)

g1 <- ggplot(data=df, aes(x=Edad, y=Educacion.num.años, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=Edad, y=Educacion.num.años, color=as.factor(1:NUM_CLUSTERS)), 
             size=10, alpha=.3, show.legend=FALSE)

g2 <- ggplot(data=df, aes(x=Edad, y=Nivel.de.ingresos, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=Edad,y=Nivel.de.ingresos, color=as.factor(1:NUM_CLUSTERS)), 
             size=10, alpha=.3, show.legend=FALSE)

g3 <- ggplot(data=df, aes(x=Educacion.num.años, y=Nivel.de.ingresos, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=Educacion.num.años, y=Nivel.de.ingresos, color=as.factor(1:NUM_CLUSTERS)), 
             size=10, alpha=.3, show.legend=FALSE)

grid.arrange(arrangeGrob(grobs=list(g1 + theme(legend.position="none"),
                                    g2 + theme(legend.position="none"),
                                    g3 + theme(legend.position="none")),
                         main ="Adult Population Income Analysis", ncol=2))

# (d) A la vista de las gráficas, describir las propiedades características de cada uno de los clusters obtenidos.
# Respuesta: Los clusters 1 y 2 agrupan a las personas según su nivel de ingresos. Se puede observar en la primera
# gráfica la tendencia de que, generalmente, a mayor edad y mayor número de años de educación, también es mayor
# el nivel de ingresos

# (e) Calcular para cada uno de los clusters la tabla de frecuencia de la variable 'Raza'.
df_cluster1<-subset.data.frame(df, cluster == 1)$Raza
df_cluster1 <- sapply(df_cluster1, function(x) names(race_to_num_mapping[as.numeric(round(x*1e08))]))
df_cluster2<-subset.data.frame(df, cluster == 2)$Raza
df_cluster2 <- sapply(df_cluster2, function(x) names(race_to_num_mapping[as.numeric(round(x*1e08))]))
table(df_cluster1); round(prop.table(table(df_cluster1))*100, 2) # Cluster 1 (<=50k)
table(df_cluster2); round(prop.table(table(df_cluster2))*100, 2) # Cluster 2 (>50k)
table(SubFrame1$Raza); round(prop.table(table(SubFrame1$Raza))*100, 2) # Overall
round(table(df_cluster2) / table(SubFrame1$Raza) * 100, 2) # High income percentage per race

# (f) Comparar las tablas de frecuencia anteriores con la tabla de frecuencias de la variable 'Raza' obtenida en la sección anterior.
# Respuesta: se puede observar que en el cluster 1, que agrupa las personas con menor nivel de ingresos, la proporción de personas de raza 
# blanca es menor que en el cluster 2. Mientras que la proporción del resto de razas es generalmente mayor en el
# cluster 1 (la única excepción es la raza "asian-pacific-islander". De la misma forma, la proporción de pertenencia
# al cluster 2, por razas, es mayor para las razas blanca y asiático-pacífica.