# Soluciones del Ejercicio 1

# 1. Objetos en R

# (a) ¿Qué contiene la constante de R LETTERS?
LETTERS # Contiene un vector con las letras del abecedario en mayúsculas

# (b) Usar la función de R class para determinar qué tipo de objeto es.
class(LETTERS) # Es de tipo "character"

# (c) Construir el vector MisLetras con la 20 primeras letras mayúsculas del alfabeto.
MisLetras <- LETTERS[1:20]
MisLetras

# (d) Extraer de MisLetras el vector con los elemento del 5 al 14, ambos incluidos.
MisLetras[5:14]

# (e) Construir un vector con los 100 primeros números positivos pares.
cien_pares <- 2*(1:100)
cien_pares

# (f) Construir la matriz 10x10 MiMatriz con los 100 primeros números positivos pares, introducidos por filas.
MiMatriz <- matrix(cien_pares, 10, 10, TRUE)
MiMatriz

# (g) Extraer de MiMatriz la primera fila y la segunda columna.
MiMatriz[1,] # Obtener la primera fila
MiMatriz[,2] # Obtener la segunda columna
MiMatriz[-1, -2] # La matriz sin la primera fila ni la segunda columna

# (h) Usar la función de R diag para construir la matriz identidad 5x5 MatrizId5.
MatrizId5 <- diag(5)
MatrizId5

# (i) Construir la lista MiLista con los objetos MisLetras, MiMatriz y MatrizId5.
MiLista <- list(MisLetras, MiMatriz, MatrizId5)
MiLista

# (j) Construir el frame MiFrame con las variables 'Nombre', 'Edad', 'Carrera' y 'Curso', rellenándolo con los datos descritos a continuación:
#   'Juan' 20 'Informática' 3
#   'Ana' 19 'Matemáticas' 2
#   'Cristina' 22 'Periodismo' 1
#   'Alberto' 18 'Telecomunicaciones' 1
#   'Hugo' 21 'Informática' 4
Nombre <- c("Juan","Ana","Cristina", "Alberto", "Hugo")
Edad <- c(20, 19, 22, 18, 21)
Carrera <- c('Informática', 'Matemáticas', 'Periodismo', 'Telecomunicaciones', 'Informática')
Curso <- c(3, 2, 1, 1, 4)
MiFrame = data.frame(Nombre, Edad, Carrera, Curso)

# (k) Extraer de MiFrame la variable 'Edad'.
MiFrame$Edad

# (l) Extraer de MiFrame las variables 'Edad' y 'Curso' y asignarlas al frame MiSubframe:
MiSubframe <-subset.data.frame(MiFrame, select=c(Edad, Curso))
MiSubframe

# 2. Análisis estadístico básico

# (a) Calcular la tabla de frecuencia de la variable 'Carrera' del frame MiFrame:
table(MiFrame$Carrera)

# (b) Obtener la tabla de frecuencia relativa de la variable 'Curso' del frame MiFrame:
prop.table(table(MiFrame$Curso))

# (c) Calcular la media de la variable 'Edad':
mean(MiFrame$Edad)

# (d) Calcular mínimo, 1er cuartil, mediana, media, 3er cuartil y máximo de la variable 'Edad':
summary(MiFrame$Edad)

# (e) Calcular la desviación típica y la varianza de la variable 'Edad':
var(MiFrame$Edad); sd(MiFrame$Edad)

# (f) Representar gráficamente las variables ('Edad','Curso'):
plot(MiFrame$Edad, MiFrame$Curso)

# (g) Calcular (sin usar la función lm) los parámetros de la recta de regresión (y = ax + b) de las variables ('Edad','Curso'):
a <- cov(MiFrame$Edad, MiFrame$Curso) / var(MiFrame$Edad)
b <- mean(MiFrame$Curso) - a * mean(MiFrame$Edad)
a; b
# (h) Representar gráficamente las variables ('Edad','Curso') y la recta de regresión con un color distinto al de los marcadores:
plot(MiFrame$Edad, MiFrame$Curso, xlab = 'Edad', ylab = 'Curso', col = 3, pch = 16, cex = 1)
abline(b, a, col=2)

# Comprobar con la función lm
RectaRegresion <- lm(MiFrame$Edad~MiFrame$Curso)
abline(RectaRegresion)

# (i) Cargar los datos del archivo medidas_cuerpo.csv (disponible en Moodle) y comprueba la estructura del dataframe resultante:
repo <- "C:/Users/alex/Desktop/Máster Software Embebido/2 Segundo Semestre/2 Ciencia de Datos/Ejercicios"
setwd(repo); getwd()
datosCSV <- as.data.frame(read.csv("Ficheros/medidas_cuerpo.csv", header=TRUE, sep='\t', comment.char="#"))
head(datosCSV)

# (j) Representar gráficamente las variables ('peso','altura') y la recta de regresión , poniendo títulos a los ejes y cambiando los parámetros de forma, tamaño y color de los marcadores:
plot(datosCSV$peso, datosCSV$altura, xlab = 'peso', ylab = 'altura', col = 3, pch = 16, cex = 1)
RectaRegresion2 <- lm(datosCSV$altura~datosCSV$peso)
RectaRegresion2
abline(RectaRegresion2)
