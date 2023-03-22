# Crear un vector con los títulos
Ej1 <- as.vector(games$title)
Ej1

# Crear un vector numérico con el precio final
pFinal <- as.numeric(games$price_final)
pFinal

#Condición lógica de precios bajos
pBajos <- pFinal < 10.99
pBajos

#Sumar 5 al vector creado
nPrecio <- pBajos + 5
nPrecio

# Crear un vector numérico con la puntuación y dividir entre 2
rPunt <- as.numeric(games$positive_ratio)
rPuntMitad <- rPunt / 2
rPuntMitad

#Calcular la media, moda, max, min de los datos de tipo numérico.
#Verficar conla función Class
class(games)

# Calcular la media de los datos de tipo numérico
mRating <- mean(games$positive_ratio)
mRating

mUsers <- mean(games$user_reviews)
mUsers

mPO <- mean(games$price_original)
mPO

mPF <- mean(games$price_final)
mPF

mDiscount <- mean(games$discount)
mDiscount

# Calcular la moda de los datos de tipo numérico
modRating <- mode(games$positive_ratio)
modRating

modUsers <- mode(games$user_reviews)
modUsers

modPO <- mode(games$price_original)
modPO

modPF <- mode(games$price_final)
modPF

modDiscount <- mode(games$discount)
modDiscount

# Calcular el maximo de los datos de tipo numérico
maxRating <- max(games$positive_ratio)
maxRating

maxUsers <- max(games$user_reviews)
maxUsers

maxPO <- max(games$price_original)
maxPO

maxPF <- max(games$price_final)
maxPF

maxDiscount <- max(games$discount)
maxDiscount

# Calcular el minimo de los datos de tipo numérico
minRating <- min(games$positive_ratio)
minRating

minUsers <- min(games$user_reviews)
minUsers

minPO <- min(games$price_original)
minPO

minPF <- min(games$price_final)
minPF

minDiscount <- min(games$discount)
mDiscount

# Crear un dataFrame de 13 columnas con la base de datos
nDf <- data.frame(games[,1:13])
nDf

#Agregar filas y columnas a la matriz
nMat <- matrix(c(games$price_final[1:9]), nrow = 3)
nMatr <- cbind(nMat, 1)
Juegos <- c(0.8, 0.9, 0.7, 1)
nMatri <- rbind(nMatr, Juegos)
nMatri

#Eliminar filas y columnas de la matriz
colnames(nMatri) <- NULL
rownames(nMatri) <- NULL
nMatri

#Seleccionar elementos de la matriz
nMatri[1,2]
nMatri[3,3]

# Convertir la matriz en un df y colocar nombres a las columnas
df <- as.data.frame(nMatri)
colnames(df) <- c("A", "B", "C","D")
df

#Acceder a los datos del dataframe
df$A
df$B

#Cambiar nombre de dataframe
NuevoNombre <- assign("NuevoNombre",nMatri)
NuevoNombre

#Seleccionar elementos del nuevo df
NuevoNombre[1,2]
NuevoNombre[3,3]

#Importar datos y ordenar los datos con la función order()
pFinal_Ord <- order(games$price_final)
pFinal_Ord

#Mostrar el dataframe ordenado de manera ascendente y descendente
pFinal_OrdDes <- order(pFinal_Ord, decreasing = TRUE)
pFinal_OrdDes

pFinal_OrdAsc <- order(games$price_final)
pFinal_OrdAsc

#Calcular el resumen estadístico de los datos con la función que corresponde
summary(games)
summary(pFinal_Ord)

#Realizar las graficas
plot(games$price_final,games$positive_ratio)
g1 <- subset(games, games$title %in% c('Inquisitor','Call of Duty: World at War'))
barplot(g1$price_final, main = "Comparación de precio de videojuego",
        ylab = "Precio", xlab = "Videojuego",
        names.arg = c("Call of Duty","Inquisitor"))

#Implementar una función para la multiplicación de dos vectores(xy)
#Probar con valores
DescuentoPfinal <- games$price_final*0.10
pFinalDesc <- games$price_final - DescuentoPfinal
pFinalDesc

funDesc <- function(desc) {
  DescuentoPfinal <- games$price_final - DescuentoPfinal
  return(DescuentoPfinal)
}
funDesc()


#Implementar una función con la ecuación de Bhaskara
#Probar con valores

bhaskara <- function(a, b, c) {
  discriminante <- b^2 - 4 * a * c
  if (discriminante < 0) {
    print("No hay soluciones reales")
  } else if (discriminante == 0) {
    x <- -b / (2 * a)
    print(paste("Hay una solución real:", x))
  } else {
    x1 <- (-b + sqrt(discriminante)) / (2 * a)
    x2 <- (-b - sqrt(discriminante)) / (2 * a)
    print(paste("Hay dos soluciones reales:", x1, "y", x2))
  }
}

bhaskara(1,-5,6)
bhaskara(3,4,5)

#Conocer la media muestral de n observaciones obtenidas
#independientemente de una distribución normal con media = 0 y varianza =1

dbMuestra <- rnorm(5)
print(dbMuestra)
mean(dbMuestra)

#Realizar una simulación
#Calcular las estadísticas descriptivas
#Aplicar la función que corresponde y graficar

summary(dbMuestra)
barplot(dbMuestra, names.arg = c("A","B","C","D","E"), ylab = "Valores",
        xlab = "Nombre de categoría", main = "Graficando")

funBp <- function(bp) {
  GrfBp <- barplot(dbMuestra, names.arg = c("A","B","C","D","E"), ylab = "Valores",
                   xlab = "Nombre de categoría", main = "Graficando un barplot")
  return(funBp)
}
funBp()

funPlt <- function(plt) {
  GrfPlt <- plot(dbMuestra, ylab = "Valores",
                   xlab = "Nombre de categoría", main = "Graficando un plot")
  return(funPlt)
}
funPlt()
