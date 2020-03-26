# Configuración ----
#install.packages('xlsx')
#install.packages('corrplot')
#install.packages('ggplot2')
#install.package('readxl')
library(xlsx)
library(ggplot2)
library(readxl)
setwd('/home/augusto/udesa/econometria/tp1')


# Parámetros ----

#Le damos valor a los parámetros b poblacionales
b0 <- 300
b1 <- 1
b2 <- 1
b3 <- -1
# Ejercicio 1 ----
# 1.
#Creamos 50 dataframes con el nombre dfi que contienen [x1, x3, x3, u, y]
#definimos x1, x2, x3 como vectores de 100 observaciones con distribución uniforme [0,100].
#u sigue una distribución normal con media 0 y varianza 1600
#y = b0 + b1*x1 + b2*x2 +  b3*x3 + u
for (i in 1:50) {
  set.seed(i)
  x1 <- runif(100, 0, 100)
  x2 <- runif(100, 0, 100)
  x3 <- runif(100, 0, 100)
  u <- rnorm(100, 0, sqrt(1600))
  y <- b0 + b1 * x1 + b2 * x2 +  b3 * x3 + u
  nam <- paste('df', i, sep = '')
  assign(nam, data.frame(x1, x2, x3, u, y))
}

#2.

correlation_sample_df <- function(df) {
  #'grafica la matriz de correlación para df
  require(corrplot)
  independent_variables <- data.frame(df$x1, df$x2, df$x3)
  colnames(independent_variables)  <- c('x1', 'x2', 'x3')
  corr <- cor(independent_variables)
  corrplot(corr, method = 'number')
  #corrplot.mixed(corr)
}
correlation_sample_df(df1)



#Hacemos una lista de todas las variables que comienzan con df en el Enviroment
df_list <- lapply (ls(patt = '^df'), get)

#Estimamos por OLS el modelo y = β0 + β1 x1 + β2 x2 + β3 x3 + ui para todos los df
ols <- lapply(df_list, function(x) {
  lm(y ~ x1 + x2 + x3, data = x)
})

#Extraemos los coeficientes y los exportamos en un excel
coefficients <- sapply(ols, function(x) x$coefficients)
coefficients <- t(coefficients)
colnames(coefficients) <- c('b0', 'b1', 'b2', 'b3')
write.xlsx(coefficients, 'primera_estimacion.xlsx', row.names = FALSE)

#4.

#Importamos el archivo generado anteriormente
df <- read_excel('primera_estimacion.xlsx')

#Graficamos b1 contra b2 y b1 contra b3 (hay qu tunearle los índices)
par(mfrow=c(1,2))
plot(df$b1, df$b2)
plot(df$b1, df$b3)

#Son buenas las estimaciones?

#5.
rm(df)
x2_corr <- function(x1) {
  #'La función genera x2, altamente correlacionada con x1
  x2 <- scale(matrix(rnorm(100), ncol = 1))
  xs <- cbind(scale(x1), x2)
  c1 <- var(xs)
  chol1 <- solve(chol(c1))
  newx <- xs
  newc <- matrix(c(1 , 0.987,
                   0.987, 1), ncol = 2)
  eigen(newc)
  chol2 <- chol(newc)
  xm2 <- newx %*% chol2 * sd(x1) + mean(x1)
  x2 <- xm2[, 2]
}


#Creamos 50 dataframes con el nombre dfi que contienen [x1, x3, x3, u, y]
#definimos x1 y x3 como vectores de 100 observaciones con distribución uniforme [0,100].
#x2 se define segun x2_corr, para estar altamente correlacionada con x1
#u sigue una distribución normal con media 0 y varianza 1600
#y = b0 + b1*x1 + b2*x2 +  b3*x3 + u
for (i in 1:50) {
  set.seed(i)
  x1 <- runif(100, 0, 100)
  x2 <- x2_corr(x1)
  x3 <- runif(100, 0, 100)
  u <- rnorm(100, 0, sqrt(1600))
  y <- b0 + b1 * x1 + b2 * x2 +  b3 * x3 + u
  nam <- paste('df', i, sep = '')
  assign(nam, data.frame(x1, x2, x3, u, y))
}


#6.
#Hacemos una lista de todas las variables que comienzan con df en el Enviroment
df_list <- lapply (ls(patt = '^df'), get)

#Aplicamos la estimación de OLS a todos los df
ols <- lapply(df_list, function(x) {
  lm(y ~ x1 + x2 + x3, data = x)
})

#Extraemos los coeficientes y los exportamos en un excel
coefficients <- sapply(ols, function(x)
  x$coefficients)
coefficients <- t(coefficients)
colnames(coefficients) <- c('b0', 'b1', 'b2', 'b3')
write.xlsx(coefficients, 'segunda_estimacion.xlsx', row.names = FALSE)


#7.
#Importamos el archivo generado anteriormente
df <- read_excel('segunda_estimacion.xlsx')

#Graficamos b1 contra b2 y b1 contra b3 (hay qu tunearle los índices)

#hay dos versiones (revisar indices)
par(mfrow=c(1,1))
plot(df$b1, df$b2)


#Dejaron de ser MELI?

#8. Que problema hay


# Ejercicio 2 ----
#rm(list = ls())
#1. #Creamos los mismos dataframes que en el inciso 1 del ejercicio 1 (baja correlación x1, x2)
for (i in 1:50) {
  set.seed(i)
  x1 <- runif(100, 0, 100)
  x2 <- runif(100, 0, 100)
  x3 <- runif(100, 0, 100)
  u <- rnorm(100, 0, sqrt(1600))
  y <- b0 + b1 * x1 + b2 * x2 +  b3 * x3 + u
  nam <- paste('df', i, sep = '')
  assign(nam, data.frame(x1, x2, x3, u, y))
}

#Hacemos una lista de todas las variables que comienzan con df en el Enviroment
df_list <- lapply (ls(patt = '^df'), get)

##Estimamos por OLS el modelo y = β0 + β1x1  + β3x3 + ui para todos los df
ols <- lapply(df_list, function(x) {
  lm(y ~ x1 + x3, data = x)
})

#Extraemos los coeficientes y los exportamos en un excel
coefficients <- sapply(ols, function(x) x$coefficients)
coefficients <- t(coefficients)
colnames(coefficients) <- c('b0', 'b1', 'b3')
write.xlsx(coefficients, 'tercera_estimacion.xlsx', row.names = FALSE)

#Importamos el archivo generado anteriormente
df <- read_excel('tercera_estimacion.xlsx')

#hay dos versiones (revisar indices)
par(mfrow=c(1,1))
plot(df$b1, df$b3)

#2. #Creamos los mismos dataframes que en el inciso 5 del ejercicio (alta correlacion x1, x2)
rm(df)

x2_corr <- function(x1) {
  #'La función genera x2, altamente correlacionada con x1
  x2 <- scale(matrix(rnorm(100), ncol = 1))
  xs <- cbind(scale(x1), x2)
  c1 <- var(xs)
  chol1 <- solve(chol(c1))
  newx <- xs
  newc <- matrix(c(1 , 0.987,
                   0.987, 1), ncol = 2)
  eigen(newc)
  chol2 <- chol(newc)
  xm2 <- newx %*% chol2 * sd(x1) + mean(x1)
  x2 <- xm2[, 2]
}
for (i in 1:50) {
  set.seed(i)
  x1 <- runif(100, 0, 100)
  x2 <- x2_corr(x1)
  x3 <- runif(100, 0, 100)
  u <- rnorm(100, 0, sqrt(1600))
  y <- b0 + b1 * x1 + b2 * x2 +  b3 * x3 + u
  nam <- paste('df', i, sep = '')
  assign(nam, data.frame(x1, x2, x3, u, y))
}

#Hacemos una lista de todas las variables que comienzan con df en el Enviroment
df_list <- lapply (ls(patt = '^df'), get)

##Estimamos por OLS el modelo y = β0 + β1x1  + β3x3 + ui para todos los df
ols <- lapply(df_list, function(x) {
  lm(y ~ x1 + x3, data = x)
})

#Extraemos los coeficientes y los exportamos en un excel
coefficients <- sapply(ols, function(x) x$coefficients)
coefficients <- t(coefficients)
colnames(coefficients) <- c('b0', 'b1', 'b3')
write.xlsx(coefficients, 'cuarta_estimacion.xlsx', row.names = FALSE)

#Importamos el archivo generado anteriormente
df <- read_excel('cuarta_estimacion.xlsx')

#hay dos versiones (revisar indices)
par(mfrow=c(1,1))
plot(df$b1, df$b3)
