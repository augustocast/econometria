# Configuración ----
#Las líneas comentadas se deben correr si los paquetes no están instalados
#install.packages('xlsx')
#install.packages('readxl')
#install.packages('corrplot')
library(xlsx)
library(readxl)
setwd('/home/augusto/udesa/econometria/tp1')

# Parámetros ----
#Le damos valor a los parámetros b poblacionales que vamos a usar a lo largo
#de todo el ejercicio
b0 <- 300
b1 <- 1
b2 <- 1
b3 <- -1
# Ejercicio 1 ----
# 1.
#Creamos la lista df_list con 50 dataframes que contienen [x1, x3, x3, u, y]
#definimos x1, x2, x3 como vectores de 100 observaciones con distribución uniforme [0,100].
#u sigue una distribución normal con media 0 y varianza 1600
#y = b0 + b1*x1 + b2*x2 +  b3*x3 + u
# dfi <- df_list[[i]]
df_list <- lapply(seq(1, 50), function(i) {
  set.seed(i)
  x1 <- runif(100, 0, 100)
  x2 <- runif(100, 0, 100)
  x3 <- runif(100, 0, 100)
  u <- rnorm(100, 0, sqrt(1600))
  y <- b0 + b1 * x1 + b2 * x2 +  b3 * x3 + u
  data.frame(x1, x2, x3, u, y)
})

#2.
correlation_sample_df <- function(df_list, i) {
  #grafica la matriz de correlación para df_i de la lista df_list
  require(corrplot)
  df <- df_list[[i]]
  independent_variables <- data.frame(df$x1, df$x2, df$x3)
  colnames(independent_variables)  <- c('x1', 'x2', 'x3')
  corr <- cor(independent_variables)
  corrplot(corr, method = 'number')
}

correlation_sample_df(df_list, 1)

#3.
#Estimamos el modelo y = β0 + β1 x1 + β2 x2 + β3 x3 + ui para todos los df_i contenidos en df_list
ols <- lapply(df_list, function(x) {
  lm(y ~ x1 + x2 + x3, data = x)
})

#Extraemos los coeficientes y los exportamos en un excel
coefficients <- t(sapply(ols, function(x)
  x$coefficients))
colnames(coefficients) <- c('b0', 'b1', 'b2', 'b3')
write.xlsx(coefficients, 'primera_estimacion.xlsx', row.names = FALSE)

#4.
#Importamos el archivo generado anteriormente
coefficients <- read_excel('primera_estimacion.xlsx')

#Graficamos b1 contra b2 y b1 contra b3 (hay qu tunearle los índices)
par(mfrow = c(1, 2))
plot(coefficients$b1, coefficients$b2)
plot(coefficients$b1, coefficients$b3)

#5.
x2_corr <- function(x1) {
  #La función genera x2, altamente correlacionada con x1
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

#Creamos la lista df_list_corr con 50 dataframes que contienen [x1, x3, x3, u, y]
#definimos x1 y x3 como vectores de 100 observaciones con distribución uniforme [0,100].
#x2 se define segun x2_corr, para estar altamente correlacionada con x1
#u sigue una distribución normal con media 0 y varianza 1600
#dfi <- df_list_corr[[i]]
df_list_corr <- lapply(seq(1, 50), function(i) {
  set.seed(i)
  x1 <- runif(100, 0, 100)
  x2 <- x2_corr(x1)
  x3 <- runif(100, 0, 100)
  u <- rnorm(100, 0, sqrt(1600))
  y <- b0 + b1 * x1 + b2 * x2 +  b3 * x3 + u
  data.frame(x1, x2, x3, u, y)
 })

#6.
#Estimamos el modelo y = β0 + β1 x1 + β2 x2 + β3 x3 + ui para todos los df_i contenidos en df_list_corr
ols_corr <- lapply(df_list_corr, function(x) {
  lm(y ~ x1 + x2 + x3, data = x)
})

#Extraemos los coeficientes y los exportamos en un excel
coefficients_corr <- t(sapply(ols_corr, function(x)
  x$coefficients))

colnames(coefficients_corr) <- c('b0', 'b1', 'b2', 'b3')
write.xlsx(coefficients_corr, 'segunda_estimacion.xlsx', row.names = FALSE)

#7.
#Importamos el archivo generado anteriormente
coefficients_corr <- read_excel('segunda_estimacion.xlsx')

#Graficamos b1 contra b2 y b1 contra b3 (hay qu tunearle los índices)
par(mfrow = c(1, 1))
plot(coefficients_corr$b1, coefficients_corr$b2)

# Ejercicio 2 ----
#1.
##Estimamos por OLS el modelo y = β0 + β1x1  + β3x3 + ui para todos los df de df_list
ols_bias <- lapply(df_list, function(x) {
  lm(y ~ x1 + x3, data = x)
})

#Extraemos los coeficientes y los exportamos en un excel
coefficients_bias <- sapply(ols_bias, function(x)
  x$coefficients)
coefficients_bias <- t(coefficients_bias)
colnames(coefficients_bias) <- c('b0', 'b1', 'b3')
write.xlsx(coefficients_bias, 'tercera_estimacion.xlsx', row.names = FALSE)

#Importamos el archivo generado anteriormente
coefficients_bias <- read_excel('tercera_estimacion.xlsx')

#Graficamos b1 contra b3. Hay que revisar graficos
par(mfrow = c(1, 1))
plot(coefficients_bias$b1, coefficients_bias$b3)

#2.
##Estimamos por OLS el modelo y = β0 + β1x1  + β3x3 + ui
#para todos los df de df_list_corr, correspondiente al ejercicio 5.1
ols_corr_bias <- lapply(df_list_corr, function(x) {
  lm(y ~ x1 + x3, data = x)
})

#Extraemos los coeficientes y los exportamos en un excel
coefficients_corr_bias <-
  sapply(ols_corr_bias, function(x)
    x$coefficients)
coefficients_corr_bias <- t(coefficients_corr_bias)
colnames(coefficients_corr_bias) <- c('b0', 'b1', 'b3')
write.xlsx(coefficients_corr_bias, 'cuarta_estimacion.xlsx', row.names = FALSE)

#Importamos el archivo generado anteriormente
coefficients_corr_bias <- read_excel('cuarta_estimacion.xlsx')

#hay dos versiones (revisar indices)
par(mfrow = c(1, 1))
plot(coefficients_corr_bias$b1, coefficients_corr_bias$b3)
