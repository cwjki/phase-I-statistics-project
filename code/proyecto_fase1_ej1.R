#Seg?n el estudio Anibes (2013), la ingesta media diaria de az?cares a?adidos
#por una persona
population <- rnorm(500, 34, 1)

#Auxiliares
library(modeest)
index <- c(20,30,50,150)

#Intervalo de confianza para la media
IntervalMedia <- function(sample, alpha, var = NULL){
  n = length(sample)
  i = 1 - alpha/2
  sqrt_n = sqrt(n)
  x = mean(sample)
  z = qnorm(i)
  var_s = var(sample)
  
  if (!is.null(var))
  {
    e = z * (var / sqrt_n)
    return (c(x - e, x + e))
  }
  else if(n <= 30)
  {
    e = qt(i, n - 1) * (var_s / sqrt_n)
    return (c(x - e, x + e))
  }
  else
  {
    e = z * (var_s / sqrt_n)
    return (c(x - e, x + e))
  }
}

#Intervalo de confianza para la varianza
IntervalVar <- function(sample, alpha){
  i = 1 - alpha / 2
  var_s = var(sample)
  n = length(sample)
  e1 = qchisq(i, n - 1)
  e2 = qchisq(alpha / 2, n - 1)
  
  return (c(((n - 1) * var_s) / e1 , ((n - 1) * var_s) / e2 ))
}


#Muestras sin reemplazo
for (i in 1:4){
  my_sample <- sample(population, index[i], replace=FALSE)

  r.mean <- mean(my_sample)
  r.median <- median(my_sample)
  r.mode <- mlv(my_sample, method = "meanshift")
  r.var <- var(my_sample)
  r.sd <- sd(my_sample)
  r.cv <- r.sd / r.mean 
  
  my_interval_media <- IntervalMedia(my_sample, 0.05, r.var)
  my_interval_var <- IntervalVar(my_sample, 0.05)
  
  #imprimir todo:
  print("**********************************************************************")
  print(paste("Estad?sticos Descriptivos para muestra sin reemplazo de tama?o: ", index[i]))
  print("**********************************************************************")
  print(paste('Media Aritm?tica = ', r.mean))
  print(paste('Mediana = ', r.median))
  print(paste('Moda = ', r.mode))
  print(paste('Varianza = ', r.var))
  print(paste('Desviaci?n Est?ndar = ', r.sd))
  print(paste('Coeficiente de Variaci?n = ', r.cv))
  print("Cuartiles:")
  print(quantile(my_sample))
  print("Intervalo de Confianza para la Media:")
  print(my_interval_media)
  print("Intervalo de Confianza para la Varianza:")
  print(my_interval_var)
  print("**********************************************************************")
  
}

#Muestras con reemplazo
for (i in 1:4){
  my_sample <- sample(population, index[i], replace=TRUE)
  
  r.mean <- mean(my_sample)
  r.median <- median(my_sample)
  r.mode <- mlv(my_sample, method = "meanshift")
  r.var <- var(my_sample)
  r.sd <- sd(my_sample)
  r.cv <- r.sd / r.mean
  
  my_interval_media <- IntervalMedia(my_sample, 0.05, r.var)
  my_interval_var <- IntervalVar(my_sample, 0.05)
  
  #Imprimir todo:
  print("**********************************************************************")
  print(paste("Estad?sticos Descriptivos para muestra con reemplazo de tama?o: ", index[i]))
  print("**********************************************************************")
  print(paste('Media Aritm?tica = ', r.mean))
  print(paste('Mediana = ', r.median))
  print(paste('Moda = ', r.mode))
  print(paste('Varianza = ', r.var))
  print(paste('Desviaci?n Est?ndar = ', r.sd))
  print(paste('Coeficiente de Variaci?n = ', r.cv))
  print("Cuartiles:")
  print(quantile(my_sample))
  print("Intervalo de Confianza para la Media:")
  print(my_interval_media)
  print("Intervalo de Confianza para la Varianza:")
  print(my_interval_var)
  print("**********************************************************************")
}

#Medidas de tendencia central para la poblaci?n
r.mean <- mean(population)
r.median <- median(population)
r.mode <- mlv(population, method = "meanshift")
r.var <- var(population)
r.sd <- sd(population)
r.cv <- r.sd / r.mean

#Imprimir todo:
print("**********************************************************************")
print("Estad?sticos Descriptivos para la poblaci?n: 500")
print("**********************************************************************")
print(paste('Media Aritm?tica = ', r.mean))
print(paste('Mediana = ', r.median))
print(paste('Moda = ', r.mode))
print(paste('Varianza = ', r.var))
print(paste('Desviaci?n Est?ndar = ', r.sd))
print(paste('Coeficiente de Variaci?n = ', r.cv))
print("Cuartiles:")
print(quantile(population))
my_interval_media <- IntervalMedia(population, 0.05, r.var)
my_interval_var <- IntervalVar(population, 0.05)
print("Intervalo de Confianza para la Media:")
print(my_interval_media)
print("Intervalo de Confianza para la Varianza:")
print(my_interval_var)
print("**********************************************************************")


#Gr?ficos para la poblaci?n
par(mfrow = c(1,2))
boxplot(population, col="red3", main = "Gráfico de caja y bigote", ylab = "Gramos de azúcar")
hist(population,freq=F,col="green3", main = "Histograma", xlab = "Gramos de azúcar", ylab = '');
box()






