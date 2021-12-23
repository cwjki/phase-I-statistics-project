#install.packages("modeest")
library(modeest)

# Importamos la tabla con los datos de las pel?culas
my_data <- read.csv("IMDB.csv", sep = ",", dec = ".")

title <- my_data$Title


#Para entender datos num?ricos que aparecen acompa?ados de caracteres no num?ricos
convertCurrency <- function(currency){
  currency1 <- sub('$','',as.character(currency), fixed=TRUE)
  currency2 <- as.numeric(gsub('\\,','',as.character(currency1)))
}
convertTime <- function(time){
  time1 <- as.numeric(sub(' min','',as.character(time), fixed=TRUE))
}


#Par?metro 1: Rating
rating <- my_data$Rating
print("**********************************************************************")
print("Estad?sticos Descriptivos para el Rating de las pel?culas")
print("**********************************************************************")
r_mean <- mean(rating)
r_sd <- sd(rating)
print(paste('Media Aritm?tica = ', r_mean))
print(paste('Mediana = ', median(rating)))
print(paste('Moda = ', mlv(rating, method = "meanshift")))
print(paste('Varianza = ', var(rating)))
print(paste('Desviaci?n Est?ndar = ', r_sd))
print(paste('Coeficiente de Variaci?n = ', r_sd / r_mean))
print("Cuartiles:")
print(quantile(rating))



##para las conclusiones, las peliculas que cumplen con la moda
#for (i in (1:length(rating))){
#  r <- rating[i]
#  if (! is.na(r) && r == mfv(rating, na_rm = TRUE)){
#    print(title[i])
#  }
#}

print("**********************************************************************")




#Par?metro 2: Total de Votos
my_data$TotalVotes <- convertCurrency(my_data$TotalVotes)
totalvotes <- my_data$TotalVotes

print("**********************************************************************")
print("Estad?sticos Descriptivos para el Total de Votos de las pel?culas")
print("**********************************************************************")
tv_mean <- mean(totalvotes)
tv_sd <- sd(totalvotes)
print(paste('Media Aritm?tica = ', tv_mean))
print(paste('Mediana = ', median(totalvotes)))
print(paste('Moda = ', mlv(totalvotes, method = "meanshift")))
print(paste('Varianza = ', var(totalvotes)))
print(paste('Desviaci?n Est?ndar = ', tv_sd))
print(paste('Coeficiente de Variaci?n = ', tv_sd / tv_mean))
print("Cuartiles:")
print(quantile(totalvotes))


##para las conclusiones, las peliculas que cumplen con la moda
#for (i in (1:length(totalvotes))){
#  r <- totalvotes[i]
#  if (! is.na(r) && r == mfv(totalvotes, na_rm = TRUE)){
#    print(title[i])
#  }
#}

print("**********************************************************************")




#Par?metro 3: Duraci?n
my_data$Runtime <- convertTime(my_data$Runtime)
runtime <- my_data$Runtime

print("**********************************************************************")
print("Estad?sticos Descriptivos para la Duraci?n de las pel?culas")
print("**********************************************************************")
ru_mean <- mean(runtime, na.rm = TRUE)
ru_sd <- sd(runtime, na.rm = TRUE)
print(paste('Media Aritm?tica = ', ru_mean))
print(paste('Mediana = ', median(runtime, na.rm = TRUE)))
print(paste('Moda = ', mfv(runtime, na_rm = TRUE)))
print(paste('Varianza = ', var(runtime, na.rm = TRUE)))
print(paste('Desviaci?n Est?ndar = ', ru_sd))
print(paste('Coeficiente de Variaci?n = ', ru_sd / ru_mean))
print("Cuartiles:")
print(quantile(runtime, na.rm = TRUE))


##para las conclusiones, las peliculas que cumplen con la moda
#for (i in (1:length(runtime))){
#  r <- runtime[i]
#  if (! is.na(r) && r == mfv(runtime, na_rm = TRUE)){
#    print(title[i])
#  }
#}


print("**********************************************************************")


#Gr?ficos
par(mfrow = c(2,3))

#Cajas de antena y bigote (Rating, TotalVotes, Runtime)
boxplot(rating,main = "Rating", col="red3", ylab = "Puntuaci?n")

boxplot(totalvotes,main = "Total de Votos", col="red3", ylab = "Votos")

boxplot(runtime,main="Tiempo de Duraci?n", col="red3", ylab = "Minutos")

#Histogramas (Rating, TotalVotes, Runtime)
hist(rating,freq=F,col="green3",main="Rating", xlab = "Puntuaci?n", ylab = '');
box()

hist(totalvotes,freq=F,col="green3",main="Total de Votos", xlab = "Votos", ylab = '');
box()

hist(runtime,freq=F,col="green3",main="Tiempo de Duraci?n", xlab = "Minutos", ylab = '');
box()
