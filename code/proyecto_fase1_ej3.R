# Importamos la tabla con los datos de las películas
my_data <- read.csv("IMDB.csv", sep = ",", dec = ".")

#Para entender datos numéricos que aparecen acompañados de caracteres no numéricos
convertCurrency <- function(currency){
  currency1 <- sub('$','',as.character(currency), fixed=TRUE)
  currency2 <- sub(' ', '', as.character(currency1), fixed = TRUE)
  currenc32 <- as.numeric(gsub('\\,','',as.character(currency2)))
}
convertGenre <- function(currency){
  currency1 <- gsub(' ','',as.character(currency), fixed=TRUE)
}

my_data$Budget <- convertCurrency(my_data$Budget)
budget <- my_data$Budget

my_data$Genre1 <- convertGenre(my_data$Genre1)
genre1 <- my_data$Genre1
my_data$Genre2 <- convertGenre(my_data$Genre2)
genre2 <- my_data$Genre2
my_data$Genre3 <- convertGenre(my_data$Genre3)
genre3 <- my_data$Genre3

#Buscar que haya Acción y Drama en los tres posibles géneros
total_drama_budget <- 0
total_action_budget <- 0
total_drama_film <- 0
total_action_film <- 0
count_drama_na <- 0
count_action_na <- 0

for (i in (1:length(budget))){
  r <- budget[i]
  
  #Buscando Drama
  if(genre1[i] == "Drama" | genre2[i] == "Drama" | genre3[i] == "Drama"){
    total_drama_film <- total_drama_film + 1
    if(is.na(r)){
      count_drama_na <- count_drama_na + 1
    }
    else{
      total_drama_budget <- total_drama_budget + r
    }
  }
  
  #Buscando Acción
  if(genre1[i] == "Action" | genre2[i] == "Action" | genre3[i] == "Action"){
    total_action_film <- total_action_film + 1
    if(is.na(r)){
      count_action_na <- count_action_na + 1
    }
    else{
      total_action_budget <- total_action_budget + r
    }
  }
}


#Promedio de presupuesto para películas de Drama
drama_mean <-  total_drama_budget / (total_drama_film - count_drama_na)
total_drama_budget <- total_drama_budget + drama_mean*count_drama_na
drama_mean <- total_drama_budget / total_drama_film
 
#Promedio de presupuesto para películas de Acción
action_mean <-  total_action_budget / (total_action_film - count_action_na)
total_action_budget <- total_action_budget + action_mean*count_action_na
action_mean <- total_action_budget / total_action_film


#Preprocesamiento de datos para el presupuesto de películas de Drama (sin NA)
only_drama_budget <- my_data[my_data$Genre1 == "Drama" | my_data$Genre2 == "Drama" | my_data$Genre3 == "Drama",]
only_drama_budget <- na.omit(only_drama_budget$Budget)
#varianza de películas de Drama
var_drama <- var(only_drama_budget)

#Preprocesamiento de datos para el presupuesto de películas de Acción (sin NA)
only_action_budget <- my_data[my_data$Genre1 == "Action" | my_data$Genre2 == "Action" | my_data$Genre3 == "Action",]
only_action_budget <- na.omit(only_action_budget$Budget)
#varianza de películas de Acción
var_action <- var(only_action_budget)


#Para la Prueba de Hipótesis de Medias se debe hacer primero una Prueba de Hipótesis para las Varianzas

#Prueba de Hipótesis de igualdad contra diferencia para las varianzas
var.test(only_drama_budget, only_action_budget)

#Como las varianzas son distintas, se hace la Prueba de Hipótesis de Medias correspondiente
t.test(only_drama_budget, only_action_budget, alternative = "two.sided", var.equal = FALSE, conf.level = 0.05)











