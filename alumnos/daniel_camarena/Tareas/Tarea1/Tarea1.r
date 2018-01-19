library(readr)
library(dplyr)
library(arules)
library(tidyr) 

##leemos los datos desde el csv
recetas <- read_csv("datos/recetas/srep00196-s3.csv")

##Vemos las distintas regiones para poder filtrar
regiones <- recetas %>% select(region) %>% distinct(.)

##Filtramos el data set, quitamos la columna de region y escribimos un archivo csv"
latinAmerican <- recetas %>% filter(region=="LatinAmerican")
latinAmerican <- latinAmerican[,-1]
write.csv(latinAmerican, "datos/recetas/latinAmerican.csv")

##Leemos nuestro archivo CSV como un objeto transaction, asi lo pide arules y es un dolor en los tanates
tr <- read.transactions("datos/recetas/latinAmerican.csv", rm.duplicates=FALSE, format="basket", sep=",")

##Aplicamos el apriori para contar la frecuencia de cada uno de los items
pars <- list(supp = 0.005, target='frequent itemsets')
ap <- apriori(tr, parameter = pars)
ap_1 <- subset(ap, size(ap) == 2) 
inspect(sort(ap_1, by='support')) 
