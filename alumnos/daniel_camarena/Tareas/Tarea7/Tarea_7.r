library(dplyr)
setwd("~/Documents/CienciaDatos/GitHub/metodos-analiticos-2018/alumnos/daniel_camarena/Tareas/Tarea7")
if(!require(wordVectors)){
  devtools::install_github("bmschmidt/wordVectors")
}
library(wordVectors)

model <- read.vectors("Modelo/noticias_vectors.bin")
ejemplos <- model %>% closest_to("gol", n = 5)
ejemplos

model %>% closest_to("presidente", n = 5)

adv <- model[["lentamente"]] - model[["lento"]]
vector <-  model[["rápido"]] + adv
model %>% closest_to(vector, n = 10) %>% filter(word != "lentamente")

plural_1 <- model[["pelotas"]] - model[["pelota"]]
vector <-  model[["carro"]] + plural_1
model %>% closest_to(vector, n = 5) %>% filter(word != "juego")

fem_1 <- model[["niños"]] - model[["niño"]]
vector <-  model[["niña"]] + fem_1
model %>% closest_to(vector, n = 5) %>% filter(word != "niña")
