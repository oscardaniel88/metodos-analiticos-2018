library(readr)
library(dplyr)
library(arules)
library(tidyr) 
library(tidygraph)
library(ggraph)
library(plotly)


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
ap_1 <- subset(ap, size(ap) == 3) 
inspect(sort(ap_1, by='support')) 


###Generamos la funcion de hyperlift
agregar_hyperlift <- function(reglas, trans){
  quality(reglas) <- cbind(quality(reglas), 
                           hyper_lift = interestMeasure(reglas, measure = "hyperLift", 
                                                        transactions = trans))
  reglas
}


###Ejectuamos apriori con soporte 0.001 y confianza de 0.1
pars <- list(support = 0.01,
             confidence = 0.3,
             minlen = 2,
             target='rules', ext = TRUE)

b_reglas <- apriori(tr, parameter = pars)

#Agregamos hyperlift a lo que encontramos
b_reglas <- agregar_hyperlift(b_reglas, tr)

#Filtramos de las reglas obtenidas todo lo que tenga un hl mayor a 2.5 y que el tamaño de la regla sea menor a 4 con soporte del lado izquierdo mayor a 0.1
b_reglas_lift <- subset(b_reglas, 
                        hyper_lift > 2.5 & size(b_reglas) < 4 &
                          lhs.support > 0.01)
b_reglas_lift <- sort(b_reglas_lift, by = 'hyper_lift')

##Vemos lo que nos trajo el filtro
DT::datatable(DATAFRAME(b_reglas_lift)  %>%
                mutate_if(is.numeric, funs(round(., 3))))

##Sacamos otro set de reglas
b_reglas_lift <- subset(b_reglas, 
                        hyper_lift > 1.75 & confidence > 0.1)
reglas_f <- subset(b_reglas_lift, size(b_reglas_lift)==2)


##Graficamos
df_reglas <- reglas_f %>% DATAFRAME %>% rename(from=LHS, to=RHS) %>% as_data_frame
df_reglas$weight <- log(df_reglas$hyper_lift)
graph_1 <- as_tbl_graph(df_reglas) %>%
  mutate(centrality = centrality_degree(mode = "all")) 
ggraph(graph_1, layout = 'fr', start.temp=100) +
  geom_edge_link(aes(alpha=lift), 
                 colour = 'red',
                 arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  geom_node_text(aes(label = name), size=4,
                 colour = 'gray20', repel=TRUE) +
  theme_graph()


##Canastas grandes

pars <- list(support = 0.02,
             confidence = 0.20,
             minlen = 2,
             target='rules', ext = TRUE)
reglas_1 <- apriori(tr, parameter = pars)

plotly_arules(reglas_1, colors=c('red','gray'))

reglas_1 <- agregar_hyperlift(reglas_1, tr)

DT::datatable(DATAFRAME(sort(reglas_1, by='hyper_lift')) %>%
                mutate_if(is.numeric, funs(round(., 3))))

reglas_f2 <- subset(reglas_1, hyper_lift > 1.3, confidence > 0.4)
df_reglas <- reglas_f2 %>% DATAFRAME %>% rename(from=LHS, to=RHS) %>% as_data_frame
df_reglas$weight <- log(df_reglas$hyper_lift)
graph_1 <- as_tbl_graph(df_reglas) %>%
  mutate(centrality = centrality_degree(mode = 'all'))

ggraph(graph_1, layout = 'fr', start.temp=100) +
  geom_edge_link(aes(alpha=hyper_lift), colour = 'red',arrow = arrow(length = unit(4, 'mm'))) + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  geom_node_text(aes(label = name), size=4,
                 colour = 'gray20', repel=TRUE) +
  theme_graph()

