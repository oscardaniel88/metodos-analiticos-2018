library(tidyverse)
library(stringr)
muestra_nf <- readRDS('datos/netflix/dat_muestra_nflix.rds')
pelis_nombres <- read_csv('datos/netflix/movies_title_fix.csv', 
                          col_names = FALSE, na = c("", "NA", "NULL"))
names(pelis_nombres) <- c('peli_id','año','nombre')
dim(muestra_nf)

head(muestra_nf)

##Medias Calificaciones
medias_pelis <- muestra_nf %>% 
  group_by(peli_id) %>% 
  summarise(media_peli = mean(calif), num_calif_peli = length(calif))
medias_pelis <- left_join(medias_pelis, pelis_nombres)

arrange(medias_pelis, desc(media_peli)) %>% 
  top_n(200, media_peli) %>% 
  mutate(media_peli = round(media_peli, 2)) %>%
  DT::datatable()

mean(muestra_nf$calif)

ggplot(medias_pelis, aes(x=num_calif_peli, y=media_peli)) + 
  geom_point(alpha=0.2)

arrange(medias_pelis, desc(media_peli)) %>% 
  filter(num_calif_peli > 500) %>%
  top_n(200, media_peli) %>% 
  mutate(media_peli = round(media_peli, 2)) %>%
  DT::datatable()


##Separamos el data set en usuarios y peliculas para entrenamiento y evaluacion

set.seed(28882)
valida_usuarios <- sample(unique(muestra_nf$usuario_id), 20000)
valida_pelis <- sample(unique(muestra_nf$peli_id), 2000)
dat_2 <- muestra_nf %>%
  mutate(valida_usu = usuario_id %in% valida_usuarios) %>%
  mutate(valida_peli = peli_id %in% valida_pelis)

# En validación van aquellas evaluaciones de las películas y
# usuario que seleccionamos
dat_valida <- filter(dat_2, valida_usu & valida_peli)
# En entrenamiento va el resto: algunas evaluaciones de usuarios
# seleccionados van en entrenamiento, por ejemplo (para películas
# no seleccionadas en validación)
dat_entrena <- filter(dat_2, !valida_usu | !valida_peli)

nrow(dat_entrena) + nrow(dat_valida)

##Modelo simple de prediccion

medias_pred <- dat_entrena %>%
  group_by(peli_id) %>%
  summarise(media_pred = mean(calif))
dat_valida_pred <- left_join(dat_valida, medias_pred)

table(is.na(dat_valida_pred$media_pred))

##Calculamos error medio cuadrático

recm <- function(calif, pred){
  sqrt(mean((calif - pred)^2))
}

dat_valida_pred %>% ungroup %>%
  summarise(error = recm(calif, media_pred))

entrena_usu <- sample(unique(dat_entrena$usuario_id), 50)
muestra_graf <- filter(dat_entrena, usuario_id %in% entrena_usu)
# medias generales por usuario, ee de la media
muestra_res <- muestra_graf %>% group_by(usuario_id) %>%
  summarise(media_calif = mean(calif), 
            sd_calif = sd(calif)/sqrt(length(calif)))

muestra_res$usuario_id <- reorder(factor(muestra_res$usuario_id), muestra_res$media_calif)
ggplot(muestra_res, aes(x=factor(usuario_id), y = media_calif, 
                        ymin = media_calif - sd_calif, ymax = media_calif + sd_calif)) + 
  geom_linerange() + geom_point() + xlab('Usuario') +
  theme(axis.text.x=element_blank())

set.seed(128)
n <- 50
niveles <- data_frame(persona = 1:n, nivel = rnorm(n,2))

x <- rnorm(n)
gustos <- data.frame(persona=1:n, gusto_1 = x + rnorm(n),
                     gusto_2 = -x + rnorm(n))
head(gustos,3)
cor(gustos[,2:3])


ggplot(gustos, aes(x=gusto_1, y=gusto_2)) + geom_point() +
  geom_smooth()

medicion_1 <- niveles$nivel + gustos$gusto_1+rnorm(n,0.3)
medicion_2 <- niveles$nivel + gustos$gusto_2+rnorm(n,0.3)
mediciones <- data_frame(persona = 1:n, medicion_1, medicion_2)
cor(mediciones[,2:3])

ggplot(mediciones, aes(x=medicion_1, y=medicion_2)) +
  geom_point() + geom_smooth()

medias_usuario_ent <- dat_entrena %>% 
  group_by(usuario_id) %>%
  summarise(media_usu = mean(calif), num_calif_usu = length(calif))
medias_peliculas_ent <- dat_entrena %>% 
  group_by(peli_id) %>%
  summarise(media_peli = mean(calif), num_calif_peli = length(calif))
media_gral_ent <- mean(dat_entrena$calif)
dat_valida_2 <- dat_valida %>%
  left_join(medias_usuario_ent) %>%
  left_join(medias_peliculas_ent) %>%
  mutate(media_gral = media_gral_ent) %>%
  mutate(prediccion = media_peli + (media_usu - media_gral))

dat_valida_2$prediccion[is.na(dat_valida_2$prediccion)] <- media_gral_ent

dat_valida_2 %>% ungroup %>% summarise(error = recm(calif, prediccion))

medias_peliculas <- muestra_nf %>% group_by(peli_id) %>% summarise(media_peli = mean(calif), num_calif_peli = length(calif))
media_gral <- mean(muestra_nf$calif)
medias_p_2 <- left_join(medias_peliculas, pelis_nombres)

arrange(medias_p_2, desc(media_peli)) %>% head


pred_encogida <- function(media, num_usuario, num_peli, media_usu,
                          media_peli, lambda){
  coef_usu <- num_usuario/(num_usuario+lambda)
  coef_peli <- num_peli/(num_peli + lambda)
  media + coef_usu * (media_usu - media) + coef_peli*(media_peli - media)
}

error_valida <- sapply(c(0.001,0.01,0.1,1,5,10,20,40,60,80,100,200), 
                       function(lambda){
                         dat_valida_2 <- dat_valida %>%
                           left_join(medias_usuario_ent, by='usuario_id') %>%
                           left_join(medias_peliculas_ent, by='peli_id') %>%
                           mutate(media_gral = media_gral_ent) %>%
                           mutate(prediccion = pred_encogida(
                             media_gral, num_calif_usu, num_calif_peli,
                             media_usu, media_peli, lambda)
                           )
                         faltantes <- is.na(dat_valida_2$prediccion)
                         dat_valida_2$prediccion[faltantes] <- media_gral_ent
                         error <- dat_valida_2 %>% ungroup %>%
                           summarise(error = recm(calif, prediccion))
                         as.numeric(error)
                       })

plot(error_valida)

dat_entrena_c <- dat_entrena %>%
  group_by(usuario_id) %>%
  mutate(calif_c = calif - mean(calif))

dat_entrena_c$id_seq <- as.numeric(factor(dat_entrena_c$usuario_id))
filter(pelis_nombres, str_detect(nombre,'Gremlins'))
filter(pelis_nombres, str_detect(nombre,'Harry Met'))

dat_1 <- filter(dat_entrena_c, peli_id==2897)
dat_2 <- filter(dat_entrena_c, peli_id==6482)
dat_3 <- filter(dat_entrena_c, peli_id==2660)

comunes <- inner_join(dat_1[, c('usuario_id','calif_c')], dat_2[, c('usuario_id','calif_c')] %>% rename(calif_c_2=calif_c))
comunes_2 <- inner_join(dat_1[, c('usuario_id','calif_c')], dat_3[, c('usuario_id','calif_c')] %>% rename(calif_c_2=calif_c))

ggplot(comunes, aes(x=calif_c, y=calif_c_2)) + 
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) + 
  geom_smooth() + xlab('Gremlins 1') + ylab('Gremlins 2')

ggplot(comunes_2, aes(x=calif_c, y=calif_c_2))  + 
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) + 
  geom_smooth() + xlab('Gremlins 1') + ylab('When Harry met Sally')

sim_cos <- function(x,y){
  sum(x*y, na.rm = T)/(sqrt(sum(x^2, na.rm = T))*sqrt(sum(y^2, na.rm = T)))
}

sim_cos(comunes$calif_c, comunes$calif_c_2)

sim_cos(comunes_2$calif_c, comunes_2$calif_c_2)

dat_entrena_2 <- dat_entrena_c %>% 
  ungroup() %>% 
  select(peli_id, id_seq, calif_c)


##Ejercicio 1 hacemos que la funcion ejemplos regrese el numero de pares usados 
##para evaluar la distancia coseno
ejemplos <- function(pelicula, asc=1){
  mi_peli <- filter(dat_entrena_2, peli_id==pelicula) %>% 
    rename(peli_id_1 = peli_id, calif_c_1 = calif_c)
  # vamos a calcular todas las similitudes con mi_peli - esto no es buena
  # idea y discutiremos más adelante cómo evitarlo
  datos_comp <- left_join(dat_entrena_2, mi_peli)
  # calcular similitudes
  out_sum <- datos_comp %>% 
    filter(!is.na(peli_id_1)) %>% 
    group_by(peli_id) %>%
    summarise(dist = sim_cos(calif_c, calif_c_1),
              n_pares=n()) %>% 
    left_join(medias_p_2)
  if(asc==1){
    out_sum %>% arrange((dist))  %>% select(nombre, dist, n_pares)
  }else{
    out_sum %>% arrange(desc(dist))  %>% select(nombre, dist, n_pares)
  }
  
}


##Ejercicio2
#Leaving las vegas 15084
ejemplos(15084) %>% filter(dist<0,n_pares>20) %>% head(2)
ejemplos(15084,asc=0) %>% filter(dist>0,n_pares>20) %>% head(2)
##Casino 12575
ejemplos(12575) %>% filter(dist<0,n_pares>10) %>% head(2)
ejemplos(12575,asc=0) %>% filter(dist>0,n_pares>10) %>% head(2) 

##Ejercicio3
##Man on fire 1220
ejemplos(1220,asc=0) %>% filter(dist>0,n_pares>10) %>% head(10) 

