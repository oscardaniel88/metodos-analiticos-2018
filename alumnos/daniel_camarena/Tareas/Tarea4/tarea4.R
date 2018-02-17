install.packages('itertools')
install.packages('RcppRoll')
library(iterators)
library(itertools)
library(tidyverse)
library(digest)
library(R6)
library(stringr)
generar_trans <- function(){
  id_num <- sample.int(10000, 1)
  monto <- runif(1, 100,10000) 
  trans <- list(id = id_num, monto = monto)
}

set.seed(312)
trans <- itertools::timeout(generar_trans, 2) %>% as.list
length(trans)

sapply(trans, function(elem) elem$monto) %>% median

seleccionar_rng <- function(trans, prop = 0.01){
  runif(1) < prop
}

trans_filtradas <- keep(trans, seleccionar_rng)
length(trans_filtradas)

sapply(trans_filtradas, function(elem) elem$monto) %>% median

trans <- itertools::timeout(generar_trans, 2) %>% as.list
length(trans)

df <- bind_rows(trans)
length(unique(df$id))

df %>% group_by(id) %>% summarise(monto_max = max(monto)) %>%
  pull(monto_max) %>% median


seleccionar <- function(trans){
  hash_xx <- digest::digest(as.character(trans$id), 'xxhash32', serialize = FALSE)
  strtoi(substr(hash_xx, 1, 7) , 16L) %% 10 == 0
}
trans_filtradas_c <- keep(trans, seleccionar)
df <- bind_rows(trans_filtradas_c)
length(unique(df$id))

df %>% group_by(id) %>% summarise(monto_max = max(monto)) %>%
  pull(monto_max) %>% median

trans_filtradas <- keep(trans, function(x) seleccionar_rng(x, prop = 0.10))
length(trans_filtradas)

df <- bind_rows(trans_filtradas)
df %>% group_by(id) %>% summarise(monto_max = max(monto)) %>%
  pull(monto_max) %>% median


###Filtro de Bloom

S <- c(15, 523, 922)
hash_lista <- list(h_1 = function(x) x %% 11 + 1,
                   h_2 = function(x) (5*x + 3) %% 11 + 1)

v <- rep(0, 11)
for(i in 1:length(S)){
  indices <- sapply(hash_lista, function(h) h(S[i]))
  indices
  print(indices)
  v[indices] <- 1 
  print(v)
}

v
x <- 219
h_x <- sapply(hash_lista, function(h) h(x))
h_x
v[h_x]
all(v[h_x])

x <- 523
h_x <- sapply(hash_lista, function(h) h(x))
all(v[h_x])

x <- 413
h_x <- sapply(hash_lista, function(h) h(x))
all(v[h_x])

tasa_fp <- function(n, s, k) {
  (1 - (1 - (1 / n)) ^ (k * s)) ^ k
}

df <- expand.grid(list(s = c(1e5, 1e6, 1e7, 1e8),
                       k = seq(1, 20),
                       n = 2^seq(20,30,1)
)) %>%
  mutate(millones_bits = round(n/1e6)) %>%
  mutate(tasa_falsos_p = tasa_fp(n, s, k)) %>%
  mutate(s_str = paste0(s, ' insertados'))

ggplot(df, aes(x = k, y = tasa_falsos_p, 
               colour=factor(millones_bits), group=millones_bits)) + 
  geom_line(size=1.2) +
  facet_wrap(~s_str) +
  labs(x="k = número de hashes", 
       colour = "Mill bits \n en vector") +
  scale_y_sqrt(breaks = c(0.01,0.05,0.1,0.25,0.5,1)) 


df_opt <- df %>% select(n, s) %>%  
  mutate(k = ceiling((n/s)*log(2))) %>% unique %>%
  mutate(tasa_falsos_p = tasa_fp(n, s, k)) %>%
  mutate(s_str = paste0(s, ' insertados'))

ggplot(df, aes(x = k, y = tasa_falsos_p)) +
  geom_line(aes(colour=factor(millones_bits), group=millones_bits),
            size=1.2) +
  facet_wrap(~s_str) +
  labs(x="k = número de hashes", 
       colour = "Mill bits \n en vector") +
  scale_y_sqrt(breaks = c(0.01,0.05,0.1,0.25,0.5,1)) +
  geom_point(data = df_opt, col='red') +
  xlim(0,20)


##Corrector ortográfico Filtro de Bloom

diccionario <- read.csv("datos/diccionario/es_dic.txt", 
                        header = FALSE, stringsAsFactors =FALSE)
diccionario <- iconv(diccionario[, 1], to = 'utf-8')
m <- length(diccionario)
m

df <- expand.grid(list(s = 300000,
                       k = seq(4, 10),
                       n = c(1e6, 2e6, 4e6, 8e6)
)) %>%
  mutate(millones_bits = (n/1e6)) %>%
  mutate(tasa_falsos_p = tasa_fp(n, s, k)) %>%
  mutate(s_str = paste0(s, ' insertados'))

ggplot(df, aes(x = k, y = tasa_falsos_p, 
               colour=factor(millones_bits), group=millones_bits)) + 
  geom_line(size=1.2) +
  facet_wrap(~s_str) +
  labs(x="k = número de hashes", 
       colour = "Mill bits \n en vector") +
  scale_y_log10(breaks= c(0.0001, 0.001, 0.01, 0.1))

n <- 8e6
tasa_fp(n, 3e5, 6)


set.seed(123)
hash_generator <- function(k = 6, n){
  seeds <- sample.int(652346, k)
  hasher <- function(x){
    sapply(seeds, function(seed){
      # en digest, serialize puede ser false, pues trabajamos con cadenas
      # la salida de xxhash32 son 8 caracteres hexadecimales, pero 
      # solo tomamos 7 para poder convertir a un entero
      sub_str <- substr(digest::digest(x, "xxhash32", serialize = FALSE, seed = seed), 1, 7)
      strtoi(sub_str, base = 16L) %% n + 1
    })
  }
  hasher
}
hashes <- hash_generator(6, n)  

hashes('él')
hashes('el')
hashes('árbol')

BloomFilter <- R6Class("BloomFilter",
                       public = list(
                         v = NULL,
                         n = NULL,
                         hasher = NULL,
                         seeds = NULL,
                         initialize = function(num_hashes, n){
                           self$n <- n
                           self$seeds <- sample.int(883123, num_hashes)
                           self$hasher <- function(x){
                             sapply(self$seeds, function(seed){
                               sub_str <- substr(digest::digest(x, "xxhash32", 
                                                                serialize = FALSE, seed = seed), 1, 7)
                               strtoi(sub_str, base = 16L) %% n + 1
                             })
                           }
                           # usamos representación en bits para ahorrar espacio
                           self$v <- raw(self$n) 
                         },
                         add = function(x){
                           x <- iconv(x, to = 'utf-8')
                           self$v[self$hasher(x)] <- as.raw(1)
                         },
                         in_filter = function(x){
                           x <- iconv(x, to = 'utf-8')
                           all(as.logical(self$v[self$hasher(x)]))
                         }
                       ))

bloom_filter <- BloomFilter$new(num_hashes = 6, n = 8e6)

set.seed(3434)
system.time(
  for(i in seq_along(diccionario)){
    bloom_filter$add(diccionario[i])
  }
)

palabras_prueba <- c('árbol', 'arbol', 'explicásemos', 'xexplicasemos',
                     'gato', 'perror', 'perro', 'alluda','ayuda')
df_palabras <- data_frame(palabra = palabras_prueba) %>%
  mutate(pertenece = map_lgl(palabra, bloom_filter$in_filter))
df_palabras

##Correccion

generar_dist_1 <- function(palabra){
  caracteres <- c(letters, 'á', 'é', 'í', 'ó', 'ú', 'ñ')
  pares <- lapply(0:(nchar(palabra)), function(i){
    c(str_sub(palabra, 1, i), str_sub(palabra, i+1, nchar(palabra)))
  })
  eliminaciones <- pares %>% map(function(x){ paste0(x[1], str_sub(x[2],2,-1))})
  sustituciones <- pares %>% map(function(x)
    map(caracteres, function(car){
      paste0(x[1], car, str_sub(x[2], 2 ,-1))
    })) %>% flatten
  inserciones <- pares %>% map(function(x){
    map(caracteres, function(car) paste0(x[1], car, x[2]))
  }) %>% flatten
  transposiciones <- pares %>% map(function(x){
    paste0(x[1], str_sub(x[2],2,2), str_sub(x[2],1,1), str_sub(x[2],3,-1))
  })
  c(eliminaciones, sustituciones, transposiciones, inserciones)
}

generar_dist_1('perror') %>% keep(bloom_filter$in_filter)

generar_dist_1('explicasemos') %>% keep(bloom_filter$in_filter)                                  
generar_dist_1('hayuda') %>% keep(bloom_filter$in_filter) 

##Ejercicio 1

palabras_prueba <- c('chingon')
df_palabras <- data_frame(palabra = palabras_prueba) %>%
  mutate(pertenece = map_lgl(palabra, bloom_filter$in_filter))
df_palabras

bloom_filter$add('chingon')

generar_dist_1('xhingon') %>% keep(bloom_filter$in_filter)

##Ejercicio 3


N <- 100000
n_0 <- 1000
set.seed(103)
lambda <- 10*abs(sin(1:N/200)) 
datos <- data_frame(n = 1:N, res = rgamma(N, 0.01, 1)) %>% mutate(obs = cumsum(res))
ggplot(datos %>% filter(n < n_0), aes(x = n, y = obs)) +
  geom_line()

prom_exponencial <- function(init, c){
  actual <- init
  function(x){
    actual <<- c*x + (1-c)*actual
    actual
  }
}
prom_c <- prom_exponencial(datos$obs[1], 0.01)
datos_pexp <- datos %>% 
  mutate(prom_exp = map_dbl(obs, prom_c))
ggplot(datos_pexp %>% filter(n < 5000), aes(x = n)) +
  geom_line(aes(y = obs), alpha =0.5) +
  geom_line(aes(y = prom_exp), colour = 'red')


prom_c <- prom_exponencial(datos$obs[1], 0.005)
datos_pexp <- datos %>% 
  mutate(prom_exp = map_dbl(obs, prom_c))
ggplot(datos_pexp %>% filter(n < 5000), aes(x = n)) +
  geom_line(aes(y = obs), alpha =0.5) +
  geom_line(aes(y = prom_exp), colour = 'red')

