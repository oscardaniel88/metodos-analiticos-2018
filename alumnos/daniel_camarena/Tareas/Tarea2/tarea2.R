##Tarea 2
##Oscar Daniel Camarena Gomez

library(dplyr)
library(tidyverse)
library(textreuse)

##Considera la siguiente matriz de tejas-documentos:

mat <- matrix(c(0,1,0,1,0,1,0,0,1,0,0,1,0,0,1,0,0,0,1,1,1,0,0,0),
              nrow = 6, byrow = TRUE)
colnames(mat) <- c('d_1','d_2','d_3','d_4')
rownames(mat) <- c(0,1,2,3,4,5)
mat

## Sin permutar esta matriz, calcula la matriz de firmas minhash usando las siguientes funciones hash: 

# a) h1(x)=2x+1 mod 6
# b) h2(x)=3x+1 mod 6
# c) h3(x)=5x+2 mod 6

hash1 <- function(x){
  h1<-((2*x)+1) %% 6
  return (h1)
}

hash2 <- function(x){
  h2<-((3*x)+1) %% 6
  return (h2)
}

hash3 <- function(x){
  h3<-((5*x)+2) %% 6
  return (h3)
}

## Calculamos las funciones hash
x<-as.integer(rownames(mat))
h1 <- matrix(unlist(lapply(x,hash1)),ncol=1,byrow=TRUE)
colnames(h1)<-("h1")
h2 <- matrix(unlist(lapply(x,hash2)),ncol=1,byrow=TRUE)
colnames(h2)<-("h2")
h3 <- matrix(unlist(lapply(x,hash3)),ncol=1,byrow=TRUE)
colnames(h3)<-("h3")


## Agregamos los hashes a la matriz original
mat<-cbind(mat,h1)
mat<-cbind(mat,h2)
mat<-cbind(mat,h3)

mat

## Creamos la matriz de firmas inicializada en infinito
sign <- matrix(1000, nrow = 3, ncol = 4)
colnames(sign) <- c("S1","S2","S3","S4")
rownames(sign) <- c("h1","h2","h3")

## Encontramos la matriz minHash

for(i in 1:nrow(mat)){
  hash_number <- mat[i,5:7]
  sign_ones <- mat[i,1:4]==1
  ones <- which(sign_ones==TRUE)
  for (j in ones){
    for(k in 1:nrow(sign)){
      if(sign[k,j]>hash_number[k]){
        sign[k,j]<-hash_number[k]
      }
    }
  }
}


#Imprimimos la matriz minhash
sign

#La unica verdadera permutación es la funcion h3

#¿Qué tan cerca están las similitudes de Jaccard estimadas por minhash de las verdaderas similitudes?
##Similitudes verdadras
###d1 vs d2
sum(mat[,1]==mat[,2])/6
###d1 vs d3
sum(mat[,1]==mat[,3])/6
###d1 vs d4
sum(mat[,1]==mat[,4])/6
###d2 vs d3
sum(mat[,2]==mat[,3])/6
###d2 vs d4
sum(mat[,2]==mat[,4])/6
###d3 vs d4
sum(mat[,3]==mat[,4])/6

##Similitudes minhash
###d1 vs d2
sum(sign[,1]==sign[,2])/6
###d1 vs d3
sum(sign[,1]==sign[,3])/6
###d1 vs d4
sum(sign[,1]==sign[,4])/6
###d2 vs d3
sum(sign[,2]==sign[,3])/6
###d2 vs d4
sum(sign[,2]==sign[,4])/6
###d3 vs d4
sum(sign[,3]==sign[,4])/6

## Calcula la similitud de jaccard de las cadenas “Este es el ejemplo 1” y “Este es el ejemplo 2”, usando tejas de tamaño 3.
str<-character(2)
str[1]<-'Este es el ejemplo 1'
str[2]<-'Este es el ejemplo 2'

sim_jaccard <- function(a, b){
  length(intersect(a, b)) / length(union(a, b))
}

shingle_chars <- function(string, lowercase = FALSE, k = 3){
  # produce shingles (con repeticiones)
  if(lowercase) {
    string <- stringr::str_to_lower(string)
  }
  shingles <- seq(1, nchar(string) - k + 1) %>%
    map_chr(function(x) substr(string, x, x + k - 1))
  shingles
}

str_tejas <- lapply(str, shingle_chars, k = 3)

sim_jaccard(str_tejas[[1]], str_tejas[[2]])


##Funciones hash. Como vimos en clase, podemos directamente hacer hash de las tejas (que son cadenas de texto), en lugar de usar hashes de números enteros (número de renglón). Para lo siguiente, puedes usar la función hash_string del paquete textreuse (o usar la función pyhash.murmur3_32 de la librería pyhash):
str <- character(3)
str[1]<-'a'
str[2]<-'Este es el ejemplo 1'
str[3]<-'Este es el ejemplo 2'

hash_strings <- lapply(str,hash_string)

tejas <- shingle_chars(str[2],3)

hash_tejas <- lapply(tejas, hash_string)

set.seed(253)
minhash <- minhash_generator(10)

doc1 <- TextReuseTextDocument('Este es el ejemplo 1' ,  meta = list(id = "1"),hash_func = minhash,
                             keep_tokens = TRUE)
mhash1<-minhash(tokens(doc1))

doc2 <- TextReuseTextDocument('Este es el ejemplo 2' ,  meta = list(id = "2"),hash_func = minhash,
                              keep_tokens = TRUE)

mhash2<-minhash(tokens(doc2))

##Sim Jaccard
sum(mhash1==mhash2)/10

