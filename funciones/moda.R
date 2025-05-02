moda <- function(unVector){
  frecuencias <- table(unVector)
  as.numeric(names(frecuencias[frecuencias == max(frecuencias)]))
}