calcularMediana <- function(unVector) {
  vectorOrdenado <- sort(unVector)
  if (length(vectorOrdenado) %% 2 == 0) {
    (vectorOrdenado[length(vectorOrdenado) / 2] + vectorOrdenado[(length(vectorOrdenado) / 2) + 1]) / 2
  } else {
    vectorOrdenado[(length(vectorOrdenado) / 2) + 1]
  }
}