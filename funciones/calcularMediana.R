calcularMediana <- function(aVector) {
  dataOrdenado <- sort(aVector)
  if (length(dataOrdenado) %% 2 == 0) {
    (largosOrd[length(dataOrdenado) / 2] + dataOrdenado[(length(dataOrdenado) / 2) + 1]) / 2
  } else {
    dataOrdenado[(length(dataOrdenado) / 2) + 1]
  }
}