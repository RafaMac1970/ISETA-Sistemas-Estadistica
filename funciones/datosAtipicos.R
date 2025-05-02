datosAtipicos <- function (aVector) {
  riq <- q3(aVector) - q1(aVector) # riq viene de rango inter quiartil
  min <- q1(aVector) - 1.5 * riq
  max <- q3(aVector) + 1.5 * riq
  aVector[aVector < min | aVector > max]
} 