gananciasDelProducto <- function(indiceProducto) {
  sum(ganancias2Meses[,indiceProducto,])
}
gananciasDeLaSucursal <- function(indiceSucursal) {
  sum(ganancias2Meses[indiceSucursal,,])
}
gananciasDeProductoEnSucursal <- function(indiceProducto, indiceSucursal) {
  sum(ganancias2Meses[indiceSucursal,indiceProducto,])
}