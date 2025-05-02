probEventoMontecarlo <- function(espacioMuestral,evento,n) {
  resultado <- replicate(n, {
    evento == sample(espacioMuestral, 1)
  })
  mean(resultado)
}