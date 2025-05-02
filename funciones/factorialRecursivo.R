factorialRecursivo <- function(n) {
  if (n == 0) {
    1
  } else {
    n * factorialRecursivo(n-1)
  }
}