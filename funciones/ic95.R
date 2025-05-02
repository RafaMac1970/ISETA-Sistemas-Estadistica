ic95 <- function(aVector) {
  alpha <- 0.05
  z <- qnorm (p = alpha / 2, lower.tail = FALSE )
  c(mean(aVector) - z * std.error(aVector), mean(aVector) + z * std.error(aVector))
}