library(ggplot2)
Cuadratica <- setRefClass(
  "Cuadratica",
  fields = list(a = "numeric", b = "numeric", c = "numeric"),
  methods = list(
    initialize = function(a, b, c) {
      .self$a <- a
      .self$b <- b
      .self$c <- c
    },
    imprimir = function() {
      return(paste0("f(x) =", a, "x² +", b, "x + ", c))
    },
    f = function(aNumber) {
      return(a * aNumber ^ 2 + b * aNumber + c)
    },
    vertice = function() {
      h = -b / (2 * a)
      k = .self$f(h)
      return(c(h,k))
    },
    discriminante = function() {
      return(b^2 - 4 * a * c)
    },
    raices = function() {
      # Calculo raices con Baskara
      if (.self$discriminante() < 0) {
        preal <- (-b) / (2 * a)
        pimag <- sqrt(-.self$discriminante()) / (2 * a)
        x1 <- as.complex(paste(preal, "+", pimag, "i", sep = ""))
        x2 <- as.complex(paste(preal, "-", pimag, "i", sep = ""))
        return(c(x1, x2))
      } else {
        x1 <- (-b + sqrt(.self$discriminante())) / (2 * a)
        x2 <- (-b - sqrt(.self$discriminante())) / (2 * a)
        return(c(x1, x2))
      }
      
    },
    dibujar = function(unVector) {
      df <- data.frame(x = unVector)
      for (i in df) {
        df$y <- .self$f(i)
      }
      if (.self$discriminante() < 0) {
        puntos <- data.frame(x = c(0, .self$vertice()[1]) , y = c(.self$f(0),.self$vertice()[2]), etiqueta = c("Ord.Or.", "Vértice"))
      } else {
        puntos <- data.frame(x = c(0, .self$vertice()[1], .self$raices()[1], .self$raices()[2]) , y = c(.self$f(0),.self$vertice()[2], 0, 0), etiqueta = c("Ord.Or.", "Vértice", "Raíz", "Raíz"))
      }
      grafico <- ggplot(df) +
        aes(x = x, y = y) +
        geom_line() +
        geom_point(data = puntos, color = "blue") +
        geom_text(data = puntos, aes(label = etiqueta), hjust= 0.5, vjust = -2, size= 3, color= "black")  +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        labs(title = .self$imprimir(), 
             subtitle = paste0("raíces: (", round(.self$raices()[1], 2), ", ", round(.self$raices()[2], 2), "), Ord.Origen: ", .self$f(0), ", vertice: (", .self$vertice()[1], " ,", .self$vertice()[2], ")"))
      return(grafico)
    }
  )
)