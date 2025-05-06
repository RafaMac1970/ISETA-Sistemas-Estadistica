library(plotrix)
library(moments)
source("funciones/quantiles.R")
source("funciones/ic95.R")
source("funciones/cv.R")
# El argumento debe ser un tibble agrupado de acuerdo a la agregación que se desee
todosLosIndices <- function(unTibbleAgrupado){
   reframe(unTibbleAgrupado, n = n(), 
                             min = min(valor), 
                             q1 = q1(valor), 
                             mediana = median(valor), 
                             q3 = q3(valor), 
                             RIQ = q3 - q1, 
                             Max = max(valor), 
                             RAbs = Max - min, 
                             media = mean(valor), 
                             varianza = var(valor), 
                             sd = sd(valor), 
                             cv = sd/media, 
                             EE = std.error(valor), 
                             asimetria = skewness(valor), 
                             curtosis = kurtosis(valor), 
                             "IC95-" = ic95(valor)[1], 
                             "IC95+" = ic95(valor)[2])
  
}
