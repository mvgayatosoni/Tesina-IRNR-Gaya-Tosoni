##Modelo costo 2 

Modelo_cost2 <- function(lpars){
  Modelo_cost(c(exp(lpars),u=-5.47e-04,gamma1 = 15.314955, gamma2 = 1.01, A= 1.11))
  #Modelo_BW_cost(lpars)
}
