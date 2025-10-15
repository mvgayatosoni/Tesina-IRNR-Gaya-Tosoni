## Validación

# Cargamos las librerias

require(deSolve)
library(FME)
library(readr)

# Ingresamos como valores de los parámetros de la calibración 2021
pars <- c(
  u = -5.47e-04,
  tmax = 19.91775487 ,
  r0 = 0.02381268,
  gamma1 = 15.314955 ,
  gamma2 = 1.01,
  gamma3 = 2.87916933,
  
  k = 1.91295902,
  lam = 21.76254683,
  A= 1.11) 
#Tablas con datos reales Modelo1

data_real <- read.csv("datos_agrupados_por _semana1819.csv")
df_out_real <- as.data.frame(data_real) # Convertir el objeto out_real en un data.frame


Data_m_real<-cbind(time=df_out_real$Times,m=df_out_real$FERTILES,sd_m = sd(df_out_real$FERTILES))
Data_n_real<-cbind(time=df_out_real$Times,n=df_out_real$ESTERILES,sd_n = sd(df_out_real$ESTERILES))

# Resolvemos el modelo con los parametros iniciales (ModeloBW.R contiene el modelo a usar)

source("Modelo.R")
times <- df_out_real$Times # Vector de tiempos reales (semanas)
state<- c(m = 130,n = 349) # Condiciones iniciales
out_real <- ode(y = state, parms = pars, times = times, func = Modelo)
ini<-out_real
plot(ini)    

#Cálculo del costo del modelo. (ModeloBW_cost.R calcula el costo)

source("Costo.R")
Modelo_cost(pars)$model

##########################
# Calibracion del modelo
##########################

source("Costo2.R")
Pars <- pars[c(2,3,6,7,8)] # Selecciona 5 parametros de acuerdo a la calibración 2020-2021.
Fit <- modFit(f = Modelo_cost2, p = log(Pars)) # Realiza la calibracion con parametros logaritmados

summary(Fit)      # Brinda un resumen del ajuste (incluye el p-valor de los parametros optimales logaritmados)

c(exp(coef(Fit)),u=-5.47e-04,gamma1 = 15.314955, gamma2 = 1.01, A= 1.11)    # Brinda los valores de los parametros optimales. 

#########################################################################################################
# Grafica para comparativa entre los datos reales, el modelo ajustado inicial y el modelo calibrado final
#########################################################################################################

ini_ <- as.data.frame(ini)  # ini lo calculamos al principio
final  <- ode(y = state, parms = c(exp(coef(Fit)),u=-5.47e-04,gamma1 = 15.314955, gamma2 = 1.01, A= 1.11), times = times, func = Modelo)
final_ <- as.data.frame(final)
par(mfrow = c(1,2))
plot(Data_m_real, xlab = "t (semanas)", ylab = "fértiles",type = "b", pch = 16, col = "lightblue", lty = 2)
lines(times, ini_$m, lty = 2)
lines(times, final_$m, lty=1)
legend("topleft", c("datos", "inicial", "ajustada"), lty = c(NA,2,1), pch = c(1, NA, NA), inset = c(-0.1, -0.499), xpd = TRUE)
plot(Data_n_real, xlab = "t (semanas)", ylab = "estériles",type = "b", pch = 16, col = "orange", lty = 2)
lines(times, ini_$n, lty = 2)
lines(times, final_$n,lty=1)
par(mfrow = c(1, 1))
