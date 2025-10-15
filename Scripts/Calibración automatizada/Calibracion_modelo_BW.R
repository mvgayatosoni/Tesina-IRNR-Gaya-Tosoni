## Modelo BW (febrero)

# Cargamos las librerias

require(deSolve)
library(FME)
library(readr)

# Ingresamos como valores de los parámetros del jupyter notebook (se realizó una calibración manual)

pars <- c(
  u = -5.47e-04,
  tmax = 17.79,
  r0 = 3.00359863e-02,
  gamma1 = 15.314955,
  gamma2 = 1.01,
  gamma3 = 3,
  
  k = 2.6,
  lam = 20,
  A= 1.11) 
#Tablas con datos reales Modelo

data_real <- read.csv("datos_agrupados_por _semana2021.csv")

# Eliminar las 2 primeras filas de los datos porque arrancamos en la 3ra fila.

df_out_real <- data_real[-c(1, 2), ]

times <- seq(6, 52, by = 1) # Vector de tiempos reales (semanas)



Data_m_real<-cbind(time=times,m=df_out_real$FERTILES,sd_m = sd(df_out_real$FERTILES))
Data_n_real<-cbind(time=times,n=df_out_real$ESTERILES,sd_n = sd(df_out_real$ESTERILES))


# Resolvemos el modelo con los parametros iniciales (Modelo_BW.R contiene el modelo a usar)

source("Modelo_BW.R")
times <- seq(6, 52, by = 1) # Vector de tiempos reales (semanas)
state<- c(m = 130,n = 349) # Condiciones iniciales
out_real <- ode(y = state, parms = pars, times = times, func = Modelo_BW)
ini<-out_real
plot(ini)    

#Cálculo del costo del modelo. (ModeloBW_cost.R calcula el costo)

source("Modelo_BW_cost.R")
Modelo_BW_cost(pars)$model

#################
# Precalibracion
#################

# Análisis de Sensitividad Local

Sfun <- sensFun(Modelo_BW_cost, pars)
summary(Sfun)
plot(Sfun,which=c("m"),xlab="time",lwd =2,legpos="topleft")
plot(Sfun,which=c("n"),xlab="time",lwd =2,legpos="topleft")

##Descargar cuadro de del análisis de sensitividad

Analisis_sensitividad<- as.data.frame(summary(Sfun))
write.csv(Analisis_sensitividad, file = "Analisis_sensitividad.csv", row.names = TRUE)

# Análisis de multicolinealidad

ident <- collin(Sfun)
View(ident) # Visualiza el dataframe con los indices de multicolinealidad y permite elegir, junto con el analisis de sensitividad que parametros calibrar

##Descargar cuadro del análisis de multicolinealidad

Analisis_multicolinealidad<- as.data.frame(ident)
write.csv(Analisis_multicolinealidad, file = "Analisis_multicolinealidad.csv", row.names = TRUE)

#Cálculo de la matriz de correlaciones

pairs(Sfun, which = c("m","n"), col = c("lightblue", "orange"))

##########################
# Calibracion del modelo
##########################

source("Modelo_BW_cost2.R")
Pars <- pars[c(2,3,6,7,8)] # Selecciona 5 parametros variables en el ajuste
Fit <- modFit(f = Modelo_BW_cost2, p = log(Pars)) # Realiza la calibracion con parametros logaritmados

summary(Fit)      # Brinda un resumen del ajuste (incluye el p-valor de los parametros optimales logaritmados)

c(exp(coef(Fit)),u=-5.47e-04,gamma1 = 15.314955, gamma2 = 1.01, A= 1.11)    # Brinda los valores de los parametros optimales. 

#########################################################################################################
# Grafica para comparativa entre los datos reales, el modelo ajustado inicial y el modelo calibrado final
#########################################################################################################

ini_ <- as.data.frame(ini)  # ini calculado en la seción inicial
final  <- ode(y = state, parms = c(exp(coef(Fit)),u=-5.47e-04,gamma1 = 15.314955, gamma2 = 1.01, A= 1.11), times = times, func = Modelo_BW)
final_ <- as.data.frame(final)
par(mfrow = c(1,2))
plot(Data_m_real, xlab = "t (semanas)", ylab = "fétiles",type = "b", pch = 16, col = "lightblue", lty = 2)
lines(times, ini_$m, lty = 2)
lines(times, final_$m, lty=1)
legend("topleft",c("datos", "inicial", "ajustada"), lty = c(NA,2,1), pch = c(1, NA, NA), inset = c(-0.1, -0.499), xpd = TRUE)
plot(Data_n_real, xlab = "t (semanas)", ylab = "estériles",type = "b", pch = 16, col = "orange", lty = 2)
lines(times, ini_$n, lty = 2)
lines(times, final_$n,lty=1)
par(mfrow = c(1, 1))

