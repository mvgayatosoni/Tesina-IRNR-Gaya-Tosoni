## Validación predictiva

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

df_out_real1<- df_out_real[-c(1:5), ]

Data_m_real<-cbind(time=df_out_real1$Times,m=df_out_real1$FERTILES,sd_m = sd(df_out_real1$FERTILES))
Data_n_real<-cbind(time=df_out_real1$Times,n=df_out_real1$ESTERILES,sd_n = sd(df_out_real1$ESTERILES))

# Resolvemos el modelo con los parametros iniciales (ModeloBW.R contiene el modelo a usar)

source("Modelo.R")
times <- df_out_real1$Times # Vector de tiempos reales (semanas)
state<- c(m = 130,n = 349) # Condiciones iniciales
out_real <- ode(y = state, parms = pars, times = times, func = Modelo)
ini<-out_real


#########################################################################################################
# Grafica para comparativa entre los datos reales, el modelo ajustado inicial y el modelo calibrado final
#########################################################################################################

ini_<- as.data.frame(ini)  # ini lo calculamos al principio
par(mfrow = c(1,2))
plot(Data_m_real, xlab = "t (semanas)", ylab = "fértiles",type = "b", pch = 16, col = "lightblue", lty = 2)
lines(ini_$time, ini_$m, lty = 1)
legend("topleft", c("datos", "ajuste"), lty = c(NA,1), pch = c(1, NA, NA), inset = c(-0.1, -0.499), xpd = TRUE)
plot(Data_n_real, xlab = "t (semanas)", ylab = "estériles",type = "b", pch = 16, col = "orange", lty = 2)
lines(ini_$time, ini_$n, lty = 1)
par(mfrow = c(1, 1))

# ----------------------------
# Comparación estadística del ajuste
# ----------------------------

# Convertimos a data.frame si es necesario
ini_df <- as.data.frame(ini)

# Extraemos los datos del modelo y los reales
obs_m <- Data_m_real[, "m"]
sim_m <- ini_df$m

obs_n <- Data_n_real[, "n"]
sim_n <- ini_df$n

# Definimos funciones para las métricas
RMSE <- function(obs, sim) sqrt(mean((obs - sim)^2))
MAE  <- function(obs, sim) mean(abs(obs - sim))
R2   <- function(obs, sim) cor(obs, sim)^2
NSE  <- function(obs, sim) 1 - sum((obs - sim)^2) / sum((obs - mean(obs))^2)

# Calculamos las métricas para fértiles y estériles
stats <- data.frame(
  Variable = c("Fértiles (m)", "Estériles (n)"),
  RMSE = c(RMSE(obs_m, sim_m), RMSE(obs_n, sim_n)),
  MAE  = c(MAE(obs_m, sim_m), MAE(obs_n, sim_n)),
  R2   = c(R2(obs_m, sim_m), R2(obs_n, sim_n)),
  NSE  = c(NSE(obs_m, sim_m), NSE(obs_n, sim_n))
)

# Mostramos resultados
print("Comparación estadística del ajuste:")
print(stats, row.names = FALSE)

# ----------------------------
# Cálculo de errores relativos (%)
# ----------------------------

mean_obs_m <- mean(obs_m)
mean_obs_n <- mean(obs_n)

stats$RMSE_perc <- c(RMSE(obs_m, sim_m)/mean_obs_m*100,
                     RMSE(obs_n, sim_n)/mean_obs_n*100)

stats$MAE_perc <- c(MAE(obs_m, sim_m)/mean_obs_m*100,
                    MAE(obs_n, sim_n)/mean_obs_n*100)

# Mostramos los resultados ampliados
print("Comparación estadística con errores relativos (%):")
print(stats, row.names = FALSE)

# Conversión a csv de la salida estadística

Estadisticas<- as.data.frame(stats)
write.csv(Estadisticas, file = "estadisticas.csv", row.names = TRUE)

