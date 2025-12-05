#Modelo - Función de contiene el modelo 

Modelo<- function (times,state,pars) { #(revisar cond inic.)
  with (as.list(c(state, pars)), {
    dm <- (u * gamma3 * (times - tmax * gamma2)^2 + gamma1 * r0) * m / (m + n) * m
    dn <- (1/times) * ((k - 1) - k * (times / lam)^k) * n * A
    return(list(c(dm, dn)))
  })
  
}

