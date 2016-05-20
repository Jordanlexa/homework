

library(deSolve)

Pzom.hum <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  S <- y[3]
  with(as.list(p), {
    dH.dt <- r * H*(1-H/k) - b * H * Z
    dZ.dt <- c * H * Z - m * Z -  d * Z * S
    dS.dt <- e * Z * S - n * S
    return(list(c(dH.dt, dZ.dt, dS.dt)))
  })
}

p <- c('r' = 1,
       'b' = 1,
       'c' = 1,
       'k' = 1,
       'm' = 0.1,
       'd' = 1,
       'e' = 1,
       'n' = 0.1)
y0 <- c('H' = 1, 'Z' = 0.1, 'S' = 0.1)
t <- 1:100

simP <- ode(y = y0, times = t, func = Pzom.hum, parms = p, method = 'lsoda')
simP <- as.data.frame(simP)

plot(H ~ time, data = simP, type = 'l', col = 'darkgreen', bty = 'l', lwd = 2, ylim = c(0,2))
points(Z ~ time, data = simP, type = 'l', col = 'purple', lty = 2, lwd = 2)
points(S ~ time, data = simP, type = 'l', col = 'red', lty = 2, lwd = 2)

