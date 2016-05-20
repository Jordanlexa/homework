
library(deSolve)

zom.hum <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  with(as.list(p), {
    dH.dt <- r * H*(1-H/k) - b * H * Z
    dZ.dt <- c * H * Z - m * Z
    return(list(c(dH.dt, dZ.dt)))
  })
}

p <- c('r' = 1,
       'b' = 1,
       'c' = 1,
       'k' = 1,
       'm' = 0.1)
y0 <- c('H' = 1, 'Z' = 0.1)
t <- 1:100

sim <- ode(y = y0, times = t, func = zom.hum, parms = p, method = 'lsoda')
sim <- as.data.frame(sim)

plot(H ~ time, data = sim, type = 'l', col = 'darkgreen', bty = 'l', lwd = 2)
points(Z ~ time, data = sim, type = 'l', col = 'purple', lty = 2, lwd = 2)











