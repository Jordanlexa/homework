library(deSolve)
log.growth <- function(t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1 - (N / K)^theta)
    return(list(dN.dt))
  })
}

pt <- c('r' = 0.2, 'K' = 1.05, 'theta' = 1.05)
y0t <- c('N' = 0.01)
tt <- 1:100

simt <- ode(y = y0t, times = tt, func = log.growth, parms = pt, method = 'lsoda')

simt <- as.data.frame(sim)

plot(N ~ time, data = simt, type = 'l', lwd = 2, bty = 'l', col = 'red')

pg <- c('r' = 0.28, 'K' = .75, 'theta' = 1)
y0g <- c('N' = 0.01)
tg <- 1:100

simg <- ode(y = y0g, times = tg, func = log.growth, parms = pg, method = 'lsoda')

points(N ~ time, data = simg, type = 'l', lwd = 2, bty = 'l', lty = 2, col = 'green')

pp <- c('r' = 0.15, 'K' = 1, 'theta' = 1)
y0p <- c('N' = 0.01)
tp <- 1:100

simp <- ode(y = y0p, times = tp, func = log.growth, parms = pp, method = 'lsoda')

points(N ~ time, data = simp, type = 'l', lwd = 2, bty = 'l', lty = 2, col = 'yellow')

