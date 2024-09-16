plot_sim_E <- function(sd) {
  E <- rnorm(1000, mean = 50, sd = sd)
  E <- density(E)
  x <- c(0, pmax(0, pmin(100, E$x)), 100, 0)
  y <- c(0,                   E$y  , 0  , 0)
  plot(x, y, type = "l")
}
