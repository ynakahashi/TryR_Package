library(GA)

f <- function(x) {
   (x^2 + x) * cos(x)
}

min <- -10
max <- 10
curve(f, min, max, n = 1000)

GA <- ga(type = "real-valued", fitness = f, min = min, max = max, monitor = F)
summary(GA)
plot(GA)
curve(f, min, max, n = 1000)
points(GA@solution, GA@fitnessValue, col = 2, pch = 19)



Rastrigin <- function(x1, x2) {
   20 + x1^2 + x2^2 - 10 * (cos(2 * pi * x1) + cos(2 * pi * x2))
}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20, color.palette = bl2gr.colors)

filled.contour(x1, x2, f, color.palette = bl2gr.colors)

GA <- ga(type = "real-valued", fitness = function(x) -Rastrigin(x[1], x[2]),
         min = rep(-5.12, 2), max = rep(5.12, 2),
         popSize = 50, maxiter = 1000, run = 100)
summary(GA)
plot(GA)
filled.contour(x1, x2, f, color.palette = bl2gr.colors,
               plot.axes = { axis(1); axis(2);
                  points(GA@solution[, 1], GA@solution[, 2],
                         pch = 3, cex = 2, col = "white", lwd = 2)})

GA <- ga(type = "real-valued", fitness = function(x) -Rastrigin(x[1], x[2]),
         min = rep(-5.12, 2), max = rep(5.12, 2),
         popSize = 50, maxiter = 1000, run = 100,
         optim = T)
summary(GA)
plot(GA)
filled.contour(x1, x2, f, color.palette = bl2gr.colors,
               plot.axes = { axis(1); axis(2);
                  points(GA@solution[, 1], GA@solution[, 2],
                         pch = 3, cex = 2, col = "white", lwd = 2)})


GA <- gaisl(type = "real-valued", fitness = function(x) -Rastrigin(x[1], x[2]),
            min = rep(-5.12, 2), max = rep(5.12, 2),
            popSize = 100, maxiter = 1000, run = 100,
            numIslands = 4, migrationRate = 0.2, migrationInterval = 50)
summary(GA)
plot(GA, log = "x")