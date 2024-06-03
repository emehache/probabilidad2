
K <- 12
k <- seq(-K, K, 1)
n <- seq(-K, K, .5)

f <- function(x) 2*cos(2*pi*x) + 2*x^2 + 3
fk <- Vectorize(function(x) max(k[which(k <= f(x))]))
fn <- Vectorize(function(x) max(n[which(n <= f(x))]))

curve(f, 0, 2, axes = F, ylim = c(-3,K))
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
curve(fk, add = T, col = 2, n = 1001)
curve(fk, add = T, col = 2, n = 1001, type = "h")
curve(fn, add = T, col = 4, n = 1001)
curve(fn, add = T, col = 4, n = 1001, type = "h")
