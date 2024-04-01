
n <- 1000
X <- sample(x = c(-1,1), size = n, prob = c(19,18), replace = TRUE)
promedio <- cumsum(X)*(1/1:n)
plot(1:n, promedio, type = "p", pch = 20, cex = .5)
abline(h = -1/37, col = 2, lwd = 2)

mean(promedio == (-1/37))

plot(promedio + 1/37, pch = 20, cex = .5)
abline(h = 0, col = 2, lwd = 2)

plot(sqrt(1:n)*(promedio + 1/37), pch = 20, cex = .5)
abline(h = 0, col = 2, lwd = 2)

