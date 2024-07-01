

# -------------------------------------------------------------------------


m <- 1e4

N <- c(3, 7, 15, 50, 100, 1e3)

set.seed(12345)
sim <- sapply(N, \(n) {
  replicate(m, {
    # x <- runif(n, -1, 1)
    x <- rexp(n)
  })
})

res <- sapply(1:length(sim), \(i){
  # sqrt(N[i])*colMeans(sim[[i]])*sqrt(3)
  sqrt(N[i])*colMeans(sim[[i]] - 1)
})


par(mfrow = c(2,3), mai = c(.5,.5,.5,.5))
sapply(1:6, \(i) {
  curve(dnorm, col = 2, add = F, -3.5, 3.5, lwd = 2, main = sprintf("n = %s", N[i]), xlab = "", ylab = "")
  # hist(res[,i], freq = F, breaks = 20, add = T)
  lines(density(res[,i], bw = "bcv"), main = sprintf("n = %s", N[i]), xlab = "", ylab = "")
  curve(dnorm, col = 2, add = T, lwd = 2)
})


# -------------------------------------------------------------------------


  # tcl <- sapply(N, \(n) {
#   replicate(m, {
#     x <- runif(n, -1, 1)
#     sqrt(n)*mean(x)/3
#   })
# })




