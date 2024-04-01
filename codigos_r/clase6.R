

# -------------------------------------------------------------------------

set.seed(1234)
n <- 5e3
m <- 50
p <- .5
res <- replicate(m, {
  X <- sample(0:1, n, prob = c(1-p, p), rep = T)
  (cumsum(X)/(1:n))
})

matplot(1:n, res, type = "l")
abline(h = p, col = "grey20", lwd = 2)

# -------------------------------------------------------------------------

set.seed(1234)
p <- seq(.1,.9,.1)
m <- 50
res <- sapply(p, \(pp) {
  X <- sample(0:1, n, prob = c(1-pp, pp), rep = T)
  (cumsum(X)/(1:n))
})

matplot(x = p, t(res[30:1e3,]), type = "l", asp = 1)

# -------------------------------------------------------------------------
library(magrittr)

sortear_matriz <- function(n, k, prob){
  x1 <- rbinom(n = n*(n-1)/2, size = 1, prob = .5)
  x2 <- rbinom(n = k*(k-1)/2, size = 1, prob = prob)
  
  M = matrix(0, n, n)
  M[upper.tri(M, diag = FALSE)] <- x1
  M[1:k, 1:k][upper.tri(M[1:k, 1:k], diag = FALSE)] <- x2
  M <- M + t(M)
  return(M)
}

n <- 10000
# k <- 300
k <- 4*sqrt(n*log(n)) %>% ceiling

X2 <- rbinom(n-k, size = n-1, prob = .5)
# X1 <- rbinom()
hist(X2, "FD")

A <- sortear_matriz(n, k, prob = 1)
rowSums(A) %>% hist("FD")


# -------------------------------------------------------------------------


image(A, x = 1:n, y = 1:n, asp = 1)


