
set.seed(1234)

eps <- .05
# n <- 5e3
n <- 500
m <- 5e4
p <- .5
res <- replicate(m, {
  X <- sample(0:1, n, prob = c(1-p, p), rep = T)
  (cumsum(X)/(1:n)) - p
})

matplot(1:n, res, type = "l")
abline(h = 0, col = "grey20", lwd = 3)
abline(h = c(-eps,eps), col = "grey20", lwd = 3, lty = 2)


# -------------------------------------------------------------------------

N <- 200:500

apply(res, 1, \(x) mean(abs(x)> eps)) %>% 
  .[N] %>% 
  plot(x = N, type = "o", pch = 20)
curve(p*(1-p)/x/eps^2, add =T, col = 4, lwd = 2)
curve(1/x, add = T, col = 2, lwd = 2, n = 1001)

hmas <- function(eps) (p+eps)*log((p+eps)/p)+(1-p-eps)*log((1-p-eps)/(1-p))
bernstein <- function(n, eps) exp(-n*hmas(eps)) + exp(-n*hmas(-eps))
exp2 <- function(n, eps) exp(-n^2*hmas(eps)) + exp(-n^2*hmas(-eps))
curve(bernstein(x,eps), add = T, col = 4, lwd = 2, n = 1001)
curve(exp2(x,eps), add = T, col = 4, lwd = 2, n = 1001)
# -------------------------------------------------------------------------


