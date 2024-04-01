
# Sucesiones de funciones reales ----------------------------------------------------------------

# Ejemplo x^n

N <- 50
x <- .95

f <- function(x, n) x^n

color <- colorRampPalette(c("grey", "black"))(N)


par(mai = c(.5,.5,.1,.1))

curve(f(x, 0), ylim = c(0, 1), type = "n", axes = F)
axis(1, pos = 0)
axis(2, pos = 0)
invisible(sapply(1:N, function(n) {
  Sys.sleep(.1)
  curve(f(x, n), add = T, col = color[n])
} ))



points(x, 0, col = 2)

sapply(1:N, \(n) {
  Sys.sleep(.1)
  points(x, f(x,n), pch = 20, col = 2, cex = .5)
})

arrows(0,0,1,0, col = 4, lwd = 2, len = .1)
points(1,1,col = 4, pch = 20)


# -------------------------------------------------------------------------

eps <- .2
x0 <- .95

plot(0, 0, type="n", xlim = 0:1, ylim = c(-1,1))
abline(h = c(-eps, eps), col = 2, lty = 2)

n <- 5
curve(f(x, n), add = T)
points(x0, f(x0,n), pch = 20, col = 4)

n <- 6
curve(f(x, n), add = T)
points(x0, f(x0,n), pch = 20, col = 4)


(n <- ceiling(log(eps)/log(x0)))
curve(f(x, n), add = T)
points(x0, f(x0,n), pch = 20, col = 4)


# Ejemplo nx/(nx+1) -------------------------------------------------------

f <- function(x, n) n*x / (n*x + 1)

N <- 50
x <- .95

color <- colorRampPalette(c("grey", "black"))(N)


par(mai = c(.5,.5,.1,.1))

curve(f(x, 0), ylim = c(0, 1), type = "n", axes = F)
axis(1, pos = 0)
axis(2, pos = 0)
invisible(sapply(1:N, function(n) {
  # Sys.sleep(.1)
  curve(f(x, n), add = T, col = color[n])
} ))



points(x, 0, col = 2)

sapply(1:N, \(n) {
  Sys.sleep(.1)
  points(x, f(x,n), pch = 20, col = 2, cex = .5)
})



# Ejemplo nx/(n+x) --------------------------------------------------------


f <- function(x, n) n*x/(n+x)

N <- 500
x <- .95

color <- colorRampPalette(c("grey", "black"))(N)


curve(f(x, 0), ylim = c(0, 1), type = "n", axes = F)
axis(1, pos = 0)
axis(2, pos = 0)
invisible(sapply(1:N, function(n) {
  # Sys.sleep(.1)
  curve(f(x, n), add = T, col = color[n])
} ))


# -------------------------------------------------------------------------



points(x, 0, col = 2)

sapply(1:N, \(n) {
  Sys.sleep(.1)
  points(x, f(x,n), pch = 20, col = 2, cex = .5)
})



# convergencia puntual de la serie de fourier -----------------------------
N <- 35
set.seed(12345)
w <- runif(1,-2,2)

Sx<-function(n,x){
  M<-0
  for (i in 1:n) M[i]<- ((-1)^i-1)/pi/i^2*cos(i*x)+(-1)^(i+1)/i*sin(i*x)
  s<-pi/4+sum(M)
  return(s)
}

S=Vectorize(Sx,vectorize.args = "x")

x<-seq(-pi,pi,0.01)
y<-c(0*x[x<0],x[x>=0])


# X11()
invisible(
  sapply(1:N, function(i) {
    Sys.sleep(.1)
    plot(x,y,"n",col=2,lwd=2,axes=F,ylim=c(-1,pi),xlim=c(-2.5,2.5))
    axis(1,pos=0)
    axis(2,pos=0)
    lines(x,y,col=2,lwd=2)
    curve(S(i,x),add=T,col=4)
    points(w,S(i,w),col=3,pch=20)
  })
)
lines(x,y,col=2,lwd=2)


# visto con zoom ----------------------------------------------------------

invisible(
  sapply(1:50, function(i) {
    Sys.sleep(.15)
    plot(x,y,"n",col=2,lwd=2,axes=F, ylim = c(w-.2, w +.2) ,xlim=c(w-.1, w + .1))
    axis(1, pos = w - .2)
    axis(2, pos = w - .1)
    lines(x,y,col=2,lwd=2)
    curve(S(i,x),add=T,col=4)
    segments(w,w,w,S(i,w), lty = 2, col = 3)
    points(w,S(i,w),col=3,pch=20)
  })
)
lines(x,y,col=2,lwd=2)


# fourier 2 ---------------------------------------------------------------


L <- 1
N <- 10
fn <- function(x, N) {n <- 0:N; 4/pi * sum(sin((2*n+1)*pi*x/L)/(2*n+1))}
# fn <- function(x, N) {n <- 1:N; 4/pi * sum(sin(n*pi*x/L)/n)}
fn <- Vectorize(fn, vectorize.args = "x")

curve(fn(x, 10), type = "n", 0, 2)
segments(0, 1, L, 1, col = 2, lwd = 2)
segments(L, -1, 2*L, -1, col = 2, lwd = 2)
curve(fn(x, 10), n = 301, add = T, col = 4, lwd = 1.5)


curve(fn(x, 1), 0, 2, type = "n", axes = F)
axis(1, pos = 0)
axis(2, pos = 0)
invisible(sapply(1:N, function(n) {
  curve(fn(x, n), add = T, col = "grey50", n = 201)
} ))

# -------------------------------------------------------------------------

n <- 200
curve(f0, type = "n", axes = F, ylim = c(-.2, 1.2))
axis(1,pos=0)
axis(2,pos=0)
segments(0,0,.5,0, col = 2, lwd = 2)
segments(.5,1,1,1, col = 2, lwd = 2)
curve(fourier(f, n)(x), add = T, col = 4, lwd = 1.5, n = 10001)    

# -------------------------------------------------------------------------

graf <- function(a,b) {
  curve(x*0+1,a,b,xlim=c(0,1),lwd=2,col=2,ylim=c(0,1),axes=F)
  axis(1,pos=0)
  axis(2,pos=0)
  lines(c(a,a),c(0,1),lty=2)
  lines(c(b,b),c(0,1),lty=2)
  if (b != 1)  curve(x*0, b, 1, add = T, col = 2, lwd = 2)
  if (a != 0)  curve(x*0, 0, a, add = T, col = 2, lwd = 2)
}

invisible(
  sapply(1:30,function(i) {
    Sys.sleep(.1)
    graf(0,1/i)
  })
)

