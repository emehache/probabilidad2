
# Sucesiones de funciones reales ----------------------------------------------------------------



graficar <- function(f, N = 100) {
  color <- colorRampPalette(c("grey", 'black'))(N)
  curve(f(x, 0), ylim =  c(0, 1),col = 'grey', type = 'n')
  invisible(sapply(1:N, function(n) {curve(f(x, n), add = T, col = color[n]) } ))
}

f <- function(x, n) x^n
graficar(f)

f <- function(x, n) n*x / (n*x + 1)
graficar(f)

f <- function(x, n) n*x/(n+x)
graficar(f)

# 20240311 ----------------------------------------------------------------
N <- 20
x <- .9

graficar <- function(f, N = 100) {
  color <- colorRampPalette(c("grey", "black"))(N)
  curve(f(x, 0), ylim = c(0, 1),col = "grey", type = "n")
  invisible(sapply(1:N, function(n) {
    Sys.sleep(.1)
    curve(f(x, n), add = T, col = color[n])
    } ))
}

f <- function(x, n) x^n; graficar(f, N = N)
sapply(1:N, \(n) {
  Sys.sleep(.1)
  points(x, x^n, pch = 20, col = 2, cex = .5)
  })
arrows(0,0,1,0, col = 4, lwd = 2, len = .1)
points(1,1,col = 4, pch = 20)

# -------------------------------------------------------------------------


f <- function(x, n) n*x / (n*x + 1); graficar(f)
f <- function(x, n) n*x/(n+x); graficar(f)

# convergencia puntual de la serie de fourier -----------------------------

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
  sapply(1:35, function(i) {
    Sys.sleep(.15)
    plot(x,y,"n",col=2,lwd=2,axes=F,ylim=c(-1,pi),xlim=c(-2.5,2.5))
    axis(1,pos=0)
    axis(2,pos=0)
    lines(x,y,col=2,lwd=2)
    curve(S(i,x),add=T,col=4)
    points(w,S(i,w),col=3,pch=20)
  })
)


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


# otro --------------------------------------------------------------------


m <- 20

M <- NULL
invisible(
  sapply(1:m,function(i) {a <- i
  sapply(1:i, function(x) {b <- x
  M <<- rbind(M,c(a,b))
  })
  }) )

u <- runif(1)

#png(file="gif/gif%03d.png", width=600, height=600)
invisible(
  sapply(1:nrow(M), function(i){
    inf <- (M[i,2]-1)/M[i,1]
    sup <- M[i,2]/M[i,1]
    graf( inf , sup )
    points(u,as.numeric(u<sup && u > inf),col=4,pch=20)
    value <- as.numeric(u<sup && u > inf)
    text(.8,.7,sprintf("X_%d = %s", i, value))
    Sys.sleep(.1)
    cat(sprintf("X_%03d = %s \n", i, value))
  } )
)
#dev.off()
#system("convert -delay 50 gif/*.png animacion.gif")


res <- invisible(sapply(1:nrow(M), function(i) {
  inf <- (M[i,2]-1)/M[i,1]
  sup <- M[i,2]/M[i,1]
  as.numeric(u<sup && u > inf)
}))

plot(res, pch = 20, type = 'o')


# ejemplo fourier  -------------------------------------------------------------------

