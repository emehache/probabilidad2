
library(data.table)
library(magrittr)
library(stringi)
library(data.tree)
library(DiagrammeR)

# -------------------------------------------------------------------------

d <- 20
x <- expand.grid(rep(list(0:1), d))
omega <- apply(x, 1, stri_c, collapse = "")




# -------------------------------------------------------------------------

generate_tree <- function(node, depth) {
  if (depth == 0) {
    return(node)
  } else {
    for (outcome in c("S", "F")) {
      child <- node$AddChild(name = outcome)
      generate_tree(child, depth - 1)
    }
    return(node)
  }
}

root <- Node$new("Start")

root <- generate_tree(root, 5)

print(root, "level")
plot(root)


# -------------------------------------------------------------------------


d <- 20
x <- expand.grid(rep(list(0:1), d))
omega <- apply(x, 1, paste0, collapse = "")

# grepl(paste0(rep(c(".",1,"."), c(2,2,d-4)), collapse = ""), x = s)

res <- sapply(1:(d/2), function(i) {
  palabra <- paste0(rep(c(".",1,"."), times = c(2*(i-1),2,d-2*(i-1)-2)), collapse = "")
  !grepl(palabra, x = omega)
  # grepl(paste0(rep(c(".",1,"."), c(2*i,2,d-2*i-2)), collapse = ""), x = s)
})

colnames(res) <- paste0("A",1:(d/2))


# X <- sapply(2:d, \(i) stri_sub(omega, 1, i))
X <- sapply(1:(d/2), \(i) stri_sub(omega, 1, 2*i))


# -------------------------------------------------------------------------

load("/home/manuel/Dropbox/Documentos/Nextcloud/2024-Probabilidad_2/R/clase9.RData")

as.data.table(omega) %>% 
  cbind(res) %>%
  .[, W := !(omega %like% 11)] %>%
  .[]

as.data.table(omega) %>% 
  cbind(res) %>%
  .[, W := !(omega %like% 11)] %>% 
  View()
  


matrix(!(X %like% 11), ncol = d/2) %>% 
  colMeans %>% 
  plot(ylim = 0:1)
points(1:(d/2), .75^(1:(d/2)), pch = 20, col = 2)

# save.image("/home/manuel/Dropbox/Documentos/Nextcloud/2024-Probabilidad_2/R/clase9.RData")  

# matrix(!(X %like% 11), ncol = d-1) %>% 
#   colMeans %>% 
#   plot

# -------------------------------------------------------------------------


data.table(x) %>% 
  .[, W := !(s %like% 11)] %>% 
  .[, A1 := !(Var1==1 & Var2==1)] %>% 
  .[, A2 := !(Var3==1 & Var4==1)] %>% 
  .[]
  .[,-c(1:10)] %>% 
  apply(2, mean)


