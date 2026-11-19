## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(fig.align = "center", 
               out.width = "90%",
               fig.width = 6, 
               fig.height = 5.5,
               dev.args = list(pointsize=10),
               par = TRUE, # needed for setting hook 
               collapse = TRUE, # collapse input & ouput code in chunks
               cache = FALSE,
               warning = FALSE, message = FALSE)
knit_hooks$set(par = function(before, options, envir)
  { if(before && options$fig.show != "none") 
       par(mar=c(4.1,4.1,1.1,1.1), mgp=c(3,1,0), tcl=-0.5)
})
if(interactive()) setwd("vignettes")

## ----message = FALSE, echo=1--------------------------------------------------
library(mixggm)
cat(mixggm:::mixggmStartupMessage(), sep="")

## -----------------------------------------------------------------------------
data(iris)
mod = mixGGM(iris[,-5], model = "concentration")
summary(mod)
summary(mod, graphs = TRUE, clusters = TRUE, parameters = TRUE)

## ----fig.width=8, fig.height=4------------------------------------------------
plot(mod, what = "graph")

## ----fig.width=8, fig.height=4, out.width="80%"-------------------------------
plot(mod, what = "adjacency")

## -----------------------------------------------------------------------------
plot(mod, what = "classification")

## -----------------------------------------------------------------------------
plot(mod, what = "common")

## ----echo=-1------------------------------------------------------------------
set.seed(20190105)
N = 500
V = 20
tau = c(0.3, 0.7)
Nk = rowSums( rmultinom(N, 1, tau) )
class = rep(1:2, Nk)
sigma1 = rWishart(1, V+1, diag(V))[,,1]
mu1 = rep(0, V)
mu2 = rnorm(V, 0.5, 2)
x1 = MASS::mvrnorm(Nk[1], mu1, sigma1)
x2 = matrix(NA, Nk[2], V)
x2[,1] = rnorm(Nk[2])
for(j in 2:V) 
  x2[,j] = x2[,j-1] + rnorm(Nk[2], mu2[j], sd = 0.5)
x = rbind(x1, x2)
colnames(x) = paste0("x", 1:ncol(x))

## ----echo=1:2, eval=FALSE-----------------------------------------------------
# mod = mixGGM(x, K = 1:4, model = "concentration",
#              penalty = "ebic", beta = 0.5)
# if(!file.exists("simcomplex.Rdata"))
#    save(mod, file = "simcomplex.Rdata")

## ----echo=-1------------------------------------------------------------------
load(file = "simcomplex.Rdata")
summary(mod)

## -----------------------------------------------------------------------------
plot(mod, what = "classification", dimens = c(1,5,10,15,20))

## ----fig.width=8, fig.height=4------------------------------------------------
plot(mod, what = "graph")

## ----fig.width=8, fig.height=4, out.width="80%"-------------------------------
plot(mod, what = "adjacency")
table(class, mod$classification)

## -----------------------------------------------------------------------------
data(wine, package = "gclus")

## ----echo=1:2, eval=FALSE-----------------------------------------------------
# mod = mixGGM(wine[,-1], K = 1:4, model = "covariance",
#              penalty = "erdos", beta = 0.01)
# if(!file.exists("wine.Rdata"))
#   save(mod, file = "wine.Rdata")

## ----echo=-1------------------------------------------------------------------
load(file = "wine.Rdata")
summary(mod, graphs = TRUE)

## -----------------------------------------------------------------------------
plot(mod, what = "graph")

## ----fig.width=8, fig.height=4, out.width="80%"-------------------------------
plot(mod, what = "adjacency")

## -----------------------------------------------------------------------------
plot(mod, what = "classification", dimens = 1:4)
table(wine$Class, mod$classification)
mclust::adjustedRandIndex(wine$Class, mod$classification)

## ----echo=-1------------------------------------------------------------------
set.seed(20190105)
N = 200
tau = c(0.3, 0.7)
Nk = rowSums( rmultinom(N, 1, tau) )
class = rep(1:2, Nk)
sigma1 = diag(2)                          # independent variables
sigma2 = matrix( c(1,0.9,0.9,1), 2,2 )    # correlated variables
mu1 = c(0, 0)
mu2 = c(2, 3)
x = rbind(MASS::mvrnorm(Nk[1], mu1, sigma1),
           MASS::mvrnorm(Nk[2], mu2, sigma2))
colnames(x) = paste0("x", 1:ncol(x))
mod = mixGGM(x, model = "covariance")
summary(mod, graphs = TRUE, parameters = TRUE)

## ----fig.width=8, fig.height=4, out.width="80%"-------------------------------
plot(mod, what = "graph")

## ----fig.width=8, fig.height=4, out.width="80%"-------------------------------
plot(mod, what = "adjacency")

## ----out.width="80%"----------------------------------------------------------
plot(mod, what = "classification")

## ----echo=FALSE, results="asis"-----------------------------------------------
cat(format(citation("mixggm"), style = "md"), sep = "\n\n")

## -----------------------------------------------------------------------------
devtools::session_info()

