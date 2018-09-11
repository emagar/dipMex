library("arm")
library ("MCMCpack")
library (foreign)
library (car)
library (gtools)

library (R2WinBUGS)
#library (R2OpenBUGS)
#library(BRugs)

rm(list = ls())
##
workdir <- c("~/Dropbox/data/rollcall/DipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/DipMex")
setwd(workdir)

load("ip60_5+5k.RData")

results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )

post.draw <- rbind(chains[[1]], chains[[2]], chains[[3]])
post.x     <- post.draw[,grep("x", colnames(post.draw))]
post.y     <- post.draw[,grep("y", colnames(post.draw))]
post.alpha <- post.draw[,grep("alpha", colnames(post.draw))]
post.beta  <- post.draw[,grep("beta", colnames(post.draw))]
post.delta <- post.draw[,grep("delta", colnames(post.draw))]

## SACA CONSTANTE Y PENDIENTE DE CUTLINES
a <- post.beta / post.delta  ## pendiente de cutline
b <- post.alpha / post.delta ## constante de cutline

## MATRIZ CON MEDIANA E INTERVALOS DE CREDIBILIDAD BAYESIANA
jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }

## MIN y MAX (PUEDE SER UTIL)
tmp <- c(jotas[,1],jotas[,4])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
min <- min( tmp )
tmp <- c(jotas[,3],jotas[,6])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
max <- max( tmp )
lims <- c(NA,NA)
lims[1] <- ifelse(abs(min)>max, min, -max)
lims[2] <- ifelse(abs(min)>max, abs(min), max)

par(mar = c(3.1, 3.1, 2.1, 2.1) )
## dipdat$color[dipdat$color=="."] <- "black"
##plot(c(min(jotas[,2]),max(jotas[,2])), c(min(jotas[,5]),max(jotas[,5])), type="n")
plot(c(-3.5,3), c(-3.5,3), type="n", 
           xlab="", 
           ylab="", 
           main="60th Leg. 2006-09")
##abline(0,1)
abline(v=seq(-3,3,by=1),col="grey",lty=3); ## OPCIONAL
abline(h=seq(-3,3,by=1),col="grey",lty=3); ## OPCIONAL
points(jotas[,2],jotas[,5], pch=19, cex=.75, col=dipdat$color)
##points(jotas[,5],jotas.gac[,5], pch=19, cex=.75, col=dipdat$color)
legend(2.2,-1.6, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")

## cutlines
amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }
#
plot(c(-2.2,2.2), c(-2.2,2.2), type="n", 
           xlab=c(""), 
           ylab=c(""), 
           main="60th Leg. 2006-09")
    for (i in 1:I){
        abline(a=bmed[i], b=amed[i], col="grey") } ## OJO: a en mi modelo es slope, en R es constant
    for (j in 1:J){
        points(jotas[j,2],jotas[j,5],pch=20,col=dipdat$color[j])
        }


