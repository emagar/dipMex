## USE AFTER RUNNING DIPBUGS61

library("arm")
library ("MCMCpack")
library (foreign)
library (car)
library (gtools)

rm(list = ls())
##
#workdir <- c("~/Dropbox/data/rollcall/DipMex")
workdir <- c("d:/01/Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/dipMex")
setwd(workdir)
##
set.seed(1970)

load(paste(workdir, "/ip60_5+5k_gaceta.RData", sep=""))
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )
results_gaceta <- results; chains_gaceta <- chains; votdat_gaceta <- votdat; dipdat_gaceta <- dipdat; I_gaceta <- I;
load(paste(workdir, "/ip60_5+5k.RData", sep=""))
#results <- ip2Dj
#load(paste(workdir, "/ip61_2DpjSimultaneos.RData", sep=""))

results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )

party <- rep(NA, times=J)
party <- ifelse(dipdat$part=="pan", 1,
          ifelse(dipdat$part=="pri", 2,
           ifelse(dipdat$part=="prd", 3,
            ifelse(dipdat$part=="pt", 4,
             ifelse(dipdat$part=="pvem", 5,
              ifelse(dipdat$part=="conve", 6,
               ifelse(dipdat$part=="panal", 7, 
                ifelse(dipdat$part=="psd", 8, NA ))))))))

## PLOT CHAINS TO CHECK CONVERGENCE
tmp <- dimnames(results[[1]]$sims.matrix); tmp <- tmp[[2]]
cplt <- function(X)
    {
    tmp2 <- min(as.numeric(chains[[1]][,X]), as.numeric(chains[[2]][,X]), as.numeric(chains[[3]][,X]));
#    tmp2 <- min(as.numeric(results$mcmc[[1]][,X]), as.numeric(results[[2]][,X]), as.numeric(results[[3]][,X]));
    tmp3 <- max(as.numeric(chains[[1]][,X]), as.numeric(chains[[2]][,X]), as.numeric(chains[[3]][,X]));
    plot(c(1,results[[1]]$n.sims), c(tmp2,tmp3), type="n", main=tmp[X]);
    lines(1:(results[[1]]$n.sims), as.numeric(chains[[1]][,X]),col="red");
    lines(1:(results[[1]]$n.sims), as.numeric(chains[[2]][,X]),col="blue");
    lines(1:(results[[1]]$n.sims), as.numeric(chains[[3]][,X]),col="green");
    }
##
setwd(paste(workdir, "/graphs/convergence", sep=""))
for (i in 1:length(chains[[1]][1,]))
    {
    pdf( paste("tmp", i, ".pdf", sep=""))
    cplt(i)
    dev.off()
    }
setwd(workdir)

post.draw <- rbind(chains[[1]], chains[[2]], chains[[3]])
post.x         <- post.draw[,grep("x", colnames(post.draw))]
post.y         <- post.draw[,grep("y", colnames(post.draw))]
post.alpha     <- post.draw[,grep("alpha", colnames(post.draw))]
post.beta      <- post.draw[,grep("beta", colnames(post.draw))]
post.delta     <- post.draw[,grep("delta", colnames(post.draw))]
post.mu.x      <- post.draw[,grep("mu.x", colnames(post.draw))]
post.sigma.x   <- post.draw[,grep("sigma.x", colnames(post.draw))]
post.b0        <- post.draw[,grep("b0", colnames(post.draw))]
post.b1        <- post.draw[,grep("b1", colnames(post.draw))]
post.mu.p      <- post.draw[,grep("mu.p", colnames(post.draw))]
post.sigma.loc <- post.draw[,grep("sigma.loc", colnames(post.draw))]
## OPTIONAL: MAGNIFY SCALE
#post.x <- post.x*10; post.y <- post.y*10

post.draw_gaceta <- rbind(chains_gaceta[[1]], chains_gaceta[[2]], chains_gaceta[[3]])
post.x_g         <- post.draw_gaceta[,grep("x", colnames(post.draw_gaceta))]
post.y_g         <- post.draw_gaceta[,grep("y", colnames(post.draw_gaceta))]
post.alpha_g     <- post.draw_gaceta[,grep("alpha", colnames(post.draw_gaceta))]
post.beta_g      <- post.draw_gaceta[,grep("beta", colnames(post.draw_gaceta))]
post.delta_g     <- post.draw_gaceta[,grep("delta", colnames(post.draw_gaceta))]

## PLOT PARTY MUs DESCRIPTIVES
require(grDevices)
tmp<-rep(NA,P)
qu <- data.frame(pty=part.list,q025=tmp,q25=tmp,q50=tmp,q75=tmp,q975=tmp)
for (p in 1:P){
    qu[p,2] <- quantile (post.mu.p[,p], 0.025, names=F)
    qu[p,3] <- quantile (post.mu.p[,p], 0.25, names=F)
    qu[p,4] <- quantile (post.mu.p[,p], 0.5, names=F)
    qu[p,5] <- quantile (post.mu.p[,p], 0.75, names=F)
    qu[p,6] <- quantile (post.mu.p[,p], 0.975, names=F)
              }
tmp<-order(qu$q50) ## sort parties by median mu
plot(c(-2.5,2.5), c(1,7.25), type="n",
           xlab="Left-Right", 
           ylab="", 
           axes=FALSE,
           main="Party positions, 2009 elite survey")
axis(1, at=-2:2)
#axis(2, at=1:7, labels=part.list[tmp], las=1, lty=0)
for (p in 1:7){
    points(qu$q50[tmp[p]],p, pch=19)
    lines(c(qu$q025[tmp[p]],qu$q975[tmp[p]]), c(p,p))
    lines(c(qu$q25[tmp[p]],qu$q75[tmp[p]]), c(p,p), lwd=2)
    text(qu$q50[tmp[p]],p, labels=part.list[tmp][p], pos=3)
    }

## RESOLVES NON-IDENTIFICATION OF DIM2 AS JACKMAN 2001 DOES
ch.means <- data.frame(ch1=rep(NA,J), ch2=rep(NA,J), ch3=rep(NA,J))
choice <- post.x ## CHOOSE CHAIN TO FIX
for (j in 1:J){
    ch.means[j,1] <- mean(choice[1:100,j])
    ch.means[j,2] <- mean(choice[101:200,j])
    ch.means[j,3] <- mean(choice[201:300,j])
    }
ch.means <- ch.means[order(party),]; tmp <- party[order(party)] ## SORT BY PARTY (CH.MEANS ONLY!!!)
plot(c(1,J), c(min(ch.means), max(ch.means)), type="n")
#points(c(1:J),ch.means$ch1, pch=19, cex=.5, col="red")
#points(c(1:J),ch.means$ch2, pch=19, cex=.5, col="blue")
#points(c(1:J),ch.means$ch3, pch=19, cex=.5, col="green")
##points(c(1:J)[party==3],ch.means$ch1[party==3], pch=19, cex=.5, col="red")
points(c(1:J)[party==1],ch.means$ch2[party==1], pch=19, cex=.5, col="blue")
points(c(1:J)[party==1],ch.means$ch3[party==1], pch=19, cex=.5, col="green")
##
post.y[101:300,] <- -post.y[101:300,] ## FLIPS CHAINS 2 AND 3
chains[[2]][,grep("y", colnames(chains[[2]]))] <- post.y[101:200,] ## REPLACES THIS IN CHAINS
chains[[3]][,grep("y", colnames(chains[[3]]))] <- post.y[201:300,]
## FALTARIA CORREGIR LOS PARAMETROS DE ITEMS

## STANDARDIZE X SCALE
##post.x <- (post.x - mean(as.vector(post.x)))/sd(as.vector(post.x))
post.x <- (post.x - mean(post.x))*10/sd(as.vector(post.y)) 
chains[[1]][,grep("x", colnames(chains[[1]]))] <- post.x[1:100,] ## REPLACES THIS IN CHAINS
chains[[2]][,grep("x", colnames(chains[[2]]))] <- post.x[101:200,] 
chains[[3]][,grep("x", colnames(chains[[3]]))] <- post.x[201:300,] 

## STANDARDIZE Y SCALE
##post.y <- (post.y - mean(as.vector(post.y)))/sd(as.vector(post.y))
post.y <- (post.y - mean(post.y))*10/sd(as.vector(post.y)) 
chains[[1]][,grep("y", colnames(chains[[1]]))] <- post.y[1:100,] ## REPLACES THIS IN CHAINS
chains[[2]][,grep("y", colnames(chains[[2]]))] <- post.y[101:200,] 
chains[[3]][,grep("y", colnames(chains[[3]]))] <- post.y[201:300,] 

## SACA CONSTANTE Y PENDIENTE DE CUTLINES
post.a <- post.beta / post.delta  ## pendiente de cutline
post.b <- post.alpha / post.delta ## constante de cutline
post.d <- -post.delta                  ## término discriminante

## MATRIZ CON INTERVALOS DE CONFIANZA BAYESIANOS DE PUNTOS IDEALES
jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }

jotas_g <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas_g[j,1] <- quantile (post.x_g[,j], 0.025, names=F)
    jotas_g[j,2] <- quantile (post.x_g[,j], 0.50, names=F)
    jotas_g[j,3] <- quantile (post.x_g[,j], 0.975, names=F)
    jotas_g[j,4] <- quantile (post.y_g[,j], 0.025, names=F)
    jotas_g[j,5] <- quantile (post.y_g[,j], 0.50, names=F)
    jotas_g[j,6] <- quantile (post.y_g[,j], 0.975, names=F)
    }

## ESTO HABÍA GENERADO PROBLEMAS EN 60 LEGIS...
jotas <- jotas[order(dipdat$id),]
jotas_g <- jotas_g[order(dipdat_gaceta$id),]
dipdat <- dipdat[order(dipdat$id),]
dipdat_gaceta <- dipdat_gaceta[order(dipdat_gaceta$id),]

## GRAFICA PUNTOS IDEALES
## dipdat$color[dipdat$color=="."] <- "black"
##plot(c(min(jotas[,2]),max(jotas[,2])), c(min(jotas[,5]),max(jotas[,5])), type="n")
#par(mai=c(.4, .4, .4, .4)) ## SETS B L U R MARGIN SIZES
#plot(c(min(jotas[,2]),max(jotas[,2])), c(min(jotas[,5]),max(jotas[,5])), type="n", 
plot(c(-2.2,2.2),c(-2.2,2.2), type="n", 
#plot(c(-4,4),c(-4,4), type="n", 
           xlab="", 
           ylab="", 
           xaxt="n",
           yaxt="n",
#           xlab=c("dim2_all"), 
#           ylab=c("dim2_gaceta"), 
#           main="60th Leg. SMDs")
           main="60th Leg. 2006-09")
#           main="Dimension 2")
#           main="")
axis(1,at=c(-2,-1,0,1,2))
axis(2,at=c(-2,-1,0,1,2))
#abline(0,1)
points(jotas[,2],jotas[,5], pch=19, cex=.75, col=dipdat$color)
#points(jotas[dipdat$dsm==1,2],jotas[dipdat$dsm==1,5], pch=19, cex=.75, col=dipdat$color[dipdat$dsm==1])
#points(jotas[,2],jotas[,5], col=dipdat$dcoord); ## pone coordinadores
legend(1.3,2.25, legend=part.list, cex=1, pch=20, pt.cex=1.25, col=color.list, bg="white")
#points(jotas[,5],jotas_g[,5], pch=19, cex=.75, col=dipdat$color)

## CON CUTLINES
amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }
#
plot(c(-3,2), c(-2.5,2.5), type="n", 
           xlab=c(""), 
           ylab=c(""), 
           main="61st Leg. 2009-11")
    for (i in 1:I){
        abline(a=bmed[i], b=amed[i], col="grey") } ## OJO: a en mi modelo es slope, en R es constant
    for (j in 1:J){
        points(jotas[j,2],jotas[j,5],pch=20,col=dipdat$color[j])
        }

## 45-DEGREE CLOCKWISE ROTATION OF COORDINATES IF NEEDED
xR <- post.x*cos(pi/4) + post.y*sin(pi/4)
yR <- -post.x*sin(pi/4) + post.y*cos(pi/4)
post.x <- xR; post.y <- yR;
## 45-DEGREE CLOCKWISE ROTATION OF CUTLINES
xA <- -b/a; yA <- rep(0, length(a)); xO <- rep(0, length(a)); yO <- b  ## coords de Abscisa al origen y Ordenada al origan de c/cutline
xAR <- xA*cos(pi/4) + yA*sin(pi/4)
yAR <- -xA*sin(pi/4) + yA*cos(pi/4)
xOR <- xO*cos(pi/4) + yO*sin(pi/4)
yOR <- -xO*sin(pi/4) + yO*cos(pi/4)
X <- xAR; Y <- yAR; XX <- xOR; YY <- yOR ## simplifica notación
aR <- (YY-Y)/(XX-X)           ## pendiente del cutline rotado
bR <- YY -((YY-Y)/(XX-X))*XX  ## constante del cutline rotado
rm(X,Y,XX,YY)
a <- aR; b <- bR

## PARTY DIM-BY-DIM MEDIANS
sample.size <- dim(post.x)[1]
tmpx <- data.frame(pan=rep(NA,sample.size), 
                     pri=rep(NA,sample.size), 
                     prd=rep(NA,sample.size), 
                     pt=rep(NA,sample.size), 
                     pvem=rep(NA,sample.size), 
                     conve=rep(NA,sample.size), 
                     panal=rep(NA,sample.size))
tmpy <- tmpx
for (i in 1:sample.size){
    tmpx[i,1] <- quantile (post.x[i,dipdat$part=="pan"], 0.50, names=F);
    tmpx[i,2] <- quantile (post.x[i,dipdat$part=="pri"], 0.50, names=F);
    tmpx[i,3] <- quantile (post.x[i,dipdat$part=="prd"], 0.50, names=F);
    tmpx[i,4] <- quantile (post.x[i,dipdat$part=="pt"], 0.50, names=F);
    tmpx[i,5] <- quantile (post.x[i,dipdat$part=="pvem"], 0.50, names=F);
    tmpx[i,6] <- quantile (post.x[i,dipdat$part=="conve"], 0.50, names=F);
    tmpx[i,7] <- quantile (post.x[i,dipdat$part=="panal"], 0.50, names=F);
    tmpx[i,8] <- quantile (post.x[i,dipdat$part=="psd"], 0.50, names=F);
    tmpy[i,1] <- quantile (post.y[i,dipdat$part=="pan"], 0.50, names=F);
    tmpy[i,2] <- quantile (post.y[i,dipdat$part=="pri"], 0.50, names=F);
    tmpy[i,3] <- quantile (post.y[i,dipdat$part=="prd"], 0.50, names=F);
    tmpy[i,4] <- quantile (post.y[i,dipdat$part=="pt"], 0.50, names=F);
    tmpy[i,5] <- quantile (post.y[i,dipdat$part=="pvem"], 0.50, names=F);
    tmpy[i,6] <- quantile (post.y[i,dipdat$part=="conve"], 0.50, names=F);
    tmpy[i,7] <- quantile (post.y[i,dipdat$part=="panal"], 0.50, names=F);
    tmpy[i,8] <- quantile (post.y[i,dipdat$part=="psd"], 0.50, names=F);
    }
ptymed <- list( x=tmpx, y=tmpy ); 
#
tmp1 <- post.x - ptymed$x[,1]
tmp2 <- post.x - ptymed$x[,2]
tmp3 <- post.x - ptymed$x[,3]
tmp4 <- post.x - ptymed$x[,4]
tmp5 <- post.x - ptymed$x[,5]
tmp6 <- post.x - ptymed$x[,6]
tmp7 <- post.x - ptymed$x[,7]
tmp8 <- post.x - ptymed$x[,8]
tmpx <- post.x; tmpx[,] <- NA
    tmpx[,dipdat$part=="pan"] <- tmp1[,dipdat$part=="pan"]
    tmpx[,dipdat$part=="pri"] <- tmp2[,dipdat$part=="pri"]
    tmpx[,dipdat$part=="prd"] <- tmp3[,dipdat$part=="prd"]
    tmpx[,dipdat$part=="pt"] <- tmp4[,dipdat$part=="pt"]
    tmpx[,dipdat$part=="pvem"] <- tmp5[,dipdat$part=="pvem"]
    tmpx[,dipdat$part=="conve"] <- tmp6[,dipdat$part=="conve"]
    tmpx[,dipdat$part=="panal"] <- tmp7[,dipdat$part=="panal"]
    tmpx[,dipdat$part=="psd"] <- tmp8[,dipdat$part=="psd"]
tmp1 <- post.y - ptymed$y[,1]
tmp2 <- post.y - ptymed$y[,2]
tmp3 <- post.y - ptymed$y[,3]
tmp4 <- post.y - ptymed$y[,4]
tmp5 <- post.y - ptymed$y[,5]
tmp6 <- post.y - ptymed$y[,6]
tmp7 <- post.y - ptymed$y[,7]
tmp8 <- post.y - ptymed$y[,8]
tmpy <- post.y; tmpy[,] <- NA
    tmpy[,dipdat$part=="pan"] <- tmp1[,dipdat$part=="pan"]
    tmpy[,dipdat$part=="pri"] <- tmp2[,dipdat$part=="pri"]
    tmpy[,dipdat$part=="prd"] <- tmp3[,dipdat$part=="prd"]
    tmpy[,dipdat$part=="pt"] <- tmp4[,dipdat$part=="pt"]
    tmpy[,dipdat$part=="pvem"] <- tmp5[,dipdat$part=="pvem"]
    tmpy[,dipdat$part=="conve"] <- tmp6[,dipdat$part=="conve"]
    tmpy[,dipdat$part=="panal"] <- tmp7[,dipdat$part=="panal"]
    tmpy[,dipdat$part=="psd"] <- tmp8[,dipdat$part=="psd"]
tmp1 <- sqrt(tmpx^2 + tmpy^2)
tmp2 <- tmpx; tmp2[,] <- NA
tmp2[tmpx<=0] <- 0; tmp2[tmpx>0] <- 1; ##tmp2[tmpx<0] <- -1; 
tmp3 <- tmpy; tmp3[,] <- NA
tmp3[tmpy<=0] <- 0; tmp3[tmpy>0] <- 1; ##tmp3[tmpy<0] <- -1; 
dismed <- list(x=tmpx, y=tmpy, d=tmp1, xplus=tmp2, yplus=tmp3)
rm(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7,tmp8,tmpx,tmpy)
#

## NUMBER OF SMD DEPUTIES BY PARTY-STATE
tmp <- colSums(dismed$xplus)/sample.size
tmp2 <- data.frame(pan=rep(NA,32),
                    pri=rep(NA,32),
                    prd=rep(NA,32),
                    pt=rep(NA,32),
                    pvem=rep(NA,32),
                    conve=rep(NA,32),
                    panal=rep(NA,32),
                    psd=rep(NA,32))
rownames(tmp2) <- levels(dipdat$edo)
for (i in 1:32){
    tmp2[i,1] <- length(tmp[dipdat$dsmd==1 & dipdat$part=="pan" & dipdat$edon==i]);
    tmp2[i,2] <- length(tmp[dipdat$dsmd==1 & dipdat$part=="pri" & dipdat$edon==i]);
    tmp2[i,3] <- length(tmp[dipdat$dsmd==1 & dipdat$part=="prd" & dipdat$edon==i]);
    tmp2[i,4] <- length(tmp[dipdat$dsmd==1 & dipdat$part=="pt" & dipdat$edon==i]);
    tmp2[i,5] <- length(tmp[dipdat$dsmd==1 & dipdat$part=="pvem" & dipdat$edon==i]);
    tmp2[i,6] <- length(tmp[dipdat$dsmd==1 & dipdat$part=="conve" & dipdat$edon==i]);
    tmp2[i,7] <- length(tmp[dipdat$dsmd==1 & dipdat$part=="panal" & dipdat$edon==i]);
    tmp2[i,8] <- length(tmp[dipdat$dsmd==1 & dipdat$part=="psd" & dipdat$edon==i]);
    }
## PERCENTAGE STATE'S DEPUTIES ABOVE MEDIAN EACH DIM
tmp3 <- data.frame(pan=rep(NA,32),
                    pri=rep(NA,32),
                    prd=rep(NA,32),
                    pt=rep(NA,32),
                    pvem=rep(NA,32),
                    conve=rep(NA,32),
                    panal=rep(NA,32),
                    psd=rep(NA,32))
rownames(tmp3) <- levels(dipdat$edo)
shrup <- list(ndip=tmp2, x=tmp3, y=tmp3)
for (i in 1:32){
    tmp <- dismed$xplus[,(dipdat$dsmd==1 & dipdat$part=="pan" & dipdat$edon==i)];
    if (shrup$ndip[i,1]>1) {shrup$x[i,1]  <- sum(tmp)/length(tmp)} else {shrup$x[i,1]  <- NA};
    tmp <- dismed$xplus[,(dipdat$dsmd==1 & dipdat$part=="pri" & dipdat$edon==i)];
    if (shrup$ndip[i,2]>1) {shrup$x[i,2]  <- sum(tmp)/length(tmp)} else {shrup$x[i,2]  <- NA};
    tmp <- dismed$xplus[,(dipdat$dsmd==1 & dipdat$part=="prd" & dipdat$edon==i)];
    if (shrup$ndip[i,3]>1) {shrup$x[i,3]  <- sum(tmp)/length(tmp)} else {shrup$x[i,3]  <- NA};
    tmp <- dismed$xplus[,(dipdat$dsmd==1 & dipdat$part=="pt" & dipdat$edon==i)];
    if (shrup$ndip[i,4]>1) {shrup$x[i,4]  <- sum(tmp)/length(tmp)} else {shrup$x[i,4]  <- NA};
    tmp <- dismed$xplus[,(dipdat$dsmd==1 & dipdat$part=="pvem" & dipdat$edon==i)];
    if (shrup$ndip[i,5]>1) {shrup$x[i,5]  <- sum(tmp)/length(tmp)} else {shrup$x[i,5]  <- NA};
    tmp <- dismed$xplus[,(dipdat$dsmd==1 & dipdat$part=="conve" & dipdat$edon==i)];
    if (shrup$ndip[i,6]>1) {shrup$x[i,6]  <- sum(tmp)/length(tmp)} else {shrup$x[i,6]  <- NA};
    tmp <- dismed$xplus[,(dipdat$dsmd==1 & dipdat$part=="panal" & dipdat$edon==i)];
    if (shrup$ndip[i,7]>1) {shrup$x[i,7]  <- sum(tmp)/length(tmp)} else {shrup$x[i,7]  <- NA};
    tmp <- dismed$xplus[,(dipdat$dsmd==1 & dipdat$part=="psd" & dipdat$edon==i)];
    if (shrup$ndip[i,8]>1) {shrup$x[i,8]  <- sum(tmp)/length(tmp)} else {shrup$x[i,8]  <- NA};
    tmp <- dismed$yplus[,(dipdat$dsmd==1 & dipdat$part=="pan" & dipdat$edon==i)];
    if (shrup$ndip[i,1]>1) {shrup$y[i,1]  <- sum(tmp)/length(tmp)} else {shrup$y[i,1]  <- NA};
    tmp <- dismed$yplus[,(dipdat$dsmd==1 & dipdat$part=="pri" & dipdat$edon==i)];
    if (shrup$ndip[i,2]>1) {shrup$y[i,2]  <- sum(tmp)/length(tmp)} else {shrup$y[i,2]  <- NA};
    tmp <- dismed$yplus[,(dipdat$dsmd==1 & dipdat$part=="prd" & dipdat$edon==i)];
    if (shrup$ndip[i,3]>1) {shrup$y[i,3]  <- sum(tmp)/length(tmp)} else {shrup$y[i,3]  <- NA};
    tmp <- dismed$yplus[,(dipdat$dsmd==1 & dipdat$part=="pt" & dipdat$edon==i)];
    if (shrup$ndip[i,4]>1) {shrup$y[i,4]  <- sum(tmp)/length(tmp)} else {shrup$y[i,4]  <- NA};
    tmp <- dismed$yplus[,(dipdat$dsmd==1 & dipdat$part=="pvem" & dipdat$edon==i)];
    if (shrup$ndip[i,5]>1) {shrup$y[i,5]  <- sum(tmp)/length(tmp)} else {shrup$y[i,5]  <- NA};
    tmp <- dismed$yplus[,(dipdat$dsmd==1 & dipdat$part=="conve" & dipdat$edon==i)];
    if (shrup$ndip[i,6]>1) {shrup$y[i,6]  <- sum(tmp)/length(tmp)} else {shrup$y[i,6]  <- NA};
    tmp <- dismed$yplus[,(dipdat$dsmd==1 & dipdat$part=="panal" & dipdat$edon==i)];
    if (shrup$ndip[i,7]>1) {shrup$y[i,7]  <- sum(tmp)/length(tmp)} else {shrup$y[i,7]  <- NA};
    tmp <- dismed$yplus[,(dipdat$dsmd==1 & dipdat$part=="psd" & dipdat$edon==i)];
    if (shrup$ndip[i,8]>1) {shrup$y[i,8]  <- sum(tmp)/length(tmp)} else {shrup$y[i,8]  <- NA};
    }
#
# PLOT, STATE BY STATE, PARTY MEDIAN AND ARROWS FOR SMD DEPUTIES
tmp <- data.frame(x=rep(NA,8),y=rep(NA,8)); rownames(tmp) <- c("pan","pri","prd","pt","pvem","conve","panal", "psd")
for (p in 1:8){
    tmp[p,1] <- quantile (ptymed$x[,p], 0.50, names=F);
    tmp[p,2] <- quantile (ptymed$y[,p], 0.50, names=F);
    }
point.medians <- tmp

#
## AVERAGE DIRECTIONAL SQUARE DISTANCE OF STATE-PARTY CONTINGENT
tmp <- rep(0, 32)
sq.dir <- data.frame(npan=shrup$ndip[,1], xpan=tmp, ypan=tmp,
              npri=shrup$ndip[,2], xpri=tmp, ypri=tmp,
              nprd=shrup$ndip[,3], xprd=tmp, yprd=tmp)
for (e in 1:32){
    tmp <- dismed$x[,(dipdat$dsmd==1 & dipdat$part=="pan" & dipdat$edon==e)];
    if (shrup$ndip[e,1]>0) {sq.dir$xpan[e]  <- round(sum(sign(tmp)*tmp^2)/length(tmp), digits=3)} else {sq.dir$xpan[e]  <- 0};
    tmp <- dismed$y[,(dipdat$dsmd==1 & dipdat$part=="pan" & dipdat$edon==e)];
    if (shrup$ndip[e,1]>0) {sq.dir$ypan[e]  <- round(sum(sign(tmp)*tmp^2)/length(tmp), digits=3)} else {sq.dir$ypan[e]  <- 0};
    tmp <- dismed$x[,(dipdat$dsmd==1 & dipdat$part=="pri" & dipdat$edon==e)];
    if (shrup$ndip[e,2]>0) {sq.dir$xpri[e]  <- round(sum(sign(tmp)*tmp^2)/length(tmp), digits=3)} else {sq.dir$xpri[e]  <- 0};
    tmp <- dismed$y[,(dipdat$dsmd==1 & dipdat$part=="pri" & dipdat$edon==e)];
    if (shrup$ndip[e,2]>0) {sq.dir$ypri[e]  <- round(sum(sign(tmp)*tmp^2)/length(tmp), digits=3)} else {sq.dir$ypri[e]  <- 0};
    tmp <- dismed$x[,(dipdat$dsmd==1 & dipdat$part=="prd" & dipdat$edon==e)];
    if (shrup$ndip[e,3]>0) {sq.dir$xprd[e]  <- round(sum(sign(tmp)*tmp^2)/length(tmp), digits=3)} else {sq.dir$xprd[e]  <- 0};
    tmp <- dismed$y[,(dipdat$dsmd==1 & dipdat$part=="prd" & dipdat$edon==e)];
    if (shrup$ndip[e,3]>0) {sq.dir$yprd[e]  <- round(sum(sign(tmp)*tmp^2)/length(tmp), digits=3)} else {sq.dir$yprd[e]  <- 0};
    }
#

## HOW MANY IN STATE-PARTY GO TO EACH QUADRANT, HOW FAR FROM MEDIAN
tmp <- rep(0, 32)
quads <- data.frame(npan=shrup$ndip[,1], pan1=tmp, pan2=tmp, pan3=tmp, pan4=tmp,
              npri=shrup$ndip[,2], pri1=tmp, pri2=tmp, pri3=tmp, pri4=tmp,
              nprd=shrup$ndip[,3], prd1=tmp, prd2=tmp, prd3=tmp, prd4=tmp)
rownames(quads) <- levels(dipdat$edo)
point.ideals <- data.frame(x=jotas[,2], y=jotas[,5])
## 45-DEGREE CLOCKWISE ROTATION TO ALSO SEE DIAGONAL MOVEMENTS
xR <- post.x*cos(pi/4) + post.y*sin(pi/4)
yR <- -post.x*sin(pi/4) + post.y*cos(pi/4)
xM <- point.medians$x*cos(pi/4) + point.medians$y*sin(pi/4)
yM <- -point.medians$x*sin(pi/4) + point.medians$y*cos(pi/4)
tmp <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    tmp[j,1] <- quantile (xR[,j], 0.025, names=F)
    tmp[j,2] <- quantile (xR[,j], 0.50, names=F)
    tmp[j,3] <- quantile (xR[,j], 0.975, names=F)
    tmp[j,4] <- quantile (yR[,j], 0.025, names=F)
    tmp[j,5] <- quantile (yR[,j], 0.50, names=F)
    tmp[j,6] <- quantile (yR[,j], 0.975, names=F)
    }
point.ideals45 <- data.frame(x=tmp[,2], y=tmp[,5])
point.medians45 <- data.frame(x=xM, y=yM)
rm(xR, yR, xM, yM)
point.plus <- point.ideals; point.plus[,] <- NA; point.plus45 <- point.plus
for (j in 1:J){
    if (point.ideals$x[j] - point.medians$x[party[j]] > 0) {point.plus$x[j] <- 1} else {point.plus$x[j] <- 0}
    if (point.ideals$y[j] - point.medians$y[party[j]] > 0) {point.plus$y[j] <- 1} else {point.plus$y[j] <- 0}
    }
for (j in 1:J){
    if (point.ideals45$x[j] - point.medians45$x[party[j]] > 0) {point.plus45$x[j] <- 1} else {point.plus45$x[j] <- 0}
    if (point.ideals45$y[j] - point.medians45$y[party[j]] > 0) {point.plus45$y[j] <- 1} else {point.plus45$y[j] <- 0}
    }
point.plus$q1 <- 0; point.plus$q2 <- 0; point.plus$q3 <- 0; point.plus$q4 <- 0; 
for (j in 1:J){
    if (point.plus$x[j]==1 & point.plus$y[j]==1) {point.plus$q1[j] <- 1} 
    if (point.plus$x[j]==1 & point.plus$y[j]==0) {point.plus$q2[j] <- 1} 
    if (point.plus$x[j]==0 & point.plus$y[j]==0) {point.plus$q3[j] <- 1} 
    if (point.plus$x[j]==0 & point.plus$y[j]==1) {point.plus$q4[j] <- 1} 
    }
point.plus45$q1 <- 0; point.plus45$q2 <- 0; point.plus45$q3 <- 0; point.plus45$q4 <- 0; 
for (j in 1:J){
    if (point.plus45$x[j]==1 & point.plus45$y[j]==1) {point.plus45$q1[j] <- 1} 
    if (point.plus45$x[j]==1 & point.plus45$y[j]==0) {point.plus45$q2[j] <- 1} 
    if (point.plus45$x[j]==0 & point.plus45$y[j]==0) {point.plus45$q3[j] <- 1} 
    if (point.plus45$x[j]==0 & point.plus45$y[j]==1) {point.plus45$q4[j] <- 1} 
    }
tmp <- rep(0, 32); used45 <- data.frame(pan=tmp, pri=tmp, prd=tmp); rownames(used45) <- levels(dipdat$edo) ## WHICH ROTATION USED
for (e in 1:32){
    select <- (dipdat$dsmd==1 & dipdat$part=="pan" & dipdat$edon==e)
    tmp1 <- colSums(point.plus[select,3:6]);
    tmp2 <- colSums(point.plus45[select,3:6]);
    ## SELECTS ROTATION WITH MAX IN SAME QUADRANT
    if ( max(tmp2) > max(tmp1) ) {quads[e,2:5] <- colSums(point.plus45[select,3:6]); used45[e,1]<-1} else {quads[e,2:5] <- colSums(point.plus[select,3:6])}
    select <- (dipdat$dsmd==1 & dipdat$part=="pri" & dipdat$edon==e)
    tmp1 <- colSums(point.plus[select,3:6]);
    tmp2 <- colSums(point.plus45[select,3:6]);
    ## SELECTS ROTATION WITH MAX IN SAME QUADRANT
    if ( max(tmp2) > max(tmp1) ) {quads[e,7:10] <- colSums(point.plus45[select,3:6]); used45[e,2]<-1} else {quads[e,7:10] <- colSums(point.plus[select,3:6])}
    select <- (dipdat$dsmd==1 & dipdat$part=="prd" & dipdat$edon==e)
    tmp1 <- colSums(point.plus[select,3:6]);
    tmp2 <- colSums(point.plus45[select,3:6]);
    ## SELECTS ROTATION WITH MAX IN SAME QUADRANT
    if ( max(tmp2) > max(tmp1) ) {quads[e,12:15] <- colSums(point.plus45[select,3:6]); used45[e,3]<-1} else {quads[e,12:15] <- colSums(point.plus[select,3:6])}
    }
##
## NATIONAL FIGS
quads.nal <- quads[1,]; quads.nal[,] <- NA; rownames(quads.nal) <- "nal"; used45.nal <- rep(0,3)
 select <- (dipdat$dsmd==1 & dipdat$part=="pan")
 tmp1 <- colSums(point.plus[select,3:6]);
 tmp2 <- colSums(point.plus45[select,3:6]);
 ## SELECTS ROTATION WITH MAX IN SAME QUADRANT
 if ( max(tmp2) > max(tmp1) ) {quads.nal[1,2:5] <- colSums(point.plus45[select,3:6]); used45.nal[1]<-1} else {quads.nal[1,2:5] <- colSums(point.plus[select,3:6])}
 quads.nal[1,1] <- sum(tmp1)
 select <- (dipdat$dsmd==1 & dipdat$part=="pri")
 tmp1 <- colSums(point.plus[select,3:6]);
 tmp2 <- colSums(point.plus45[select,3:6]);
 ## SELECTS ROTATION WITH MAX IN SAME QUADRANT
 if ( max(tmp2) > max(tmp1) ) {quads.nal[1,7:10] <- colSums(point.plus45[select,3:6]); used45.nal[2]<-1} else {quads.nal[1,7:10] <- colSums(point.plus[select,3:6])}
 quads.nal[1,6] <- sum(tmp1)
 select <- (dipdat$dsmd==1 & dipdat$part=="prd")
 tmp1 <- colSums(point.plus[select,3:6]);
 tmp2 <- colSums(point.plus45[select,3:6]);
 ## SELECTS ROTATION WITH MAX IN SAME QUADRANT
 if ( max(tmp2) > max(tmp1) ) {quads.nal[1,12:15] <- colSums(point.plus45[select,3:6]); used45.nal[3]<-1} else {quads.nal[1,12:15] <- colSums(point.plus[select,3:6])}
 quads.nal[1,11] <- sum(tmp1)


## MEDIANA DEL CONTINGENTE DEL CUADRANTE VS MEDIANA DEL PARTIDO, SUM.SQ.DIST STATE-PARTY-IN-QUADRANT
howfar <- quads; howfar[,c(2:5,7:10, 12:15)] <- NA
sum.sq.dist <- quads; sum.sq.dist[,c(2:5,7:10, 12:15)] <- NA
mean.sq.dist <- quads; sum.sq.dist[,c(2:5,7:10, 12:15)] <- NA
for (e in 1:32){
    for (p in 1:3){
        tmp1 <- NA; tmp2 <- NA; tmp3 <- NA; tmp4 <- NA; tmp5 <- NA; tmp6 <- NA; tmp7 <- NA; tmp8 <- NA
        if (used45[e,p]==1) {select <- (dipdat$dsmd==1 & party==p & dipdat$edon==e & point.plus45$q1==1)} else {select <- (dipdat$dsmd==1 & party==p & dipdat$edon==e & point.plus$q1==1)}
        if (used45[e,p]==1) {tmp <- point.ideals45[select,]; tmpm <- point.medians45} else {tmp <- point.ideals[select,]; tmpm <- point.medians}
        if (quads[e,(5*p-4+1)]==1) {tmp1 <- round((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2, 3)} else 
         if (quads[e,(5*p-4+1)]>1) {tmp1 <- round((quantile(tmp$x, .5, names=FALSE) - tmpm$x[p])^2 + (quantile(tmp$y, .5, names=FALSE) - tmpm$y[p])^2, 3)}
        if (quads[e,(5*p-4+1)]==0) {tmp5 <- 0} else
         if (quads[e,(5*p-4+1)]>0) {tmp5 <- round(sum((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2), 1)}
        if (used45[e,p]==1) {select <- (dipdat$dsmd==1 & party==p & dipdat$edon==e & point.plus45$q2==1)} else {select <- (dipdat$dsmd==1 & party==p & dipdat$edon==e & point.plus$q2==1)}
        if (used45[e,p]==1) {tmp <- point.ideals45[select,]; tmpm <- point.medians45} else {tmp <- point.ideals[select,]; tmpm <- point.medians}
        if (quads[e,(5*p-4+2)]==1) {tmp2 <- round((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2, 3)} else 
         if (quads[e,(5*p-4+2)]>1) {tmp2 <- round((quantile(tmp$x, .5, names=FALSE) - tmpm$x[p])^2 + (quantile(tmp$y, .5, names=FALSE) - tmpm$y[p])^2, 3)}
        if (quads[e,(5*p-4+2)]==0) {tmp6 <- 0} else
         if (quads[e,(5*p-4+2)]>0) {tmp6 <- round(sum((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2), 1)}
        if (used45[e,p]==1) {select <- (dipdat$dsmd==1 & party==p & dipdat$edon==e & point.plus45$q3==1)} else {select <- (dipdat$dsmd==1 & party==p & dipdat$edon==e & point.plus$q3==1)}
        if (used45[e,p]==1) {tmp <- point.ideals45[select,]; tmpm <- point.medians45} else {tmp <- point.ideals[select,]; tmpm <- point.medians}
        if (quads[e,(5*p-4+3)]==1) {tmp3 <- round((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2, 3)} else 
         if (quads[e,(5*p-4+3)]>1) {tmp3 <- round((quantile(tmp$x, .5, names=FALSE) - tmpm$x[p])^2 + (quantile(tmp$y, .5, names=FALSE) - tmpm$y[p])^2, 3)}
        if (quads[e,(5*p-4+3)]==0) {tmp7 <- 0} else
         if (quads[e,(5*p-4+3)]>0) {tmp7 <- round(sum((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2), 1)}
        if (used45[e,p]==1) {select <- (dipdat$dsmd==1 & party==p & dipdat$edon==e & point.plus45$q4==1)} else {select <- (dipdat$dsmd==1 & party==p & dipdat$edon==e & point.plus$q4==1)}
        if (used45[e,p]==1) {tmp <- point.ideals45[select,]; tmpm <- point.medians45} else {tmp <- point.ideals[select,]; tmpm <- point.medians}
        if (quads[e,(5*p-4+4)]==1) {tmp4 <- round((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2, 3)} else 
         if (quads[e,(5*p-4+4)]>1) {tmp4 <- round((quantile(tmp$x, .5, names=FALSE) - tmpm$x[p])^2 + (quantile(tmp$y, .5, names=FALSE) - tmpm$y[p])^2, 3)}
        if (quads[e,(5*p-4+4)]==0) {tmp8 <- 0} else
         if (quads[e,(5*p-4+4)]>0) {tmp8 <- round(sum((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2), 1)}
        howfar[e,(5*p-4+1)] <- tmp1; howfar[e,(5*p-4+2)] <- tmp2; howfar[e,(5*p-4+3)] <- tmp3; howfar[e,(5*p-4+4)] <- tmp4;
        sum.sq.dist[e,(5*p-4+1)] <- tmp5; sum.sq.dist[e,(5*p-4+2)] <- tmp6; sum.sq.dist[e,(5*p-4+3)] <- tmp7; sum.sq.dist[e,(5*p-4+4)] <- tmp8;
        }
    }
rm(tmp, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8, tmpm)
##
for (e in 1:32){
    for (p in 1:3){
        if (quads[e,(5*p-4+1)]>0) {mean.sq.dist[e,(5*p-4+1)] <- round(sum.sq.dist[e,(5*p-4+1)] / quads[e,(5*p-4+1)],1)}
        if (quads[e,(5*p-4+2)]>0) {mean.sq.dist[e,(5*p-4+2)] <- round(sum.sq.dist[e,(5*p-4+2)] / quads[e,(5*p-4+2)],1)}
        if (quads[e,(5*p-4+3)]>0) {mean.sq.dist[e,(5*p-4+3)] <- round(sum.sq.dist[e,(5*p-4+3)] / quads[e,(5*p-4+3)],1)}
        if (quads[e,(5*p-4+4)]>0) {mean.sq.dist[e,(5*p-4+4)] <- round(sum.sq.dist[e,(5*p-4+4)] / quads[e,(5*p-4+4)],1)}
        }
    }
##
## MAKES Q1 REPORT MAXIMUM IN SAME QUADRANT IN STATE-PARTY (OTHERS ROTATED ACCORDINGLY)
tmp <- array(1:(dim(quads)[1]*dim(quads)[2]*2), dim=c(dim(quads),4))
tmp[,,1] <- as.matrix(quads); tmp[,,2] <- as.matrix(howfar); tmp[,,3] <- as.matrix(sum.sq.dist)
tmp[,,4] <- as.matrix(mean.sq.dist)
qmax <- used45; qmax[,] <- NA
##
for (e in 1:32){
    for (p in 1:3){
        tmp1 <- tmp[e,(5*p-4+1),]; tmp2 <- tmp[e,(5*p-4+2),]; tmp3 <- tmp[e,(5*p-4+3),]; tmp4 <- tmp[e,(5*p-4+4),]; 
        tmp5 <- max(tmp1[1], tmp2[1], tmp3[1], tmp4[1])
        if (tmp1[1]==tmp5) {tmpa <- tmp1; tmpb <- tmp2; tmpc <- tmp3; tmpd <- tmp4; qmax[e,p] <- 1} else
         if (tmp2[1]==tmp5) {tmpa <- tmp2; tmpb <- tmp3; tmpc <- tmp4; tmpd <- tmp1; qmax[e,p] <- 2} else
          if (tmp3[1]==tmp5) {tmpa <- tmp3; tmpb <- tmp4; tmpc <- tmp1; tmpd <- tmp2; qmax[e,p] <- 3} else
           if (tmp4[1]==tmp5) {tmpa <- tmp4; tmpb <- tmp1; tmpc <- tmp2; tmpd <- tmp3; qmax[e,p] <- 4}
        tmp[e,(5*p-4+1),] <- tmpa; tmp[e,(5*p-4+2),] <- tmpb; tmp[e,(5*p-4+3),] <- tmpc; tmp[e,(5*p-4+4),] <- tmpd; 
        }
    }
for (e in 1:32){
    quads[e,] <- tmp[e,,1];
    howfar[e,] <- tmp[e,,2];
    sum.sq.dist[e,] <- tmp[e,,3];
    mean.sq.dist[e,] <- tmp[e,,4];
    }
##
sum.sq.dist.nal <- quads.nal
mean.sq.dist.nal <- quads.nal
for (p in 1:3){
 tmp1 <- NA; tmp2 <- NA; tmp3 <- NA; tmp4 <- NA; tmp5 <- NA; tmp6 <- NA; tmp7 <- NA; tmp8 <- NA
 if (used45.nal[p]==1) {select <- (dipdat$dsmd==1 & party==p & point.plus45$q1==1)} else {select <- (dipdat$dsmd==1 & party==p & point.plus$q1==1)}
 if (used45.nal[p]==1) {tmp <- point.ideals45[select,]; tmpm <- point.medians45} else {tmp <- point.ideals[select,]; tmpm <- point.medians}
 if (quads.nal[1,(5*p-4+1)]==1) {tmp1 <- round((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2, 3)} else 
  if (quads.nal[1,(5*p-4+1)]>1) {tmp1 <- round((quantile(tmp$x, .5, names=FALSE) - tmpm$x[p])^2 + (quantile(tmp$y, .5, names=FALSE) - tmpm$y[p])^2, 3)}
 if (quads.nal[1,(5*p-4+1)]==0) {tmp5 <- 0} else
  if (quads.nal[1,(5*p-4+1)]>0) {tmp5 <- round(sum((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2), 1)}
 if (used45.nal[p]==1) {select <- (dipdat$dsmd==1 & party==p & point.plus45$q2==1)} else {select <- (dipdat$dsmd==1 & party==p & point.plus$q2==1)}
 if (used45.nal[p]==1) {tmp <- point.ideals45[select,]; tmpm <- point.medians45} else {tmp <- point.ideals[select,]; tmpm <- point.medians}
 if (quads.nal[1,(5*p-4+2)]==1) {tmp2 <- round((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2, 3)} else 
  if (quads.nal[1,(5*p-4+2)]>1) {tmp2 <- round((quantile(tmp$x, .5, names=FALSE) - tmpm$x[p])^2 + (quantile(tmp$y, .5, names=FALSE) - tmpm$y[p])^2, 3)}
 if (quads.nal[1,(5*p-4+2)]==0) {tmp6 <- 0} else
  if (quads.nal[1,(5*p-4+2)]>0) {tmp6 <- round(sum((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2), 1)}
 if (used45.nal[p]==1) {select <- (dipdat$dsmd==1 & party==p & point.plus45$q3==1)} else {select <- (dipdat$dsmd==1 & party==p & point.plus$q3==1)}
 if (used45.nal[p]==1) {tmp <- point.ideals45[select,]; tmpm <- point.medians45} else {tmp <- point.ideals[select,]; tmpm <- point.medians}
 if (quads.nal[1,(5*p-4+3)]==1) {tmp3 <- round((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2, 3)} else 
  if (quads.nal[1,(5*p-4+3)]>1) {tmp3 <- round((quantile(tmp$x, .5, names=FALSE) - tmpm$x[p])^2 + (quantile(tmp$y, .5, names=FALSE) - tmpm$y[p])^2, 3)}
 if (quads.nal[1,(5*p-4+3)]==0) {tmp7 <- 0} else
  if (quads.nal[1,(5*p-4+3)]>0) {tmp7 <- round(sum((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2), 1)}
 if (used45.nal[p]==1) {select <- (dipdat$dsmd==1 & party==p & point.plus45$q4==1)} else {select <- (dipdat$dsmd==1 & party==p & point.plus$q4==1)}
 if (used45.nal[p]==1) {tmp <- point.ideals45[select,]; tmpm <- point.medians45} else {tmp <- point.ideals[select,]; tmpm <- point.medians}
 if (quads.nal[1,(5*p-4+4)]==1) {tmp4 <- round((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2, 3)} else 
  if (quads.nal[1,(5*p-4+4)]>1) {tmp4 <- round((quantile(tmp$x, .5, names=FALSE) - tmpm$x[p])^2 + (quantile(tmp$y, .5, names=FALSE) - tmpm$y[p])^2, 3)}
 if (quads.nal[1,(5*p-4+4)]==0) {tmp8 <- 0} else
  if (quads.nal[1,(5*p-4+4)]>0) {tmp8 <- round(sum((tmp$x - tmpm$x[p])^2 + (tmp$y - tmpm$y[p])^2), 1)}
 #howfar[1,(5*p-4+1)] <- tmp1; howfar[1,(5*p-4+2)] <- tmp2; howfar[1,(5*p-4+3)] <- tmp3; howfar[1,(5*p-4+4)] <- tmp4;
 sum.sq.dist.nal[1,(5*p-4+1)] <- tmp5; sum.sq.dist.nal[1,(5*p-4+2)] <- tmp6; sum.sq.dist.nal[1,(5*p-4+3)] <- tmp7; sum.sq.dist.nal[1,(5*p-4+4)] <- tmp8;
 }
##
for (p in 1:3){
    if (quads.nal[1,(5*p-4+1)]>0) {mean.sq.dist.nal[1,(5*p-4+1)] <- round(sum.sq.dist.nal[1,(5*p-4+1)] / quads.nal[1,(5*p-4+1)],1)}
    if (quads.nal[1,(5*p-4+2)]>0) {mean.sq.dist.nal[1,(5*p-4+2)] <- round(sum.sq.dist.nal[1,(5*p-4+2)] / quads.nal[1,(5*p-4+2)],1)}
    if (quads.nal[1,(5*p-4+3)]>0) {mean.sq.dist.nal[1,(5*p-4+3)] <- round(sum.sq.dist.nal[1,(5*p-4+3)] / quads.nal[1,(5*p-4+3)],1)}
    if (quads.nal[1,(5*p-4+4)]>0) {mean.sq.dist.nal[1,(5*p-4+4)] <- round(sum.sq.dist.nal[1,(5*p-4+4)] / quads.nal[1,(5*p-4+4)],1)}
    }
rm(tmp, tmp1, tmp2, tmp3, tmp4, tmp5)

P<-8
cplt <- function(X)
    {
    tmp <- c( min(jotas[dipdat$dsmd==1,2], jotas[dipdat$dsmd==1,5]), max(jotas[dipdat$dsmd==1,2], jotas[dipdat$dsmd==1,5]) )
    plot(tmp, tmp, type="n",
               xlab="",
               ylab="",
               xaxt="n",
               yaxt="n",
               main=paste("smds from", levels(dipdat$edo)[X]));
    largo <- (tmp[2]-tmp[1])/10;
    points(point.medians[shrup$ndip[X,]>0,1], point.medians[shrup$ndip[X,]>0,2], cex=.75, col=color.list[shrup$ndip[X,]>0])
    tmp <- used45; tmp[,4:8] <- 0;
    for (p in c(1:P)[shrup$ndip[X,]>0]){
    segments(point.medians$x[p], point.medians$y[p], point.medians$x[p], point.medians$y[p]+largo, col="grey");
    segments(point.medians$x[p], point.medians$y[p], point.medians$x[p], point.medians$y[p]-largo, col="grey");
    segments(point.medians$x[p], point.medians$y[p], point.medians$x[p]+largo, point.medians$y[p], col="grey");
    segments(point.medians$x[p], point.medians$y[p], point.medians$x[p]-largo, point.medians$y[p], col="grey");
## THIS BATCH USES ROTATED QUADRANTS
#    if (tmp[X,p]==1) {segments(point.medians$x[p], point.medians$y[p], point.medians$x[p]+largo*.8, point.medians$y[p]+largo*.8, col="grey")} else {segments(point.medians$x[p], point.medians$y[p], point.medians$x[p], point.medians$y[p]+largo, col="grey")};
#    if (tmp[X,p]==1) {segments(point.medians$x[p], point.medians$y[p], point.medians$x[p]-largo*.8, point.medians$y[p]-largo*.8, col="grey")} else {segments(point.medians$x[p], point.medians$y[p], point.medians$x[p], point.medians$y[p]-largo, col="grey")};
#    if (tmp[X,p]==1) {segments(point.medians$x[p], point.medians$y[p], point.medians$x[p]+largo*.8, point.medians$y[p]-largo*.8, col="grey")} else {segments(point.medians$x[p], point.medians$y[p], point.medians$x[p]+largo, point.medians$y[p], col="grey")};
#    if (tmp[X,p]==1) {segments(point.medians$x[p], point.medians$y[p], point.medians$x[p]-largo*.8, point.medians$y[p]+largo*.8, col="grey")} else {segments(point.medians$x[p], point.medians$y[p], point.medians$x[p]-largo, point.medians$y[p], col="grey")};
    }
#
    tmp <- dipdat$dsmd==1 & dipdat$edon==X;
    for (j in c(1:J)[tmp]){
        points(jotas[j,2],jotas[j,5], pch=19, cex=.75, col=dipdat$color[j])
 #       text(jotas[j,2],jotas[j,5], label=dipdat$id[j], cex=.75); ## pone ids
        }
    }
##
setwd(paste(workdir, "/graphs/states", sep=""))
for (e in 1:32){
    pdf( paste("60state", e, ".pdf", sep=""))
    cplt(e)
    dev.off()
    }
setwd(workdir)



## STATE PARTY CONTINGENT MEDIANS
tmp <- as.data.frame(matrix(NA, nrow=300, ncol=32)); tmp2<- tmp
p <- 1
for (e in 1:32){
    select <- dipdat$dsmd==1 & dipdat$edon==e & party==p
    if (quads[e,(5*p-4)]>1) {tmp[,e] <- apply(post.x[,select],1,median)}
    if (quads[e,(5*p-4)]>1) {tmp2[,e] <- apply(post.y[,select],1,median)}
    }
pantmp <- list(x=tmp, y=tmp2)
p <- 2
for (e in 1:32){
    select <- dipdat$dsmd==1 & dipdat$edon==e & party==p
    if (quads[e,(5*p-4)]>1) {tmp[,e] <- apply(post.x[,select],1,median)}
    if (quads[e,(5*p-4)]>1) {tmp2[,e] <- apply(post.y[,select],1,median)}
    }
pritmp <- list(x=tmp, y=tmp2)
p <- 3
for (e in 1:32){
    select <- dipdat$dsmd==1 & dipdat$edon==e & party==p
    if (quads[e,(5*p-4)]>1) {tmp[,e] <- apply(post.x[,select],1,median)}
    if (quads[e,(5*p-4)]>1) {tmp2[,e] <- apply(post.y[,select],1,median)}
    }
prdtmp <- list(x=tmp, y=tmp2)
post.state <- list(pan=pantmp, pri=pritmp, prd=prdtmp)
#
tmp <- rep(NA, 32*3); state.party <- data.frame(x.025=tmp, x.5=tmp, x.975=tmp, y.025=tmp, y.5=tmp, y.975=tmp)
rownames(state.party) <- paste(levels(dipdat$edo), ".", c(rep("pan",32),rep("pri",32),rep("prd",32)), sep="")
for (e in 1:32){
    if (quads[e,1]>1) {state.party[e,1] <- quantile (post.state$pan$x[,e], 0.025, names=F)}
    if (quads[e,1]>1) {state.party[e,2] <- quantile (post.state$pan$x[,e], 0.5, names=F)}
    if (quads[e,1]>1) {state.party[e,3] <- quantile (post.state$pan$x[,e], 0.975, names=F)}
    if (quads[e,1]>1) {state.party[e,4] <- quantile (post.state$pan$y[,e], 0.025, names=F)}
    if (quads[e,1]>1) {state.party[e,5] <- quantile (post.state$pan$y[,e], 0.5, names=F)}
    if (quads[e,1]>1) {state.party[e,6] <- quantile (post.state$pan$y[,e], 0.975, names=F)}
    if (quads[e,6]>1) {state.party[32+e,1] <- quantile (post.state$pri$x[,e], 0.025, names=F)}
    if (quads[e,6]>1) {state.party[32+e,2] <- quantile (post.state$pri$x[,e], 0.5, names=F)}
    if (quads[e,6]>1) {state.party[32+e,3] <- quantile (post.state$pri$x[,e], 0.975, names=F)}
    if (quads[e,6]>1) {state.party[32+e,4] <- quantile (post.state$pri$y[,e], 0.025, names=F)}
    if (quads[e,6]>1) {state.party[32+e,5] <- quantile (post.state$pri$y[,e], 0.5, names=F)}
    if (quads[e,6]>1) {state.party[32+e,6] <- quantile (post.state$pri$y[,e], 0.975, names=F)}
    if (quads[e,11]>1) {state.party[64+e,1] <- quantile (post.state$prd$x[,e], 0.025, names=F)}
    if (quads[e,11]>1) {state.party[64+e,2] <- quantile (post.state$prd$x[,e], 0.5, names=F)}
    if (quads[e,11]>1) {state.party[64+e,3] <- quantile (post.state$prd$x[,e], 0.975, names=F)}
    if (quads[e,11]>1) {state.party[64+e,4] <- quantile (post.state$prd$y[,e], 0.025, names=F)}
    if (quads[e,11]>1) {state.party[64+e,5] <- quantile (post.state$prd$y[,e], 0.5, names=F)}
    if (quads[e,11]>1) {state.party[64+e,6] <- quantile (post.state$prd$y[,e], 0.975, names=F)}
    }
##
## PLOT
## dipdat$color[dipdat$color=="."] <- "black"
##plot(c(min(jotas[,2]),max(jotas[,2])), c(min(jotas[,5]),max(jotas[,5])), type="n")
par(mai=c(.4, .4, .4, .4)) ## SETS B L U R MARGIN SIZES
select <- dipdat$dsmd==1
plot(c(min(jotas[select,1]),max(jotas[select,3])), c(min(jotas[select,4]),max(jotas[select,6])), type="n", 
           xlab="", 
           ylab="", 
           xaxt="n",
           yaxt="n",
#           xlab=c("dim2_all"), 
#           ylab=c("dim2_gaceta"), 
#           main="61st Leg. SMDs 2009-11")
           main="")
##abline(0,1)
##points(jotas[,2],jotas[,5], pch=19, cex=.75, col=dipdat$color)
##points(jotas[dipdat$dsm==1,2],jotas[dipdat$dsm==1,5], pch=19, cex=.75, col=dipdat$color[dipdat$dsm==1])
segments(jotas[select,1],jotas[select,5], jotas[select,3],jotas[select,5], col=dipdat$color[select])
segments(jotas[select,2],jotas[select,4], jotas[select,2],jotas[select,6], col=dipdat$color[select])
#### STATE PARTY MEDIAN 95CIs
##segments(state.party[,1],state.party[,5], state.party[,3],state.party[,5], col=c(rep(color.list[1],32),rep(color.list[2],32),rep(color.list[3],32)))
##segments(state.party[,2],state.party[,4], state.party[,2],state.party[,6], col=c(rep(color.list[1],32),rep(color.list[2],32),rep(color.list[3],32)))
points(state.party[,2],state.party[,5], pch=19, cex=1, col="white"); ## pone medias estatales
points(state.party[,2],state.party[,5], pch=19, cex=.75); ## pone medias estatales
legend(-2.25,2.15, legend=part.list[1:6], cex=.75, pch=3, pt.cex=1.25, col=color.list[1:6], bg="white")
##points(jotas[,5],jotas.gac[,5], pch=19, cex=.75, col=dipdat$color)




## EXTRACT FIRST-DIMENSION CUTPOINTS
post.cutpoint <- -post.b/post.a
## POSTERIOR FIRST-DIMENSION PREDICTIVE ACCURACY
v <- rc  
v <- apply(v, 2, recode, recodes="-1=0; 0=NA; 1=1") ## RECODES VOTES TO 0,1
tmp <- rc; tmp[,] <- NA
post.mu <- tmp; post.v.star <- tmp; post.p <- tmp; post.v.hat <- tmp; rm(tmp)
##
for (i in 1:I){
  for (j in 1:J){
    post.mu[i,j] <- post.d[i]*(post.x[j] - post.cutpoint[i]);                 ## utility differential
    post.v.hat[i,j] <- ifelse( post.mu[i,j]>0, 1, 0 );                        ## voting rule
                }
              }
post.dvoteOK <- 1 - abs( v - post.v.hat )
tmp <- as.numeric(post.dvoteOK)
##
## PERCENTAGE VOTES PREDICTED CORRECTLY:
sum(tmp[is.na(tmp)==FALSE])/length(tmp[is.na(tmp)==FALSE])
##
## PLOT
select <- dipdat$dsmd==1
tmp <- jotas[select,1:3]
tmp2 <- dipdat$color[select]
J2 <- length(tmp2)
tmp <- tmp[order(jotas[select,2]),]
tmp2 <- tmp2[order(jotas[select,2])]
#
plot(c(min(tmp[,1]),max(tmp[,3])), c(1,J2), type="n",
    xlab="First-dimension coordinate",
    ylab="SMD deputies")
##for (j in 1:J2){
##    segments(tmp[j,1],j,tmp[j,3],j, cex=.2, col=tmp2[j])
##    }
##for (j in 1:J2){
##    points(tmp[j,2],j,pch=19,cex=.75,col="white")
##    }
for (j in 1:J2){
    points(tmp[j,2],j,pch=19,cex=.5,col=tmp2[j])
    }




##howfar.std <- howfar
##for (e in 1:32){
##    for (p in 1:3){
##        if (quads[e,(5*p-4+1)]>1) {howfar.std[e,(5*p-4+1)] <- round(howfar[e,(5*p-4+1)]/mean.sq.dist[e,(5*p-4+1)],3)}
##        if (quads[e,(5*p-4+2)]>1) {howfar.std[e,(5*p-4+2)] <- round(howfar[e,(5*p-4+2)]/mean.sq.dist[e,(5*p-4+2)],3)}
##        if (quads[e,(5*p-4+3)]>1) {howfar.std[e,(5*p-4+3)] <- round(howfar[e,(5*p-4+3)]/mean.sq.dist[e,(5*p-4+3)],3)}
##        if (quads[e,(5*p-4+4)]>1) {howfar.std[e,(5*p-4+4)] <- round(howfar[e,(5*p-4+4)]/mean.sq.dist[e,(5*p-4+4)],3)}
##        }
##    }


### EXPORTA COORDENADAS DE TODOS LOS DIPUTADOS
tmp <- data.frame(x=jotas[,2], y=jotas[,5], id=dipdat$id, edon=dipdat$edon, party=dipdat$part, nombre=dipdat$nom)
write.table(tmp, file="leg61IdPts.csv", sep=",")

tmp <- matrix(NA, nrow=J, ncol=5)
tmp[,1] <- as.numeric(jotas[,2])
tmp[,2] <- jotas[,5]
tmp[,3] <- dipdat$id,
tmp[,4] <- dipdat$nom,
tmp[,5] <- dipdat$part
tmp<-data.matrix(tmp)
tmp[,1:2] <- as.numeric(tmp[,1:2])
write.table(tmp, file="leg61IdPts.csv", sep=",")



## POSTERIOR PREDICTIVE ACCURACY
v <- rc  
v <- apply(v, 2, recode, recodes="-1=0; 0=NA; 1=1") ## RECODES VOTES TO 0,1
tmp <- rc; tmp[,] <- NA
post.mu <- tmp; post.v.star <- tmp; post.p <- tmp; post.v.hat <- tmp; rm(tmp)
##
for (i in 1:I){
  for (j in 1:J){
    post.mu[i,j] <- post.d[i]*post.a[i]*post.x[j] 
                    + post.d[i]*post.b[i] 
                    - post.d[i]*post.y[j];                                   ## utility differential
    post.v.star[i,j] <- rnorm(1,post.mu[i,j],1);                             ## random utility model
    post.v.hat[i,j] <- ifelse( post.v.star[i,j]>0, 1, 0 );                   ## voting rule
#    v[i,j] <- ifelse(is.na(v[i,j])==TRUE, rbinom(1,1,.5), v[i,j]);           ## FILL NAs WITH COINTOSS
                }
              }
post.dvoteOK <- 1 - abs( v - post.v.hat )
tmp <- as.numeric(post.dvoteOK)
##
## PERCENTAGE VOTES PREDICTED CORRECTLY:
sum(tmp[is.na(tmp)==FALSE])/length(tmp[is.na(tmp)==FALSE])


