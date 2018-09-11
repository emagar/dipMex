#RUN dipbugs**.r WITH NO SEED UP TO SAMPLING, THEN THIS

#ch <- 1

## NEEDED TO RUN IN JAGS INSTEAD OF BUGS (ABSTENTIONS AND ABSENCES AS MISSING)
#rc <- apply(rc, 2, recode, recodes="-1=0; 0=NA") 

## ## SORT BY PARTY 60th
## tmp <- 1:dim(rc)[1]
## tmp <- ifelse ( dipdat$part=="pan", tmp,
##         ifelse ( dipdat$part=="pri", tmp+1000,
##          ifelse ( dipdat$part=="prd", tmp+2000,
##           ifelse ( dipdat$part=="pt", tmp+3000,
##            ifelse ( dipdat$part=="pvem", tmp+4000,
##             ifelse ( dipdat$part=="conve", tmp+5000,
##              ifelse ( dipdat$part=="panal", tmp+6000, 
##               ifelse ( dipdat$part=="psd", tmp+7000, tmp+8000 ))))))))
## ## rcold <- rc; dipold <- dipdat
## rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## ## PARTY INDICES (FOR ANCHORS)
## PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
## PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])
## PVEM <- PT+length(tmp[tmp>4000&tmp<5001]); CONVE <- PVEM+length(tmp[tmp>5000&tmp<6001])
## PANAL <- CONVE+length(tmp[tmp>6000&tmp<7001]); PSD <- PANAL+length(tmp[tmp>7000&tmp<8001])

## SORT BY PARTY 61st
tmp <- 1:nrow(rc)
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse ( dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000,
           ifelse ( dipdat$part=="pvem", tmp+4000,
            ifelse ( dipdat$part=="conve", tmp+5000,
             ifelse ( dipdat$part=="panal", tmp+6000, tmp+7000 )))))))
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])
PVEM <- PT+length(tmp[tmp>4000&tmp<5001]); CONVE <- PVEM+length(tmp[tmp>5000&tmp<6001])
PANAL <- CONVE+length(tmp[tmp>6000&tmp<7001])

## DROPS VOTES NOT IN GACETA
#rcG <- rc[dgaceta==1,]
#votdatG <- votdat[dgaceta==1,]

## USED FOR SEED IN SEPARATE RUNS
#set.seed(round(ch*runif(1)*1000))
#runif(1) ## RANDOM NUMBER TO CHECK

##### LOADS SAVED RUNS
###load("loops5k.RData")


## BUGS VERSION
#####################################################################################
###   Static 2Dimensions, arbitrary three party-anchors, irt paremeterization     ###
#####################################################################################
#
## ## FOR 60th
## cat("
## model {
##   for (j in 1:J){                ## loop over diputados
##     for (i in 1:I){              ## loop over items
##       #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
##       #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
##       v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
##       mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j] ## utility differential
##                   }
##                 }
## ## ESTO LO PUEDO SACAR POST ESTIMACION
## ##  for (i in 1:I){
## ##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
## ##  b[i] <- alpha[i] / delta[i] ## constante de cutline
## ##  }
##   ## priors ################
## for (j in 1:PAN){
##     x[j] ~  dnorm(1, 4)   # PAN
##     y[j] ~  dnorm(-1, 4)
##     }
## for (j in (PAN+1):PRI){
##     x[j] ~  dnorm(0, 4)    # PRI
##     y[j] ~  dnorm(.5, 4)
##     }
## for (j in (PRI+1):PRD){
##     x[j] ~  dnorm(-1, 4)    # PRD SEMI-INFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
## for (j in (PRD+1):PVEM){
##     x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
## for (j in (PVEM+1):CONVE){
##     x[j] ~  dnorm(-1, 4)    # CONVE
##     y[j] ~  dnorm(1, 4)
##     }
## for (j in (CONVE+1):J){
##     x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
##     for(i in 1:I){
##         alpha[i] ~ dnorm( 0, 1)
##         beta[i]  ~ dnorm( 0, 1)
##         delta[i] ~ dnorm( 0, 1)
##                  }
## }
## ", file="model2Dj.irt.txt")
#
### FOR 61st
cat("
model {
  for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j] ## utility differential
                  }
                }
## ESTO LO PUEDO SACAR POST ESTIMACION
##  for (i in 1:I){
##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
##  b[i] <- alpha[i] / delta[i] ## constante de cutline
##  }
  ## priors ################
for (j in 1:PAN){
    x[j] ~  dnorm(1, 4)   # PAN
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PAN+1):PRI){
    x[j] ~  dnorm(0, 4)    # PRI
    y[j] ~  dnorm(1, 4)
    }
for (j in (PRI+1):PRD){
    x[j] ~  dnorm(-1, 4)    # PRD
    y[j] ~  dnorm(0, 1)
    }
for (j in (PRD+1):PT){
    x[j] ~  dnorm(-1, 4)    # PT
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PT+1):J){
    x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
    y[j] ~  dnorm(0, .1)
    }
    for(i in 1:I){
        alpha[i] ~ dnorm( 0, 1)
        beta[i]  ~ dnorm( 0, 1)
        delta[i] ~ dnorm( 0, 1)
                 }
}
", file="model2Dj.irt.txt")

## ## JAGS VERSION
## #####################################################################################
## ###   Static 2Dimensions, arbitrary three party-anchors, irt paremeterization     ###
## #####################################################################################
## ##
## ## FOR 60th
## cat("
## model {
##   for (j in 1:J){                                             ## loop over diputados
##     for (i in 1:I){                                           ## loop over items
##       v[j,i] ~ dbern(p[j,i]);                                 ## voting rule
##       probit(p[j,i]) <- mu[j,i];                              ## sets 0<p<1
##       mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j]      ## utility differential
##                   }
##                 }
## ## ESTO LO PUEDO SACAR POST ESTIMACION
## ##  for (i in 1:I){
## ##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
## ##  b[i] <- alpha[i] / delta[i] ## constante de cutline
## ##  }
##   ## priors ################
## for (j in 1:PAN){
##     x[j] ~  dnorm(1, 4)   # PAN
##     y[j] ~  dnorm(-1, 4)
##     }
## for (j in (PAN+1):PRI){
##     x[j] ~  dnorm(0, 4)    # PRI
##     y[j] ~  dnorm(.5, 4)
##     }
## for (j in (PRI+1):PRD){
##     x[j] ~  dnorm(-1, 4)    # PRD SEMI-INFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
## for (j in (PRD+1):PVEM){
##     x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
## for (j in (PVEM+1):CONVE){
##     x[j] ~  dnorm(-1, 4)    # CONVE
##     y[j] ~  dnorm(1, 4)
##     }
## for (j in (CONVE+1):J){
##     x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
##     y[j] ~  dnorm(0, .1)
##     }
##     for(i in 1:I){
##         alpha[i] ~ dnorm( 0, 1)
##         beta[i]  ~ dnorm( 0, 1)
##         delta[i] ~ dnorm( 0, 1)
##                  }
## }
## ", file="model2Dj.irt.txt")
## ##
## ###FOR 61st
## ###cat("
## ###model {
## ###  for (j in 1:J){                                             ## loop over diputados
## ###    for (i in 1:I){                                           ## loop over items
## ###      v[j,i] ~ dbern(p[j,i]);                                 ## voting rule
## ###      probit(p[j,i]) <- mu[j,i];                              ## sets 0<p<1
## ###      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j]      ## utility differential
## ###                  }
## ###                }
## ##### ESTO LO PUEDO SACAR POST ESTIMACION
## #####  for (i in 1:I){
## #####  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
## #####  b[i] <- alpha[i] / delta[i] ## constante de cutline
## #####  }
## ###  ## priors ################
## ###for (j in 1:PAN){
## ###    x[j] ~  dnorm(1, 4)   # PAN
## ###    y[j] ~  dnorm(-1, 4)
## ###    }
## ###for (j in (PAN+1):PRI){
## ###    x[j] ~  dnorm(0, 4)    # PRI
## ###    y[j] ~  dnorm(1, 4)
## ###    }
## ###for (j in (PRI+1):PRD){
## ###    x[j] ~  dnorm(-1, 4)    # PRD
## ###    y[j] ~  dnorm(-1, 4)
## ###    }
## ###for (j in (PRD+1):J){
## ###    x[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
## ###    y[j] ~  dnorm(0, .1)
## ###    }
## ###    for(i in 1:I){
## ###        alpha[i] ~ dnorm( 0, 1)
## ###        beta[i]  ~ dnorm( 0, 1)
## ###        delta[i] ~ dnorm( 0, 1)
## ###                 }
## ###}
## ###", file="model2Dj.irt.txt")


myrun <- function (data=rc, votdat.data=votdat, sample.size=50)
{
start.time <- proc.time()
#### WORK WITH VOTE SAMPLE
sel <- sample(votdat.data$votid, size=sample.size); sel <- sel[order(sel)]
data <- data[votdat.data$votid %in% sel,]
votdat.data <- votdat.data[votdat.data$votid %in% sel,]
###rnd <- runif(nrow(rcG))
###rcG <- rcG[rnd<sample.size,]
###votdatG <- votdatG[rnd<sample.size,]
#rm(rnd)
J <- ncol(data); I <- nrow(data)
##
v <- data
v <- t(v)
#J <- nrow(v); I <- ncol(v)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
ip.data <- list ("J", "I", "lo.v", "hi.v", "PAN", "PRI", "PRD", "PT")
ip.inits <- function (){
    list (
    v.star=vstar,
    delta=rnorm(I),
    alpha=rnorm(I),
    beta=rnorm(I),
    x=rnorm(J),
    y=rnorm(J)
    )
    }
ip.parameters <- c("delta","beta", "alpha", "x", "y") #, "deviance")
#
results <- bugs (ip.data, ip.inits, ip.parameters, 
                "model2Dj.irt.txt", n.chains=2, 
                n.iter=6000, n.burnin=4000, n.thin=20, debug=F,
#                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS")
                )
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n"))
res <- list("results"<-results, "sample"=sel)
return(res)
}


for (l in 26:33){
    print(paste("Loop G", l))
    results <- myrun(data=rcG, votdat.data=votdatG)
    assign(paste("resG", 33*(ch-1)+l, sep=""), results)
    }

third <- 1 # or 2 or 3
l <- 1
for (l in 1:33){
    print(paste("Loop", l))
    results <- myrun(data=rc, votdat.data=votdat)
    assign(paste("res", 33*(third-1)+l, sep=""), results)
    }


## LOAD DATA FOR POST ESTIMATION ANALYSIS

rm(list=ls())
#workdir <- c("/media/shared/01/Dropbox/data/rollcall/dipMex")
workdir <- c("~/Dropbox/data/rollcall/dipMex")
#workdir <- c("d:/01/Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/dipMex")
setwd(workdir)
#
#### IMPORT IDEAL POINTS ESTIMATED WITH ALL DATA
## 60th
load(file="ip60_5+5k.RData") 
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )
chains <- rbind(chains[[1]],chains[[2]],chains[[3]])
tmpx <-  chains[,grep(colnames(chains),pattern = "x")]
tmpy <-  chains[,grep(colnames(chains),pattern = "y")]
ips.x.60 <- rep(NA,times=ncol(tmpx)); ips.y.60 <- rep(NA,times=ncol(tmpx));
for (j in 1:ncol(tmpx)){
  ips.x.60[j] <- quantile (tmpx[,j], 0.5, names=F);
  ips.y.60[j] <- quantile (tmpx[,j], 0.5, names=F)
}
load(file="ip60_5+5k_gaceta.RData") 
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )
chains <- rbind(chains[[1]],chains[[2]],chains[[3]])
tmpx <-  chains[,grep(colnames(chains),pattern = "x")]
tmpy <-  chains[,grep(colnames(chains),pattern = "y")]
ips.g.x.60 <- rep(NA,times=ncol(tmpx)); ips.g.y.60 <- rep(NA,times=ncol(tmpx));
for (j in 1:ncol(tmpx)){
  ips.g.x.60[j] <- quantile (tmpx[,j], 0.5, names=F);
  ips.g.y.60[j] <- quantile (tmpy[,j], 0.5, names=F)
}


## 61st
load(file="ip61_2Dj_5+5k.RData") 
ips.x.61 <- results$median$x
ips.y.61 <- results$median$y
rm(ch,chains,color.list,CONVE,count.votes,dgaceta,dipdat,dmember,hi.v,i,I,ip.data,ip.inits,ip.parameters,j,J,lo.v,PAN,PANAL,part.list,PRD,PRI,PSD,PT,PVEM,rc,results,time.elapsed,tmp,tmpx,tmpy,votdat,vstar)
#
load(file="ip61_2Dj_5+5k_gaceta.RData") 
ips.g.x.61 <- results$median$x
ips.g.y.61 <- results$median$y
rm(color.list,CONVE,count.votes,dgaceta,dipdat,dmember,hi.v,i,I,ip.data,ip.inits,ip.parameters,j,J,lo.v,PAN,PANAL,part.list,PRD,PRI,PT,PVEM,rc,results,time.elapsed,tmp,votdat,vstar)
## NEEDS TO BE RE SET PROPERLY
#workdir <- c("/media/shared/01/Dropbox/data/rollcall/dipMex")
workdir <- c("~/Dropbox/data/rollcall/dipMex")
#workdir <- c("d:/01/Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/dipMex")


load("loops6k_60.RData")
#load("loops6k_61.RData")
workdir <- c("~/Dropbox/data/rollcall/dipMex")

resall <- list(res1 , res2 , res3 , res4 , res5 , res6 , res7 , res8 , res9 , res10, res11, res12, res13, res14, res15, res16, res17, res18, res19, res20, res21, res22, res23, res24, res25, res26, res27, res28, res29, res30, res31, res32, res33, res34, res35, res36, res37, res38, res39, res40, res41, res42, res43, res44, res45, res46, res47, res48, res49, res50, res51, res52, res53, res54, res55, res56, res57, res58, res59, res60, res61, res62, res63, res64, res65, res66, res67, res68, res69, res70, res71, res72, res73, res74, res75, res76, res77, res78, res79, res80, res81, res82, res83, res84, res85, res86, res87, res88, res89, res90, res91, res92, res93, res94, res95, res96, res97, res98, res99)

## point.x.all <- resall[[1]]$median$x
## point.x.allG <- resGall[[1]]$median$x
## point.y.all <- resall[[1]]$median$y
## point.y.allG <- resGall[[1]]$median$y

mi <- min(c(point.x.all, point.x.allG)); ma <- max(c(point.x.all, point.x.allG))
#par(mai=c(.4, .4, .4, .4)) ## SETS B L U R MARGIN SIZES
plot(c(mi,ma), c(mi,ma), type="n", 
#           xlab="", 
#           ylab="", 
#           xaxt="n",
#           yaxt="n",
           xlab=c("1st dim. All votes"), 
           ylab=c("1st dim. Gaceta votes only"), 
           main="60th Legislature")
#           main="")
abline(0,1)
points(point.x.all, point.x.allG, pch=19, cex=.75, col=dipdat$color)

mi <- min(c(point.y.all, point.y.allG)); ma <- max(c(point.y.all, point.y.allG))
#par(mai=c(.4, .4, .4, .4)) ## SETS B L U R MARGIN SIZES
plot(c(mi,ma), c(mi,ma), type="n", 
#           xlab="", 
#           ylab="", 
#           xaxt="n",
#           yaxt="n",
           xlab=c("2nd dim. All votes"), 
           ylab=c("2nd dim. Gaceta votes only"), 
           main="60th Legislature")
#           main="")
abline(0,1)
points(point.y.all, point.y.allG, pch=19, cex=.75, col=dipdat$color)
legend(1.8,-1.5, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")


## 1st DIMENSION
myplot <- function(X)
    {
    tmp0 <- rep(0, 99)  ## TO RECEIVE COUNT OF THETAS BEYOND 95% CIs 
    plot(c(1,99), c(-4,4), type="n", 
               xlab="Sample of Gaceta votes only",
               ylab="Deviation from theta",
               main=paste("1st dim.", dipdat$part[X], "-", dipdat$id[X]))
    abline(0,0)
    for (l in 1:99){
        tmp <- get(paste("resG", l, sep=""))
        tmp <- tmp[[1]]$sims.list$x[,X]             ## COORDENADAS DEL DIP j EN LOOP l
    #    points(rep(l,100), tmp-point.x.all[X], pch=19, cex=.25, col=dipdat$color[X])
        tmp2 <- quantile (tmp-point.x.all[X], 0.025, names=F)
        tmp3 <- quantile (tmp-point.x.all[X], 0.975, names=F)
        if (tmp3<0 | tmp2>0){tmp0[l] <- 1}
        lines(c(l,l), c(tmp2,tmp3), lwd=.25)#, col=dipdat$color[X])
        tmp2 <- quantile (tmp-point.x.all[X], 0.25, names=F)
        tmp3 <- quantile (tmp-point.x.all[X], 0.75, names=F)
        lines(c(l,l), c(tmp2,tmp3), lwd=1)#, col=dipdat$color[X])
        }
    text(80,4, labels=paste("N off limits:", sum(tmp0)))
    }

j <- 450
myplot(j)

setwd("d:/01/Dropbox/MexRollCalls/graphs/thetaVSsampleG")
for (j in 1:ncol(rc))
    {
    pdf( paste("dim1-", j, ".pdf", sep=""))
    myplot(j)
    dev.off()
    }
setwd(workdir)

## 2nd DIMENSION
myplot <- function(X)
    {
    tmp0 <- rep(0, 99)  ## TO RECEIVE COUNT OF THETAS BEYOND 95% CIs 
    plot(c(1,99), c(-4,4), type="n", 
               xlab="Sample of Gaceta votes only",
               ylab="Deviation from theta",
               main=paste("2nd dim.", dipdat$part[X], "-", dipdat$id[X]))
    abline(0,0)
    for (l in 1:99){
        tmp <- get(paste("resG", l, sep=""))
        tmp <- tmp[[1]]$sims.list$y[,X]             ## COORDENADAS DEL DIP j EN LOOP l
    #    points(rep(l,100), tmp-point.y.all[X], pch=19, cex=.25, col=dipdat$color[X])
        tmp2 <- quantile (tmp-point.y.all[X], 0.025, names=F)
        tmp3 <- quantile (tmp-point.y.all[X], 0.975, names=F)
        if (tmp3<0 | tmp2>0){tmp0[l] <- 1}
        lines(c(l,l), c(tmp2,tmp3), lwd=.25)#, col=dipdat$color[X])
        tmp2 <- quantile (tmp-point.y.all[X], 0.25, names=F)
        tmp3 <- quantile (tmp-point.y.all[X], 0.75, names=F)
        lines(c(l,l), c(tmp2,tmp3), lwd=1)#, col=dipdat$color[X])
        }
    text(80,4, labels=paste("N off limits:", sum(tmp0)))
    }

j <- 450
myplot(j)

setwd("d:/01/Dropbox/MexRollCalls/graphs/thetaVSsampleG")
for (j in 1:ncol(rc))
    {
    pdf( paste("dim2-", j, ".pdf", sep=""))
    myplot(j)
    dev.off()
    }
setwd(workdir)



## GRAPH TO REPORT
j <- 100
#setwd(paste(workdir, "/graphs/", sep=""))
#pdf(file="tmp.pdf", width=10, height=5)
plot(c(1,ncol(rc)), c(-7,7), type="n", 
#plot(c(1,150), c(-4,4), type="n", 
#           xlab="Rank-ordered deputies (by gaceta-only coordinate)",
#           ylab="Samples net of gaceta-only coordinate",
           xlab="Diputados ordenados por coordenada (sólo Gaceta)",
           ylab="Cambio de coordenada en la muestra",
#           ylab="Deviation from gaceta-only coordinate",
#           main=paste("1st dimension"))
           main=paste("1a dimensión"))
abline(0,0)
ord <- rank(ips.g.x.61) # point.x.all)                           ## FOR RANK-ORDERING
for (j in c(1:length(ord))[order(ord)]){
#for (j in c(1:ncol(rc))[order(ord)]){
#for (j in c(1:150)[order(ord)]){
    tmp0 <- res1[[1]]$sims.list$x[,j]             ## COORDENADAS DEL DIP j EN LOOP 1
    for (l in 2:99){
        tmp <- get(paste("res", l, sep=""))
        tmp <- tmp[[1]]$sims.list$x[,j]             ## COORDENADAS DEL DIP j EN LOOP l
        tmp0 <- c(tmp0, tmp)                        ## COORDENADAS DEL DIP j EN TODOS LOS LOOPS
        }
    tmp2 <- quantile (tmp0-ips.g.x.61[j], 0.025, names=F)
    tmp3 <- quantile (tmp0-ips.g.x.61[j], 0.975, names=F)
    lines(c(ord[j],ord[j]), c(tmp2,tmp3), lwd=.25, col=dipdat$color[j])
    tmp2 <- quantile (tmp0-ips.g.x.61[j], 0.25, names=F)
    tmp3 <- quantile (tmp0-ips.g.x.61[j], 0.75, names=F)
    lines(c(ord[j],ord[j]), c(tmp2,tmp3), lwd=1, col=dipdat$color[j])
    }
#dev.off()
setwd(workdir)

j <- 100
setwd(paste(workdir, "/graphs/", sep=""))
pdf(file="tmp.pdf", width=10, height=5)
#plot(c(1,ncol(rc)), c(-6,8), type="n", 
plot(c(1,ncol(rc)), c(-7,7), type="n", 
#plot(c(1,150), c(-7,7), type="n", 
#           xlab="Rank-ordered deputies (by gaceta-only coordinate)",
#           ylab="Samples net of gaceta-only coordinate",
           xlab="Diputados ordenados por coordenada (sólo Gaceta)",
           ylab="Cambio de coordenada en la muestra",
#           ylab="Deviation from gaceta-only coordinate",
#           main=paste("2nd dimension"))
           main=paste("2a dimensión"))
abline(0,0)
ord <- rank(ips.g.y.61) # point.y.all)                           ## FOR RANK-ORDERING
for (j in c(1:length(ord))[order(ord)]){
#for (j in c(1:150)[order(ord)]){
    tmp0 <- res1[[1]]$sims.list$y[,j]             ## COORDENADAS DEL DIP j EN LOOP 1
    for (l in 2:99){
        tmp <- get(paste("res", l, sep=""))
        tmp <- tmp[[1]]$sims.list$y[,j]             ## COORDENADAS DEL DIP j EN LOOP l
        tmp0 <- c(tmp0, tmp)                        ## COORDENADAS DEL DIP j EN TODOS LOS LOOPS
        }
    tmp2 <- quantile (tmp0-ips.g.y.61[j], 0.025, names=F)
    tmp3 <- quantile (tmp0-ips.g.y.61[j], 0.975, names=F)
    lines(c(ord[j],ord[j]), c(tmp2,tmp3), lwd=.25, col=dipdat$color[j])
    tmp2 <- quantile (tmp0-ips.g.y.61[j], 0.25, names=F)
    tmp3 <- quantile (tmp0-ips.g.y.61[j], 0.75, names=F)
    lines(c(ord[j],ord[j]), c(tmp2,tmp3), lwd=1, col=dipdat$color[j])
    }
dev.off()
setwd(workdir)



myplot <- function(X)
    {
    tmp0 <- rep(0, 99)  ## TO RECEIVE COUNT OF THETAS BEYOND 95% CIs 
    plot(c(1,99), c(-4,4), type="n", 
               xlab="Sample of Gaceta votes only",
               ylab="Deviation from theta",
               main=paste("1st dim.", dipdat$part[X], "-", dipdat$id[X]))
    abline(0,0)
    for (l in 1:99){
        tmp <- get(paste("resG", l, sep=""))
        tmp <- tmp[[1]]$sims.list$x[,X]             ## COORDENADAS DEL DIP j EN LOOP l
    #    points(rep(l,100), tmp-point.x.all[X], pch=19, cex=.25, col=dipdat$color[X])
        tmp2 <- quantile (tmp-point.x.all[X], 0.025, names=F)
        tmp3 <- quantile (tmp-point.x.all[X], 0.975, names=F)
        if (tmp3<0 | tmp2>0){tmp0[l] <- 1}
        lines(c(l,l), c(tmp2,tmp3), lwd=.25)#, col=dipdat$color[X])
        tmp2 <- quantile (tmp-point.x.all[X], 0.25, names=F)
        tmp3 <- quantile (tmp-point.x.all[X], 0.75, names=F)
        lines(c(l,l), c(tmp2,tmp3), lwd=1)#, col=dipdat$color[X])
        }
    text(80,4, labels=paste("N off limits:", sum(tmp0)))
    }

j <- 450
myplot(j)

