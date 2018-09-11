# Invoca WinBugs desde R

library("arm")
library ("MCMCpack")
library (foreign)
library (car)
library (gtools)

library (R2WinBUGS)
#library (R2jags)
#library(BRugs)

rm(list = ls())
##
#workdir <- c("~/Dropbox/data/rollcall/DipMex")
workdir <- c("d:/01/Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/dipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/dipMex")
setwd(workdir)
##
set.seed(1970) ## COMMENT IF DOING PERMUTATIONS / UNCOMMENT IF RUNNING CHAINS SEPARATELY

load(file="rc61.RData") 
dipdat$id <- as.character(dipdat$id)
dipdat$part <- as.character(dipdat$part)

## ADDS VOTE ID
votdat$votid <- 1:nrow(votdat) 

## DROP 2012 VOTES IF SO WISHED
#rc <- rc[votdat$yr<2012,1:1000]
#dgaceta <- dgaceta[votdat$yr<2012]
#votdat <- votdat[votdat$yr<2012,]
#dipdat <- dipdat[1:1000,]

### IMPORT DIPUTADO INFO (PREPARED IN EXCEL): MOST LIKELY REDUNDANT BEC rc61.RData HAS IT
#setwd(paste(workdir, "/diputados", sep=""))
#dipdat <- read.csv("dip61.csv", header=TRUE)
#setwd(workdir)

### PARA SABER QUIEN ES QUIEN IR AL ARCHIVO dip61.xls
#
coord <- rep("", times=1000)
coord[dipdat$id=="mexrp07p"] <- "Rojas"    ## pri
coord[dipdat$id=="mexrp01p"] <- "JVM"      ## pan1
coord[dipdat$id=="jal10p"] <- "R.Acuña"    ## pan2 (desde  6sep11)
coord[dipdat$id=="mexrp12p"] <- "P.Cuevas" ## pan3 (desde 24abr12)
coord[dipdat$id=="dfrp12p"] <- "Encinas"   ## prd1
coord[dipdat$id=="gue03p"] <- "R.Píter"    ## prd2
coord[dipdat$id=="coarp05p"] <- "Guajardo" ## prd3 (desde 31mar12)
coord[dipdat$id=="mexrp20p"] <- "Guerra"   ## pvem
coord[dipdat$id=="tabrp05p"] <- "J.León"   ## conve
coord[dipdat$id=="nlrp06p"] <- "Vzq.Glz"   ## pt
coord[dipdat$id=="dfrp25p"] <- "Kahwagi"   ## panal
#
dcoord <- rep(NA, times=1000)
dcoord[dipdat$id=="mexrp07p"] <- 1 ## pri
dcoord[dipdat$id=="mexrp01p"] <- 1 ## pan1
dcoord[dipdat$id=="jal10p"] <- 1   ## pan2
dcoord[dipdat$id=="mexrp12p"] <- 1 ## pan3
dcoord[dipdat$id=="dfrp12p"] <- 1  ## prd1
dcoord[dipdat$id=="gue03p"] <- 1   ## prd2
dcoord[dipdat$id=="coarp05p"] <- 1 ## prd3
dcoord[dipdat$id=="mexrp20p"] <- 1 ## pvem
dcoord[dipdat$id=="tabrp05p"] <- 1 ## conve
dcoord[dipdat$id=="nlrp06p"] <- 1  ## pt
dcoord[dipdat$id=="dfrp25p"] <- 1  ## panal
## 
color <- rep(".", times=1000) 
color[dipdat$part=="pan"] <- "darkblue" 
color[dipdat$part=="pri"] <- "forestgreen" 
color[dipdat$part=="prd"] <- "gold" 
color[dipdat$part=="pt"] <- "red" 
color[dipdat$part=="pvem"] <- "darkolivegreen2" 
color[dipdat$part=="conve"] <- "orange" 
color[dipdat$part=="panal"] <- "cyan" 
## 
part.list <- c("PAN", "PRI", "PRD", "PT", "PVEM", 
                "Conv.", "PANAL")
color.list <- c("darkblue", "forestgreen", "gold", 
                "red", "darkolivegreen2", "orange", 
                "cyan")
##
dipdat$color <- as.character(color)
dipdat$dcoord <- dcoord
dipdat$coord <- coord
rm(color, dcoord, coord)
##
## RECODE RCs TO -1=nay 0=abstain/absent 1=aye
rc <- apply(rc, 2, recode, recodes="2=-1; 3=0; 4=0; 5=0") 
##

## VOTE AGGREGATES FUNCTION
count.votes <- function (X)
{
   ayes <- length(X[is.na(X)==FALSE & X==1])
   nays <- length(X[is.na(X)==FALSE & X==-1])
   valid <- ayes+nays
   abs <- length(X[is.na(X)==FALSE & X==0])
   tot <- valid+abs
   categ <- c (ayes, nays, valid, abs, tot)
   return (categ)
}
##
names(votdat)[1:5] <- c("ayes","nays","valid","abs","tot") ## CHANGE VOTE STATS
I <- dim(rc)[1]; J <- dim(rc)[2]
for (i in 1:I){
    votdat$ayes[i] <- count.votes(rc[i,])[1]
    votdat$nays[i] <- count.votes(rc[i,])[2]
    votdat$valid[i] <- count.votes(rc[i,])[3]
    votdat$abs[i] <- count.votes(rc[i,])[4]
    votdat$tot[i] <- count.votes(rc[i,])[5]
              }
##

##
#for(i in 1:I){
#    print(colnames(rc)[i])
#    print(table(rc[,i]))
#    }

## PROPIETARIO/SUPLENTE/NEITHER WAS MEMBER
votdat$date <- as.Date(paste(votdat$yr[1:I],votdat$mo[1:I],votdat$dy[1:I],sep="-"))
## IF NEED TO SEE DATES; USE FOLLOWING COMMAND
#votdat$date <- format(votdat$date, format="%d %b %y")
#
dipdat$in1 <- rep(NA,J); dipdat$out1 <- rep(NA,J); dipdat$in2  <- rep(NA,J); dipdat$out2 <- rep(NA,J)
for (j in 1:J){
    dipdat$in1[j]  <- ifelse( dipdat$yrin1[j]=="." ,  NA, as.Date(paste(dipdat$yrin1[j],dipdat$moin1[j],dipdat$dyin1[j],sep="-")) )
    dipdat$out1[j] <- ifelse( dipdat$yrout1[j]==".",  NA, as.Date(paste(dipdat$yrout1[j],dipdat$moout1[j],dipdat$dyout1[j],sep="-")) )
    dipdat$in2[j]  <- ifelse( dipdat$yrin2[j]=="." ,  NA, as.Date(paste(dipdat$yrin2[j],dipdat$moin2[j],dipdat$dyin2[j],sep="-")) )
    dipdat$out2[j] <- ifelse( dipdat$yrout2[j]==".",  NA, as.Date(paste(dipdat$yrout2[j],dipdat$moout2[j],dipdat$dyout2[j],sep="-")) )
    }
## IF NEED TO SEE DATES; USE FOLLOWING COMMAND
#dipdat$in1  <- format(dipdat$in1 , format="%d %b %y")
#dipdat$out1 <- format(dipdat$out1, format="%d %b %y")
#dipdat$in2  <- format(dipdat$in2 , format="%d %b %y")
#dipdat$out2 <- format(dipdat$out2, format="%d %b %y")
#
dmember <- rc
dmember[,] <- NA
#
dmember[,is.na(dipdat$in1)==TRUE] <- 0 
for (i in 1:I){
    dmember[i,(votdat$date[i]< dipdat$in1  & is.na(dipdat$in1)==FALSE)] <- 0;
    dmember[i,(dipdat$in1<=votdat$date[i]  & is.na(dipdat$in1)==FALSE)] <- 1;
    dmember[i,(dipdat$out1<=votdat$date[i] & is.na(dipdat$out1)==FALSE)] <- 0;
    dmember[i,(dipdat$in2<=votdat$date[i]  & is.na(dipdat$in2)==FALSE)] <- 1;
    dmember[i,(dipdat$out2<=votdat$date[i] & is.na(dipdat$out2)==FALSE)] <- 0;
    }
rm(i,j)

## DEPUTY'S ABSTENTION RATE OVERALL (TO DROP THOSE NOT VOTING ENOUGH OVERALL)
noVoteRate <- as.numeric(rc[1,])
for (j in 1:J){
    noVoteRate[j] <- count.votes(rc[,j])[4] / count.votes(rc[,j])[5]
#    noVoteRate[j] <- length(rc[rc[,j]==0,d])/dim(rc)[1]
    }
dipdat$noVoteRate <- noVoteRate
rm(noVoteRate)
#
## DEPUTY'S ABSTENTION RATE WHEN CHAMBER MEMBER (FOR DESCRIPTIVE STATS)
noVoteRateMem <- as.numeric(rc[1,])
tmp <- rc*dmember  
## DMEMBER STILL HAS PROBLEMS, IN/OUT DATES DO NOT MATCH VOTES EXACTLY
j <- 75; cbind(dmember[,j], rc[,j]) ## AS THIS SHOWS FOR SOME js
for (j in 1:J){
    dmember[abs(rc[,j])==1,j] <- 1  ## IMPERFECT BUT PRACTICAL SOLUTION: IMPLY dmember IS ONE if rc IS -1 or 1
    }
ttmp <- as.numeric(rc[1,])
tmp <- rc*dmember  
for (j in 1:J){
    ttmp[j] <- sum(dmember[,j])
    noVoteRateMem[j] <- 1-count.votes(tmp[,j])[3] / sum(dmember[,j])
    }
dipdat$noVoteRateMem <- noVoteRateMem
rm(j, tmp, ttmp, noVoteRateMem)
##
## DROP DIPUTADOS WHO NEVER PLEDGED
dropUnpledged <- rep(0, times=J)
for (j in 1:J){
    dropUnpledged[j] <- ifelse(count.votes(rc[,j])[3]==0, -1, 0)
                 }
length(dropUnpledged[dropUnpledged<0])  ## HOW MANY NEVER PLEDGED (DESCRIPTIVES)
dropUnpledged <- dropUnpledged * (1:J)
dropUnpledged <- dropUnpledged[dropUnpledged<0] ## NEGATIVE INDEX FOR THOSE WHO NEVER PLEDGED
rc <- rc[,dropUnpledged]; dipdat <- dipdat[dropUnpledged,]; dmember <- dmember[,dropUnpledged]
tmp <- rc; tmp <- t(tmp); table(tmp) ## CHECA QUE TODO SEA -1 0 1
J <- dim(rc)[2]
##
## DROP DIPUTADOS WHO VOTED IN LESS THAN 10% OF ALL VOTES
D <- dim(dipdat)[1]
dropAbstainers <- rep(0, times=D)
for (d in 1:D){
    dropAbstainers[d] <- ifelse(dipdat$noVoteRate[d]>.9, -1, 0)
                 }
##
length(dropAbstainers[dropAbstainers<0])  ## HOW MANY HAD TOO FEW VOTES (DESCRIPTIVES)
dipdat$id[dropAbstainers<0]               ## WHO THEY ARE (MOSTLY SUPLENTES)
##
dropAbstainers <- dropAbstainers * (1:D)
dropAbstainers <- dropAbstainers[dropAbstainers<0] ## NEGATIVE INDEX FOR THOSE WHO NEVER VOTED
rc <- rc[,dropAbstainers]; dipdat <- dipdat[dropAbstainers,]
rm(d, D, dropAbstainers, dropUnpledged)
##
## DROP UNCONTESTED VOTES
I <- dim(rc)[1]
dropUncontested <- ifelse(votdat$ayes==0 | votdat$nays==0, -1, 0)
##
length(dropUncontested[dropUncontested<0])  ## HOW MANY UNCONTESTED VOTES (DESCRIPTIVES)
##
dropUncontested <- dropUncontested * (1:I)
dropUncontested <- dropUncontested[dropUncontested<0] ## NEGATIVE INDEX 
rc <- rc[dropUncontested,]
votdat <- votdat[dropUncontested,]
dgaceta <- dgaceta[dropUncontested]
rm(j, dropUncontested, tmp)
##
## DROP UNIFORMATIVE VOTES WITH MINORITY < 2.5%
I <- dim(rc)[1]
dropUninformative <- ifelse( votdat$ayes/votdat$valid<.025 | votdat$nays/votdat$valid<.025, -1, 0)
##
length(dropUninformative[dropUninformative<0])  ## HOW MANY UNINFORMATIVE VOTES (DESCRIPTIVES)
##
dropUninformative <- dropUninformative * (1:I)
dropUninformative <- dropUninformative[dropUninformative<0] ## NEGATIVE INDEX 
rc <- rc[dropUninformative,]
votdat <- votdat[dropUninformative,]
dgaceta <- dgaceta[dropUninformative]
rm(dropUninformative)

#### DROP VOTES NOT IN GACETA IF SO WISHED
##rc <- rc[dgaceta==1,]
##votdat <- votdat[dgaceta==1,]
####
#### WORK WITH SAMPLE OF VOTES IF SO WISHED
##rnd <- runif(dim(rc)[1])
##rc <- rc[rnd<.2,]
##votdat <- votdat[rnd<.2,]
##rm(rnd)
##
#### WORK WITH SAMPLE OF DIPUTADOS IF SO WISHED
##rnd <- runif(dim(rc)[2])
##rc <- rc[,rnd<.25]
##dipdat <- dipdat[rnd<.25,]
##rm(rnd)
####
J <- ncol(rc); I <- nrow(rc)

## ## ONE-DIM ARRANGEMENT
## ## AGREEMENT MATRIX --- LA GUARDE PORQUE ESTO TARDA AÑOS
## load("agreeMatrix.Rdata")
## #agreeMatrix <- matrix(NA, ncol=J, nrow=J); tmp <- rep(NA, times=I)
## #datt <- t(dat)
## #for (j in 1:J){
## #    agreeMatrix[j,j] <- 1  ## DIAGONAL
## #              }
## #for (j1 in 2:J){
## #    for (j2 in (j1-1):1){
## #        for (i in 1:I){
## #            tmp[i] <- ifelse(datt[i,j1]==datt[i,j2], 1, 0)
## #                      }
## #        agreeMatrix[j2,j1] <- sum(tmp)/I; agreeMatrix[j1,j2] <- agreeMatrix[j2,j1]
## #        print( paste("j1 =",j1,"; j2 =",j2) )
## #                        }
## #               } 
## #rm(datt)
## #save(agreeMatrix, file="agreeMatrix.Rdata")
## ## SQUARED DISTANCES
## sd <- (1-agreeMatrix)^2
## ## DOUBLE-CENTRED MATRIX
## pmean <- rep(NA, times=J); mmat <- mean(sd); dc <- sd
## for (j in 1:J){
##     pmean[j] <- mean(sd[j,])
##               }
## for (r in 1:J){
##     for (c in 1:J){
##         dc[r,c] <- (sd[r,c] - pmean[r] - pmean[c] + mmat)/-2
##                   }
##               }
## ## SIMPLE ONE-DIM IDEAL POINTS
## tmp <- sqrt(dc[1,1])
## ip  <- c(tmp, dc[2:J,1]/tmp)
## summary(ip)
## ##
## ## EXTREMA DERECHA
## thr <- .14
## data.frame(ip=ip[c(1:J)[ip>thr]], id=dipdat$id[c(1:J)[ip>thr]], nom=dipdat$nom[c(1:J)[ip>thr]], part=dipdat$part[c(1:J)[ip>thr]], noVote=dipdat$noVoteRate[c(1:J)[ip>thr]])
## ##EXTREMA IZQUIERDA
## thr <- -.185
## data.frame(ip=ip[c(1:J)[ip<thr]], id=dipdat$id[c(1:J)[ip<thr]], nom=dipdat$nom[c(1:J)[ip<thr]], part=dipdat$part[c(1:J)[ip<thr]], noVote=dipdat$noVoteRate[c(1:J)[ip< thr]])
## ##
## tmp <- dat[,dimnames(dat)[[2]]=="dpanid" | dimnames(dat)[[2]]=="dpriid" | dimnames(dat)[[2]]=="dprdid"]
## tmp[,4] <- 1 - tmp[,1] - tmp[,2] - tmp[,3]
## dimnames(tmp)[[2]][4] <- "dothid"
## tmp[,2] <- tmp[,2]*2; tmp[,3] <- tmp[,3]*3; tmp[,4] <- tmp[,4]*4
## part <- as.vector(apply(tmp, 1, sum))
## #
## rnd <- (runif(dim(dat)[1])-.5)/10 ## random jitter
## plot(c(-.5,.3), c(1,4), type="n")
## for (j in 1:J){
##     points(ip[j], part[j]+rnd[j], pch=20, cex=.5, col=envud$color[j])
##     }
## # 
## ##################################################
## ### FACTOR ANALYSIS TO ESTIMATE DIMENSIONALITY ###
## ##################################################
## #
## ### ALLOWS TO DROP CASES FROM ANALYSIS
## #year1 <- ifelse( (cuad==1 | cuad==2 | cuad==3), 1, 0 ) ## FALTA QUITAR A UN DIPUTADO (M?S?) QUE NO ENTR? HASTA DESPU?S
## #drop <- ifelse(year1==0,1,0)
## #tmp<-RCs[drop==0,]
## #RCs<-tmp
## #sem<-sem[drop==0]; cuad<-cuad[drop==0]; trim<-trim[drop==0]
## #
## #votes <- rc
## #votes[votes==-1] <- 0  # los -1s se vuelven 0s # DEJA ABSTENCION == NAY  
## #votes <- t(votes)
## #votes <- votes[,1:50] ## subset to test
## #
## cor(dat)
## factanal(dat, factors=4) # varimax is the default
## #
## factanal(dat, factors=3, rotation="promax")
## #
## # A little demonstration, v2 is just v1 with noise,
## # and same for v4 vs. v3 and v6 vs. v5
## # Last four cases are there to add noise
## # and introduce a positive manifold (g factor)
## v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
## v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
## v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
## v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
## v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
## v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
## m1 <- cbind(v1,v2,v3,v4,v5,v6)
## cor(m1)
## factanal(m1, factors=3) # varimax is the default
## factanal(m1, factors=3, rotation="promax")
## # The following shows the g factor as PC1
## prcomp(m1)
## #
## ## formula interface
## factanal(~v1+v2+v3+v4+v5+v6, factors = 3,
##         scores = "Bartlett")$scores
## #
## ## a realistic example from Barthlomew (1987, pp. 61-65)
## example(ability.cov)


#####################################################################################
###   Static 2Dimensions, arbitrary three party-anchors, irt paremeterization     ###
#####################################################################################

## NEEDED TO RUN IN JAGS INSTEAD OF BUGS (ABSTENTIONS AND ABSENCES AS MISSING)
#rc <- apply(rc, 2, recode, recodes="-1=0; 0=NA") 

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse ( dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000,
           ifelse ( dipdat$part=="pvem", tmp+4000,
            ifelse ( dipdat$part=="conve", tmp+5000,
             ifelse ( dipdat$part=="panal", tmp+6000, tmp+7000 )))))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])
PVEM <- PT+length(tmp[tmp>4000&tmp<5001]); CONVE <- PVEM+length(tmp[tmp>5000&tmp<6001])
PANAL <- CONVE+length(tmp[tmp>6000&tmp<7001])
#
PAN; PRI; PRD; PVEM; CONVE

## ## JAGS VERSION
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
##     y[j] ~  dnorm(1, 4)
##     }
## for (j in (PRI+1):PRD){
##     x[j] ~  dnorm(-1, 4)    # PRD
##     y[j] ~  dnorm(-1, 4)
##     }
## for (j in (PRD+1):PT){
##     x[j] ~  dnorm(-1, 4)    # PT
##     y[j] ~  dnorm(-1, 4)
##     }
## for (j in (PT+1):PVEM){
##     x[j] ~  dnorm(0, 4)    # PVEM
##     y[j] ~  dnorm(1, 4)
##     }
## for (j in (PVEM+1):J){
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

## BUGS VERSION
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
    y[j] ~  dnorm(-1, 4)
    }
for (j in (PRD+1):J){
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

#####################################################################
##### static model in Two Dimensions WITH CUTLINE ESTIMATES 
#####################################################################
##cat("
##model {
##  for (j in 1:J){                ## loop over diputados
##    for (i in 1:I){              ## loop over items
##      #y.hat[j,i] ~ dbern(p[j,i]);                              ## voting rule
##      #p[j,i] <- phi(v.star[j,i]);                              ## sets 0<p<1
##      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);    ## truncated normal sampling
##      mu[j,i] <- d[i]*a[i]*x[j] + d[i]*b[i] - d[i]*y[j]        ## utility differential
##                  }
##                }
##  for (i in 1:I){
##  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
##  }
##  ## priors ################
##for (j in 1:PAN){
##    x[j] ~  dnorm(1, 4)   # Mrs. Vázques Mota Josefina (coord PAN)
##    y[j] ~  dnorm(1, 4)
##    }
##for (j in (PAN+1):PRI){
##    x[j] ~  dnorm(.5, 4)    # Mr.Rojas G Francisco (coord PRI)
##    y[j] ~  dnorm(-1, 4)
##    }
##for (j in (PRI+1):PRD){
##    x[j] ~  dnorm(-.5, 4)    # Mr. Encinas Alejandro (coord PRD til 13apr2011)
##    y[j] ~  dnorm(.5, 4)
##    }
##for (j in (PRD+1):PT){
##    x[j] ~  dnorm(-1, 4)    # Mr. Vázquez González (coord PT)
##    y[j] ~  dnorm(1, 4)
##    }
##for (j in (PT+1):J){
##    x[j] ~  dnorm(0, .1)
##    y[j] ~  dnorm(0, .1)
##    }
##    for(i in 1:I){
##        d[i] ~ dnorm( 0, 0.1)
##        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
##        b[i] ~ dnorm( 0, .1)
##                 }
##}
##", file="modelSta2Dj.txt")
###
####################################################
###### static model in 1 dimension, item anchors
####################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over councilors  
###    for (i in 1:I){              ## loop over items
###     #y.hat[j,i] ~ dbern(p[j,i])                                   ## voting rule
###     #p[j,i] <- phi(y.star[j,i])                                   ## sets 0<p<1
###     y.star[j,i] ~ dnorm(mu[j,i],1)I(lower.y[j,i],upper.y[j,i])   ## truncated normal sampling
###     mu[j,i] <- delta[i]*x[j] - n[i]                              ## utility differential
###     }
###  }
###  for (i in 1:I){
###     m[i] <- n[i] / delta[i]                                      ## midpoint 
###  } 
###  ## priors
###     for (j in 1:J){
###         x[j] ~ dnorm(0, .1)
###                   }
###    for(i in 1:31){
###        delta[i] ~ dnorm( 0, 0.25)
###                  }
###    delta[32] ~ dnorm( 4, 4)      ## folio 390, right=nay
###    for(i in 33:227){
###        delta[i] ~ dnorm( 0, 0.25)
###                   }
###    delta[228] ~ dnorm(-4, 4)      ## folio 1045, right=aye 
###    for(i in 229:I){
###        delta[i] ~ dnorm( 0, 0.25)
###                  }
###    for(i in 1:I){
###        n[i] ~ dnorm( 0, 0.25)
###                 }
###}
###", file="modelSta1Dj.txt")
### #
###### dynamic model for 66 members in Two Dimensions WITH CUTLINE ESTIMATES
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling, cf. Jackman
###      mu[j,i] <- delta[i]*a[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i]) 
###                + delta[i]*b[i] - delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
###                  }
###      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
###      xTwo[j] ~   dnorm (xOne[j],15);
###      xThree[j] ~ dnorm (xTwo[j],15);
###      xFour[j] ~  dnorm (xThree[j],15);
###      xFive[j] ~  dnorm (xFour[j],15);
###      xSix[j] ~   dnorm (xFive[j],15);
###      xSeven[j] ~ dnorm (xSix[j],15);
###      xEight[j] ~ dnorm (xSeven[j],15);
###      yOne[j] ~   dnorm (yZero[j],15);
###      yTwo[j] ~   dnorm (yOne[j],15);
###      yThree[j] ~ dnorm (yTwo[j],15);
###      yFour[j] ~  dnorm (yThree[j],15);
###      yFive[j] ~  dnorm (yFour[j],15);
###      ySix[j] ~   dnorm (yFive[j],15);
###      ySeven[j] ~ dnorm (ySix[j],15);
###      yEight[j] ~ dnorm (ySeven[j],15);
###                }
###  for (i in 1:I){
###  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
###  }
###  ################
###  ## priors
###  ################
###for (j in 1:(N-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[N] ~  dnorm(0, 4)    # Mrs. NORTH Piña Olmedo Laura (PRD)
###    yZero[N] ~  dnorm(2, 4)    
####    xZero[N] <- 0
####    yZero[N] <- 2
###for (j in (N+1):(W-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[W] ~  dnorm(-2, 4)   # Mr. WEST Méndez Rangel Avelino (PRD)
###    yZero[W] ~  dnorm(0, 4)    
####    xZero[W] <- -2
####    yZero[W] <- 0
###for (j in (W+1):(E-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[E] ~  dnorm(2, 4)    # Mrs. EAST Paula Adriana Soto Maldonado (PAN)
###    yZero[E] ~  dnorm(0, 4)    
####    xZero[E] <- 2
####    yZero[E] <- 0
###for (j in (E+1):(S-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[S] ~  dnorm(0, 4)    # Mr. SOUTH Tenorio Antiga Xiuh (PANAL)
###    yZero[S] ~  dnorm(-2, 4)    
####    xZero[S] <- 0
####    yZero[S] <- -2
###for (j in (S+1):J){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    for(i in 1:I){
###        delta[i] ~ dnorm( 0, 0.01)
###        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###        b[i] ~ dnorm( 0, .01)
###                 }
###}
###", file="model66Dyn2Dj.txt")
####
####
######################################################################
###### static model for 66 members in Two Dimensions, four ITEM anchors WITH CUTLINE ESTIMATES
######################################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
###                  }
###                }
###  for (i in 1:I){
###  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
###  }
###  ## priors ################
###for (j in 1:J){
###    x[j] ~  dnorm(0, 1)
###    y[j] ~  dnorm(0, 1)
###    }
###for(i in 1:(V1-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[V1] ~ dnorm( -4, 4)
###angle[V1] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
###b[V1] ~ dnorm( 0, 4)
###for(i in (V1+1):(H1-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[H1] ~ dnorm( -4, 4)
###angle[H1] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
###b[H1] ~ dnorm( 0, 4)
###for(i in (H1+1):(V2-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[V2] ~ dnorm( 4, 4)
###angle[V2] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
###b[V2] ~ dnorm( 0, 4)
###for(i in (V2+1):(H2-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[H2] ~ dnorm( -4, 4)
###angle[H2] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
###b[H2] ~ dnorm( 0, 4)
###for(i in (H2+1):I){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###}
###", file="model66Sta2Di4.txt")
####
######################################################################
###### static model for 66 members in Two Dimensions, two ITEM anchors WITH CUTLINE ESTIMATES
######################################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- delta[i]*a[i]*x[j] + delta[i]*b[i] - delta[i]*y[j] ## utility differential
###                  }
###                }
###  for (i in 1:I){
###  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
###  }
###  ## priors ################
###for (j in 1:J){
###    x[j] ~  dnorm(0, 1)
###    y[j] ~  dnorm(0, 1)
###    }
###for(i in 1:(V1-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[V1] ~ dnorm( -4, 4)
###angle[V1] ~ dunif(1.37,1.77) # (7pi/16,9pi/16) ---- VERTICAL
###b[V1] ~ dnorm( 0, 4)
###for(i in (V1+1):(H1-1)){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###delta[H1] ~ dnorm( -4, 4)
###angle[H1] ~ dunif(-0.2,0.2) # (-pi/16,pi/16) ------ HORIZONTAL
###b[H1] ~ dnorm( 0, 4)
###for(i in (H1+1):I){
###    delta[i] ~ dnorm( 0, 0.25)
####    angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
###    angle[i] ~ dunif(.392,1.178) # (pi/8,3pi/8)
###    b[i] ~ dnorm( 0, .25)
###    }
###}
###", file="model66Sta2Di2.txt")
####
###### dynamic model for 66 members in Two Dimensions -- IRT PARAMETERIZATION
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i])
###                - alpha[i] + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
###                  }
###      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
###      xTwo[j] ~   dnorm (xOne[j],15);
###      xThree[j] ~ dnorm (xTwo[j],15);
###      xFour[j] ~  dnorm (xThree[j],15);
###      xFive[j] ~  dnorm (xFour[j],15);
###      xSix[j] ~   dnorm (xFive[j],15);
###      xSeven[j] ~ dnorm (xSix[j],15);
###      xEight[j] ~ dnorm (xSeven[j],15);
###      yOne[j] ~   dnorm (yZero[j],15);
###      yTwo[j] ~   dnorm (yOne[j],15);
###      yThree[j] ~ dnorm (yTwo[j],15);
###      yFour[j] ~  dnorm (yThree[j],15);
###      yFive[j] ~  dnorm (yFour[j],15);
###      ySix[j] ~   dnorm (yFive[j],15);
###      ySeven[j] ~ dnorm (ySix[j],15);
###      yEight[j] ~ dnorm (ySeven[j],15);
###                }
##### ESTO LO PUEDO SACAR POST ESTIMACION
#####  for (i in 1:I){
#####  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
#####  b[i] <- alpha[i] / delta[i] ## constante de cutline
#####  }
###  ################
###  ## priors
###  ################
##### 1a dim ############
###for (j in 1:(N-1)){
###    xZero[j] ~  dnorm(0, 1)
###    }
###    xZero[N] ~  dnorm(0, 4)    # Mrs. NORTH Piña Olmedo Laura (PRD)
####    xZero[N] <- 0
###for (j in (N+1):(W-1)){
###    xZero[j] ~  dnorm(0, 1)
###    }
###    xZero[W] ~  dnorm(-2, 4)    # Mr. WEST Méndez Rangel Avelino (PRD)
####    xZero[W] <- -2
###for (j in (W+1):(E-1)){
###    xZero[j] ~  dnorm(0, 1)
###    }
###    xZero[E] ~  dnorm(2, 4)    # Mrs. EAST Paula Adriana Soto Maldonado (PAN)
####    xZero[E] <- 2
###for (j in (E+1):(S-1)){
###    xZero[j] ~  dnorm(0, 1)
###    }
###    xZero[S] ~  dnorm(0, 4)    # Mr. SOUTH Tenorio Antiga Xiuh (PANAL)
####    xZero[S] <- 0
###for (j in (S+1):J){
###    xZero[j] ~  dnorm(0, 1)
###    }
##### 2a dim  ############
###for (j in 1:(N-1)){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    yZero[N] ~  dnorm(2, 4)    # Mrs. NORTH
####    yZero[N] <- 2
###for (j in (N+1):(W-1)){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    yZero[W] ~  dnorm(0, 4)    # Mr. WEST
####    yZero[W] <- 0
###for (j in (W+1):(E-1)){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    yZero[E] ~  dnorm(0, 4)    # Mrs. EAST
####    yZero[E] <- 0
###for (j in (E+1):(S-1)){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    yZero[S] ~  dnorm(-2, 4)    # Mr. SOUTH
####    yZero[S] <- -2
###for (j in (S+1):J){
###    yZero[j] ~  dnorm(0, 1)
###    }
###    for(i in 1:I){
###        alpha[i] ~ dnorm( 0, 1)
###        beta[i]  ~ dnorm( 0, 1)
###        delta[i] ~ dnorm( 0, 1)
###                 }
###}
###", file="model66Dyn2Dj.irt.txt")
####
####
######################################################################
###### static model for 66 members in Two Dimensions, four ITEM anchors -- IRT PARAMETERIZATION
######################################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j] ## utility differential
###                  }
###                }
##### ESTO LO PUEDO SACAR POST ESTIMACION
#####  for (i in 1:I){
#####  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
#####  b[i] <- alpha[i] / delta[i] ## constante de cutline
#####  }
###  ## priors ################
###for (j in 1:J){
###    x[j] ~  dnorm(0, 1)
###    y[j] ~  dnorm(0, 1)
###    }
###for(i in 1:(V1-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[V1] ~ dnorm( 0, 1)
###beta[V1]  ~ dnorm(-4, 20)
###delta[V1] ~ dnorm(-4, 20)
###for(i in (V1+1):(H1-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[H1] ~ dnorm( 0, 1)
###beta[H1]  ~ dnorm( 4, 20)
###delta[H1] ~ dnorm(-4, 20)
###for(i in (H1+1):(V2-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[V2] ~ dnorm( 0, 1)
###beta[V2]  ~ dnorm( 4, 20)
###delta[V2] ~ dnorm( 4, 20)
###for(i in (V2+1):(H2-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[H2] ~ dnorm( 0, 1)
###beta[H2]  ~ dnorm( 4, 20)
###delta[H2] ~ dnorm(-4, 20)
###for(i in (H2+1):I){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###}
###", file="model66Sta2Di4.irt.txt")
####
##########################################################################################
###### dynamic model for 66 members in Two Dimensions Four Item anchors-- IRT PARAMETERIZATION
##########################################################################################
###cat("
###model {
###  for (j in 1:J){                ## loop over diputados
###    for (i in 1:I){              ## loop over items
###      #v.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
###      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
###      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling
###      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] 
###                 + xFour[j]*d4[i] + xFive[j]*d5[i] + xSix[j]*d6[i] + xSeven[j]*d7[i] 
###                 + xEight[j]*d8[i])
###                 - alpha[i] + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] 
###                 + yFour[j]*d4[i] + yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] 
###                 + yEight[j]*d8[i])  ## utility differential
###                  }
###      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
###      xTwo[j] ~   dnorm (xOne[j],15);
###      xThree[j] ~ dnorm (xTwo[j],15);
###      xFour[j] ~  dnorm (xThree[j],15);
###      xFive[j] ~  dnorm (xFour[j],15);
###      xSix[j] ~   dnorm (xFive[j],15);
###      xSeven[j] ~ dnorm (xSix[j],15);
###      xEight[j] ~ dnorm (xSeven[j],15);
###      yOne[j] ~   dnorm (yZero[j],15);
###      yTwo[j] ~   dnorm (yOne[j],15);
###      yThree[j] ~ dnorm (yTwo[j],15);
###      yFour[j] ~  dnorm (yThree[j],15);
###      yFive[j] ~  dnorm (yFour[j],15);
###      ySix[j] ~   dnorm (yFive[j],15);
###      ySeven[j] ~ dnorm (ySix[j],15);
###      yEight[j] ~ dnorm (ySeven[j],15);
###                }
##### ESTO LO PUEDO SACAR POST ESTIMACION
#####  for (i in 1:I){
#####  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
#####  b[i] <- alpha[i] / delta[i] ## constante de cutline
#####  }
###  ################
###  ## priors
###  ################
##### 1a dim ############
###for (j in 1:(N-1)){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###    xZero[N] ~  dnorm(-2, 4)    # Mrs. NORTH Piña Olmedo Laura (PRD)
###    yZero[N] ~  dnorm(2, 4)    
####    xZero[N] <- 0
####    yZero[N] <- 2
###for (j in (N+1):J){
###    xZero[j] ~  dnorm(0, 1)
###    yZero[j] ~  dnorm(0, 1)
###    }
###for(i in 1:(V1-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[V1] ~ dnorm( 0, 1)
###beta[V1]  ~ dnorm(-4, 20)
###delta[V1] ~ dnorm(-4, 20)
###for(i in (V1+1):(H1-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[H1] ~ dnorm( 0, 1)
###beta[H1]  ~ dnorm( 4, 20)
###delta[H1] ~ dnorm(-4, 20)
###for(i in (H1+1):(V2-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[V2] ~ dnorm( 0, 1)
###beta[V2]  ~ dnorm( 4, 20)
###delta[V2] ~ dnorm( 4, 20)
###for(i in (V2+1):(H2-1)){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###alpha[H2] ~ dnorm( 0, 1)
###beta[H2]  ~ dnorm( 4, 20)
###delta[H2] ~ dnorm(-4, 20)
###for(i in (H2+1):I){
###    alpha[i] ~ dnorm( 0, 1)
###    beta[i]  ~ dnorm( 0, 1)
###    delta[i] ~ dnorm( 0, 1)
###    }
###}
###", file="model66Dyn2Di4.irt.txt")
####
############################################################################################

## ## JAGS VERSION
## v <- t(rc)
## J <- nrow(v); I <- ncol(v)
## ip.data <- list ("J", "I", "v", "PAN", "PRI", "PRD", "PT", "PVEM")
## ip.inits <- function (){
##     list (
## #    v=rbern(J*I),  
##     delta=rnorm(I),
##     alpha=rnorm(I),
##     beta=rnorm(I),
##     x=rnorm(J),
##     y=rnorm(J)
##     )
##     }
## ip.parameters <- c("delta","beta", "alpha", "x", "y")#, "deviance")

## BUGS VERSION
v <- rc
v <- t(v)
J <- nrow(v); I <- ncol(v)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
ip.data <- list ("J", "I", "lo.v", "hi.v", "PAN", "PRI", "PRD")
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

#test ride to see program works
start.time <- proc.time()
results <- bugs (ip.data, ip.inits, ip.parameters, 
                "model2Dj.irt.txt", n.chains=2, 
                n.iter=10, n.thin=1, debug=T,
#                bugs.seed = ch*10000,
                bugs.directory = "C:/Program Files (x86)/WinBUGS14",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
#                bugs.directory = "~/.wine/dosdevices/c:/Program Files (x86)/WinBUGS14",
                program = c("WinBUGS")
                 )
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")); rm(time.elapsed)

#longer run
start.time <- proc.time()
results <- bugs (ip.data, ip.inits, ip.parameters, 
                "model2Dj.irt.txt", n.chains=2, 
                n.iter=6000, n.burnin=4000, n.thin=20, debug=F,
#                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS")
                )
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
print(cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")); rm(time.elapsed)

plot(results)
print(results)

#to continue running
tmp1<-list (
    v.star=vstar,
    delta=results$last.values[[1]]$delta,
    angle=results$last.values[[1]]$angle,
    b=results$last.values[[1]]$b,
    x=results$last.values[[1]]$x,
    y=results$last.values[[1]]$y
    )
tmp2<-list (
    v.star=vstar,
    delta=dip.61$last.values[[2]]$delta,
    angle=dip.61$last.values[[2]]$angle,
    b=dip.61$last.values[[2]]$b,
    x=dip.61$last.values[[2]]$x,
    y=dip.61$last.values[[2]]$y
    )
tmp3<-list (
    v.star=vstar,
    delta=dip.61$last.values[[3]]$delta,
    angle=dip.61$last.values[[3]]$angle,
    b=dip.61$last.values[[3]]$b,
    x=dip.61$last.values[[3]]$x,
    y=dip.61$last.values[[3]]$y
    )
### for (chain in 1:3){dip.61$last.values[[chain]]$v.star <- vstar}
dip.61.2 <- bugs (dip.data, 
                inits=list(tmp1,tmp2,tmp3), 
                dip.parameters, 
#                "modelSta2Dj.irt.txt", n.chains=3, 
                "modelSta2Dj.txt", n.chains=3, 
                n.iter=2000, n.thin=10, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

## change .ch number appropriately for each chain
results.3 <- results
save(results.3, file="tmp3.RData")
#rm(results)

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED results.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
## results <- results.1 ## so that it inherits mcmc.list attribute
## results[[2]] <- results.2[[1]]
## results[[3]] <- results.3[[1]]
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )

## 
load(paste(workdir, "/ip61_2Dj.RData", sep=""))
results <- ip2Dj

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
post.x     <- post.draw[,grep("x", colnames(post.draw))]
post.y     <- post.draw[,grep("y", colnames(post.draw))]
post.alpha <- post.draw[,grep("alpha", colnames(post.draw))]
post.beta  <- post.draw[,grep("beta", colnames(post.draw))]
post.delta <- post.draw[,grep("delta", colnames(post.draw))]

## SACA CONSTANTE Y PENDIENTE DE CUTLINES
post.a <- post.beta / post.delta  ## pendiente de cutline
post.b <- post.alpha / post.delta ## constante de cutline
post.d <- -post.delta                  ## término discriminante

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

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }

## dipdat$color[dipdat$color=="."] <- "black"
##plot(c(min(jotas[,2]),max(jotas[,2])), c(min(jotas[,5]),max(jotas[,5])), type="n")
plot(c(-3,2), c(-2.5,2.5), type="n", 
           xlab="", 
           ylab="", 
#           xlab=c("dim2_all"), 
#           ylab=c("dim2_gaceta"), 
           main="61st Leg. 2009-11")
##abline(0,1)
points(jotas[,2],jotas[,5], pch=19, cex=.75, col=dipdat$color)
##points(jotas[,5],jotas.gac[,5], pch=19, cex=.75, col=dipdat$color)

## cutlines
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


### Exporta coordenadas de todos los diputados
tmp <- matrix(NA, nrow=67, ncol=4)
tmp[,1] <- as.numeric(jotas[,2])
tmp[,2] <- jotas[,5]
tmp[,3] <- names.67
tmp[,4] <- part.67
tmp<-data.matrix(tmp)
tmp[,1:2] <- as.numeric(tmp[,1:2])
write.table(tmp, file="aldfStaticIdPts.xls", sep=",")

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

# For use in rollrates analysis
postNorthprd <- rep(0,67)
postSouthprd <- rep(0,67)
for (j in 1:67){
    postNorthprd[j] <- ifelse (jotas[j,5]>=0 & part.67[j]=="prd", 1, 0) 
    postSouthprd[j] <- ifelse (jotas[j,5]<0 & part.67[j]=="prd", 1, 0) 
    }

amed <- rep(NA,times=I)
bmed <- rep(NA,times=I)
for (i in 1:I){
    amed[i] <- quantile (a[,i], 0.50, names=F)
    bmed[i] <- quantile (b[,i], 0.50, names=F)  }

### TO RESET GRAPH PARAMETERS SAY par(oldpar) ###
oldpar <- par(no.readonly=TRUE)

#par(mfrow=c(3,3))
#par("pin" = c(.63,.58)) #width and height of plot region in inches

## FUNCTION TO DRAW ELLIPSES OVOIDS
ellipsePoints <- function(a,b, alpha = 0, loc = c(0,0), n = 201)
{
    ## Purpose: ellipse points,radially equispaced, given geometric par.s
    ## -------------------------------------------------------------------------
    ## Arguments: a, b : length of half axes in (x,y) direction
    ##            alpha: angle (in degrees) for rotation
    ##            loc  : center of ellipse
    ##            n    : number of points
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 19 Mar 2002, 16:26
    B <- min(a,b)
    A <- max(a,b)
    ## B <= A
    d2 <- (A-B)*(A+B)                   #= A^2 - B^2
    phi <- 2*pi*seq(0,1, len = n)
    sp <- sin(phi)
    cp <- cos(phi)
    r <- a*b / sqrt(B^2 + d2 * sp^2)
    xy <- r * cbind(cp, sp)
    ## xy are the ellipse points for alpha = 0 and loc = (0,0)
    al <- alpha * pi/180
    ca <- cos(al)
    sa <- sin(al)
    xy %*% rbind(c(ca, sa), c(-sa, ca)) + cbind(rep(loc[1],n),
                                                rep(loc[2],n))
}

tmp <- c(jotas[,1],jotas[,4])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
min <- min( tmp )
tmp <- c(jotas[,3],jotas[,6])
for (i in 1:length(tmp)) { tmp[i] <- ifelse(is.na(tmp[i])==TRUE,0,tmp[i]) }
max <- max( tmp )
lims <- c(NA,NA)
lims[1] <- ifelse(abs(min)>max, min, -max)
lims[2] <- ifelse(abs(min)>max, abs(min), max)

#### FOR USE IF ELLIPSES WILL BE GRAPHED
##eps <- array(NA, dim=c(201,2,67))
##eps[,,1] <- ellipsePoints(a=jotas[t,2,1]-jotas[t,1,1],b=jotas[t,5,1]-jotas[t,4,1],alpha=0,loc=c(jotas[t,2,1],jotas[t,5,1]),n=201)
##eps[,,2] <- ellipsePoints(a=jotas[t,2,2]-jotas[t,1,2],b=jotas[t,5,2]-jotas[t,4,2],alpha=0,loc=c(jotas[t,2,2],jotas[t,5,2]),n=201)
##etcetera

par(mar = c(3.1, 3.1, 2.1, 2.1) )
plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
       xlab=c(""), ##xlab=c("pro-SQ                                         pro-change"),
       ylab=c(""), ##ylab=c("interpretivist                                            literalist"),
       main=c("")) ##main=paste("Acc+Con (ancla j) time=",t,I,"obs"))
abline(-1.5,0,col="grey",lty=3); abline(-1,0,col="grey",lty=3); abline(-.5,0,col="grey",lty=3);
       abline(0,0,col="grey",lty=3); abline(.5,0,col="grey",lty=3); abline(1,0,col="grey",lty=3);
       abline(1.5,0,col="grey",lty=3);
abline(v=-1.5,col="grey",lty=3); abline(v=-1,col="grey",lty=3); abline(v=-.5,col="grey",lty=3);
       abline(v=0,col="grey",lty=3); abline(v=.5,col="grey",lty=3); abline(v=1,col="grey",lty=3);
       abline(v=1.5,col="grey",lty=3);
legend(1.1,-.65, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")
##for (j in 1:J){
##    segments(jotas[t,1,j],jotas[t,5,j],jotas[t,3,j],jotas[t,5,j],col="gray")
##    segments(jotas[t,2,j],jotas[t,4,j],jotas[t,2,j],jotas[t,6,j],col="gray")
##    }
##for (j in 1:J){
##    lines(eps[,,j],col=color.67[j])
##    }
for (j in 1:J){
    points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
    }
for (j in 1:J){
    points(jotas[j,2],jotas[j,5], col=dCoord[j]); ## pone coordinadores
    }
##for (j in 1:J){
##    text(jotas[j,2],jotas[j,5],labels=coords[j])
##    }


## cutlines
#cuad <- 1*d1+2*d2+3*d3+4*d4+5*d5+6*d6+7*d7+8*d8 ## could be handy to draw cutlines for some t in static map
    plot(c(-1.5,1.5),c(-1.5,1.5),type="n",
           xlab=c(""), 
           ylab=c(""), 
           main=c("")) 
#    atmp <- amed[cuad==t]; btmp <- bmed[cuad==t];
    atmp <- amed; btmp <- bmed;
    N <- length(atmp)
    for (n in 1:N){
        abline(a=btmp[n], b=atmp[n], col="grey") } ## OJO: a en mi modelo es slope, en R es constant
    for (j in 1:J){
        points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
        }

## cutlines one-by-one
setwd("d:/01/data/rollcall/aldf/graphs/cutlinesOnebyOne")
    atmp <- amed; btmp <- bmed;
    N <- length(atmp)
#    n <- 12
    for (n in 1:N){
        plot(c(-2,2),c(-2,2),type="n",
           xlab=c(""), 
           ylab=c(""), 
           main=paste(RCs$yr[n],"-",RCs$mo[n],"-",RCs$dy[n],"#",RCs$folio[n],"  (",RCs$favor[n],"/",RCs$contra[n],"/",RCs$absten[n],")",sep="")) 
        abline(a=btmp[n], b=atmp[n], col="black") 
            for (j in 1:J){
                points(jotas[j,2],jotas[j,5],pch=20,col=color.67[j])
                }
        savePlot(filename = paste("cutline",n, sep=""), type = "pdf")
    }
setwd("d:/01/data/rollcall/aldf")






####################################################################
###   Static 2Dimensions, dim1 identified with elite09 survey    ###
###   à la Zucco, irt paremeterization, SEQUENTIAL ESTIMATION    ###
####################################################################

## IMPORT EXOGENOUS PARTY POSITION ESTIMATES
## (CAN BE RE-DONE IN /ideol/elite09 SUBDIRECTORY; SHOULD
##  BE RE-DONE USING ABORTION, GAY, TLC, ETC TO GET 1-DIM
##  LATENT IDEOLOGY INSTEAD OF L/R 5-POINT SCALE)
load ( paste(workdir, "/ideol/elite2009/el09partypos.RData", sep="") )
elite09.party.positions$post.mu.quant ## LIST QUANTILES
mu.p <- elite09.party.positions$post.mu.quant$q50
##elite09.party.positions$post.mu.p.quant ## LIST QUANTILES
##mu.p <- elite09.party.positions$post.mu.p.quant$q50

cat("
model {
  for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
      #v.hat[j,i] ~ dbern(pr[j,i]);                                 ## voting rule
      #pr[j,i] <- phi(v.star[j,i]);                                 ## sets 0<p<1
      v.star[j,i] ~ dnorm(v.star.hat[j,i],1)I(lo.v[j,i],hi.v[j,i]); ## truncated normal sampling
      v.star.hat[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j];   ## utility differential
                  }
                }
  ## IDENTIFY HORIZONTAL AXIS WITH SURVEY PARTY MUs
  for (j in 1:J){
    x[j] ~ dnorm(mu.x[party[j]], tau.x);   
    y[j] ~ dnorm(0, .1)                  ## DIM2 UNINFORMATIVE
    }
  for (p in 1:P){
    mu.x[p] ~ dnorm(mu.p[p], 4);
    }
  tau.x <- pow(sigma.x, -2);
  sigma.x ~ dunif(0, 100);
  for(i in 1:I){
    alpha[i] ~ dnorm( 0, 1);
    beta[i]  ~ dnorm( 0, 1);
    delta[i] ~ dnorm( 0, 1);
    }
}
", file="model.txt")

party <- rep(NA, times=J)
party <- ifelse(dipdat$part=="pan", 1, 
          ifelse(dipdat$part=="pri", 2,  
           ifelse(dipdat$part=="prd", 3,  
            ifelse(dipdat$part=="pt", 4,  
             ifelse(dipdat$part=="pvem", 5,  
              ifelse(dipdat$part=="conve", 6,  
               ifelse(dipdat$part=="panal", 7, NA )))))))
v <- rc
v <- t(v)
J <- nrow(v); I <- ncol(v); P <- max(party)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
##ip.data <- list ("J", "I", "lo.v", "hi.v", "mu.p", "party")
ip.data <- list ("J", "I", "P", "lo.v", "hi.v", "mu.p", "party")
ip.inits <- function (){
    list (
    v.star=vstar,
    delta=rnorm(I),
    alpha=rnorm(I),
    beta=rnorm(I),
    mu.x=rnorm(P), 
    sigma.x=runif(1),
    x=rnorm(J),
    y=rnorm(J)
    )
    }
##ip.parameters <- c("delta","beta", "alpha", "x", "y")
ip.parameters <- c("delta","beta", "alpha", "mu.x", "sigma.x", "x", "y")

###test ride to see program works
##results <- bugs (ip.data, ip.inits, ip.parameters,
##                "model.txt", n.chains=1,
##                n.iter=10, n.thin=1, debug=T,
##                bugs.seed = ch*10000,
##                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
###                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
##                program = c("WinBUGS"))

#longer run
results <- bugs (ip.data, ip.inits, ip.parameters,
                "model.txt", n.chains=1,
                n.iter=5000, n.burnin=4000, n.thin=10, debug=F,
                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

plot(results)
print(results)

## change .ch number appropriately for each chain
results.3 <- results
save(results.3, file="tmp3.RData")
#rm(results)

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED results.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
## results <- results.1 ## so that it inherits mcmc.list attribute
## results[[2]] <- results.2[[1]]
## results[[3]] <- results.3[[1]]
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )







eric  ####################################################################
###   Static 2Dimensions, dim1 identified with elite09 survey    ###
###  à la Zucco, irt paremeterization, SIMULTANEOUS ESTIMATION   ###
####################################################################

## IMPORT ELITE SURVEY DATA
el09 <- read.spss(paste(workdir, "/ideol/elite2009/Diputados09.sav", sep=""), to.data.frame=TRUE)
## HAY MUCHOS REACTIVOS QUE NO ESTOY USANDO
## HABRA QUE AFINAR EL EJERCICIO REMPLAZANDO L/R FIVE-POINT SCALES POR UNA ESTIMACION UNIDIMENSIONAL
## DE IDEOLOGIA DE CADA DIPUTADO CON REACTIVOS DE VALORES (TIPO ABORTO, GAY, TLC, ETC)
tmp <- c(colnames(el09)=="folio" | 
  colnames(el09)=="p5" |
  colnames(el09)=="p6" |
  colnames(el09)=="p41a" |
  colnames(el09)=="p41b" |
  colnames(el09)=="p41e" |
  colnames(el09)=="p41g" |
  colnames(el09)=="p42" |
  colnames(el09)=="p43a" |
  colnames(el09)=="p43b" |
  colnames(el09)=="p43c" |
  colnames(el09)=="p43d" |
  colnames(el09)=="p43e" |
  colnames(el09)=="p43f" |
  colnames(el09)=="p43g" |
  colnames(el09)=="p44" |
  colnames(el09)=="d" |
  colnames(el09)=="e" |
  colnames(el09)=="h" 
  )
el09work <- el09[,tmp]
R <- nrow(el09work); IS <- ncol(el09work)
colnames(el09work) <- c("folio", 
                   "dcfePriv",
                   "dpemexPriv",
                   "libreCom",
                   "protecNec",
                   "aborIleg",
                   "matriGay",
                   "LRselfpol",
                   "LRpan",
                   "LRpri",
                   "LRprd",
                   "LRpvem",
                   "LRpt",
                   "LRconve",
                   "LRpanal",
                   "LRselfsoc",
                   "dSMD",
                   "party",
                   "edo"
  )
el09work$dcfePriv <- ifelse(as.numeric(el09work$dcfePriv)==1, 0, 
                 ifelse(as.numeric(el09work$dcfePriv)==2, 1, NA))
el09work$dpemexPriv <- ifelse(as.numeric(el09work$dpemexPriv)==1, 0, 
                 ifelse(as.numeric(el09work$dpemexPriv)==2, 1, NA))
el09work$libreCom <- as.numeric(el09work$libreCom); el09work$libreCom <- ifelse(el09work$libreCom==5, NA, el09work$libreCom)
el09work$protecNec <- as.numeric(el09work$protecNec); el09work$protecNec <- ifelse(el09work$protecNec==5, NA, el09work$protecNec)
el09work$freeTr <- (el09work$libreCom + (-el09work$protecNec+5))/2 ## AVG FREE_TRADE_PROMOTION & TARIFFS_NEVER_NECESSARY
el09work$aborIleg <- as.numeric(el09work$aborIleg); el09work$aborIleg <- ifelse(el09work$aborIleg==5, NA, el09work$aborIleg)
el09work$matriGay <- as.numeric(el09work$matriGay); el09work$matriGay <- ifelse(el09work$matriGay==5, NA, el09work$matriGay)
el09work$LRselfpol <- as.numeric(el09work$LRselfpol); el09work$LRselfpol <- ifelse(el09work$LRselfpol==6, NA, el09work$LRselfpol)
el09work$LRpan <- as.numeric(el09work$LRpan); el09work$LRpan <- ifelse(el09work$LRpan==6, NA, el09work$LRpan)
el09work$LRpri <- as.numeric(el09work$LRpri); el09work$LRpri <- ifelse(el09work$LRpri==6, NA, el09work$LRpri)
el09work$LRprd <- as.numeric(el09work$LRprd); el09work$LRprd <- ifelse(el09work$LRprd==6, NA, el09work$LRprd)
el09work$LRpt <- as.numeric(el09work$LRpt); el09work$LRpt <- ifelse(el09work$LRpt==6, NA, el09work$LRpt)
el09work$LRpvem <- as.numeric(el09work$LRpvem); el09work$LRpvem <- ifelse(el09work$LRpvem==6, NA, el09work$LRpvem)
el09work$LRconve <- as.numeric(el09work$LRconve); el09work$LRconve <- ifelse(el09work$LRconve==6, NA, el09work$LRconve)
el09work$LRpanal <- as.numeric(el09work$LRpanal); el09work$LRpanal <- ifelse(el09work$LRpanal==6, NA, el09work$LRpanal)
el09work$LRselfsoc <- as.numeric(el09work$LRselfsoc); el09work$LRselfsoc <- ifelse(el09work$LRselfsoc==6, NA, el09work$LRselfsoc)
el09work$dSMD <- ifelse(as.numeric(el09work$dSMD)==1, 0, 
                 ifelse(as.numeric(el09work$dSMD)==2, 1, NA))
el09work$edon <- as.numeric(el09work$edo)-1; el09work$edon <- ifelse(el09work$edon==0, NA, el09work$edon)
el09work$edo <- rep(NA, times=R)
el09work$edo <-  ifelse (el09work$edon==1 , "ags" ,
             ifelse (el09work$edon==2 , "bc"  ,
              ifelse (el09work$edon==3 , "bcs" ,
               ifelse (el09work$edon==4 , "cam" ,
                ifelse (el09work$edon==5 , "coa" ,
                 ifelse (el09work$edon==6 , "col" ,
                  ifelse (el09work$edon==7 , "cps" ,
                   ifelse (el09work$edon==8 , "cua" ,
                    ifelse (el09work$edon==9 , "df"  ,
                     ifelse (el09work$edon==10, "dgo" ,
                      ifelse (el09work$edon==11, "gua" ,
                       ifelse (el09work$edon==12, "gue" ,
                        ifelse (el09work$edon==13, "hgo" ,
                         ifelse (el09work$edon==14, "jal" ,
                          ifelse (el09work$edon==15, "mex" ,
                           ifelse (el09work$edon==16, "mic" ,
                            ifelse (el09work$edon==17, "mor" ,
                             ifelse (el09work$edon==18, "nay" ,
                              ifelse (el09work$edon==19, "nl"  ,
                               ifelse (el09work$edon==20, "oax" ,
                                ifelse (el09work$edon==21, "pue" ,
                                 ifelse (el09work$edon==22, "que" ,
                                  ifelse (el09work$edon==23, "qui" ,
                                   ifelse (el09work$edon==24, "san" ,
                                    ifelse (el09work$edon==25, "sin" ,
                                     ifelse (el09work$edon==26, "son" ,
                                      ifelse (el09work$edon==27, "tab" ,
                                       ifelse (el09work$edon==28, "tam" ,
                                        ifelse (el09work$edon==29, "tla" ,
                                         ifelse (el09work$edon==30, "ver" ,
                                          ifelse (el09work$edon==31, "yuc" ,
                                           ifelse (el09work$edon==32, "zac" , NA
                                            ))))))))))))))))))))))))))))))))
tmp <- c(colnames(el09work)=="LRpan" | 
colnames(el09work)=="LRpri" | 
colnames(el09work)=="LRprd" | 
colnames(el09work)=="LRpvem" |
colnames(el09work)=="LRpt" | 
colnames(el09work)=="LRconve" |
colnames(el09work)=="LRpanal")
loc <- el09work[,tmp]-3 ## zero-centered: scale left -2 -1 0 1 2 right
loc <- loc[,c(1:3,5,4,6,7)] ## MAKES PT AND PVEM BE PARTIES 4 AND 5, RESPECTIVELY, AS IN MY DATA
rm(el09work, el09, tmp)

cat("
model {
## ESTIMATE PARTY POSITIONS WITH SURVEY DATA
    for (r in 1:R){     ### loop over respondents (anonymous deputies)
      for (p in 1:P){   ### loop over parties
        loc[r,p] ~ dnorm(loc.hat[r,p], tau.loc);
        loc.hat[r,p] <- b0[r] + b1[r]*mu.p[p];
                    }
                  }
      ############################
      ## priors non-informative ##
      ############################
    for (r in 1:R){
      b0[r] ~ dnorm(0,.01);
#      b1[r] ~ dnorm(0,.01);
      b1[r] ~ dunif(0,1.5);  ## SLOPE POSITIVE TO RETAIN POLARITY (cota derecha determina escala)
                  }
    for (p in 1:P){
      mu.p[p] ~ dnorm(0,.01);
                  }
    tau.loc <- pow(sigma.loc, -2);
    sigma.loc ~ dunif(0,100);
##
## ESTIMATE IDEAL POINTS WITH ROLL CALL DATA
  for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
      #v.hat[j,i] ~ dbern(pr[j,i]);                                 ## voting rule
      #pr[j,i] <- phi(v.star[j,i]);                                 ## sets 0<p<1
      v.star[j,i] ~ dnorm(v.star.hat[j,i],1)I(lo.v[j,i],hi.v[j,i]); ## truncated normal sampling
      v.star.hat[j,i] <- beta[i]*x[j] - alpha[i] + delta[i]*y[j];   ## utility differential
                  }
                }
  ## IDENTIFY HORIZONTAL AXIS WITH SURVEY PARTY MUs
  for (j in 1:J){
    x[j] ~ dnorm(mu.x[party[j]], tau.x);   
    y[j] ~ dnorm(0, .1)                  ## DIM2 UNINFORMATIVE
    }
  for (p in 1:P){
    mu.x[p] ~ dnorm(mu.p[p], 4);
    }
  tau.x <- pow(sigma.x, -2);
  sigma.x ~ dunif(0, 1);
  for(i in 1:I){
    alpha[i] ~ dnorm( 0, 1);
    beta[i]  ~ dnorm( 0, 1);
    delta[i] ~ dnorm( 0, 1)
    }
}
", file="model.txt")

R <- nrow(loc)
P<- ncol(loc) 
loc <- as.matrix(loc)
party <- rep(NA, times=J)
party <- ifelse(dipdat$part=="pan", 1, 
          ifelse(dipdat$part=="pri", 2,  
           ifelse(dipdat$part=="prd", 3,  
            ifelse(dipdat$part=="pt", 4,  
             ifelse(dipdat$part=="pvem", 5,  
              ifelse(dipdat$part=="conve", 6,  
               ifelse(dipdat$part=="panal", 7, NA )))))))
v <- rc
v <- t(v)
J <- nrow(v); I <- ncol(v); P <- max(party)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
##ip.data <- list ("J", "I", "lo.v", "hi.v", "mu.p", "party")
ip.data <- list ("J", "I", "P", "lo.v", "hi.v", "party", "loc", "R")
ip.inits <- function (){
    list (
    v.star=vstar,
    delta=rnorm(I),
    alpha=rnorm(I),
    beta=rnorm(I),
    mu.x=rnorm(P), 
    sigma.x=runif(1),
    x=rnorm(J),
    y=rnorm(J),
    b0=rnorm(R),
#    b1=rnorm(R),
    b1=runif(R),
    mu.p=rnorm(P),
    sigma.loc=runif(1)
    )
    }
##ip.parameters <- c("delta","beta", "alpha", "x", "y")
ip.parameters <- c("delta", "beta", "alpha", "mu.x", "sigma.x", "x", "y", "b0", "b1", "mu.p")

###test ride to see program works
##results <- bugs (ip.data, ip.inits, ip.parameters,
##                "model.txt", n.chains=1,
##                n.iter=10, n.thin=1, debug=T,
##                bugs.seed = ch*10000,
##                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
###                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
##                program = c("WinBUGS"))

#longer run
start.time <- proc.time()
results <- bugs (ip.data, ip.inits, ip.parameters,
                "model.txt", n.chains=1,
                n.iter=4000, n.burnin=3000, n.thin=10, debug=F,
                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")


plot(results)
print(results)

## change .ch number appropriately for each chain
results.3 <- results
save(results.3, file="tmp3.RData")
#rm(results)

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED results.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
## results <- results.1 ## so that it inherits mcmc.list attribute
## results[[2]] <- results.2[[1]]
## results[[3]] <- results.3[[1]]
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )

## 
load(paste(workdir, "/ip61_2Dpj.RData", sep=""))
results <- ip2Dpj




########################################################################
###     Static 1Dimension, identified with elite09 survey à la       ###
###   Zucco, spatial model parameterization, SEQUENTIAL ESTIMATION   ###
########################################################################

## IMPORT EXOGENOUS PARTY POSITION ESTIMATES
## (CAN BE RE-DONE IN /ideol/elite09 SUBDIRECTORY; SHOULD
##  BE RE-DONE USING ABORTION, GAY, TLC, ETC TO GET 1-DIM
##  LATENT IDEOLOGY INSTEAD OF L/R 5-POINT SCALE)
load ( paste(workdir, "/ideol/elite2009/el09partypos.RData", sep="") )
##elite09.party.positions$post.mu.quant ## LIST QUANTILES
mu.p <- elite09.party.positions$post.mu.quant$q50
##elite09.party.positions$post.mu.p.quant ## LIST QUANTILES
##mu.p <- elite09.party.positions$post.mu.p.quant$q50

cat("
model {
  for (j in 1:J){                ## loop over deputies
    for (i in 1:I){              ## loop over items
     #v.hat[j,i] ~ dbern(p[j,i])                             ## voting rule
     #p[j,i] <- phi(y.star[j,i])                             ## sets 0<p<1
     v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i])    ## truncated normal sampling
     mu[j,i] <- d[i]*x[j] - n[i]                             ## utility differential
     }
  }
#  for (i in 1:I){
#     m[i] <- n[i] / d[i]                                    ## midpoint puedo sacarlo post-estimación
#  }
  ## IDENTIFY AXIS WITH SURVEY PARTY MUs
  for (j in 1:J){
    x[j] ~ dnorm(mu.x[party[j]], tau.x)
##    x[j] ~ dnorm(mu.x[party[j]], 4)
##    x[j] ~ dnorm(mu.p[party[j]], 4)
    }
  for (p in 1:P){
    mu.x[p] ~ dnorm(mu.p[p], 4);
    }
  tau.x <- pow(sigma.x, -2);
  sigma.x ~ dunif(0, 100);
  for(i in 1:I){
    d[i] ~ dnorm( 0, 0.25)
    n[i] ~ dnorm( 0, 0.25)
    }
}
", file="model.txt")

party <- rep(NA, times=J)
party <- ifelse(dipdat$part=="pan", 1,
          ifelse(dipdat$part=="pri", 2,
           ifelse(dipdat$part=="prd", 3,
            ifelse(dipdat$part=="pt", 4,
             ifelse(dipdat$part=="pvem", 5,
              ifelse(dipdat$part=="conve", 6,
               ifelse(dipdat$part=="panal", 7, NA )))))))
v <- rc
v <- t(v)
J <- nrow(v); I <- ncol(v); P <- max(party)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
ip.data <- list ("J", "I", "P", "lo.v", "hi.v", "mu.p", "party")
##ip.data <- list ("J", "I", "lo.v", "hi.v", "mu.p", "party")
ip.inits <- function (){
    list (
    v.star=vstar,
    mu.x=rnorm(P),
    sigma.x=runif(1),
    x=rnorm(J),
    d=rnorm(I),
    n=rnorm(I)
    )
    }
ip.parameters <- c("d","n", "mu.x", "sigma.x", "x")
##ip.parameters <- c("d","n","x")

###test ride to see program works
##start.time <- proc.time()
##results <- bugs (ip.data, ip.inits, ip.parameters,
##                "model.txt", n.chains=1,
##                n.iter=10, n.thin=1, debug=T,
##                bugs.seed = ch*10000,
##                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
###                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
##                program = c("WinBUGS"))
##time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
##cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")

#longer run
start.time <- proc.time()
results <- bugs (ip.data, ip.inits, ip.parameters,
                "model.txt", n.chains=1,
                n.iter=5000, n.burnin=4000, n.thin=10, debug=F,
                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")

plot(results)
print(results)

## change .ch number appropriately for each chain
results.2 <- results
save(results.2, file="tmp2.RData")
#rm(results)

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED results.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
## results <- results.1 ## so that it inherits mcmc.list attribute
## results[[2]] <- results.2[[1]]
## results[[3]] <- results.3[[1]]
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )






##############################################################
### DYNAMIC 2D PARTY ANCHORS WITH POST-ESTIMATION ANALYSIS ###
##############################################################

per <- votdat$mo
per[per<9]<- 1 ## HASTA AGOSTO ES 1ER PERIODO
per[per>=9]<- 2
tmp<-(votdat$yr-2009)*2 ## to work with cuadrimestres 
per<-per+tmp
per<-per-min(per)+1
#
T<-max(per)
#
d1 <- ifelse(per==1,1,0) ## 
d2 <- ifelse(per==2,1,0) ## 
d3 <- ifelse(per==3,1,0) ## 
d4 <- ifelse(per==4,1,0) ## 
d5 <- ifelse(per==5,1,0) ## 
##d6 <- ifelse(per==6,1,0) ## 
##d7 <- ifelse(per==7,1,0) ## 
##d8 <- ifelse(per==8,1,0) ## 

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse ( dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000,
           ifelse ( dipdat$part=="pvem", tmp+4000,
            ifelse ( dipdat$part=="conve", tmp+5000,
             ifelse ( dipdat$part=="panal", tmp+6000, tmp+7000 )))))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])
PVEM <- PT+length(tmp[tmp>4000&tmp<5001]); CONVE <- PVEM+length(tmp[tmp>5000&tmp<6001])
PANAL <- CONVE+length(tmp[tmp>6000&tmp<7001])

###################################################################
### Dynamic model in Two Dimensions by periodo ordinario
###################################################################
cat("
model {
  for (j in 1:J){                ## loop over diputados
    for (i in 1:I){              ## loop over items
      #y.hat[j,i] ~ dbern(p[j,i]);                                  ## voting rule
      #p[j,i] <- phi(v.star[j,i]);                                  ## sets 0<p<1
      v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i]);   ## truncated normal sampling, cf. Jackman
      mu[j,i] <- beta[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] 
                          + xFive[j]*d5[i])
                - alpha[i] 
                + delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] 
                            + yFive[j]*d5[i]);  ## utility differential
#      mu[j,i] <- delta[i]*a[i]*(xOne[j]*d1[i] + xTwo[j]*d2[i] + xThree[j]*d3[i] + xFour[j]*d4[i] + xFive[j]*d5[i] + 
#      xSix[j]*d6[i] + xSeven[j]*d7[i] + xEight[j]*d8[i])
#                + delta[i]*b[i] - delta[i]*(yOne[j]*d1[i] + yTwo[j]*d2[i] + yThree[j]*d3[i] + yFour[j]*d4[i] + 
#                yFive[j]*d5[i] + ySix[j]*d6[i] + ySeven[j]*d7[i] + yEight[j]*d8[i])  ## utility differential
                  }
      xOne[j] ~   dnorm (xZero[j],15);  ## en 2do intento slack era 20, 3ro 10
      xTwo[j] ~   dnorm (xOne[j],15);
      xThree[j] ~ dnorm (xTwo[j],15);
      xFour[j] ~  dnorm (xThree[j],15);
      xFive[j] ~  dnorm (xFour[j],15);
#      xSix[j] ~   dnorm (xFive[j],15);
      yOne[j] ~   dnorm (yZero[j],15);
      yTwo[j] ~   dnorm (yOne[j],15);
      yThree[j] ~ dnorm (yTwo[j],15);
      yFour[j] ~  dnorm (yThree[j],15);
      yFive[j] ~  dnorm (yFour[j],15);
#      ySix[j] ~   dnorm (yFive[j],15);
                }
#  for (i in 1:I){
#  a[i] <- sin(angle[i]) / sqrt(1-sin(angle[i])*sin(angle[i]))
#  }
## ESTO LO PUEDO SACAR POST ESTIMACION
##  for (i in 1:I){
##  a[i] <- beta[i] / delta[i]  ## pendiente de cutline
##  b[i] <- alpha[i] / delta[i] ## constante de cutline
##  }
  ################
  ## priors
  ################
for (j in 1:PAN){
    xZero[j] ~  dnorm(1, 4)   # PAN
    yZero[j] ~  dnorm(-1, 4)
    }
for (j in (PAN+1):PRI){
    xZero[j] ~  dnorm(0, 4)    # PRI
    yZero[j] ~  dnorm(1, 4)
    }
for (j in (PRI+1):PRD){
    xZero[j] ~  dnorm(-1, 4)    # PRD
    yZero[j] ~  dnorm(-1, 4)
    }
for (j in (PRD+1):J){
    xZero[j] ~  dnorm(0, .1)    # REST UNINFORMATIVE
    yZero[j] ~  dnorm(0, .1)
    }
    for(i in 1:I){
        alpha[i] ~ dnorm( 0, 1)
        beta[i]  ~ dnorm( 0, 1)
        delta[i] ~ dnorm( 0, 1)
                 }
#    for(i in 1:I){
#        delta[i] ~ dnorm( 0, 0.01)
#        angle[i] ~ dunif(-1.57,1.57) # (-pi/2,pi/2)
#        b[i] ~ dnorm( 0, .01)
#                 }
}
", file="model.txt")

v <- rc
v <- t(v)
J <- nrow(v); I <- ncol(v)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
ip.data <- list ("d1", "d2", "d3", "d4", "d5", "J", "I", "lo.v", "hi.v", "PAN", "PRI", "PRD")
ip.inits <- function (){
    list (
    v.star=vstar,
    xOne=rnorm(J),
    yOne=rnorm(J),
    xTwo=rnorm(J),
    yTwo=rnorm(J),
    xThree=rnorm(J),
    yThree=rnorm(J),
    xFour=rnorm(J),
    yFour=rnorm(J),
    xFive=rnorm(J),
    yFive=rnorm(J),
##    xSix=rnorm(J),
##    ySix=rnorm(J),
    delta=rnorm(I),
    alpha=rnorm(I),
    beta=rnorm(I)
    )
    }
ip.parameters <- c("delta","beta", "alpha", "xOne", "xTwo", "xThree", "xFour", "xFive", #"xSix",  
                              "yOne", "yTwo", "yThree", "yFour", "yFive") #, "ySix")

###test ride to see program works
##start.time <- proc.time()
##results <- bugs (ip.data, ip.inits, ip.parameters, 
##                "model.txt", n.chains=1, 
##                n.iter=100, n.thin=1, debug=F,
##                bugs.seed = ch*10000,
##                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
###                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
##                program = c("WinBUGS"))
##time.elapsed <- round(((proc.time()-start.time)[3]),2); rm(start.time)
##cat("\tTime elapsed in estimation:",time.elapsed,"seconds","\n")

#longer run
start.time <- proc.time()
results <- bugs (ip.data, ip.inits, ip.parameters, 
                "model.txt", n.chains=1, 
                n.iter=5000, n.thin=25, debug=F,
                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))
time.elapsed <- round(((proc.time()-start.time)[3])/60/60,2); rm(start.time)
cat("\tTime elapsed in estimation:",time.elapsed,"hours","\n")



## change .ch number appropriately for each chain
results.2 <- results
save(results.2, file="tmp2.RData")
#rm(results)

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED results.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
## results <- results.1 ## so that it inherits mcmc.list attribute
## results[[2]] <- results.2[[1]]
## results[[3]] <- results.3[[1]]
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )


## 
load(paste(workdir, "/ip61_dyn_2D5k.RData", sep=""))

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
post.x     <- post.draw[,grep("x", colnames(post.draw))]
post.y     <- post.draw[,grep("y", colnames(post.draw))]
post.alpha <- post.draw[,grep("alpha", colnames(post.draw))]
post.beta  <- post.draw[,grep("beta", colnames(post.draw))]
post.delta <- post.draw[,grep("delta", colnames(post.draw))]

## SACA CONSTANTE Y PENDIENTE DE CUTLINES
post.a <- post.beta / post.delta  ## pendiente de cutline
post.b <- post.alpha / post.delta ## constante de cutline
post.d <- -post.delta                  ## término discriminante

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

jotas <- matrix(NA, nrow=J, ncol=6)
for (j in 1:J){
    jotas[j,1] <- quantile (post.x[,j], 0.025, names=F)
    jotas[j,2] <- quantile (post.x[,j], 0.50, names=F)
    jotas[j,3] <- quantile (post.x[,j], 0.975, names=F)
    jotas[j,4] <- quantile (post.y[,j], 0.025, names=F)
    jotas[j,5] <- quantile (post.y[,j], 0.50, names=F)
    jotas[j,6] <- quantile (post.y[,j], 0.975, names=F)
    }



jotas <- array(NA, dim=c(T,6,J)); colnames(jotas) <- c("x.025","x.5","x.975","y.025","y.5","y.975");
for (j in 1:J){
    tmp <- post.x[,grep("xOne", colnames(post.x))]
    jotas[1,1,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[1,2,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[1,3,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.x[,grep("xTwo", colnames(post.x))]
    jotas[2,1,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[2,2,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[2,3,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.x[,grep("xThree", colnames(post.x))]
    jotas[3,1,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[3,2,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[3,3,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.x[,grep("xFour", colnames(post.x))]
    jotas[4,1,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[4,2,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[4,3,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.x[,grep("xFive", colnames(post.x))]
    jotas[5,1,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[5,2,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[5,3,j] <- quantile (tmp[,j], 0.975, names=F)
#    tmp <- post.x[,grep("xSix", colnames(post.x))]
#    jotas[6,1,j] <- quantile (tmp[,j], 0.025, names=F)
#    jotas[6,2,j] <- quantile (tmp[,j], 0.50, names=F)
#    jotas[6,3,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.y[,grep("yOne", colnames(post.y))]
    jotas[1,4,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[1,5,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[1,6,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.y[,grep("yTwo", colnames(post.y))]
    jotas[2,4,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[2,5,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[2,6,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.y[,grep("yThree", colnames(post.y))]
    jotas[3,4,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[3,5,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[3,6,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.y[,grep("yFour", colnames(post.y))]
    jotas[4,4,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[4,5,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[4,6,j] <- quantile (tmp[,j], 0.975, names=F)
    tmp <- post.y[,grep("yFive", colnames(post.y))]
    jotas[5,4,j] <- quantile (tmp[,j], 0.025, names=F)
    jotas[5,5,j] <- quantile (tmp[,j], 0.50, names=F)
    jotas[5,6,j] <- quantile (tmp[,j], 0.975, names=F)
#    tmp <- post.y[,grep("ySix", colnames(post.y))]
#    jotas[6,4,j] <- quantile (tmp[,j], 0.025, names=F)
#    jotas[6,5,j] <- quantile (tmp[,j], 0.50, names=F)
#    jotas[6,6,j] <- quantile (tmp[,j], 0.975, names=F)
    }
###Quita retiros y sustituciones FALTA ESTO !!!!
##for (t in 8) jotas[t,,65] <- rep(NA,6) ## Jorge Díaz Cuervo pide licencia 23/9/2008=inicio cuad 7
##for (t in 1:6) jotas[t,,66] <- rep(NA,6) ## Carla Sánchez Armas, la suplente

### GRAPH PERIOD BY PERIOD
t <- 1
for (t in 1:T){
par(mar = c(3.1, 3.1, 2.1, 2.1) )
plot(c(min(jotas[,2,]),max(jotas[,2,])),c(min(jotas[,5,]),max(jotas[,5,])),type="n",
       xlab=c(""), 
       ylab=c(""), 
       xaxt="n",
       yaxt="n",
       main=paste("61, periodo ordinario", t)) 
abline(h=seq(from=round(min(jotas[,5,])*2,0)/2, to=round(max(jotas[,5,])*2,0)/2, by=.5), col="grey",lty=3); 
abline(v=seq(from=round(min(jotas[,2,])*2,0)/2, to=round(max(jotas[,2,])*2,0)/2, by=.5), col="grey",lty=3); 
#legend(1.1,-.65, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")
##for (j in 1:J){
##    segments(jotas[t,1,j],jotas[t,5,j],jotas[t,3,j],jotas[t,5,j],col="gray")
##    segments(jotas[t,2,j],jotas[t,4,j],jotas[t,2,j],jotas[t,6,j],col="gray")
##    }
##for (j in 1:J){
##    lines(eps[,,j],col=color.67[j])
##    }
for (j in 1:J){
    points(jotas[t,2,j],jotas[t,5,j],pch=20,col=dipdat$color[j])
    }
##for (j in 1:J){ 
##    points(jotas[t,2,j],jotas[t,5,j], col=dCoord[j]); ## pone coordinadores
##    }
setwd(paste(workdir, "/graphs", sep=""))
savePlot(filename = paste("ip61dyn", t, sep=""), type = "pdf")
setwd(workdir)
}

### GRAPH ALL PERIODS STATE BY STATE
e <- 2
for (e in 1:32){
select <- (dipdat$dsmd==1 & dipdat$edon==e)
par(mar = c(3.1, 3.1, 2.1, 2.1) )
plot(c(min(jotas[,2,]),max(jotas[,2,])),c(min(jotas[,5,]),max(jotas[,5,])),type="n",
       xlab=c(""), 
       ylab=c(""), 
       xaxt="n",
       yaxt="n",
       main="")#paste("61st, SMDs from", levels(dipdat$edo)[e])) 
abline(h=seq(from=round(min(jotas[,5,])*2,0)/2, to=round(max(jotas[,5,])*2,0)/2, by=.5), col="grey",lty=3); 
abline(v=seq(from=round(min(jotas[,2,])*2,0)/2, to=round(max(jotas[,2,])*2,0)/2, by=.5), col="grey",lty=3); 
#legend(1.1,-.65, legend=part.list, cex=.75, pch=20, pt.cex=1.25, col=color.list, bg="white")
##for (j in 1:J){
##    segments(jotas[t,1,j],jotas[t,5,j],jotas[t,3,j],jotas[t,5,j],col="gray")
##    segments(jotas[t,2,j],jotas[t,4,j],jotas[t,2,j],jotas[t,6,j],col="gray")
##    }
##for (j in 1:J){
##    lines(eps[,,j],col=color.67[j])
##    }
#points(jotas[1,2,select],jotas[1,5,select],pch=20,col=dipdat$color[select])
segments(jotas[1,2,select],jotas[1,5,select], jotas[2,2,select],jotas[2,5,select], col=dipdat$color[select])
segments(jotas[2,2,select],jotas[2,5,select], jotas[3,2,select],jotas[3,5,select], col=dipdat$color[select])
segments(jotas[3,2,select],jotas[3,5,select], jotas[4,2,select],jotas[4,5,select], col=dipdat$color[select])
segments(jotas[4,2,select],jotas[4,5,select], jotas[5,2,select],jotas[5,5,select], col=dipdat$color[select])
arrows(jotas[1,2,select],jotas[1,5,select], jotas[5,2,select],jotas[5,5,select], col=dipdat$color[select], length=.1, lwd=2)
setwd(paste(workdir, "/graphs/states", sep=""))
savePlot(filename = paste("stdyn", e, sep=""), type = "pdf")
setwd(workdir)
}

################################################
### END DYNAMIC AND POST-ESTIMATION ANALYSIS ###
################################################









###########################################################
###########################################################
###    Static 1Dimension two extremist party anchors    ###
###########################################################
###########################################################

## SORT BY PARTY
tmp <- 1:dim(rc)[1]
tmp <- ifelse ( dipdat$part=="pan", tmp,
        ifelse ( dipdat$part=="pri", tmp+1000,
         ifelse ( dipdat$part=="prd", tmp+2000,
          ifelse ( dipdat$part=="pt", tmp+3000,
           ifelse ( dipdat$part=="pvem", tmp+4000,
            ifelse ( dipdat$part=="conve", tmp+5000,
             ifelse ( dipdat$part=="panal", tmp+6000, tmp+7000 )))))))
## rcold <- rc; dipold <- dipdat
rc <- rc[,order(tmp)]; dipdat <- dipdat[order(tmp),]
## PARTY INDICES (FOR ANCHORS)
PAN <- length(tmp[tmp<1001]); PRI <- PAN+length(tmp[tmp>1000&tmp<2001]);
PRD <- PRI+length(tmp[tmp>2000&tmp<3001]); PT <- PRD+length(tmp[tmp>3000&tmp<4001])
PVEM <- PT+length(tmp[tmp>4000&tmp<5001]); CONVE <- PVEM+length(tmp[tmp>5000&tmp<6001])
PANAL <- CONVE+length(tmp[tmp>6000&tmp<7001])
##
PAN; PRI; PRD; PT; PVEM; CONVE; PANAL

cat("
model {
  for (j in 1:J){                ## loop over deputies
    for (i in 1:I){              ## loop over items
     #v.hat[j,i] ~ dbern(p[j,i])                             ## voting rule
     #p[j,i] <- phi(y.star[j,i])                             ## sets 0<p<1
     v.star[j,i] ~ dnorm(mu[j,i],1)I(lo.v[j,i],hi.v[j,i])    ## truncated normal sampling
     mu[j,i] <- d[i]*x[j] - n[i]                             ## utility differential
     }
  }
#  for (i in 1:I){
#     m[i] <- n[i] / d[i]                                    ## midpoint puedo sacarlo post-estimación
#  }
  ## priors
for (j in 1:PAN){
    x[j] ~  dnorm(1, 4)    # RIGHT PAN
    }
for (j in (PAN+1):PRD){
    x[j] ~  dnorm(0, .1)   # REST NON-INFORMATIVE
    }
for (j in (PRD+1):PT){
    x[j] ~  dnorm(-1, 4)    # LEFT PT
    }
for (j in (PT+1):J){
    x[j] ~  dnorm(0, .1)
    }
for (i in 1:I){
    d[i] ~ dnorm( 0, 0.25)
    n[i] ~ dnorm( 0, 0.25)
    }
}
", file="model1Dj.txt")
## END MODEL

v <- rc
v <- t(v)
J <- nrow(v); I <- ncol(v)
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -5)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 5)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
    for (i in 1:I){
        vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
rm(v)
ip.data <- list ("J", "I", "lo.v", "hi.v", "PAN", "PRD", "PT")
ip.inits <- function (){
    list (
    v.star=vstar,
    d=rnorm(I),
    x=rnorm(J),
    n=rnorm(I)
    )
    }
ip.parameters <- c("d", "x", "n") #, "deviance")

#test ride to see program works
results <- bugs (ip.data, ip.inits, ip.parameters, 
                "model1Dj.txt", n.chains=1, 
                n.iter=10, n.thin=1, debug=F,
                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

#longer run
results <- bugs (ip.data, ip.inits, ip.parameters, 
                "model1Dj.txt", n.chains=1, 
                n.iter=2500, n.thin=10, debug=F,
#                n.iter=5000, n.burnin=2500, n.thin=25, debug=F,
                bugs.seed = ch*10000,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

plot(results)
print(results)

## change .ch number appropriately for each chain
results.3 <- results
save(results.3, file="tmp3.RData")
#rm(results)

## COMBINE UNICHAINS RUN SEPARATELY: LOAD EACH CHAIN NAMED results.ch, THEN:
load("tmp2.RData"); load("tmp3.RData")
## results <- results.1 ## so that it inherits mcmc.list attribute
## results[[2]] <- results.2[[1]]
## results[[3]] <- results.3[[1]]
results <- list(results, results.2, results.3)
rm(results.2, results.3)
chains <- list ( results[[1]]$sims.matrix, results[[2]]$sims.matrix, results[[3]]$sims.matrix )


#####################################################
#####################################################
###    Static 1Dimension two extremist anchors    ###
#####################################################
#####################################################

## ANCHORS
##          AgsPan                      Noroña
#DER<-grep("ags01p", dip$id); IZQ<-grep("df19p", dip$id)
##          Luken                      Cárdenas
DER<-grep("bc05p", dip$id); IZQ<-grep("df04p", dip$id)

votes <- rc
## TAKE SAMPLE HERE IF SO WISHED
rnd <- runif(dim(rc)[1]); votes <- votes[rnd<.3,]
votes <- t(votes)
J <- nrow(votes); I <- ncol(votes)
v <- votes
lo.v <- ifelse(is.na(v)==TRUE | v== 1, 0, -50)
hi.v <- ifelse(is.na(v)==TRUE | v==-1,  0, 50)
vstar <- matrix (NA, nrow=J, ncol=I)
for (j in 1:J){
for (i in 1:I){
  vstar[j,i] <- ifelse(v[j,i]==0, 0, ifelse(v[j,i]==1, runif(1), -1*runif(1)))}}
dip.data <- list ("J", "I", "lo.v", "hi.v", "DER", "IZQ")
dip.inits <- function (){
    list (
    v.star=vstar,
    x=rnorm(J),
    delta=rnorm(I),
    n=rnorm(I)
    )
    }
dip.parameters <- c("delta","n", "x")

#test ride to see program works
dip.61 <- bugs (dip.data, dip.inits, dip.parameters, 
                "modelSta1Dj.txt", n.chains=3, 
                n.iter=10, n.thin=1, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

#longer run
dip.61 <- bugs (dip.data, dip.inits, dip.parameters, 
                "modelSta1Dj.txt", n.chains=3, 
                n.iter=1000, n.thin=10, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

plot(dip.61)
print(dip.61)

#to continue running
tmp1<-list (
    v.star=vstar,
    x=dip.61$last.values[[1]]$x,
    delta=dip.61$last.values[[1]]$delta,
    n=dip.61$last.values[[1]]$n
    )
tmp2<-list (
    v.star=vstar,
    x=dip.61$last.values[[2]]$x,
    delta=dip.61$last.values[[2]]$delta,
    n=dip.61$last.values[[2]]$n
    )
tmp3<-list (
    v.star=vstar,
    x=dip.61$last.values[[3]]$x,
    delta=dip.61$last.values[[3]]$delta,
    n=dip.61$last.values[[3]]$n
    )
### for (chain in 1:3){dip.61$last.values[[chain]]$v.star <- vstar}
dip.61.2 <- bugs (dip.data, 
                inits=list(tmp1,tmp2,tmp3), 
                dip.parameters, 
                "modelSta1Dj.txt", n.chains=3, 
                n.iter=1000, n.thin=10, debug=T,
                bugs.directory = "c:/Program Files (x86)/WinBUGS14/",
#                bugs.directory = "c:/Archivos de programa/WinBUGS14/",
                program = c("WinBUGS"))

dip.61 <- dip.61.2; rm(dip.61.2)

attach.bugs(dip.61)
ip <- abs(x)

tmp <- rep(NA, times=J)
ips <- data.frame(p025=tmp, p50=tmp, p975=tmp, name=dip$name, part=dip$part, id=dip$id)
for (j in 1:J){
    ips[j,1] <- quantile (ip[,j], 0.025, names=F)
    ips[j,2] <- quantile (ip[,j], 0.50, names=F)
    ips[j,3] <- quantile (ip[,j], 0.975, names=F)
              }
ips <- ips[,c(-1,-3)]
ips[order(ips$p50),]




# Prob of being median
attach.bugs(trife.edos.1)
is.median <- matrix(NA, nrow=3000, ncol=7)
med <- rep(NA, times=3000)
for (i in 1:3000){
    med[i]<-median(x[i,1:7])}
for (i in 1:3000){
    for (j in 1:7){
        is.median[i,j] <- ifelse(x[i,j]==med[i],1,0)}}
pr.median <- rep(NA, times=7, names=names.1)
for (j in 1:7){
    pr.median[j] <- sum(is.median[,j]/3000)}
names.1
pr.median
