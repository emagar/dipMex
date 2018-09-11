rm(list = ls())
#
## SET YOUR WORKING DIRECTORY HERE (SAVE DATA FILES IN THIS DIRECTORY)
workdir <- c("~/Dropbox/data/rollcall/dipMex")
#workdir <- c("d:/01/Dropbox/data/rollcall/DipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/DipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/DipMex")
setwd(workdir)

# load and prepare data objects
load(file="rc60.RData")
rc60 <- list(dgaceta=dgaceta, dipdat=dipdat, rc=rc, votdat=votdat)
rm(dgaceta, dipdat, rc, votdat)
load(file="rc61.RData")
rc61 <- list(dgaceta=dgaceta, dipdat=dipdat, rc=rc, votdat=votdat)
rm(dgaceta, dipdat, rc, votdat)

# summarize votes by party
tmp <- rc60$rc[,rc60$dipdat$part=="pan"]
pan60 <- pri60 <- prd60 <- data.frame(vote=1:nrow(tmp), ayes=rep(NA,nrow(tmp)), nays=rep(NA,nrow(tmp)), abs=rep(NA,nrow(tmp)), noshow=rep(NA,nrow(tmp)))
tmp2 <- tmp;
tmp2[,] <- 0; tmp2[tmp==1] <- 1; pan60$ayes <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==2] <- 1; pan60$nays <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==3 | tmp==4] <- 1; pan60$abs <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==5] <- 1; pan60$noshow <- apply(tmp2, 1, sum)
#
tmp <- rc60$rc[,rc60$dipdat$part=="pri"]
tmp2 <- tmp;
tmp2[,] <- 0; tmp2[tmp==1] <- 1; pri60$ayes <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==2] <- 1; pri60$nays <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==3 | tmp==4] <- 1; pri60$abs <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==5] <- 1; pri60$noshow <- apply(tmp2, 1, sum)
#
tmp <- rc60$rc[,rc60$dipdat$part=="prd"]
tmp2 <- tmp;
tmp2[,] <- 0; tmp2[tmp==1] <- 1; prd60$ayes <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==2] <- 1; prd60$nays <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==3 | tmp==4] <- 1; prd60$abs <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==5] <- 1; prd60$noshow <- apply(tmp2, 1, sum)
#
tmp <- rc61$rc[,rc61$dipdat$part=="pan"]
pan61 <- pri61 <- prd61 <- data.frame(vote=1:nrow(tmp), ayes=rep(NA,nrow(tmp)), nays=rep(NA,nrow(tmp)), abs=rep(NA,nrow(tmp)), noshow=rep(NA,nrow(tmp)))
tmp2 <- tmp;
tmp2[,] <- 0; tmp2[tmp==1] <- 1; pan61$ayes <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==2] <- 1; pan61$nays <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==3 | tmp==4] <- 1; pan61$abs <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==5] <- 1; pan61$noshow <- apply(tmp2, 1, sum)
#
tmp <- rc61$rc[,rc61$dipdat$part=="pri"]
tmp2 <- tmp;
tmp2[,] <- 0; tmp2[tmp==1] <- 1; pri61$ayes <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==2] <- 1; pri61$nays <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==3 | tmp==4] <- 1; pri61$abs <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==5] <- 1; pri61$noshow <- apply(tmp2, 1, sum)
#
tmp <- rc61$rc[,rc61$dipdat$part=="prd"]
tmp2 <- tmp;
tmp2[,] <- 0; tmp2[tmp==1] <- 1; prd61$ayes <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==2] <- 1; prd61$nays <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==3 | tmp==4] <- 1; prd61$abs <- apply(tmp2, 1, sum)
tmp2[,] <- 0; tmp2[tmp==5] <- 1; prd61$noshow <- apply(tmp2, 1, sum)

# total votes excluding abstentions and no-shows
pan60$tot <- pan60$ayes + pan60$nays
pri60$tot <- pri60$ayes + pri60$nays
prd60$tot <- prd60$ayes + prd60$nays
pan61$tot <- pan61$ayes + pan61$nays
pri61$tot <- pri61$ayes + pri61$nays
prd61$tot <- prd61$ayes + prd61$nays

## rice index of intra-party likeness: vote i, party j: abs(aye-nay)/(aye+nay)
pan60$likeness <- abs(pan60$ayes - pan60$nays) / pan60$tot
pri60$likeness <- abs(pri60$ayes - pri60$nays) / pri60$tot
prd60$likeness <- abs(prd60$ayes - prd60$nays) / prd60$tot
pan61$likeness <- abs(pan61$ayes - pan61$nays) / pan61$tot
pri61$likeness <- abs(pri61$ayes - pri61$nays) / pri61$tot
prd61$likeness <- abs(prd61$ayes - prd61$nays) / prd61$tot
#
prd60$likeness[prd60$tot==0] <- 1 ## all prd no-showed in one vote
pan61$likeness[pan61$tot==0] <- 1 ## all pan no-showed in one vote
prd61$likeness[prd61$tot==0] <- 1 ## all prd no-showed in one vote

## rice index of inter-party disimilarity:  vote i, parties j and k: absolute difference in proportion of j voting aye and proportion of k voting aye
pan60$panpri <- pri60$panpri <- abs(pan60$ayes/pan60$tot - pri60$ayes/pri60$tot)
pan60$panprd <- prd60$panprd <- abs(pan60$ayes/pan60$tot - prd60$ayes/prd60$tot)
pri60$priprd <- prd60$priprd <- abs(pri60$ayes/pri60$tot - prd60$ayes/prd60$tot)
pan61$panpri <- pri61$panpri <- abs(pan61$ayes/pan61$tot - pri61$ayes/pri61$tot)
pan61$panprd <- prd61$panprd <- abs(pan61$ayes/pan61$tot - prd61$ayes/prd61$tot)
pri61$priprd <- prd61$priprd <- abs(pri61$ayes/pri61$tot - prd61$ayes/prd61$tot)
#
pan60$panprd[prd60$tot==0] <- prd60$panprd[prd60$tot==0] <- abs(pan60$ayes[prd60$tot==0]/pan60$tot[prd60$tot==0] - 0)
pri60$priprd[prd60$tot==0] <- prd60$priprd[prd60$tot==0] <- abs(pri60$ayes[prd60$tot==0]/pri60$tot[prd60$tot==0] - 0)
#
pan61$panpri[pan61$tot==0] <- pri61$panpri[pan61$tot==0] <- abs(0 - pri61$ayes[pan61$tot==0]/pri61$tot[pan61$tot==0])
pan61$panprd[pan61$tot==0] <- prd61$panprd[pan61$tot==0] <- abs(0 - prd61$ayes[pan61$tot==0]/prd61$tot[pan61$tot==0])
#
pan61$panprd[prd61$tot==0] <- prd61$panprd[prd61$tot==0] <- abs(pan61$ayes[prd61$tot==0]/pan61$tot[prd61$tot==0] - 0)
pri61$priprd[prd61$tot==0] <- prd61$priprd[prd61$tot==0] <- abs(pri61$ayes[prd61$tot==0]/pri61$tot[prd61$tot==0] - 0)



## general summary
paste("pan likeness 60th =", sprintf("%.2f", mean(pan60$likeness)))
paste("pri likeness 60th =", sprintf("%.2f", mean(pri60$likeness)))
paste("prd likeness 60th =", sprintf("%.2f", mean(prd60$likeness)))
paste("pan likeness 61th =", sprintf("%.2f", mean(pan61$likeness)))
paste("pri likeness 61th =", sprintf("%.2f", mean(pri61$likeness)))
paste("prd likeness 61th =", sprintf("%.2f", mean(prd61$likeness)))
#
paste("pan-pri disimmilarity 60th =", sprintf("%.2f", mean(pan60$panpri)))
paste("pan-prd disimmilarity 60th =", sprintf("%.2f", mean(pan60$panprd)))
paste("pri-prd disimmilarity 60th =", sprintf("%.2f", mean(pri60$priprd)))
paste("pan-pri disimmilarity 61th =", sprintf("%.2f", mean(pan61$panpri)))
paste("pan-prd disimmilarity 61th =", sprintf("%.2f", mean(pan61$panprd)))
paste("pri-prd disimmilarity 61th =", sprintf("%.2f", mean(pri61$priprd)))
#
paste("(pan likeness 60th | pan-pri disim > .5) =", sprintf("%.2f", mean(pan60$likeness[pan60$panpri>.5])))
paste("(pan likeness 60th | pan-prd disim > .5) =", sprintf("%.2f", mean(pan60$likeness[pan60$panprd>.5])))
paste("(pri likeness 60th | pan-pri disim > .5) =", sprintf("%.2f", mean(pri60$likeness[pan60$panpri>.5])))
paste("(pri likeness 60th | pri-prd disim > .5) =", sprintf("%.2f", mean(pri60$likeness[pri60$priprd>.5])))
paste("(prd likeness 60th | pan-prd disim > .5) =", sprintf("%.2f", mean(prd60$likeness[pan60$panprd>.5])))
paste("(prd likeness 60th | pri-prd disim > .5) =", sprintf("%.2f", mean(prd60$likeness[pri60$priprd>.5])))
#
paste("(pan likeness 61th | pan-pri disim > .5) =", sprintf("%.2f", mean(pan61$likeness[pan61$panpri>.5])))
paste("(pan likeness 61th | pan-prd disim > .5) =", sprintf("%.2f", mean(pan61$likeness[pan61$panprd>.5])))
paste("(pri likeness 61th | pan-pri disim > .5) =", sprintf("%.2f", mean(pri61$likeness[pan61$panpri>.5])))
paste("(pri likeness 61th | pri-prd disim > .5) =", sprintf("%.2f", mean(pri61$likeness[pri61$priprd>.5])))
paste("(prd likeness 61th | pan-prd disim > .5) =", sprintf("%.2f", mean(prd61$likeness[pan61$panprd>.5])))
paste("(prd likeness 61th | pri-prd disim > .5) =", sprintf("%.2f", mean(prd61$likeness[pri61$priprd>.5])))
