rm(list = ls())
#
## SET YOUR WORKING DIRECTORY HERE (SAVE DATA FILES IN THIS DIRECTORY)
wd <- c("~/Dropbox/data/rollcall/dipMex/data")
#wd <- c("d:/01/Dropbox/data/rollcall/dipMex/data")
#wd <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/dipMex/data")
#wd <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/dipMex/data")
setwd(wd)

# if only loading saved data
load(file=paste(wd, "/votesForWeb", "rc62til7oct2014.RData", sep="/"))
ls()
votdat[1,]

## IMPORT DIPUTADO NAMES AND DISTRICTS (PREPARED IN EXCEL)
dipdat <- read.csv( paste(wd, "/diputados/dip62.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
colnames(dipdat)
#
id <- dipdat$id
names <- dipdat$nom
names.alt <- dipdat$nomRegEx ## to find names in web roll calls
pty <- dipdat$part

# adapt regex to match names in any order
tmp.orig <- dipdat$nomRegEx
tmp.modif <- sub(pattern = "^", replacement = "^(?=.*", x = tmp.orig)
tmp.modif <- sub(pattern = "$", replacement = ").*?", x = tmp.modif)
tmp.modif <- gsub(pattern = " ", replacement = ")(?=.*", x = tmp.modif) 
dipdat$nomRegEx <- names.alt <- tmp.modif
rm(tmp.orig, tmp.modif)
#
# cases that need fine tuning
dipdat$nomRegEx[dipdat$id=="c2rp20p"] <- "^(?=.*Francisco)(?=.*Agust[íi]n)*(?=.*Arroyo)(?=.*Vieyra).*?" ## Agustín is sometimes dropped
dipdat$nomRegEx[dipdat$id=="c3rp12p"] <- "^(?=.*Mar[íi]a)*(?=.*Beatriz)(?=.*Zavala)(?=.*Peniche).*?"    ## María is sometimes dropped
dipdat$nomRegEx[dipdat$id=="pue08p"] <- "^(?=.*Ana)*(?=.*Isabel)(?=.*Allende)(?=.*Cano).*?"             ## Ana aparece como Ma en algunas

####################################################################
## 62 LEG FROM INFOSIL 
####################################################################
#
leg <- 62
load(paste(wd, "/infosilFilenames62.RData",sep=""))
head(filenames)
## DROP
## filenames <- gsub(pattern = "votes/62", replacement = "votes/62/62", filenames) # data now in subdirectory
#
I <- length(filenames)
tmp <- rep(NA, times=I)
# votdat2 and rc2 will be rbind to votdat and rc in case they are adding new votes to the previously saved. Else they are renamed votdat and rc at the end
votdat2 <- data.frame(favor=tmp, contra=tmp, absten=tmp, quorum=tmp, ausen=tmp,
                      title=NA, leg=rep(leg, times=I),
                      filename=sub(filenames, pattern=".*(sil62.*).txt", replacement="\\1"),
                      yr=tmp, mo=tmp, dy=tmp, haveinfo=rep(1, times=I))
rc2 <- matrix(0,  nrow=I, ncol=1000); rc2 <- as.data.frame(rc2); colnames(rc2) <- id

# determine character encoding # SHOULD LEARN TO DO WITH BEAUTIFUL SOUP
encod <- rep(NA, I)
for (i in 1:I){
    info <- readLines( con=filenames[i] , encoding = "utf8" ) ## works for i = 1
    encod[i] <- ifelse( length(grep(info, pattern="Martínez", perl=TRUE))>0, "utf8", NA) # the name should appear in every file
}
for(i in which(is.na(encod)==TRUE)){
    info <- readLines( con=filenames[i] , encoding = "latin1" ) ## works for i = 2
    encod[i] <- ifelse( length(grep(info, pattern="Martínez", perl=TRUE))>0, "latin1", NA) # the name should appear in every file
}
which(is.na(encod)==TRUE) # should be empty

i <- 64; j <- 1 #debug
for (i in 1:I){
    print(paste("Iteration",i,"of",I, sep = " "))
    info <- readLines( con=filenames[i] , encoding = encod[i] ) 
#    info <- readLines( con=filenames[i] , encoding = "utf8" ) ## works for i = 1
#    info <- readLines( con=filenames[i] , encoding = "latin1" ) ## works for i = 2
#    length(info) #debug
#    info[1] #debug
    votdat2$title[i] <- info[4]
#    votdat2$dy[i] <- as.numeric(sub(info[1], pattern=".* SESI[ÓO]N DEL ([0-9]*)(-.*-)([0-9]*).*", replacement="\\1")) # jalaba con i=1, que estandaricé
#    votdat2$yr[i] <- as.numeric(sub(info[1], pattern=".* SESI[ÓO]N DEL ([0-9]*)(-.*-)([0-9]*).*", replacement="\\3")) # jalaba con i=1, que estandaricé
    votdat2$dy[i] <- as.numeric(sub(info[1], pattern="fch = ([0-9]*)(-.*-)([0-9]*)", replacement="\\1")) # jala con i=2
    votdat2$yr[i] <- as.numeric(sub(info[1], pattern="fch = ([0-9]*)(-.*-)([0-9]*)", replacement="\\3")) # jala con i=2
#    tmp <- sub(info[1], pattern=".* SESI[ÓO]N DEL ([0-9]*)(-.*-)([0-9]*).*", replacement="\\2") # jalaba con i=1, que estandaricé
    tmp <- sub(info[1], pattern="fch = ([0-9]*)-(.*)-([0-9]*)", replacement="\\2") # jala con i=2
    tmp <- sub(tmp, pattern="[Ee]nero"     , replacement="1")
    tmp <- sub(tmp, pattern="[Ff]ebrero"   , replacement="2")
    tmp <- sub(tmp, pattern="[Mm]arzo"     , replacement="3")
    tmp <- sub(tmp, pattern="[Aa]bril"     , replacement="4")
    tmp <- sub(tmp, pattern="[Mm]ayo"      , replacement="5")
    tmp <- sub(tmp, pattern="[Jj]unio"     , replacement="6")
    tmp <- sub(tmp, pattern="[Jj]ulio"     , replacement="7")
    tmp <- sub(tmp, pattern="[Aa]gosto"    , replacement="8")
    tmp <- sub(tmp, pattern="[Ss]eptiembre", replacement="9")
    tmp <- sub(tmp, pattern="[Oo]ctubre"   , replacement="10")
    tmp <- sub(tmp, pattern="[Nn]oviembre" , replacement="11")
    tmp <- sub(tmp, pattern="[Dd]iciembre" , replacement="12")
    votdat2$mo[i] <- as.numeric(tmp)
    #
    ## NEEDED IF DEPUTY'S PARTY WILL BE CHECKED
    #panstart <- grep(info, pattern="emmPanVoteStart"); pristart <- grep(info, pattern="emmPriVoteStart")
    #prdstart <- grep(info, pattern="emmPrdVoteStart"); ptstart <- grep(info, pattern="emmPtVoteStart")
    #pvemstart <- grep(info, pattern="emmPvemVoteStart"); convestart <- grep(info, pattern="emmConveVoteStart")
    #panalstart <- grep(info, pattern="emmPanalVoteStart"); indepstart <- grep(info, pattern="emmIndepVoteStart")
    #
    j <- 3 # debug
    for (j in 1:1000){
        #print(c(i,j))
        # info[grep(info, pattern="^(?=.*[Williams|Willy])(?=.*Oswaldo)(?=.*Ochoa)(?=.*Gallegos).*$", perl=TRUE)] # debug
        tmp <- grep(info, pattern=dipdat$nomRegEx[j], perl=TRUE) # DIP'S NAME
        tmp2 <- ifelse( length(tmp)>0, sub(info[tmp+1], pattern=".*<span class.*> (.*)</span></td>", replacement="\\1"), 0)
        tmp2 <- gsub(tmp2, pattern="A favor", replacement="1")
        tmp2 <- gsub(tmp2, pattern="En contra", replacement="2")
        tmp2 <- gsub(tmp2, pattern="Abstención", replacement="3")
        tmp2 <- gsub(tmp2, pattern="Sólo asistencia", replacement="4")
        tmp2 <- gsub(tmp2, pattern="Ausente", replacement="5")
        rc2[i,j] <- as.numeric(tmp2)
    }
    for (j in 1:999){
        ## ASSUMES SUPLENTE IS NEXT OBS
        if (dipdat$dsup[j]==0 & rc2[i,j]==0 & rc2[i,j+1]==0) {rc2[i,j] <- 6} else next
    }
}
## EXCEPTIONS
#
who <- which(id=="oax07p")
rc2[1,who] <- 1 # votó de viva voz en primera votación nominal
#
when <- which(votdat2$yr==2012 & votdat2$mo==9 & votdat2$dy<=27)
who <- which(id=="c3rp35p")
rc2[when, who] <- 0 ## (RACIEL LOPEZ SALAZAR NO JURA (PROCURADOR CPS), SUPLENTE TARDA)
who <- which(id=="c3rp35s")
rc2[when, who] <- 5 ## (RACIEL LOPEZ SALAZAR NO JURA (PROCURADOR CPS), SUPLENTE TARDA)
#
when <- which(votdat2$yr==2012 & votdat2$mo==12 & votdat2$dy>=5)
who <- which(id=="c5rp14p") 
rc2[when, who] <- 0 ## Murillo Karam a PGR, suplente tarda
when <- which(votdat2$yr==2012 & votdat2$mo==12 & votdat2$dy<11)
who <- which(id=="c5rp14s") 
rc2[when, who] <- 0 ## Murillo Karam a PGR, suplente tarda
#
when <- which(votdat2$yr==2013 & votdat2$mo==2 & votdat2$dy>=7)
who <- which(id=="c1rp16p")
rc2[when, who] <- 0 ## (CASTRO TRENTI VA POR GUBER BC, SUPLENTE TARDA)
when <- which(votdat2$yr==2013 & votdat2$mo==2 & votdat2$dy<14)
who <- which(id=="c1rp16s")
rc2[when, who] <- 0 ## (CASTRO TRENTI VA POR GUBER BC, SUPLENTE TARDA)
#
when <- which(votdat2$yr==2013 & votdat2$mo==2 & votdat2$dy>=28)
who <- which(id=="bc03p")
rc2[when, who] <- 0 ## (Hirata VA POR Alcaldía Ensenada, SUPLENTE TARDA)
when <- which(votdat2$yr==2013 & votdat2$mo==3 )
rc2[when, who] <- 0 ## (Hirata VA POR Alcaldía Ensenada, SUPLENTE TARDA)
when <- which(votdat2$yr==2013 & votdat2$mo==3 & votdat2$dy<7)
who <- which(id=="bc03s")
rc2[when, who] <- 0 ## (Hirata VA POR Alcaldía Ensenada, SUPLENTE TARDA)
when <- which(votdat2$yr==2013 & votdat2$mo==2)
rc2[when, who] <- 0 ## (Hirata VA POR Alcaldía Ensenada, SUPLENTE TARDA)
#
when <- which(votdat2$yr==2013 & votdat2$mo==3 & votdat2$dy>=12)
who <- which(id=="bc01p")
rc2[when, who] <- 0 ## (castillo valdez licencia, SUPLENTE TARDA)
when <- which(votdat2$yr==2013 & votdat2$mo==3 & votdat2$dy<19)
who <- which(id=="bc01s")
rc2[when, who] <- 0 ## (castillo valdez licencia, SUPLENTE TARDA)
#
when <- which(votdat2$yr==2013 & votdat2$mo==4 & (votdat2$dy>=4 | votdat2$dy<25))
who <- which(id=="pue03p")
rc2[when, who] <- 0 ## (diez palacios licencia breve sin suplente)
#
when <- which(votdat2$yr==2013 & votdat2$mo==4 & votdat2$dy>=4)
who <- which(id=="pue10p")
rc2[when, who] <- 0 ## (lorenzini licencia, SUPLENTE TARDA)
when <- which(votdat2$yr==2013 & votdat2$mo>4 & votdat2$dy<7)
rc2[when, who] <- 0 
when <- which(votdat2$yr==2013 & votdat2$mo==7 & votdat2$dy<=16)
rc2[when, who] <- 0 
when <- which(votdat2$yr==2013 & votdat2$mo>=4 & votdat2$mo<7)
who <- which(id=="pue10s")
rc2[when, who] <- 0
when <- which(votdat2$yr==2013 & votdat2$mo==7 & votdat2$dy<16)
rc2[when, who] <- 0
#
when <- which(votdat2$yr==2013 & votdat2$mo==4 & (votdat2$dy>=4 | votdat2$dy<25))
who <- which(id=="sin04p")
rc2[when, who] <- 0 ## (camacho ochoa licencia breve sin suplente)
#
when <- which( (votdat2$yr==2013 & votdat2$mo==3 & votdat2$dy>=22) | (votdat2$yr==2013 & votdat2$mo==4) )
who <- which(id=="sin07p")
rc2[when, who] <- 0 ## (torres félix licencia, SUPLENTE TARDA)
when <- which( (votdat2$yr==2013 & votdat2$mo==3) | (votdat2$yr==2013 & votdat2$mo==4 & votdat2$dy<4)  )
who <- which(id=="sin07s")
rc2[when, who] <- 0
#
when <- which( (votdat2$yr==2013 & votdat2$mo==3 & votdat2$dy>=21) | (votdat2$yr==2013 & votdat2$mo==4) )
who <- which(id=="ver12p")
rc2[when, who] <- 0 ## (acosta croda licencia, SUPLENTE TARDA)
when <- which( (votdat2$yr==2013 & votdat2$mo==3) | (votdat2$yr==2013 & votdat2$mo==4 & votdat2$dy<12)  )
who <- which(id=="ver12s")
rc2[when, who] <- 0
#
when <- which( votdat2$yr==2013 & votdat2$mo==4 & votdat2$dy>=5 )
who <- which(id=="ver15p")
rc2[when, who] <- 0 ## (diez francos licencia, SUPLENTE TARDA)
when <- which( votdat2$yr==2013 & votdat2$mo==4 & votdat2$dy<18  )
who <- which(id=="ver15s")
rc2[when, who] <- 0
#
when <- which( (votdat2$yr==2013 & votdat2$mo==3 & votdat2$dy>=21) | (votdat2$yr==2013 & votdat2$mo>3 & votdat2$mo<10) | (votdat2$yr==2013 & votdat2$mo==10 & votdat2$dy<=24) )
who <- which(id=="ver21p")
rc2[when, who] <- 0 ## (parissi licencia, SUPLENTE TARDA)
when <- which( (votdat2$yr==2013 & votdat2$mo>=3 & votdat2$mo<10) | (votdat2$yr==2013 & votdat2$mo==10 & votdat2$dy<24) )
who <- which(id=="ver21s")
rc2[when, who] <- 0
#
when <- which( (votdat2$yr==2013 & votdat2$mo==9 & votdat2$dy>=26) | (votdat2$yr==2013 & votdat2$mo==10) )
who <- which(id=="c2rp17p")
rc2[when, who] <- 0 ## (lázara nelly licencia, SUPLENTE TARDA)
when <- which( votdat2$yr==2013 & votdat2$mo==9 & votdat2$mo<26  )
who <- which(id=="c2rp17s")
rc2[when, who] <- 0
#
when <- which( votdat2$filename=="62yr2or2-sil-440" )
who <- which(id=="bc07p" | id=="cps09p" | id=="gua06p" | id=="zac04p" | id=="c2rp39p" | id=="c3rp36p" | id=="c4rp37p" | id=="c5rp36p" | id=="c5rp37p")
rc2[when, who] <- 4 # do not appear in this roll call vote, all voted aye in previous
#
when <- which( (votdat2$yr==2014 & votdat2$mo==3 & votdat2$dy>=6) )
who <- which(id=="c2rp11p")
rc2[when, who] <- 0 ## (ricardo anaya licencia, SUPLENTE TARDA)
when <- which( votdat2$yr==2014 & votdat2$mo==3 & votdat2$mo<6  )
who <- which(id=="c2rp11s")
rc2[when, who] <- 0
#
when <- which(votdat2$yr==2014 & votdat2$mo==7 & votdat2$dy>=8 & votdat2$dy<=28)
who <- which(id=="c4rp9p")
rc2[when, who] <- 0 ## (Leonor Romero Sevilla solicitó licencia temporal sin que suplente asumiera)
who <- which(id=="c4rp9s")
rc2[when, who] <- 0 
#
when <- which(votdat2$yr==2014 & votdat2$mo==7 & votdat2$dy>=8 & votdat2$dy<=28)
who <- which(id=="c4rp10p")
rc2[when, who] <- 0  ## (Aurora de la Luz Aguilar Rodríguez solicitó licencia temporal sin que suplente asumiera)
who <- which(id=="c4rp10s")
rc2[when, who] <- 0  
#
when <- which( (votdat2$yr==2014 & votdat2$mo==9 & votdat2$dy==17) )
who <- which(id=="c1rp29p")
rc2[when, who] <- 0  ## (Rodrigo González Barrios solicitó licencia, suplente no llegó al primer voto)
who <- which(id=="c1rp29s")
rc2[when, who] <- 5
#
when <- which(votdat2$filename=="sil62vot440")
who <- which(id=="bc07p" | id=="cps09p" | id=="gua06p" | id=="zac04p" | id=="c2rp39p" | id=="c3rp36p" | id=="c4rp37p" | id=="c5rp36p" | id=="c5rp37p")
rc2[when, who] <- 0  ## no aparecen en ese voto
#
when <- which( (votdat2$yr==2014 & votdat2$mo==9 & votdat2$dy>=4 & votdat2$dy<=16) )
who <- which(id=="pue08p")
rc2[when, who] <- 0  ## (Allende Cano licencia de 20 días y vuelve)


# check if sixes are still present
max(rc2)


## USADOS PARA DEBUG SEISES
d<-0
d <- d+1; paste(d, id[d], dipdat$nom[d]); rc2[,d]; votdat2$filename[rc2[,d]==6] ## ÚLT LISTA SOLO LOS SEISES

v <- 475
v <- v+1; paste(v, votdat2$filename[v], id[rc2[v,]==6], dipdat$nom[rc2[v,]==6]); table(as.numeric(rc2[v,]))
votdat2[v,]
rc2[v,]
z
#v <- 2; paste(v, votdat2$filename[v], id[rc2[v,]==6], dipdat$nomRegEx[rc2[v,]==6]); table(as.numeric(rc2[v,]))

# find roll calls with sixes
tmp <- apply( rc2, 1, function(x) length(which(x==6)) )
tmp <- tmp[tmp>0]


as.numeric(rc[,id=="mexrp02p"]) ## UN DIPUTADO
as.numeric(rc[v,]) ## EL VOTO

head(votdat2)

# compute vote totals
votdat2$favor <- apply( rc2, 1, function(x) length(which(x==1)) )
votdat2$contra <- apply( rc2, 1, function(x) length(which(x==2)) )
votdat2$absten <- apply( rc2, 1, function(x) length(which(x==3)) )
votdat2$quorum <- apply( rc2, 1, function(x) length(which(x==4)) )
votdat2$ausen <- apply( rc2, 1, function(x) length(which(x==5)) )
#votdat2[1:5, c("favor","contra")]

ls()
save.image( paste(wd, "votesForWeb", "rc62til7oct2014.RData", sep="/") )

# fix 1st title
#votdat2$title[votdat2$filename=="62yr1or1-sil-1"] <- gsub(pattern = "^.*<BR> (.*)</B>.*", replacement = "\\1", votdat2$title[1])
votdat2$title[votdat2$filename=="sil62vot1"] <- gsub(pattern = "^.*<BR> (.*)</B>.*", replacement = "\\1", votdat2$title[1])
# cleaning
rm(encod, filenames, i, I, id, info, j, leg, names, names.alt, pty, tmp, tmp2, when, who, v)
rc <- rc2; votdat <- votdat2;
#rc <- rbind(rc, rc2); votdat <- rbind(votdat, votdat2); # if adding more votes
rm(rc2, votdat2)
ls()

save.image( paste(wd, "votesForWeb", "rc62til7oct2014.RData", sep="/") )
# csv versions
write.table(dipdat, file = paste(wd, "votesForWeb", "dipdat62.csv", sep="/"), sep=",", row.names = FALSE)
write.table(votdat, file = paste(wd, "votesForWeb", "votdat62.csv", sep="/"), sep=",", row.names = FALSE)
write.table(rc, file = paste(wd, "votesForWeb", "rc62.csv", sep="/"), sep=",", row.names = FALSE)






## WORK IN PROGRESS ## PARTY AGGREGATES TO VERIFY THEY MATCH
VOTES tmp <- rep(NA, times=length(filenames)) tots <-
data.frame(aypan=tmp, nypan=tmp, abpan=tmp, qupan=tmp,
aspan=tmp,
                   aypri=tmp, nypri=tmp, abpri=tmp, qupri=tmp, aspri=tmp,
                   ayprd=tmp, nyprd=tmp, abprd=tmp, quprd=tmp, asprd=tmp,
                   aypt=tmp,  nypt=tmp,  abpt=tmp,  qupt=tmp,  aspt=tmp,
                   aypvem=tmp,nypvem=tmp,abpvem=tmp,qupvem=tmp,aspvem=tmp,
                   ayconve=tmp, nyconve=tmp, abconve=tmp, quconve=tmp, asconve=tmp,
                   aypas=tmp, nypas=tmp, abpas=tmp, qupas=tmp, aspas=tmp,
                   aypsn=tmp, nypsn=tmp, abpsn=tmp, qupsn=tmp, aspsn=tmp,
                   ayasd=tmp, nyasd=tmp, abasd=tmp, quasd=tmp, asasd=tmp,
                   aypanal=tmp, nypanal=tmp, abpanal=tmp, qupanal=tmp, aspanal=tmp,
                   aysp=tmp, nysp=tmp, absp=tmp, qusp=tmp, assp=tmp,
                   aytot=tmp, nytot=tmp, abtot=tmp, qutot=tmp, astot=tmp,
                   problem=tmp)
#
allpart <- c("Partido Acción Nacional",
             "Partido Revolucionario Institucional",
             "Partido de la Revolución Democrática")
work <- 1:length(filenames); work <- work[votdat$haveinfo==1]
n <- 13
for (n in work){
fil <- readLines(filenames[n])
ayloc <- grep(fil, pattern="emmAyesStart")
nyloc <- grep(fil, pattern="emmNaysStart")
abloc <- grep(fil, pattern="emmAbstentionsStart")
quloc <- grep(fil, pattern="emmPresentNoVoteStart")
asloc <- grep(fil, pattern="emmAbsencesStart")
enloc <- grep(fil, pattern="emmFileEnds")
ayloc <- c((ayloc+1):(nyloc-1))
nyloc <- c((nyloc+1):(abloc-1))
abloc <- c((abloc+1):(quloc-1))
quloc <- c((quloc+1):(asloc-1))
asloc <- c((asloc+1):(enloc-1))
rm(enloc)
#
fil <- sub(fil, pattern="<FONT COLOR=#990000><center>", replacement="")
##tmp2 <- grep(fil[ayloc], pattern="Diputados .* que.*")                    ##
##tmp <- fil[ayloc]                                                         ##
##tmp <- sub(tmp[tmp2], pattern="Diputados (.*) que.*", replacement="\\1")  ##
##tmp <- sub(tmp, pattern="del Partido", replacement="Partido")             ##
##allpart <- append(allpart, tmp)                                           ##
##} ## UNSTAR SEGMENT & END LOOP HERE TO GET PARTY LABEL COUNT              ##
##table(allpart)                                                            ##
##    ##LAST RESULT allpart
##    ##                     de Convergencia    de Convergencia por la Democracia
##    ##                      Independientes              Partido Acción Nacional
##    ##              Partido Alianza Social                  Partido Alternativa
##    ##Partido de la Revolución Democrática                  Partido del Trabajo
##    ##               Partido Nueva Alianza Partido Revolucionario Institucional
##    ##       Partido Sociedad Nacionalista   Partido Verde Ecologista de México
##    ##                         sin partido
tmp <- fil[ayloc]
subset <- tmp[grep(tmp, pattern=".*Partido Acción Nacional.*")]
tots$aypan[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Revolucionario Institucional.*")]
tots$aypri[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido de la Revolución Democrática.*")]
tots$ayprd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido del Trabajo.*")]
tots$aypt[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Nueva Alianza.*")]
tots$aypanal[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Verde.*")]
tots$aypvem[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Convergencia.*")]
tots$ayconve[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Alianza Social.*")]
tots$aypas[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Sociedad Nacionalista.*")]
tots$aypsn[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Alternativa.*")]
tots$ayasd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*[Iis][ni][dn][e ]p[ea][nr][dt]i[ed][no].*")]
tots$aysp[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
tmp <- fil[nyloc]
subset <- tmp[grep(tmp, pattern=".*Partido Acción Nacional.*")]
tots$nypan[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Revolucionario Institucional.*")]
tots$nypri[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido de la Revolución Democrática.*")]
tots$nyprd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido del Trabajo.*")]
tots$nypt[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Nueva Alianza.*")]
tots$nypanal[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Verde.*")]
tots$nypvem[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Convergencia.*")]
tots$nyconve[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Alianza Social.*")]
tots$nypas[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Sociedad Nacionalista.*")]
tots$nypsn[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Alternativa.*")]
tots$nyasd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*[Iis][ni][dn][e ]p[ea][nr][dt]i[ed][no].*")]
tots$nysp[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
tmp <- fil[abloc]
subset <- tmp[grep(tmp, pattern=".*Partido Acción Nacional.*")]
tots$abpan[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Revolucionario Institucional.*")]
tots$abpri[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido de la Revolución Democrática.*")]
tots$abprd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido del Trabajo.*")]
tots$abpt[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Nueva Alianza.*")]
tots$abpanal[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Verde.*")]
tots$abpvem[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Convergencia.*")]
tots$abconve[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Alianza Social.*")]
tots$abpas[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Sociedad Nacionalista.*")]
tots$abpsn[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Alternativa.*")]
tots$abasd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*[Iis][ni][dn][e ]p[ea][nr][dt]i[ed][no].*")]
tots$absp[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
tmp <- fil[quloc]
subset <- tmp[grep(tmp, pattern=".*Partido Acción Nacional.*")]
tots$qupan[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Revolucionario Institucional.*")]
tots$qupri[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido de la Revolución Democrática.*")]
tots$quprd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido del Trabajo.*")]
tots$qupt[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Nueva Alianza.*")]
tots$qupanal[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Verde.*")]
tots$qupvem[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Convergencia.*")]
tots$quconve[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Alianza Social.*")]
tots$qupas[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Sociedad Nacionalista.*")]
tots$qupsn[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Alternativa.*")]
tots$quasd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*[Iis][ni][dn][e ]p[ea][nr][dt]i[ed][no].*")]
tots$qusp[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
tmp <- fil[asloc]
subset <- tmp[grep(tmp, pattern=".*Partido Acción Nacional.*")]
tots$aspan[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Revolucionario Institucional.*")]
tots$aspri[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido de la Revolución Democrática.*")]
tots$asprd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido del Trabajo.*")]
tots$aspt[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Nueva Alianza.*")]
tots$aspanal[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Verde.*")]
tots$aspvem[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Convergencia.*")]
tots$asconve[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Partido Alianza Social.*")]
tots$aspas[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Sociedad Nacionalista.*")]
tots$aspsn[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*Alternativa.*")]
tots$asasd[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
subset <- tmp[grep(tmp, pattern=".*[Iis][ni][dn][e ]p[ea][nr][dt]i[ed][no].*")]
tots$assp[n] <- ifelse(length(subset)>0,
        sub(subset, pattern=".*: ([0-9]*)<.*", replacement="\\1"), NA)
}

save.image(paste(wd, "/tmp.RData", sep=""))
bkp <- tots

tots <- bkp
for (n in 1:ncol(tots)){
    tots[,n] <- as.numeric(tots[,n])
    }
tmp <- tots
tmp[is.na(tmp)] <- 0
tots$aytot <- tmp$aypan+  tmp$aypri+  tmp$ayprd+  tmp$aypt+
              tmp$aypvem+ tmp$ayconve+ tmp$aypas+  tmp$aypsn+  tmp$ayasd+
              tmp$aypanal+ tmp$aysp
tots$nytot <- tmp$nypan+  tmp$nypri+  tmp$nyprd+  tmp$nypt+
              tmp$nypvem+ tmp$nyconve+ tmp$nypas+  tmp$nypsn+  tmp$nyasd+
              tmp$nypanal+ tmp$nysp
tots$abtot <- tmp$abpan+  tmp$abpri+  tmp$abprd+  tmp$abpt+
              tmp$abpvem+ tmp$abconve+ tmp$abpas+  tmp$abpsn+  tmp$abasd+
              tmp$abpanal+ tmp$absp
tots$qutot <- tmp$qupan+   tmp$qupri+   tmp$quprd+   tmp$qupt+
              tmp$qupvem+  tmp$quconve+ tmp$qupas+   tmp$qupsn+   tmp$quasd+
              tmp$qupanal+ tmp$qusp
tots$astot <- tmp$aspan+ tmp$aspri+ tmp$asprd+ tmp$aspt+
              tmp$aspvem+ tmp$asconve+ tmp$aspas+ tmp$aspsn+ tmp$asasd+
              tmp$aspanal+ tmp$assp
tots$tottot <- tots$aytot + tots$nytot + tots$abtot + tots$qutot + tots$astot
#
tots$problem <- 0
tots$problem[ tots$aytot != votdat$favor ]  <- 1
sum(tots$problem)
tots$problem[ tots$nytot != votdat$contra ] <- 1
sum(tots$problem)
tots$problem[ tots$abtot != votdat$absten ] <- 1
sum(tots$problem)
tots$problem[ tots$qutot != votdat$quorum ] <- 1
sum(tots$problem)
tots$problem[ tots$astot != votdat$ausen ]  <- 1
sum(tots$problem)

tmp <- data.frame(leg=votdat$leg, per=votdat$filename, day=tots$aytot - votdat$favor, ayes=tots$aytot, fav=votdat$favor,
dny=tots$nytot - votdat$contra, nayes=tots$nytot, con=votdat$contra)
tmp[votdat$haveinfo==1 & tots$problem==1,]

BUSCAR PROPIETARIO O SUPLENTE: SI AMBOS EN CERO, DIFFERENT SPELLING

## CONDITIONAL EXAMPLE
#    if (votdat$favor[n] > 0)
#       { direc <- c("http://gaceta.diputados.gob.mx/voto60/ordi22/8/21.lola")
#         url <- url(direc,open="rt")
#         tmp <- append(tmp, readLines(url, warn=FALSE))
#         close(url) } else next
#    tmp <- append(tmp, c("emmAyesEnd"))
