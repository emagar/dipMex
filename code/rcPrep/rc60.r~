rm(list = ls())
#
## SET YOUR WORKING DIRECTORY HERE (SAVE DATA FILES IN THIS DIRECTORY)
workdir <- c("~/Dropbox/data/rollcall/dipMex")
#workdir <- c("d:/01/Dropbox/data/rollcall/DipMex")
#workdir <- c("C:/Documents and Settings/emagarm/Mis documentos/My Dropbox/data/rollcall/DipMex")
#workdir <- c("C:/Documents and Settings/emm/Mis documentos/My Dropbox/data/rollcall/DipMex")
setwd(workdir)

# if only loading saved data
load(file=paste(workdir, "data/votesForWeb", "rc60.RData", sep="/"))

## IMPORT DIPUTADO NAMES AND DISTRICTS (PREPARED IN EXCEL)
dipdat <- read.csv( paste(workdir, "data/diputados/dip60.csv", sep="/"), header=TRUE)  ## PREPARED IN EXCEL, SEE 61ST MODEL
colnames(dipdat)
dipdat$idOldDrop <- NULL  # drop old IDs

##########################
## 6O LEG FROM INFOSIL  ##
##########################
#
# Import filenames
load(paste(workdir, "data/infosilFilenames60.RData",sep="/"))
#
## object filenames prepared for windows, change to linux path if necessary
#filenames <- sub(pattern = "d:/01/dropbox/data/rollcall/DipMex", replacement = "~/Dropbox/data/rollcall/dipMex", filenames)
#

## # convert to uft-8 RUN DIRECTLY FROM CONSOLE IF NEEDED (IT SHOULD NOT)
## iconv -f Latin1 -t utf-8 "sil60vot1.txt"   > "sil60vot1.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot2.txt"   > "sil60vot2.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot3.txt"   > "sil60vot3.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot4.txt"   > "sil60vot4.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot5.txt"   > "sil60vot5.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot6.txt"   > "sil60vot6.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot7.txt"   > "sil60vot7.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot8.txt"   > "sil60vot8.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot9.txt"   > "sil60vot9.txt"  
## iconv -f Latin1 -t utf-8 "sil60vot10.txt"  > "sil60vot10.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot11.txt"  > "sil60vot11.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot12.txt"  > "sil60vot12.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot13.txt"  > "sil60vot13.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot14.txt"  > "sil60vot14.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot15.txt"  > "sil60vot15.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot16.txt"  > "sil60vot16.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot17.txt"  > "sil60vot17.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot18.txt"  > "sil60vot18.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot19.txt"  > "sil60vot19.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot20.txt"  > "sil60vot20.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot21.txt"  > "sil60vot21.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot22.txt"  > "sil60vot22.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot23.txt"  > "sil60vot23.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot24.txt"  > "sil60vot24.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot25.txt"  > "sil60vot25.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot26.txt"  > "sil60vot26.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot27.txt"  > "sil60vot27.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot28.txt"  > "sil60vot28.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot29.txt"  > "sil60vot29.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot30.txt"  > "sil60vot30.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot31.txt"  > "sil60vot31.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot32.txt"  > "sil60vot32.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot33.txt"  > "sil60vot33.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot34.txt"  > "sil60vot34.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot35.txt"  > "sil60vot35.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot36.txt"  > "sil60vot36.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot37.txt"  > "sil60vot37.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot38.txt"  > "sil60vot38.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot39.txt"  > "sil60vot39.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot40.txt"  > "sil60vot40.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot41.txt"  > "sil60vot41.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot42.txt"  > "sil60vot42.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot43.txt"  > "sil60vot43.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot44.txt"  > "sil60vot44.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot45.txt"  > "sil60vot45.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot46.txt"  > "sil60vot46.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot47.txt"  > "sil60vot47.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot48.txt"  > "sil60vot48.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot49.txt"  > "sil60vot49.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot50.txt"  > "sil60vot50.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot51.txt"  > "sil60vot51.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot52.txt"  > "sil60vot52.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot53.txt"  > "sil60vot53.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot54.txt"  > "sil60vot54.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot55.txt"  > "sil60vot55.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot56.txt"  > "sil60vot56.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot57.txt"  > "sil60vot57.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot58.txt"  > "sil60vot58.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot59.txt"  > "sil60vot59.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot60.txt"  > "sil60vot60.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot61.txt"  > "sil60vot61.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot62.txt"  > "sil60vot62.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot63.txt"  > "sil60vot63.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot64.txt"  > "sil60vot64.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot65.txt"  > "sil60vot65.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot66.txt"  > "sil60vot66.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot67.txt"  > "sil60vot67.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot68.txt"  > "sil60vot68.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot69.txt"  > "sil60vot69.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot70.txt"  > "sil60vot70.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot71.txt"  > "sil60vot71.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot72.txt"  > "sil60vot72.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot73.txt"  > "sil60vot73.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot74.txt"  > "sil60vot74.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot75.txt"  > "sil60vot75.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot76.txt"  > "sil60vot76.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot77.txt"  > "sil60vot77.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot78.txt"  > "sil60vot78.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot79.txt"  > "sil60vot79.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot80.txt"  > "sil60vot80.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot81.txt"  > "sil60vot81.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot82.txt"  > "sil60vot82.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot83.txt"  > "sil60vot83.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot84.txt"  > "sil60vot84.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot85.txt"  > "sil60vot85.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot86.txt"  > "sil60vot86.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot87.txt"  > "sil60vot87.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot88.txt"  > "sil60vot88.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot89.txt"  > "sil60vot89.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot90.txt"  > "sil60vot90.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot91.txt"  > "sil60vot91.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot92.txt"  > "sil60vot92.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot93.txt"  > "sil60vot93.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot94.txt"  > "sil60vot94.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot95.txt"  > "sil60vot95.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot96.txt"  > "sil60vot96.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot97.txt"  > "sil60vot97.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot98.txt"  > "sil60vot98.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot99.txt"  > "sil60vot99.txt" 
## iconv -f Latin1 -t utf-8 "sil60vot100.txt" > "sil60vot100.txt"
## iconv -f Latin1 -t utf-8 "sil60vot101.txt" > "sil60vot101.txt"
## iconv -f Latin1 -t utf-8 "sil60vot102.txt" > "sil60vot102.txt"
## iconv -f Latin1 -t utf-8 "sil60vot103.txt" > "sil60vot103.txt"
## iconv -f Latin1 -t utf-8 "sil60vot104.txt" > "sil60vot104.txt"
## iconv -f Latin1 -t utf-8 "sil60vot105.txt" > "sil60vot105.txt"
## iconv -f Latin1 -t utf-8 "sil60vot106.txt" > "sil60vot106.txt"
## iconv -f Latin1 -t utf-8 "sil60vot107.txt" > "sil60vot107.txt"
## iconv -f Latin1 -t utf-8 "sil60vot108.txt" > "sil60vot108.txt"
## iconv -f Latin1 -t utf-8 "sil60vot109.txt" > "sil60vot109.txt"
## iconv -f Latin1 -t utf-8 "sil60vot110.txt" > "sil60vot110.txt"
## iconv -f Latin1 -t utf-8 "sil60vot111.txt" > "sil60vot111.txt"
## iconv -f Latin1 -t utf-8 "sil60vot112.txt" > "sil60vot112.txt"
## iconv -f Latin1 -t utf-8 "sil60vot113.txt" > "sil60vot113.txt"
## iconv -f Latin1 -t utf-8 "sil60vot114.txt" > "sil60vot114.txt"
## iconv -f Latin1 -t utf-8 "sil60vot115.txt" > "sil60vot115.txt"
## iconv -f Latin1 -t utf-8 "sil60vot116.txt" > "sil60vot116.txt"
## iconv -f Latin1 -t utf-8 "sil60vot117.txt" > "sil60vot117.txt"
## iconv -f Latin1 -t utf-8 "sil60vot118.txt" > "sil60vot118.txt"
## iconv -f Latin1 -t utf-8 "sil60vot119.txt" > "sil60vot119.txt"
## iconv -f Latin1 -t utf-8 "sil60vot120.txt" > "sil60vot120.txt"
## iconv -f Latin1 -t utf-8 "sil60vot121.txt" > "sil60vot121.txt"
## iconv -f Latin1 -t utf-8 "sil60vot122.txt" > "sil60vot122.txt"
## iconv -f Latin1 -t utf-8 "sil60vot123.txt" > "sil60vot123.txt"
## iconv -f Latin1 -t utf-8 "sil60vot124.txt" > "sil60vot124.txt"
## iconv -f Latin1 -t utf-8 "sil60vot125.txt" > "sil60vot125.txt"
## iconv -f Latin1 -t utf-8 "sil60vot126.txt" > "sil60vot126.txt"
## iconv -f Latin1 -t utf-8 "sil60vot127.txt" > "sil60vot127.txt"
## iconv -f Latin1 -t utf-8 "sil60vot128.txt" > "sil60vot128.txt"
## iconv -f Latin1 -t utf-8 "sil60vot129.txt" > "sil60vot129.txt"
## iconv -f Latin1 -t utf-8 "sil60vot130.txt" > "sil60vot130.txt"
## iconv -f Latin1 -t utf-8 "sil60vot131.txt" > "sil60vot131.txt"
## iconv -f Latin1 -t utf-8 "sil60vot132.txt" > "sil60vot132.txt"
## iconv -f Latin1 -t utf-8 "sil60vot133.txt" > "sil60vot133.txt"
## iconv -f Latin1 -t utf-8 "sil60vot134.txt" > "sil60vot134.txt"
## iconv -f Latin1 -t utf-8 "sil60vot135.txt" > "sil60vot135.txt"
## iconv -f Latin1 -t utf-8 "sil60vot136.txt" > "sil60vot136.txt"
## iconv -f Latin1 -t utf-8 "sil60vot137.txt" > "sil60vot137.txt"
## iconv -f Latin1 -t utf-8 "sil60vot138.txt" > "sil60vot138.txt"
## iconv -f Latin1 -t utf-8 "sil60vot139.txt" > "sil60vot139.txt"
## iconv -f Latin1 -t utf-8 "sil60vot140.txt" > "sil60vot140.txt"
## iconv -f Latin1 -t utf-8 "sil60vot141.txt" > "sil60vot141.txt"
## iconv -f Latin1 -t utf-8 "sil60vot142.txt" > "sil60vot142.txt"
## iconv -f Latin1 -t utf-8 "sil60vot143.txt" > "sil60vot143.txt"
## iconv -f Latin1 -t utf-8 "sil60vot144.txt" > "sil60vot144.txt"
## iconv -f Latin1 -t utf-8 "sil60vot145.txt" > "sil60vot145.txt"
## iconv -f Latin1 -t utf-8 "sil60vot146.txt" > "sil60vot146.txt"
## iconv -f Latin1 -t utf-8 "sil60vot147.txt" > "sil60vot147.txt"
## iconv -f Latin1 -t utf-8 "sil60vot148.txt" > "sil60vot148.txt"
## iconv -f Latin1 -t utf-8 "sil60vot149.txt" > "sil60vot149.txt"
## iconv -f Latin1 -t utf-8 "sil60vot150.txt" > "sil60vot150.txt"
## iconv -f Latin1 -t utf-8 "sil60vot151.txt" > "sil60vot151.txt"
## iconv -f Latin1 -t utf-8 "sil60vot152.txt" > "sil60vot152.txt"
## iconv -f Latin1 -t utf-8 "sil60vot153.txt" > "sil60vot153.txt"
## iconv -f Latin1 -t utf-8 "sil60vot154.txt" > "sil60vot154.txt"
## iconv -f Latin1 -t utf-8 "sil60vot155.txt" > "sil60vot155.txt"
## iconv -f Latin1 -t utf-8 "sil60vot156.txt" > "sil60vot156.txt"
## iconv -f Latin1 -t utf-8 "sil60vot157.txt" > "sil60vot157.txt"
## iconv -f Latin1 -t utf-8 "sil60vot158.txt" > "sil60vot158.txt"
## iconv -f Latin1 -t utf-8 "sil60vot159.txt" > "sil60vot159.txt"
## iconv -f Latin1 -t utf-8 "sil60vot160.txt" > "sil60vot160.txt"
## iconv -f Latin1 -t utf-8 "sil60vot161.txt" > "sil60vot161.txt"
## iconv -f Latin1 -t utf-8 "sil60vot162.txt" > "sil60vot162.txt"
## iconv -f Latin1 -t utf-8 "sil60vot163.txt" > "sil60vot163.txt"
## iconv -f Latin1 -t utf-8 "sil60vot164.txt" > "sil60vot164.txt"
## iconv -f Latin1 -t utf-8 "sil60vot165.txt" > "sil60vot165.txt"
## iconv -f Latin1 -t utf-8 "sil60vot166.txt" > "sil60vot166.txt"
## iconv -f Latin1 -t utf-8 "sil60vot167.txt" > "sil60vot167.txt"
## iconv -f Latin1 -t utf-8 "sil60vot168.txt" > "sil60vot168.txt"
## iconv -f Latin1 -t utf-8 "sil60vot169.txt" > "sil60vot169.txt"
## iconv -f Latin1 -t utf-8 "sil60vot170.txt" > "sil60vot170.txt"
## iconv -f Latin1 -t utf-8 "sil60vot171.txt" > "sil60vot171.txt"
## iconv -f Latin1 -t utf-8 "sil60vot172.txt" > "sil60vot172.txt"
## iconv -f Latin1 -t utf-8 "sil60vot173.txt" > "sil60vot173.txt"
## iconv -f Latin1 -t utf-8 "sil60vot174.txt" > "sil60vot174.txt"
## iconv -f Latin1 -t utf-8 "sil60vot175.txt" > "sil60vot175.txt"
## iconv -f Latin1 -t utf-8 "sil60vot176.txt" > "sil60vot176.txt"
## iconv -f Latin1 -t utf-8 "sil60vot177.txt" > "sil60vot177.txt"
## iconv -f Latin1 -t utf-8 "sil60vot178.txt" > "sil60vot178.txt"
## iconv -f Latin1 -t utf-8 "sil60vot179.txt" > "sil60vot179.txt"
## iconv -f Latin1 -t utf-8 "sil60vot180.txt" > "sil60vot180.txt"
## iconv -f Latin1 -t utf-8 "sil60vot181.txt" > "sil60vot181.txt"
## iconv -f Latin1 -t utf-8 "sil60vot182.txt" > "sil60vot182.txt"
## iconv -f Latin1 -t utf-8 "sil60vot183.txt" > "sil60vot183.txt"
## iconv -f Latin1 -t utf-8 "sil60vot184.txt" > "sil60vot184.txt"
## iconv -f Latin1 -t utf-8 "sil60vot185.txt" > "sil60vot185.txt"
## iconv -f Latin1 -t utf-8 "sil60vot186.txt" > "sil60vot186.txt"
## iconv -f Latin1 -t utf-8 "sil60vot187.txt" > "sil60vot187.txt"
## iconv -f Latin1 -t utf-8 "sil60vot188.txt" > "sil60vot188.txt"
## iconv -f Latin1 -t utf-8 "sil60vot189.txt" > "sil60vot189.txt"
## iconv -f Latin1 -t utf-8 "sil60vot190.txt" > "sil60vot190.txt"
## iconv -f Latin1 -t utf-8 "sil60vot191.txt" > "sil60vot191.txt"
## iconv -f Latin1 -t utf-8 "sil60vot192.txt" > "sil60vot192.txt"
## iconv -f Latin1 -t utf-8 "sil60vot193.txt" > "sil60vot193.txt"
## iconv -f Latin1 -t utf-8 "sil60vot194.txt" > "sil60vot194.txt"
## iconv -f Latin1 -t utf-8 "sil60vot195.txt" > "sil60vot195.txt"
## iconv -f Latin1 -t utf-8 "sil60vot196.txt" > "sil60vot196.txt"
## iconv -f Latin1 -t utf-8 "sil60vot197.txt" > "sil60vot197.txt"
## iconv -f Latin1 -t utf-8 "sil60vot198.txt" > "sil60vot198.txt"
## iconv -f Latin1 -t utf-8 "sil60vot199.txt" > "sil60vot199.txt"
## iconv -f Latin1 -t utf-8 "sil60vot200.txt" > "sil60vot200.txt"
## iconv -f Latin1 -t utf-8 "sil60vot201.txt" > "sil60vot201.txt"
## iconv -f Latin1 -t utf-8 "sil60vot202.txt" > "sil60vot202.txt"
## iconv -f Latin1 -t utf-8 "sil60vot203.txt" > "sil60vot203.txt"
## iconv -f Latin1 -t utf-8 "sil60vot204.txt" > "sil60vot204.txt"
## iconv -f Latin1 -t utf-8 "sil60vot205.txt" > "sil60vot205.txt"
## iconv -f Latin1 -t utf-8 "sil60vot206.txt" > "sil60vot206.txt"
## iconv -f Latin1 -t utf-8 "sil60vot207.txt" > "sil60vot207.txt"
## iconv -f Latin1 -t utf-8 "sil60vot208.txt" > "sil60vot208.txt"
## iconv -f Latin1 -t utf-8 "sil60vot209.txt" > "sil60vot209.txt"
## iconv -f Latin1 -t utf-8 "sil60vot210.txt" > "sil60vot210.txt"
## iconv -f Latin1 -t utf-8 "sil60vot211.txt" > "sil60vot211.txt"
## iconv -f Latin1 -t utf-8 "sil60vot212.txt" > "sil60vot212.txt"
## iconv -f Latin1 -t utf-8 "sil60vot213.txt" > "sil60vot213.txt"
## iconv -f Latin1 -t utf-8 "sil60vot214.txt" > "sil60vot214.txt"
## iconv -f Latin1 -t utf-8 "sil60vot215.txt" > "sil60vot215.txt"
## iconv -f Latin1 -t utf-8 "sil60vot216.txt" > "sil60vot216.txt"
## iconv -f Latin1 -t utf-8 "sil60vot217.txt" > "sil60vot217.txt"
## iconv -f Latin1 -t utf-8 "sil60vot218.txt" > "sil60vot218.txt"
## iconv -f Latin1 -t utf-8 "sil60vot219.txt" > "sil60vot219.txt"
## iconv -f Latin1 -t utf-8 "sil60vot220.txt" > "sil60vot220.txt"
## iconv -f Latin1 -t utf-8 "sil60vot221.txt" > "sil60vot221.txt"
## iconv -f Latin1 -t utf-8 "sil60vot222.txt" > "sil60vot222.txt"
## iconv -f Latin1 -t utf-8 "sil60vot223.txt" > "sil60vot223.txt"
## iconv -f Latin1 -t utf-8 "sil60vot224.txt" > "sil60vot224.txt"
## iconv -f Latin1 -t utf-8 "sil60vot225.txt" > "sil60vot225.txt"
## iconv -f Latin1 -t utf-8 "sil60vot226.txt" > "sil60vot226.txt"
## iconv -f Latin1 -t utf-8 "sil60vot227.txt" > "sil60vot227.txt"
## iconv -f Latin1 -t utf-8 "sil60vot228.txt" > "sil60vot228.txt"
## iconv -f Latin1 -t utf-8 "sil60vot229.txt" > "sil60vot229.txt"
## iconv -f Latin1 -t utf-8 "sil60vot230.txt" > "sil60vot230.txt"
## iconv -f Latin1 -t utf-8 "sil60vot231.txt" > "sil60vot231.txt"
## iconv -f Latin1 -t utf-8 "sil60vot232.txt" > "sil60vot232.txt"
## iconv -f Latin1 -t utf-8 "sil60vot233.txt" > "sil60vot233.txt"
## iconv -f Latin1 -t utf-8 "sil60vot234.txt" > "sil60vot234.txt"
## iconv -f Latin1 -t utf-8 "sil60vot235.txt" > "sil60vot235.txt"
## iconv -f Latin1 -t utf-8 "sil60vot236.txt" > "sil60vot236.txt"
## iconv -f Latin1 -t utf-8 "sil60vot237.txt" > "sil60vot237.txt"
## iconv -f Latin1 -t utf-8 "sil60vot238.txt" > "sil60vot238.txt"
## iconv -f Latin1 -t utf-8 "sil60vot239.txt" > "sil60vot239.txt"
## iconv -f Latin1 -t utf-8 "sil60vot240.txt" > "sil60vot240.txt"
## iconv -f Latin1 -t utf-8 "sil60vot241.txt" > "sil60vot241.txt"
## iconv -f Latin1 -t utf-8 "sil60vot242.txt" > "sil60vot242.txt"
## iconv -f Latin1 -t utf-8 "sil60vot243.txt" > "sil60vot243.txt"
## iconv -f Latin1 -t utf-8 "sil60vot244.txt" > "sil60vot244.txt"
## iconv -f Latin1 -t utf-8 "sil60vot245.txt" > "sil60vot245.txt"
## iconv -f Latin1 -t utf-8 "sil60vot246.txt" > "sil60vot246.txt"
## iconv -f Latin1 -t utf-8 "sil60vot247.txt" > "sil60vot247.txt"
## iconv -f Latin1 -t utf-8 "sil60vot248.txt" > "sil60vot248.txt"
## iconv -f Latin1 -t utf-8 "sil60vot249.txt" > "sil60vot249.txt"
## iconv -f Latin1 -t utf-8 "sil60vot250.txt" > "sil60vot250.txt"
## iconv -f Latin1 -t utf-8 "sil60vot251.txt" > "sil60vot251.txt"
## iconv -f Latin1 -t utf-8 "sil60vot252.txt" > "sil60vot252.txt"
## iconv -f Latin1 -t utf-8 "sil60vot253.txt" > "sil60vot253.txt"
## iconv -f Latin1 -t utf-8 "sil60vot254.txt" > "sil60vot254.txt"
## iconv -f Latin1 -t utf-8 "sil60vot255.txt" > "sil60vot255.txt"
## iconv -f Latin1 -t utf-8 "sil60vot256.txt" > "sil60vot256.txt"
## iconv -f Latin1 -t utf-8 "sil60vot257.txt" > "sil60vot257.txt"
## iconv -f Latin1 -t utf-8 "sil60vot258.txt" > "sil60vot258.txt"
## iconv -f Latin1 -t utf-8 "sil60vot259.txt" > "sil60vot259.txt"
## iconv -f Latin1 -t utf-8 "sil60vot260.txt" > "sil60vot260.txt"
## iconv -f Latin1 -t utf-8 "sil60vot261.txt" > "sil60vot261.txt"
## iconv -f Latin1 -t utf-8 "sil60vot262.txt" > "sil60vot262.txt"
## iconv -f Latin1 -t utf-8 "sil60vot263.txt" > "sil60vot263.txt"
## iconv -f Latin1 -t utf-8 "sil60vot265.txt" > "sil60vot265.txt"
## iconv -f Latin1 -t utf-8 "sil60vot266.txt" > "sil60vot266.txt"
## iconv -f Latin1 -t utf-8 "sil60vot267.txt" > "sil60vot267.txt"
## iconv -f Latin1 -t utf-8 "sil60vot268.txt" > "sil60vot268.txt"
## iconv -f Latin1 -t utf-8 "sil60vot269.txt" > "sil60vot269.txt"
## iconv -f Latin1 -t utf-8 "sil60vot270.txt" > "sil60vot270.txt"
## iconv -f Latin1 -t utf-8 "sil60vot271.txt" > "sil60vot271.txt"
## iconv -f Latin1 -t utf-8 "sil60vot272.txt" > "sil60vot272.txt"
## iconv -f Latin1 -t utf-8 "sil60vot273.txt" > "sil60vot273.txt"
## iconv -f Latin1 -t utf-8 "sil60vot274.txt" > "sil60vot274.txt"
## iconv -f Latin1 -t utf-8 "sil60vot275.txt" > "sil60vot275.txt"
## iconv -f Latin1 -t utf-8 "sil60vot276.txt" > "sil60vot276.txt"
## iconv -f Latin1 -t utf-8 "sil60vot277.txt" > "sil60vot277.txt"
## iconv -f Latin1 -t utf-8 "sil60vot278.txt" > "sil60vot278.txt"
## iconv -f Latin1 -t utf-8 "sil60vot279.txt" > "sil60vot279.txt"
## iconv -f Latin1 -t utf-8 "sil60vot280.txt" > "sil60vot280.txt"
## iconv -f Latin1 -t utf-8 "sil60vot281.txt" > "sil60vot281.txt"
## iconv -f Latin1 -t utf-8 "sil60vot282.txt" > "sil60vot282.txt"
## iconv -f Latin1 -t utf-8 "sil60vot283.txt" > "sil60vot283.txt"
## iconv -f Latin1 -t utf-8 "sil60vot284.txt" > "sil60vot284.txt"
## iconv -f Latin1 -t utf-8 "sil60vot285.txt" > "sil60vot285.txt"
## iconv -f Latin1 -t utf-8 "sil60vot286.txt" > "sil60vot286.txt"
## iconv -f Latin1 -t utf-8 "sil60vot287.txt" > "sil60vot287.txt"
## iconv -f Latin1 -t utf-8 "sil60vot288.txt" > "sil60vot288.txt"
## iconv -f Latin1 -t utf-8 "sil60vot289.txt" > "sil60vot289.txt"
## iconv -f Latin1 -t utf-8 "sil60vot290.txt" > "sil60vot290.txt"
## iconv -f Latin1 -t utf-8 "sil60vot291.txt" > "sil60vot291.txt"
## iconv -f Latin1 -t utf-8 "sil60vot292.txt" > "sil60vot292.txt"
## iconv -f Latin1 -t utf-8 "sil60vot293.txt" > "sil60vot293.txt"
## iconv -f Latin1 -t utf-8 "sil60vot294.txt" > "sil60vot294.txt"
## iconv -f Latin1 -t utf-8 "sil60vot295.txt" > "sil60vot295.txt"
## iconv -f Latin1 -t utf-8 "sil60vot296.txt" > "sil60vot296.txt"
## iconv -f Latin1 -t utf-8 "sil60vot297.txt" > "sil60vot297.txt"
## iconv -f Latin1 -t utf-8 "sil60vot310.txt" > "sil60vot310.txt"
## iconv -f Latin1 -t utf-8 "sil60vot311.txt" > "sil60vot311.txt"
## iconv -f Latin1 -t utf-8 "sil60vot312.txt" > "sil60vot312.txt"
## iconv -f Latin1 -t utf-8 "sil60vot313.txt" > "sil60vot313.txt"
## iconv -f Latin1 -t utf-8 "sil60vot314.txt" > "sil60vot314.txt"
## iconv -f Latin1 -t utf-8 "sil60vot315.txt" > "sil60vot315.txt"
## iconv -f Latin1 -t utf-8 "sil60vot316.txt" > "sil60vot316.txt"
## iconv -f Latin1 -t utf-8 "sil60vot317.txt" > "sil60vot317.txt"
## iconv -f Latin1 -t utf-8 "sil60vot318.txt" > "sil60vot318.txt"
## iconv -f Latin1 -t utf-8 "sil60vot319.txt" > "sil60vot319.txt"
## iconv -f Latin1 -t utf-8 "sil60vot320.txt" > "sil60vot320.txt"
## iconv -f Latin1 -t utf-8 "sil60vot321.txt" > "sil60vot321.txt"
## iconv -f Latin1 -t utf-8 "sil60vot322.txt" > "sil60vot322.txt"
## iconv -f Latin1 -t utf-8 "sil60vot323.txt" > "sil60vot323.txt"
## iconv -f Latin1 -t utf-8 "sil60vot324.txt" > "sil60vot324.txt"
## iconv -f Latin1 -t utf-8 "sil60vot325.txt" > "sil60vot325.txt"
## iconv -f Latin1 -t utf-8 "sil60vot326.txt" > "sil60vot326.txt"
## iconv -f Latin1 -t utf-8 "sil60vot327.txt" > "sil60vot327.txt"
## iconv -f Latin1 -t utf-8 "sil60vot328.txt" > "sil60vot328.txt"
## iconv -f Latin1 -t utf-8 "sil60vot329.txt" > "sil60vot329.txt"
## iconv -f Latin1 -t utf-8 "sil60vot330.txt" > "sil60vot330.txt"
## iconv -f Latin1 -t utf-8 "sil60vot331.txt" > "sil60vot331.txt"
## iconv -f Latin1 -t utf-8 "sil60vot332.txt" > "sil60vot332.txt"
## iconv -f Latin1 -t utf-8 "sil60vot333.txt" > "sil60vot333.txt"
## iconv -f Latin1 -t utf-8 "sil60vot334.txt" > "sil60vot334.txt"
## iconv -f Latin1 -t utf-8 "sil60vot335.txt" > "sil60vot335.txt"
## iconv -f Latin1 -t utf-8 "sil60vot336.txt" > "sil60vot336.txt"
## iconv -f Latin1 -t utf-8 "sil60vot337.txt" > "sil60vot337.txt"
## iconv -f Latin1 -t utf-8 "sil60vot338.txt" > "sil60vot338.txt"
## iconv -f Latin1 -t utf-8 "sil60vot339.txt" > "sil60vot339.txt"
## iconv -f Latin1 -t utf-8 "sil60vot340.txt" > "sil60vot340.txt"
## iconv -f Latin1 -t utf-8 "sil60vot341.txt" > "sil60vot341.txt"
## iconv -f Latin1 -t utf-8 "sil60vot342.txt" > "sil60vot342.txt"
## iconv -f Latin1 -t utf-8 "sil60vot343.txt" > "sil60vot343.txt"
## iconv -f Latin1 -t utf-8 "sil60vot344.txt" > "sil60vot344.txt"
## iconv -f Latin1 -t utf-8 "sil60vot345.txt" > "sil60vot345.txt"
## iconv -f Latin1 -t utf-8 "sil60vot346.txt" > "sil60vot346.txt"
## iconv -f Latin1 -t utf-8 "sil60vot347.txt" > "sil60vot347.txt"
## iconv -f Latin1 -t utf-8 "sil60vot348.txt" > "sil60vot348.txt"
## iconv -f Latin1 -t utf-8 "sil60vot349.txt" > "sil60vot349.txt"
## iconv -f Latin1 -t utf-8 "sil60vot350.txt" > "sil60vot350.txt"
## iconv -f Latin1 -t utf-8 "sil60vot351.txt" > "sil60vot351.txt"
## iconv -f Latin1 -t utf-8 "sil60vot352.txt" > "sil60vot352.txt"
## iconv -f Latin1 -t utf-8 "sil60vot353.txt" > "sil60vot353.txt"
## iconv -f Latin1 -t utf-8 "sil60vot354.txt" > "sil60vot354.txt"
## iconv -f Latin1 -t utf-8 "sil60vot355.txt" > "sil60vot355.txt"
## iconv -f Latin1 -t utf-8 "sil60vot356.txt" > "sil60vot356.txt"
## iconv -f Latin1 -t utf-8 "sil60vot357.txt" > "sil60vot357.txt"
## iconv -f Latin1 -t utf-8 "sil60vot358.txt" > "sil60vot358.txt"
## iconv -f Latin1 -t utf-8 "sil60vot359.txt" > "sil60vot359.txt"
## iconv -f Latin1 -t utf-8 "sil60vot360.txt" > "sil60vot360.txt"
## iconv -f Latin1 -t utf-8 "sil60vot361.txt" > "sil60vot361.txt"
## iconv -f Latin1 -t utf-8 "sil60vot362.txt" > "sil60vot362.txt"
## iconv -f Latin1 -t utf-8 "sil60vot363.txt" > "sil60vot363.txt"
## iconv -f Latin1 -t utf-8 "sil60vot364.txt" > "sil60vot364.txt"
## iconv -f Latin1 -t utf-8 "sil60vot365.txt" > "sil60vot365.txt"
## iconv -f Latin1 -t utf-8 "sil60vot366.txt" > "sil60vot366.txt"
## iconv -f Latin1 -t utf-8 "sil60vot367.txt" > "sil60vot367.txt"
## iconv -f Latin1 -t utf-8 "sil60vot368.txt" > "sil60vot368.txt"
## iconv -f Latin1 -t utf-8 "sil60vot369.txt" > "sil60vot369.txt"
## iconv -f Latin1 -t utf-8 "sil60vot370.txt" > "sil60vot370.txt"
## iconv -f Latin1 -t utf-8 "sil60vot371.txt" > "sil60vot371.txt"
## iconv -f Latin1 -t utf-8 "sil60vot372.txt" > "sil60vot372.txt"
## iconv -f Latin1 -t utf-8 "sil60vot373.txt" > "sil60vot373.txt"
## iconv -f Latin1 -t utf-8 "sil60vot374.txt" > "sil60vot374.txt"
## iconv -f Latin1 -t utf-8 "sil60vot375.txt" > "sil60vot375.txt"
## iconv -f Latin1 -t utf-8 "sil60vot376.txt" > "sil60vot376.txt"
## iconv -f Latin1 -t utf-8 "sil60vot377.txt" > "sil60vot377.txt"
## iconv -f Latin1 -t utf-8 "sil60vot378.txt" > "sil60vot378.txt"
## iconv -f Latin1 -t utf-8 "sil60vot379.txt" > "sil60vot379.txt"
## iconv -f Latin1 -t utf-8 "sil60vot380.txt" > "sil60vot380.txt"
## iconv -f Latin1 -t utf-8 "sil60vot381.txt" > "sil60vot381.txt"
## iconv -f Latin1 -t utf-8 "sil60vot382.txt" > "sil60vot382.txt"
## iconv -f Latin1 -t utf-8 "sil60vot383.txt" > "sil60vot383.txt"
## iconv -f Latin1 -t utf-8 "sil60vot384.txt" > "sil60vot384.txt"
## iconv -f Latin1 -t utf-8 "sil60vot385.txt" > "sil60vot385.txt"
## iconv -f Latin1 -t utf-8 "sil60vot386.txt" > "sil60vot386.txt"
## iconv -f Latin1 -t utf-8 "sil60vot387.txt" > "sil60vot387.txt"
## iconv -f Latin1 -t utf-8 "sil60vot388.txt" > "sil60vot388.txt"
## iconv -f Latin1 -t utf-8 "sil60vot389.txt" > "sil60vot389.txt"
## iconv -f Latin1 -t utf-8 "sil60vot390.txt" > "sil60vot390.txt"
## iconv -f Latin1 -t utf-8 "sil60vot391.txt" > "sil60vot391.txt"
## iconv -f Latin1 -t utf-8 "sil60vot392.txt" > "sil60vot392.txt"
## iconv -f Latin1 -t utf-8 "sil60vot393.txt" > "sil60vot393.txt"
## iconv -f Latin1 -t utf-8 "sil60vot394.txt" > "sil60vot394.txt"
## iconv -f Latin1 -t utf-8 "sil60vot395.txt" > "sil60vot395.txt"
## iconv -f Latin1 -t utf-8 "sil60vot396.txt" > "sil60vot396.txt"
## iconv -f Latin1 -t utf-8 "sil60vot397.txt" > "sil60vot397.txt"
## iconv -f Latin1 -t utf-8 "sil60vot398.txt" > "sil60vot398.txt"
## iconv -f Latin1 -t utf-8 "sil60vot399.txt" > "sil60vot399.txt"
## iconv -f Latin1 -t utf-8 "sil60vot400.txt" > "sil60vot400.txt"
## iconv -f Latin1 -t utf-8 "sil60vot401.txt" > "sil60vot401.txt"
## iconv -f Latin1 -t utf-8 "sil60vot402.txt" > "sil60vot402.txt"
## iconv -f Latin1 -t utf-8 "sil60vot403.txt" > "sil60vot403.txt"
## iconv -f Latin1 -t utf-8 "sil60vot404.txt" > "sil60vot404.txt"
## iconv -f Latin1 -t utf-8 "sil60vot405.txt" > "sil60vot405.txt"
## iconv -f Latin1 -t utf-8 "sil60vot406.txt" > "sil60vot406.txt"
## iconv -f Latin1 -t utf-8 "sil60vot407.txt" > "sil60vot407.txt"
## iconv -f Latin1 -t utf-8 "sil60vot408.txt" > "sil60vot408.txt"
## iconv -f Latin1 -t utf-8 "sil60vot409.txt" > "sil60vot409.txt"
## iconv -f Latin1 -t utf-8 "sil60vot410.txt" > "sil60vot410.txt"
## iconv -f Latin1 -t utf-8 "sil60vot411.txt" > "sil60vot411.txt"
## iconv -f Latin1 -t utf-8 "sil60vot412.txt" > "sil60vot412.txt"
## iconv -f Latin1 -t utf-8 "sil60vot413.txt" > "sil60vot413.txt"
## iconv -f Latin1 -t utf-8 "sil60vot414.txt" > "sil60vot414.txt"
## iconv -f Latin1 -t utf-8 "sil60vot415.txt" > "sil60vot415.txt"
## iconv -f Latin1 -t utf-8 "sil60vot416.txt" > "sil60vot416.txt"
## iconv -f Latin1 -t utf-8 "sil60vot417.txt" > "sil60vot417.txt"
## iconv -f Latin1 -t utf-8 "sil60vot418.txt" > "sil60vot418.txt"
## iconv -f Latin1 -t utf-8 "sil60vot419.txt" > "sil60vot419.txt"
## iconv -f Latin1 -t utf-8 "sil60vot420.txt" > "sil60vot420.txt"
## iconv -f Latin1 -t utf-8 "sil60vot421.txt" > "sil60vot421.txt"
## iconv -f Latin1 -t utf-8 "sil60vot422.txt" > "sil60vot422.txt"
## iconv -f Latin1 -t utf-8 "sil60vot423.txt" > "sil60vot423.txt"
## iconv -f Latin1 -t utf-8 "sil60vot424.txt" > "sil60vot424.txt"
## iconv -f Latin1 -t utf-8 "sil60vot425.txt" > "sil60vot425.txt"
## iconv -f Latin1 -t utf-8 "sil60vot426.txt" > "sil60vot426.txt"
## iconv -f Latin1 -t utf-8 "sil60vot427.txt" > "sil60vot427.txt"
## iconv -f Latin1 -t utf-8 "sil60vot428.txt" > "sil60vot428.txt"
## iconv -f Latin1 -t utf-8 "sil60vot429.txt" > "sil60vot429.txt"
## iconv -f Latin1 -t utf-8 "sil60vot430.txt" > "sil60vot430.txt"
## iconv -f Latin1 -t utf-8 "sil60vot431.txt" > "sil60vot431.txt"
## iconv -f Latin1 -t utf-8 "sil60vot432.txt" > "sil60vot432.txt"
## iconv -f Latin1 -t utf-8 "sil60vot433.txt" > "sil60vot433.txt"
## iconv -f Latin1 -t utf-8 "sil60vot434.txt" > "sil60vot434.txt"
## iconv -f Latin1 -t utf-8 "sil60vot435.txt" > "sil60vot435.txt"
## iconv -f Latin1 -t utf-8 "sil60vot436.txt" > "sil60vot436.txt"
## iconv -f Latin1 -t utf-8 "sil60vot437.txt" > "sil60vot437.txt"
## iconv -f Latin1 -t utf-8 "sil60vot438.txt" > "sil60vot438.txt"
## iconv -f Latin1 -t utf-8 "sil60vot439.txt" > "sil60vot439.txt"
## iconv -f Latin1 -t utf-8 "sil60vot440.txt" > "sil60vot440.txt"
## iconv -f Latin1 -t utf-8 "sil60vot441.txt" > "sil60vot441.txt"
## iconv -f Latin1 -t utf-8 "sil60vot442.txt" > "sil60vot442.txt"
## iconv -f Latin1 -t utf-8 "sil60vot443.txt" > "sil60vot443.txt"
## iconv -f Latin1 -t utf-8 "sil60vot444.txt" > "sil60vot444.txt"
## iconv -f Latin1 -t utf-8 "sil60vot445.txt" > "sil60vot445.txt"
## iconv -f Latin1 -t utf-8 "sil60vot446.txt" > "sil60vot446.txt"
## iconv -f Latin1 -t utf-8 "sil60vot447.txt" > "sil60vot447.txt"
## iconv -f Latin1 -t utf-8 "sil60vot448.txt" > "sil60vot448.txt"
## iconv -f Latin1 -t utf-8 "sil60vot449.txt" > "sil60vot449.txt"
## iconv -f Latin1 -t utf-8 "sil60vot450.txt" > "sil60vot450.txt"
## iconv -f Latin1 -t utf-8 "sil60vot451.txt" > "sil60vot451.txt"
## iconv -f Latin1 -t utf-8 "sil60vot452.txt" > "sil60vot452.txt"
## iconv -f Latin1 -t utf-8 "sil60vot453.txt" > "sil60vot453.txt"
## iconv -f Latin1 -t utf-8 "sil60vot454.txt" > "sil60vot454.txt"
## iconv -f Latin1 -t utf-8 "sil60vot455.txt" > "sil60vot455.txt"
## iconv -f Latin1 -t utf-8 "sil60vot456.txt" > "sil60vot456.txt"
## iconv -f Latin1 -t utf-8 "sil60vot457.txt" > "sil60vot457.txt"
## iconv -f Latin1 -t utf-8 "sil60vot458.txt" > "sil60vot458.txt"
## iconv -f Latin1 -t utf-8 "sil60vot459.txt" > "sil60vot459.txt"
## iconv -f Latin1 -t utf-8 "sil60vot460.txt" > "sil60vot460.txt"
## iconv -f Latin1 -t utf-8 "sil60vot461.txt" > "sil60vot461.txt"
## iconv -f Latin1 -t utf-8 "sil60vot462.txt" > "sil60vot462.txt"
## iconv -f Latin1 -t utf-8 "sil60vot463.txt" > "sil60vot463.txt"
## iconv -f Latin1 -t utf-8 "sil60vot464.txt" > "sil60vot464.txt"
## iconv -f Latin1 -t utf-8 "sil60vot465.txt" > "sil60vot465.txt"
## iconv -f Latin1 -t utf-8 "sil60vot466.txt" > "sil60vot466.txt"
## iconv -f Latin1 -t utf-8 "sil60vot467.txt" > "sil60vot467.txt"
## iconv -f Latin1 -t utf-8 "sil60vot468.txt" > "sil60vot468.txt"
## iconv -f Latin1 -t utf-8 "sil60vot469.txt" > "sil60vot469.txt"
## iconv -f Latin1 -t utf-8 "sil60vot470.txt" > "sil60vot470.txt"
## iconv -f Latin1 -t utf-8 "sil60vot471.txt" > "sil60vot471.txt"
## iconv -f Latin1 -t utf-8 "sil60vot472.txt" > "sil60vot472.txt"
## iconv -f Latin1 -t utf-8 "sil60vot473.txt" > "sil60vot473.txt"
## iconv -f Latin1 -t utf-8 "sil60vot474.txt" > "sil60vot474.txt"
## iconv -f Latin1 -t utf-8 "sil60vot475.txt" > "sil60vot475.txt"
## iconv -f Latin1 -t utf-8 "sil60vot476.txt" > "sil60vot476.txt"
## iconv -f Latin1 -t utf-8 "sil60vot477.txt" > "sil60vot477.txt"
## iconv -f Latin1 -t utf-8 "sil60vot478.txt" > "sil60vot478.txt"
## iconv -f Latin1 -t utf-8 "sil60vot479.txt" > "sil60vot479.txt"
## iconv -f Latin1 -t utf-8 "sil60vot480.txt" > "sil60vot480.txt"
## iconv -f Latin1 -t utf-8 "sil60vot481.txt" > "sil60vot481.txt"
## iconv -f Latin1 -t utf-8 "sil60vot482.txt" > "sil60vot482.txt"
## iconv -f Latin1 -t utf-8 "sil60vot483.txt" > "sil60vot483.txt"
## iconv -f Latin1 -t utf-8 "sil60vot484.txt" > "sil60vot484.txt"
## iconv -f Latin1 -t utf-8 "sil60vot485.txt" > "sil60vot485.txt"
## iconv -f Latin1 -t utf-8 "sil60vot486.txt" > "sil60vot486.txt"
## iconv -f Latin1 -t utf-8 "sil60vot487.txt" > "sil60vot487.txt"
## iconv -f Latin1 -t utf-8 "sil60vot488.txt" > "sil60vot488.txt"
## iconv -f Latin1 -t utf-8 "sil60vot489.txt" > "sil60vot489.txt"
## iconv -f Latin1 -t utf-8 "sil60vot490.txt" > "sil60vot490.txt"
## iconv -f Latin1 -t utf-8 "sil60vot491.txt" > "sil60vot491.txt"
## iconv -f Latin1 -t utf-8 "sil60vot492.txt" > "sil60vot492.txt"
## iconv -f Latin1 -t utf-8 "sil60vot493.txt" > "sil60vot493.txt"
## iconv -f Latin1 -t utf-8 "sil60vot494.txt" > "sil60vot494.txt"
## iconv -f Latin1 -t utf-8 "sil60vot495.txt" > "sil60vot495.txt"
## iconv -f Latin1 -t utf-8 "sil60vot496.txt" > "sil60vot496.txt"
## iconv -f Latin1 -t utf-8 "sil60vot497.txt" > "sil60vot497.txt"
## iconv -f Latin1 -t utf-8 "sil60vot498.txt" > "sil60vot498.txt"
## iconv -f Latin1 -t utf-8 "sil60vot499.txt" > "sil60vot499.txt"
## iconv -f Latin1 -t utf-8 "sil60vot500.txt" > "sil60vot500.txt"
## iconv -f Latin1 -t utf-8 "sil60vot501.txt" > "sil60vot501.txt"
## iconv -f Latin1 -t utf-8 "sil60vot502.txt" > "sil60vot502.txt"
## iconv -f Latin1 -t utf-8 "sil60vot503.txt" > "sil60vot503.txt"
## iconv -f Latin1 -t utf-8 "sil60vot504.txt" > "sil60vot504.txt"
## iconv -f Latin1 -t utf-8 "sil60vot505.txt" > "sil60vot505.txt"
## iconv -f Latin1 -t utf-8 "sil60vot506.txt" > "sil60vot506.txt"
## iconv -f Latin1 -t utf-8 "sil60vot507.txt" > "sil60vot507.txt"
## iconv -f Latin1 -t utf-8 "sil60vot508.txt" > "sil60vot508.txt"
## iconv -f Latin1 -t utf-8 "sil60vot509.txt" > "sil60vot509.txt"
## iconv -f Latin1 -t utf-8 "sil60vot510.txt" > "sil60vot510.txt"
## iconv -f Latin1 -t utf-8 "sil60vot511.txt" > "sil60vot511.txt"
## iconv -f Latin1 -t utf-8 "sil60vot512.txt" > "sil60vot512.txt"
## iconv -f Latin1 -t utf-8 "sil60vot513.txt" > "sil60vot513.txt"
## iconv -f Latin1 -t utf-8 "sil60vot514.txt" > "sil60vot514.txt"
## iconv -f Latin1 -t utf-8 "sil60vot515.txt" > "sil60vot515.txt"
## iconv -f Latin1 -t utf-8 "sil60vot516.txt" > "sil60vot516.txt"
## iconv -f Latin1 -t utf-8 "sil60vot517.txt" > "sil60vot517.txt"
## iconv -f Latin1 -t utf-8 "sil60vot518.txt" > "sil60vot518.txt"
## iconv -f Latin1 -t utf-8 "sil60vot519.txt" > "sil60vot519.txt"
## iconv -f Latin1 -t utf-8 "sil60vot520.txt" > "sil60vot520.txt"
## iconv -f Latin1 -t utf-8 "sil60vot521.txt" > "sil60vot521.txt"
## iconv -f Latin1 -t utf-8 "sil60vot522.txt" > "sil60vot522.txt"
## iconv -f Latin1 -t utf-8 "sil60vot523.txt" > "sil60vot523.txt"
## iconv -f Latin1 -t utf-8 "sil60vot524.txt" > "sil60vot524.txt"
## iconv -f Latin1 -t utf-8 "sil60vot525.txt" > "sil60vot525.txt"
## iconv -f Latin1 -t utf-8 "sil60vot526.txt" > "sil60vot526.txt"
## iconv -f Latin1 -t utf-8 "sil60vot527.txt" > "sil60vot527.txt"
## iconv -f Latin1 -t utf-8 "sil60vot528.txt" > "sil60vot528.txt"
## iconv -f Latin1 -t utf-8 "sil60vot529.txt" > "sil60vot529.txt"
## iconv -f Latin1 -t utf-8 "sil60vot530.txt" > "sil60vot530.txt"
## iconv -f Latin1 -t utf-8 "sil60vot531.txt" > "sil60vot531.txt"
## iconv -f Latin1 -t utf-8 "sil60vot532.txt" > "sil60vot532.txt"
## iconv -f Latin1 -t utf-8 "sil60vot533.txt" > "sil60vot533.txt"
## iconv -f Latin1 -t utf-8 "sil60vot534.txt" > "sil60vot534.txt"
## iconv -f Latin1 -t utf-8 "sil60vot535.txt" > "sil60vot535.txt"
## iconv -f Latin1 -t utf-8 "sil60vot536.txt" > "sil60vot536.txt"
## iconv -f Latin1 -t utf-8 "sil60vot537.txt" > "sil60vot537.txt"
## iconv -f Latin1 -t utf-8 "sil60vot538.txt" > "sil60vot538.txt"
## iconv -f Latin1 -t utf-8 "sil60vot539.txt" > "sil60vot539.txt"
## iconv -f Latin1 -t utf-8 "sil60vot540.txt" > "sil60vot540.txt"
## iconv -f Latin1 -t utf-8 "sil60vot541.txt" > "sil60vot541.txt"
## iconv -f Latin1 -t utf-8 "sil60vot542.txt" > "sil60vot542.txt"
## iconv -f Latin1 -t utf-8 "sil60vot543.txt" > "sil60vot543.txt"
## iconv -f Latin1 -t utf-8 "sil60vot544.txt" > "sil60vot544.txt"
## iconv -f Latin1 -t utf-8 "sil60vot545.txt" > "sil60vot545.txt"
## iconv -f Latin1 -t utf-8 "sil60vot546.txt" > "sil60vot546.txt"
## iconv -f Latin1 -t utf-8 "sil60vot547.txt" > "sil60vot547.txt"
## iconv -f Latin1 -t utf-8 "sil60vot548.txt" > "sil60vot548.txt"
## iconv -f Latin1 -t utf-8 "sil60vot549.txt" > "sil60vot549.txt"
## iconv -f Latin1 -t utf-8 "sil60vot550.txt" > "sil60vot550.txt"
## iconv -f Latin1 -t utf-8 "sil60vot551.txt" > "sil60vot551.txt"
## iconv -f Latin1 -t utf-8 "sil60vot552.txt" > "sil60vot552.txt"
## iconv -f Latin1 -t utf-8 "sil60vot553.txt" > "sil60vot553.txt"
## iconv -f Latin1 -t utf-8 "sil60vot554.txt" > "sil60vot554.txt"
## iconv -f Latin1 -t utf-8 "sil60vot555.txt" > "sil60vot555.txt"
## iconv -f Latin1 -t utf-8 "sil60vot556.txt" > "sil60vot556.txt"
## iconv -f Latin1 -t utf-8 "sil60vot557.txt" > "sil60vot557.txt"
## iconv -f Latin1 -t utf-8 "sil60vot558.txt" > "sil60vot558.txt"
## iconv -f Latin1 -t utf-8 "sil60vot559.txt" > "sil60vot559.txt"
## iconv -f Latin1 -t utf-8 "sil60vot560.txt" > "sil60vot560.txt"
## iconv -f Latin1 -t utf-8 "sil60vot561.txt" > "sil60vot561.txt"
## iconv -f Latin1 -t utf-8 "sil60vot562.txt" > "sil60vot562.txt"
## iconv -f Latin1 -t utf-8 "sil60vot563.txt" > "sil60vot563.txt"
## iconv -f Latin1 -t utf-8 "sil60vot564.txt" > "sil60vot564.txt"
## iconv -f Latin1 -t utf-8 "sil60vot565.txt" > "sil60vot565.txt"
## iconv -f Latin1 -t utf-8 "sil60vot566.txt" > "sil60vot566.txt"
## iconv -f Latin1 -t utf-8 "sil60vot567.txt" > "sil60vot567.txt"
## iconv -f Latin1 -t utf-8 "sil60vot568.txt" > "sil60vot568.txt"
## iconv -f Latin1 -t utf-8 "sil60vot569.txt" > "sil60vot569.txt"
## iconv -f Latin1 -t utf-8 "sil60vot570.txt" > "sil60vot570.txt"
## iconv -f Latin1 -t utf-8 "sil60vot571.txt" > "sil60vot571.txt"
## iconv -f Latin1 -t utf-8 "sil60vot572.txt" > "sil60vot572.txt"
## iconv -f Latin1 -t utf-8 "sil60vot573.txt" > "sil60vot573.txt"
## iconv -f Latin1 -t utf-8 "sil60vot574.txt" > "sil60vot574.txt"
## iconv -f Latin1 -t utf-8 "sil60vot575.txt" > "sil60vot575.txt"
## iconv -f Latin1 -t utf-8 "sil60vot576.txt" > "sil60vot576.txt"
## iconv -f Latin1 -t utf-8 "sil60vot577.txt" > "sil60vot577.txt"
## iconv -f Latin1 -t utf-8 "sil60vot579.txt" > "sil60vot579.txt"
## iconv -f Latin1 -t utf-8 "sil60vot580.txt" > "sil60vot580.txt"
## iconv -f Latin1 -t utf-8 "sil60vot581.txt" > "sil60vot581.txt"
## iconv -f Latin1 -t utf-8 "sil60vot582.txt" > "sil60vot582.txt"
## iconv -f Latin1 -t utf-8 "sil60vot583.txt" > "sil60vot583.txt"
## iconv -f Latin1 -t utf-8 "sil60vot584.txt" > "sil60vot584.txt"
## iconv -f Latin1 -t utf-8 "sil60vot585.txt" > "sil60vot585.txt"
## iconv -f Latin1 -t utf-8 "sil60vot586.txt" > "sil60vot586.txt"
## iconv -f Latin1 -t utf-8 "sil60vot587.txt" > "sil60vot587.txt"
## iconv -f Latin1 -t utf-8 "sil60vot588.txt" > "sil60vot588.txt"
## iconv -f Latin1 -t utf-8 "sil60vot589.txt" > "sil60vot589.txt"
## iconv -f Latin1 -t utf-8 "sil60vot590.txt" > "sil60vot590.txt"
## iconv -f Latin1 -t utf-8 "sil60vot591.txt" > "sil60vot591.txt"
## iconv -f Latin1 -t utf-8 "sil60vot592.txt" > "sil60vot592.txt"
## iconv -f Latin1 -t utf-8 "sil60vot593.txt" > "sil60vot593.txt"
## iconv -f Latin1 -t utf-8 "sil60vot594.txt" > "sil60vot594.txt"
## iconv -f Latin1 -t utf-8 "sil60vot595.txt" > "sil60vot595.txt"
## iconv -f Latin1 -t utf-8 "sil60vot596.txt" > "sil60vot596.txt"
## iconv -f Latin1 -t utf-8 "sil60vot597.txt" > "sil60vot597.txt"
## iconv -f Latin1 -t utf-8 "sil60vot598.txt" > "sil60vot598.txt"
## iconv -f Latin1 -t utf-8 "sil60vot599.txt" > "sil60vot599.txt"
## iconv -f Latin1 -t utf-8 "sil60vot600.txt" > "sil60vot600.txt"
## iconv -f Latin1 -t utf-8 "sil60vot601.txt" > "sil60vot601.txt"
## iconv -f Latin1 -t utf-8 "sil60vot602.txt" > "sil60vot602.txt"
## iconv -f Latin1 -t utf-8 "sil60vot603.txt" > "sil60vot603.txt"
## iconv -f Latin1 -t utf-8 "sil60vot604.txt" > "sil60vot604.txt"

head(filenames)
filenames <- paste("/home/eric/Dropbox/data/rollcall/dipMex/data/fromWeb/votes/60", filenames, sep = "/")
filenames[1]

I <- length(filenames)
J <- nrow(dipdat)

tmp <- rep(NA, times=I)
votdat <- data.frame(favor=tmp, contra=tmp, absten=tmp, quorum=tmp, ausen=tmp, 
                      title=NA, leg=rep(60, times=I), 
                      filename=sub(filenames, pattern=".*60/(.*).txt", replacement="\\1"), 
                      yr=tmp, mo=tmp, dy=tmp, haveinfo=rep(1, times=I))
rc <- matrix(0,  nrow=I, ncol=J); rc <- as.data.frame(rc); colnames(rc) <- dipdat$id
id <- colnames(rc)
votdat[1,]

# determine character encoding # SHOULD LEARN TO DO WITH BEAUTIFUL SOUP
encod <- rep(NA, I)
for (i in 1:I){
    info <- readLines( con=filenames[i] , encoding = "utf8" ) 
    encod[i] <- ifelse( length(grep(info, pattern="Martínez", perl=TRUE))>0, "utf8", NA) # the name should appear in every file
}
for(i in which(is.na(encod)==TRUE)){
    info <- readLines( con=filenames[i] , encoding = "latin1" ) 
    encod[i] <- ifelse( length(grep(info, pattern="Martínez", perl=TRUE))>0, "latin1", NA) # the name should appear in every file
}
which(is.na(encod)==TRUE) # should be empty

i <- 1; j <- 1 # debug
for (i in 1:I){
    print(paste("loop ", i, " of ", I, " (", round(i*100/I, digits = 0), "%)", sep = ""))
    info <- readLines( con=filenames[i] , encoding = encod[i] ) 
    votdat$title[i] <- info[4]
    votdat$dy[i] <- as.numeric(sub(info[1], pattern="fch = ([0-9]*)(-.*-)([0-9]*)", replacement="\\1"))
    votdat$yr[i] <- as.numeric(sub(info[1], pattern="fch = ([0-9]*)(-.*-)([0-9]*)", replacement="\\3"))
    tmp <- sub(info[1], pattern="fch = ([0-9]*-)(.*)(-[0-9]*)", replacement="\\2")
    tmp <- sub(tmp, pattern="[Ee]nero", replacement="1")
    tmp <- sub(tmp, pattern="[Ff]ebrero", replacement="2")
    tmp <- sub(tmp, pattern="[Mm]arzo", replacement="3")
    tmp <- sub(tmp, pattern="[Aa]bril", replacement="4")
    tmp <- sub(tmp, pattern="[Mm]ayo", replacement="5")
    tmp <- sub(tmp, pattern="[Jj]unio", replacement="6")
    tmp <- sub(tmp, pattern="[Jj]ulio", replacement="7")
    tmp <- sub(tmp, pattern="[Aa]gosto", replacement="8")
    tmp <- sub(tmp, pattern="[Ss]eptiembre", replacement="9")
    tmp <- sub(tmp, pattern="[Oo]ctubre", replacement="10")
    tmp <- sub(tmp, pattern="[Nn]oviembre", replacement="11")
    tmp <- sub(tmp, pattern="[Dd]iciembre", replacement="12")
    votdat$mo[i] <- as.numeric(tmp)
    #
    ## NEEDED IF DEPUTY'S PARTY WILL BE CHECKED
    #panstart <- grep(info, pattern="emmPanVoteStart"); pristart <- grep(info, pattern="emmPriVoteStart")
    #prdstart <- grep(info, pattern="emmPrdVoteStart"); ptstart <- grep(info, pattern="emmPtVoteStart")
    #pvemstart <- grep(info, pattern="emmPvemVoteStart"); convestart <- grep(info, pattern="emmConveVoteStart")
    #panalstart <- grep(info, pattern="emmPanalVoteStart"); indepstart <- grep(info, pattern="emmIndepVoteStart")
    #
    for (j in 1:J){
        #print(c(j,i))
        tmp <- grep(info, pattern=dipdat$nomregexp[j], perl = TRUE) # DIP'S NAME
        tmp2 <- ifelse( length(tmp)>0, sub(info[tmp+1], pattern=".*<span class.*> (.*)</span></td>", replacement="\\1"), 0)
        tmp2 <- gsub(tmp2, pattern="A favor", replacement="1")
        tmp2 <- gsub(tmp2, pattern="En contra", replacement="2")
        tmp2 <- gsub(tmp2, pattern="Abstención", replacement="3")
        tmp2 <- gsub(tmp2, pattern="Sólo asistencia", replacement="4")
        tmp2 <- gsub(tmp2, pattern="Ausente", replacement="5")
        rc[i,j] <- as.numeric(tmp2)
    }
    tmp <- (( ( 1:(J/2) )*2 )-1) # sequence of propietarios assuming suplente is next obs
    for (j in tmp){
        if (rc[i,j]==0 & rc[i,j+1]==0) {rc[i,j] <- 6} else next
    }
}
#save.image("tmp.RData") # debug
#load("tmp.RData")       # debug
#info[grep(pattern = "iddipt=12&", info)] # debug
#dipdat[dipdat$id=="c5rp40p",]            # debug
#info[grep(pattern = dipdat$nomregexp[dipdat$id=="c5rp40p"], info, perl = TRUE)]  # debug
#
## exceptions
#paste(i, votdat$filename[i], id[rc[i,]==6], dipdat$part[rc[i,]==6], dipdat$nom[rc[i,]==6]); table(as.numeric(rc[i,]))
    ##
    rc[votdat$yr==2007 & votdat$mo==12 & votdat$dy==4, id=="bc07p"] <- 5    ## ENRÍQUEZ M LICENCIA, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2006 & votdat$mo==9 & votdat$dy<21, id=="c3rp05p"] <- 5  ## NORDHAUSEN NO ASUME, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy==26, id=="c5rp14p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy<=19, id=="c5rp14p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2007 & votdat$mo==2 & votdat$dy==8, id=="cps08p"] <- 5    ## CORDERO DESPARACE ESTA SESION
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==23, id=="cua03p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2007 & votdat$mo==3 & votdat$dy>20 & votdat$dy<29, id=="cua04p"] <- 5 ## VALENCIA DESAPARECE UN PERIODO
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy==5, id=="df19p"] <- 5     ## OLIVA LICENCIA, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy>=3 & votdat$dy<=5, id=="df24p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy>=12 & votdat$dy<=19, id=="c4rp14p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==2, id=="c4rp15p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==15, id=="gua11p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009,                                id=="c2rp22p"] <- 5 ## CHAURAND LICENCIA, SUPLENTE NO ENTRA
    rc[votdat$yr==2008 & votdat$mo==6, id=="gue09p"] <- 5                   ## FLORES MALDONADO DESAPARECE UN PERIODO
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==30, id=="c4rp08p"] <- 5 ## ARIZMENDI LICENCIA, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy>=26, id=="mex01p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy>=3 & votdat$dy<=5, id=="mex01p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy==26, id=="mex05p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy==24, id=="mex08p"] <- 5   ## MTZ MTZ LICENCIA, SUPLENTE TARDA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy>=3 & votdat$dy<=19, id=="mex09p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==28, id=="mex10p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy==26, id=="mex12p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==2, id=="mex12p"] <- 5    ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy>=23, id=="mex15p"] <- 5   ## LANDERO LICENCIA, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy==24, id=="mex25p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy>=28, id=="mex26p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy>=3 & votdat$dy<=19, id=="mex28p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy==31, id=="mex29p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy==26, id=="mex35p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy>=3 & votdat$dy<=5, id=="mex35p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy>=3 & votdat$dy<=12, id=="mex36p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy==24, id=="mex39p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy>=26, id=="mex39p"] <- 5   ## SAN MARTÍN DESPARECE ESTA SESION
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy>=3 & votdat$dy<=12, id=="mex39p"] <- 5  ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==28, id=="mex40p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==28, id=="c5rp10p"] <- 5 ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy>=28, id=="c5rp06p"] <- 5 ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy>=28, id=="c5rp03p"] <- 5 ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr<2008,                                 id=="c5rp41p"] <- 5 ## RIVADENEYRA NO ASUME, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2008 & votdat$mo<9,                  id=="c5rp41p"] <- 5 ## RIVADENEYRA NO ASUME, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2008 & votdat$mo==9 & votdat$dy<11, id=="c5rp41p"] <- 5  ## RIVADENEYRA NO ASUME, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==28, id=="c5rp21p"] <- 5 ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2008 & votdat$mo==4 & votdat$dy>=10, id=="c1rp32p"] <- 5 ## NAVARRO QUINTERO DESAPARECE UN PERIODO
    rc[votdat$yr==2008 & votdat$mo>4 & votdat$mo<8, id=="c1rp32p"] <- 5    ## NAVARRO QUINTERO DESAPARECE UN PERIODO
    rc[votdat$yr==2006 & votdat$mo==10 & votdat$dy>=30, id=="c2rp08p"] <- 5 ## OROZCO LICENCIA, SUPLENTE ENTRA TARDE
    rc[votdat$yr<2009, id=="c3rp41p"] <- 5                                 ## MELO LLEGA TARDE, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==1 & votdat$dy<14, id=="c3rp41p"] <- 5  ## MELO LLEGA TARDE, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy>=28, id=="c3rp35p"] <- 5 ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy==31, id=="c2rp27p"] <- 5 ## ORTIZ P LICENCIA, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo>3, id=="c2rp27p"] <- 5                  ## ORTIZ P LICENCIA, SUPLENTE NO ENTRA
    rc[votdat$yr==2007 & votdat$mo==2 & votdat$dy<27, id=="qui01p"] <- 5    ## RUIZ CHAVEZ DESAPARECE UN PERIODO
    rc[votdat$yr==2009, id=="qui01p"] <- 5                                  ## RUIZ CHAVEZ DESAPARECE UN PERIODO
    rc[votdat$yr==2008 & votdat$mo==12 & votdat$dy==11, id=="c2rp23p"] <- 5  ## RMZ STABROS DESPARACE ESTA SESION
    rc[votdat$yr==2006 & votdat$mo<10, id=="tab04p"] <- 5                   ## MAYANS LLEGA TARDE, SUPLENTE NO ENTRA
    rc[votdat$yr==2006 & votdat$mo==10 & votdat$dy<24, id=="tab04p"] <- 5   ## MAYANS LLEGA TARDE, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy>=17, id=="tla02p"] <- 5   ## ESCOBAR LICENCIA, SUPLENTE TARDA
    rc[votdat$yr==2008 & votdat$mo==12 & votdat$dy==11, id=="ver04p"] <- 5  ## DESCHAMPS LICENCIA, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2006 & votdat$mo==10 & votdat$dy>19, id=="ver07p"] <- 5   ## DE LA TORRE LICENCIA, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2007 & votdat$mo>=4 & votdat$mo<=8, id=="ver14p"] <- 5    ## USCANGA DESAPARECE UN PERIODO
    rc[votdat$yr==2007 & votdat$mo==9 & votdat$dy<=4, id=="ver14p"] <- 5    ## USCANGA DESAPARECE UN PERIODO
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==30, id=="c3rp33p"] <- 5 ## ELIZONDO LICENCIA, SUPLENTE NO ENTRA
    rc[votdat$yr==2008 & votdat$mo==8 & votdat$dy==28, id=="yuc03p"] <- 5   ## CASTRO ROMERO LICENCIA, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2009 & votdat$mo==3 & votdat$dy==26, id=="yuc04p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==4 & votdat$dy==2, id=="c3rp16p"] <- 5   ## DESAPARECE HACIA FIN CONGRESO, SUPLENTE NO ENTRA
    rc[votdat$yr==2009 & votdat$mo==2 & votdat$dy<17, id=="zac02p"] <- 5    ## BERMUDEZ LICENCIA, SUPLENTE ENTRA TARDE
    rc[votdat$yr==2006 & votdat$mo==12 & votdat$dy==21, id=="c3rp06p"] <- 5    ## Cuevas Melo LICENCIA, SUPLENTE TARDA
    rc[votdat$yr==2007 & votdat$mo==8 & votdat$dy==30, id=="c2rp03p"] <- 5    ## Zermeño desaparece un día
    rc[votdat$yr==2008 & votdat$mo==8 & votdat$dy==30, id=="c3rp37p"] <- 5    ## Esteva licencia, suplente no entra
    rc[votdat$yr==2008 & votdat$mo>=9                , id=="c3rp37p"] <- 5    ## Esteva licencia, suplente no entra
    rc[votdat$yr==2009                               , id=="c3rp37p"] <- 5    ## Esteva licencia, suplente no entra
    rc[votdat$yr==2008 & votdat$mo==8 & votdat$dy==30, id=="c5rp04p"] <- 5    ## Morelos licencia, suplente no entra
    rc[votdat$yr==2008 & votdat$mo>=9                , id=="c5rp04p"] <- 5    ## Morelos licencia, suplente no entra
    rc[votdat$yr==2009                               , id=="c5rp04p"] <- 5    ## Morelos licencia, suplente no entra
    ##
    # compute vote totals
for (i in 1:I){
    tmp <- which(rc[i,]==1); votdat$favor[i] <- length(tmp)
    tmp <- which(rc[i,]==2); votdat$contra[i] <- length(tmp)
    tmp <- which(rc[i,]==3); votdat$absten[i] <- length(tmp)
    tmp <- which(rc[i,]==4); votdat$quorum[i] <- length(tmp)
    tmp <- which(rc[i,]==5); votdat$ausen[i] <- length(tmp)
              }

## USADOS PARA EXPLORAR SEISES
d<-0
d <- d+1; paste(d, id[d], dipdat$nom[d]); rc[,d]; votdat$filename[rc[,d]==6] ## ÚLT LISTA SOLO LOS SEISES
d<-grep(id,pattern="mex35p")
paste(d, id[d], dipdat$nom[d]); data.frame(x=rc[,d],y=paste(votdat$yr, votdat$mo, votdat$dy, sep=""))

v <- 0
v <- v+1; paste(v, votdat$filename[v], id[rc[v,]==6], paste(votdat$yr[v], votdat$mo[v], votdat$dy[v], sep=""), dipdat$part[rc[v,]==6], dipdat$nom[rc[v,]==6]); #table(as.numeric(rc[v,]))
x
as.numeric(rc[,id=="c5rp12p"]) ## UN DIPUTADO
as.numeric(rc[v,]) ## EL VOTO

## SORT
tmp <- 1:nrow(votdat) ## ORDORIG
votdat <- votdat[order(votdat$leg, votdat$yr, votdat$mo, votdat$dy, tmp),]
rc <- rc[order(votdat$leg, votdat$yr, votdat$mo, votdat$dy, tmp),]

save.image( paste(workdir, "data/votesForWeb", "rc60.RData", sep="/") )
# csv versions
write.table(dipdat, file = paste(workdir, "data/votesForWeb", "dipdat60.csv", sep="/"), sep=",", row.names = FALSE)
write.table(votdat, file = paste(workdir, "data/votesForWeb", "votdat60.csv", sep="/"), sep=",", row.names = FALSE)
write.table(rc, file = paste(workdir, "data/votesForWeb", "rc60.csv", sep="/"), sep=",", row.names = FALSE)




