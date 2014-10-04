rm(list=ls(all=TRUE))
setwd("C:\\Users\\maya2301\\Documents\\Cours\\IFT501\\IFT501-aut-14\\TP1\\")
#setwd("~/TP1")
etud1 <- "Levesque-Gobeil, Simon"
etud2 <- "Provencher, Alex"
myData <- as.matrix(read.table("C:\\Users\\maya2301\\Documents\\Cours\\IFT501\\IFT501-aut-14\\TP1\\data.txt"))
#myData <- as.matrix(read.table("data.txt"))
myData

dataClass  <- read.table("C:\\Users\\maya2301\\Documents\\Cours\\IFT501\\IFT501-aut-14\\TP1\\classe.txt")
#dataClass  <- read.table("classe.txt")
dataClass

nbData <- nrow(myData)
nbData
nbAttribut <-ncol(myData)
nbAttribut
##===================== no 1 ==============================
nbSeg <- 10
lgSeg <- nbData / nbSeg
sortedData <- myData[sort.list(myData[,"A1"]), ]

splData <- lapply(seq(from = 1, to = nbData, by = lgSeg), function(i, dt) dt[i:(i+lgSeg - 1),], dt = sortedData)
splData <- lapply(splData, colMeans)
memFr <- do.call(rbind, splData)

rownames(memFr) <- rownames(memFr, do.NULL = FALSE, prefix = "cl-")
memFr <- memFr[,-1]

##===================== no 2 ==============================
dataEntro <- cbind(myData, dataClass[,"dataName"])
colnames(dataEntro)[nbAttribut+1] <- "dataName"
splDataEntro <- lapply(1:3,function(i,dt) dt[dt[,"dataName"] == i,],dt=dataEntro)
id1 <- c(min(splDataEntro[[1]][,"A2"]), max(splDataEntro[[1]][,"A2"]))
id2 <- c(min(splDataEntro[[2]][,"A2"]), max(splDataEntro[[2]][,"A2"]))
id3 <- c(min(splDataEntro[[3]][,"A2"]), max(splDataEntro[[3]][,"A2"]))

######

sortedDataEntro<- myData[sort.list(myData[,"A2"]), ]

T1 <- (sortedDataEntro[nbData/2+1,"A2"] - sortedDataEntro[nbData/2, "A2"])/2 + sortedDataEntro[nbData/2, "A2"]
T2 <- (sortedDataEntro[nbData/4+1,"A2"] - sortedDataEntro[nbData/4, "A2"])/2 + sortedDataEntro[nbData/4, "A2"]
#....

ent <- 0
for(cls in splDataEntro)
{
  freq <- nrow(cls) / nbData
  ent <- ent - freq * log(freq, 2)
}




##===================== no 3 ==============================
dd <- myData[,-1]
cov_matrix <- cov(myData[,-1])

maha <- numeric ()

for(i in (1:length(myData[,1]))){
  for(j in (1:length(myData[,1]))){
    if(i != j ){
      x <- dd[i,]
      y <- dd[j,]
      
      m <- solve(cov_matrix)
      
      d <- mahalanobis(x,y,cov_matrix)
      
      n <- myData[i,1]
      m <- myData[j,1]
      
      maha <- rbind(maha, c(d,n,m))
    }
  }
}

maha <- maha[order(maha[,1],decreasing=TRUE),]

maha1 <- maha[1,2]
maha2 <- maha[1,3]

##===================== fin du TP ==============================
ecrireReponse <- function() {
  reponseFileName <- paste0(etud1, " _ ", etud2,".txt")
  file.create(paste0("C:\\Users\\maya2301\\Documents\\Cours\\IFT501\\IFT501-aut-14\\TP1\\",etud1, " _ ", etud2,".txt"))
  #file.create(paste0("TP1",etud1, " _ ", etud2,".txt"))
  
  ecrire <- function(arg) {
    if (is.matrix(arg)){
      write.table(encodeString(arg), reponseFileName, quote = FALSE, sep="\t", append=TRUE)
    } else {
      write (encodeString(arg), reponseFileName,  sep="\t", append=TRUE)
    }
   }
  ecrire(reponseFileName)
  ecrire("---    reponse #1    ---") 
  ecrire(memFr)
  
  ecrire("---   no 2   ---")
  ecrire(id1)
  ecrire(id2)
  ecrire(id3)
  
  ecrire("---   no 3  ---")
  ecrire(maha1)
  ecrire(maha2)
  }
ecrireReponse()

