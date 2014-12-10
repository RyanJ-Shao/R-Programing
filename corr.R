#2014-12-10
#third part
corr <- function(directory, threshold = 0) {
  source("complete.R")
  id=1:332
  comm=complete(directory, id)
  comm=comm[which(comm[,2]>threshold),]
  a = list.files(directory)
  filename<-paste(directory,"\\",a,sep="")
  if (length(comm[,1])==0){
    return (as.numeric(c()))
  }
  b=read.csv(filename[comm[,1][1]])
  b=na.omit(b)
  realb=cor(b[,2],b[,3])
  if (length(comm[,1])>1){
  for (i in 2:length(comm[,1])) {
    d=read.csv(filename[comm[,1][i]])
    d=na.omit(d)
    reald=cor(d[,2],d[,3])
    realb=c(realb,reald)
    }
  }
  return (realb)
}
