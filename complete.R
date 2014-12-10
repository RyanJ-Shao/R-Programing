#first part
#
complete <- function(directory, id=1:332) {
  a = list.files(directory)
  filename<-paste(directory,"\\",a,sep="")
  bb=read.csv(filename[id[1]])
  bb=na.omit(bb)
  nobs=length(bb[,2])
  if (length(id)>1){
  for (i in 2:length(id)) {
    d=read.csv(filename[id[i]])
    d=na.omit(d)
    comd=length(d[,2])
    nobs=c(nobs,comd)
  }
  }
  return (data.frame(id,nobs))
}
