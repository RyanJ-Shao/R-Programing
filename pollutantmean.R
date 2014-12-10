#2014-12-10
#second part
pollutantmean <- function(directory, pollutant,id=1:332) {
  cho=c("sulfate"=2,"nitrate"=3)
  a = list.files(directory)
  filename<-paste(directory,"\\",a,sep="")
  b=read.csv(filename[id[1]])[cho[[pollutant]]]
  badb=is.na(b)
  b=b[!badb]
  plusb=sum(b)
  numb=length(b)
  if (length(id)>1){
  for (i in 2:length(id)) {
    d=read.csv(filename[id[i]])[cho[[pollutant]]]
    badd=is.na(d)
    d=d[!badd]
    plusd=c(sum(d))
    numd=c(length(d))
    plusb=c(plusb,plusd)  
    numb=c(numb,numd)
  }
  }
  return (sum(plusb)/sum(numb))
  
}
