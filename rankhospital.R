##This function takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), 
##and the ranking of a hospital in that state for that outcome (num). 
##The function reads the csv ﬁle and returns a character vector with the name of the hospital that has the ranking 
##speciﬁed by the num argument. 

rankhospital <- function(state, outcome, num = "best") { 
  hospname=c("heart attack","heart failure","pneumonia")
  hospnum=c(11,17,23)
  hospnn=data.frame(hospname,hospnum)
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  splitout<-split(outcomedata,outcomedata$State)
  stahosp<-splitout[[state]]
  if (length(stahosp)==0){stop("invalid state", call. = TRUE)}
  outnum<-hospnn$hospnum[which(hospnn$hospname==outcome)]
  if (length(outnum)==0){stop("invalid outcome", call. = TRUE)}
  unihosp<-stahosp[,c(outnum,2)]
  unihosp<-unihosp[unihosp[[1]]!="Not Available",]
  unihosp<-unihosp[order(as.numeric(unihosp[[1]]),unihosp[[2]]),]
  if (num=="best"){
    num<-1
  }
  if (num=="worst"){
    num<-(nrow(unihosp))
  }
  reshosp<-unihosp[num,2] 
  return (reshosp)
}
