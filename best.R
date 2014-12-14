##This function take two arguments: the 2-character abbreviated name of a state and an outcome name. 
##The function reads the csv ﬁle and returns a character vector with the name of the hospital that has the best 
##(i.e. lowest) 30-day mortality for the speciﬁed outcome in that state. 
##The hospital name is the name provided in the Hospital.Name variable. 
##The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. 
##Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the 
##rankings.

best <- function(state, outcome) {
  hospname=c("heart attack","heart failure","pneumonia")
  hospnum=c(11,17,23)
  hospnn=data.frame(hospname,hospnum)
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  splitout<-split(outcomedata,outcomedata$State)
  stahosp<-splitout[[state]]
  if (length(stahosp)==0){stop("invalid state ", call. = TRUE)}
  outnum<-hospnn$hospnum[which(hospnn$hospname==outcome)]
  if (length(outnum)==0){stop("invalid outcome ", call. = TRUE)}
  perout<-stahosp[[outnum]]
  
  mindeath<-min(as.numeric(perout[perout!="Not Available"]))
  vectout<-perout==mindeath
  return (stahosp[vectout,2][1])
}
