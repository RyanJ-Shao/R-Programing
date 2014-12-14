## This function takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
##The function reads the .csv ﬁle and returns a 2-column data frame containing the hospital in each 
##state that has the ranking speciﬁed in num. 
##For example the function call rankall("heart attack", "best") would return a data frame containing the names of 
##the hospitals that are the best in their respective states for 30-day heart attack death rates. 
##The function should return a value for every state (some may be NA). 
##The ﬁrst column in the data frame is named hospital, which contains the hospital name, 
##and the second column is named state, which contains the 2-character abbreviation for the state name. 
##Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals 
##when deciding the rankings.


rankall <- function(outcome, num = "best") { ## Read outcome data
  hospname=c("heart attack","heart failure","pneumonia")
  hospnum=c(11,17,23)
  hospnn=data.frame(hospname,hospnum)
  outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  splitout<-split(outcomedata,outcomedata$State)
  hospital<-1000
  for (sta in splitout) {
    stahosp<-sta
    outnum<-hospnn$hospnum[which(hospnn$hospname==outcome)]
    if (length(outnum)==0){stop("invalid outcome", call. = TRUE)}
    unihosp<-stahosp[,c(outnum,2)]
    unihosp<-unihosp[unihosp[[1]]!="Not Available",]
    unihosp<-unihosp[order(as.numeric(unihosp[[1]]),unihosp[[2]]),]
    numm=num
    if (num=="best"){
      numm<-1
    }
    if (num=="worst"){
      numm<-nrow(unihosp)
    }
    hospital<-c(hospital,unihosp[numm,2])
  }
  hospital<-hospital[hospital!=1000]
  state<-names(splitout)
  resu<-data.frame(hospital,state)
  rownames(resu)<-state
  return (resu)
}
