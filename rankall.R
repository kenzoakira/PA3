rankall <- function(outcome, num = "best"){
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  cat<-c("heart attack", "heart failure", "pneumonia")
  state<-NULL
  Rankinfor<-NULL
  rank<-num
  
  if(!outcome %in% cat){stop("invalid outcome")}
  
  if(outcome=="heart attack"){outcome<-11}
  if(outcome=="heart failure"){outcome<-17}
  if(outcome=="pneumonia"){outcome<-23}
  
  data<-as.data.frame(cbind(data$Hospital.Name,data$State,data[,outcome]))
  colnames(data)<-c("Hospital","State","Rate")
  data<-data[!is.na(data$Rate),]
  data<-data[order(data$State),]
  
  for(state in unique(data$State)){
    stateInfor<-data[data$State==state,]
    stateInfor$Hospital<-as.character(stateInfor$Hospital)
    stateInfor$Rate<-as.numeric(levels(stateInfor$Rate))[stateInfor$Rate]
    stateInfor<-stateInfor[order(stateInfor$Rate,stateInfor$Hospital),]
    if(num=="best"){rank<-1}
    if(num=="worst"){rank<-nrow(stateInfor)}
    if(rank>nrow(stateInfor)){
      singleRank<-cbind(NA,state)
    }
    else{
      singleRank<-cbind(stateInfor$Hospital[rank],state)
    }
    Rankinfor<-rbind(Rankinfor,singleRank)
    stateInfor<-NULL
  }
  Rankinfor<-as.data.frame(Rankinfor)
  colnames(Rankinfor)<-c("hospital","state")
  print(Rankinfor)
  
}