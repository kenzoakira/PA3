#read data and get ranked data

rankhospital <- function(state, outcome, num = "best"){
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  cat<-c("heart attack", "heart failure", "pneumonia")
  
  if(!(state %in% data$State)){stop("invalid state")}
  if(!outcome %in% cat){stop("invalid outcome")}
  
  if(outcome=="heart attack"){outcome<-11}
  if(outcome=="heart failure"){outcome<-17}
  if(outcome=="pneumonia"){outcome<-23}
  
  colnames(data)[outcome]<-"Rate"
  data<-subset(data,State==state,select=c(Hospital.Name,Rate,State))
  data$Rate<-as.numeric(data$Rate)
  data<-data[!is.na(data$Rate),]
  
  if(num=="best"){num<-1}
  if(num=="worst"){num<-nrow(data)}
  
  if(num>nrow(data)){Hosname=NA}
  else{
  Rank<-1:nrow(data)
  
  data<-data[order(data$Rate,as.character(data$Hospital.Name)),]  
  table<-cbind(data$Hospital.Name,data$Rate,Rank)
  table<-as.data.frame(table)
  colnames(table)<-c("Hospital.Name","Rate","Rank")
  table$Hospital.Name<-as.character(table$Hospital.Name)
  Hosname<-table[num,]$Hospital.Name
  }
  print(Hosname)
  
  
  
}