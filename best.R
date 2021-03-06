best<-function(state,outcome){
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character",na.strings="Not Available")
  cat<-c("heart attack", "heart failure", "pneumonia")

  
  if(!(state %in% data$State)){stop("invalid state")}
  if(!outcome %in% cat){stop("invalid outcome")}
  
  returnMIN<-function(x,y){
    data<-data[data$State==y,]
    min<-min(as.numeric(data[,x]),na.rm=TRUE)
    list<-subset(data,data[,x]==min,select=Hospital.Name,drop=TRUE)
    list<-list[order(list)]
    as.character(list[1])
  }
  if(outcome=="heart attack"){
    m<-returnMIN(11,state)
  }
  if(outcome=="heart failure"){
    m<-returnMIN(17,state)
  }
  if(outcome=="pneumonia"){
    m<-returnMIN(23,state)
  }
  return(m)
 
  
}