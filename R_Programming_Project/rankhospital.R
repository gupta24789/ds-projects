library(dplyr)

rankhospital <- function(State,Outcome,Rank){
  
  #load the data
  mydata <- read.csv("outcome-of-care-measures.csv",header=TRUE,stringsAsFactors = FALSE)
  
  #checking for valid state and outcome
  if(!(State %in% mydata$State)){
    stop("invalid state")
  }else if(!(Outcome %in% c("heart attack","heart failure","pneumonia"))){
    stop("invalid outcome")
  }
  
  
  
  #data cleaning 
  subdata <- select(mydata,Hospital.Name,State,starts_with("Hospital.30.Day.Death"))
  names(subdata) <- gsub("[.]"," ",tolower(names(subdata)))
  names(subdata)[3:length(names(subdata))] <- c("heart attack","heart failure","pneumonia")
  subdata[grepl("Not Available",subdata$`heart attack`),] <- NA
  subdata[grepl("Not Available",subdata$`heart failure`),] <- NA
  subdata[grepl("Not Available",subdata$pneumonia),] <- NA
  subdata[3:5] <- apply(subdata[3:5],2,as.numeric)
  
  
  State="NC"
  Outcome="heart attack"
  Rank="Worst"
  
  #solution for the problem
  desiredHospital <- subdata %>% 
    filter(state == State) %>% 
    select(`hospital name`,state,contains(Outcome)) %>% 
    arrange_(.dots = c(as.name(Outcome),as.name("hospital name")))
   
  if(tolower(Rank)=="best"){
    pos <- 1
  }else if(tolower(Rank) =="worst"){
    pos <- nrow(desiredHospital)
  }else{
    pos <- Rank
  } 
  
  desiredHospital <- slice(desiredHospital,pos)
  desiredHospital$`hospital name`        
  
}