library(dplyr)

best <- function(State,Outcome){
  
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
  
  State="SC"
  Outcome="heart attack"
  
  #solution for the problem
  desiredHospital <- subdata %>% 
                      filter(state == State) %>% 
                      select(`hospital name`,state,contains(Outcome)) %>% 
                      arrange_(.dots = as.name(Outcome)) %>% 
                      slice(1)
  
  desiredHospital$`hospital name`              

}