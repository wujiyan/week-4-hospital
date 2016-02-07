best <- function(state,outcome) {
  x <- read.csv("C:/R/week 4 hospital/outcome-of-care-measures.csv")   #read the data
  statelist <- x$State
  if((state %in% statelist) == 0){
    print("invalid state")
    stop()
  }
  dislist <- c("heart attack","heart failure","pneumonia")
  if((outcome %in% dislist) == 0){
    print("invalid outcome")
    stop()
  }
  staterestriction <- subset(x, State == state)    #select specific data of seleted state
  if(outcome == "heart attack"){
    dismort <- staterestriction[,11]
    staterestriction <- staterestriction[!(is.na(dismort)),]
    hosrow <- which.min(as.numeric(as.character(staterestriction[,11])))    #get the hospital row with the minimum mortality
    hospitalname <- staterestriction[hosrow,2]
    hospitalname
  } else if(outcome == "heart failure"){
    dismort <- staterestriction[,17]
    staterestriction <- staterestriction[!(is.na(dismort)),]
    hosrow <- which.min(as.numeric(as.character(staterestriction[,17])))    #get the hospital row with the minimum mortality
    hospitalname <- staterestriction[hosrow,2]
    hospitalname
  } else if(outcome == "pneumonia"){
    dismort <- staterestriction[,23]
    staterestriction <- staterestriction[!(is.na(dismort)),]
    hosrow <- which.min(as.numeric(as.character(staterestriction[,23])))     #get the hospital row with the minimum mortality
    hospitalname <- staterestriction[hosrow,2]
    hospitalname
  }
}

rankhospital <- function(state,outcome,num = "best"){
  x <- read.csv("C:/R/week 4 hospital/outcome-of-care-measures.csv")   #read the data
  statelist <- x$State
  if((state %in% statelist) == 0){
    print("invalid state")
    stop()
  }
  dislist <- c("heart attack","heart failure","pneumonia")
  if((outcome %in% dislist) == 0){
    print("invalid outcome")
    stop()
  }
  newdata <- subset(x, State == state)    #select specific data of seleted state
  if(outcome == "heart attack"){
    rcolumn <- 11
  }else if(outcome == "heart failure"){
    rcolumn <- 17
  }else if(outcome == "pneumonia"){
    rcolumn <-23
  }
  newdata[,rcolumn] <- as.numeric(as.character(newdata[,rcolumn]))
  scolumn <- newdata[,rcolumn]       #specific column
  newdata <- newdata[!(is.na(scolumn)),]
  newdata <- newdata[order(newdata[,rcolumn]),]
  totalnumber <- nrow(newdata)
  if(num == "best"){
    newdata[1,2]
  }else if(num == "worst"){
    newdata[totalnumber,2]
  }else if(num <= totalnumber){
    newdata[num,2]
  }else {
    print("NA")
  }
}

rankall <- function(outcome, num = "best"){
  x <- read.csv("C:/R/week 4 hospital/outcome-of-care-measures.csv")   #read the data
  states_list <- levels(x[,7])
  total_number <- length(states_list)
  target <- data.frame(hospital_name = character(0),state = character(0),stringsAsFactors = FALSE)
  i <- 1
  for(i in 1:total_number){
    hospital_name <- rankhospital(states_list[i],outcome,num)
    target[i,] <- c(as.character(hospital_name),as.character(states_list[i]))
    i <- i+1
  }
  target
}