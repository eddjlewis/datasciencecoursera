best <- function(state, condition){
        # condition can be "heart attack" , "heart failure", or "pneumonia"       
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if(nrow(outcome[outcome$State == state,]) == 0) {
                stop("invalid state")
        }
        
        column <- NULL
        if(condition == "heart attack") {
                
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(condition == "heart failure") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if( condition == "pneumonia") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        #cooerce as int
        outcome[,column] <- as.numeric(outcome[,column])
        
        stateSplit <- split(outcome[,c("Hospital.Name","State",column)], outcome$State)
       
        getWinner <- function(stateFrame){
                results <- stateFrame[which(stateFrame[,column] == min(stateFrame[,column], na.rm = TRUE)),"Hospital.Name"]
                sort(results)   
        }
        
        getWinner(stateSplit[[state]])
}