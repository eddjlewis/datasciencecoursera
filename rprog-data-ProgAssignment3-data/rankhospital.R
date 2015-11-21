rankhospital <- function(state, condition, num){
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
        
        #num can be best/worst or int
        #if bigger than list, return na
        
       
        #cooerce as int
        outcome[,column] <- as.numeric(outcome[,column])
        stateSplit <- split(outcome[,c("Hospital.Name","State",column)], outcome$State)
        
        stateView <- stateSplit[[state]]
        stateView <- stateView[order(-stateView[,column],stateView[,"Hospital.Name"],na.last = NA, decreasing = TRUE),]
        
        if(num == "best") num <- 1
        else if(num == "worst") num <- nrow(stateView)
        
        if(num == 0 | num > nrow(stateView)) return(NA)
        return(stateView[num,"Hospital.Name"])
}