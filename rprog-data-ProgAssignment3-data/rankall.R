rankall <- function(condition, num = "best"){
        # condition can be "heart attack" , "heart failure", or "pneumonia"       
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if(condition == "heart attack") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(condition == "heart failure") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if( condition == "pneumonia") {
                column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                stop("invalid outcome")
        }
        
        #I've written 2 solutions, this variable is how I switch between them
        solution <- 2
        
        if(solution == 1) {
                #Solution 1
                message("solution 1")
                #cooerce as int
                outcome[,column] <- as.numeric(outcome[,column])
                stateSplit <- split(outcome[,c("Hospital.Name","State",column)], outcome$State)
                
                getRanking <- function(stateView){
                        stateView <- stateView[order(-stateView[,column],stateView[,"Hospital.Name"],na.last = NA, decreasing = TRUE),]
                        
                        #num can be best/worst or int
                        #if bigger than list, return na
                        if(num == "best") num <- 1
                        
                        hospitalName <- NA
                        stateName <- stateView$State[1]
                        
                        if(is.numeric(num) && num > nrow(stateView)) hospitalName <- NA
                        else if(num == "worst") hospitalName <- stateView[nrow(stateView),"Hospital.Name"]
                        else hospitalName <- stateView[num,"Hospital.Name"]
                        
                        return(c(hospital = hospitalName, state = stateName))
                }
                
                result <- t(sapply(stateSplit,getRanking))
                result<- data.frame(result)
                result <- result[order(result$state),]
                
        } else if(solution == 2){
                #another way to do things
                message("solution 2")
                
                if(num == "best") num <- 1
                #         #Solution 2
                
                outcome <- outcome[order(as.numeric(outcome[,column]),outcome$Hospital.Name, na.last=NA),]
                
                getRightOne <- function(stateDf){
                        
                        if(is.numeric(num) && num > length(stateDf)) return(NA)
                        else if(num == "worst") return(stateDf[length(stateDf)])
                        else return(stateDf[num])
                }
                
                hospital <- tapply(outcome[,"Hospital.Name"],outcome$State,getRightOne)
                data.frame(hospital, state=names(hospital))
        }
}