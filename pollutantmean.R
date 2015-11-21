pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        #Solution 1
        #         grandTotal <- 0
        #         grandTotalRows <- 0
        #         
        #         for(n in id){
        #                 cat('file - ',n, '\n')
        #                 file <- loadFromCsv(n)
        #                        
        #                 colSum <- sum(file[, "sulfate"], na.rm = TRUE)
        #                 numberOfRows <- sum(!is.na(file[,"sulfate"]))
        #                 
        #                 grandTotal <- grandTotal + colSum
        #                 grandTotalRows <- grandTotalRows + numberOfRows
        #               
        #         }
        # 
        #         grandTotal / grandTotalRows
        
        #Solution 2
        #setwd(directory)
        #get vector of filenames
        fileNames <- paste(directory,"/",sprintf('%03d', id),'.csv',sep = "")
        #get vector of dataframes
        dataFrames <- lapply(fileNames, read.csv)
        #rowbind all dataframes together
        data <- do.call("rbind",dataFrames)
        mean(data[,pollutant], na.rm = TRUE)
}