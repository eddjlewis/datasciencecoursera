complete <- function(directory) {
        
        #get all files in directory with full paths
        fileNames <- list.files(directory, full.names=TRUE)
        #create dataframe with two vectors, file paths, and the list of non na rows for all the csvs
        data.frame(fileNames, 
                   nobs = sapply(fileNames, function(x) sum(complete.cases(read.csv(x)))), row.names = NULL
        )
}

corr <- function(directory, threshold = 0) {
        
        nobItems <- complete(directory)
        #filter rows for items where column 'nobs' is greater than threshold, and then filter to only 'filenames' column
        goodFiles <- as.vector(nobItems[nobItems$nobs > threshold,"fileNames"])
        
        #if empty, return zero length numeric vector
        if(length(goodFiles) == 0) return(numeric())
        
        #initialise result cr   
        cr <- c()
        for(f in 1:length(goodFiles)){
                #read a single csv file
                data <- read.csv(goodFiles[f])
                #trim data to only complete rows
                data <- data[complete.cases(data),]
                #get correlation of two columns in dataframe and add it to a vector
                cr <- c(cr, cor(data$sulfate, data$nitrate) ) # append corralations
        }
        
        return(cr)
}    