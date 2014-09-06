complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    sum <- vector()
    for (i in id) {
#         print(paste("i=",i,sep=""))
        path <- paste("./",directory,"/",sprintf("%03d",i),".csv", sep = "")
#         print(path)
        data <- read.csv(path)        
        tempsum <- sum(complete.cases(data))
#         print(tempsum)
        sum <- append(sum,tempsum)
#         print(sum)
    }
#     print(id)
    result <- data.frame(id=id, nobs=sum)
    result
}