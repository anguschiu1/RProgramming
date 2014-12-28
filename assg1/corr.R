corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    cr <- numeric()
    comp_res <- complete(directory)
    for (i in comp_res[,1]) {
#                 message(paste("i=",i,sep=""))
        path <- paste("./",directory,"/",sprintf("%03d",i),".csv", sep = "")
        #         print(path)
        data <- read.csv(path)
        if (threshold > comp_res[i,2]) {
#             message(comp_res[i,1],",",comp_res[i,2], " does not have enough data of ",threshold)
#             cr[i]=0
        } else {
            nitrate <- data[["nitrate"]]
            sulfate <- data[["sulfate"]]
            good <- complete.cases(nitrate,sulfate)
#             message("i=",i," ,cor=",cor(nitrate[good],sulfate[good]))
            cr[i]=round(cor(nitrate[good],sulfate[good]),5)
        }
        
    }
cr[complete.cases(cr)]
    #     print(id)
#     result <- data.frame(id=id, nobs=sum)
#     result
}