rankhospital <- function(state, outcome, num="best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    err<-F
    
    ## Check that state and outcome are valid
    if (outcome!="heart attack" && outcome!="heart failure" && outcome!="pneumonia") {
#         message("Error in rankhospital(\"",state,"\"",", \"",outcome,"\") : invalid outcome")
#         err=T
        stop("invalid outcome")
    }
    if (num!="best" && num!="worst" && is.na(suppressWarnings(as.numeric(num)))) {
#         message("Error in rankhospital(\"",state,"\"",", \"",outcome,"\", \"",num,"\") : invalid num")
#         err=T
        stop("invalid num")
    }
    if (!(state %in% data[,7])) {
#         message("Error in rankhospital(\"",state,"\"",", \"",outcome,"\") : invalid state")
#         err=T
        stop("invalid state")
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate   
    mortality_v <- NULL
    hospital_v <- NULL
    index <- NULL

    if (outcome=="heart attack") {
        index = 11
    } else if (outcome=="heart failure") {
        index = 17
    } else if (outcome=="pneumonia") {
        index = 23
    } else {
        err=T
    }
    if (!err) {
        for (i in seq_len(nrow(data))) {
            if (data[i,7]==state){
                hospital_v <- append(hospital_v, data[i,2])
                mortality_v <- append(mortality_v, suppressWarnings(as.numeric(data[i,index])))
            }
        }
#         message(NROW(hospital_v))
#         message(NROW(mortality_v))
        unsorted_res <- data.frame(hospital_v, mortality_v, stringsAsFactors=F)
        sorted_res <- unsorted_res[order(unsorted_res[,2],unsorted_res[,1]),]
        if (num=="best") {
            sorted_res[1,1]   
        } else if (num=="worst") {
            minus <- 0
            while (is.na(sorted_res[nrow(sorted_res)-minus,2])) {
                minus <- minus+1
            }
            sorted_res[nrow(sorted_res)-minus,1]
#             sorted_res
        } else {
            if (as.numeric(num)>nrow(sorted_res)||(as.numeric(num)<1)){
                result <- NA
                result
            } else {
                sorted_res[as.numeric(num),1]
            }
        }
    }
}