
best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    err<-F
    
    ## Check that state and outcome are valid
    if (outcome!='heart attack' && outcome!='heart failure' && outcome!='pneumonia') {
        message("Error in best(\"",state,"\"",", \"",outcome,"\") : invalid outcome")
        err=T
    }
    if (!(state %in% data[,7])) {
        message("Error in best(\"",state,"\"",", \"",outcome,"\") : invalid state")
        err=T
    }
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    mortality_v <- NULL
    hospital_v <- NULL
    index <- NULL
    if (outcome=='heart attack') {
        index = 11
    } else if (outcome=='heart failure') {
        index = 17
    } else if (outcome=='pneumonia') {
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
        min <- min(mortality_v, na.rm=T)
        #        message("min=",min)
        resHos <- NULL
        for (i in seq_len(NROW(hospital_v))) {
            if (!is.na(mortality_v[i]) && mortality_v[i] == min) {
                resHos <- append(resHos,hospital_v[i])
            }
        }
        sort(resHos)    
    }
}