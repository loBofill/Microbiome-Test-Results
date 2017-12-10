getDiseaseProbabilities <- function(simptoms) {
    
    correlations <- read.csv2("simptomesGT simple.csv", stringsAsFactors = FALSE)
    correlations$X <- gsub(" ", "", correlations$X)
    
    probabilities <- simptoms[-c(1:nrow(simptoms)),]
    
    for (i in 2:ncol(correlations)) {
        probabilities[i-1,] <- c(names(correlations)[i], rep(0, ncol(simptoms) - 1))
        for (j in 2:ncol(simptoms)) {
            probabilities[i-1,j] <- sum(apply(simptoms[, c(1,j)], 1,
                                              function(x) getDiseaseProbability(x, correlations[,c(1,i)])))
        }
    }
    
    return(probabilities)
}

getDiseaseProbability <- function(simptomValue, correlation) {
    if (simptomValue[2] != "NORMAL" & simptomValue[1] %in% correlation$X) {
        correlationValue <- correlation[which(correlation$X == simptomValue[1]),2]
        if (!is.na(correlationValue) & is.numeric(correlationValue)) {
            convertedValue <- valueConvert(simptomValue[2])
            if (sign(correlationValue) == sign(convertedValue)) {
                return(abs(convertedValue))
            }
        }
    }
    return(0)
}

valueConvert <- function(simptom) {
    switch(simptom,
           "ALTO" = 1,
           "NORMAL/ALTO" = 0.5,
           "NORMAL" = 0,
           "NORMAL/BAJO" = -0.5,
           "BAJO" = -1)
}
