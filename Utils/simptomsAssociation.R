getDiseaseProbabilities <- function(simptoms) {
    
    correlations <- read.csv2("simptomesGT simple.csv", stringsAsFactors = FALSE)
    correlations$X <- gsub(" ", "", correlations$X)
    
    probabilities <- simptoms[-c(1:nrow(simptoms)),]
    
    for (i in 2:ncol(correlations)) {
        probabilities[i,] <- c(names(correlations[i]), rep(0, ncol(simptoms) - 1))
        for (j in 2:ncol(simptoms)) {
            probabilities[i,j] <- sum(sapply(simptoms[,j], function(x) getDiseaseProbability(x, correlations[,c(1,i)])))
        }
    }
}

getDiseaseProbability <- function(simptomValue, correlations) {
    
}