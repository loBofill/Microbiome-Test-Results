library(plyr)
library(dplyr)
library(formattable)

getPiCrustPercentiles <- function(piCrustResults, namePosition) {
    piCrustData <- fread('piCrust data.csv', header = TRUE)
    
    piCrustResults <- getFinalPiCrustResults(piCrustResults, piCrustData, namePosition)
    
    newPiCrustData <- prepareNewPiCrustData(piCrustData, piCrustResults)
    
    return(evaluatePiCrustPercentiles(newPiCrustData, names(piCrustResults)))
}

evaluatePiCrustPercentiles <- function(piCrustData, sampleNames) {
    samples <- sapply(sampleNames, function(x) which(x == names(piCrustData)))[-1]
    piCrustPercentiles <- as.data.frame(setNames(replicate(length(sampleNames), numeric(0), simplify = F), sampleNames))
    for(i in 1:nrow(piCrustData)) {
        samplePercentiles <- c()
        for(j in 1:length(samples)) {
            samplePercentiles[j] <- getSinglePercentile(piCrustData[i,], as.numeric(samples[j]))
        }
        piCrustPercentiles[i,] <- c(piCrustData$`Gene Abundance`[i], as.character(percent(samplePercentiles)))
    }
    return(piCrustPercentiles)
}

getSinglePercentile <- function(data, sample) {
    return(pnorm(as.numeric(unlist(data)[sample]), 
                 mean = mean(as.numeric(unlist(data)[-1])), 
                 sd = sd(as.numeric(unlist(data)[-1]))))
}

getFinalPiCrustResults <- function(piCrustResults, piCrustData, namePosition) {
    names(piCrustResults) <- c('otuID', sapply(names(piCrustResults)[-1], function(x) 
        ifelse(length(strsplit(x, "-")[[1]]) > 1, strsplit(x, "-")[[1]][namePosition], 
               ifelse(length(strsplit(x, "\\.")[[1]]) > 1, strsplit(x, "\\.")[[1]][namePosition], x))))
    
    specificResults <- piCrustResults[piCrustResults$otuID %in% piCrustData$`Gene Abundance`, ] %>% 
        select(-KEGG_Pathways)
    xenobioticsName <- piCrustData$`Gene Abundance`[grep('Xenobiotics', piCrustData$`Gene Abundance`)]
    xenobioticsResults <- as.list(c(xenobioticsName, 
                                    as.vector(sapply(select(piCrustResults[grep('Xenobiotics', piCrustResults$KEGG_Pathways), ], 
                                                            -c(otuID, KEGG_Pathways)), sum))))
    lpsResults <- as.list(c('LPS', as.vector(sapply(select(piCrustResults[grep('Lipopoly', piCrustResults$KEGG_Pathways), ], 
                                                           -c(otuID, KEGG_Pathways)), sum))))
    
    return(rbind(specificResults, xenobioticsResults, lpsResults))
}

prepareNewPiCrustData <- function(piCrustData, piCrustResults) {

    sampleNames <- names(piCrustResults)
    piCrustResults[,-1] <- sapply(piCrustResults[,-1], as.numeric)
    names(piCrustResults) <- sampleNames

    if(sum(sampleNames[-1] %in% names(piCrustData)) != 
       length(sampleNames[-1])) {
        
        newPiCrustData <- merge(x = piCrustData, y = piCrustResults[, c(1, which(!sampleNames[-1] %in% names(piCrustData)) + 1)], 
                                by.x=c('Gene Abundance'), by.y = c('otuID'), all.x = TRUE)
        
        piCrustDataOrder <- sapply(piCrustData$'Gene Abundance', function(x) 
            which(x == newPiCrustData$`Gene Abundance`))
        
        newPiCrustData <- newPiCrustData[piCrustDataOrder,]
        
        fwrite(newPiCrustData, file = 'piCrust data.csv', quote = FALSE, sep = ",", row.names = FALSE)
        return(newPiCrustData)
    }
    else {return(piCrustData)}
}
