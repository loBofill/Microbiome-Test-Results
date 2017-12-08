
LEVEL_NAMES <- c("BAJO", "NORMAL/BAJO", "NORMAL", "NORMAL/ALTO", "ALTO")

getPhylumLevels <- function(phylumData, generaData) {

    phylumDataFiltered <- filterAndRound(phylumData, phylums)
    
    archaeaBacteriaData <- sapply(filter(generaData, name %in% archaeaGeneras)[,-1], sum)
    archaeaBacteria <- c(name = phylums[7], round(archaeaBacteriaData / (100 - archaeaBacteriaData),5))
    
    firmicutesBacteroidetes <- c(name = phylums[8], round(phylumDataFiltered[2,-1] / phylumDataFiltered[1,-1], 2))
    
    phylumDataFinal <- rbind(phylumDataFiltered, archaeaBacteria, firmicutesBacteroidetes)
    phylumDataFinal[,-1] <- apply(phylumDataFinal[,-1], 2, function(x) as.numeric(x))
    
    phylumReferences <- read.xlsx2("VALORS REF.xlsx", sheetIndex = 3, stringsAsFactors = FALSE)
    names(phylumReferences) <- c("name", LEVEL_NAMES)
    phylumLevels <- getLevels(phylumDataFinal, phylumReferences)
    
    return(rbind(phylumDataFinal, phylumLevels))
}

getFamilyLevels <- function(familyData) {
    
    familyDataFiltered <- filterAndRound(familyData, unlist(families, use.names = FALSE))
    
    familyReferences <- read.xlsx2("VALORS REF.xlsx", sheetIndex = 4, stringsAsFactors = FALSE)
    names(familyReferences) <- c("name", LEVEL_NAMES)
    familyLevels <- getLevels(familyDataFiltered, familyReferences)
    
    return(rbind(familyDataFiltered, familyLevels))
}

getClusterLevels <- function(familyData) {
    
    clusterDataFiltered <- filterAndRound(familyData, unlist(clusterFamilies, use.names = FALSE))
    
    clusterData <- clusterDataFiltered[-c(1:nrow(clusterDataFiltered)), ]
    for (i in 1:length(clusterFamilies)) {
        clusterData[i,] <- c(names(clusterFamilies)[i], 
                             sapply(filter(clusterDataFiltered, 
                                           name %in% clusterFamilies[[i]])[,-1], 
                                    sum))
    }
    clusterData[,-1] <- apply(clusterData[,-1], 2, function(x) as.numeric(x))
    
    clusterReferences <- read.xlsx2("VALORS REF.xlsx", sheetIndex = 4, stringsAsFactors = FALSE)
    names(clusterReferences) <- c("name", LEVEL_NAMES)
    clusterLevels <- getLevels(clusterData, clusterReferences)
    
    return(rbind(clusterData, clusterLevels))
}

getGeneraLevels <- function(generaData) {
    
    generaDataFiltered <- filterAndRound(generaData, unlist(generas, use.names = FALSE))
    
    generaReferences <- read.xlsx2("VALORS REF.xlsx", sheetIndex = 5, stringsAsFactors = FALSE)
    names(generaReferences) <- c("name", LEVEL_NAMES)
    generaLevels <- getLevels(generaDataFiltered, generaReferences)
    
    return(rbind(generaDataFiltered, generaLevels))
}

filterAndRound <- function(data, groupNames) {
    dataFiltered <- data %>% filter(name %in% groupNames)
    dataFiltered <- dataFiltered[match(groupNames, dataFiltered$name),]
    dataFiltered[,-1] <- round(dataFiltered[,-1], 2)
    return(dataFiltered)
}

getLevels <- function(data, references) {
    levels <- data[-c(1:nrow(data)),]
    for (i in 1:nrow(data)) {
        reference <- as.vector(references %>% filter(name == data$name[i]) %>% select(-name))
        levels[i,] <- c(data[i,1], sapply(data[i,-1], 
                                          function(x) getLevel(x, reference)))
    }
    return(levels)
}

getLevel <- function(observation, reference) {
    for (i in 1:length(reference)) {
        referenceValue <- as.numeric(reference[1,i])
        if (!is.na(referenceValue) && observation >= referenceValue) {
            i <- i + 1
        } else {
            break
        }
    }
    return (LEVEL_NAMES[i])
}
