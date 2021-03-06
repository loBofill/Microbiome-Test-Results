source('./Utils/bacteriaLists.R')

LEVEL_NAMES <- c("BAJO", "NORMAL/BAJO", "NORMAL", "NORMAL/ALTO", "ALTO")

getPhylumLevels <- function(phylumData, generaData) {

    phylumDataFiltered <- filterAndRound(phylumData, phylums)
    
    # archaeaBacteriaData <- sapply(filter(generaData, name %in% archaeaGeneras)[,-1], sum)
    # archaeaBacteria <- c(name = "Archaea:Bacteria", 
                         # ifelse(length(archaeaBacteriaData) > 0, 
                         # archaeaBacteriaData / (100 - archaeaBacteriaData),
                         # 0))
    
    firmicutesBacteroidetes <- c(name = "Firmicutes:Bacteroides", 
                                 phylumDataFiltered[2,-1] / phylumDataFiltered[1,-1])
    
    phylumDataFinal <- rbind(phylumDataFiltered, firmicutesBacteroidetes)
    phylumDataFinal[,-1] <- sapply(phylumDataFinal[,-1], as.numeric)
    
    phylumReferences <- read.xlsx2("VALORS REF.xlsx", sheetIndex = 3, stringsAsFactors = FALSE)
    names(phylumReferences) <- c("name", LEVEL_NAMES)
    phylumLevels <- getLevels(phylumDataFinal, phylumReferences)
    
    return(list(phylumDataFinal, phylumLevels))
}

getFamilyLevels <- function(familyData) {
    
    familyDataFiltered <- filterAndRound(familyData, unlist(families, use.names = FALSE))
    
    familyReferences <- read.xlsx2("VALORS REF.xlsx", sheetIndex = 4, stringsAsFactors = FALSE)
    names(familyReferences) <- c("name", LEVEL_NAMES)
    familyLevels <- getLevels(familyDataFiltered, familyReferences)
    
    return(list(familyDataFiltered, familyLevels))
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
    clusterData[,-1] <- sapply(clusterData[,-1], as.numeric)
    
    clusterReferences <- read.xlsx2("VALORS REF.xlsx", sheetIndex = 4, stringsAsFactors = FALSE)
    names(clusterReferences) <- c("name", LEVEL_NAMES)
    clusterLevels <- getLevels(clusterData, clusterReferences)
    
    return(list(clusterData, clusterLevels))
}

getGeneraLevels <- function(generaData) {
    
    generaDataFiltered <- filterAndRound(generaData, unlist(generas, use.names = FALSE))
    escherichiaShigella <- c('Escherichia/Shigella', sapply(filter(generaDataFiltered, name %in% generas$Proteobacteria$`Escherichia/Shigella`)[,-1], sum))
    escherichiaIndex <- which(generaDataFiltered$name == "Escherichia")
    generaDataFiltered <- rbind(generaDataFiltered[1 : escherichiaIndex - 1,], 
                                escherichiaShigella, 
                                generaDataFiltered[(escherichiaIndex + 2) : nrow(generaDataFiltered), ])
    generaDataFiltered[,-1] <- sapply(generaDataFiltered[,-1], as.numeric)
    
    generaReferences <- read.xlsx2("VALORS REF.xlsx", sheetIndex = 5, stringsAsFactors = FALSE)
    names(generaReferences) <- c("name", LEVEL_NAMES)
    generaLevels <- getLevels(generaDataFiltered, generaReferences)
    
    return(list(generaDataFiltered, generaLevels))
}

filterAndRound <- function(data, groupNames) {
    dataFiltered <- data %>% filter(name %in% groupNames) %>% na.omit()
    missing <- groupNames[!(groupNames %in% dataFiltered$name)]
    if (length(missing) > 0) {
        for (i in 1:length(missing)) {
            dataFiltered[nrow(dataFiltered) + 1,] <- as.list(c(missing[i], rep(0, ncol(dataFiltered) - 1)))
        }
    }
    dataFiltered <- dataFiltered[match(groupNames, dataFiltered$name),]
    dataFiltered[,-1] <- sapply(dataFiltered[,-1], as.numeric)
    rownames(dataFiltered) <- NULL
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
