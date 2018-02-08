library(data.table)

source('./Utils/referenceLevels.R')
source('./Utils/simptomsAssociation.R')
source('./Utils/piCrustPercentiles.R')

loadData <- function(reportId, namePosition) {
    unzip(paste(reportId, ".zip", sep = ""), exdir = reportId)
    
    setwd(reportId)
    setwd(paste(list.dirs(recursive = FALSE)[1], "/Organism_Comparison", sep = ""))
    phylumFileNumber <- grep("xlsx", list.files("Phylum"))
    familyFileNumber <- grep("xlsx", list.files("Family"))
    generaFileNumber <- grep("xlsx", list.files("Genera"))
    phylumData <- read.xlsx2(paste("Phylum/", list.files("Phylum")[phylumFileNumber], sep = ""), sheetIndex = 1, stringsAsFactors = FALSE)
    familyData <- read.xlsx2(paste("Family/", list.files("Family")[familyFileNumber], sep = ""), sheetIndex = 1, stringsAsFactors = FALSE)
    generaData <- read.xlsx2(paste("Genera/", list.files("Genera")[generaFileNumber], sep = ""), sheetIndex = 1, stringsAsFactors = FALSE)
    setwd(paste0(strsplit(getwd(), "Microbiome")[[1]][1], "Microbiome-Test-Results/Data"))
    
    piCrustResults <- data.frame(fread(paste0(reportId,"/", reportId, "/","Picrust","/","3_predicted_metagenomes.txt")))
    
    dfs <- list(phylumData, familyData, generaData, piCrustResults)
    for(i in 1:3){ dfs[[i]] <- cleanData(dfs[[i]], namePosition) }
    
    return(dfs)
}

cleanData <- function(data, namePosition) {
    names(data)[-1] <- sapply(names(data)[-1], function(x) 
        ifelse(length(strsplit(x, "-")[[1]]) > 1, strsplit(x, "-")[[1]][namePosition], 
               ifelse(length(strsplit(x, "\\.")[[1]]) > 1, strsplit(x, "\\.")[[1]][namePosition], x)))
    data[, -1] <- lapply(data[, -1], as.numeric)
    data$name <- gsub('\\[[0-9]\\]+', '', as.character(data$name))
    return(data)
}

generateReportData <- function(files, namePosition) {
    phylumResults <- getPhylumLevels(files[[1]], files[[3]])
    familyResults <- getFamilyLevels(files[[2]])
    clusterResults <- getClusterLevels(files[[2]])
    generaResults <- getGeneraLevels(files[[3]])
    
    simptomsAssociation <- getDiseaseProbabilities(rbind(phylumResults[[2]],
                                                         familyResults[[2]],
                                                         clusterResults[[2]],
                                                         generaResults[[2]]))
    
    phylumResults <- preparePhylumResults(phylumResults)

    familyResults[[1]][, -1] <- sapply(familyResults[[1]][, -1], function(x) as.character(percent(x/100)))
    clusterResults[[1]][, -1] <- sapply(clusterResults[[1]][, -1], function(x) as.character(percent(x/100)))
    generaResults[[1]][, -1] <- sapply(generaResults[[1]][, -1], function(x) as.character(percent(x/100)))
    
    piCrustPercentiles <- getPiCrustPercentiles(files[[4]], namePosition)
    names(piCrustPercentiles) <- c("name", gsub("X", "", names(piCrustPercentiles[-1])))
    
    piCrustPercentiles <- piCrustPercentiles %>% removeXenobioticsName
    
    report <- rbind(phylumResults[[1]], phylumResults[[2]],
                    familyResults[[1]], familyResults[[2]],
                    clusterResults[[1]], clusterResults[[2]],
                    generaResults[[1]], generaResults[[2]],
                    simptomsAssociation, piCrustPercentiles)
    
    write.csv2(report,
               file = paste0(reportId, "/", reportId, " Data.csv"),
               quote = FALSE,
               dec = ".",
               row.names = FALSE)
    
    return(report)
}

preparePhylumResults <- function(phylumResults) {
    archaeaBacteriaRow <- which(phylumResults[[1]]$name == "Archaea:Bacteria")
    firmicutesBacteroidesRow <- which(phylumResults[[1]]$name == "Firmicutes:Bacteroides")
    phylumResults[[1]][-c(archaeaBacteriaRow, firmicutesBacteroidesRow), -1] <- 
        sapply(phylumResults[[1]][-c(archaeaBacteriaRow, firmicutesBacteroidesRow), -1], 
               function(x) as.character(percent(x/100)))
    phylumResults[[1]][archaeaBacteriaRow, -1] <- sapply(phylumResults[[1]][archaeaBacteriaRow, -1], 
                                                         function(x) as.character(digits(x, 5, format = 'f')))
    phylumResults[[1]][firmicutesBacteroidesRow, -1] <- sapply(phylumResults[[1]][firmicutesBacteroidesRow, -1], 
                                                               function(x) as.character(digits(x, 2, format = 'f')))
    return(phylumResults)
}

removeXenobioticsName <- function(data) {
    xenobioticsRow <- grepl("Xenobiotics", data$name)
    
    xenobioTicsName <- data$name[xenobioticsRow]
    
    data$name[xenobioticsRow] <- base::strsplit(xenobioTicsName, ";")[[1]][2]
    
    return(data)
}

