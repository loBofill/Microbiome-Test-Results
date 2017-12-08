loadData <- function(reportId) {
    unzip(paste(reportId, ".zip", sep = ""), exdir = reportId)
    
    setwd(paste(reportId, "/Organism_Comparison", sep = ""))
    phylumData <- read.xlsx2(paste("Phylum/", list.files("Phylum")[3], sep = ""), sheetIndex = 1, stringsAsFactors = FALSE)
    familyData <- read.xlsx2(paste("Family/", list.files("Family")[3], sep = ""), sheetIndex = 1, stringsAsFactors = FALSE)
    generaData <- read.xlsx2(paste("Genera/", list.files("Genera")[3], sep = ""), sheetIndex = 1, stringsAsFactors = FALSE)
    
    dfs <- list(phylumData, familyData, generaData)
    for(i in 1:length(dfs)){ dfs[[i]] <- cleanData(dfs[[i]]) }
    
    return(dfs)
}

cleanData <- function(data) {
    names(data)[-1] <- lapply(strsplit(names(data)[-1], "\\."), function(x) x[4])
    data[, -1] <- lapply(data[, -1], as.numeric)
    data$name <- gsub('\\[[0-9]\\]+', '', as.character(data$name))
    return(data)
}

loadReferences <- function() {
    phylumReferences <- 
    picrust <- read.xlsx2("PICRUST SUMMARY ALL.xlsx", sheetIndex = 1, stringsAsFactors = FALSE) %>% select(-REF.VALUES)
    simptoms <- read.csv2("simptomesGT simple.csv", stringsAsFactors = FALSE)
    simptoms$X <- gsub(" ", "", simptoms$X)
}

percentile <- function(x,perc) ecdf(x)(perc)

loadPicrustData <- function(reportId) {
    setwd(paste(reportId, "/Organism_Comparison", sep = ""))
    unzip("Picrust.zip")
    #unlink("Picrust.zip")
}