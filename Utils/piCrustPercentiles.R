percentile <- function(x,perc) ecdf(x)(perc)

loadPicrustData <- function(reportId) {
    setwd(paste(reportId, "/Organism_Comparison", sep = ""))
    unzip("Picrust.zip")
    #unlink("Picrust.zip")
}