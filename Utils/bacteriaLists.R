phylums <- c("Bacteroidetes",
                "Firmicutes",
                "Proteobacteria",
                "Actinobacteria",
                "Verrucomicrobia",
                "Fusobacteria")

bacteroidetesFamilies <- c("Bacteroidaceae",
                           "Porphyromonadaceae",
                           "Rikenellaceae",
                           "Prevotellaceae")

firmicutesFamilies <- c("Eubacteriaceae",
                        "Lachnospiraceae",
                        "Ruminococcaceae",
                        "Lactobacillaceae")

proteobacteriaFamilies <- c("Enterobacteriaceae")

actinobacteriaFamilies <- c("Bifidobacteriaceae")

groups <- c("Clostridium")

clusters <- c("Cluster IV",
              "Cluster IX",
              "Cluster XIVa")

clusterFamilies <- list("Ruminococcaceae",
                        c("Acidaminococcaceae",
                          "Veillonellaceae"),
                        "Lachnospiraceae")

bacteroidetesGeneras <- c("Bacteroides",
                          "Parabacteroides",
                          "Alistipes",
                          "Prevotella")

firmicutesGeneras <- c("Dorea",
                       "Blautia",
                       "Butyrivibrio",
                       "Roseburia",
                       "Faecalibacterium",
                       "Ruminococcus",
                       "Subdoligranulum",
                       "Anaerotruncus",
                       "Clostridium",
                       "Eubacterium",
                       "Holdemania",
                       "Enterococcus",
                       "Streptococcus",
                       "Lactobacillus")

proteobacteriaGeneras <- list('Escherichia/Shigella' = c("Escherichia","Shigella"),
                           "Klebsiella",
                           "Desulfovibrio",
                           "Campylobacter",
                           "Haemophilus")

names(proteobacteriaGeneras) <- c("Escherichia/Shigella",
                                  "Klebsiella",
                                  "Desulfovibrio", 
                                  "Campylobacter",
                                  "Haemophilus")

actinobacteriaGeneras <- c("Collinsella",
                           "Bifidobacterium")

verrucomicrobiaGeneras <- c("Akkermansia")

fusobacteriaGeneras <- c("Fusobacterium")

archaeaGeneras <- c("Methanobrevibacter")

families <- list(bacteroidetesFamilies, 
                 firmicutesFamilies, 
                 proteobacteriaFamilies, 
                 actinobacteriaFamilies)

names(families) <- phylums[1:4]

groupClusters <- list(clusters)
names(groupClusters) <- groups
names(clusterFamilies) <- clusters

generas <- list(bacteroidetesGeneras, 
                firmicutesGeneras, 
                proteobacteriaGeneras, 
                actinobacteriaGeneras, 
                verrucomicrobiaGeneras,
                fusobacteriaGeneras,
                archaeaGeneras)

names(generas) <- c(phylums[1:6], "archaea")
