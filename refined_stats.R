map <- read.delim("~/Documents/Aspen/EXP_REFINEFINAL1.cmap", header = FALSE, comment.char = "#")
names(map) <- c("CMapId", "ContigLength", "NumSites", "SiteID",
                "LabelChannel", "Position", "StdDev", "Coverage",
                "Occurrence", "GmeanSNR", "lnSNRsd SNR count", "SNR")

library(dplyr)

#subset file for just one row per contig (unique values) and arrange low to high by length
unmap <- map %>% select(CMapId:ContigLength) %>% distinct() %>% arrange(ContigLength)

nrow(unmap) #number of contig maps
sum(unmap$ContigLength) #total length of refined map

mean(unmap$ContigLength) #average length of contigs
min(unmap$ContigLength) #minimum length of contigs
max(unmap$ContigLength) #maximum length of contigs

#N50
#create a column with cumlative sum
unmap$cum_sum <- cumsum(unmap$ContigLength)

# Where is N50?
unmap[unmap$cum_sum > max(unmap$cum_sum)/2,][1,]


#