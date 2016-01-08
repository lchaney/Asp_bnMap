#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.2.1 
# This script is used after you have a refined bionano assembly
# It calculates number of rmaps, length of assembly
# Min, Max, Average, and N50 for rmaps
#==============================================================================================#


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
unmap[unmap$cum_sum > max(unmap$cum_sum)/2,][1,2]


#similar plot as Crocker Fig 2
library(ggplot2)

ggplot(data = unmap, aes(ContigLength)) + 
  geom_histogram(fill = "darkblue") +
  scale_x_continuous(labels = function(x) x/1000) + #convert bp to kb for x
  labs(x = "Length of Consesus Maps (KB)", #x axis label
       y = "Genome Map Number") + #y axis label
  theme_bw()
