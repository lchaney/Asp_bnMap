#compare to Tricharpa

Asp_to_Ptric <- read.delim("~/Documents/Aspen/Asp_04_2_2015-05-18_09_45_Assemble_Molecules_to_Ptrichocarpa_210_v3.0_knickers_q1c1.map", header=TRUE, comment.char="#")
map <- Asp_to_Ptric[, 1:18] #variables 19-690 are just errors from reading in data

library(ggplot2)
#histogram of maped molecule pvalues
ggplot(data = map, aes(Log10Pvalue)) + geom_histogram(fill = "darkblue") + theme_bw() 


#plot of unique molecule placement
library(dplyr)
map_unique <- map %>% group_by(MoleculeId) %>% summarize(n=n()) %>% arrange(n)

ggplot(data = map_unique, aes(n)) + geom_histogram(fill = "darkblue") + theme_bw() + 
  labs(x = "Times Molecule Mapped", #x axis label
       y = "Count") #y axis label