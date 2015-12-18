#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.2.1 
# This script is used to calculate and visulaize raw molecules stats 
#==============================================================================================#

mol_stats <- read.delim("~/Documents/Aspen/mol_stats.txt")

dim(mol_stats)[1] #number of molecules

median(mol_stats$length) #median molecule length
mean(mol_stats$length) #mean molecule length
max(mol_stats$length) #max molecule length
min(mol_stats$length) #min molecule length (filter >100kb)


#N50
library(dplyr)
#new data frame
mol2 <- mol_stats %>% arrange(length) %>% mutate(cum_sum = cumsum(length))
#N50
mol2[mol2$cum_sum > max(mol2$cum_sum)/2,][1,2]


print(tot <- sum(mol_stats$length)) #total throughput
asp_size <- 485000000 #genome size ~485MB
tot/asp_size #coverage

#number of labels
sum(mol_stats$labels)

#average label density (per 100kb)
round((sum(mol_stats$labels)/sum(mol_stats$length)*100000), 3)


#plot raw molecule size
library(ggplot2)
all_mol_hist <- ggplot(data = mol_stats, 
                       aes(x = length)) + 
                geom_histogram(fill = "darkblue") +
                theme_bw() +
                labs(x = "Molecule Length (KB)", #x axis label
                     y = "Counts") + #y axis label
                scale_x_continuous(labels = function(x) x/1000) #convert bp to kb for x

#plot subset of >500kb
above500_mol_hist <- ggplot(data = mol_stats[which(mol_stats$length >= 500000),], 
                            aes(x = length)) + 
                      geom_histogram(fill = "darkblue") +
                      theme_bw() +
                      labs(x = "Molecule Length (KB)", #x axis label
                           y = "Counts") + #y axis label
                      scale_x_continuous(labels = function(x) x/1000) #convert bp to kb for x

#plot with inset
library(grid)
full <- function() {
  print(all_mol_hist)
  theme_set(theme_bw(base_size = 8))
  theme_minimal()
  print(above500_mol_hist, vp = vp)
  theme_set(theme_bw())
}

vp <- viewport(width = 0.5, height = 0.5, x = unit(0.72, "npc"), y = unit(0.7, "npc"))
full()




#plot label density
levels <- c(100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, Inf)
lr_labels <- c("100-150", "150-200", "200-250", "250-300", "300-350", "350-400", "400-450", "450-500", ">500")

mol_stats <- mol_stats %>% 
  mutate(lrange = cut(length, breaks = levels, labels = c(1:9), right = FALSE), 
         labden = (labels/length)*100000)

labden_plot <- ggplot(data = mol_stats, 
                        aes(x = lrange, y = labden)) + 
  geom_boxplot(aes(fill = factor(lrange))) + 
  theme_bw() + 
  theme(legend.position = "none") +
  labs(x = "Length (kb)", #x axis label
       y = "Label density (per 100 kb)") + #y axis label
  scale_x_discrete(labels = lr_labels) + 
  scale_fill_brewer(palette = "Dark2")


ggplot(data = mol_stats, aes(x = length, y = labels)) + geom_point()


#filter out points with label density above and below whiskers at alpha 1.5
boxstat <- boxplot.stats(mol_stats$labden)$stats

out_mol_stats <- mol_stats %>% filter(labden <= boxstat[1] | labden >= boxstat[5])
fil_mol_stats <- mol_stats %>% filter(labden > boxstat[1] & labden < boxstat[5])

