#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.2.1 
# This is an earilier version of OMVis
# Graphical visulaization of your paramater search
#==============================================================================================#

Quality <- read.delim("~/Documents/Aspen/Quality.txt")

convert <- function(x){}

Quality$fp <- as.factor(gsub("_.*", "", gsub(".*fp", "", Quality$Assembly.Directory) ) ) # fp
Quality$fn <- as.factor(gsub("_.*", "", gsub(".*fn", "", Quality$Assembly.Directory) ) )# fn
Quality$pval <- as.factor(gsub("_.*", "", gsub(".*pval", "", Quality$Assembly.Directory) ) )# Pvalues
Quality$minlen <- as.factor(gsub("_.*", "", gsub(".*minlen", "", Quality$Assembly.Directory) ) )#minlen
Quality$minsites <- as.factor(gsub("_.*", "", gsub(".*minsites", "", Quality$Assembly.Directory) ) )#minsite

Quality$pval2 <- signif(as.numeric(gsub("_.*", "", gsub(".*pval", "", Quality$Assembly.Directory) ) ), 3)# Pvalues

head(Quality)
str(Quality)
#g <- c("group1","group1","group2","group3","group1")
#f <- as.factor(g)

BPtoMB <- function(x){ 
  x/1000000 
}

library(ggplot2)

ggplot(data = Quality, #specify input data
       aes(x = Length.in.Basepairs, y = Contig.Count)) + #specify x and y
  geom_point(aes(color = minsites, #data as points, specify color 
                 shape = fp, #specify shape
                 size = fn), #specify size
             alpha = .6) + #add transparency to points so you can see overlaps
  scale_size_manual(values = c(3, 5, 7)) + #specify size of points
  facet_grid(minlen ~ pval, labeller = label_both) + #plot split into subplots w/ labels
  theme_bw() + #change the aesthetics of graph with white background
  scale_x_continuous(labels = function(x) x/1000000) + #convert bp to mb for x
  labs(x = "Length (MB)", #x axis label
       y = "Contig Count") + #y axis label
  scale_color_manual(
    values = c("#d7191c", "#fdae61", "#2c7bb6")) + #colors red, yellow, blue
  scale_shape_discrete(solid = TRUE) #fill in shapes

#creating the plot
param_graph <- ggplot(data = Quality, #specify input data
                      aes(x = Length.in.Basepairs, y = Contig.Count)) + #specify x and y
  geom_point(aes(color = minlen, #data as points, specify color 
                 shape = minsites, #specify shape
                 size = pval), #specify size
             alpha = .6) + #add transparency to points so you can see overlaps
  scale_size_manual(values = c(3, 4, 5, 6, 7)) + #specify size of points
  #  facet_grid(fp ~ fn, labeller = label_both) + #plot split into subplots w/ labels
  theme_bw() + #change the aesthetics of graph with white background
  scale_x_continuous(labels = function(x) x/1000000) + #convert bp to mb for x
  labs(x = "Length (MB)", #x axis label
       y = "Contig Count") + #y axis label
  scale_color_manual(
    values = c("#d7191c", "#fdae61", "#2c7bb6")) + #colors red, yellow, blue
  scale_shape_discrete(solid = TRUE) #fill in shapes

#save the plot
ggsave(filename = "aspen_param_graph.png", #file extension will speciry type
       plot = param_graph, #which plot to save
       width = 190, #width of plot
       height = 190, #height of plot
       units = "mm", #units used for width and height 
       dpi = 600) #resolution






ggplot(data = Quality, aes(x = Length.in.Basepairs, y = Contig.Count)) +
  geom_point(aes(color = minlen, shape = minsites,
                 size = pval), alpha = .6) +
#  facet_grid(fp ~ fn, labeller = label_both) +
  theme_bw() +
  scale_x_continuous(labels = function(x) x/1000000) +
  labs(tittle = "Aspen Parameter Search", x = "Length (MB)", y = "Contig Count") +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#2c7bb6")) +
  scale_shape_discrete(solid = TRUE)


ggplot(data = Quality, aes(x = Length.in.Basepairs, y = Contig.Count)) +
  geom_point(aes(color = minlen, shape = minsites,
                 size = pval), alpha = .6) +
#  facet_grid(fp ~ fn, labeller = label_both) +
  theme_bw() +
  scale_x_continuous(labels = BPtoMB) +
  labs(tittle = "Aspen Parameter Search", x = "Length (MB)", y = "Contig Count") +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#2c7bb6")) +
  scale_shape_discrete(solid = TRUE)



ggplot(data = Qual, aes(x = Length.in.Basepairs, y = Contig.Count)) +
  geom_point(aes(color = as.factor(minlen), shape = as.factor(minsites),
                 size = as.factor(pval)), alpha = .6) +
  facet_grid(fp ~ fn, labeller = label_both) +
  theme_bw() +
  scale_x_continuous(labels = BPtoMB) +
  labs(tittle = "Aspen Parameter Search", x = "Length (MB)", y = "Contig Count") +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#2c7bb6")) +
  scale_shape_discrete(solid = TRUE)

#zoom in to contigs under 500
library(dplyr)
qualsmall <- filter(Qual, ContigCount <= 500)
ggplot(data = qualsmall, aes(x = LengthBP, y = ContigCount)) +
  geom_point(aes(color = as.factor(MINLEN), shape = as.factor(MINSITES),
                 size = as.factor(PVAL)), alpha = .6) +
  #facet_grid(FP ~ FN, labeller = label_both) +
  theme_bw() +
  scale_x_continuous(labels = BPtoMB) +
  labs(tittle = "Aspen Parameter Search", x = "Length (MB)", y = "Contig Count") +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#2c7bb6")) +
  scale_shape_discrete(solid = TRUE)



#Alternative view - facet my minlength and minsites -- doesn't look as good
ggplot(data = Qual, aes(x = LengthBP, y = ContigCount)) +
  geom_point(aes(color = as.factor(FN), shape = as.factor(FP),
                 size = as.factor(PVAL)), alpha = .6) +
  facet_grid(MINLEN ~ MINSITES, labeller = label_both) +
  theme_bw() +
  scale_x_continuous(labels = BPtoMB) +
  labs(tittle = "Aspen Parameter Search", x = "Length (MB)", y = "Contig Count") +
  scale_color_manual(values = c("#d7191c", "#fdae61", "#2c7bb6")) +
  scale_shape_discrete(solid = TRUE)


ggplot(data = Qual, aes(x = LengthBP, y = ContigCount)) +
  geom_point(aes(size = as.factor(PVAL)), alpha = .6) +
  facet_grid(MINLEN + MINSITES ~ FN + FP, labeller = label_both)




library(dplyr)
library(tidyr)

test <- read.csv("~/Documents/Aspen/test_qual2.csv")


test$pval <- as.factor(gsub("_.*", "", gsub(".*pval", "", Quality$Assembly.Directory) ) )
test$minlen <- as.factor(gsub("_.*", "", gsub(".*minlen", "", Quality$Assembly.Directory) ) )
test$minsites <- as.factor(gsub("_.*", "", gsub(".*minsites", "", Quality$Assembly.Directory) ) )



t2 <- test %>% 
  separate(Assembly.Directory, into = c("Input", "pval"), sep = "_")


tx2 <- gsub("pval", "", t2)
tx2 <- gsub("[A:z]","-", )
t2
gsub
#could not figure out how to separate the string column so I resorted to excel
qual <- Quality %>% separate("Assembly.Directory", c("input, params"), sep = ("assembly_all_above_7.bnx_"))
qual
Qual <- read.csv("~/Documents/Aspen/Quality.csv")