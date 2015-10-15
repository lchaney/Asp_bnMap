Quality <- read.delim("~/Documents/Aspen/Quality.txt")

convert <- function(x){
  
  
}

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


ggplot(data = Quality, aes(x = Length.in.Basepairs, y = Contig.Count)) +
  geom_point(aes(color = minlen, shape = pval,
                 size = minsites), alpha = .6) +
  facet_grid(fp ~ fn, labeller = label_both) +
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