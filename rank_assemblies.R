#==============================================================================================#
# Script created by Aaron Sharp 2015
# This script can be used to determine what are the best paramaters to use
# It ranks each assembly by length and number of contigs
# The 'best' assembly 
#==============================================================================================#


simp_stat <- read.table('~/Documents/Aspen/Quality.txt', 
                        header=T, row.names=1, sep="\t")

lengths <- simp_stat[,"Length.in.Basepairs"]
length_percentile <- ecdf(lengths)
length_ranks <- length_percentile(simp_stat[,"Length.in.Basepairs"])

counts <- simp_stat[,"Contig.Count"]
count_percentile <- ecdf(counts)
count_ranks <- count_percentile(simp_stat[,"Contig.Count"])

ranks=data.frame(rownames=row.names(simp_stat))
ranks$length_percentile=length_ranks
ranks$count_percentile=count_ranks
ranks$score=ranks$length_percentile-ranks$count_percentile
head(ranks[order(-ranks$score),], n=1)
