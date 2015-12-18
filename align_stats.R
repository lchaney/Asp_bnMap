#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# modified from Aaron Sharp
# Script created in version R 3.2.1 
# This script is used to describe unique molecule counts
# It calculates the regression of the length vs molecule counts and plots
# It can aid in filtering out poor molecules
#==============================================================================================#

#/fslhome/sharpa/apps/POMM/Operations/BioNano/CheckMolecules/calculate_equation.R
#change source location
data <- read.table('~/Documents/Aspen/align_stats.txt', 
                   header = T, sep = "\t", quote = "\"'", dec = ".", 
                   row.names = 1, stringsAsFactors = F)

summary(data)
equation <- lm(counts ~ length, data)
summary(equation)
coefficients(equation)

library(ggplot2)
ggplot(data = data, #input data
       aes(x = length, y = counts)) + #specify x and y
  geom_point(alpha = .5, #half transparent; points 
             color = "darkblue") + #point color is darkblue
  geom_smooth(method = lm, #add regression line
              se = FALSE, #no standard error
              color = "black") + #black
  theme_bw() + #change the aesthetics of graph with white background
  labs(x = "Molecule Length (MB)", #x axis label
       y = "Number of alignments") + #y axis label
  scale_x_continuous(labels = function(x) x/1000000, #convert bp to mb for x
                     expand = c(0,0)) + #set so axis starts at 0
  scale_y_continuous(expand = c(0, 0)) + #set so axis starts at 0
  expand_limits(x = 0, y = 0) #set so axis starts at 0
