mol_stats <- read.delim("~/Documents/Aspen/mol_stats.txt") #load data into R
mol_stats$labden <- (mol_stats$labels/mol_stats$length)*100000 #create new variable for label density
boxstat <- boxplot.stats(mol_stats$labden)$stats #calculate label density boxplot stats, coenf = 1.5
out_mol <- mol_stats[mol_stats$labden <= boxstat[1] | mol_stats$labden >= boxstat[5], ] #saves data with label density above or below outlier threshold
outmol_id <- out_mol$mol_id #saves vector of label ids for outliers
length(outmol_id) #how many outliers

keep_mol <- mol_stats[mol_stats$labden > boxstat[1] & mol_stats$labden < boxstat[5], ] #saves data with label density within outlier threshold
keepmol_id <- keep_mol$mol_id #saves vector of label ids for non-outliers
length(keepmol_id) #how many non-outliers or keepers

write.table(keepmol_id, "keep_mol.txt", col.names = FALSE, row.names = FALSE) #export keepmol_id
