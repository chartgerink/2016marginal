####Script for creation of testing sample for FYP (2017), data available here: 

#------------------------------------------------------
#Creation of stratified sample (by journal)
#------------------------------------------------------
#Load required package
if(!require("splitstackshape")){install.packages("splitstackshape")}
library(splitstackshape)

#load data file
dat.marginal <- read.csv("../data/cleaned_restrictedp_marginal_dataset.csv", stringsAsFactors = F)

sort(table(dat.marginal$journal))
#5 journals have 4 or fewer rows in the dataframe, and we would have to sample a prohibitively large proportion
#of entries (>25%) for these to show up in the stratified sample. These are:
which(table(dat.marginal$journal) < 5) 
#All other journal consist >=9 rows, i.e. it is sufficient to sample ~6% for these to show up in the sample.

#Take a stratified sample, consisting of approximately 6% of entries from each journal
set.seed(1)
stratified.sample <- stratified(dat.marginal, "journal", 0.06)

#---------------------------------------------
##Save finished dataset
#---------------------------------------------
write.csv(stratified.sample, file = "../data/test_sample_marginal_dataset.csv", row.names = F)

#-------------------------------------------------------
