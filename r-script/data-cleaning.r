####Script for cleaning dataset for FYP (2017), original data available here: https://osf.io/28gxz/

#-----------------------------------------------------
##Startup
#----------------------------------------------------
#Set working directory

#Load dataset
dat <- read.csv("marginal_dataset.csv", stringsAsFactors = FALSE, strip.white = TRUE)
#Force dat$value into a numeric variable
dat$value <- as.numeric(dat$value)
#Remove scientific notation
options(scipen = 999)
#Check structure
str(dat)
head(dat)
tail(dat)

#------------------------------------------------------
##Examination of missing values
#------------------------------------------------------

#Total number of missing values
sum(is.na(dat)) #26733

#number of missing $values
sum(is.na(dat$value)) #1073
#As a percentage of total rows
sum(is.na(dat$value))/nrow(dat) #0.001357874
#Too high values, misreported or misread in extraction
sum(dat$value > 1, na.rm = T) #309
#Note that there might be additional miscoded values that are below 1, however, we would have to recalculate p-values to find these

#number of missing comparisons
sum(is.na(dat$comparison)) #None

#Number of missing doi
sum(is.na(dat$doi)) #None. 
#Note, however, from the tail-function earlier that the missing dois have a userdefined value corresponding to "nodoi" and a number.
#number of userdefined missing doi
sum(grepl("nodoi", dat$doi)) #51

#Number of missing $results
sum(is.na(dat$result)) #None

#Number of missing $journals
sum(is.na(dat$journal)) #12830

#number of missing $years
sum(is.na(dat$year)) #12830

#Checking to see if all entries who are missing $year are the same as those missing $journal
sum(is.na(dat$year) & is.na(dat$journal)) #12830, i.e. if an entry is missing $journal it is also missing $year and vice versa

#number of missing #pre
sum(is.na(dat$pre)) #None

#number of missing $post
sum(is.na(dat$post)) #None

##Summary of missing: We have a small number of missing $values and some missing metadata (doi, journal names and years)
#Unfortunately the dataset does not contain enough information to find out the missing dois, but since there are only 
#51/790206 rows with missing dois the impact of removal should be low. 

#-------------------------------------------------------------
##Initial cleaning of data
#------------------------------------------------------------
#removal of entries with missing doi
dat <- dat[!grepl("nodoi", dat$doi),]

dat$doi
#The extracted doi format appears to be incorrect, containing a "_" where there should be a "/", e.g "10.1007_BF01045073" should be "10.1007/BF01045073".
#correction of doi format
dat$doi <- gsub("_", "/", dat$doi)

#---------------------------------------
##Retrieval of missing metadata (journal names and years)
#---------------------------------------

#Subset of dois for those entries missing journal name and year (these are the same entries)
doi.mis.journal.year <- subset(dat$doi, is.na(dat$journal))
length(doi.mis.journal.year) #12779 dois

#separate into smaller groups to decrease time for retrieval of metadata per task
doi.head.50.percent <- head(doi.mis.journal.year, n = 6389)
doi.tail.50.percent <- tail(doi.mis.journal.year, n = 6390)

#load required library
if(!require(rcrossref)){install.packages("rcrossref")}
library(rcrossref)

#retrieve metadata for all dois with missing years and journals
retrieved.metadata.head <- cr_works(dois = doi.head.50.percent)
retrieved.metadata.tail <- cr_works(dois = doi.tail.50.percent)

#separate out doi, journal name and year from metadata
retrieved.journal.year.head <- data.frame("doi"= retrieved.metadata.head$data$DOI, "year" = retrieved.metadata.head$data$issued, "journal" = retrieved.metadata.head$data$container.title)
retrieved.journal.year.tail <- data.frame("doi"= retrieved.metadata.tail$data$DOI, "year" = retrieved.metadata.tail$data$issued, "journal" = retrieved.metadata.tail$data$container.title)

#Merge the data frames into one
additional.metadata <- rbind(retrieved.journal.year.head, retrieved.journal.year.tail)

#Temporary save-file
write.csv(additional.metadata, file = "missing_metadata", row.names = F)
additional.metadata <- read.csv("missing_metadata.csv", stringsAsFactors = FALSE)

#Add the missing metadata to the original dataframe
dat$year[is.na(dat$year)] <- additional.metadata$year[match(dat$doi[is.na(dat$year)], additional.metadata$doi)]
dat$journal[is.na(dat$journal)] <- additional.metadata$journal[match(dat$doi[is.na(dat$journal)], additional.metadata$doi)]

#Check if worked                                            
sum(is.na(dat$year)) #not for 1295 rows
which(is.na(dat$year))
dat[12399,] #closer look at one example
dat[dat$doi == "10.1037/0003-066X.60.8.750",]
additional.metadata[additional.metadata$doi == "10.1037/0003-066X.60.8.750",] #does not work
additional.metadata[additional.metadata$doi == "10.1037/0003-066x.60.8.750",] #does work
#Problem appears to be that some letters ("x" here) are capitalized in original dataset, but not in the dois retrieved from crossref

#retrieve the still missing vmetadata from crossref
still.missing <- dat$doi[is.na(dat$year)]
retrieved.metadata.still.missing <- cr_works(dois = still.missing)
missing.metadata <- data.frame("doi"= retrieved.metadata.still.missing$data$DOI, "year" = retrieved.metadata.still.missing$data$issued, "journal" = retrieved.metadata.still.missing$data$container.title, stringsAsFactors = FALSE)

#Temporary save-file
write.csv(missing.metadata, file = "still_missing_metadata.csv", row.names = F)
missing.metadata <- read.csv("still_missing_metadata.csv", stringsAsFactors = FALSE)

#Complete the original dataset
dat$year[is.na(dat$year)] <- missing.metadata$year[match(tolower(dat$doi[is.na(dat$year)]), missing.metadata$doi)]
dat$journal[is.na(dat$journal)] <- missing.metadata$journal[match(tolower(dat$doi[is.na(dat$journal)]), missing.metadata$doi)]

#Check if solved
which(is.na(dat$year)) #none missing

#------------------------------------------
##Removal of final NAs
#-----------------------------------------
sum(is.na(dat)) #1073 remaining NA

#These are $values
sum(is.na(dat$value)) #1073

#Removal
dat <- na.omit(dat)

#check
sum(is.na(dat)) # 0

#-------------------------------------------
##Add information on topics for each journal
#----------------------------------
#check so that all journal names are written correctly
unique(dat$journal) 

#a number of journal names need to be updated for consistency
dat$journal <- gsub("Canadian Journal Of Behavioural Science", "Canadian Journal of Behavioural Science", dat$journal)
dat$journal <- gsub("Canadian Journal of Behavioural Science/Revue canadienne des Sciences du comportement", "Canadian Journal of Behavioural Science", dat$journal)
dat$journal <- gsub("Canadian Journal of Behavioural Science / Revue canadienne des sciences du comportement", "Canadian Journal of Behavioural Science", dat$journal)
dat$journal <- gsub("Canadian Journal of Behavioural Science/Revue canadienne des sciences du comportement", "Canadian Journal of Behavioural Science", dat$journal)

dat$journal <- gsub("Canadian Journal of Experimental Psychology/Revue canadienne de psychologie expérimentale", "Canadian Journal of Experimental Psychology", dat$journal)
dat$journal <- gsub("Canadian Journal of Psychology Revue Canadienne de Psychologie", "Canadian Journal of Experimental Psychology", dat$journal)
dat$journal <- gsub("Canadian Journal of Psychology/Revue canadienne de psychologie", "Canadian Journal of Experimental Psychology", dat$journal)
dat$journal <- gsub("Canadian Journal of Experimental Psychology/Revue canadienne de psychologie expÃ©rimentale", "Canadian Journal of Experimental Psychology", dat$journal)

dat$journal <- gsub("Canadian Psychology Psychologie Canadienne", "Canadian Psychology", dat$journal)
dat$journal <- gsub("Canadian Psychology/Psychologie canadienne", "Canadian Psychology", dat$journal)
dat$journal <- gsub("Canadian Psychology/Psychologie Canadienne", "Canadian Psychology", dat$journal)

dat$journal <- gsub("Cultural Diversity & Mental Health", "Cultural Diversity & Ethnic Minority Psychology", dat$journal)
dat$journal <- gsub("Cultural Diversity and Ethnic Minority Psychology", "Cultural Diversity & Ethnic Minority Psychology", dat$journal)
dat$journal <- gsub("Cultural Diversity and Mental Health", "Cultural Diversity & Ethnic Minority Psychology", dat$journal)

dat$journal <- gsub("Journal of Experimental Psychology: Human Perception & Performance", "Journal of Experimental Psychology: Human Perception and Performance", dat$journal)

dat$journal <- gsub("Journal of Social, Evolutionary, and Cultural Psychology", "Evolutionary Behavioral Sciences", dat$journal)

dat$journal <- gsub("Professional School Psychology", "School Psychology Quarterly", dat$journal)

dat$journal <- gsub("Psychological Assessment: A Journal of Consulting and Clinical Psychology", "Psychological Assessment", dat$journal)

dat$journal <- gsub("Psychomusicology: A Journal of Research in Music Cognition", "Psychomusicology: Music, Mind, and Brain", dat$journal)
dat$journal <- gsub("Psychomusicology: Music, Mind and Brain", "Psychomusicology: Music, Mind, and Brain", dat$journal)

dat$journal <- gsub("Psychosocial Rehabilitation Journal", "Psychiatric Rehabilitation Journal", dat$journal)

dat$journal <- gsub("Psychotherapy: Theory, Research, Practice, Training", "Psychotherapy", dat$journal)

dat$journal <- gsub("Theoretical & Philosophical Psychology", "Journal of Theoretical and Philosophical Psychology", dat$journal)

#load file with journal names and APA-topics
topics <- read.csv2("apa_topics_dummies.csv", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)


#Merge with main dataframe
dat <- merge(dat, topics, by = "journal")

write.csv(dat, file = "cleaned_full_marginal_dataset.csv", row.names = F)


#-------------------------------------------------------
##Create dataset with only values between .05 < p <= .1
#------------------------------------------------------
dat.marginal <- dat[dat$value > 0.05 & dat$value <= 0.1,]

write.csv(dat.marginal, file = "cleaned_restrictedp_marginal_dataset.csv", row.names = F)

#----------------------------------------------------
#Create a stratified sample (by journal) for testing and pre-registration
#------------------------------------------------------
#Load required package
if(!require("splitstackshape")){install.packages("splitstackshape")}
library(splitstackshape)

#load data file
dat.marginal <- read.csv("cleaned_restrictedp_marginal_dataset.csv", stringsAsFactors = F)

sort(table(dat.marginal$journal))
#5 journals have 4 or fewer rows in the dataframe, and we would have to sample a prohibitively large proportion
#of entries (>25%) for these to show up in the stratified sample. These are:
which(table(dat.marginal$journal) < 5) 
#All other journal consist >=10 rows, i.e. it is sufficient to sample anything >5% for these to show up in the sample.

#Take a stratified sample, consisting of approximately 6% of entries from each journal
set.seed(1)
stratified.sample <- stratified(dat.marginal, "journal", 0.06)


write.csv(stratified.sample, file = "test_sample_marginal_dataset.csv", row.names = F)

#-------------------------------------------------------

