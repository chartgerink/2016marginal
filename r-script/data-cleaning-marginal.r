####Script for cleaning dataset for FYP (2017), original data available here: https://osf.io/28gxz/

#-----------------------------------------------------
##Startup
#----------------------------------------------------
#Set working directory

#Load dataset
dat <- read.csv("marginal_dataset.csv", stringsAsFactors = FALSE)
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

#--------------------------------------------------------
##Initial cleaning of data
#--------------------------------------------------------
#removal of entries with missing doi
dat <- dat[!grepl("nodoi", dat$doi),]

#removal of entries with missing $values
dat <- dat[!is.na(dat$value),]

dat$doi
#The extracted doi format appears to be incorrect, containing a "_" where there should be a "/", e.g "10.1007_BF01045073" should be "10.1007/BF01045073".
#correction of doi format
dat$doi <- gsub("_", "/", dat$doi)

#Limit dataset to only values between .05 < p <= .1
dat <- dat[dat$value > 0.05 & dat$value <= 0.1,]

#--------------------------------------------------------
##Retrieval of missing metadata (journal names and years)
#--------------------------------------------------------

#Subset of dois for those entries missing journal name and year (these are the same entries)
doi.missing.metadata <- subset(dat$doi, is.na(dat$journal))
length(doi.missing.metadata) #844 dois

#load required library
if(!require(rcrossref)){install.packages("rcrossref")}
library(rcrossref)

#retrieve metadata for all dois with missing years and journals, NB! can take some time
retrieved.metadata <- cr_works(dois = doi.missing.metadata)

#separate out doi, journal name and year from metadata
missing.metadata <- data.frame("doi"= retrieved.metadata$data$DOI, "year" = retrieved.metadata$data$issued, "journal" = retrieved.metadata$data$container.title)

#Add the missing metadata to the dataset
dat$year[is.na(dat$year)] <- missing.metadata$year[match(dat$doi[is.na(dat$year)], missing.metadata$doi)]
dat$journal[is.na(dat$journal)] <- missing.metadata$journal[match(dat$doi[is.na(dat$journal)], missing.metadata$doi)]

#Check if worked                                            
sum(is.na(dat$year)) #not for 77 rows
which(is.na(dat$year))
dat[13848,] #closer look at one example ("10.1037/0033-295X.101.1.3")
missing.metadata[missing.metadata$doi == "10.1037/0033-295X.101.1.3",] #does not work
missing.metadata[missing.metadata$doi == "10.1037/0033-295x.101.1.3",] #does work
#Problem appears to be that some letters ("x" here) are capitalized in original dataset, but not in the dois retrieved from crossref

#Select out the non-matching metadata
not.matching <- subset(missing.metadata, !(missing.metadata$doi %in% dat$doi))

#Merge the dataset with the still missing metadata with the help of the function 'tolower'
dat$year[is.na(dat$year)] <- not.matching$year[match(tolower(dat$doi[is.na(dat$year)]), not.matching$doi)]
dat$journal[is.na(dat$journal)] <- not.matching$journal[match(tolower(dat$doi[is.na(dat$journal)]), not.matching$doi)]

#Check if any missing values remaining
sum(is.na(dat)) #none missing

#--------------------------------------------
##Add information on topics for each journal
#--------------------------------------------
#check so that all journal names are written correctly
unique(dat$journal) 

#a number of journal names need to be updated for consistency
dat$journal <- gsub("Canadian Journal of Behavioural Science/Revue canadienne des Sciences du comportement", "Canadian Journal of Behavioural Science", dat$journal)
dat$journal <- gsub("Canadian Journal of Behavioural Science / Revue canadienne des sciences du comportement", "Canadian Journal of Behavioural Science", dat$journal)
dat$journal <- gsub("Canadian Journal of Behavioural Science/Revue canadienne des sciences du comportement", "Canadian Journal of Behavioural Science", dat$journal)

dat$journal <- gsub("Canadian Journal of Experimental Psychology/Revue canadienne de psychologie exp�rimentale", "Canadian Journal of Experimental Psychology", dat$journal)
dat$journal <- gsub("Canadian Journal of Psychology Revue Canadienne de Psychologie", "Canadian Journal of Experimental Psychology", dat$journal)
dat$journal <- gsub("Canadian Journal of Psychology/Revue canadienne de psychologie", "Canadian Journal of Experimental Psychology", dat$journal)
dat$journal <- gsub("Canadian Journal of Experimental Psychology/Revue canadienne de psychologie expérimentale", "Canadian Journal of Experimental Psychology", dat$journal)

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

#---------------------------------------------
##Save finished dataset
#---------------------------------------------
write.csv(dat, file = "cleaned_restrictedp_marginal_dataset.csv", row.names = F)

#---------------------------------------------