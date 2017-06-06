##Tables for marginal significance paper, data here: https://osf.io/28gxz/ and here: https://github.com/chartgerink/2016marginal/tree/master/data

#------------------------------------------
##Startup
#------------------------------------------
#load the full cleaned dataset with added topics
dat <- read.csv("../data/cleaned_full_marginal_dataset.csv", stringsAsFactors = FALSE)

#Restricted dataset
dat2 <- dat[dat$value > 0.05 & dat$value <= 0.1,]
dat2 <- dat2[!(dat2$value == 0.1 & dat2$comparison == ">"),]
#Add a variable indicating whether a p-value appears to reported as marginally significant to restricted dataset
dat2$marginal <- grepl("marginal|approach", dat2$pre) | grepl("marginal|approach", dat2$post)

#------------------------------------------
##Replication/Pritschet et al table - JPSP and DP
#------------------------------------------

years <- c(1990, 2000, 2010)

#Number of articles replication
  
a.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  a.jpsp[year] <- length(unique(dat$doi[dat$journal == "Journal of Personality and Social Psychology" & dat$year == years[year]])) }

a.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  a.dp[year] <- length(unique(dat$doi[dat$journal == "Developmental Psychology" & dat$year == years[year]])) }

#Number of p-values replication

p.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  p.jpsp[year] <- length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == years[year]]) }
  
p.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  p.dp[year] <- length(dat$result[dat$journal == "Developmental Psychology" & dat$year == years[year]]) }
     
#Number of p-values/article replication
  
results.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  results.jpsp[year] <- round(length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == years[year]]) / 
    a.jpsp[year], digits = 2) }
  
results.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  results.dp[year] <- round(length(dat$result[dat$journal == "Developmental Psychology" & dat$year == years[year]]) / 
    a.dp[year], digits = 2) }
 

#number of p-values .05 < p <= .1 replication
  
p.limited.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  p.limited.jpsp[year] <- length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology" & dat2$year == years[year]]) }
  
p.limited.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  p.limited.dp[year] <- length(dat2$result[dat2$journal == "Developmental Psychology" & dat2$year == years[year]])  }
    

#Number of .05 < p <= .1 per article replication
limited.a.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  limited.a.jpsp[year] <- round(length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology" & dat2$year == years[year]]) / 
    a.jpsp[year], digits = 2) }
  
limited.a.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  limited.a.dp[year] <- round(length(dat2$result[dat2$journal == "Developmental Psychology" & dat2$year == years[year]])  / 
    a.dp[year], digits = 2) }
 
#Percentage of .05 < p <= .1 marginally significant replication

marg.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  marg.jpsp[year] <- 100*(sum(dat2$marginal[dat2$journal == "Journal of Personality and Social Psychology" & dat2$year == years[year]]) /
                      length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology" & dat2$year == years[year]])) }
  
marg.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  marg.dp[year] <- 100*(sum(dat2$marginal[dat2$journal == "Developmental Psychology" & dat2$year == years[year]]) /
                      length(dat2$result[dat2$journal == "Developmental Psychology" & dat2$year == years[year]])) }
   
#Data frame of replication data
df.rep <- data.frame("Field" = c(rep("JPSP",3), rep("DP",3)), "Journals.year" = rep(years,2), 
                  "Articles" = c(a.jpsp, a.dp), 
                  "P-values.and.per.article" = c(paste0(p.jpsp, " (", results.jpsp, ")"), paste0(p.dp, " (", results.dp, ")")),
                  "0.05.p.0.1.and.per.article" = c(paste0(p.limited.jpsp, " (", limited.a.jpsp, ")"), paste0(p.limited.dp, " (", limited.a.dp, ")")), 
                  "percent.marginal" = round(c(marg.jpsp, marg.dp), digits = 2))



#Load data from Pritschet et al
  dat3 <- read.csv("../data/marginals_psych_science_revision_corrections.csv", stringsAsFactors = FALSE)
  #Resaved the file as .csv, will have to look up how to open a .xlsx another day
  dat3 <- dat3[dat3$Year == 1990 | dat3$Year == 2000 | dat3$Year == 2010, ]
  dat3 <- dat3[dat3$Field == 2 | dat3$Field == 3,]
  #Field 2 is Developmental psychology, and field 3 JPSP
  dat3 <- data.frame(dat3$Field, dat3$Year, dat3$Marginals.Yes.No)

#Number of articles per year Pritschet et al
pritschet.articles.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  pritschet.articles.jpsp[year] <- nrow(dat3[dat3$dat3.Field == 3 & dat3$dat3.Year == years[year],]) }

pritschet.articles.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  pritschet.articles.dp[year] <- nrow(dat3[dat3$dat3.Field == 2 & dat3$dat3.Year == years[year],]) }

#percentage of articles containing at least one marginally significant result Pritschet et al
pritschet.marginal.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  pritschet.marginal.jpsp[year] <- 100*(sum(dat3$dat3.Marginals.Yes.No[dat3$dat3.Field == 3 & dat3$dat3.Year == years[year]]) / 
                                      pritschet.articles.jpsp[year]) }

pritschet.marginal.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  pritschet.marginal.dp[year] <- 100*(sum(dat3$dat3.Marginals.Yes.No[dat3$dat3.Field == 2 & dat3$dat3.Year == years[year]]) / 
                                      pritschet.articles.dp[year]) }
    
#dataframe pritschet et al
df.pritschet <- data.frame("Field" = c(rep("JPSP",3), rep("DP",3)), "Journals.year" = rep(years, 2), 
                  "Articles" = round(c(pritschet.articles.jpsp, pritschet.articles.dp), digits = 2),
                  "P-values.and.per.article" = rep(NA,3),
                  "0.05.p.0.1.and.per.article" = rep(NA,3),
                  "percent.marginal" = round(c(pritschet.marginal.jpsp, pritschet.marginal.dp), digits = 2))


##Merge replication and Pritchet dataframes

df.table <- rbind(df.pritschet, df.rep)

View(df.table)
#-------------------------------------------------------
##Table for subfields and overall
#-------------------------------------------------------

#p-values per article
p.per.article.overall <- round(length(dat$result) / length(unique(dat$doi)), digits = 2)
p.per.article.clinical <- round(length(dat$result[dat$Clinical.Psychology == 1]) / length(unique(dat$doi[dat$Clinical.Psychology == 1])), digits = 2)
p.per.article.cognitive <- round(length(dat$result[dat$Neuroscience...Cognition == 1]) / length(unique(dat$doi[dat$Neuroscience...Cognition == 1])), digits = 2)
p.per.article.core <- round(length(dat$result[dat$Core.of.Psychology == 1]) / length(unique(dat$doi[dat$Core.of.Psychology == 1])), digits = 2)
p.per.article.developmental <- round(length(dat$result[dat$Developmental.Psychology== 1]) / length(unique(dat$doi[dat$Developmental.Psychology == 1])), digits = 2)
p.per.article.educational <- round(length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]) / length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1])), digits = 2)
p.per.article.experimental <- round(length(dat$result[dat$Basic...Experimental.Psychology == 1]) / length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1])), digits = 2)
p.per.article.forensic <- round(length(dat$result[dat$Forensic.Psychology == 1]) / length(unique(dat$doi[dat$Forensic.Psychology == 1])), digits = 2)
p.per.article.health <- round(length(dat$result[dat$Health.Psychology...Medicine == 1]) / length(unique(dat$doi[dat$Health.Psychology...Medicine == 1])), digits = 2)
p.per.article.organizational <- round(length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]) / length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1])), digits = 2)
p.per.article.social <- round(length(dat$result[dat$Social.Psychology...Social.Processes == 1]) / length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1])), digits = 2)

# .05 < p <= .1 per article
p.limited.overall <- round(length(dat2$result) / length(unique(dat2$doi)), digits = 2)
p.limited.clinical <- round(length(dat2$result[dat2$Clinical.Psychology == 1]) / length(unique(dat2$doi[dat2$Clinical.Psychology == 1])), digits = 2)
p.limited.cognitive <- round(length(dat2$result[dat2$Neuroscience...Cognition == 1]) / length(unique(dat2$doi[dat2$Neuroscience...Cognition == 1])), digits = 2)
p.limited.core <- round(length(dat2$result[dat2$Core.of.Psychology == 1]) / length(unique(dat2$doi[dat2$Core.of.Psychology == 1])), digits = 2)
p.limited.developmental <- round(length(dat2$result[dat2$Developmental.Psychology == 1]) / length(unique(dat2$doi[dat2$Developmental.Psychology == 1])), digits = 2)
p.limited.educational <- round(length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]) / length(unique(dat2$doi[dat2$Educational.Psychology..School.Psychology...Training == 1])), digits = 2)
p.limited.experimental <- round(length(dat2$result[dat2$Basic...Experimental.Psychology == 1]) / length(unique(dat2$doi[dat2$Basic...Experimental.Psychology == 1])), digits = 2)
p.limited.forensic <- round(length(dat2$result[dat2$Forensic.Psychology == 1]) / length(unique(dat2$doi[dat2$Forensic.Psychology == 1])), digits = 2)
p.limited.health <- round(length(dat2$result[dat2$Health.Psychology...Medicine == 1]) / length(unique(dat2$doi[dat2$Health.Psychology...Medicine == 1])), digits = 2)
p.limited.organizational <- round(length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]) / length(unique(dat2$doi[dat2$Industrial.Organizational.Psychology...Management == 1])), digits = 2)
p.limited.social <- round(length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]) / length(unique(dat2$doi[dat2$Social.Psychology...Social.Processes == 1])), digits = 2)


#marginally significant (%)
marg.overall <- 100*(sum(dat2$marginal) / length(dat2$result))

marg.clinical <- 100*(sum(dat2$marginal[dat2$Clinical.Psychology == 1]) / length(dat2$result[dat2$Clinical.Psychology == 1]))

marg.cognitive <- 100*(sum(dat2$marginal[dat2$Neuroscience...Cognition == 1]) / length(dat2$result[dat2$Neuroscience...Cognition == 1]))

marg.core <- 100*(sum(dat2$marginal[dat2$Core.of.Psychology == 1]) / length(dat2$result[dat2$Core.of.Psychology == 1]))

marg.developmental <- 100*(sum(dat2$marginal[dat2$Developmental.Psychology == 1]) / length(dat2$result[dat2$Developmental.Psychology == 1]))

marg.educational <- 100*(sum(dat2$marginal[dat2$Educational.Psychology..School.Psychology...Training == 1]) / length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]))

marg.experimental <- 100*(sum(dat2$marginal[dat2$Basic...Experimental.Psychology == 1]) / length(dat2$result[dat2$Basic...Experimental.Psychology == 1]))

marg.forensic <- 100*(sum(dat2$marginal[dat2$Forensic.Psychology == 1]) / length(dat2$result[dat2$Forensic.Psychology == 1]))
            
marg.health <- 100*(sum(dat2$marginal[dat2$Health.Psychology...Medicine == 1]) / length(dat2$result[dat2$Health.Psychology...Medicine == 1]))
                 
marg.organizational <- 100*(sum(dat2$marginal[dat2$Industrial.Organizational.Psychology...Management == 1]) / length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]))
 
marg.social <- 100*(sum(dat2$marginal[dat2$Social.Psychology...Social.Processes == 1]) / length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]))


#Dataframe for table
df.subfields <- data.frame("Field" = c("All APA journals", "Clinical", "Cognitive", "Core", "Developmental", "Educational",
                                          "Experimental", "Forensic", "Health", "Organizational", "Social"), 
                           "Journals.year" = c(length(unique(dat$journal)), 
                                          length(unique(dat$journal[dat$Clinical.Psychology == 1])), 
                                          length(unique(dat$journal[dat$Neuroscience...Cognition == 1])), 
                                          length(unique(dat$journal[dat$Core.of.Psychology == 1])), 
                                          length(unique(dat$journal[dat$Developmental.Psychology == 1])), 
                                          length(unique(dat$journal[dat$Educational.Psychology..School.Psychology...Training == 1])),
                                          length(unique(dat$journal[dat$Basic...Experimental.Psychology == 1])), 
                                          length(unique(dat$journal[dat$Forensic.Psychology == 1])),
                                          length(unique(dat$journal[dat$Health.Psychology...Medicine == 1])), 
                                          length(unique(dat$journal[dat$Industrial.Organizational.Psychology...Management == 1])),
                                          length(unique(dat$journal[dat$Social.Psychology...Social.Processes == 1]))),
                            "Articles" = c(length(unique(dat$doi)), 
                                          length(unique(dat$doi[dat$Clinical.Psychology == 1])), 
                                          length(unique(dat$doi[dat$Neuroscience...Cognition == 1])), 
                                          length(unique(dat$doi[dat$Core.of.Psychology == 1])),
                                          length(unique(dat$doi[dat$Developmental.Psychology == 1])),
                                          length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1])),
                                          length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1])),
                                          length(unique(dat$doi[dat$Forensic.Psychology == 1])),
                                          length(unique(dat$doi[dat$Health.Psychology...Medicine == 1])),
                                          length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1])),
                                          length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1]))),
                            "P-values.and.per.article" = c(paste0(length(dat$result), " (",p.per.article.overall, ")"), 
                                           paste0(length(dat$result[dat$Clinical.Psychology == 1]), " (",p.per.article.clinical, ")"), 
                                           paste0(length(dat$result[dat$Neuroscience...Cognition == 1]), " (",p.per.article.cognitive, ")"),
                                           paste0(length(dat$result[dat$Core.of.Psychology == 1]), " (",p.per.article.core, ")"),
                                           paste0(length(dat$result[dat$Developmental.Psychology == 1]), " (",p.per.article.developmental, ")"), 
                                           paste0(length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]), " (",p.per.article.educational, ")"),
                                           paste0(length(dat$result[dat$Basic...Experimental.Psychology == 1]), " (",p.per.article.experimental, ")"),
                                           paste0(length(dat$result[dat$Forensic.Psychology == 1]), " (",p.per.article.forensic, ")"),
                                           paste0(length(dat$result[dat$Health.Psychology...Medicine == 1]), " (",p.per.article.health, ")"),
                                           paste0(length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]), " (",p.per.article.organizational, ")"),
                                           paste0(length(dat$result[dat$Social.Psychology...Social.Processes == 1]), " (",p.per.article.social, ")")),
                            "0.05.p.0.1.and.per.article" = c(paste0(length(dat2$result), " (", p.limited.overall, ")"),
                                                     paste0(length(dat2$result[dat2$Clinical.Psychology == 1]), " (", p.limited.clinical, ")"),
                                                     paste0(length(dat2$result[dat2$Neuroscience...Cognition == 1]), " (", p.limited.cognitive, ")"),
                                                     paste0(length(dat2$result[dat2$Core.of.Psychology == 1]), " (", p.limited.core, ")"),
                                                     paste0(length(dat2$result[dat2$Developmental.Psychology == 1]), " (", p.limited.developmental, ")"),
                                                     paste0(length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]), " (", p.limited.educational, ")"),
                                                     paste0(length(dat2$result[dat2$Basic...Experimental.Psychology == 1]), " (", p.limited.experimental, ")"),
                                                     paste0(length(dat2$result[dat2$Forensic.Psychology == 1]), " (", p.limited.forensic, ")"), 
                                                     paste0(length(dat2$result[dat2$Health.Psychology...Medicine == 1]), " (", p.limited.health, ")"),
                                                     paste0(length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]), " (", p.limited.organizational, ")"),
                                                     paste0(length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]), " (", p.limited.social, ")")),
                           "percent.marginal" = round(c(marg.overall, marg.clinical, marg.cognitive, marg.core, marg.developmental, marg.educational,
                                                         marg.experimental, marg.forensic, marg.health, marg.organizational, marg.social), digits = 2))


                                                          

#Merge the two tables
final.table <- rbind(df.subfields, df.table)
#------------------------------------------
##List of objects for r-markdown file
#------------------------------------------
#Load original dataset
original <- read.csv("../data/marginal_dataset.csv", stringsAsFactors = FALSE, strip.white = TRUE)
#Force dat$value into a numeric variable
original$value <- as.numeric(original$value)

#"original" = original full dataset without trimming, "dat" = full dataset with nodoi and missing p-values removed + journal names corrected + metadata and topics added, 
#"dat2" = "dat" but only entries with .05 < p <= .1, "dat3" = data from Pritschet et al

marginal_list <- list(entries.original = nrow(original), 
                      nodoi = sum(grepl("nodoi", original$doi)), 
                      nodoipercent = signif(100*(sum(grepl("nodoi", original$doi))/nrow(original)), digits = 2),
                      badp = sum(is.na(original$value)), 
                      badppercent = round(100*(sum(is.na(original$value))/nrow(original)), digits = 2),
                      mis.meta = sum(is.na(original[!grepl("nodoi", original$doi) & !is.na(original$value),]$journal)),
                      mis.metapercent = round(100*(sum(is.na(original[!grepl("nodoi", original$doi) & !is.na(original$value),]$journal)) / nrow(original)), digits = 2),
                      entries.final = length(dat2$result), 
                      entries.finalpercent = round(100*(length(dat2$result) / nrow(original)), digits = 2), 
                      articles.final = length(unique(dat2$doi)), 
                      articles.finalpercent = round(100*(length(unique(dat2$doi))/length(unique(dat$doi))), digits = 2), 
                      journals.final = length(unique(dat2$journal)), 
                      difjournals = setdiff(unique(dat$journal), unique(dat2$journal)), 
                      table = final.table)

saveRDS(marginal_list, file = "../writing/marginal_rmarkdown_objects.RData")

