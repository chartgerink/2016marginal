##Tables for marginal significance paper, data here: https://osf.io/28gxz/ and here: https://github.com/chartgerink/2016marginal/tree/master/data

#------------------------------------------
##Startup
#------------------------------------------
#Set working directory

#load the full cleaned dataset with added topics
dat <- read.csv("cleaned_full_marginal_dataset.csv", stringsAsFactors = FALSE)

#Restricted dataset
dat2 <- dat[dat$value > 0.05 & dat$value <= 0.1,]
dat2 <- dat.marginal[!(dat.marginal$value == 0.1 & dat.marginal$comparison == ">"),]
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
    
#Number of p-values/article replication
  
results.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  results.jpsp[year] <- length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == years[year]]) / 
    a.jpsp[year] }
  
results.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  results.dp[year] <- length(dat$result[dat$journal == "Developmental Psychology" & dat$year == years[year]]) / 
    a.dp[year] }
 

#Percentage of p-values .05 < p <= .1 replication
  
p.jpsp <- rep(NA, 3)
for (year in seq_along(years)) {
  p.jpsp[year] <- 100*(length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology" & dat2$year == years[year]]) /
                    length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == years[year]])) }
  
p.dp <- rep(NA, 3)
for (year in seq_along(years)) {
  p.dp[year] <- 100*(length(dat2$result[dat2$journal == "Developmental Psychology" & dat2$year == years[year]]) /
                    length(dat$result[dat$journal == "Developmental Psychology" & dat$year == years[year]])) }
    
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
df.rep <- data.frame("journal" = c(rep("JPSP",3), rep("Developmental Psychology",3)), "year" = rep(years,2), 
                  "rep_articles" = c(a.jpsp, a.dp), 
                  "rep_p-values.per.article" = round(c(results.jpsp, results.dp), digits = 2),
                  "rep_percent.05.p.1" = round(c(p.jpsp, p.dp), digits = 2), 
                  "rep_marginally.significant.%" = round(c(marg.jpsp, marg.dp), digits = 2))


#Load data from Pritschet et al
  dat3 <- read.csv("marginals psych science revision_corrections.csv", stringsAsFactors = FALSE)
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
df.pritschet <- data.frame("journal" = c(rep("JPSP",3), rep("Developmental Psychology",3)), "year" = rep(years, 2), 
                  "Pritschet_articles" = round(c(pritschet.articles.jpsp, pritschet.articles.dp), digits = 2),
                  "Pritschet_marg.sig.percent" = round(c(pritschet.marginal.jpsp, pritschet.marginal.dp), digits = 2))


##Merge replication and Pritchet dataframes
df.table.rep <- merge(df.pritschet, df.rep)

#Write table
#write.table(df.table.rep, file = "replication_table.txt")

View(df.table.rep)
#-------------------------------------------------------
##Table for subfields and overall
#-------------------------------------------------------

#p-values per article
p.per.article.overall <- length(dat$result) / length(unique(dat$doi))
p.per.article.clinical <- length(dat$result[dat$Clinical.Psychology == 1]) / length(unique(dat$doi[dat$Clinical.Psychology == 1]))
p.per.article.cognitive <- length(dat$result[dat$Neuroscience...Cognition == 1]) / length(unique(dat$doi[dat$Neuroscience...Cognition == 1]))
p.per.article.core <- length(dat$result[dat$Core.of.Psychology == 1]) / length(unique(dat$doi[dat$Core.of.Psychology == 1]))
p.per.article.developmental <- length(dat$result[dat$Developmental.Psychology== 1]) / length(unique(dat$doi[dat$Developmental.Psychology == 1]))
p.per.article.educational <- length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]) / length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1]))
p.per.article.experimental <- length(dat$result[dat$Basic...Experimental.Psychology == 1]) / length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1]))
p.per.article.forensic <- length(dat$result[dat$Forensic.Psychology == 1]) / length(unique(dat$doi[dat$Forensic.Psychology == 1]))
p.per.article.health <- length(dat$result[dat$Health.Psychology...Medicine == 1]) / length(unique(dat$doi[dat$Health.Psychology...Medicine == 1]))
p.per.article.organizational <- length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]) / length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1]))
p.per.article.social <- length(dat$result[dat$Social.Psychology...Social.Processes == 1]) / length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1]))

# .05 < p <= .1 (%)
p.limited.overall <- 100*(length(dat2$result) / length(dat$result))
p.limited.clinical <- 100*(length(dat2$result[dat2$Clinical.Psychology == 1]) / length(dat$result[dat$Clinical.Psychology == 1]))
p.limited.cognitive <- 100*(length(dat2$result[dat2$Neuroscience...Cognition == 1]) / length(dat$result[dat$Neuroscience...Cognition == 1]))
p.limited.core <- 100*(length(dat2$result[dat2$Core.of.Psychology == 1]) / length(dat$result[dat$Core.of.Psychology == 1]))
p.limited.developmental <- 100*(length(dat2$result[dat2$Developmental.Psychology == 1]) / length(dat$result[dat$Developmental.Psychology == 1]))
p.limited.educational <- 100*(length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]) / length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]))
p.limited.experimental <- 100*(length(dat2$result[dat2$Basic...Experimental.Psychology == 1]) / length(dat$result[dat$Basic...Experimental.Psychology == 1]))
p.limited.forensic <- 100*(length(dat2$result[dat2$Forensic.Psychology == 1]) / length(dat$result[dat$Forensic.Psychology == 1]))
p.limited.health <- 100*(length(dat2$result[dat2$Health.Psychology...Medicine == 1]) / length(dat$result[dat$Health.Psychology...Medicine == 1]))
p.limited.organizational <- 100*(length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]) / length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]))
p.limited.social <- 100*(length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]) / length(dat$result[dat$Social.Psychology...Social.Processes == 1]))

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
                           "Journals" = c(length(unique(dat$journal)), 
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
                            "P-values" = c(length(dat$result), 
                                           length(dat$result[dat$Clinical.Psychology == 1]), 
                                           length(dat$result[dat$Neuroscience...Cognition == 1]),
                                           length(dat$result[dat$Core.of.Psychology == 1]),
                                           length(dat$result[dat$Developmental.Psychology == 1]), 
                                           length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]),
                                           length(dat$result[dat$Basic...Experimental.Psychology == 1]),
                                           length(dat$result[dat$Forensic.Psychology == 1]),
                                           length(dat$result[dat$Health.Psychology...Medicine == 1]),
                                           length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]),
                                           length(dat$result[dat$Social.Psychology...Social.Processes == 1])),
                            "P.per.article" = round(c(p.per.article.overall, p.per.article.clinical, p.per.article.cognitive, p.per.article.core,
                                                      p.per.article.developmental, p.per.article.educational, p.per.article.experimental,
                                                      p.per.article.forensic, p.per.article.health, p.per.article.organizational,
                                                      p.per.article.social), digits = 2),
                            "percent.0.05.p.0.1" = round(c(p.limited.overall, p.limited.clinical, p.limited.cognitive, p.limited.core, 
                                                           p.limited.developmental, p.limited.educational, p.limited.experimental,
                                                           p.limited.forensic, p.limited.health, p.limited.organizational, p.limited.social), digits = 2),
                            "percent.marginal" = round(c(marg.overall, marg.clinical, marg.cognitive, marg.core, marg.developmental, marg.educational,
                                                         marg.experimental, marg.forensic, marg.health, marg.organizational, marg.social), digits = 2))
                                                           
View(df.subfields)                                 
#------------------------------------------------------------------