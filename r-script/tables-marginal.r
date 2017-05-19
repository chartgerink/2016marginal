##Tables for marginal significance paper, data here: https://osf.io/28gxz/ and here: https://github.com/chartgerink/2016marginal/tree/master/data

#------------------------------------------
##Startup
#------------------------------------------
#Set working directory

#load the full cleaned dataset with added topics
dat <- read.csv("cleaned_full_marginal_dataset.csv", stringsAsFactors = FALSE)

#------------------------------------------
##Replication table
#------------------------------------------

#JPSP data from replication
  #Number of articles for 1990, 2000, and 2010
  a.jpsp.1990 <- length(unique(dat$doi[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 1990]))
  a.jpsp.2000 <- length(unique(dat$doi[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2000]))
  a.jpsp.2010 <- length(unique(dat$doi[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2010]))

  #Number of p-values/article 1990, 2000 and 2010
  results.jpsp.1990 <- length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 1990]) / a.jpsp.1990
  results.jpsp.2000 <- length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2000]) / a.jpsp.2000
  results.jpsp.2010 <- length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2010]) / a.jpsp.2010

  #Percentage of p-values .05 < p <= .1 1990, 2000 and 2010
  p.jpsp.1990 <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 1990]) /
                      length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 1990]))
                
  p.jpsp.2000 <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2000]) /
                      length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2000]))

  p.jpsp.2010 <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2010]) /
                      length(dat$result[dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2010]))

  #Percentage of p-values marginally significant 1990, 2000 and 2010
  marg.jpsp.1990 <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 1990]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 1990])) /
                        length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 1990]))

  marg.jpsp.2000 <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2000]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2000])) /
                        length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2000]))

  marg.jpsp.2010<- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2010]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2010])) /
                        length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Journal of Personality and Social Psychology" & dat$year == 2010]))

  #Create data frame
  df.r1 <- data.frame("journal" = rep("JPSP",3), "year" = c(1990,2000,2010), 
                    "rep_articles" = c(a.jpsp.1990, a.jpsp.2000, a.jpsp.2010), 
                    "rep_p-values.per.article" = round(c(results.jpsp.1990, results.jpsp.2000, results.jpsp.2010), digits = 2),
                    "rep_percent.05.p.1" = round(c(p.jpsp.1990, p.jpsp.2000, p.jpsp.2010), digits = 2), 
                    "rep_marginally.significant.%" = round(c(marg.jpsp.1990, marg.jpsp.2000, marg.jpsp.2010), digits = 2))

#Developmental Psychology data from replication
  #Number of articles for 1990, 2000, and 2010
  a.dp.1990 <- length(unique(dat$doi[dat$journal == "Developmental Psychology" & dat$year == 1990]))
  a.dp.2000 <- length(unique(dat$doi[dat$journal == "Developmental Psychology" & dat$year == 2000]))
  a.dp.2010 <- length(unique(dat$doi[dat$journal == "Developmental Psychology" & dat$year == 2010]))

  #Number of p-values/article 1990, 2000 and 2010
  results.dp.1990 <- length(dat$result[dat$journal == "Developmental Psychology" & dat$year == 1990]) / a.dp.1990
  results.dp.2000 <- length(dat$result[dat$journal == "Developmental Psychology" & dat$year == 2000]) / a.dp.2000
  results.dp.2010 <- length(dat$result[dat$journal == "Developmental Psychology" & dat$year == 2010]) / a.dp.2010

  #Percentage of p-values .05 < p <= .1 1990, 2000 and 2010
  p.dp.1990 <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 1990]) /
                      length(dat$result[dat$journal == "Developmental Psychology" & dat$year == 1990]))
                
  p.dp.2000 <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 2000]) /
                      length(dat$result[dat$journal == "Developmental Psychology" & dat$year == 2000]))

  p.dp.2010 <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 2010]) /
                      length(dat$result[dat$journal == "Developmental Psychology" & dat$year == 2010]))

  #Percentage of p-values marginally significant 1990, 2000 and 2010
  marg.dp.1990 <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 1990]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 1990])) /
                        length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 1990]))

  marg.dp.2000 <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 2000]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 2000])) /
                        length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 2000]))

  marg.dp.2010<- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 2010]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 2010])) /
                        length(dat$result[dat$value > 0.05 & dat$value <= 0.1 &
                                        dat$journal == "Developmental Psychology" & dat$year == 2010]))

  #Create data frame
  df.r2 <- data.frame("journal" = rep("Developmental Psychology",3), "year" = c(1990,2000,2010), 
                    "rep_articles" = c(a.dp.1990, a.dp.2000, a.dp.2010), 
                    "rep_p-values.per.article" = round(c(results.dp.1990, results.dp.2000, results.dp.2010), digits = 2),
                    "rep_percent.05.p.1" = round(c(p.dp.1990, p.dp.2000, p.dp.2010), digits = 2), 
                    "rep_marginally.significant.%" = round(c(marg.dp.1990, marg.dp.2000, marg.dp.2010), digits = 2))

#Merge replication JPSP and DP data
df.rep <- rbind(df.r1, df.r2)

#Load data from Pritschet et al
  dat2 <- read.csv("marginals psych science revision_corrections.csv", stringsAsFactors = FALSE)
  #Resaved the file as .csv, will have to look up how to open a .xlsx another day
  dat2 <- dat2[dat2$Year == 1990 | dat2$Year == 2000 | dat2$Year == 2010, ]
  dat2 <- dat2[dat2$Field == 2 | dat2$Field == 3,]
  #Field 2 is Developmental psychology, and field 3 JPSP
  dat2 <- data.frame(dat2$Field, dat2$Year, dat2$Marginals.Yes.No)

#Pritschet et al data from JPSP
  #Number of articles per year for JPSP in Pritschet et al, NB! each row represents an article
  pritschet.articles.jpsp.1990 <- nrow(dat2[dat2$dat2.Field == 3 & dat2$dat2.Year == 1990,])
  pritschet.articles.jpsp.2000 <- nrow(dat2[dat2$dat2.Field == 3 & dat2$dat2.Year == 2000,])
  pritschet.articles.jpsp.2010 <- nrow(dat2[dat2$dat2.Field == 3 & dat2$dat2.Year == 2010,])

  #percentage of articles containing at least one marginally significant result in JPSP
  pritschet.marginal.jpsp.1990 <- 100*(sum(dat2$dat2.Marginals.Yes.No[dat2$dat2.Field == 3 & dat2$dat2.Year == 1990]) / 
                                        pritschet.articles.jpsp.1990)
  pritschet.marginal.jpsp.2000 <- 100*(sum(dat2$dat2.Marginals.Yes.No[dat2$dat2.Field == 3 & dat2$dat2.Year == 2000]) / 
                                        pritschet.articles.jpsp.2000)
  pritschet.marginal.jpsp.2010 <- 100*(sum(dat2$dat2.Marginals.Yes.No[dat2$dat2.Field == 3 & dat2$dat2.Year == 2010]) / 
                                        pritschet.articles.jpsp.2010)

  #dataframe pritschet jpsp
  df.p1 <- data.frame("journal" = rep("JPSP",3), "year" = c(1990,2000,2010), 
                    "Pritschet_articles" = round(c(pritschet.articles.jpsp.1990, pritschet.articles.jpsp.2000, pritschet.articles.jpsp.2010), digits = 2),
                    "Pritschet_marg.sig.percent" = round(c(pritschet.marginal.jpsp.1990, pritschet.marginal.jpsp.2000, pritschet.marginal.jpsp.2010), digits = 2))

#pritschet developmental pscyhology
  #Number of articles per year for developmental pyschology in Pritschet et al, NB! each row represents an article
  pritschet.articles.dp.1990 <- nrow(dat2[dat2$dat2.Field == 2 & dat2$dat2.Year == 1990,])
  pritschet.articles.dp.2000 <- nrow(dat2[dat2$dat2.Field == 2 & dat2$dat2.Year == 2000,])
  pritschet.articles.dp.2010 <- nrow(dat2[dat2$dat2.Field == 2 & dat2$dat2.Year == 2010,])

  #percentage of articles containing at least one marginally significant result in dp
  pritschet.marginal.dp.1990 <- 100*(sum(dat2$dat2.Marginals.Yes.No[dat2$dat2.Field == 2 & dat2$dat2.Year == 1990]) / 
                                         pritschet.articles.dp.1990)
  pritschet.marginal.dp.2000 <- 100*(sum(dat2$dat2.Marginals.Yes.No[dat2$dat2.Field == 2 & dat2$dat2.Year == 2000]) / 
                                         pritschet.articles.dp.2000)
  pritschet.marginal.dp.2010 <- 100*(sum(dat2$dat2.Marginals.Yes.No[dat2$dat2.Field == 2 & dat2$dat2.Year == 2010]) / 
                                        pritschet.articles.dp.2010)
  #DP dataframe Pritschet et al
  df.p2 <- data.frame("journal" = rep("Developmental Psychology",3), "year" = c(1990,2000,2010), 
                    "Pritschet_articles" = round(c(pritschet.articles.dp.1990, pritschet.articles.dp.2000, pritschet.articles.dp.2010), digits = 2),
                    "Pritschet_marg.sig.percent" = round(c(pritschet.marginal.dp.1990, pritschet.marginal.dp.2000, pritschet.marginal.dp.2010), digits = 2))

#Merge dataframes
df.pritschet <- rbind(df.p1, df.p2)

##Merge replication and Pritchet dataframes
df.table.rep <- merge(df.pritschet, df.rep)

#Write table
#write.table(df.table.rep, file = "replication_table.txt")

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
p.limited.overall <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1]) / length(dat$result))
p.limited.clinical <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Clinical.Psychology == 1]) / length(dat$result[dat$Clinical.Psychology == 1]))
p.limited.cognitive <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Neuroscience...Cognition == 1]) / length(dat$result[dat$Neuroscience...Cognition == 1]))
p.limited.core <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Core.of.Psychology == 1]) / length(dat$result[dat$Core.of.Psychology == 1]))
p.limited.developmental <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Developmental.Psychology == 1]) / length(dat$result[dat$Developmental.Psychology == 1]))
p.limited.educational <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Educational.Psychology..School.Psychology...Training == 1]) / length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]))
p.limited.experimental <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Basic...Experimental.Psychology == 1]) / length(dat$result[dat$Basic...Experimental.Psychology == 1]))
p.limited.forensic <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Forensic.Psychology == 1]) / length(dat$result[dat$Forensic.Psychology == 1]))
p.limited.health <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Health.Psychology...Medicine == 1]) / length(dat$result[dat$Health.Psychology...Medicine == 1]))
p.limited.organizational <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Industrial.Organizational.Psychology...Management == 1]) / length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]))
p.limited.social <- 100*(length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Social.Psychology...Social.Processes == 1]) / length(dat$result[dat$Social.Psychology...Social.Processes == 1]))

#marginally significant (%)
marg.overall <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1]))

marg.clinical <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Clinical.Psychology == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Clinical.Psychology == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Clinical.Psychology == 1]))

marg.cognitive <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Neuroscience...Cognition == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Neuroscience...Cognition == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Neuroscience...Cognition == 1]))

marg.core <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Core.of.Psychology == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Core.of.Psychology == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Core.of.Psychology == 1]))

marg.developmental <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Developmental.Psychology == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Developmental.Psychology == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Developmental.Psychology == 1]))

marg.educational <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Educational.Psychology..School.Psychology...Training == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Educational.Psychology..School.Psychology...Training == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Educational.Psychology..School.Psychology...Training == 1]))

marg.experimental <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Basic...Experimental.Psychology == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Basic...Experimental.Psychology == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Basic...Experimental.Psychology == 1]))

marg.forensic <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Forensic.Psychology == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Forensic.Psychology == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Forensic.Psychology == 1]))

marg.health <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Health.Psychology...Medicine == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Health.Psychology...Medicine == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Health.Psychology...Medicine == 1]))

marg.organizational <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Industrial.Organizational.Psychology...Management == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Industrial.Organizational.Psychology...Management == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Industrial.Organizational.Psychology...Management == 1]))

marg.social <- 100*(sum(grepl("marginal|approach", dat$pre[dat$value > 0.05 & dat$value <= 0.1 & dat$Social.Psychology...Social.Processes == 1]) | 
                          grepl("marginal|approach", dat$post[dat$value > 0.05 & dat$value <= 0.1 & dat$Social.Psychology...Social.Processes == 1])) /
                      length(dat$result[dat$value > 0.05 & dat$value <= 0.1 & dat$Social.Psychology...Social.Processes == 1]))


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
                                                           
                                            
#------------------------------------------------------------------