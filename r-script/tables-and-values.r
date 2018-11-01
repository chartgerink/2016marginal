##In text values and tables - marginally significant results in APA-journals: https://osf.io/28gxz/ 

#------------------------------------------
##Startup
#------------------------------------------
#load the full cleaned dataset with added topics
dat <- read.csv("../data/marginal_dataset_with_subfields.csv", stringsAsFactors = FALSE)

#------------------------------------------
##Articles unique to 'core of psychology'
#------------------------------------------

#entries unique to 'core of psychology'

core <- dat[dat$Core.of.Psychology == 1 & dat$Social.Psychology...Social.Processes == 0 & dat$Neuroscience...Cognition == 0 & dat$Industrial.Organizational.Psychology...Management == 0 &
                      dat$Health.Psychology...Medicine == 0 & dat$Forensic.Psychology  == 0 & dat$Educational.Psychology..School.Psychology...Training == 0 &
                      dat$Developmental.Psychology == 0 & dat$Clinical.Psychology == 0 & dat$Basic...Experimental.Psychology == 0,]

#Exclude any remaining entries unique to the topic 'core of psychology' from the main dataset
dat <- dat[dat$Social.Psychology...Social.Processes == 1 | dat$Neuroscience...Cognition == 1 | dat$Industrial.Organizational.Psychology...Management == 1 |
                      dat$Health.Psychology...Medicine == 1 | dat$Forensic.Psychology  == 1 | dat$Educational.Psychology..School.Psychology...Training == 1 |
                      dat$Developmental.Psychology == 1 | dat$Clinical.Psychology == 1 | dat$Basic...Experimental.Psychology == 1,]

#------------------------------------------
##Restricted dataset
#------------------------------------------
dat2 <- dat[dat$value > 0.05 & dat$value <= 0.1,]
dat2 <- dat2[!(dat2$value == 0.1 & dat2$comparison == ">"),]
#Add a variable indicating whether a p-value appears to reported as marginally significant to restricted dataset
dat2$marginal <- grepl("margin|approach", dat2$pre) | grepl("margin|approach", dat2$post)


#------------------------------------------
##Our dataset summarized at the article level
#------------------------------------------
#Create dataset at the article level with marginal indicator in 4 steps
#1) merge full with restricted dataset to get dummy for marginal significance
dat.a <- merge(dat2, dat, all.y = TRUE) #NB! note that dat and dat2 are switched compared to plot file.. (do not work on these at the same time)

#2) Switch NA-values to zeroes (i.e. for all values not in the .05-.1 range)
dat.a$marginal <- ifelse(is.na(dat.a$marginal), 0, dat.a$marginal)

#3)Aggregate to the article level, keeping all variables of interest
dat.a <- aggregate(marginal ~ doi + journal + Social.Psychology...Social.Processes + Basic...Experimental.Psychology +
                    Clinical.Psychology  + Developmental.Psychology + Educational.Psychology..School.Psychology...Training +
                    Forensic.Psychology + Health.Psychology...Medicine + Industrial.Organizational.Psychology...Management +
                    Neuroscience...Cognition, data = dat.a, FUN = sum)
#Note: Here we keep "journal" instead of "year" as we did for the plots

#4)create a dummy to indicate if an article contains at least one marginally significant p-value (.05 - .1)
dat.a$marginal <- ifelse(dat.a$marginal > 0, 1, 0) 
names(dat.a)[names(dat.a) == "marginal"] <- "a.marginal"
#Note: previous to this step we had the _number_ of marginally significant values per article

#------------------------------------------
##Table for comparison with our data and Pritschet et al (2016) - JPSP and DP
#------------------------------------------

#Number of articles our data
  
a.jpsp <- length(unique(dat$doi[dat$journal == "Journal of Personality and Social Psychology"]))

a.dp <- length(unique(dat$doi[dat$journal == "Developmental Psychology"]))

#Number of p-values our data

p.jpsp <- length(dat$result[dat$journal == "Journal of Personality and Social Psychology"])
  
p.dp <- length(dat$result[dat$journal == "Developmental Psychology"])
     
#Number of p-values/article our data
  
results.jpsp <- format(round(length(dat$result[dat$journal == "Journal of Personality and Social Psychology"]) / a.jpsp, digits = 2), nsmall = 2)

  
results.dp <- format(round(length(dat$result[dat$journal == "Developmental Psychology"]) / a.dp, digits = 2), nsmall = 2)

#number of p-values .05 < p <= .1 our data
  
p.limited.jpsp <- length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology"])
  
p.limited.dp <- length(dat2$result[dat2$journal == "Developmental Psychology"])

#Number of .05 < p <= .1 per article our data
limited.a.jpsp <- format(round(length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology"]) / a.jpsp, digits = 2), nsmall = 2)
  
limited.a.dp <- format(round(length(dat2$result[dat2$journal == "Developmental Psychology"]) / a.dp, digits = 2), nsmall = 2)
 
#Percentage of .05 < p <= .1 marginally significant our data

marg.jpsp <- sum(dat2$marginal[dat2$journal == "Journal of Personality and Social Psychology"]) /
                      length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology"])
                  
marg.dp <- sum(dat2$marginal[dat2$journal == "Developmental Psychology"]) /
                      length(dat2$result[dat2$journal == "Developmental Psychology"])

#confidence interval for p-values (rounded and expressed as percentage with two decimals))
se.marg.jpsp <- sqrt(marg.jpsp*(1-marg.jpsp) / p.limited.jpsp) 
ci.marg.jpsp <- format(round(100*c(marg.jpsp - se.marg.jpsp*qnorm(.975), marg.jpsp + se.marg.jpsp*qnorm(.975)), digits = 2), nsmall = 2)

se.marg.dp <- sqrt(marg.dp*(1-marg.dp) / p.limited.dp) 
ci.marg.dp <- format(round(100*c(marg.dp - se.marg.dp*qnorm(.975), marg.dp + se.marg.dp*qnorm(.975)), digits = 2), nsmall = 2)

#Percentage of articles containing at least one marginally significant p-value our data
marg.jpsp.a <- sum(dat.a$a.marginal[dat.a$journal == "Journal of Personality and Social Psychology"]) /
                      length(dat.a$doi[dat.a$journal == "Journal of Personality and Social Psychology"])

marg.dp.a <- sum(dat.a$a.marginal[dat.a$journal == "Developmental Psychology"]) /
                  length(dat.a$doi[dat.a$journal == "Developmental Psychology"])

#confidence interval for articles (rounded and expressed as percentage)
se.marg.jpsp.a <- sqrt(marg.jpsp.a*(1-marg.jpsp.a) / a.jpsp)
ci.marg.jpsp.a <- format(round(100*c(marg.jpsp.a - se.marg.jpsp.a*qnorm(.975), marg.jpsp.a + se.marg.jpsp.a*qnorm(.975)), digits = 2), nsmall = 2)

se.marg.dp.a <- sqrt(marg.dp.a*(1-marg.dp.a) / a.dp)
ci.marg.dp.a <- format(round(100*c(marg.dp.a - se.marg.dp.a*qnorm(.975), marg.dp.a + se.marg.dp.a*qnorm(.975)), digits = 2), nsmall = 2) 

#Data frame of our data JSPS and DP
df.rep <- data.frame("Journal" = c("JPSP", "DP"), "Time.span" = rep("1985 - 2016", 2), 
                  "Articles" = c(prettyNum(a.jpsp, big.mark = ",", preserve.width = "none"), 
                                 prettyNum(a.dp, big.mark = ",", preserve.width = "none")), 
                  "P-values.and.per.article" = c(paste0(prettyNum(p.jpsp, big.mark = ",", preserve.width = "none"), " (", results.jpsp, ")"), 
                                                 paste0(prettyNum(p.dp, big.mark = ",", preserve.width = "none"), " (", results.dp, ")")),
                  "0.05.p.0.1.and.per.article" = c(paste0(prettyNum(p.limited.jpsp, big.mark = ",", preserve.width = "none"), " (", limited.a.jpsp, ")"), 
                                                   paste0(prettyNum(p.limited.dp, big.mark = ",", preserve.width = "none"), " (", limited.a.dp, ")")), 
                  "p.percent.marginal" = c(paste0(round(100*marg.jpsp, digits = 2), " [", ci.marg.jpsp[1], ", ", ci.marg.jpsp[2], "]"),
                                           paste0(round(100*marg.dp, digits = 2), " [", ci.marg.dp[1], ", ", ci.marg.dp[2], "]")),
                  "a.percent.marginal" = c(paste0(round(100*marg.jpsp.a, digits = 2), " [", ci.marg.jpsp.a[1], ", ", ci.marg.jpsp.a[2], "]"),
                                           paste0(round(100*marg.dp.a, digits = 2), " [", ci.marg.dp.a[1], ", ", ci.marg.dp.a[2], "]")))

#Load data from Pritschet et al (2016)
if(!require(readxl)){install.packages("readxl")}
library(readxl)

  dat3 <- read_excel("../data/marginals psych science revision_corrections.xlsx")
  dat3 <- dat3[dat3$Field == 2 | dat3$Field == 3,]
  #Field 2 is Developmental psychology, and field 3 JPSP
  dat3 <- data.frame("Field" = dat3$Field, "Marginals.Yes.No" = dat3$`Marginals Yes/No`, "Year" = dat3$Year)

#Number of articles per year Pritschet et al
pritschet.articles.jpsp <- nrow(dat3[dat3$Field == 3,])

pritschet.articles.dp <- nrow(dat3[dat3$Field == 2,])

#percentage of articles containing at least one marginally significant result Pritschet et al
pritschet.marginal.jpsp <- sum(dat3$Marginals.Yes.No[dat3$Field == 3]) / pritschet.articles.jpsp

pritschet.marginal.dp <- sum(dat3$Marginals.Yes.No[dat3$Field == 2]) / pritschet.articles.dp

#confidence interval for articles Pritchet et al (rounded and expressed as percentage with two decimals)
se.pritschet.jpsp <- sqrt(pritschet.marginal.jpsp*(1-pritschet.marginal.jpsp) / pritschet.articles.jpsp)
ci.pritschet.jpsp <- format(round(100*c(pritschet.marginal.jpsp - se.pritschet.jpsp*qnorm(.975),
                                 pritschet.marginal.jpsp + se.pritschet.jpsp*qnorm(.975)), digits = 2), nsmall = 2)

se.pritschet.dp <- sqrt(pritschet.marginal.dp*(1-pritschet.marginal.dp) / pritschet.articles.dp)
ci.pritschet.dp <- format(round(100*c(pritschet.marginal.dp - se.pritschet.dp*qnorm(.975),
                                 pritschet.marginal.dp + se.pritschet.dp*qnorm(.975)), digits = 2), nsmall = 2)  

#dataframe pritschet et al
df.pritschet <- data.frame("Journal" = c("JPSP", "DP"), "Time.span" = rep("1970 - 2010", 2), 
                  "Articles" = as.factor(c(pritschet.articles.jpsp, pritschet.articles.dp)),
                  "P-values.and.per.article" = rep(NA,2),
                  "0.05.p.0.1.and.per.article" = rep(NA,2),
                  "p.percent.marginal" = c(NA, NA),
                  "a.percent.marginal" = c(paste0(round(100*pritschet.marginal.jpsp, digits = 2), " [", ci.pritschet.jpsp[1], ", ",
                                                  ci.pritschet.jpsp[2], "]"),
                                           paste0(round(100*pritschet.marginal.dp, digits = 2), " [", ci.pritschet.dp[1], ", ",
                                                  ci.pritschet.dp[2], "]")))


##Merge our data and Pritchet dataframes

df.table2 <- rbind(df.rep, df.pritschet)

#-------------------------------------------------------
##Table for subfields and overall (our data)
#-------------------------------------------------------
#Articles
num.articles <- c(length(unique(dat$doi)),length(unique(dat$doi[dat$Clinical.Psychology == 1])), 
                  length(unique(dat$doi[dat$Neuroscience...Cognition == 1])), length(unique(dat$doi[dat$Developmental.Psychology == 1])), 
                  length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1])), length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1])), 
                  length(unique(dat$doi[dat$Forensic.Psychology == 1])), length(unique(dat$doi[dat$Health.Psychology...Medicine == 1])), 
                  length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1])), length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1])))

#p-values per article (with two decimal places)
p.per.article.overall <- format(round(length(dat$result) / length(unique(dat$doi)), digits = 2), nsmall = 2)
p.per.article.clinical <- format(round(length(dat$result[dat$Clinical.Psychology == 1]) / length(unique(dat$doi[dat$Clinical.Psychology == 1])), digits = 2), nsmall = 2)
p.per.article.cognitive <- format(round(length(dat$result[dat$Neuroscience...Cognition == 1]) / length(unique(dat$doi[dat$Neuroscience...Cognition == 1])), digits = 2), nsmall = 2)
p.per.article.developmental <- format(round(length(dat$result[dat$Developmental.Psychology== 1]) / length(unique(dat$doi[dat$Developmental.Psychology == 1])), digits = 2), nsmall = 2)
p.per.article.educational <- format(round(length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]) / length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1])), digits = 2), nsmall = 2)
p.per.article.experimental <- format(round(length(dat$result[dat$Basic...Experimental.Psychology == 1]) / length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1])), digits = 2), nsmall = 2)
p.per.article.forensic <- format(round(length(dat$result[dat$Forensic.Psychology == 1]) / length(unique(dat$doi[dat$Forensic.Psychology == 1])), digits = 2), nsmall = 2)
p.per.article.health <- format(round(length(dat$result[dat$Health.Psychology...Medicine == 1]) / length(unique(dat$doi[dat$Health.Psychology...Medicine == 1])), digits = 2), nsmall = 2)
p.per.article.organizational <- format(round(length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]) / length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1])), digits = 2), nsmall = 2)
p.per.article.social <- format(round(length(dat$result[dat$Social.Psychology...Social.Processes == 1]) / length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1])), digits = 2), nsmall = 2)

# Number of .05 < p <= .1

num.p.limited <- c(length(dat2$result), length(dat2$result[dat2$Clinical.Psychology == 1]), length(dat2$result[dat2$Neuroscience...Cognition == 1]), 
                   length(dat2$result[dat2$Developmental.Psychology == 1]),length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]), 
                   length(dat2$result[dat2$Basic...Experimental.Psychology == 1]), length(dat2$result[dat2$Forensic.Psychology == 1]),length(dat2$result[dat2$Health.Psychology...Medicine == 1]), 
                   length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]), length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]))

# .05 < p <= .1 per article
p.limited.overall <- format(round(length(dat2$result) / length(unique(dat$doi)), digits = 2), nsmall = 2)
p.limited.clinical <- format(round(length(dat2$result[dat2$Clinical.Psychology == 1]) / length(unique(dat$doi[dat$Clinical.Psychology == 1])), digits = 2), nsmall = 2)
p.limited.cognitive <- format(round(length(dat2$result[dat2$Neuroscience...Cognition == 1]) / length(unique(dat$doi[dat$Neuroscience...Cognition == 1])), digits = 2), nsmall = 2)
p.limited.developmental <- format(round(length(dat2$result[dat2$Developmental.Psychology == 1]) / length(unique(dat$doi[dat$Developmental.Psychology == 1])), digits = 2), nsmall = 2)
p.limited.educational <- format(round(length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]) / length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1])), digits = 2), nsmall = 2)
p.limited.experimental <- format(round(length(dat2$result[dat2$Basic...Experimental.Psychology == 1]) / length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1])), digits = 2), nsmall = 2)
p.limited.forensic <- format(round(length(dat2$result[dat2$Forensic.Psychology == 1]) / length(unique(dat$doi[dat$Forensic.Psychology == 1])), digits = 2), nsmall = 2)
p.limited.health <- format(round(length(dat2$result[dat2$Health.Psychology...Medicine == 1]) / length(unique(dat$doi[dat$Health.Psychology...Medicine == 1])), digits = 2), nsmall = 2)
p.limited.organizational <- format(round(length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]) / length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1])), digits = 2), nsmall = 2)
p.limited.social <- format(round(length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]) / length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1])), digits = 2), nsmall = 2)

#Percentage of .05 < p <= .1 marginally significant
marg.overall <- sum(dat2$marginal) / length(dat2$result)

marg.clinical <- sum(dat2$marginal[dat2$Clinical.Psychology == 1]) / length(dat2$result[dat2$Clinical.Psychology == 1])

marg.cognitive <- sum(dat2$marginal[dat2$Neuroscience...Cognition == 1]) / length(dat2$result[dat2$Neuroscience...Cognition == 1])

marg.developmental <- sum(dat2$marginal[dat2$Developmental.Psychology == 1]) / length(dat2$result[dat2$Developmental.Psychology == 1])

marg.educational <- sum(dat2$marginal[dat2$Educational.Psychology..School.Psychology...Training == 1]) / length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1])

marg.experimental <- sum(dat2$marginal[dat2$Basic...Experimental.Psychology == 1]) / length(dat2$result[dat2$Basic...Experimental.Psychology == 1])

marg.forensic <- sum(dat2$marginal[dat2$Forensic.Psychology == 1]) / length(dat2$result[dat2$Forensic.Psychology == 1])
            
marg.health <- sum(dat2$marginal[dat2$Health.Psychology...Medicine == 1]) / length(dat2$result[dat2$Health.Psychology...Medicine == 1])
                 
marg.organizational <- sum(dat2$marginal[dat2$Industrial.Organizational.Psychology...Management == 1]) / length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1])
 
marg.social <- sum(dat2$marginal[dat2$Social.Psychology...Social.Processes == 1]) / length(dat2$result[dat2$Social.Psychology...Social.Processes == 1])

marg.all <- c(marg.overall, marg.clinical, marg.cognitive, marg.developmental, marg.educational,
              marg.experimental, marg.forensic, marg.health, marg.organizational, marg.social)

#Percentage of articles containing at least one marginally significant p-value
overall.marg.a <- sum(dat.a$a.marginal) / length(dat.a$doi)

dat.a <- dat.a[, c(3:11, 1, 2, 12)] #reorder to facilitate loop below

sub.marg.a <- rep(NA, 9)

for(i in 1:9){
 sub.marg.a[i] <- sum(dat.a$a.marginal[dat.a[[i]] == 1]) / length(dat.a$doi[dat.a[[i]] == 1])
}

#Make sure results are in alphabetical order
sub.marg.a <- data.frame(sub.marg.a, disc = c("Social", "Experimental", "Clinical", "Developmental", "Educational",
  "Forensic", "Health","Organizational", "Cognitive"))
sub.marg.a <- sub.marg.a[order(sub.marg.a[["disc"]]),] #Order by field
marg.all.a <- c(overall.marg.a, sub.marg.a[,1])

#confidence interval for p-values (rounded and expressed as percentage)
se.marg.all <- sqrt(marg.all*(1-marg.all) / num.p.limited)
ci.marg.all <- format(round(100*c(marg.all - se.marg.all*qnorm(.975), marg.all + se.marg.all*qnorm(.975)), digits = 2), nsmall = 2)

#confidence interval for marg. articles (rounded and expressed as percentage)
se.marg.all.a <- sqrt(marg.all.a*(1-marg.all.a) / num.articles)
ci.marg.all.a <- format(round(100*c(marg.all.a - se.marg.all.a*qnorm(.975), marg.all.a + se.marg.all.a*qnorm(.975)), digits = 2), nsmall = 2)

#Dataframe for table
df.table1 <- data.frame("Field" = c("All APA journals", "Clinical", "Cognitive", "Developmental", "Educational",
                                          "Experimental", "Forensic", "Health", "Organizational", "Social"), 
                           "Journals.year" = c(length(unique(dat$journal)), 
                                          length(unique(dat$journal[dat$Clinical.Psychology == 1])), 
                                          length(unique(dat$journal[dat$Neuroscience...Cognition == 1])),
                                          length(unique(dat$journal[dat$Developmental.Psychology == 1])), 
                                          length(unique(dat$journal[dat$Educational.Psychology..School.Psychology...Training == 1])),
                                          length(unique(dat$journal[dat$Basic...Experimental.Psychology == 1])), 
                                          length(unique(dat$journal[dat$Forensic.Psychology == 1])),
                                          length(unique(dat$journal[dat$Health.Psychology...Medicine == 1])), 
                                          length(unique(dat$journal[dat$Industrial.Organizational.Psychology...Management == 1])),
                                          length(unique(dat$journal[dat$Social.Psychology...Social.Processes == 1]))),
                            "Articles" = sapply(num.articles, prettyNum, big.mark = ",", preserve.width = "none"),
                            "P-values.and.per.article" = c(paste0(prettyNum(length(dat$result), big.mark = ",", preserve.width = "none"), " (",p.per.article.overall, ")"), 
                                           paste0(prettyNum(length(dat$result[dat$Clinical.Psychology == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.clinical, ")"), 
                                           paste0(prettyNum(length(dat$result[dat$Neuroscience...Cognition == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.cognitive, ")"),
                                           paste0(prettyNum(length(dat$result[dat$Developmental.Psychology == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.developmental, ")"), 
                                           paste0(prettyNum(length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.educational, ")"),
                                           paste0(prettyNum(length(dat$result[dat$Basic...Experimental.Psychology == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.experimental, ")"),
                                           paste0(prettyNum(length(dat$result[dat$Forensic.Psychology == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.forensic, ")"),
                                           paste0(prettyNum(length(dat$result[dat$Health.Psychology...Medicine == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.health, ")"),
                                           paste0(prettyNum(length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.organizational, ")"),
                                           paste0(prettyNum(length(dat$result[dat$Social.Psychology...Social.Processes == 1]), big.mark = ",", preserve.width = "none"), " (",p.per.article.social, ")")),
                            "0.05.p.0.1.and.per.article" = c(paste0(prettyNum(num.p.limited[1], big.mark = ",", preserve.width = "none"), " (", p.limited.overall, ")"),
                                                     paste0(prettyNum(num.p.limited[2], big.mark = ",", preserve.width = "none"), " (", p.limited.clinical, ")"),
                                                     paste0(prettyNum(num.p.limited[3], big.mark = ",", preserve.width = "none"), " (", p.limited.cognitive, ")"),
                                                     paste0(prettyNum(num.p.limited[4], big.mark = ",", preserve.width = "none"), " (", p.limited.developmental, ")"),
                                                     paste0(prettyNum(num.p.limited[5], big.mark = ",", preserve.width = "none"), " (", p.limited.educational, ")"),
                                                     paste0(prettyNum(num.p.limited[6], big.mark = ",", preserve.width = "none"), " (", p.limited.experimental, ")"),
                                                     paste0(prettyNum(num.p.limited[7], big.mark = ",", preserve.width = "none"), " (", p.limited.forensic, ")"), 
                                                     paste0(prettyNum(num.p.limited[8], big.mark = ",", preserve.width = "none"), " (", p.limited.health, ")"),
                                                     paste0(prettyNum(num.p.limited[9], big.mark = ",", preserve.width = "none"), " (", p.limited.organizational, ")"),
                                                     paste0(prettyNum(num.p.limited[10], big.mark = ",", preserve.width = "none"), " (", p.limited.social, ")")),
                           "p.percent.marginal" = c(paste0(round(100*marg.all[1], digits = 2), " [", ci.marg.all[1], ", ", ci.marg.all[11], "]"),
                                                    paste0(round(100*marg.all[2], digits = 2), " [", ci.marg.all[2], ", ", ci.marg.all[12], "]"),
                                                    paste0(round(100*marg.all[3], digits = 2), " [", ci.marg.all[3], ", ", ci.marg.all[13], "]"),
                                                    paste0(round(100*marg.all[4], digits = 2), " [", ci.marg.all[4], ", ", ci.marg.all[14], "]"),
                                                    paste0(round(100*marg.all[5], digits = 2), " [", ci.marg.all[5], ", ", ci.marg.all[15], "]"),
                                                    paste0(round(100*marg.all[6], digits = 2), " [", ci.marg.all[6], ", ", ci.marg.all[16], "]"),
                                                    paste0(round(100*marg.all[7], digits = 2), " [", ci.marg.all[7], ", ", ci.marg.all[17], "]"),
                                                    paste0(round(100*marg.all[8], digits = 2), " [", ci.marg.all[8], ", ", ci.marg.all[18], "]"),
                                                    paste0(round(100*marg.all[9], digits = 2), " [", ci.marg.all[9], ", ", ci.marg.all[19], "]"),
                                                    paste0(round(100*marg.all[10], digits = 2), " [", ci.marg.all[10], ", ", ci.marg.all[20], "]")),
                        "a.percent.marginal" = c(paste0(round(100*marg.all.a[1], digits = 2), " [", ci.marg.all.a[1], ", ", ci.marg.all.a[11], "]"),
                                                 paste0(round(100*marg.all.a[2], digits = 2), " [", ci.marg.all.a[2], ", ", ci.marg.all.a[12], "]"),
                                                 paste0(round(100*marg.all.a[3], digits = 2), " [", ci.marg.all.a[3], ", ", ci.marg.all.a[13], "]"),
                                                 paste0(round(100*marg.all.a[4], digits = 2), " [", ci.marg.all.a[4], ", ", ci.marg.all.a[14], "]"),
                                                 paste0(round(100*marg.all.a[5], digits = 2), " [", ci.marg.all.a[5], ", ", ci.marg.all.a[15], "]"),
                                                 paste0(round(100*marg.all.a[6], digits = 2), " [", ci.marg.all.a[6], ", ", ci.marg.all.a[16], "]"),
                                                 paste0(round(100*marg.all.a[7], digits = 2), " [", ci.marg.all.a[7], ", ", ci.marg.all.a[17], "]"),
                                                 paste0(round(100*marg.all.a[8], digits = 2), " [", ci.marg.all.a[8], ", ", ci.marg.all.a[18], "]"),
                                                 paste0(round(100*marg.all.a[9], digits = 2), " [", ci.marg.all.a[9], ", ", ci.marg.all.a[19], "]"),
                                                 paste0(round(100*marg.all.a[10], digits = 2), " [", ci.marg.all.a[10], ", ", ci.marg.all.a[20], "]")))

#------------------------------------------
##List of objects for r-markdown file
#------------------------------------------
#Load original dataset
original <- read.csv("../data/marginal_dataset.csv", stringsAsFactors = FALSE, strip.white = TRUE)
#Force dat$value into a numeric variable
original$value <- as.numeric(original$value)

#"original" = original full dataset without trimming, "dat" = full dataset with nodoi and missing p-values removed + journal names corrected + metadata and topics added, 
#"dat2" = "dat" but only entries with .05 < p <= .1, "dat3" = data from Pritschet et al

marginal_list <- list(
                      #Method section
                      entries.original = prettyNum(nrow(original), big.mark = ",", preserve.width = "none"), #Number of p-values in 'original' dataset
                      nodoi = sum(grepl("nodoi", original$doi)), #number of entries lacking doi in original dataset
                      nodoipercent = signif(100*(sum(grepl("nodoi", original$doi))/nrow(original)), digits = 2), #as a percentage of total rows
                      badp = prettyNum(sum(is.na(original$value)), big.mark = ",", preserve.width = "none"), #Number of missing p-vales
                      badppercent = round(100*(sum(is.na(original$value))/nrow(original)), digits = 2), #as a percentage of total rows
                      mis.meta = prettyNum(sum(is.na(original[!grepl("nodoi", original$doi) & !is.na(original$value),]$journal)), big.mark = ",", preserve.width = "none"), #Number of entries lacking meta-data
                      mis.metapercent = round(100*(sum(is.na(original[!grepl("nodoi", original$doi) & !is.na(original$value),]$journal)) / nrow(original)), digits = 2), #as a percentage of total rows
                      entries.final = prettyNum(nrow(dat2), big.mark = ",", preserve.width = "none"),  #Number of p-values after cleaning
                      entries.finalpercent = round(100*(nrow(dat2) / nrow(original)), digits = 2), #as a percentage of original number of entries
                      articles.with.p = prettyNum(length(unique(dat$doi)), big.mark = ",", preserve.width = "none"), #number of articles in complete dataset (i.e. all articles in APA containing p-values)
                      articles.final = prettyNum(length(unique(dat2$doi)), big.mark = ",", preserve.width = "none"), #number of articles in final sample
                      articles.finalpercent = round(100*(length(unique(dat2$doi))/length(unique(dat$doi))), digits = 2), #as a percentage of original number of articles
                      journals.final = length(unique(dat2$journal)), #number of journals in final sample
                      corearticles = length(unique(core$doi)), #number of articles unique to the 'core' topic
                      unique.core = prettyNum(nrow(core), big.mark = ",", preserve.width = "none"), #number of p-values unique to 'core' topic
                      unique.corepercent = round(100*(nrow(core) / nrow(original)), digits = 2), #as a percentage of total
                      table1 = df.table1, #table 1
                      table2 = df.table2, #table 2
                      
                      #Results, estimates for the different disciplines and Pritschet et al's results
                      marg.jpsp = round(marg.jpsp, digits = 2),
                      marg.dp = round(marg.dp, digits = 2),
                      marg.overall = round(marg.overall, digits = 2),
                      marg.clinical = round(marg.clinical, digits = 2),
                      marg.cognitive = round(marg.cognitive, digits = 2),
                      marg.developmental = round(marg.developmental, digits = 2),
                      marg.educational = round(marg.educational, digits = 2),
                      marg.experimental = round(marg.experimental, digits = 2),
                      marg.forensic = round(marg.forensic, digits = 2),
                      marg.health = round(marg.health, digits = 2),
                      marg.organizational = round(marg.organizational, digits = 2),
                      marg.social = round(marg.social, digits = 2),
                      pritschet.jpsp = round(pritschet.marginal.jpsp, digits = 2),
                      pritschet.dp = round(pritschet.marginal.dp, digits = 2))

saveRDS(marginal_list, file = "../writing/marginal_rmarkdown_objects.RData")

#------------------------------------------
##Flowchart values
#------------------------------------------

#extracted p-values
nrow(original)

#excluded due to misreporting or extraction failure 
  #Lacking DOI and journal name/year
    sum(grepl("nodoi", original$doi))
    100*(sum(grepl("nodoi", original$doi))/nrow(original)) #%
  #Non-numerical p-value 
    sum(is.na(original$value))
    100*(sum(is.na(original$value))/nrow(original)) #%
  #total
    sum(grepl("nodoi", original$doi)) + sum(is.na(original$value))
    100*((sum(grepl("nodoi", original$doi)) + sum(is.na(original$value))) / nrow(original)) #%

#Remaining p-values
nrow(original) - sum(grepl("nodoi", original$doi)) - sum(is.na(original$value))
100*((nrow(original) - sum(grepl("nodoi", original$doi)) - sum(is.na(original$value))) / nrow(original)) #%

#Missing metadata added
sum(is.na(original[!grepl("nodoi", original$doi) & !is.na(original$value),]$journal))
100*(sum(is.na(original[!grepl("nodoi", original$doi) & !is.na(original$value),]$journal)) / nrow(original)) #%

#Excluded 'core of psychology'
nrow(core)
100*(nrow(core) / nrow(original)) #%

#Excluded: p <= .05 and p > .1
nrow(original) - sum(grepl("nodoi", original$doi)) - sum(is.na(original$value)) - nrow(core) - nrow(dat2)
100*((nrow(original) - sum(grepl("nodoi", original$doi)) - sum(is.na(original$value)) - nrow(core) - nrow(dat2)) / nrow(original)) #%

#Final dataset
nrow(dat2)
100*(nrow(dat2) / nrow(original))

#Test sample
nrow(read.csv("../data/test_sample_marginal_dataset.csv", stringsAsFactors = FALSE))
100*(nrow(read.csv("../data/test_sample_marginal_dataset.csv", stringsAsFactors = FALSE)) / nrow(dat2)) #%


#------------------------------------------
##Degrees of freedom flowchart values
#------------------------------------------
df <- read.csv("../data/df_raw_dataset.csv", stringsAsFactors = FALSE)
df2 <- read.csv("../data/final_df_dataset.csv", stringsAsFactors = FALSE)

#extracted APA-results
nrow(df)

#Excluded for missing doi
sum(grepl("nodoi", df$Source))
100*(sum(grepl("nodoi", df$Source)) / nrow(df))

#Excluded because using Z- or chisquared statistics
sum(is.na(df$df2))
100*(sum(is.na(df$df2)) / nrow(df))

#remaining results
nrow(df) - sum(grepl("nodoi", df$Source)) - sum(is.na(df$df2))
100*((nrow(df) - sum(grepl("nodoi", df$Source)) - sum(is.na(df$df2))) / nrow(df))

#Excluded 'core of psychology'
nrow(df) -  sum(grepl("nodoi", df$Source)) - sum(is.na(df$df2)) - nrow(df2)
100*((nrow(df) -  sum(grepl("nodoi", df$Source)) - sum(is.na(df$df2)) - nrow(df2)) / nrow(df))

#final df dataset
nrow(df2)
100*(nrow(df2) / nrow(df))

#----------------------------------------------
#End
  