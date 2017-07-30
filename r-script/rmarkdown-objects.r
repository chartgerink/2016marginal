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
##Table for comparison with current paper and Pritschet et al (2016) - JPSP and DP
#------------------------------------------

#Number of articles current paper
  
a.jpsp <- length(unique(dat$doi[dat$journal == "Journal of Personality and Social Psychology"]))

a.dp <- length(unique(dat$doi[dat$journal == "Developmental Psychology"]))

#Number of p-values current paper

p.jpsp <- length(dat$result[dat$journal == "Journal of Personality and Social Psychology"])
  
p.dp <- length(dat$result[dat$journal == "Developmental Psychology"])
     
#Number of p-values/article current paper
  
results.jpsp <- round(length(dat$result[dat$journal == "Journal of Personality and Social Psychology"]) / a.jpsp, digits = 2)

  
results.dp <- round(length(dat$result[dat$journal == "Developmental Psychology"]) / a.dp, digits = 2)

#number of p-values .05 < p <= .1 current paper
  
p.limited.jpsp <- length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology"])
  
p.limited.dp <- length(dat2$result[dat2$journal == "Developmental Psychology"])

#Number of .05 < p <= .1 per article current paper
limited.a.jpsp <- round(length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology"]) / a.jpsp, digits = 2)
  
limited.a.dp <- round(length(dat2$result[dat2$journal == "Developmental Psychology"]) / a.dp, digits = 2)
 
#Percentage of .05 < p <= .1 marginally significant current paper

marg.jpsp <- 100*(sum(dat2$marginal[dat2$journal == "Journal of Personality and Social Psychology"]) /
                      length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology"]))
                  
marg.dp <- 100*(sum(dat2$marginal[dat2$journal == "Developmental Psychology"]) /
                      length(dat2$result[dat2$journal == "Developmental Psychology"]))

   
#Data frame of current paper data JSPS and DP
df.rep <- data.frame("Journal" = c("JPSP", "DP"), "Time.span" = rep("1985 - 2016", 2), 
                  "Articles" = c(prettyNum(a.jpsp, big.mark = ",", preserve.width = "none"), 
                                 prettyNum(a.dp, big.mark = ",", preserve.width = "none")), 
                  "P-values.and.per.article" = c(paste0(prettyNum(p.jpsp, big.mark = ",", preserve.width = "none"), " (", results.jpsp, ")"), 
                                                 paste0(prettyNum(p.dp, big.mark = ",", preserve.width = "none"), " (", results.dp, ")")),
                  "0.05.p.0.1.and.per.article" = c(paste0(prettyNum(p.limited.jpsp, big.mark = ",", preserve.width = "none"), " (", limited.a.jpsp, ")"), 
                                                   paste0(prettyNum(p.limited.dp, big.mark = ",", preserve.width = "none"), " (", limited.a.dp, ")")), 
                  "percent.marginal" = round(c(marg.jpsp, marg.dp), digits = 2))

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
pritschet.marginal.jpsp <- 100*(sum(dat3$Marginals.Yes.No[dat3$Field == 3]) / pritschet.articles.jpsp)

pritschet.marginal.dp <- 100*(sum(dat3$Marginals.Yes.No[dat3$Field == 2]) / pritschet.articles.dp)
    
#dataframe pritschet et al
df.pritschet <- data.frame("Journal" = c("JPSP", "DP"), "Time.span" = rep("1970 - 2010", 2), 
                  "Articles" = as.factor(c(pritschet.articles.jpsp, pritschet.articles.dp)),
                  "P-values.and.per.article" = rep(NA,2),
                  "0.05.p.0.1.and.per.article" = rep(NA,2),
                  "percent.marginal" = round(c(pritschet.marginal.jpsp, pritschet.marginal.dp), digits = 2))


##Merge current paper and Pritchet dataframes

df.table2 <- rbind(df.rep, df.pritschet)

#-------------------------------------------------------
##Table for subfields and overall (current paper data)
#-------------------------------------------------------

#p-values per article
p.per.article.overall <- round(length(dat$result) / length(unique(dat$doi)), digits = 2)
p.per.article.clinical <- round(length(dat$result[dat$Clinical.Psychology == 1]) / length(unique(dat$doi[dat$Clinical.Psychology == 1])), digits = 2)
p.per.article.cognitive <- round(length(dat$result[dat$Neuroscience...Cognition == 1]) / length(unique(dat$doi[dat$Neuroscience...Cognition == 1])), digits = 2)
p.per.article.developmental <- round(length(dat$result[dat$Developmental.Psychology== 1]) / length(unique(dat$doi[dat$Developmental.Psychology == 1])), digits = 2)
p.per.article.educational <- round(length(dat$result[dat$Educational.Psychology..School.Psychology...Training == 1]) / length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1])), digits = 2)
p.per.article.experimental <- round(length(dat$result[dat$Basic...Experimental.Psychology == 1]) / length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1])), digits = 2)
p.per.article.forensic <- round(length(dat$result[dat$Forensic.Psychology == 1]) / length(unique(dat$doi[dat$Forensic.Psychology == 1])), digits = 2)
p.per.article.health <- round(length(dat$result[dat$Health.Psychology...Medicine == 1]) / length(unique(dat$doi[dat$Health.Psychology...Medicine == 1])), digits = 2)
p.per.article.organizational <- round(length(dat$result[dat$Industrial.Organizational.Psychology...Management == 1]) / length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1])), digits = 2)
p.per.article.social <- round(length(dat$result[dat$Social.Psychology...Social.Processes == 1]) / length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1])), digits = 2)

# .05 < p <= .1 per article
p.limited.overall <- round(length(dat2$result) / length(unique(dat$doi)), digits = 2)
p.limited.clinical <- round(length(dat2$result[dat2$Clinical.Psychology == 1]) / length(unique(dat$doi[dat$Clinical.Psychology == 1])), digits = 2)
p.limited.cognitive <- round(length(dat2$result[dat2$Neuroscience...Cognition == 1]) / length(unique(dat$doi[dat$Neuroscience...Cognition == 1])), digits = 2)
p.limited.developmental <- round(length(dat2$result[dat2$Developmental.Psychology == 1]) / length(unique(dat$doi[dat$Developmental.Psychology == 1])), digits = 2)
p.limited.educational <- round(length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]) / length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1])), digits = 2)
p.limited.experimental <- round(length(dat2$result[dat2$Basic...Experimental.Psychology == 1]) / length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1])), digits = 2)
p.limited.forensic <- round(length(dat2$result[dat2$Forensic.Psychology == 1]) / length(unique(dat$doi[dat$Forensic.Psychology == 1])), digits = 2)
p.limited.health <- round(length(dat2$result[dat2$Health.Psychology...Medicine == 1]) / length(unique(dat$doi[dat$Health.Psychology...Medicine == 1])), digits = 2)
p.limited.organizational <- round(length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]) / length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1])), digits = 2)
p.limited.social <- round(length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]) / length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1])), digits = 2)

#marginally significant (%)
marg.overall <- 100*(sum(dat2$marginal) / length(dat2$result))

marg.clinical <- 100*(sum(dat2$marginal[dat2$Clinical.Psychology == 1]) / length(dat2$result[dat2$Clinical.Psychology == 1]))

marg.cognitive <- 100*(sum(dat2$marginal[dat2$Neuroscience...Cognition == 1]) / length(dat2$result[dat2$Neuroscience...Cognition == 1]))

marg.developmental <- 100*(sum(dat2$marginal[dat2$Developmental.Psychology == 1]) / length(dat2$result[dat2$Developmental.Psychology == 1]))

marg.educational <- 100*(sum(dat2$marginal[dat2$Educational.Psychology..School.Psychology...Training == 1]) / length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]))

marg.experimental <- 100*(sum(dat2$marginal[dat2$Basic...Experimental.Psychology == 1]) / length(dat2$result[dat2$Basic...Experimental.Psychology == 1]))

marg.forensic <- 100*(sum(dat2$marginal[dat2$Forensic.Psychology == 1]) / length(dat2$result[dat2$Forensic.Psychology == 1]))
            
marg.health <- 100*(sum(dat2$marginal[dat2$Health.Psychology...Medicine == 1]) / length(dat2$result[dat2$Health.Psychology...Medicine == 1]))
                 
marg.organizational <- 100*(sum(dat2$marginal[dat2$Industrial.Organizational.Psychology...Management == 1]) / length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]))
 
marg.social <- 100*(sum(dat2$marginal[dat2$Social.Psychology...Social.Processes == 1]) / length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]))

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
                            "Articles" = c(prettyNum(length(unique(dat$doi)), big.mark = ",", preserve.width = "none"), 
                                          prettyNum(length(unique(dat$doi[dat$Clinical.Psychology == 1])), big.mark = ",", preserve.width = "none"), 
                                          prettyNum(length(unique(dat$doi[dat$Neuroscience...Cognition == 1])), big.mark = ",", preserve.width = "none"),
                                          prettyNum(length(unique(dat$doi[dat$Developmental.Psychology == 1])), big.mark = ",", preserve.width = "none"),
                                          prettyNum(length(unique(dat$doi[dat$Educational.Psychology..School.Psychology...Training == 1])), big.mark = ",", preserve.width = "none"),
                                          prettyNum(length(unique(dat$doi[dat$Basic...Experimental.Psychology == 1])), big.mark = ",", preserve.width = "none"),
                                          prettyNum(length(unique(dat$doi[dat$Forensic.Psychology == 1])), big.mark = ",", preserve.width = "none"),
                                          prettyNum(length(unique(dat$doi[dat$Health.Psychology...Medicine == 1])), big.mark = ",", preserve.width = "none"),
                                          prettyNum(length(unique(dat$doi[dat$Industrial.Organizational.Psychology...Management == 1])), big.mark = ",", preserve.width = "none"),
                                          prettyNum(length(unique(dat$doi[dat$Social.Psychology...Social.Processes == 1])), big.mark = ",", preserve.width = "none")),
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
                            "0.05.p.0.1.and.per.article" = c(paste0(prettyNum(length(dat2$result), big.mark = ",", preserve.width = "none"), " (", p.limited.overall, ")"),
                                                     paste0(prettyNum(length(dat2$result[dat2$Clinical.Psychology == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.clinical, ")"),
                                                     paste0(prettyNum(length(dat2$result[dat2$Neuroscience...Cognition == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.cognitive, ")"),
                                                     paste0(prettyNum(length(dat2$result[dat2$Developmental.Psychology == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.developmental, ")"),
                                                     paste0(prettyNum(length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.educational, ")"),
                                                     paste0(prettyNum(length(dat2$result[dat2$Basic...Experimental.Psychology == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.experimental, ")"),
                                                     paste0(prettyNum(length(dat2$result[dat2$Forensic.Psychology == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.forensic, ")"), 
                                                     paste0(prettyNum(length(dat2$result[dat2$Health.Psychology...Medicine == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.health, ")"),
                                                     paste0(prettyNum(length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.organizational, ")"),
                                                     paste0(prettyNum(length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]), big.mark = ",", preserve.width = "none"), " (", p.limited.social, ")")),
                           "percent.marginal" = round(c(marg.overall, marg.clinical, marg.cognitive, marg.developmental, marg.educational,
                                                         marg.experimental, marg.forensic, marg.health, marg.organizational, marg.social), digits = 2))
#------------------------------------------
##exploratory section objects
#------------------------------------------

amarg.jpsp <- length(unique(dat2$doi[dat2$journal == "Journal of Personality and Social Psychology" & dat2$marginal == 1]))

amarg.dp <- length(unique(dat2$doi[dat2$journal == "Developmental Psychology" & dat2$marginal == 1]))

result.amarg.jpsp <- round(sum(dat2$marginal[dat2$journal == "Journal of Personality and Social Psychology"]) / amarg.jpsp, digits = 2)
  
result.amarg.dp <- round(sum(dat2$marginal[dat2$journal == "Developmental Psychology"]) / amarg.dp, digits = 2)

jpsp.percent.article <- amarg.jpsp / a.jpsp
dp.percent.article <- amarg.dp / a.dp

#Trim strings to 100 characters and search them
dat2$pre100 <- substr(dat2$pre, 100, 200)
dat2$post100 <- substr(dat2$post, 1, 100)
dat2$marginal100 <- grepl("margin|approach", dat2$pre100) | grepl("margin|approach", dat2$post100)

marg.jpsp100 <- 100*(sum(dat2$marginal100[dat2$journal == "Journal of Personality and Social Psychology"]) /
                      length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology"]))
                  
marg.dp100 <- 100*(sum(dat2$marginal100[dat2$journal == "Developmental Psychology"]) /
                      length(dat2$result[dat2$journal == "Developmental Psychology"]))

#marginally significant fields and overall searching 100 characters (%)
marg.overall100 <- 100*(sum(dat2$marginal100) / length(dat2$result))

marg.clinical100 <- 100*(sum(dat2$marginal100[dat2$Clinical.Psychology == 1]) / length(dat2$result[dat2$Clinical.Psychology == 1]))

marg.cognitive100 <- 100*(sum(dat2$marginal100[dat2$Neuroscience...Cognition == 1]) / length(dat2$result[dat2$Neuroscience...Cognition == 1]))

marg.developmental100 <- 100*(sum(dat2$marginal100[dat2$Developmental.Psychology == 1]) / length(dat2$result[dat2$Developmental.Psychology == 1]))

marg.educational100 <- 100*(sum(dat2$marginal100[dat2$Educational.Psychology..School.Psychology...Training == 1]) / length(dat2$result[dat2$Educational.Psychology..School.Psychology...Training == 1]))

marg.experimental100 <- 100*(sum(dat2$marginal100[dat2$Basic...Experimental.Psychology == 1]) / length(dat2$result[dat2$Basic...Experimental.Psychology == 1]))

marg.forensic100 <- 100*(sum(dat2$marginal100[dat2$Forensic.Psychology == 1]) / length(dat2$result[dat2$Forensic.Psychology == 1]))
            
marg.health100 <- 100*(sum(dat2$marginal100[dat2$Health.Psychology...Medicine == 1]) / length(dat2$result[dat2$Health.Psychology...Medicine == 1]))
                 
marg.organizational100 <- 100*(sum(dat2$marginal100[dat2$Industrial.Organizational.Psychology...Management == 1]) / length(dat2$result[dat2$Industrial.Organizational.Psychology...Management == 1]))
 
marg.social100 <- 100*(sum(dat2$marginal100[dat2$Social.Psychology...Social.Processes == 1]) / length(dat2$result[dat2$Social.Psychology...Social.Processes == 1]))

#Dataframe for table3
df.table3 <- data.frame("Field" = c("All APA journals", "Clinical", "Cognitive", "Developmental", "Educational",
                                          "Experimental", "Forensic", "Health", "Organizational", "Social"), 
                         "percent.marginal" = round(c(marg.overall, marg.clinical, marg.cognitive, marg.developmental, marg.educational,
                                                         marg.experimental, marg.forensic, marg.health, marg.organizational, marg.social), digits = 2),
                       "percent.marginal100" = round(c(marg.overall100, marg.clinical100, marg.cognitive100, marg.developmental100, marg.educational100,
                                                         marg.experimental100, marg.forensic100, marg.health100, marg.organizational100, marg.social100), digits = 2))


#------------------------------------------
##List of objects for r-markdown file
#------------------------------------------
#Load original dataset
original <- read.csv("../data/marginal_dataset.csv", stringsAsFactors = FALSE, strip.white = TRUE)
#Force dat$value into a numeric variable
original$value <- as.numeric(original$value)

#"original" = original full dataset without trimming, "dat" = full dataset with nodoi and missing p-values removed + journal names corrected + metadata and topics added, 
#"dat2" = "dat" but only entries with .05 < p <= .1, "dat3" = data from Pritschet et al

marginal_list <- list(entries.original = prettyNum(nrow(original), big.mark = ",", preserve.width = "none"), 
                      nodoi = sum(grepl("nodoi", original$doi)), 
                      nodoipercent = signif(100*(sum(grepl("nodoi", original$doi))/nrow(original)), digits = 2),
                      badp = prettyNum(sum(is.na(original$value)), big.mark = ",", preserve.width = "none"), 
                      badppercent = round(100*(sum(is.na(original$value))/nrow(original)), digits = 2),
                      mis.meta = prettyNum(sum(is.na(original[!grepl("nodoi", original$doi) & !is.na(original$value),]$journal)), big.mark = ",", preserve.width = "none"),
                      mis.metapercent = round(100*(sum(is.na(original[!grepl("nodoi", original$doi) & !is.na(original$value),]$journal)) / nrow(original)), digits = 2),
                      entries.final = prettyNum(nrow(dat2), big.mark = ",", preserve.width = "none"), 
                      entries.finalpercent = round(100*(nrow(dat2) / nrow(original)), digits = 2), 
                      articles.with.p = prettyNum(length(unique(dat$doi)), big.mark = ",", preserve.width = "none"),
                      articles.final = prettyNum(length(unique(dat2$doi)), big.mark = ",", preserve.width = "none"), 
                      articles.finalpercent = round(100*(length(unique(dat2$doi))/length(unique(dat$doi))), digits = 2), 
                      journals.final = length(unique(dat2$journal)), 
                      difjournals = setdiff(unique(dat$journal), unique(dat2$journal)),
                      corearticles = length(unique(core$doi)),
                      unique.core = prettyNum(nrow(core), big.mark = ",", preserve.width = "none"),
                      unique.corepercent = round(100*(nrow(core) / nrow(original)), digits = 2),
                      no.marg.a.dp = prettyNum((a.dp - amarg.dp), big.mark = ",", preserve.width = "none"),
                      no.marg.p.dp = prettyNum((p.limited.dp*(1-marg.dp/100)), big.mark = ",", preserve.width = "none"),
                      result.amarg.jpsp = result.amarg.jpsp,
                      result.amarg.dp = result.amarg.dp,
                      jpsp.percent.article = round(100*jpsp.percent.article, digits = 2),
                      dp.percent.article = round(100*dp.percent.article, digits = 2),
                      pritschet.jpsp = round(pritschet.marginal.jpsp, digits = 2),
                      pritschet.dp = round(pritschet.marginal.dp, digits = 2),
                      marg.jpsp = round(marg.jpsp, digits = 2),
                      marg.dp = round(marg.dp, digits = 2),
                      marg.jpsp100 = round(marg.jpsp100, digits = 2),
                      marg.dp100 = round(marg.dp100, digits = 2),
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
                      table1 = df.table1,
                      table2 = df.table2,
                      table3 = df.table3)

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

#----------------------------------------------
#End
  