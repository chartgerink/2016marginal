##In text values and tables - marginally significant results in APA-journals: https://osf.io/28gxz/ 

#------------------------------------------
##Startup
#------------------------------------------
#load the full cleaned dataset with added topics
dat <- read.csv("../data/marginal_dataset_with_subfields.csv", stringsAsFactors = FALSE)

#Restricted dataset
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
  
p.limited.jpsp <- length(dat2$result[dat2$journal == "Journal of Personality and Social Psychology" ])
  
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
  dat3 <- data.frame("Field" = dat3$Field, "Marginals.Yes.No" = dat3$`Marginals Yes/No`)

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
##Unique articles not in 'core of psychology'
#------------------------------------------

noncore <- unique(dat$doi[dat$Neuroscience...Cognition == 1 | dat$Industrial.Organizational.Psychology...Management ==1 | dat$Health.Psychology...Medicine == 1 |
               dat$Forensic.Psychology == 1 | dat$Educational.Psychology..School.Psychology...Training == 1 | dat$Developmental.Psychology == 1 |
               dat$Clinical.Psychology == 1 | dat$Basic...Experimental.Psychology == 1 | dat$Social.Psychology...Social.Processes == 1]) 

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
                      entries.final = prettyNum(length(dat2$result), big.mark = ",", preserve.width = "none"), 
                      entries.finalpercent = round(100*(length(dat2$result) / nrow(original)), digits = 2), 
                      articles.with.p = prettyNum(length(unique(dat$doi)), big.mark = ",", preserve.width = "none"),
                      articles.final = prettyNum(length(unique(dat2$doi)), big.mark = ",", preserve.width = "none"), 
                      articles.finalpercent = round(100*(length(unique(dat2$doi))/length(unique(dat$doi))), digits = 2), 
                      journals.final = length(unique(dat2$journal)), 
                      difjournals = setdiff(unique(dat$journal), unique(dat2$journal)),
                      corearticles = length(setdiff(unique(dat$doi[dat$Core.of.Psychology == 1]), noncore)),
                      table1 = df.table1,
                      table2 = df.table2)

saveRDS(marginal_list, file = "../writing/marginal_rmarkdown_objects.RData")

