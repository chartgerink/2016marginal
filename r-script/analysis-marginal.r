##Analysis of data - marginally significant results in APA-journals: https://osf.io/28gxz/

#----------------------------------

#Load dataset
dat <- read.csv("../data/test_sample_marginal_dataset.csv", stringsAsFactors = FALSE)
#Change for correct dataset when doing full analysis

#---------------------------------------------------------------------
##Dataframes with proportion of marginal results per year for JPSP and 'Developmental Psychology'
#---------------------------------------------------------------------
jpsp.sum <- merge(aggregate(result ~ year, data = dat[dat$journal == "Journal of Personality and Social Psychology",], FUN = length), 
                    aggregate(marginal ~ year, data = dat[dat$journal == "Journal of Personality and Social Psychology",], FUN = sum), by = "year")

jpsp.sum$percentage.marginal <- 100*(jpsp.sum$marginal/jpsp.sum$result)

dp.sum <- merge(aggregate(result ~ year, data = dat[dat$journal == "Developmental Psychology",], FUN = length), 
                    aggregate(marginal ~ year, data = dat[dat$journal == "Developmental Psychology",], FUN = sum), by = "year")

dp.sum$percentage.marginal <- 100*(dp.sum$marginal/dp.sum$result)

#Combine into one dataframe for graphing
if(!require(gdata)){install.packages("gdata")}
library(gdata)

replication.sum <- combine(dp.sum, jpsp.sum)
replication.sum$source <- factor(replication.sum$source, labels = c("Developmental Psychology", "JPSP"))

#---------------------------------------------------------------------
##Dataframes with proportion of marginal results per year for different subfields, and a variable for color coding
#---------------------------------------------------------------------

#Overall
dat.sum <- merge(aggregate(result ~ year, data = dat, FUN = length), 
                 aggregate(marginal ~ year, data = dat, FUN = sum), by = "year")

dat.sum$percentage.marginal <- 100*(dat.sum$marginal/dat.sum$result)

dat.sum$Journals <- rep(NA, dim(dat.sum)[1])
for (year in seq_along(dat.sum$year)) {
  if(length(unique(dat$journal[dat$year == dat.sum$year[year]])) < 5) 
  {
    dat.sum$Journals[year] <- 1
  }
  else 
  {
    ifelse(length(unique(dat$journal[dat$year == dat.sum$year[year]])) < 11, 
           dat.sum$Journals[year] <- 2, 
           dat.sum$Journals[year] <- 3)
  }
}


#clinical psychology
clinical.sum <- merge(aggregate(result ~ year, data = dat[dat$Clinical.Psychology == 1,], FUN = length), 
                      aggregate(marginal ~ year, data = dat[dat$Clinical.Psychology == 1,], FUN = sum), by = "year")

clinical.sum$percentage.marginal <- 100*(clinical.sum$marginal/clinical.sum$result)

clinical.sum$Journals <- rep(NA, dim(clinical.sum)[1])
for (year in seq_along(clinical.sum$year)) {
  if(length(unique(dat$journal[dat$year == clinical.sum$year[year] & dat$Clinical.Psychology == 1])) < 5) 
  {
    clinical.sum$Journals[year] <- 1
  }
  else 
  {
    ifelse(length(unique(dat$journal[dat$year == clinical.sum$year[year] & dat$Clinical.Psychology == 1])) < 11, 
              clinical.sum$Journals[year] <- 2, 
              clinical.sum$Journals[year] <- 3)
  }
}

#Cognitive psychology
neuroscience.sum <- merge(aggregate(result ~ year, data = dat[dat$Neuroscience...Cognition == 1,], FUN = length),
                          aggregate(marginal ~ year, data = dat[dat$Neuroscience...Cognition == 1,], FUN = sum), by = "year")

neuroscience.sum$percentage.marginal <- 100*(neuroscience.sum$marginal/neuroscience.sum$result)

neuroscience.sum$Journals <- rep(NA, dim(neuroscience.sum)[1])
for (year in seq_along(neuroscience.sum$year)) {
  if(length(unique(dat$journal[dat$year == neuroscience.sum$year[year] & dat$Neuroscience...Cognition == 1])) < 5) 
  {
    neuroscience.sum$Journals[year] <- 1
  }
  else 
  {
    ifelse(length(unique(dat$journal[dat$year == neuroscience.sum$year[year] & dat$Neuroscience...Cognition == 1])) < 11, 
              neuroscience.sum$Journals[year] <- 2, 
              neuroscience.sum$Journals[year] <- 3)
  }
}

#Developmental psychology
developmental.sum <- merge(aggregate(result ~ year, data = dat[dat$Developmental.Psychology == 1,], FUN = length),
                           aggregate(marginal ~ year, data = dat[dat$Developmental.Psychology == 1,], FUN = sum), by = "year")

developmental.sum$percentage.marginal <- 100*(developmental.sum$marginal/developmental.sum$result)

developmental.sum$Journals <- rep(NA, dim(developmental.sum)[1])
for (year in seq_along(developmental.sum$year)) {
  if(length(unique(dat$journal[dat$year == developmental.sum$year[year] & dat$Developmental.Psychology== 1])) < 5) 
  {
    developmental.sum$Journals[year] <- 1
  }
  else 
  {
    ifelse(length(unique(dat$journal[dat$year == developmental.sum$year[year] & dat$Developmental.Psychology == 1])) < 11,
              developmental.sum$Journals[year] <- 2, 
              developmental.sum$Journals[year] <- 3)
  }
}

#Educational psychology
educational.sum <- merge(aggregate(result ~ year, data = dat[dat$Educational.Psychology..School.Psychology...Training == 1,], FUN = length),
                         aggregate(marginal ~ year, data = dat[dat$Educational.Psychology..School.Psychology...Training == 1,], FUN = sum), by = "year")

educational.sum$percentage.marginal <- 100*(educational.sum$marginal/educational.sum$result)

educational.sum$Journals <- rep(NA, dim(educational.sum)[1])
for (year in seq_along(educational.sum$year)) {
  if(length(unique(dat$journal[dat$year == educational.sum$year[year] & dat$Educational.Psychology..School.Psychology...Training == 1])) < 5)
  {
    educational.sum$Journals[year] <- 1
  }
  else 
  {
    ifelse(length(unique(dat$journal[dat$year == educational.sum$year[year] & dat$Educational.Psychology..School.Psychology...Training == 1])) < 11, 
           educational.sum$Journals[year] <- 2, 
                educational.sum$Journals[year] <- 3)
  }
}

#Experimental psychology
experimental.sum <- merge(aggregate(result ~ year, data = dat[dat$Basic...Experimental.Psychology == 1,], FUN = length), 
                          aggregate(marginal ~ year, data = dat[dat$Basic...Experimental.Psychology == 1,], FUN = sum), by = "year")

experimental.sum$percentage.marginal <- 100*(experimental.sum$marginal/experimental.sum$result)

experimental.sum$Journals <- rep(NA, dim(educational.sum)[1])
for (year in seq_along(educational.sum$year)) {
  if(length(unique(dat$journal[dat$year == educational.sum$year[year] & dat$Basic...Experimental.Psychology == 1])) < 5) 
  {
    experimental.sum$Journals[year] <- 1
  }
  else 
  {ifelse(length(unique(dat$journal[dat$year == educational.sum$year[year] & dat$Basic...Experimental.Psychology == 1])) < 11, 
          experimental.sum$Journals[year] <- 2, 
                experimental.sum$Journals[year] <- 3)
  }
}

#forensic psychology
forensic.sum <- merge(aggregate(result ~ year, data = dat[dat$Forensic.Psychology == 1,], FUN = length),
                      aggregate(marginal ~ year, data = dat[dat$Forensic.Psychology == 1,], FUN = sum), by = "year")

forensic.sum$percentage.marginal <- 100*(forensic.sum$marginal/forensic.sum$result)

forensic.sum$Journals <- rep(NA, dim(forensic.sum)[1])
for (year in seq_along(forensic.sum$year)) {
  if(length(unique(dat$journal[dat$year == forensic.sum$year[year] & dat$Forensic.Psychology == 1])) < 5) 
  {
    forensic.sum$Journals[year] <- 1
  }
  else 
  {
    ifelse (length(unique(dat$journal[dat$year == forensic.sum$year[year] & dat$Forensic.Psychology == 1])) < 11, 
            forensic.sum$Journals[year] <- 2, 
              forensic.sum$Journals[year] <- 3)
  }
}

#Health psychology
health.sum <- merge(aggregate(result ~ year, data = dat[dat$Health.Psychology...Medicine == 1,], FUN = length),
                    aggregate(marginal ~ year, data = dat[dat$Health.Psychology...Medicine == 1,], FUN = sum), by = "year")

health.sum$percentage.marginal <- 100*(health.sum$marginal/health.sum$result)

health.sum$Journals <- rep(NA, dim(health.sum)[1])
for (year in seq_along(health.sum$year)) {
  if(length(unique(dat$journal[dat$year == health.sum$year[year] & dat$Health.Psychology...Medicine == 1])) < 5) 
  {
    health.sum$Journals[year] <- 1
  }
  else 
  {
    ifelse(length(unique(dat$journal[dat$year == health.sum$year[year] & dat$Health.Psychology...Medicine == 1])) < 11,
           health.sum$Journals[year] <- 2, 
            health.sum$Journals[year] <- 3)
  }
}

#Industrial/organizational psychology
organizational.sum <- merge(aggregate(result ~ year, data = dat[dat$Industrial.Organizational.Psychology...Management == 1,], FUN = length),
                            aggregate(marginal ~ year, data = dat[dat$Industrial.Organizational.Psychology...Management == 1,], FUN = sum), by = "year")

organizational.sum$percentage.marginal <- 100*(organizational.sum$marginal/organizational.sum$result)

organizational.sum$Journals <- rep(NA, dim(organizational.sum)[1])
for (year in seq_along(organizational.sum$year)) {
  if(length(unique(dat$journal[dat$year == organizational.sum$year[year] & dat$Industrial.Organizational.Psychology...Management == 1])) < 5) 
  {
    organizational.sum$Journals[year] <- 1
  }
  else 
  {
    ifelse(length(unique(dat$journal[dat$year == organizational.sum$year[year] & dat$Industrial.Organizational.Psychology...Management == 1])) < 11, 
          organizational.sum$Journals[year] <- 2, 
                organizational.sum$Journals[year] <- 3)
  }
}


#Social psychology
social.sum <- merge(aggregate(result ~ year, data = dat[dat$Social.Psychology...Social.Processes == 1,], FUN = length), 
                    aggregate(marginal ~ year, data = dat[dat$Social.Psychology...Social.Processes == 1,], FUN = sum), by = "year")

social.sum$percentage.marginal <- 100*(social.sum$marginal/social.sum$result)

social.sum$Journals <- rep(NA, dim(social.sum)[1])
for (year in seq_along(social.sum$year)) {
  if(length(unique(dat$journal[dat$year == social.sum$year[year] & dat$Social.Psychology...Social.Processes == 1])) < 5) 
  {
    social.sum$Journals[year] <- 1
  }
  else 
  {
      ifelse(length(unique(dat$journal[dat$year == social.sum$year[year] & dat$Social.Psychology...Social.Processes == 1])) < 11, 
             social.sum$Journals[year] <- 2, 
                social.sum$Journals[year] <- 3)
  }
}


#Combine into one dataframe for graphing
results.sum <- combine(dat.sum, clinical.sum, neuroscience.sum, developmental.sum, educational.sum, experimental.sum, 
                       forensic.sum, health.sum, organizational.sum, social.sum)
results.sum$Journals <- factor(results.sum$Journals, labels = c("< 5", "5 - 10", "> 10"))
results.sum$source <- factor(results.sum$source, labels = c("All APA Journals", "Clinical", "Cognitive", "Developmental", "Educational",
                                                            "Experimental", "Forensic", "Health", "Organizational", "Social"))

#-------------------------------------------------------
##Linear models and plots
#-------------------------------------------------------
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(plyr)){install.packages("plyr")}
library(plyr)
library(ggplot2)

#Function for statistics to label graphs with

lm_eqn = function(df){
  m = lm(percentage.marginal ~ year, df);
  eq <- substitute(italic(b) == beta %.%","~~italic(r)^2~"="~r2, 
                   list(beta = round(coef(m)[2], digits = 2), 
                        r2 = round(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}

#Plot1

eq <- ddply(replication.sum, .(source), lm_eqn)

highlight <- replication.sum[replication.sum$year == 1990 | replication.sum$year == 2000 | replication.sum$year == 2010,]

p <- ggplot(replication.sum, aes(x = year, y = percentage.marginal)) +
  geom_point()+
  geom_point(data = highlight, shape = 21, color = "red", size = 5) +
  geom_line() +
  theme(strip.text = element_text(face = "bold"), 
        axis.title = element_text(size = 9), 
        axis.text = element_text(size = 9), 
        panel.background = element_rect(fill = "grey93"),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

p1 <- p + geom_label(data = eq, aes(label = V1), size = 3, x = 1985, y = Inf, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
                     label.padding = unit(0.12, "lines"), parse = TRUE, inherit.aes = FALSE) +
  facet_wrap(~source) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
  scale_y_continuous(limits = c(0,100),
                     name = expression(paste("% of .05 < ",italic("p"), " <= .1 reported as marginally significant")))

#plot2

eq <- ddply(results.sum, .(source), lm_eqn)

p <- ggplot(results.sum, aes(x = year, y = percentage.marginal)) +
  geom_point(aes(color = Journals)) +
  scale_color_manual(values = c("#66C2A5", "#8DA0CB", "#A6D854")) +
  geom_line() +
  theme(strip.text = element_text(face = "bold"), 
        axis.title = element_text(size = 9), 
        axis.text = element_text(size = 9), 
        panel.background = element_rect(fill = "grey93"),
        strip.background = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

p2 <- p + geom_label(data = eq, aes(label = V1), size = 3, x = 1985, y = Inf, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
                     label.padding = unit(0.12, "lines"), parse = TRUE, inherit.aes = FALSE) +
  facet_wrap(~source, scales = "free_x", ncol = 3) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
  scale_y_continuous(limits = c(0,100),
                     name = expression(paste("% of .05 < ",italic("p"), " <= .1 reported as marginally significant")))
