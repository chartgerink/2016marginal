##Analysis of data - marginally significant results in APA-journals: https://osf.io/28gxz/

#----------------------------------

#Load dataset
dat <- read.csv("../data/final_marginal_dataset.csv", stringsAsFactors = FALSE)
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
replication.sum$source <- factor(replication.sum$source, labels = c("DP", "JPSP"))

#---------------------------------------------------------------------
##Dataframes with proportion of marginal results per year for different subfields, and a variable for color coding
#---------------------------------------------------------------------

#Overall
dat.sum <- merge(aggregate(result ~ year, data = dat, FUN = length), 
                 aggregate(marginal ~ year, data = dat, FUN = sum), by = "year")

dat.sum$percentage.marginal <- 100*(dat.sum$marginal/dat.sum$result)

#clinical psychology
clinical.sum <- merge(aggregate(result ~ year, data = dat[dat$Clinical.Psychology == 1,], FUN = length), 
                      aggregate(marginal ~ year, data = dat[dat$Clinical.Psychology == 1,], FUN = sum), by = "year")

clinical.sum$percentage.marginal <- 100*(clinical.sum$marginal/clinical.sum$result)

#Cognitive psychology
neuroscience.sum <- merge(aggregate(result ~ year, data = dat[dat$Neuroscience...Cognition == 1,], FUN = length),
                          aggregate(marginal ~ year, data = dat[dat$Neuroscience...Cognition == 1,], FUN = sum), by = "year")

neuroscience.sum$percentage.marginal <- 100*(neuroscience.sum$marginal/neuroscience.sum$result)

#Developmental psychology
developmental.sum <- merge(aggregate(result ~ year, data = dat[dat$Developmental.Psychology == 1,], FUN = length),
                           aggregate(marginal ~ year, data = dat[dat$Developmental.Psychology == 1,], FUN = sum), by = "year")

developmental.sum$percentage.marginal <- 100*(developmental.sum$marginal/developmental.sum$result)

#Educational psychology
educational.sum <- merge(aggregate(result ~ year, data = dat[dat$Educational.Psychology..School.Psychology...Training == 1,], FUN = length),
                         aggregate(marginal ~ year, data = dat[dat$Educational.Psychology..School.Psychology...Training == 1,], FUN = sum), by = "year")

educational.sum$percentage.marginal <- 100*(educational.sum$marginal/educational.sum$result)

#Experimental psychology
experimental.sum <- merge(aggregate(result ~ year, data = dat[dat$Basic...Experimental.Psychology == 1,], FUN = length), 
                          aggregate(marginal ~ year, data = dat[dat$Basic...Experimental.Psychology == 1,], FUN = sum), by = "year")

experimental.sum$percentage.marginal <- 100*(experimental.sum$marginal/experimental.sum$result)

#forensic psychology
forensic.sum <- merge(aggregate(result ~ year, data = dat[dat$Forensic.Psychology == 1,], FUN = length),
                      aggregate(marginal ~ year, data = dat[dat$Forensic.Psychology == 1,], FUN = sum), by = "year")

forensic.sum$percentage.marginal <- 100*(forensic.sum$marginal/forensic.sum$result)

#Health psychology
health.sum <- merge(aggregate(result ~ year, data = dat[dat$Health.Psychology...Medicine == 1,], FUN = length),
                    aggregate(marginal ~ year, data = dat[dat$Health.Psychology...Medicine == 1,], FUN = sum), by = "year")

health.sum$percentage.marginal <- 100*(health.sum$marginal/health.sum$result)

#Industrial/organizational psychology
organizational.sum <- merge(aggregate(result ~ year, data = dat[dat$Industrial.Organizational.Psychology...Management == 1,], FUN = length),
                            aggregate(marginal ~ year, data = dat[dat$Industrial.Organizational.Psychology...Management == 1,], FUN = sum), by = "year")

organizational.sum$percentage.marginal <- 100*(organizational.sum$marginal/organizational.sum$result)

#Social psychology
social.sum <- merge(aggregate(result ~ year, data = dat[dat$Social.Psychology...Social.Processes == 1,], FUN = length), 
                    aggregate(marginal ~ year, data = dat[dat$Social.Psychology...Social.Processes == 1,], FUN = sum), by = "year")

social.sum$percentage.marginal <- 100*(social.sum$marginal/social.sum$result)

#Combine into one dataframe for graphing
results.sum <- combine(dat.sum, clinical.sum, neuroscience.sum, developmental.sum, educational.sum, experimental.sum, 
                       forensic.sum, health.sum, organizational.sum, social.sum)
results.sum$source <- factor(results.sum$source, labels = c("All APA Journals", "Clinical", "Cognitive", "Developmental", "Educational",
                                                            "Experimental", "Forensic", "Health", "Organizational", "Social"))
results.not.all <- subset(results.sum, source != "All APA Journals")
#-------------------------------------------------------
##Linear models and plots
#-------------------------------------------------------
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(plyr)){install.packages("plyr")}
if(!require(viridis)){install.packages("viridis")}
if(!require(scales)){install.packages("scales")}
library(plyr)
library(ggplot2)
library(viridis)
library(scales)
library(grid)

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
  geom_point(aes(color = result)) +
  scale_color_viridis(option = "viridis", direction = -1, limits = c(0, max(replication.sum$result))) +
  geom_point(data = highlight, shape = 21, color = "red", size = 5) +
  geom_line() +
  guides(color = guide_colorbar(reverse = F, title = expression(paste(italic("p"),"-values")))) +
  theme(strip.text = element_text(face = "bold"), 
        axis.title = element_text(size = 9), 
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        panel.background = element_rect(fill = "grey93"),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

p1 <- p + geom_label(data = eq, aes(label = V1), size = 3, x = 1985, y = Inf, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
                     label.padding = unit(0.12, "lines"), parse = TRUE, inherit.aes = FALSE) +
  facet_wrap(~source) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
  scale_y_continuous(name = expression(paste("% of .05 < ",italic("p"), " <= .1 reported as marginally significant")))

#plot2

eq <- ddply(results.sum, .(source), lm_eqn)

cutoff <- max(results.not.all$result)

p <- ggplot(results.sum, aes(x = year, y = percentage.marginal)) +
  geom_point(aes(color = result)) +
  geom_line() +
  scale_color_viridis(limits = c(0, cutoff), option = "viridis", direction = -1, na.value = "#440154FF") +
  guides(color = guide_colorbar(title = expression(paste(italic("p"),"-values")))) +
  theme(strip.text = element_text(face = "bold"), 
        axis.title = element_text(size = 9), 
        axis.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        panel.background = element_rect(fill = "grey93"),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

p2 <- p + geom_label(data = eq, aes(label = V1), size = 3, x = 1985, y = Inf, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
                     label.padding = unit(0.12, "lines"), parse = TRUE, inherit.aes = FALSE) +
  facet_wrap(~source, scales = "free", ncol = 3) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
  scale_y_continuous(limits = c(0,100),
                     name = expression(paste("% of .05 < ",italic("p"), " <= .1 reported as marginally significant")))

#Move 'social' plot to center of bottom row
g <- ggplotGrob(p2)

g$layout[grepl("panel-1-2", g$layout$name), c("l","r")] <- g$layout[grepl("panel-1-3", g$layout$name), c("l","r")] 
g$layout[grepl("axis-l-4-1", g$layout$name), c("l","r")] <- g$layout[grepl("axis-l-2-2", g$layout$name), c("l","r")] 
g$layout[grepl("axis-b-1-4", g$layout$name), c("l","r")] <- g$layout[grepl("axis-b-2-2", g$layout$name), c("l","r")] 
g$layout[grepl("strip-t-1-4", g$layout$name), c("l","r")] <- g$layout[grepl("strip-t-2-2", g$layout$name), c("l","r")] 

grid.newpage()
grid.draw(g)