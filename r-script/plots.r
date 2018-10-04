##Analysis of data - marginally significant results in APA-journals: https://osf.io/28gxz/

#***********************************
#p-value plots----
#***********************************

#----------------------------------
#Setup
#----------------------------------
if(!require(gdata)){install.packages("gdata")}
library(gdata)

#Load p-value (.05 - .1) dataset
dat <- read.csv("../data/final_marginal_dataset.csv", stringsAsFactors = FALSE)

#Load dataset with all p-values
dat2 <- read.csv("../data/marginal_dataset_with_subfields.csv", stringsAsFactors = FALSE)

#Exclude entries unique to the topic 'core of psychology' from full dataset
dat2 <- dat2[dat2$Social.Psychology...Social.Processes == 1 | dat2$Neuroscience...Cognition == 1 | dat2$Industrial.Organizational.Psychology...Management == 1 |
               dat2$Health.Psychology...Medicine == 1 | dat2$Forensic.Psychology  == 1 | dat2$Educational.Psychology..School.Psychology...Training == 1 |
               dat2$Developmental.Psychology == 1 | dat2$Clinical.Psychology == 1 | dat2$Basic...Experimental.Psychology == 1,]

#Create dataset at the article level with marginal indicator in 4 steps
#1) merge full with restricted dataset to get dummy for marginal significance
dat3 <- merge(dat, dat2, all.y = TRUE)

#2) Switch NA-values to zeroes (i.e. for all values not in the .05-.1 range)
dat3$marginal <- ifelse(is.na(dat3$marginal), 0, dat3$marginal)

#3)Aggregate to the article level, keeping all variables of interest
dat3 <- aggregate(marginal ~ doi + journal + year + Social.Psychology...Social.Processes + Basic...Experimental.Psychology +
                    Clinical.Psychology  + Developmental.Psychology + Educational.Psychology..School.Psychology...Training +
                    Forensic.Psychology + Health.Psychology...Medicine + Industrial.Organizational.Psychology...Management +
                    Neuroscience...Cognition, data = dat3, FUN = sum)

#4)create a dummy to indicate if an article contains at least one marginally significant p-value (.05 - .1)
dat3$marginal <- ifelse(dat3$marginal > 0, 1, 0) 
names(dat3)[names(dat3) == "marginal"] <- "a.marginal"


#For all dataframes drop redundant variables and reorder columns to facilitate aggregation in next section
dat <- dat[, !(names(dat) %in% c("Core.of.Psychology", "doi", "pre", "post", "comparison", "value"))]
dat2 <- dat2[, !(names(dat2) %in% c("Core.of.Psychology", "doi", "pre", "post", "comparison", "value"))]
dat <- dat[, c(4:12, 1:3, 13)] 
dat2 <- dat2[, c(4:12, 1:3)]
dat3 <- dat3[, c(4:12, 1:3, 13)]
names(dat2)[names(dat2) == "result"] <- "all.result" #name-change to avoid later confusion

#---------------------------------------------------------------------
##Dataframes with proportion of marginal results (p-values&articles) per year for JPSP and 'Developmental Psychology'
#---------------------------------------------------------------------
jpsp.sum <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                   list(aggregate(result ~ year, data = dat[dat$journal == "Journal of Personality and Social Psychology",], FUN = length),
                        aggregate(marginal ~ year, data = dat[dat$journal == "Journal of Personality and Social Psychology",], FUN = sum),
                        aggregate(doi ~ year, data = dat3[dat3$journal == "Journal of Personality and Social Psychology",], FUN = length),
                        aggregate(a.marginal ~ year, data = dat3[dat3$journal == "Journal of Personality and Social Psychology",], FUN = sum)))

jpsp.sum$percentage.marginal <- 100*(jpsp.sum$marginal/jpsp.sum$result)
jpsp.sum$a.percentage.marginal <- 100*(jpsp.sum$a.marginal/jpsp.sum$doi)

dp.sum <- Reduce(function(x, y) merge(x, y, all=TRUE),
                 list(aggregate(result ~ year, data = dat[dat$journal == "Developmental Psychology",], FUN = length),
                      aggregate(marginal ~ year, data = dat[dat$journal == "Developmental Psychology",], FUN = sum),
                      aggregate(doi ~ year, data = dat3[dat3$journal == "Developmental Psychology",], FUN = length),
                      aggregate(a.marginal ~ year, data = dat3[dat3$journal == "Developmental Psychology",], FUN = sum)))

dp.sum$percentage.marginal <- 100*(dp.sum$marginal/dp.sum$result)
dp.sum$a.percentage.marginal <- 100*(dp.sum$a.marginal/dp.sum$doi)

#Combine into one dataframe for graphing

replication.sum <- combine(dp.sum, jpsp.sum)
replication.sum$source <- factor(replication.sum$source, labels = c("DP", "JPSP"))

#---------------------------------------------------------------------
##Dataframes with proportion of marginal p-values (.05 - .1) and articles (>0 marg. p-values) per year for different subfields
#---------------------------------------------------------------------
#aggregated variable meanings:
#{result = #p-values between .05 and .1, marginal = #marginally sig. results,  doi = #of articles, all.result = #p-values full range,
# a.marginal = #of articles containing at least one marg. sig. p-value between .05 and .1}


#Overall

dat.sum <- Reduce(function(x, y) merge(x, y, all=TRUE), list(aggregate(all.result ~ year, data = dat2, FUN = length),
                                                             aggregate(result ~ year, data = dat, FUN = length),
                                                             aggregate(marginal ~ year, data = dat, FUN = sum), 
                                                             aggregate(doi ~ year, data = dat3, FUN = length),
                                                             aggregate(a.marginal ~ year, data = dat3, FUN = sum)))

dat.sum$percentage.marginal <- 100*(dat.sum$marginal/dat.sum$result)
dat.sum$a.percentage.marginal <- 100*(dat.sum$a.marginal/dat.sum$doi)
dat.sum$source <- "All APA Journals" 

#By subfield

collect <- vector("list", length = 9)

for(i in 1:9) { #loop over all subfields
  collect[[i]] <- Reduce(function(x, y) merge(x, y, all=TRUE),
                         list(aggregate(all.result ~ year, data = dat2[dat2[[i]] == 1,], FUN = length),
                              aggregate(result ~ year, data = dat[dat[[i]] == 1,], FUN = length),
                              aggregate(marginal ~ year, data = dat[dat[[i]] == 1,], FUN = sum), 
                              aggregate(doi ~ year, data = dat3[dat3[[i]] == 1,], FUN = length),
                              aggregate(a.marginal ~ year, data = dat3[dat3[[i]] == 1,], FUN = sum)))

  collect[[i]]$percentage.marginal <- 100*(collect[[i]]$marginal / collect[[i]]$result)
  collect[[i]]$a.percentage.marginal <- 100*(collect[[i]]$a.marginal / collect[[i]]$doi)
}

results.sum <- do.call("rbind", collect) #Combine into one dataframe

results.sum$source <- rep(c("Social", "Experimental", "Clinical", "Developmental", "Educational",
  "Forensic", "Health","Organizational", "Cognitive"), sapply(collect, nrow)) #add data providence to new dataframe

#Combine overall and by subfield for plotting
results.sum <- rbind(results.sum, dat.sum)
results.sum$source <- factor(results.sum$source) #prepare for plotting


 
#-------------------------------------------------------
##Linear models and plots
#-------------------------------------------------------
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(plyr)){install.packages("plyr")}
if(!require(cowplot)){install.packages("cowplot")}
library(ggplot2)
library(plyr)
library(cowplot)

#Function for statistics to label graphs with

lm_eqn = function(df){
  m = lm(percentage.marginal ~ year, df);
  eq <- substitute(paste(italic(p), "-values: ", italic(b) == beta), 
                   list(beta = round(coef(m)[[2]], digits = 2)))
  as.character(as.expression(eq));                
}

lm_eqn2 = function(df){
  m = lm(a.percentage.marginal ~ year, df);
  eq <- substitute(paste("articles: ", italic(b) == beta), 
                   list(beta = round(coef(m)[[2]], digits = 2)))
  as.character(as.expression(eq));                 
}

#------------------------------------------------
#Plots of JPSP and DP 
#------------------------------------------------
#marginal significance over time p-values & articles

eq <- ddply(replication.sum, .(source), lm_eqn)
eq2 <- ddply(replication.sum, .(source), lm_eqn2)

#controls location of labels in combination with geom_text
replication.sum$pmlabel <- ifelse(replication.sum$year == 2014, "p-values", NA)
replication.sum$amlabel <- ifelse(replication.sum$year == 2014, "articles", NA)

#Base for facet_wrap
p <- ggplot(replication.sum, aes(x = year, y = percentage.marginal)) +
  geom_line(linetype = "solid") +
  geom_text(aes(y = percentage.marginal + 15, x = 2005, label = pmlabel), size = 3.5, na.rm= TRUE) +
  geom_line(aes(x = year, y = a.percentage.marginal), linetype = "dashed") +
  geom_text(aes(y = a.percentage.marginal - 15, x = 2005, label = amlabel), size = 3.5, na.rm= TRUE) +
  annotate("rect", xmin = -Inf, ymin = 95, xmax = 1998, ymax = Inf, alpha = .2, fill = "transparent", color = "black") +
  theme(strip.text = element_text(face = "bold"), 
        axis.title.y = element_text(size = 12), 
        axis.text.y = element_text(size = 9),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

#Facet wrap by journal
p2 <- p + geom_text(data = eq2, aes(label = V1), size = 3.5, x = 1985 - 1, y = 99, hjust = 0, vjust = 1, parse = TRUE, inherit.aes = FALSE) + 
  geom_text(data = eq, aes(label = V1), size = 3.5, x = 1985 - 1, y = 100 + 4, hjust = 0, vjust = 1, parse = TRUE, inherit.aes = FALSE)  +
  facet_wrap(~source, ncol = 3) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
  scale_y_continuous(sec.axis = dup_axis(name = "", breaks = NULL, labels = NULL), limits = c(0,100),
                     name = "% reported as marginally significant")

#Add plot of p (.05 - .1) over time for JPSP and DP averaged over 2 years because JPSP very jagged
replication.sum$years2 <- rep(1:16, each = 2) #averaging
replication.sum$years2avg <- rep(aggregate(result ~ years2 + source, data = replication.sum, FUN = mean)$result, each = 2)

#outcome variable different, hence different function
lm_eqn.p = function(df){
  m = lm(result ~ year, df);
  eq <- substitute(paste(italic(b) == beta), 
                   list(beta = round(coef(m)[[2]], digits = 2)))
  as.character(as.expression(eq));                
}

eq.p <- ddply(replication.sum, .(source), lm_eqn.p)

#Base plot for faceting
p.over.time <- ggplot(replication.sum, aes(x = year, y = years2avg)) +
  geom_area(linetype = "solid") +
  annotate("rect", xmin = -Inf, ymin = 355, xmax = 1991, ymax = Inf, alpha = .2, fill = "transparent", color = "black") +
  theme(strip.text = element_text(face = "bold"), 
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 9),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

#Facet wrap by journal
p2.over.time <- p.over.time + 
  geom_text(data = eq.p, aes(label = V1), size = 3.5, x = 1985 - 1, y = 390, 
            hjust = 0, vjust = 1, parse = TRUE, inherit.aes = FALSE)  +
  facet_wrap(~source, ncol = 2) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
  scale_y_continuous(sec.axis = dup_axis(name = "", breaks = NULL, labels = NULL),
                     name = expression(paste("# ", italic("p"), "-values reported (.05 - .1)")))

#Combine the plot for marginal significance and number of p-values over time
combo <- ggdraw() +
  draw_plot(p2, x = 0, y = .3, width = 1, height = .7) +
  draw_plot(p2.over.time, x = 0, y = 0, width = 1, height = .325)

#save_plot("../figures/jpsp-dp.png", plot = combo, base_width = 7, base_height = 7, dpi = 600)

#------------------------------------------------
#Plot of disciplines and all journals
#------------------------------------------------

#marginally significant results over time
results.sum$pmlabel <- ifelse(results.sum$year == 2016, "p-values", NA)
results.sum$amlabel <- ifelse(results.sum$year == 2016, "articles", NA)

eq <- ddply(results.sum, .(source), lm_eqn)
eq2 <- ddply(results.sum, .(source), lm_eqn2)

#base for facet wrap
p <- ggplot(results.sum, aes(x = year, y = percentage.marginal)) +
  geom_line(linetype = "solid") +
  geom_text(aes(y = 52, x = 2009, label = pmlabel), size = 2.5, na.rm= TRUE) +
  geom_line(aes(x = year, y = a.percentage.marginal), linetype = "dashed") +
  geom_text(aes(y = 6, x = 2009, label = amlabel), size = 2.5, na.rm= TRUE) +
  annotate("rect", xmin = -Inf, ymin = 75, xmax = 2007, ymax = Inf, alpha = .2, fill = "transparent", color = "black") +
  theme(strip.text = element_text(face = "bold"), 
        axis.title = element_text(size = 9), 
        axis.text = element_text(size = 9),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

#Facet wrap by discipline
p2 <- p + geom_text(data = eq2, aes(label = V1), size = 3, x = 1985 - 1, y = 90, hjust = 0, vjust = 1, parse = TRUE, inherit.aes = FALSE) + 
  geom_text(data = eq, aes(label = V1), size = 3, x = 1985 - 1, y = 100 + 4, hjust = 0, vjust = 1, parse = TRUE, inherit.aes = FALSE)  +
  facet_wrap(~source, scales = "free", ncol = 3) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
  scale_y_continuous(sec.axis = dup_axis(name = "", breaks = NULL, labels = NULL), limits = c(0,100),
                     name = "% reported as marginally significant")

#Move 'social' plot to center of bottom row
if(!require(grid)){install.packages("grid")}
library(grid)

g <- ggplotGrob(p2)

g$layout[grepl("panel-1-2", g$layout$name), c("l","r")] <- g$layout[grepl("panel-1-3", g$layout$name), c("l","r")] 
g$layout[grepl("axis-l-4-1", g$layout$name), c("l","r")] <- g$layout[grepl("axis-l-2-2", g$layout$name), c("l","r")] 
g$layout[grepl("axis-b-1-4", g$layout$name), c("l","r")] <- g$layout[grepl("axis-b-2-2", g$layout$name), c("l","r")] 
g$layout[grepl("strip-t-1-4", g$layout$name), c("l","r")] <- g$layout[grepl("strip-t-2-2", g$layout$name), c("l","r")] 

grid.newpage()
grid.draw(g)

#ggsave("../figures/disciplines.png", plot = g, width = 7, height = 7, dpi = 600)

#***********************************
#df plot----
#***********************************
#library(ggrepel)
#library(plyr)
#library(ggplot2)

df <- read.csv("../data/final_df_dataset.csv", stringsAsFactors = FALSE)

#Function for labelling solo figures
lm_eqn3 = function(stat, df){
  m = lm(stat ~ year, df);
  eq <- substitute(paste("All APA Journals: ", italic(b) == beta), 
                   list(beta = round(coef(m)[[2]], digits = 2)))
  as.character(as.expression(eq));                
}



# drop redundant variables and reorder columns to facilitate aggregation
df <- df[, !(names(df) %in% c("Core.of.Psychology", "journal", "DOI", "Statistic", "id"))]
df <- df[, c(3:11, 1, 2)]

#Aggregate overall

df.sum <- aggregate(df2 ~ year, data = df, FUN = median)

df.sum$source <- "All APA Journals"

df.sum$years2avg <- df.sum$df2 #This is to be able to plot overall together with disciplines but w/o averaging it

#By subfield

df.collect <- vector("list", length = 9)

for(i in 1:9) { #loop over all subfields
  df.collect[[i]] <- aggregate(df2 ~ year, data = df[df[[i]] == 1,], FUN = median)
}

results.df <- do.call("rbind", df.collect) #Combine into one dataframe

results.df$source <- rep(c("Social", "Experimental", "Clinical", "Developmental", "Educational",
                            "Forensic", "Health","Organizational", "Cognitive"), sapply(df.collect, nrow)) #add data providence to new dataframe

results.df$years2 <- rep(1:16, each = 2) #averaging per two years because very jagged lines
results.df$years2avg <- rep(aggregate(df2 ~ years2 + source, data = results.df, FUN = mean)$df2, each = 2)
results.df$years2 <- NULL #remove to be able to rbind with overall dataframe

#Combine overall and by discipline for plotting
results.df<- rbind(results.df, df.sum)
results.df$source <- factor(results.df$source) #prepare for plotting

df.eq <- lm_eqn3(results.df$df2, results.df) #linear model

results.df$plabel <- ifelse(results.df$year == 2015, as.character(results.df$source), NA) #used for labelling at endpoints, because # of datapoints must match

plot.df <- ggplot(results.df, aes(x = year, y = years2avg, group = source, alpha = source)) +
  geom_line() +
  annotate("label", x = -Inf, y = Inf, label = df.eq, parse = TRUE, size = 3.5, hjust = 0, vjust = 1) +
  geom_text_repel(aes(label = plabel), nudge_x = 3.5, na.rm = TRUE, size = 3, segment.alpha = 0.3) +
  scale_alpha_manual(guide = FALSE, values = c(1, rep(0.3, 9))) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(xlim = c(1985, 2016 + 4)) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
  scale_y_continuous(name = "Median degrees of freedom", breaks = c(20, 70, 120, 170)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 9))

# ggsave("../figures/df_plot.png", plot = plot.df, width = 7, height = 7, dpi = 600)

#***********************************
#Additional alternative plots----
#***********************************
#number of p-values between .05 and .1
if(!require(ggrepel)){install.packages("ggrepel")}
library(ggrepel)

p3.eq <- lm_eqn3(results.sum$result, results.sum) #linear model, equation from line 291

results.sum$plabel <- ifelse(results.sum$year == 2015, as.character(results.sum$source), NA) #used for labelling at endpoints, because # of datapoints must match

p3 <- ggplot(results.sum, aes(x = year, y = result, group = source, color = source)) +
  geom_line() +
  annotate("label", x = -Inf, y = Inf, label = p3.eq, parse = TRUE, size = 3, hjust = 0, vjust = 1) +
  geom_text_repel(aes(label = plabel), nudge_x = 3.5, na.rm = TRUE, size = 3, segment.alpha = 0.3) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(xlim = c(1985, 2016 + 4)) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) + 
  scale_y_continuous(name = expression(paste("Frequencies .05 < ", italic(p)," <= .1")),
                     breaks = c(0, 1000, 2000)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"),
        axis.title = element_text(size = 9), 
        axis.text = element_text(size = 9))

#total number of p-values

p4.eq <- lm_eqn3(results.sum$all.result, results.sum) #linear model

p4 <- ggplot(results.sum, aes(x = year, y = all.result, group = source, color = source)) +
  geom_line() +
  annotate("label", x = -Inf, y = Inf, label = p4.eq, parse = TRUE, size = 3, hjust = 0, vjust = 1) +
  geom_text_repel(aes(label = plabel), nudge_x = 3.5, na.rm = TRUE, size = 3, segment.alpha = 0.3) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(xlim = c(1985, 2016 + 4)) +
  scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) + 
  scale_y_continuous(name = expression(paste("Frequencies all ", italic(p),"-values")),
                     breaks = c(0, 20000, 40000)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"),
        axis.title = element_text(size = 9), 
        axis.text = element_text(size = 9))

#Combine the two above plots
if(!require("cowplot")){install.packages("cowplot")}

res <- cowplot::plot_grid(p4, p3, labels = "AUTO", ncol = 1)
#save_plot("p-values.pdf", res, ncol = 1, nrow = 2, base_aspect_ratio = 1.4, base_height = 5)


