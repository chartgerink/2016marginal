##Analysis of test-sample FYP 2017

#----------------------------------

#Load dataset
dat <- read.csv("../data/test_sample_marginal_dataset.csv", stringsAsFactors = FALSE)

#---------------------------------------------------------------------
##Datasets with proportion of marginal results per year for different subfield
#---------------------------------------------------------------------
#Overall
dat.sum <- merge(aggregate(result ~ year, data = dat, FUN = length), 
                 aggregate(marginal ~ year, data = dat, FUN = sum), by = "year")

dat.sum$percentage.marginal <- 100*(dat.sum$marginal/dat.sum$result)

#Social psychology
social.sum <- merge(aggregate(result ~ year, data = dat[dat$Social.Psychology...Social.Processes == 1,], FUN = length), 
                    aggregate(marginal ~ year, data = dat[dat$Social.Psychology...Social.Processes == 1,], FUN = sum), by = "year")

social.sum$percentage.marginal <- 100*(social.sum$marginal/social.sum$result)

#Experimental psychology
experimental.sum <- merge(aggregate(result ~ year, data = dat[dat$Basic...Experimental.Psychology == 1,], FUN = length), 
                          aggregate(marginal ~ year, data = dat[dat$Basic...Experimental.Psychology == 1,], FUN = sum), by = "year")

experimental.sum$percentage.marginal <- 100*(experimental.sum$marginal/experimental.sum$result)

#clinical psychology
clinical.sum <- merge(aggregate(result ~ year, data = dat[dat$Clinical.Psychology == 1,], FUN = length), 
                      aggregate(marginal ~ year, data = dat[dat$Clinical.Psychology == 1,], FUN = sum), by = "year")

clinical.sum$percentage.marginal <- 100*(clinical.sum$marginal/clinical.sum$result)

#Developmental psychology
developmental.sum <- merge(aggregate(result ~ year, data = dat[dat$Developmental.Psychology == 1,], FUN = length),
                           aggregate(marginal ~ year, data = dat[dat$Developmental.Psychology == 1,], FUN = sum), by = "year")

developmental.sum$percentage.marginal <- 100*(developmental.sum$marginal/developmental.sum$result)

#Educational psychology
educational.sum <- merge(aggregate(result ~ year, data = dat[dat$Educational.Psychology..School.Psychology...Training == 1,], FUN = length),
                         aggregate(marginal ~ year, data = dat[dat$Educational.Psychology..School.Psychology...Training == 1,], FUN = sum), by = "year")

educational.sum$percentage.marginal <- 100*(educational.sum$marginal/educational.sum$result)

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

#Neuroscience and cognitive psychology
neuroscience.sum <- merge(aggregate(result ~ year, data = dat[dat$Neuroscience...Cognition == 1,], FUN = length),
                          aggregate(marginal ~ year, data = dat[dat$Neuroscience...Cognition == 1,], FUN = sum), by = "year")

neuroscience.sum$percentage.marginal <- 100*(neuroscience.sum$marginal/neuroscience.sum$result)


#-------------------------------------------------------
##Linear models and plots, overall and for all subfields
#-------------------------------------------------------
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(cowplot)){install.packages("cowplot")}
if(!require(scales)){install.packages("scales")}
library(ggplot2)
library(cowplot)
library(scales)

#Overall

  fit <- lm(dat.sum$percentage.marginal ~ dat.sum$year)

  #Statistics to label graph with
  dat.stat <- bquote(list(b == .(round(summary(fit)$coefficients[2,1], digits = 2)), 
                          R^2 == .(round(summary(fit)$r.squared, digits = 2)))) 

  #Plot
  p1 <- ggplot(dat.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(dat.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("All APA journals") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.background = element_rect(fill = "grey86"), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

#clinical psychology
  
  #Statistics to label graph with
  clinical.stat <- bquote(list(b == .(round(summary(lm(clinical.sum$percentage.marginal ~ clinical.sum$year))$coefficients[2,1], digits = 2)), 
                                   R^2 == .(round(summary(lm(clinical.sum$percentage.marginal ~ clinical.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p2 <- ggplot(clinical.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(clinical.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Clinical") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))
  
#cognitive psychology and Neuroscience
  
  #Statistics to label graph with
  neuroscience.stat <- bquote(list(b == .(round(summary(lm(neuroscience.sum$percentage.marginal ~ neuroscience.sum$year))$coefficients[2,1], digits = 2)), 
                                   R^2 == .(round(summary(lm(neuroscience.sum$percentage.marginal ~ neuroscience.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p3 <- ggplot(neuroscience.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(neuroscience.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Cognitive") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))
  
#Developmental psychology
  
  #Statistics to label graph with
  developmental.stat <- bquote(list(b == .(round(summary(lm(developmental.sum$percentage.marginal ~ developmental.sum$year))$coefficients[2,1], digits = 2)), 
                           R^2 == .(round(summary(lm(developmental.sum$percentage.marginal ~ developmental.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p4 <- ggplot(developmental.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(developmental.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Developmental") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))
  
  
#Educational psychology
  
  #Statistics to label graph with
  educational.stat <- bquote(list(b == .(round(summary(lm(educational.sum$percentage.marginal ~ educational.sum$year))$coefficients[2,1], digits = 2)), 
                                    R^2 == .(round(summary(lm(educational.sum$percentage.marginal ~ educational.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p5 <- ggplot(educational.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(educational.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Educational") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

#Experimental psychology
  
  #Statistics to label graph with
  experimental.stat <- bquote(list(b == .(round(summary(lm(experimental.sum$percentage.marginal ~ experimental.sum$year))$coefficients[2,1], digits = 2)), 
                                   R^2 == .(round(summary(lm(experimental.sum$percentage.marginal ~ experimental.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p6 <- ggplot(experimental.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(experimental.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Experimental") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))
  
#forensic psychology
  
  #Statistics to label graph with
  forensic.stat <- bquote(list(b == .(round(summary(lm(forensic.sum$percentage.marginal ~ forensic.sum$year))$coefficients[2,1], digits = 2)), 
                                  R^2 == .(round(summary(lm(forensic.sum$percentage.marginal ~ forensic.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p7 <- ggplot(educational.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(educational.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Forensic") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))
  
#Health psychology
  
  #Statistics to label graph with
  health.stat <- bquote(list(b == .(round(summary(lm(health.sum$percentage.marginal ~ health.sum$year))$coefficients[2,1], digits = 2)), 
                               R^2 == .(round(summary(lm(health.sum$percentage.marginal ~ health.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p8 <- ggplot(health.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(health.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Health") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))
  

#organizational psychology
  
  #Statistics to label graph with
  organizational.stat <- bquote(list(b == .(round(summary(lm(organizational.sum$percentage.marginal ~ organizational.sum$year))$coefficients[2,1], digits = 2)), 
                                     R^2 == .(round(summary(lm(organizational.sum$percentage.marginal ~ organizational.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p9 <- ggplot(organizational.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(organizational.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Organizational") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))
  
#Social psychology
  
  #Statistics to label graph with
  social.stat <- bquote(list(b == .(round(summary(lm(social.sum$percentage.marginal ~ social.sum$year))$coefficients[2,1], digits = 2)), 
                             R^2 == .(round(summary(lm(social.sum$percentage.marginal ~ social.sum$year))$r.squared, digits = 2)))) 
  
  #Plot
  p10 <- ggplot(social.sum, aes(x = year, y = percentage.marginal)) +
    geom_point() + 
    geom_line() +
    geom_label(size = 3, x = 1985, y = 100, hjust = 0, vjust = 1, label.r = unit(0, "lines"), 
               label.padding = unit(0.12, "lines"), parse = TRUE, label = deparse(social.stat)) +
    scale_x_continuous(name = "Year", breaks = c(1985,1995,2005,2015)) +
    scale_y_continuous(name = expression(atop(paste("% of .05 < ",italic("p"), " <= .1 reported as"), 
                                              paste("marginally significant"))), limits = c(0,100))+
    ggtitle("Social") +
    theme(plot.title = element_text(size = 9), 
          axis.title = element_text(size = 9), 
          axis.text = element_text(size = 9), 
          panel.border = element_rect(fill = NA, colour = "black", size = 0.5, linetype = "solid"))

#-------------------------------------------------------
##Combining plots for final output
#-------------------------------------------------------
  
#Developmental, social, and cognitive psychology
plot_grid(p4, p10, p3, nrow = 1, ncol = 3) 

#All journals, and remaining subfields
plot_grid(p1, p2, p5, p6, p7, p8, p9)
