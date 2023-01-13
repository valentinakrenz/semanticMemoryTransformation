### written by Valentina Krenz
### analyses for the manuscript "Unraveling the semantic nature of memory transformation over time 
### submitted 2022

# PACKAGES ####
#install.packages('readxl')
library(readxl) #importing data from excel

#install.packages("tidyverse")
library(tidyverse) #prepare data #needs to be loaded after Rmisc
#install.packages('psych')
library(psych)#for descriptive statistics

#install.packages('afex')
library(afex)#ANOVA
#install.packages('emmeans')

library(emmeans) #post-hoc tests for ANOVAs and  (generalized) linear mixed models
#install.packages('lsr')
library(lsr) #for cohensDlibrary(sjPlot) #plots for (g)lmer

##multilevel packages
#install.packages('lme4')
library(lme4) #fÃ¼r lmer / glmer -> mixed effects model
#install.packages('optimx')
library(optimx)
#install.packages('lmerTest')
library(lmerTest) #show p_values in mixed effetcs model

#packages for plotting
#install.packages('ggplot2')
library(ggplot2) #for bar plots

#install.packages('Rmisc')
library(Rmisc) #necessary for SE within but masks rename (and other functions from dplyr)
#install.packages('ggpubr')
library(ggpubr)
#install.packages('sjPlot')
library(sjPlot)#plotting (generalized) linear mixed models #maskes plot_grid and save_plot from cowplot
library(showtext)
library("curl")

options(scipen=999) #don't use scientific notation for p-values

# READ IN DATA####
# path settings####
# set working directory to where this script is stored
#rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd() # check
# read in data from /data
file_path <- file.path("../data") #run first the line above

# main study ####
# control analyses 
controlDf <- read_csv(file.path(file_path, "behav", "controlMeasuresDf.csv"))%>%
  mutate(delay = factor(delay), 
         gender = factor(gender), 
         Name = factor(Name)) 

controlDf$delay <- factor(controlDf$delay, levels=c("1", "28"), labels=c("1d","28d"))

# behavioral data 
behavDf <- read_excel(file.path(file_path, "behav","behavDf.xlsx")) %>%
  mutate(emotion = factor(emotion, levels = c("neutral","negative")),
         delay = factor(delay))

# model RSA data 
modelRSADf <- read_excel(file.path(file_path, "neuro", 'modelRSADf.xlsx')) %>% 
  mutate(emotion = factor(emotion, levels=c('neutral','negative')),
         Name = factor(Name), 
         model = factor(model),
         delay = factor(delay)) 

# reinstatement data
reinstatementDf <- read_excel(file.path(file_path, "neuro", "reinstatementDf.xlsx")) %>% 
  mutate(delay = factor(delay), 
         emotion = factor(emotion, levels=c('neutral','negative')), 
         Name = factor(Name)) 

# pilot study ####

# sociodemography
pilotDemoBeforeExclusionDf <- read_excel(file.path(file_path, "pilot","pilotDemoDf.xlsx")) %>%
  mutate(gender = factor(gender))

# all stimulus sets in pilot
pilotAllSetsDf <- read_excel(file.path(file_path, "pilot","pilotAllSetsDf.xlsx")) %>% 
  mutate(emotion = factor(emotion),
         Name = factor(Name),
         lureType = factor(lureType, levels = c("new","sem","per")))

# stimulus sets for main study
pilotFinalSetsDf <- read_excel(file.path(file_path, "pilot","pilotFinalSetsDf.xlsx")) %>% 
  mutate(emotion = factor(emotion),
         Name = factor(Name),
         lureType = factor(lureType, levels = c("new","sem","per"), 
                           labels = c("unrelated","semantically related","perceptually related")),
         ratingScale = factor(ratingScale, levels = c("Per","Sem"), 
                              labels = c("perceptual relatedness", "semantic relatedness"))) %>%
  aggregate(rating ~ Name + emotion + lureType + ratingScale, FUN = mean)


# FIGURE SETTINGS #####

font_add_google("Encode Sans Condensed", "Encode Sans Condensed")
showtext.auto()

dodge = 0.15

my_theme <- theme_classic()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        plot.title = element_text(hjust = 0.5, size=35, family = "Encode Sans Condensed"),
        axis.title.y=element_text(size=28, family="Encode Sans Condensed"),
        axis.text.y = element_text(size = 26, colour="black", family = "Encode Sans Condensed"),
        axis.title.x=element_text(size=28, family = "Encode Sans Condensed"),
        axis.text.x = element_text(size = 28, colour="black", family="Encode Sans Condensed"),
        legend.title = element_text(size = 28, family = "Encode Sans Condensed"),
        legend.text = element_text(size = 26, family = "Encode Sans Condensed"),
        axis.line =  element_line(linewidth=1.75),
        axis.ticks = element_line(linewidth=1.75, colour="black"),
        axis.ticks.length = unit(.2,"cm"),
        strip.text = element_text(size=28, family = "Encode Sans Condensed"),
        strip.background=element_rect(color="white"))   

# GROUPS DID NOT DIFFER IN ATTENTIVENESS DURING ENCODING (DAY 1)####
# prepare data ####

longerEncRunDf <- controlDf %>%
  dplyr::select(Name, delay, EncodingRun1_noAnswer, EncodingRun2_noAnswer, EncodingRun3_noAnswer)%>%
  pivot_longer(cols = c(EncodingRun1_noAnswer, EncodingRun2_noAnswer, EncodingRun3_noAnswer),
               names_to = "run", # define new factor run
               values_to = "missedResponse")%>% # name values
  mutate(run = factor(run, #define run factor and rename levels
                      levels=c('EncodingRun1_noAnswer', 'EncodingRun2_noAnswer', 
                               'EncodingRun3_noAnswer'),
                      labels=c('run1', 'run2', 'run3'))) 

# analyze data ####
# describe 
meanDf <- aggregate(missedResponse ~ Name, FUN = sum,  data = longerEncRunDf)
describe(meanDf$missedResponse)

# ANOVA

encoding.ANOVA <- aov_ez(
  "Name"
  ,"missedResponse"
  ,longerEncRunDf
  ,between=c("delay")
  ,within=c("run")
  ,anova_table="pes")
print(encoding.ANOVA)
summary(encoding.ANOVA)

# EMOTIONAL ENHANCEMENT OF IMMEDIATE FREE RECALL (DAY 1)####
# prepare data ####
freeRecallDf <- aggregate(freeRecall ~ Name + delay + emotion, FUN = sum, 
                          na.rm = TRUE, na.action = na.pass, 
                          subset(behavDf, itemType == 'old'))%>% 
  mutate(freeRecall = freeRecall / 30 * 100) # get percent

meanDf <- aggregate(freeRecall ~ Name + delay, FUN = sum, 
                    na.rm = TRUE, na.action = na.pass, 
                    subset(behavDf, itemType == 'old'))%>% 
  mutate(freeRecall = freeRecall / 60 * 100) # get percent

# analyze data ####
# descriptive statistics
#overall free recall geradless of delay and emotion
describe(meanDf$freeRecall)
#free recall per group and emotion
psych::describeBy(freeRecall ~ emotion + delay, data = freeRecallDf)
#free recall per emotion regardless of group
psych::describeBy(freeRecall ~ emotion, data = freeRecallDf)

# run ANOVA
freeRecall.ANOVA <- aov_ez(
  "Name"
  ,"freeRecall"
  ,freeRecallDf 
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(freeRecall.ANOVA)

# post-hoc tests
freeRecall.emmeans <- emmeans(freeRecall.ANOVA, pairwise ~ emotion)#difference between emotion averaged over delay
summary(freeRecall.emmeans, adjust="sidak")

with(freeRecallDf, cohensD(x = freeRecall[emotion=="negative"], 
                           y = freeRecall[emotion=="neutral"], 
                           method="paired"))

# Supplementary Figure 2####

svg("2a_freeRecall.svg")

plot_data <- freeRecallDf %>% #for connected data points
  mutate(x = case_when(emotion == "neutral" ~ 1 - dodge,
                       emotion == "negative" ~ 2 + dodge))

p <- ggplot(data = plot_data, aes(x = emotion, y = freeRecall, fill = emotion)) +
  stat_summary(fun = 'mean',geom = 'bar',position = position_dodge(56),
               size = 1,width = 1.42) +
  geom_point(pch = 19,position = position_dodge(6),alpha = 0.2,size = 3) +
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1)
p + stat_summary(fun.data = mean_se,geom = "errorbar",position = position_dodge(0.6),
                 width = 0,size = 1.7) +
  scale_x_discrete(labels = c("1d", "28d")) +
  labs(y = "immediate free recall (%)") +
  scale_fill_manual(values = c("azure4", "firebrick4")) +
  #annotation
  #line
  annotate("path",x = c(1.015, 1.985),y = c(85, 85),size = 1.5) +
  # stars
  annotate("text",x = 1.5,y = 87,label = "* * *",size = 15) + 
  coord_cartesian(ylim = c(0, 100)) +
  my_theme

dev.off()

# DELAY DEPENDENT INCREASE IN HITS OVER TIME (DAY 2) #####
# prepare data####
# reduce dataframe 
oldsDf <- aggregate(cbind(hit, miss, missedResponse) ~ Name + delay + emotion, 
                    FUN = sum, na.rm = TRUE, na.action = na.pass, 
                    data = subset(behavDf, itemType == 'old'))
# compute percent 
oldsDf[,c("hit","miss","missedResponse")] <- oldsDf[,c("hit","miss"
                                                       ,"missedResponse")] /30*100

# analyze data ####
# descriptive statistics 
psych::describeBy(hit ~ delay, 
                  data = oldsDf)

psych::describeBy(oldsDf$hit, oldsDf$delay) #hits depending on delay-group

psych::describeBy(cbind(hit, miss, missedResponse) ~ emotion + delay, 
                  data = oldsDf)

# run ANOVA
hits.ANOVA <- aov_ez(
  "Name"
  ,"hit"
  ,oldsDf
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(hits.ANOVA)

# post-hoc tests
##difference between emotions separately for each group
hits.emmeans <- emmeans (hits.ANOVA, pairwise ~ emotion|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(hits.emmeans, adjust="sidak") #sidak-adjustment if necessary

#difference between delay averaging over emotion
hits.emmeans <- emmeans (hits.ANOVA, pairwise ~ delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(hits.emmeans, adjust="sidak") #sidak-adjustment if necessary

#interaction contrast: difference in delay-dependent change in hits between emotions
hits.emmeans <- emmeans(hits.ANOVA, specs = ~ delay*emotion, lmer.df = "satterthwaite")
summary(hits.emmeans)

neut.1d = c(1, 0, 0, 0)
neut.28d = c(0, 1, 0, 0)
neg.1d = c(0, 0, 1, 0)
neg.28d = c(0, 0, 0, 1)

pairs(contrast(hits.emmeans, method = list("neut 1d - neut 28d" = neut.1d - neut.28d,
                                           "neg 1d - neg 28d" = neg.1d - neg.28d)),adjust="sidak")

# run new meansDf for cohensD!
meanDf <- aggregate(hit ~ Name + delay, FUN = mean, data = oldsDf) 

with(meanDf, cohensD(x = hit[delay=="1d" ], 
                     y = hit[delay=="28d"],
                     method="unequal"))

with(oldsDf, cohensD(x = hit[delay=="28d" & emotion=="neutral"], 
                     y = hit[delay=="28d"& emotion=="negative"],
                     method="paired"))

with(oldsDf, cohensD(x = hit[delay=="1d" & emotion=="negative"], 
                     y = hit[delay=="1d"& emotion=="neutral"],
                     method="paired"))


# check influence of outliers ####
# check outlier 
data <- oldsDf

#1d - neutral
log1 <- which(data$hit[data$delay == "1d" & data$emotion == "neutral"]  > (mean(data$hit[data$delay == "1d" & data$emotion == "neutral"])+3*sd(data$hit[data$delay == "1d" & data$emotion == "neutral"]))|  data$hit[data$delay == "1d" & data$emotion == "neutral"]  < (mean(data$hit[data$delay == "1d" & data$emotion == "neutral"])+ (-3*sd(data$hit[data$delay == "1d" & data$emotion == "neutral"]))))
data$hit[data$delay == "1d" & data$emotion == "neutral"][log1] = 999
#no outlier

#28d - neutral
log1 <- which(data$hit[data$delay == "28d" & data$emotion == "neutral"]  > (mean(data$hit[data$delay == "28d" & data$emotion == "neutral"])+3*sd(data$hit[data$delay == "28d" & data$emotion == "neutral"]))|  data$hit[data$delay == "28d" & data$emotion == "neutral"]  < (mean(data$hit[data$delay == "28d" & data$emotion == "neutral"])+ (-3*sd(data$hit[data$delay == "28d" & data$emotion == "neutral"]))))
data$hit[data$delay == "28d" & data$emotion == "neutral"][log1] = 999
#no outlier

#1d - negative
log1 <- which(data$hit[data$delay == "1d" & data$emotion == "negative"]  > (mean(data$hit[data$delay == "1d" & data$emotion == "negative"])+3*sd(data$hit[data$delay == "1d" & data$emotion == "negative"]))|  data$hit[data$delay == "1d" & data$emotion == "negative"]  < (mean(data$hit[data$delay == "1d" & data$emotion == "negative"])+ (-3*sd(data$hit[data$delay == "1d" & data$emotion == "negative"]))))
data$hit[data$delay == "1d" & data$emotion == "negative"][log1] = 999
#no outlier

#28d - negative
log1 <- which(data$hit[data$delay == "28d" & data$emotion == "negative"]  > (mean(data$hit[data$delay == "28d" & data$emotion == "negative"])+3*sd(data$hit[data$delay == "28d" & data$emotion == "negative"]))|  data$hit[data$delay == "28d" & data$emotion == "negative"]  < (mean(data$hit[data$delay == "28d" & data$emotion == "negative"])+ (-3*sd(data$hit[data$delay == "28d" & data$emotion == "negative"]))))
data$hit[data$delay == "28d" & data$emotion == "negative"][log1] = 999
#no outlier

#no outlier!

# Figure 2A left#####
# define and save figure
svg("2_hits.svg")

plot_data <- oldsDf %>% ##for connected individual data points
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))


p <- ggplot(data=plot_data, aes(x=delay, y=hit, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6),  
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  labs(y="% hits in delayed recognition")+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  # stars 
  annotate("text", x = 2, y = 87, label = "* *", color="black", size =15)+ 
  #line
  annotate("path", x = c(1.79, 2.21), y = c(85, 85), size=1.5) +
  # stars 
  annotate("text", x = 1.5, y = 101.00, label = "* * *", color="black", size = 15)+ 
  #line
  annotate("path", x = c(1, 2), y = c(99, 99), size=1.5) +
  coord_cartesian( ylim = c(0, 100)) +
  my_theme
dev.off()

# Supplementary Table 1 ####
# old items 
# reduce data frame
oldRawDf <- aggregate(cbind(hit, miss, missedResponse) ~ Name + delay + emotion, FUN = sum, 
                      na.rm = TRUE, na.action = na.pass, 
                      data = subset(behavDf, itemType == 'old')) 
# show descriptive statistics for old items
psych::describeBy(cbind(hit, miss, missedResponse) ~ emotion + delay, 
                  data = oldRawDf) 

# lures 
# reduce data frame
luresRawDf <- aggregate(cbind(FA, CR, missedResponse) ~ Name + delay + emotion + itemType, 
                        FUN = sum, na.rm = TRUE, na.action = na.pass, 
                        data = subset(behavDf, itemType != 'old')) 
# change level order of itemType
luresRawDf$itemType <- factor(luresRawDf$itemType, levels = c("per", "sem", "new"))
# show descriptive statistics for lures
psych::describeBy(cbind(CR, FA, missedResponse) ~ emotion + delay +  itemType, data = luresRawDf)  

# DELAY DEPENDENT INCREASE IN FALSE ALARMS SPECIFICALLY FOR SEMANTICALLY RELATED LURES (DAY 2) ####
# prepare data ####
# reduce dataframe 
luresDf <- aggregate(FA ~ Name + delay + emotion + itemType, FUN = sum, 
                     na.rm = TRUE, na.action = na.pass, 
                     data = subset(behavDf, itemType != 'old'))%>% # take only lures 
  mutate(FA = FA /30*100,# get percent
         lureType = factor(itemType, levels = c("new","per","sem"))) #new=unrelated, #per=perceptually related, #sem=semantically related

# analyze data ####
# run ANOVA
FAs.ANOVA <- aov_ez(
  "Name"
  ,"FA"
  ,luresDf
  ,between=c("delay")
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(FAs.ANOVA)

# post-hoc tests
#effect of delay depending on lure type
FAs.emmeansDelayLure <- emmeans (FAs.ANOVA, pairwise ~ delay|lureType, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansDelayLure, adjust="sidak") #sidak-adjustment if necessary

FAs.emmeansDelayLure <- emmeans (FAs.ANOVA, pairwise ~ lureType|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansDelayLure, adjust="sidak") #sidak-adjustment if necessary

#interaction contrast: difference increase in FAs over time between lure types
FAs.emmeans <- emmeans(FAs.ANOVA, specs = ~ delay*lureType, lmer.df = "satterthwaite")
summary(FAs.emmeans)

sem.1d = c(0, 0, 0, 0, 1, 0)
sem.28d = c(0, 0, 0, 0, 0, 1)
new.1d = c(1, 0, 0, 0, 0, 0)
new.28d = c(0, 1, 0, 0, 0, 0)
per.1d = c(0, 0, 1, 0, 0, 0)
per.28d = c(0, 0, 0, 1, 0, 0)

pairs(contrast(FAs.emmeans, method = list("sem 1d - sem 28d" = sem.1d - sem.28d,
                                          "new 1d - new 28d" = new.1d - new.28d,
                                          "per 1d - per 28d" = per.1d - per.28d)),adjust="sidak") 
#
FAs.emmeansEmoLureDelay <- emmeans (FAs.ANOVA, pairwise ~ emotion|lureType:delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansEmoLureDelay, adjust="sidak")

#for plotting
#differences between lure types depending on delay
FAs.emmeansDelayLure <- emmeans (FAs.ANOVA, pairwise ~ lureType|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansDelayLure, adjust="sidak") #sidak-adjustment if necessary

#differences between lure types depending on emotion and delay
FAs.emmeansEmoLureDelay <- emmeans (FAs.ANOVA, pairwise ~ lureType|emotion:delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansEmoLureDelay, adjust="sidak")

#differences between lure types depending on emotion and delay
FAs.emmeansEmoLureDelay <- emmeans (FAs.ANOVA, pairwise ~ lureType|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansEmoLureDelay, adjust="sidak")

# compute cohens d over factor emotion
meanDf <- aggregate(FA ~ Name + delay + lureType, FUN = function(x) mean(x, na.rm = TRUE), 
                    data = luresDf)

with(meanDf, cohensD(x = FA [delay == "1d" & lureType == "new"], 
                     y = FA [delay == "1d" & lureType == "per"], 
                     method = 'paired'))

with(meanDf, cohensD(x = FA [delay == "28d" & lureType == "new"], 
                     y = FA [delay == "28d" & lureType == "per"], 
                     method = 'paired'))

with(meanDf, cohensD(x = FA [delay == "1d" & lureType == "sem"], 
                     y = FA [delay == "28d" & lureType == "sem"], 
                     method = 'unequal'))

#sem 1d: neutr vs neg
with(luresDf, cohensD(x = FA[delay=="1d" & lureType=="sem" & emotion=="negative"], 
                      y = FA[delay=="1d" & lureType=="sem" & emotion=="neutral"], 
                      method = 'paired'))
###sem 28d: neutr vs neg
with(luresDf, cohensD(x = FA[delay=="28d" & lureType=="sem" & emotion=="negative"], 
                      y = FA[delay=="28d" & lureType=="sem" & emotion=="neutral"], 
                      method = 'paired'))

# post-hoc ANOVAs to check whether the emotion x delay interaction depends on lure type
# ANOVA for semantically related only
semFAs.ANOVA <- aov_ez(
  "Name"
  ,"FA"
  ,subset(luresDf, lureType == "sem")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(semFAs.ANOVA)
summary(semFAs.ANOVA)

# ANOVA for unrelated only
newFAs.ANOVA <- aov_ez(
  "Name"
  ,"FA"
  ,subset(luresDf, lureType == "new")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(newFAs.ANOVA)

# ANOVA for perceptually related only
perFAs.ANOVA <- aov_ez(
  "Name"
  ,"FA"
  ,subset(luresDf, lureType == "per")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(perFAs.ANOVA)

# check influence of outliers ####
# find outliers depending on each condition 
data <- luresDf # copy dataframe

#per
log1 <- which(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))|  data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))))
data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
# 1 outlier

log1 <- which(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"]))|  data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"]))))
data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
#none

log1 <- which(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))|  data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))))
data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
#none

log1 <- which(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"]))|  data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"]))))
data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
#none

#sem
log1 <- which(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))))
data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
#none

log1 <- which(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"]))))
data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
#none

log1 <- which(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))|  data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))))
data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
# 1 

log1 <- which(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"]))|  data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"]))))
data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
#none


#new
log1 <- which(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))|  data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))))
data$FA[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"][log1] = 999
# 1

log1 <- which(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"]))|  data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"]))))
data$FA[data$delay == "28d" & data$emotion == "neutral" & data$lureType == "new"][log1] = 999
# 1

log1 <- which(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))|  data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))))
data$FA[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
# 1

log1 <- which(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"]))|  data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"]))))
data$FA[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
# 1

# exclude outliers 
log = data$FA == 999
data$FA[log] = NA

# run ANOVA after the exclusion of those outliers 
FAs.ANOVA <- aov_ez(
  "Name"
  ,"FA"
  ,data
  ,between=c("delay")
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(FAs.ANOVA) # no change due to outlier exclusion: interaction of delay x lureType and delay x emotion x lure Type still significant!
summary(FAs.ANOVA)

# Figure 2A right####

svg("Figure2B_unrelatedLures.svg")

plot_data <- luresDf %>% 
  filter(lureType == "new")%>% 
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  geom_segment(aes(x=1,xend=2,y=20,yend=20), size=1.5) + 
  annotate(geom="text", x=1.5, y=21.25, label= c("*"), color="black", fontface = "bold", size = 15) +
  labs(y="false alarms in delayed recognition (%)")+coord_cartesian( ylim = c(0, 40)) +
  my_theme

dev.off()

svg("Figure2B_perceptuallyRelatedLures.svg")

plot_data <- luresDf %>% 
  filter(lureType == "per") %>% 
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))
p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y="false alarms in delayed recognition (%)")+coord_cartesian( ylim = c(0, 40)) +
  my_theme

dev.off()

svg("Figure2B_semanticallyRelatedLures.svg")

plot_data <- luresDf %>% 
  filter(lureType == "sem") %>% 
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  
  annotate(geom="text", x=1.5, y=25.25, label= c("* *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1,xend=2,y=24,yend=24), size=1.5) +
  
  annotate(geom="text", x=2, y=21.25, label= c("* *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.75,xend=2.25,y=20,yend=20), size=1.5) +  
  
  labs(y="false alarms in delayed recognition (%)")+coord_cartesian( ylim = c(0, 40)) +
  my_theme

dev.off()


# Supplementary Results #####
# false alarms weighted by confidence ####
# preprare data 
luresWeightedDf <- subset(behavDf, itemType != 'old') %>% # take only lures
  aggregate(FA_weighted ~ Name + delay + emotion + itemType, 
            FUN = sum, na.rm = TRUE, na.action = na.pass) %>%
  mutate(FA = FA_weighted/60*100, # compute percent
         lureType = factor(itemType, levels = c("new","per","sem")) )

# run ANOVA 
FAs.ANOVA <- aov_ez(
  "Name"
  ,"FA"
  ,luresWeightedDf
  ,between=c("delay")
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(FAs.ANOVA)
summary(FAs.ANOVA)

# post-hoc tests
#effect of delay depending on lure type
FAs.emmeansDelayLure <- emmeans (FAs.ANOVA, pairwise ~ delay|lureType, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansDelayLure, adjust="sidak") #sidak-adjustment if necessary

#interaction contrast: difference increase in FAs over time between lure types
FAs.emmeans <- emmeans(FAs.ANOVA, specs = ~ delay*lureType, lmer.df = "satterthwaite")
summary(FAs.emmeans)

sem.1d = c(0, 0, 0, 0, 1, 0)
sem.28d = c(0, 0, 0, 0, 0, 1)
new.1d = c(1, 0, 0, 0, 0, 0)
new.28d = c(0, 1, 0, 0, 0, 0)
per.1d = c(0, 0, 1, 0, 0, 0)
per.28d = c(0, 0, 0, 1, 0, 0)

pairs(contrast(FAs.emmeans, method = list("sem 1d - sem 28d" = sem.1d - sem.28d,
                                          "new 1d - new 28d" = new.1d - new.28d,
                                          "per 1d - per 28d" = per.1d - per.28d)),adjust="sidak") 

FAs.emmeansEmoLureDelay <- emmeans (FAs.ANOVA, pairwise ~ emotion|lureType:delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansEmoLureDelay, adjust="sidak")

#for plotting
#differences between lure types depending on delay
FAs.emmeansDelayLure <- emmeans (FAs.ANOVA, pairwise ~ lureType|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(FAs.emmeansDelayLure, adjust="sidak") #sidak-adjustment if necessary

# Supplementary Figure 6 ######

svg("SupplementaryFigWeighted_unrelatedLures.svg")

plot_data <- luresWeightedDf %>% 
  filter(lureType == "new")%>% 
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y="% false alarms (weighted by confidence)")+
  coord_cartesian( ylim = c(0, 40)) +
  my_theme
dev.off()

svg("SupplementaryFigWeighted_perceptuallyRelatedLures.svg")

plot_data <- luresWeightedDf %>% 
  filter(lureType == "per") %>% 
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
  stat_summary(fun='mean',geom='bar', 
               position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),
               width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y="% false alarms (weighted by confidence)")+coord_cartesian( ylim = c(0, 40)) +
  theme_classic()+
  my_theme

dev.off()

svg("SupplementaryFigWeighted_semanticallyRelatedLures.svg")

plot_data <- luresWeightedDf %>% 
  filter(lureType == "sem") %>% 
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=FA, fill=emotion))+
  stat_summary(fun='mean',geom='bar', 
               position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  # delay effect
  annotate(geom="text", x=1.5, y=25.25, label= c("* *"), color="black", size = 15) +
  geom_segment(aes(x=1,xend=2,y=24,yend=24), size=1.5) +
  #emotion effetc at 28d
  annotate(geom="text", x=2, y=21.25, label= c("* * *"), color="black",  size = 15) +
  geom_segment(aes(x=1.75,xend=2.25,y=20,yend=20), size=1.5) +  
  
  labs(y="% false alarms (weighted by confidence)") + 
  coord_cartesian( ylim = c(0, 40)) +
  my_theme

dev.off()


# confidency of FAs ####

# semantically related lures 
# prepare data
semConfDf <- subset(behavDf, itemType == 'sem') %>%
  dplyr::select(Name, delay, stimulusTypeNum, emotion, FA_confidency)
# analyze data
gLMM <- glmer(FA_confidency ~ delay * emotion +
                (1| Name)+(1|stimulusTypeNum), data = semConfDf, family = "binomial", 
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(gLMM) 

# perceptually related lures 
# prepare data 
perConfDf <- subset(behavDf, itemType == 'per') %>%
  dplyr::select(Name, delay, stimulusTypeNum, emotion, FA_confidency)
# analyze data
gLMM <- glmer(FA_confidency ~ delay * emotion +
                (1| Name)+(1|stimulusTypeNum), data = perConfDf, family = "binomial", 
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(gLMM)

# unrelated lures 
# prepare data 
newConfDf <- subset(behavDf, itemType == 'new') %>%
  dplyr::select(Name, delay, stimulusTypeNum, emotion, FA_confidency)
# run LMM
gLMM <- glmer(FA_confidency ~ delay * emotion +
                (1| Name), data = newConfDf, family = "binomial", 
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5))) 

summary(gLMM) 

# MEMORY SPECIFICITY ON THE LEVEL OF EACH INDIVIDUAL RELATED STIMULUS SET #####
# prepare data ####
# reduce data frame without aggregating 
transformationDf <- subset(behavDf, itemType == "old") %>%
  dplyr::select ("Name", "set", "delay", "emotion", "detailed", "forgotten", 
                 "semOnly_transformed", "perOnly_transformed") %>%
  mutate(detailed = factor(detailed),
         forgotten = factor(forgotten),
         semOnly_transformed = factor(semOnly_transformed),
         perOnly_transformed = factor(perOnly_transformed),
         set = factor(set) )

# prepare smaller df for plotting and descriptive statistics in perc
smaller_transformationDf <- subset(behavDf, itemType == "old") %>%
  dplyr::select ("Name", "set", "delay", "emotion", "detailed", "forgotten", 
                 "semOnly_transformed", "perOnly_transformed") %>%
  mutate(detailed = as.numeric(detailed),
         forgotten = as.numeric(forgotten),
         semOnly_transformed = as.numeric(semOnly_transformed),
         perOnly_transformed = as.numeric(perOnly_transformed)) %>% 
  aggregate(cbind(detailed, forgotten, semOnly_transformed, 
                  perOnly_transformed) ~ Name + delay + emotion, 
            FUN = sum, na.rm = TRUE, na.action = na.pass)

# analyze data ####
# descriptive statistics
# average of missing data in data frame
transformationDf["missingTransCategory"] <- rowMeans(is.na(transformationDf[, c("detailed", "semOnly_transformed", "forgotten", "perOnly_transformed")]))

meanDf <- aggregate(missingTransCategory ~ Name + delay, FUN = sum, data = transformationDf)
meanDf["missingTransCategory_perc"] <- meanDf["missingTransCategory"]/60*100

describe(meanDf$missingTransCategory_perc)


# SHOW MEAN AND SEM IN PERCENT
psych::describeBy(cbind(detailed, forgotten, semOnly_transformed, 
                        perOnly_transformed) ~ emotion + delay, 
                  data = smaller_transformationDf)

# detailed memories 
#fit binomial generalized linear mixed model on detailed memory 
detailed_glmm <- glmer(detailed ~ delay * emotion +
                         (1| Name)+(1|set), data = transformationDf, family = "binomial", 
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5))) 

summary(detailed_glmm)
sjPlot::tab_model(detailed_glmm) # plot results table
# post-hoc test 
emmeans<- emmeans(detailed_glmm, pairwise ~ delay, lmer.df = "satterthwaite") #1d vs 28d
summary(emmeans, adjust="sidak")
eff_size(emmeans, sigma= sigma(detailed_glmm), edf = Inf)

# forgotten memories 
# fit binomial generalized linear mixed model on forgotten sets
forgotten_glmm <- glmer(forgotten ~ delay * emotion +
                          (1 | Name) + (1|set), data = transformationDf, family = "binomial", 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e5))) 

summary(forgotten_glmm)
sjPlot::tab_model(forgotten_glmm) # plot results table

# post-hoc tests 
# difference between emotions at different delays
forgotten.emmeansEmoDelay <- emmeans (forgotten_glmm, pairwise ~ emotion|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(forgotten.emmeansEmoDelay, adjust="sidak") #sidak-adjustment if necessary
eff_size(forgotten.emmeansEmoDelay, sigma= sigma(forgotten_glmm), edf = Inf) #sigma is the smallest error SS from model
#neutral vs negative at 28d *

# difference between between delays in different emotions
forgotten.emmeansDelayEmo <- emmeans (forgotten_glmm, pairwise ~ delay|emotion, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(forgotten.emmeansDelayEmo, adjust="sidak") #sidak-adjustment if necessary
eff_size(forgotten.emmeansDelayEmo, sigma= sigma(forgotten_glmm), edf = Inf) #sigma is the smallest error SS from model
#1d vs 28d in neutral ***   1d vs 28d in negative **

#interaction contrast: difference in delay-dependent increase between emotions
forgotten.emmeans<- emmeans(forgotten_glmm, specs = ~ delay*emotion, lmer.df = "satterthwaite")
summary(forgotten.emmeans)

neutral.1d = c(1, 0, 0, 0)
neutral.28d = c(0, 1, 0, 0)
negative.1d = c(0, 0, 1, 0)
negative.28d = c(0, 0, 0, 1)

forgotten.emmeansInteraction <- pairs(contrast(forgotten.emmeans, method = list("neutral 28d - neutral 1d" = neutral.28d - neutral.1d, 
                                                                                "negative 28d - negative 1d" = negative.28d - negative.1d)),adjust="sidak")
summary(forgotten.emmeansInteraction) #higher increase in forgotten items for neutral than for negative **


# semantically transformed memories 
#fit binomial generalized linear mixed model on semantically transformed memories
semOnly_transformed_glmm <- glmer(semOnly_transformed ~ delay * emotion +
                                    (1 | Name) + (1|set), data = transformationDf, family = "binomial", 
                                  control=glmerControl(optimizer="bobyqa",
                                                       optCtrl=list(maxfun=2e5))) 
summary(semOnly_transformed_glmm)

# post-hoc tests 
# 1d vs 28d
emmeans<- emmeans(semOnly_transformed_glmm, pairwise ~ delay, lmer.df = "satterthwaite")
summary(emmeans) # 1d < 28d ***

#interaction contrast: difference in delay-dependent increase between emotions
emmeans<- emmeans(semOnly_transformed_glmm, specs = ~ delay*emotion, lmer.df = "satterthwaite")
summary(emmeans)

neutral.1d = c(1, 0, 0, 0)
neutral.28d = c(0, 1, 0, 0)
negative.1d = c(0, 0, 1, 0)
negative.28d = c(0, 0, 0, 1)

emmeansInteraction <- pairs(contrast(emmeans, method = list("neutral 28d - neutral 1d" = neutral.28d - neutral.1d, 
                                                            "negative 28d - negative 1d" = negative.28d - negative.1d)),adjust="sidak")
summary(emmeansInteraction)

# for plotting:
# difference between emotions at different delays
emmeansEmoDelay <- emmeans (semOnly_transformed_glmm, pairwise ~ emotion|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(emmeansEmoDelay, adjust="sidak") #sidak-adjustment if necessary #neutral vs negative n.s.
eff_size(emmeansEmoDelay, sigma= sigma(semOnly_transformed_glmm), edf = Inf) 

# difference between delays for different emotions
emmeansDelayEmo <- emmeans (semOnly_transformed_glmm, pairwise ~ delay|emotion, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(emmeansDelayEmo, adjust="sidak") #sidak-adjustment if necessary #1d vs 28d in neutral* 1d vs 28d in negative ***
eff_size(emmeansDelayEmo, sigma= sigma(semOnly_transformed_glmm), edf = Inf) 

# perceptually transformed memories 
#fit binomial generalized linear mixed model on perceptually transformed memory 
perOnly_transformed_glmm <- glmer(perOnly_transformed ~ delay * emotion +
                                    (1 | Name) + (1|set), data = transformationDf, family = "binomial", 
                                  control=glmerControl(optimizer="bobyqa",
                                                       optCtrl=list(maxfun=2e5))) 

summary(perOnly_transformed_glmm) 

# Figure 2B ######
# prepare data 
# prepare for connected individual data points
smaller_transformationDf[,c("detailed", "forgotten", 
                            "semOnly_transformed", 
                            "perOnly_transformed")] <- smaller_transformationDf[,c("detailed", 
                                                                                   "forgotten", "semOnly_transformed", 
                                                                                   "perOnly_transformed")] / 30 *100

plot_data <- smaller_transformationDf %>% 
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))
# define dodge
dodge = 0.15
# plot mean perceptually transformed memory in percent 
svg("Figure2B_perTransformed.svg")
p <- ggplot (data=plot_data, aes(x=delay, y=perOnly_transformed, fill=emotion))+
  #facet_grid(~ emotion)+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), # new version (no more jittering)
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  coord_cartesian( ylim = c(0, 100)) +
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  my_theme 

# plot mean semantically transformed memory in percent   
svg("Figure2B_semTransformed.svg")
p <- ggplot (data=plot_data, aes(x=delay, y=semOnly_transformed, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  #1d vs 28d
  annotate(geom="text", x=1.5, y=0.9, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.03,xend=1.97,y=0.885,yend=0.885), size=1.5) + 
  #neut: 1d vs neg
  annotate(geom="text", x=1.25, y=0.8, label= c("*"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=0.79,xend=1.76,y=0.789,yend=0.789), size=1.5) +
  #neg: 1d vs 28d
  annotate(geom="text", x=1.75, y=0.7, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.29,xend=2.21,y=0.685,yend=0.685), size=1.5) +  
  #aestehtics
  coord_cartesian( ylim = c(0, 100)) +
  my_theme 

dev.off()
# plot mean forgotten memory in percent 
svg("Figure2B_forgotten.svg")
p <- ggplot (data=plot_data, aes(x=delay, y=forgotten, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), # new version (no more jittering)
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  
  annotate(geom="text", x=1.25, y=0.8, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=0.79,xend=1.76,y=0.789,yend=0.789), size=1.5) +
  #
  annotate(geom="text", x=2, y=0.6, label= c("*"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.79,xend=2.21,y=0.585,yend=0.585), size=1.5) +  
  #
  annotate(geom="text", x=1.75, y=0.7, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.29,xend=2.21,y=0.685,yend=0.685), size=1.5) +  
  #
  annotate(geom="text", x=1.5, y=0.9, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.03,xend=1.97,y=0.885,yend=0.885), size=1.5) + 
  coord_cartesian( ylim = c(0, 100)) +
  my_theme 
dev.off()

# plot mean detailed memory in percent 
svg("Figure2B_detailled.svg")
p <- ggplot (data=plot_data, aes(x=delay, y=detailed, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  #1d vs 28d
  annotate(geom="text", x=1.5, y=0.9, label= c("* * *"), color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1.03,xend=1.97,y=0.885,yend=0.885), size=1.5) + 
  labs(y="memory specificity in delayed recognition (%)")+
  coord_cartesian( ylim = c(0, 1)) +
  my_theme 

dev.off()

# RELATEDNESS RATING ON DAY 3####
# prepare data ####
smaller_simRatingsDf <- subset(behavDf, itemType != "old") %>%
  aggregate(cbind(percRating, semRating) ~ Name + delay + emotion + itemType, 
            FUN = mean) %>% 
  mutate(lureType = factor(itemType))
# over emotion and delay
meanDf <- aggregate(cbind(percRating, semRating) ~ Name + lureType, 
                    FUN = function(x) mean(x, na.rm = TRUE), 
                    data = smaller_simRatingsDf)

# analyze data #####
# descriptive statistics for Supplementary Table 1 ####
psych::describeBy(percRating ~ emotion + lureType + delay, data = smaller_simRatingsDf)
psych::describeBy(semRating ~ emotion + lureType + delay, data = smaller_simRatingsDf)

psych::describeBy(percRating ~ lureType, data = meanDf)
psych::describeBy(semRating ~ lureType, data = meanDf)

# analyze semantic relatedness rating #####
# run ANOVA 
semRating.ANOVA <- aov_ez(
  "Name"
  ,"semRating"
  ,smaller_simRatingsDf
  ,between=c("delay")
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(semRating.ANOVA)
summary(semRating.ANOVA)

# post hoc tests 
semRating.emmeansDelayLure <- emmeans (semRating.ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
summary(semRating.emmeansDelayLure, adjust="sidak") # sidak adjustment when needed

with(meanDf, cohensD(x = semRating[lureType=="sem"], 
                     y = semRating[lureType=="per"], 
                     method="paired"))

with(meanDf, cohensD(x = semRating[lureType=="sem"], 
                     y = semRating[lureType=="new"], 
                     method="paired"))

# analyze perceptual relatedness rating ####
# run ANOVA 
perRating.ANOVA <- aov_ez(
  "Name"
  ,"percRating"
  ,smaller_simRatingsDf
  ,between=c("delay")
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(perRating.ANOVA)
summary(perRating.ANOVA)

# post hoc tests 
#difference in perceptual relatedness rating between lure
perRating.emmeansDelayLure <- emmeans (perRating.ANOVA, pairwise ~ lureType, 
                                       lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
summary(perRating.emmeansDelayLure, adjust="sidak") # sidak adjustment when needed

perRating.emmeansDelayLure <- emmeans (perRating.ANOVA, pairwise ~ delay|lureType, 
                                       lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
summary(perRating.emmeansDelayLure, adjust="sidak") # sidak adjustment when needed

perRating.emmeansDelayLure <- emmeans (perRating.ANOVA, pairwise ~ lureType|delay, 
                                       lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
summary(perRating.emmeansDelayLure, adjust="sidak") # sidak adjustment when needed

# watch out to define the new meanDf first!
meanDf <- aggregate(cbind(percRating, semRating) ~ Name + delay + lureType, 
                    FUN = mean, data = smaller_simRatingsDf)

with(meanDf, cohensD(x = percRating[lureType=="sem"], 
                     y = percRating[lureType=="new"],
                     method = 'paired'))

with(meanDf, cohensD(x = percRating[lureType=="sem"], 
                     y = percRating[lureType=="per"],
                     method = 'paired'))

with(meanDf, cohensD(x = semRating[lureType=="sem"], 
                     y = percRating[lureType=="sem"], 
                     method="paired"))
with(meanDf, cohensD(x = percRating[lureType=="per"], 
                     y = semRating[lureType=="per"], 
                     method="paired"))

with(meanDf, t.test(percRating[lureType=="per"], 
                    semRating[lureType=="per"], paired = TRUE)) 

with(meanDf, t.test(percRating[lureType=="new"], 
                    semRating[lureType=="new"], paired = TRUE)) 
with(meanDf, cohensD(percRating[lureType=="new"], 
                     semRating[lureType=="new"], method = "paired")) 


# difference between rating types in lures ####
# semantically related lurses
with(meanDf, t.test(semRating[lureType=="sem"], 
                    percRating[lureType=="sem"], 
                    paired = TRUE)) 
with(meanDf, cohensD(x = semRating[lureType=="sem"], 
                     y = percRating[lureType=="sem"], 
                     method="paired"))
# perceptually related lures
with(meanDf, t.test(semRating[lureType=="per"], 
                    percRating[lureType=="per"], 
                    paired = TRUE)) 
with(meanDf, cohensD(x = semRating[lureType=="per"], 
                     y = percRating[lureType=="per"], 
                     method="paired"))
# unrelated lures
with(meanDf, t.test(semRating[lureType=="new"], 
                    percRating[lureType=="new"], 
                    paired = TRUE)) 
with(meanDf, cohensD(x = semRating[lureType=="new"], 
                     y = percRating[lureType=="new"], 
                     method="paired"))

# check influence of outliers #####
data <- smaller_simRatingsDf

#semantic relatedness rating

#neutral
#1d - neutral - per 
log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))|  data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))))
data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
#no outlier

#1d - neutral - sem
log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))))
data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
#no outlier

#1d - neutral - new
log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))|  data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))))
data$semRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"][log1] = 999
#no outlier

#28d - neutral - per 
log1 <- which(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]))|  data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]))))
data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
#no outlier

#28d - neutral - sem
log1 <- which(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]))))
data$semRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
#no outlier

#negative
#1d - negative - per 
log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))|  data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))))
data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
#no outlier

#1d - negative - sem
log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))|  data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))))
data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
#no outlier

#1d - negative - new
log1 <- which(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))|  data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))))
data$semRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
#no outlier

#28d - negative - per 
log1 <- which(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]))|  data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]))))
data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
#no outlier

#28d - negative - sem
log1 <- which(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]))|  data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]))))
data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
#no outlier


#28d - negative - new
log1 <- which(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]))|  data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$semRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]))))
data$semRating[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
#no outlier

# no outliers!

# perceptual relatedness rating

#neutral
#1d - neutral - per 
log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))|  data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"]))))
data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
#no outlier

#1d - neutral - sem
log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"]))))
data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
#no outlier

#1d - neutral - new
log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))|  data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"]))))
data$percRating[data$delay == "1d" & data$emotion == "neutral" & data$lureType == "new"][log1] = 999
#no outlier

#28d - neutral - per 
log1 <- which(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]))|  data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"]))))
data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "per"][log1] = 999
#no outlier

#28d - neutral - sem
log1 <- which(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]))|  data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"]))))
data$percRating[data$delay == "28" & data$emotion == "neutral" & data$lureType == "sem"][log1] = 999
#no outlier

#negative
#1d - negative - per 
log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))|  data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"]))))
data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
#no outlier

#1d - negative - sem
log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))|  data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"]))))
data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
#no outlier

#1d - negative - new
log1 <- which(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))|  data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"]))))
data$percRating[data$delay == "1d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
#no outlier

#28d - negative - per 
log1 <- which(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]))|  data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"]))))
data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "per"][log1] = 999
#no outlier

#28d - negative - sem
log1 <- which(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]))|  data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"]))))
data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "sem"][log1] = 999
#no outlier

#28d - negative - new
log1 <- which(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]  > (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"])+3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]))|  data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]  < (mean(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"])+ (-3*sd(data$percRating[data$delay == "28" & data$emotion == "negative" & data$lureType == "new"]))))
data$percRating[data$delay == "28d" & data$emotion == "negative" & data$lureType == "new"][log1] = 999
#no outlier

# no outliers!



# Figure 2C left ####
# prepare data 
# stack rating categories to the new variable ratingType with the value named rating
plot_data <- meanDf %>% # make sure to run the new meanDf first
  pivot_longer(cols = c("percRating", "semRating"),
               names_to = "ratingType", 
               values_to = "rating") %>%
  mutate(lureType = factor(lureType, levels = c("per", "sem", "new"))) %>%
  mutate(x = case_when(
    lureType == "per" & ratingType == "percRating" ~ 1 - dodge,
    lureType == "per" & ratingType == "semRating" ~ 1 + dodge,
    lureType == "sem" & ratingType == "percRating" ~ 2 - dodge,
    lureType == "sem" & ratingType == "semRating" ~ 2 + dodge,
    lureType == "new" & ratingType == "percRating" ~ 3 - dodge,
    lureType == "new" & ratingType == "semRating" ~ 3 + dodge,
  ))

# plot
svg("Figure2Cleft.svg")

p <- ggplot (data=plot_data, aes(x=lureType, y=rating, fill=ratingType))+   
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, lureType)), alpha = 0.1) +
  labs(y="relatedness rating")
p + theme_classic()+
  #annotation
  #percRel vs semRel in sem rating
  # stars 
  annotate("text", x = 1.75, y = 11, label = "* * *", size =15)+ 
  #line
  annotate("path", x = c(1.24, 2.21), y = c(10.8, 10.8), size=1.5) +
  #sem vs new in sem rating
  # stars 
  annotate("text", x = 2.75, y = 11, label = "* * *", size =15)+ 
  #line
  annotate("path", x = c(2.24, 3.21),  y = c(10.8, 10.8), size=1.5) +
  #sem vs perc rating in semLure
  # stars 
  annotate("text", x = 2, y = 9.9, label = "* * *", size =15)+ 
  #line
  annotate("path", x = c(0.79, 1.76), y = c(7.7, 7.7), size=1.5) +
  #line
  annotate("path", x = c(0.79, 1.21), y = c(6.6, 6.6), size=1.5) +
  # stars 
  annotate("text", x = 1, y = 6.8, label = "***", size = 15)+ 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_fill_manual(values=c("lightsteelblue", "deepskyblue4"))+ labs(x="lure type")+
  scale_x_discrete(labels=c("perceptually \n related","semantically \n related","unrelated"))+
  coord_cartesian( ylim = c(0, 11)) + 
  scale_y_continuous(name ="relatedness rating", breaks=c(0, 2, 4, 6, 8, 10))+
  my_theme 

dev.off()

# Supplementary Figure 3 ####
svg("SupplFig7_distributionOfRating.svg", height=10, width=5) 

plot_data <- smaller_simRatingsDf %>%
  pivot_longer(cols = c("percRating", "semRating"),
               names_to = "ratingType", 
               values_to = "rating") %>%
  mutate(lureType = factor(lureType, levels=c('per','sem','new'), 
                           labels=c('perceptually related','semantically related','unrelated')))

ggplot(plot_data, aes(x = rating, fill = ratingType, linetype=delay)) +
  facet_wrap(lureType ~ emotion, ncol=2)+
  geom_density(alpha = .4, size=1) +
  scale_linetype_manual(values=c("solid", "dashed"))+
  scale_fill_manual(labels = c('perceptual','semantical'), 
                    values=c("lightsteelblue", "deepskyblue4"))+ 
  labs(x = "relatedness rating", y = "density") +
  theme_classic()+
  theme(panel.spacing.x = unit(2, "lines"),
        axis.title.y=element_text(size=16, 
                                  family="Encode Sans Condensed"),
        axis.text.y = element_text(size = 12, colour="black", 
                                   family = "Encode Sans Condensed"),
        axis.title.x=element_text(size=16, family = "Encode Sans Condensed"),
        axis.text.x = element_text(size = 12, colour="black", 
                                   family="Encode Sans Condensed"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, 
                                   family = "Encode Sans Condensed"),
        legend.position = "top",
        axis.line =  element_line(size=1.75),
        axis.ticks = element_line(size=1.75, colour="black"),
        axis.ticks.length = unit(.2,"cm"),
        strip.text = element_text(size=16, 
                                  family = "Encode Sans Condensed"),
        strip.background=element_rect(color="white"))

dev.off()
# INCREASE IN PROBABILITY FOR A FALSE ALARM ON DAY 2 DEPENDING ON RELATEDNESS RATING ON DAY 3 ####
# prepare data ####
# long file for LMM
simRatingsDf <- subset(behavDf, itemType != "old") %>% 
  select("Name", "set", "stimulusTypeNum", "delay", "emotion", 
         "itemType", "FA", "semRating", "percRating") %>% 
  mutate(lureType = factor(itemType, levels=c("new","sem","per")))

# group mean centering ratings

simRatingsDf_1d <- simRatingsDf %>% 
  filter(delay == '1d')

simRatingsDf_1d$semRatingGroupMeanCent <- scale(simRatingsDf_1d$semRating, scale=FALSE)
simRatingsDf_1d$percRatingGroupMeanCent <- scale(simRatingsDf_1d$percRating, scale=FALSE)

simRatingsDf_28d <- simRatingsDf %>% 
  filter(delay == '28d')

simRatingsDf_28d$semRatingGroupMeanCent <- scale(simRatingsDf_28d$semRating, scale=FALSE)
simRatingsDf_28d$percRatingGroupMeanCent <- scale(simRatingsDf_28d$percRating, scale=FALSE)

simRatingsDf <- rbind(simRatingsDf_1d, simRatingsDf_28d)

# analyze data ####
# fit binomial generalized linear mixed model with group mean centered ratings 
glmm <- glmer(FA ~ delay*semRatingGroupMeanCent*percRatingGroupMeanCent*emotion + 
                (1 | Name) + (1 | stimulusTypeNum), data = simRatingsDf, family = "binomial",
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(glmm)

# FOR COMPARISON: fit binomial generalized linear mixed model with grand mean centered ratings 
# grand mean centering ratings 
simRatingsDf$semRatingGrandMeanCent <- scale(simRatingsDf$semRating, scale=FALSE)
simRatingsDf$percRatingGrandMeanCent <- scale(simRatingsDf$percRating, scale=FALSE)

# fit gLMMM
glmm <- glmer(FA ~ delay*semRatingGrandMeanCent*percRatingGrandMeanCent*emotion + 
                (1 | Name) + (1 | stimulusTypeNum), data = simRatingsDf, family = "binomial",
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(glmm) # no change

# Figure 2C right panel #####
# perceptual relatedness 

svg("Figure2CRight_perceptualRelatedness.svg")

p <- plot_model(glmm_groupMeanCent, type = "pred", terms = c("percRatingGroupMeanCent","emotion","delay"),
                show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                dot.size = 4, grid = FALSE, line.size = 2, 
                axis.title = c("perceptual relatedness","% false alarms in delayed recognition"), #set x-and y-axis title
                colors = c("azure4", "firebrick4")) + 
  p + 
  ggMarginal(p, type="density")
coord_cartesian( ylim = c(0, 0.16)) +
  my_theme

dev.off()

# semantic relatedness 

svg("Figure2CRight_semanticRelatedness.svg")

p <- plot_model(glmm_groupMeanCent, type = "pred", terms = c("semRatingGroupMeanCent","emotion","delay"), 
                dot.size = 4, grid = FALSE, line.size = 3, 
                axis.title = c("semantic relatedness","probability of a false alarm in %"), #set x-and y-axis title
                colors = c("azure4", "firebrick4")) 
p + 
  coord_cartesian( ylim = c(0, 0.16)) +
  my_theme

dev.off()

# FA FOR SEMANTICALLY RELATED LURES LOW VS HIGH IN SEMANTIC RELATEDNESS ####
#median split
median <- median(simRatingsDf$percRating) #compute median of perceptual relatedness rating
median

simRatingsDf$percGroup <- simRatingsDf$percRating #new variable for median split the perceptual relatedness rating
simRatingsDf$percGroup <- ifelse(simRatingsDf$percGroup <= median, 'lowpercRating', 'highpercRating') #group perceptual relatedness rating depending whether low/equal or higher than median


# prepare file for glmm
semOnlyDf <- subset(simRatingsDf, lureType == "sem") %>%  # only semantically related lures
  dplyr::select(Name, delay, semRating, emotion, stimulusTypeNum, FA, percGroup) %>%
  mutate(percGroup = factor(percGroup, levels=c("lowpercRating","highpercRating")))

# run glmm
glmm <- glmer(FA ~ percGroup*delay*emotion + 
                (1 | Name) + (1 | stimulusTypeNum), 
              data = semOnlyDf, 
              family = "binomial",
              control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) 
summary(glmm) #no difference of high or low perceptual relatedness on the probability for a false alarm

# MODEL-BASED REPRESENTATIONAL-SIMILARITY ANALYSIS####
# HIPPOCAMPAL LONG AXIS ####
# LEFT HIPPOCAMPUS ######
# prepare data ####
longaxisL_modelRSADf <- modelRSADf %>% 
  dplyr::select(Name, model, emotion, delay, 
                anteriorHC_L, posteriorHC_L) %>%
  pivot_longer(cols = c(anteriorHC_L, posteriorHC_L),
               names_to = "longaxis",
               values_to = "fit") %>%
  mutate(longaxis = factor(longaxis))

# analyze data ####

#run ANOVA 
results.ANOVA <- aov_ez(
  "Name"
  ,"fit"
  ,longaxisL_modelRSADf
  ,between=c("delay")
  ,within=c("emotion","model","longaxis")
  ,anova_table="pes")
print(results.ANOVA) ### --> trend for delay x model x longaxis interaction, sign. delay x model x long axis x emotion interaction
summary(results.ANOVA)

# post hoc tests 
#anterior vs posterior separately for both groups
lHC.emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*longaxis, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(lHC.emmeans, adjusts="sidak") #adjustment if necessary

# significant decrease in fit from 1d to 28d in anterior HC (after exclusion from one outlier) in model 1
# significant decrease in fit from 1d to 28d in anterior HC (after exclusion from one outlier) in model 2

#anterior vs posterior separately for both groups
lHC.emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*emotion*longaxis, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(lHC.emmeans, adjusts="sidak") #adjustment if necessary

# trend p = 0.760 in model 2 (1d-28d, aHC) that disappears after outlier exclusion, i.e. is driven by one outlier

# test influence of outliers #####
# look for outliers ####
data <- modelRSADf %>% 
  group_by(Name) %>% 
  dplyr::select(Name, model, emotion, delay, anteriorHC_L, posteriorHC_L) 

#anterior HC
#model 1
log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#1

log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#1 outlier 3L

#model 2
log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
# # 1 outlier 

#model 3
log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
data$anteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none 

log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
data$anteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
data$anteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
data$anteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#1 outlier 3L

#posterior HC
#model 1
log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#1 outlier

#model 2
log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#1 outlier 

log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
#none

#model 3
log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
data$posteriorHC_L[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
data$posteriorHC_L[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
data$posteriorHC_L[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
data$posteriorHC_L[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

# prepare data ####
# remove outlier 
log = data$anteriorHC_L == 999
data$anteriorHC_L[log] = NA

log = data$posteriorHC_L == 999
data$posteriorHC_L[log] = NA

# prepare data without outlier 
longaxisL_modelRSADf <- data %>% 
  dplyr::select(Name, model, emotion, 
                delay, anteriorHC_L, posteriorHC_L) %>%
  pivot_longer(cols = c(anteriorHC_L, posteriorHC_L),
               names_to = "longaxis",
               values_to = "fit") %>%
  mutate(longaxis = factor(longaxis))

# analyze data ####
# run ANOVA before outlier exclusion
results.ANOVA <- aov_ez(
  "Name"
  ,"fit"
  ,longaxisL_modelRSADf
  ,between=c("delay")
  ,within=c("emotion","model","longaxis")
  ,anova_table="pes")
print(results.ANOVA) # -> not consistent with previous data (outlier sj07 biased results, especially for model 2) -> outlier corrected data further used
summary(results.ANOVA)

# post hoc tests
# anterior vs posterior separately for both groups
lHC.emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*longaxis, 
                        lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(lHC.emmeans, adjusts="sidak") #adjustment if necessary


lHC.emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*emotion*longaxis, 
                        lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(lHC.emmeans, adjusts="sidak") #adjustment if necessary

meanDf <- aggregate(fit ~ Name + delay + model + longaxis, 
                    FUN = mean, data = longaxisL_modelRSADf)

# anterior 
### model 1: 1d vs 28d
with(meanOverEmo, cohensD(x = fit[delay == "1d" & longaxis == "anteriorHC_L" 
                                  & model == "model3"], 
                          y = fit[delay == "28d" & longaxis == "anteriorHC_L" 
                                  & model == "model3"],
                          method = 'unequal'))

### model 3: 1d vs 28d
with(meanOverEmo, cohensD(x = fit[delay == "1d" & longaxis == "anteriorHC_L" 
                                  & model == "model1"], 
                          y = fit[delay == "28d" & longaxis == "anteriorHC_L" 
                                  & model == "model1"],
                          method = 'unequal'))

### model 1 negative: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay == "1d" & longaxis == "anteriorHC_L" 
                                           & model == "model1" & emotion == "negative"], 
                                   y = fit[delay == "28d" & longaxis == "anteriorHC_L" 
                                           & model == "model1" & emotion == "negative"], 
                                   method = 'unequal'))

### model 1 neutral: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay == "1d" & longaxis == "anteriorHC_L" 
                                           & model == "model1" & emotion == "neutral"], 
                                   y = fit[delay == "28d" & longaxis == "anteriorHC_L" 
                                           & model == "model1" & emotion == "neutral"], 
                                   method = 'unequal'))


### model 3: 1d vs 28d
with(meanOverEmo, cohensD(x = fit[delay == "1d" & longaxis == "anteriorHC_L" 
                                  & model == "model3"], 
                          y = fit[delay == "28d" & longaxis == "anteriorHC_L" 
                                  & model == "model3"], 
                          method = 'unequal'))

### model 3 negative: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay == "1d" & longaxis == "anteriorHC_L" 
                                           & model == "model3" & emotion == "negative"], 
                                   y = fit[delay == "28d" & longaxis == "anteriorHC_L" 
                                           & model == "model3" & emotion == "negative"], 
                                   method = 'unequal'))

### model 3 neutral: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay == "1d" & longaxis == "anteriorHC_L" 
                                           & model == "model3" & emotion == "neutral"], 
                                   y = fit[delay == "28d" & longaxis == "anteriorHC_L" 
                                           & model == "model3" & emotion == "neutral"], 
                                   method = 'unequal'))


### model 2: 1d vs 28d
with(meanOverEmo, cohensD(x = fit[delay == "1d" & longaxis == "anteriorHC_L" 
                                  & model == "model2"], 
                          y = fit[delay == "28d" & longaxis == "anteriorHC_L" 
                                  & model == "model2"], 
                          method = 'unequal'))

### model 2 neutral: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay== "1d" & longaxis=="anteriorHC_L" 
                                           & model=="model2" & emotion=="neutral"],
                                   y = fit[delay=="28d" & longaxis=="anteriorHC_L" 
                                           & model=="model2" & emotion=="neutral"],
                                   method = 'unequal'))

### model 2 negative: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay== "1d" & longaxis=="anteriorHC_L" 
                                           & model=="model2" & emotion=="negative"],
                                   y = fit[delay=="28d" & longaxis=="anteriorHC_L" 
                                           & model=="model2" & emotion=="negative"],
                                   method = 'unequal'))

# posterior 
### model 1: 1d vs 28d
with(meanOverEmo, cohensD(x = fit[delay== "1d" & longaxis=="posteriorHC_L" 
                                  & model=="model1"],
                          y = fit[delay=="28d" & longaxis=="posteriorHC_L" 
                                  & model=="model1"],
                          method = 'unequal'))

### model 1 negative: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay== "1d" & longaxis=="posteriorHC_L" 
                                           & model=="model1" & emotion=="negative"],
                                   y = fit[delay=="28d" & longaxis=="posteriorHC_L" 
                                           & model=="model1" & emotion=="negative"],
                                   method = 'unequal'))

### model 1 neutral: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay== "1d" & longaxis=="posteriorHC_L" 
                                           & model=="model1" & emotion=="neutral"],
                                   y = fit[delay=="28d" & longaxis=="posteriorHC_L" 
                                           & model=="model1" & emotion=="neutral"],
                                   method = 'unequal'))


### model 3: 1d vs 28d
with(meanOverEmo, cohensD(x = fit[delay== "1d" & longaxis=="anteriorHC_L" 
                                  & model=="model3"],
                          y = fit[delay=="28d" & longaxis=="anteriorHC_L" 
                                  & model=="model3"],
                          method = 'unequal'))


### model 3 negative: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay== "1d" & longaxis=="anteriorHC_L" 
                                           & model=="model3" & emotion=="negative"],
                                   y = fit[delay=="28d" & longaxis=="anteriorHC_L" 
                                           & model=="model3" & emotion=="negative"],
                                   method = 'unequal'))

### model 3 neutral: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay== "1d" & longaxis=="anteriorHC_L" 
                                           & model=="model3" & emotion=="neutral"],
                                   y = fit[delay=="28d" & longaxis=="anteriorHC_L" 
                                           & model=="model3" & emotion=="neutral"],
                                   method = 'unequal'))

### model 2: 1d vs 28d
with(meanOverEmo, cohensD(x = fit[delay== "1d" & longaxis=="anteriorHC_L" 
                                  & model=="model2"],
                          y = fit[delay=="28d" & longaxis=="anteriorHC_L" 
                                  & model=="model2"],
                          method = 'unequal'))

### model 2 neutral: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay== "1d" & longaxis=="posteriorHC_L" 
                                           & model=="model2" & emotion=="negative"],
                                   y = fit[delay=="28d" & longaxis=="posteriorHC_L" 
                                           & model=="model2" & emotion=="negative"],
                                   method = 'unequal'))

### model 2 negative: 1d vs 28d
with(longaxisL_modelRSADf, cohensD(x = fit[delay== "1d" & longaxis=="posteriorHC_L" 
                                           & model=="model2" & emotion=="neutral"],
                                   y = fit[delay=="28d" & longaxis=="posteriorHC_L" 
                                           & model=="model2" & emotion=="neutral"],
                                   method = 'unequal'))

# Figure 3B ####
#drop outlier for whole plot as it is also dropped for the ANOVA 
#don't run this line if you want to see data with outlier
data<-data[!(data$Name=="sj07"),] 

my_y_title <- expression(paste("Fisher transformed", italic("rho")))

# model 1
plot_data <- subset(data, model == "model1") %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

svg("Figure4B_laHC_model1.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=anteriorHC_L, fill=emotion))+
  facet_grid(~ model)+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  annotate(geom="text", x=1.5, y=0.052, label= c("*"), color="black", size = 15) +
  geom_segment(aes(x=1,xend=2,y=0.046,yend=0.046), size=1.5) +
  labs(y=my_y_title)+ coord_cartesian( ylim =c(-0.06, 0.10)) +
  my_theme

dev.off()

svg("Figure4B_lpHC_model1.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=posteriorHC_L, fill=emotion))+
  facet_grid(~ model)+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.2) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.06, 0.10)) +
  my_theme

dev.off()

# model 2
plot_data <- subset(data, model == "model2") %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

svg("Figure4B_laHC_model2.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=anteriorHC_L, fill=emotion))+
  facet_grid(~ model)+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y=my_y_title)+ coord_cartesian(ylim =c(-0.06, 0.10)) +
  my_theme

dev.off()

svg("Figure4B_lpHC_model2.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=posteriorHC_L, fill=emotion))+
  facet_grid(~ model)+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.06, 0.10)) +
  my_theme

dev.off()

# model 3
plot_data <- subset(data, model == "model3") %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

svg("Figure4B_laHC_model3.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=anteriorHC_L, fill=emotion))+
  facet_grid(~ model)+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  annotate(geom="text", x=1.5, y=0.052, label= c("* *"), color="black", size = 15) +
  geom_segment(aes(x=1,xend=2,y=0.046,yend=0.046), size=1.5) +
  labs(y=my_y_title)+ coord_cartesian( ylim =c(-0.06, 0.10)) +
  theme_classic()+
  my_theme

dev.off()

svg("Figure4B_lpHC_model3.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=posteriorHC_L, fill=emotion))+
  facet_grid(~ model)+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.06, 0.10)) +
  my_theme

dev.off()

# RIGHT HIPPOCAMPUS ####
# prepare data ####
longaxisR_modelRSADf <- modelRSADf %>% 
  dplyr::select(Name, model, emotion, delay, 
                anteriorHC_R, posteriorHC_R) %>%
  pivot_longer(cols = c(anteriorHC_R, posteriorHC_R),
               names_to = "longaxis",
               values_to = "fit") %>%
  mutate(longaxis = factor(longaxis))

# analyze data ####
# run ANOVA 
results.ANOVA <- aov_ez(
  "Name"
  ,"fit"
  ,longaxisR_modelRSADf
  ,between=c("delay")
  ,within=c("emotion","model","longaxis")
  ,anova_table="pes")
print(results.ANOVA)
summary(results.ANOVA)
# post hoc tests 
#anterior vs posterior separately for both groups
rHC.emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*emotion*longaxis, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(rHC.emmeans, adjusts="sidak") #adjustment if necessary

# check influence of outliers #####
# look for outliers ####
data <- modelRSADf

#anterior HC
#model 1
log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#3

#model 2
log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
# 1 outlier

#model 3
log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
data$anteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
# 1 outlier

log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
data$anteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
data$anteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
data$anteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

#posterior HC
#model 1
log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#3

#model 2
log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
#none

#model 3
log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
data$posteriorHC_R[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
data$posteriorHC_R[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
data$posteriorHC_R[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
data$posteriorHC_R[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

# prepare data ####
# drop outlier
# removing sj07 and sj10
log = data$anteriorHC_R == 999
data$anteriorHC_R[log] = NA

log = data$posteriorHC_R == 999
data$posteriorHC_R[log] = NA

# prepare data without outliers 
longaxisR_modelRSADf <- data %>% 
  dplyr::select(Name, model, emotion, 
                delay, anteriorHC_R, posteriorHC_R) %>%
  pivot_longer(cols = c(anteriorHC_R, posteriorHC_R),
               names_to = "longaxis",
               values_to = "fit") %>%
  mutate(longaxis = factor(longaxis))

# analyze data ####
# run ANOVA without outliers
results.ANOVA <- aov_ez(
  "Name"
  ,"fit"
  ,longaxisR_modelRSADf
  ,between=c("delay")
  ,within=c("emotion","model","longaxis")
  ,anova_table="pes")
print(results.ANOVA) # removing 2 outliers (sj07 and sj10) 
summary(results.ANOVA)

# post hoc tests after removing outliers
#anterior vs posterior separately for both groups
rHC.emmeans <- emmeans (results.ANOVA,pairwise ~ delay|model*emotion*longaxis, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(rHC.emmeans, adjusts="sidak") #adjustment if necessary
eff_size(rHC.emmeans, sigma = Inf, edf = 50) #sigma is the smallest error SS from model

# NEOCORTEX#####
# neocortical memory ROI #####
# analyze data ####

# ANOVA 
neocort.ANOVA <- aov_ez(
  "Name"
  ,"neocorticalMemoryROI"
  ,modelRSADf
  ,between=c("delay")
  ,within=c("emotion","model")
  ,anova_table="pes")
print(neocort.ANOVA)
summary(neocort.ANOVA)

# post hoc tests 

neocort.emmeans <- emmeans (neocort.ANOVA, pairwise ~ delay|model, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(neocort.emmeans, adjusts="sidak") #adjustment if necessary

neocort.emmeans <- emmeans (neocort.ANOVA, pairwise ~ model|delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(neocort.emmeans, adjusts="sidak") #adjustment if necessary

#prepare Df
meanDf <- aggregate(neocorticalMemoryROI ~ Name + delay + model, FUN = mean, data = modelRSADf)

# Calculate Cohen's d for model2
with(meanDf, cohensD(x = neocorticalMemoryROI[delay=="1d" & model=="model2"], 
                     y = neocorticalMemoryROI[delay=="28d" & model=="model2"], 
                     method = "unequal"))

# Calculate Cohen's d for model3
with(meanDf, cohensD(x = neocorticalMemoryROI[delay=="1d" & model=="model3"], 
                     y = neocorticalMemoryROI[delay=="28d" & model=="model3"], 
                     method = "unequal"))

# Calculate Cohen's d for model1
with(meanDf, cohensD(x = neocorticalMemoryROI[delay=="1d" & model=="model1"], 
                     y = neocorticalMemoryROI[delay=="28d" & model=="model1"], 
                     method = "unequal"))

#difference in fit between models in seperate delay groups
neocort.emmeans <- emmeans (neocort.ANOVA, pairwise ~ model|delay, 
                            lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(neocort.emmeans, adjusts="sidak") #adjustment if necessary

# Calculate Cohen's d for model1 vs model2 at delay 1d
with(meanDf, cohensD(x = neocorticalMemoryROI[delay=="1d" & model=="model1"], 
                     y = neocorticalMemoryROI[delay=="1d" & model=="model2"], 
                     method = "paired"))

# Calculate Cohen's d for model2 vs model3 at delay 1d
with(meanDf, cohensD(x = neocorticalMemoryROI[delay=="1d" & model=="model2"], 
                     y = neocorticalMemoryROI[delay=="1d" & model=="model3"], 
                     method = "paired"))

# Calculate Cohen's d for model2 vs model1 at delay 28d
with(meanDf, cohensD(x = neocorticalMemoryROI[delay=="28d" & model=="model2"], 
                     y = neocorticalMemoryROI[delay=="28d" & model=="model1"], 
                     method = "paired"))

# Calculate Cohen's d for model2 vs model3 at delay 28d
with(meanDf, cohensD(x = neocorticalMemoryROI[delay=="28d" & model=="model2"], 
                     y = neocorticalMemoryROI[delay=="28d" & model=="model3"], 
                     method = "paired"))

# check influence of outliers #####
# look for outliers 

data <- modelRSADf

#model 1
log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"]))))
data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"]))))
data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"]))))
data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#none

log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"]))))
data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model1"][log1] = 999
#none

#model 2
log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"]))))
data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"]))))
data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"]))))
data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
#none

log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"]))))
data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model2"][log1] = 999
# none

#model 3
log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"]))))
data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none 

log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"]))))
data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "neutral" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))|  data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"]))))
data$neocorticalMemoryROI[data$delay == "1d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

log1 <- which(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  > (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))|  data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]  < (mean(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"])+ (-3*sd(data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"]))))
data$neocorticalMemoryROI[data$delay == "28d" & data$emotion == "negative" & data$model == "model3"][log1] = 999
#none

# no outlier!

# Figure 3C upper panel #####

my_y_title <- expression(paste("Fisher transformed", italic(" rho")))

svg("Figure3C_neocort_model1.svg")

plot_data <- behavDf_model1 %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=neocorticalMemoryROI, fill=emotion))+
  facet_grid(~ model)+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
  my_theme

dev.off()


svg("Figure3C_neocort_model2.svg")

plot_data <- behavDf_model2 %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=neocorticalMemoryROI, fill=emotion))+
  facet_grid(~ model)+
  stat_summary(stat = 'identity', fun='mean',geom='bar', 
               position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  # delay effect
  annotate(geom="text", x=1.5, y=0.11, label= c("*"), color="black",  size = 15) +
  geom_segment(aes(x=1,xend=2,y=0.1,yend=0.1), size=1.5) +
  
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
  my_theme

dev.off()


svg("Figure4C_neocort_model3.svg")

plot_data <- behavDf_model3 %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=neocorticalMemoryROI, 
                                 fill=emotion))+
  facet_grid(~ model)+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  position=position_dodge(0.9),
               width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
  my_theme

dev.off()

# individual ROIs ####
# vmPFC ####
vmPFC.ANOVA <- aov_ez(
  "Name"
  ,"vmPFC"
  ,subset(modelRSADf, model == "model2")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(vmPFC.ANOVA)
summary(vmPFC.ANOVA)

vmpPFC.emmeans <- emmeans (vmPFC.ANOVA, pairwise ~ delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(vmpPFC.emmeans, adjust="sidak") #adjustment if necessary

meanDf <- aggregate(vmPFC ~ Name + delay, FUN = mean, 
                    data = subset(modelRSADf, model == "model2"))

with(meanDf, cohensD(x = vmPFC[delay=="1d"], 
                     y = vmPFC[delay=="28d"], 
                     method="unequal"))

# IFG####
IFG.ANOVA <- aov_ez(
  "Name"
  ,"IFG"
  ,subset(modelRSADf, model == "model2")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(IFG.ANOVA)
summary(IFG.ANOVA)

# aCC ####
ACC.ANOVA <- aov_ez(
  "Name"
  ,"aCC"
  ,subset(modelRSADf, model == "model2")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(ACC.ANOVA)
summary(ACC.ANOVA)
# Angular Gyrus####
# right #####
angularGyrus_R.ANOVA <- aov_ez(
  "Name"
  ,"angularGyrus_R"
  ,subset(modelRSADf, model == "model2")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(angularGyrus_R.ANOVA)
summary(angularGyrus_R.ANOVA)

angularGyrus_R.emmeans <- emmeans (angularGyrus_R.ANOVA, pairwise ~ delay, 
                                   lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(angularGyrus_R.emmeans, adjust="sidak") #adjustment if necessary

meanDf <- aggregate(angularGyrus_R ~ Name + delay, FUN = mean, 
                    data = subset(modelRSADf, model == "model2"))

with(meanDf, cohensD(x = angularGyrus_R[delay=="1d"], 
                     y = angularGyrus_R[delay=="28d"]), 
     method = "unequal")

# left ####
angularGyrus_L.ANOVA <- aov_ez(
  "Name"
  ,"angularGyrus_L"
  ,subset(modelRSADf, model == "model2")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(angularGyrus_L.ANOVA)
summary(angularGyrus_L.ANOVA)

# Precuneus #####

Precuneus.ANOVA <- aov_ez(
  "Name"
  ,"Precuneus"
  ,subset(modelRSADf, model == "model2")
  ,between=c("delay")
  ,within=c("emotion")
  ,anova_table="pes")
print(Precuneus.ANOVA)
summary(Precuneus.ANOVA)

Precuneus.emmeans <- emmeans (Precuneus.ANOVA, pairwise ~ delay, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(Precuneus.emmeans, adjust="sidak") #adjustment if necessary

meanDf <- aggregate(Precuneus ~ Name + delay, FUN = mean, 
                    data = subset(modelRSADf, model == "model2"))

with(meanDf, cohensD(x = Precuneus[delay=="1d"], 
                     y = Precuneus[delay=="28d"]), 
     method = "unequal")

# Figure 4C lower panel #####
plot_data <- subset(modelRSADf, model == "model2") %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

svg("Figure4C_Precuneus.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=Precuneus, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  annotate(geom="text", x=1.5, y=0.165, label= c("+"), color="black", size = 15) +
  geom_segment(aes(x=1,xend=2,y=0.15,yend=0.15), size=1.5) +
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
  my_theme

dev.off()


svg("Figure4C_AGR.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=angularGyrus_R, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 

p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  annotate(geom="text", x=1.5, y=0.165, label= c("*"), color="black", 
           fontface = "bold", size = 15) +
  geom_segment(aes(x=1,xend=2,y=0.15,yend=0.15), size=1.5) +
  labs(y=my_y_title)+coord_cartesian( ylim = c(-0.1, 0.3)) +
  theme_classic()+
  my_theme

dev.off()

svg("Figure4C_AGL.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=angularGyrus_R, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 

p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y=my_y_title)+coord_cartesian( ylim = c(-0.1, 0.3)) +
  my_theme

dev.off()

svg("Figure4C_vmPFC.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=vmPFC, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  annotate(geom="text", x=1.5, y=0.165, label= c("*"), color="black", 
           fontface = "bold", size = 15) +
  geom_segment(aes(x=1,xend=2,y=0.15,yend=0.15), size=1.5) +
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
  my_theme

dev.off()


svg("Figure3C_IFG.svg")
p <- ggplot (data=plot_data, aes(x=delay, y=IFG, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
  my_theme

dev.off()


svg("Figure3C_aCC.svg")

p <- ggplot (data=plot_data, aes(x=delay, y=aCC, fill=emotion))+
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y=my_y_title)+ coord_cartesian( ylim = c(-0.1, 0.3)) +
  my_theme

dev.off()

# sensory control ROIs #####
# occipital pole ####

results.ANOVA <- aov_ez(
  "Name"
  ,"occPole"
  ,modelRSADf
  ,between=c("delay")
  ,within=c("emotion","model")
  ,anova_table="pes")
print(results.ANOVA)
summary(results.ANOVA)

results.emmeans <- emmeans (results.ANOVA, pairwise ~ model, lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(results.emmeans, adjust="sidak") #adjustment if necessary

meanDf <- aggregate(occPole ~ Name + model, FUN = mean, 
                    data = modelRSADf) # mean over emotion and delay

with(meanDf, cohensD(occPole[mean$model=='model1'], 
                     occPole[mean$model=='model3'], 
                     method="paired"))

with(meanDf, cohensD(occPole[model=='model2'], 
                     occPole[model=='model3'], 
                     method="paired"))

# Heschl's gyrus ####

heschl.ANOVA <- aov_ez(
  "Name"
  ,"HeschlGyrus"
  ,modelRSADf
  ,between=c("delay")
  ,within=c("emotion","model")
  ,anova_table="pes")
print(heschl.ANOVA)
summary(heschl.ANOVA)

# MEMORY REINSTATEMENT OVER TIME #####
# HIPPOCAMPAL LONGAXIS #####  
# left hippocampus ####
# prepare data ####
# for analysis of delay-dependent change along the hippocampus
longaxisL_ERSDf <- subset(reinstatementDf, itemType == 'old' & EncRuns == 'AllEncRuns') %>%
  select(Name, set, delay, emotion, 
         anteriorHC_L,posteriorHC_L)%>%
  dplyr::rename(anterior = anteriorHC_L, posterior = posteriorHC_L)%>%
  pivot_longer(cols=c(anterior,posterior),
               names_to="longaxis",
               values_to = "ERS")%>%
  mutate(longaxis = factor(longaxis))

# for analyses dependening on behavior  
behavDf$reactionTime <- behavDf$reactionTimeLastAnswer

smallerBehavDf <- subset(behavDf, itemType == "old", 
                         select = c("Name", "set", "delay", "emotion", 
                                    "detailed", "miss", "sem_FA", "per_FA","hit",
                                    "reactionTime"))%>%
  mutate(detailed = factor(detailed),
         hit = factor(hit),
         sem_FA = factor(sem_FA),
         per_FA = factor(per_FA))

mergedERSDf <- merge(longaxisL_ERSDf, 
                     smallerBehavDf, by = c("Name","delay","emotion","set")) 

# change in memory reinstatement over time ####
# analyze data ####
# run linear mixed model
LMM_lHC <- lmer(ERS ~ delay*emotion*longaxis +
                  (1 | Name) + (1 | set), 
                data = longaxisL_ERSDf)
summary(LMM_lHC)

# hits only
hitsLongaxisL_ERSDf <- subset(mergedERSDf, hit == "1")

LMM_lHC_hit <- lmer(ERS ~ delay*emotion*longaxis +
                      (1 | Name) + (1 | set), data = hitsLongaxisL_ERSDf)
summary(LMM_lHC_hit)

# Figure 4 right upper panel #####
# anterior hippocampus 
svg("Figure4Upper_leftAnteriorHC.svg")

plot_data <- subset(longaxisL_ERSDf, longaxis == "anterior")%>%
  aggregate(ERS ~ Name + delay + emotion, 
            FUN = mean) %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=ERS, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y="Fisher transformed r")+ coord_cartesian( ylim = c(-0.02, 0.05)) +
  my_theme

dev.off()

# posterior hippocampus
svg("Figure4Upper_leftPosteriorHC.svg")

plot_data <- subset(longaxisL_ERSDf, longaxis == "posterior")%>%
  aggregate(ERS ~ Name + delay + emotion, 
            FUN = mean) %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=ERS, fill=emotion))+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y="Fisher transformed r")+ coord_cartesian( ylim = c(-0.02, 0.05)) +
  annotate(geom="text", x=1.5, y=0.11, label= c("* *"), 
           color="black", fontface = "bold", size = 15) +
  geom_segment(aes(x=1,xend=2,y=0.1,yend=0.1), size=1.5) +
  my_theme

dev.off()

# association of remote posterior ERS with memory semantization #####
# prepare data ####
postHCERSDf <- subset(mergedERSDf, longaxis == "posterior")

# analyze data ####
# semantically related lures 
glmm_semFA <- glmer(sem_FA ~ ERS*emotion*delay +
                      (1| Name)+(1|set), data =  postHCERSDf, family = "binomial", 
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5))) 
summary(glmm_semFA)

# perceptually related lures 
glmm_perFA <- glmer(per_FA ~ ERS*emotion*delay +
                      (1| Name)+(1|set), data =  postHCERSDf, 
                    family = "binomial", 
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl=list(maxfun=2e5))) 
summary(glmm_perFA)

# Figure 4 lower right panel ####
# semantically related lures
svg("Figure4lower_semanticallyRelated.svg")

p <- plot_model(glmm_semFA, type = "eff", terms = c("ERS","delay"), 
                show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                dot.size = 4, grid = FALSE, line.size = 3, 
                axis.title = c("ERS in Fisher transformed r",
                               "probability for a false alarm in %"), #set x-and y-axis title
                colors = c("deepskyblue4", "deepskyblue4"))  
p + 
  coord_cartesian( ylim = c(0, 0.50)) +
  my_theme

dev.off()

# perceptually related lures
svg("Figure4lower_perceptuallyRelated.svg")

p <- plot_model(glmm_perFA, type = "eff", terms = c("ERS","delay"), 
                show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                dot.size = 4, grid = FALSE, line.size = 3, 
                axis.title = c("ERS in Fisher transformed r",
                               "probability for a false alarm in %"), #set x-and y-axis title
                colors = c("lightsteelblue", "lightsteelblue"))  
p + 
  coord_cartesian( ylim = c(0, 0.50)) +
  my_theme

dev.off()

# additional analyses ####
# hits
glmm <- glmer(hit ~ ERS*emotion*delay +
                (1| Name)+(1|set), data = postHCERSDf, 
              family = "binomial", 
              control=glmerControl(optimizer="bobyqa",
                                   optCtrl=list(maxfun=2e5))) 
summary(glmm)

# detailed   
glmm_detailed <- glmer(detailed ~ ERS*emotion*delay +
                         (1| Name)+(1|set), data = postHCERSDf, 
                       family = "binomial", 
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e5))) 
summary(glmm_detailed)

# reaction time
LMM <- lmer(reactionTime ~ ERS*delay*emotion +
              (1 | Name)+(1|set), data = postHCERSDf)
summary(LMM)#

# Supplementary Figure 4 
svg("S4_ERSandDetailedMemory.svg", height = 8, width = 12)

p <- plot_model(glmm_detailed, type = "pred", terms = c("ERS[all]","emotion","delay"), 
                show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                dot.size = 4, grid = FALSE, line.size = 3, 
                axis.title = c("ERS in Fisher transformed r",
                               "probability of detailed memory in %"), #set x-and y-axis title
                colors = (values=c("azure4", "firebrick4")))  
p + 
  coord_cartesian( ylim = c(0, 1)) + 
  ggtitle("posterior hippocampal ERS and detailed memory")+
  my_theme

dev.off()

# right hippocampus #####
# prepare data ####
longaxisR_ERSDf <- subset(reinstatementDf, itemType == 'old' & EncRuns == 'AllEncRuns') %>%
  select(Name, set, delay, emotion, 
         anteriorHC_R,posteriorHC_R)%>%
  dplyr::rename(anterior = anteriorHC_R, posterior = posteriorHC_R)%>%
  pivot_longer(cols=c(anterior,posterior),
               names_to="longaxis",
               values_to = "ERS")%>%
  mutate(longaxis = factor(longaxis))
# analyze data ####  
LMM <- lmer(ERS ~ delay*emotion*longaxis +
              (1 | set), data = longaxisR_ERSDf)
summary(LMM)#

# EXPLORE REINSTATEMENT BY RELATED STIMULI ######
# reinstatement by SEMANTICALLY related stimuli ####    
# prepare data ####
# long-file with longaxis as factor 
longaxisL_EncSemSimDf <-  subset(reinstatementDf, itemType == 'sem' & EncRuns == 'AllEncRuns') %>%
  select(Name, set, delay, emotion, 
         anteriorHC_L,posteriorHC_L)%>%
  dplyr::rename(anterior = anteriorHC_L, posterior = posteriorHC_L)%>%
  pivot_longer(cols=c(anterior,posterior),
               names_to="longaxis",
               values_to = "EncSemSim")%>%
  mutate(longaxis = factor(longaxis))
# analyze data ####
# run LMM    
LMM <- lmer(EncSemSim ~ delay*emotion*longaxis +
              (1 | Name) + (1 | set), data = longaxisL_EncSemSimDf)
summary(LMM)#

# post hoc tests
lHC.emmeans <- emmeans (LMM,pairwise ~ longaxis, lmerTest.limit = 6240, 
                        lmer.df = "satterthwaite") #satterwhaite for fastening up computation, does not change results of contrasts
summary(lHC.emmeans, adjusts="sidak") #adjustment if necessary

# Supplementary Figure 5 ####

svg("S5_semReinstatement.svg", width=10, height=5)

plot_data <- longaxisL_EncSemSimDf %>%
  aggregate(EncSemSim ~ Name + delay + emotion + longaxis, 
            FUN = mean) %>%
  mutate(x = case_when(
    delay == "1d" & emotion == "neutral" ~ 1 - dodge,
    delay == "1d" & emotion == "negative" ~ 1 + dodge,
    delay == "28d" & emotion == "neutral" ~ 2 - dodge,
    delay == "28d" & emotion == "negative" ~ 2 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=delay, y=EncSemSim, fill=emotion))+
  facet_grid(~ longaxis)+
  stat_summary(fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, delay)), alpha = 0.1) 
p + 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_x_discrete(labels=c("1d", "28d"))+
  scale_fill_manual(values=c("azure4", "firebrick4"))+
  labs(y="Fisher transformed r")+ coord_cartesian( ylim = c(-0.02, 0.05)) +
  my_theme

dev.off()

# reinstatement by PERCEPTUALLY related stimuli ####
# prepare data ####
longaxisL_EncPercSimDf <-  subset(reinstatementDf, itemType == 'per' & EncRuns == 'AllEncRuns') %>%
  select(Name, set, delay, emotion, 
         anteriorHC_L,posteriorHC_L)%>%
  dplyr::rename(anterior = anteriorHC_L, posterior = posteriorHC_L)%>%
  pivot_longer(cols=c(anterior,posterior),
               names_to="longaxis",
               values_to = "EncPercSim")%>%
  mutate(longaxis = factor(longaxis))

# analyze data ####
LMM <- lmer(EncPercSim ~ delay*emotion*longaxis +
              (1 | Name), data = longaxisL_EncPercSimDf)
summary(LMM)

# EXPLORE REINSTATEMENT IN THE NEOCORTEX ####
# ERS ####
ERSDf <-subset(reinstatementDf, itemType == 'old' & EncRuns == 'AllEncRuns')

LMM <- lmer(neocorticalMemoryROI ~ delay*emotion +
              (1 | Name)+(1|set), data = ERSDf)
summary(LMM)#

LMM <- lmer(angularGyrus_R ~ delay*emotion +
              (1 | Name), data = ERSDf)
summary(LMM)#

LMM <- lmer(angularGyrus_R ~ delay*emotion +
              (1 | Name)+(1|set), data = ERSDf)
summary(LMM)#

LMM <- lmer(Precuneus ~ delay*emotion +
              (1 | Name)+(1|set), data = ERSDf)
summary(LMM)#

LMM <- lmer(vmPFC ~ delay*emotion +
              (1 | Name), data = ERSDf)
summary(LMM)#

LMM <- lmer(IFG ~ delay*emotion +
              (1 | Name)+(1|set), data = ERSDf)
summary(LMM)#

LMM <- lmer(aCC ~ delay*emotion +
              (1 | Name), data = ERSDf)
summary(LMM)#

LMM <- lmer(occPole ~ delay*emotion +
              (1 | Name)+(1|set), data = ERSDf)
summary(LMM)#

LMM <- lmer(HeschlGyrus ~ delay*emotion +
              (1 | Name)+(1|set), data = ERSDf)
summary(LMM)#

# reinstatement by related lures #####

# reinstatement by semantically related lures ####
EncSemSimDf <-  subset(reinstatementDf, itemType == 'sem' & EncRuns == 'AllEncRuns')

LMM <- lmer(neocorticalMemoryROI ~ delay*emotion +
              (1 | Name)+(1|set), data = EncSemSimDf)
summary(LMM)#

LMM <- lmer(angularGyrus_R ~ delay*emotion +
              (1 | Name), data = EncSemSimDf)
summary(LMM)#

LMM <- lmer(angularGyrus_R ~ delay*emotion +
              (1 | Name)+(1|set), data = ESSDf)
summary(LMM)#

LMM <- lmer(Precuneus ~ delay*emotion +
              (1 | Name)+(1|set), data = ESSDf)
summary(LMM)#

LMM <- lmer(vmPFC ~ delay*emotion +
              (1 | Name), data = ESSDf)
summary(LMM)#

LMM <- lmer(IFG ~ delay*emotion +
              (1 | Name)+(1|set), data = ESSDf)
summary(LMM)#

LMM <- lmer(aCC ~ delay*emotion +
              (1 | Name), data = ESSDf)
summary(LMM)#

LMM <- lmer(occPole ~ delay*emotion +
              (1 | Name)+(1|set), data = ESSDf)
summary(LMM)#

LMM <- lmer(HeschlGyrus ~ delay*emotion +
              (1 | Name), data = ESSDf)
summary(LMM)#

# reinstatement by perceptually related lures ####
EncPercSimDf <-  subset(reinstatementDf, itemType == 'per' & EncRuns == 'AllEncRuns')

LMM <- lmer(neocorticalMemoryROI ~ delay*emotion +
              (1 | Name)+(1|set), data = EncPercSimDf)
summary(LMM)#

LMM <- lmer(angularGyrus_R ~ delay*emotion +
              (1 | Name), data = EncPercSimDf)
summary(LMM)#

LMM <- lmer(angularGyrus_R ~ delay*emotion +
              (1 | Name)+(1|set), data = EncPercSimDf)
summary(LMM)#

LMM <- lmer(Precuneus ~ delay*emotion +
              (1 | Name)+(1|set), data = EncPercSimDf)
summary(LMM)#

LMM <- lmer(vmPFC ~ delay*emotion +
              (1 | Name), data = EncPercSimDf)
summary(LMM)#

LMM <- lmer(IFG ~ delay*emotion +
              (1 | Name)+(1|set), data = EncPercSimDf)
summary(LMM)#

LMM <- lmer(aCC ~ delay*emotion +
              (1 | Name), data = EncPercSimDf)
summary(LMM)#

LMM <- lmer(occPole ~ delay*emotion +
              (1 | Name)+(1|set), data = EncPercSimDf)
summary(LMM)#

LMM <- lmer(HeschlGyrus ~ delay*emotion +
              (1 | Name)+(1|set), data = EncPercSimDf)
summary(LMM)#

# VALENCE & AROUSAL RATING####
# prepare data ####
emoRatingsDf <- aggregate(cbind(arousalRating, valenceRating) ~ Name + delay + emotion, 
                          FUN = mean, data = behavDf) 

# analyze data ####
psych::describeBy(cbind(arousalRating, valenceRating) ~ emotion, data = meanDf)

with(emoRatingsDf, t.test(arousalRating ~ emotion, paired = TRUE))
with(emoRatingsDf, cohensD(x = arousalRating[emotion == "negative"],
                           y = arousalRating[emotion == "neutral"], method = "paired"))

with(emoRatingsDf, t.test(valenceRating ~ emotion, paired = TRUE))
with(emoRatingsDf, cohensD(x = valenceRating[emotion == "negative"],
                           y = valenceRating[emotion == "neutral"], method = "paired"))

# DIFFERENCE BETWEEN D2 AND D3 IN DELAY BETWEEN GROUPS ####
describe(controlDf$delayD3)

with(controlDf, t.test(delayD3 ~ delay))
with(controlDf, cohensD(delayD3 ~ delay, method="unequal"))

# QUESTIONNAIRES ####
#BDI
with(controlDf, t.test(BDI_SUM ~ delay))
with(controlDf, cohensD(BDI_SUM ~ delay, method="unequal"))

#TICS
with(controlDf, t.test(TICS_SSCS ~ delay))
with(controlDf, cohensD(TICS_SSCS ~ delay, method="unequal"))

#STAI - state
with(controlDf, t.test(STAI_State_SUM ~ delay))
with(controlDf, cohensD(STAI_State_SUM ~ delay, method="unequal"))

#STAI- trait
with(controlDf, t.test(STAI_Trait_SUM ~ delay))
with(controlDf, cohensD(STAI_Trait_SUM ~ delay, method="unequal"))

#PSQI
PSQI_file <- controlDf %>%
  select(Name, delay, PSQI_comp1_sleepquality_28d, PSQI_24h_SUM,PSQI_24h_4, PSQI_24h_6) %>%
  drop_na()

psych::describeBy(PSQI_file$PSQI_24h_SUM , group = PSQI_file$delay)

#28d
with(controlDf, t.test(PSQI_28d_SUM ~ delay))
with(controlDf, cohensD(PSQI_28d_SUM ~ delay, method="unequal"))

#24h
with(controlDf, t.test(PSQI_24h_SUM ~ delay))
with(controlDf, cohensD(PSQI_24h_SUM ~ delay, method="unequal"))

#24h - sleep duration
with(controlDf, t.test(PSQI_24h_4 ~ delay))
with(controlDf, cohensD(PSQI_24h_4 ~ delay, method="unequal"))

#24h - sleep quality
with(controlDf, t.test(PSQI_24h_6 ~ delay))
with(controlDf, cohensD(PSQI_24h_6 ~ delay, method="unequal"))

# BEHAVIORAL PILOT####
# sociodemographics ####
# before exclusion/ dropout
describe(pilotDemoBeforeExclusionDf$age)
as.data.frame(table(pilotDemoBeforeExclusionDf$gender))

# final sample
pilotFinalDemoDf <- subset(pilotDemoBeforeExclusionDf, Name != 'pilot_62') ##drop participant who didn't finish task
describe(pilotFinalDemoDf$age)
as.data.frame(table(pilotFinalDemoDf$gender))

# results for all pilot sets####
# prepare data 
perRating_PilotDf  <- aggregate(rating ~ Name + lureType + emotion, FUN = mean, data = subset(pilotAllSetsDf, ratingScale == "Per"))
semRating_PilotDf  <- aggregate(rating ~ Name + lureType + emotion, FUN = mean, data = subset(pilotAllSetsDf, ratingScale == "Sem"))
# analyze data 
# perceptual relatedness Rating
# run ANOVA 
perRating.ANOVA <- aov_ez(
  "Name"
  ,"rating"
  ,perRating_PilotDf
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(perRating.ANOVA)
summary(perRating.ANOVA)
# post hoc tests 
#difference in perceptual relatedness rating between lures
perRating.emmeansLure <- emmeans (perRating.ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
summary(perRating.emmeansLure, adjust="sidak") # sidak adjustment when needed

# semantic relatedness Rating 
# run ANOVA 
semRating.ANOVA <- aov_ez(
  "Name"
  ,"rating"
  ,semRating_PilotDf
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(perRating.ANOVA)
summary(perRating.ANOVA)

# post hoc tests
semRating.emmeansLure <- emmeans (semRating.ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
summary(semRating.emmeansLure, adjust="sidak") # sidak adjustment when needed

# results of final sets ####
# descriptive statistics
meanDf <- aggregate(rating ~ Name + lureType + ratingScale, FUN = mean, 
                    data = pilotFinalSetsDf)    

psych::describeBy(rating ~ lureType + ratingScale, data = meanDf)

# perceptual relatedness Rating####
# run ANOVA 
perRating.ANOVA <- aov_ez(
  "Name"
  ,"rating"
  ,subset(pilotFinalSetsDf, ratingScale == "perceptual relatedness")
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(perRating.ANOVA)
summary(perRating.ANOVA)
# post hoc tests 
#difference in perceptual relatedness rating between lures
perRating.emmeansDelayLure <- emmeans (perRating.ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
summary(perRating.emmeansDelayLure, adjust="sidak") # sidak adjustment when needed

with(meanDf, cohensD(x = rating[lureType == "semantically related" & ratingScale == "perceptual relatedness"], 
                     y = rating[lureType == "perceptually related" & ratingScale == "perceptual relatedness"],
                     method="paired"))

with(meanDf, cohensD(x = rating[lureType == "perceptually related" & ratingScale == "perceptual relatedness"], 
                     y = rating[lureType == "unrelated" & ratingScale == "perceptual relatedness"],
                     method="paired"))

# semantic relatedness Rating ####
# run ANOVA
semRating.ANOVA <- aov_ez(
  "Name"
  ,"rating"
  ,subset(pilotFinalSetsDf, ratingScale == "semantic relatedness")
  ,within=c("emotion","lureType")
  ,anova_table="pes")
print(semRating.ANOVA)
summary(semRating.ANOVA)

# post hoc tests
semRating.emmeansDelayLure <- emmeans (semRating.ANOVA, pairwise ~ lureType, lmer.df = "satterthwaite") # satterwhaite for fastening up computation, does not change results of contrasts
summary(semRating.emmeansDelayLure, adjust="sidak") # sidak adjustment when needed

with(meanDf, cohensD(x = rating[lureType == "semantically related" & ratingScale == "semantic relatedness"], 
                     y = rating[lureType == "perceptually related" & ratingScale == "semantic relatedness"],
                     method="paired"))

with(meanDf, cohensD(x = rating[lureType == "semantically related" & ratingScale == "semantic relatedness"], 
                     y = rating[lureType == "unrelated" & ratingScale == "semantic relatedness"],
                     method="paired"))

# Supplementary Figure 1 RelatednessRating in Pilot ####

svg("SupplFigureRelatednessRating.svg")

plot_data <- pilotFinalSetsDf %>%
  mutate(x = case_when(
    lureType == "perceptually related" & ratingScale == "perceptual relatedness" ~ 1 - dodge,
    lureType == "perceptually related" & ratingScale == "semantic relatedness" ~ 1 + dodge,
    lureType == "semantically related" & ratingScale == "perceptual relatedness" ~ 2 - dodge,
    lureType == "semantically related" & ratingScale == "semantic relatedness" ~ 2 + dodge,
    lureType == "unrelated" & ratingScale == "perceptual relatedness" ~ 3 - dodge,
    lureType == "unrelated" & ratingScale == "semantic relatedness" ~ 3 + dodge,
  ))

p <- ggplot (data=plot_data, aes(x=lureType, y=rating, fill=ratingScale))+   
  stat_summary(stat = 'identity', fun='mean',geom='bar', position=position_dodge())+
  geom_point(pch = 19, position = position_dodge(0.6), 
             alpha = 0.2, size=3)+
  geom_line(aes(x = x, group = interaction(Name, lureType)), alpha = 0.1) +
  labs(y="relatedness rating")
p + theme_classic()+
  #annotation
  #percRel vs semRel in sem rating
  # stars 
  annotate("text", x = 1.75, y = 11, label = "* * *", size =15)+ #!!
  #line
  annotate("path", x = c(1.24, 2.21), y = c(10.8, 10.8), size=1.5) +
  #sem vs new in sem rating
  # stars 
  annotate("text", x = 2.75, y = 11, label = "* * *", size =15)+ #!!
  #line
  annotate("path", x = c(2.24, 3.21),  y = c(10.8, 10.8), size=1.5) +
  #sem vs perc rating in semLure
  # stars 
  annotate("text", x = 2, y = 9.9, label = "* * *", size =15)+ #!!
  #line
  annotate("path", x = c(1.79, 2.21), y = c(9.7, 9.7), size=1.5) +
  # stars 
  annotate("text", x = 1.25, y = 8.9, label = "* * *", size = 15)+ #!!
  #line
  annotate("path", x = c(0.79, 1.76), y = c(8.7, 8.7), size=1.5) +
  #line
  annotate("path", x = c(0.79, 1.21), y = c(7.6, 7.6), size=1.5) +
  # stars 
  annotate("text", x = 1, y = 7.8, label = "* * *", size = 15)+ 
  stat_summary(fun.data = mean_se, geom = "errorbar",  
               position=position_dodge(0.9),width = 0, size = 1.7)+
  scale_fill_manual(values=c("lightsteelblue", "deepskyblue4"))+ 
  labs(x="lure type")+
  scale_x_discrete(title="rating scale", 
                   labels=c("perceptually \n related","semantically \n related",
                            "unrelated"))+
  coord_cartesian( ylim = c(0, 11)) + 
  scale_y_continuous(name ="relatedness rating", breaks=c(0, 2, 4, 6, 8, 10))+
  my_theme
dev.off()

