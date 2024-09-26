
#Description 
#This script provides a summary and explores the differences between the VHE and AFE metrics within the households.

#Load packages 
if (!require('haven')) install.packages('haven'); library('haven')
if (!require('here')) install.packages('here'); library('here')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('ggstatsplot')) install.packages('ggstatsplot'); library('ggstatsplot')
if (!require('magrittr')) install.packages('magrittr'); library('magrittr')
if (!require('nortest')) install.packages('nortest'); library('nortest')
if (!require('ggpubr')) install.packages('ggpubr'); library('ggpubr')
if (!require('psych')) install.packages('psych'); library('psych')


#Reading the VHE/AFE file 
hh.hme <- read.csv(here::here('vuln.group/hh.eq.factors.csv'))

#Summary statistics 
ad.test(hh.hme$vulnerable_equiv_zn) #checking for normality test 
describe(hh.hme$hh.afe)


vuln.va.afe <- hh.hme %>% select(HHID, vulnerable_equiv_va, hh.afe) #vitA vs AFE distributions

vuln.va.afe <- vuln.va.afe %>% pivot_longer(
  cols = c('vulnerable_equiv_va', 'hh.afe'), 
  names_to = "factor_type", 
  values_to = "factor_value")

vuln.va.afe$factor_type[vuln.va.afe$factor_type=='vulnerable_equiv_va'] <- 'VHE'
vuln.va.afe$factor_type[vuln.va.afe$factor_type=='hh.afe'] <- 'AFE'



ggwithinstats( # paired samples
  data = vuln.va.afe,
  x = factor_type,
  y = factor_value,
  type = "nonparametric", # for wilcoxon
  centrality.plotting = TRUE, #include median
  xlab = "Type of Equivalent Factor",                       # label for the x-axis variable
  ylab = "Household Equivalent Factor",               # label for the y-axis variable
  title = "Paired analysis of AFE and VHE for vitamin A
", 
  #point.path = FALSE
) 


vuln.zn.afe <- hh.hme %>% select(HHID, vulnerable_equiv_zn, hh.afe) #ZInc vs AFE distributions

vuln.zn.afe <- vuln.zn.afe %>% pivot_longer(
  cols = c('vulnerable_equiv_zn', 'hh.afe'), 
  names_to = "factor_type", 
  values_to = "factor_value")

vuln.zn.afe$factor_type[vuln.zn.afe$factor_type=='vulnerable_equiv_zn'] <- 'VHE'
vuln.zn.afe$factor_type[vuln.zn.afe$factor_type=='hh.afe'] <- 'AFE'

ggwithinstats( # paired samples
  data = vuln.zn.afe,
  x = factor_type,
  y = factor_value,
  type = "nonparametric", # for wilcoxon
  centrality.plotting = TRUE, #include median
  xlab = "Type of Equivalent Factor",                       # label for the x-axis variable
  ylab = "Household Equivalent Factor",               # label for the y-axis variable
  title = "Paired analysis of AFE and VHE for zinc", 
  #point.path = FALSE
) 

#3. Plotting the H-AR threshold for the vulnerable group 

df.HAR.va <- hh.hme %>% 
  group_by(vulnerable_equiv_H_AR_va) %>% 
  summarise(counts = n())

ggplot(df.HAR.va, aes(x = vulnerable_equiv_H_AR_va, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

#Creating a pie chart from the counts 
#a)Arrange the grouping variable (HAR_va) in descending order. This important to compute the y coordinates of labels.
#b)compute the proportion (counts/total) of each category
#c)compute the position of the text labels as the cumulative sum of the proportion. 
#d)To put the labels in the center of pies, we’ll use cumsum(prop) - 0.5*prop as label position.

df.HAR.va <- df.HAR.va %>%
  arrange(desc(vulnerable_equiv_H_AR_va)) %>%
  mutate(prop = round(counts*100/sum(counts), 0),
         lab.ypos = cumsum(prop) - 0.5*prop)
df.HAR.va <- df.HAR.va %>% rename('H-AR for vitamin A' = 'vulnerable_equiv_H_AR_va')

df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '1020'] <- '1020mcg (Lactating women)'
df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '580'] <- '580mcg (Males 15-17yr)'
df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '570'] <- '570mcg (Males 18-60+yr)'
df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '540'] <- '540mcg (Pregnant women)'
df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '490'] <- '490mcg (Females 15-60+yr (AFE H-AR)'
df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '480'] <- '480mcg (Males/Females 11-14yr)'
df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '320'] <- '320mcg (Children 7-10yr)'
df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '245'] <- '245mcg (Children 4-6yr)'
df.HAR.va$`H-AR for vitamin A`[df.HAR.va$`H-AR for vitamin A` == '205'] <- '205mcg (Children 1-3yr)'

ggplot(df.HAR.va, aes(x = "", y = prop, fill = `H-AR for vitamin A`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  theme_void()

df.HAR.va <-  df.HAR.va[-c(2, 4, 7, 8), ]  

df.HAR.va <- df.HAR.va %>% 
  rename('H_AR' = 1 )


df.HAR.va <- arrange(df.HAR.va, prop)

p1 <- ggpie(
  df.HAR.va, x = "prop", label = "prop",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "H_AR", color = "white",
  palette = "jco", 
  #title = 'Proportion of the H-AR for zinc for the most vulnerable household member'
) + 
  theme(legend.position = c(1.23, .4))

p1 + scale_fill_manual(values = c("480mcg (Males/Females 11-14yr)" = "#7F7D9C",
                             "205mcg (Children 1-3yr)" = "#BCAAA4",
                             "570mcg (Males 18-60+yr)" = "#7B8794",
                             "1020mcg (Lactating women)" = "#B8B2A7",
                             "490mcg (Females 15-60+yr (AFE H-AR)" = "#424242"))


#4. Plotting the H-AR threshold for the vulnerable group 

df.HAR.zn <- hh.hme %>% 
  group_by(vulnerable_equiv_H_AR_zn) %>% 
  summarise(counts = n())

#Creating a pie chart from the counts 

df.HAR.zn <- df.HAR.zn %>%
  arrange(desc(vulnerable_equiv_H_AR_zn)) %>%
  mutate(prop = round(counts*100/sum(counts), 0),
         lab.ypos = cumsum(prop) - 0.5*prop)
df.HAR.zn <- df.HAR.zn %>% rename('H-AR for zinc' = 'vulnerable_equiv_H_AR_zn')

df.HAR.zn$`H-AR for zinc`[df.HAR.zn$`H-AR for zinc` == 13.7] <- '13.7mg (Lactating women)'
df.HAR.zn$`H-AR for zinc`[df.HAR.zn$`H-AR for zinc` == 12.7] <- '12.7mg (Males 18-60+yr'
df.HAR.zn$`H-AR for zinc`[df.HAR.zn$`H-AR for zinc` == 11.8] <- '11.8mg (Males 15-17yr)'
df.HAR.zn$`H-AR for zinc`[df.HAR.zn$`H-AR for zinc` == 11.5] <- '11.5mg (Pregnant women)'
df.HAR.zn$`H-AR for zinc`[df.HAR.zn$`H-AR for zinc` == 10.2] <- '10.2mg (Females 18-60+yr (AFE H-AR)'
df.HAR.zn$`H-AR for zinc`[df.HAR.zn$`H-AR for zinc` == 9.9] <- '9.9mg (Females 15-17yr)'
df.HAR.zn$`H-AR for zinc`[df.HAR.zn$`H-AR for zinc` == 3.6] <- '3.6mg (Children 1-3yr)'

df.HAR.zn <-  df.HAR.zn[-c(3, 4, 6), ]  

ggplot(df.HAR.zn, aes(x = "", y = prop, fill = `H-AR for zinc`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  theme_void()

 df.HAR.zn <- df.HAR.zn %>% 
   rename('H_AR' = 1 )

 df.HAR.zn <- arrange(df.HAR.zn, prop)
 p2 <- ggpie(
  df.HAR.zn, x = "prop", label = "prop",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "H_AR", color = "white",
  palette = "jco", 
  #title = 'Proportion of the H-AR for zinc for the most vulnerable household member'
) + 
  theme(legend.position = c(1.23, .4))

p2 + scale_fill_manual(values = c("3.6mg (Children 1-3yr)" = "#BCAAA4",
                                  "13.7mg (Lactating women)" = "#757575",
                                  "12.7mg (Males 18-60+yr" = "#7B8794",
                                  "10.2mg (Females 18-60+yr (AFE H-AR)" = "#424242"))
