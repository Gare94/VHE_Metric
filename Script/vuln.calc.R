
#Background 
# Vulnerable Household Equivalent (VHE) Metric
#Research has so far taken the ‘adult female’ (non-pregnant, non-lactating) as the default comparator household member (Tang et al., 2021). 
#This has a sound basis, given the often higher requirements relative to energy than men, and for the direct benefit that women’s well-being often confers on their children.
#Here we undertook a household-specific analysis by comparing the apparent dietary micronutrient supply of the whole household to the requirements of the most vulnerable member of that household (VHE)
#based on greatest critical nutrient density threshold. 
#Results were compared to the prevalence of apparent inadequacy arising from the use of the non-pregnant, non-lactating female requirements (AFE)
#We utilized the 2019/20 Malawi's Fifth Integrated Household Survey (IHS5) to compare the perfomance of the new VHE metric with the existing AFE metric'

#Therefore, this script is a detailed code for creating the new VHE approach, while also calculating the existing AFE metric for comparison. 

#Load Packages
if (!require('haven')) install.packages('haven'); library('haven')
if (!require('here')) install.packages('here'); library('here')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readxl')) install.packages('readxl'); library('readxl')


#Read in the required files
ihs5.ene <- read_excel(here::here('vuln.group/vuln.energy.req.xlsx')) #Energy and micronutrient requirements by age, sex and condition
ihs5.roster <- read_dta(here::here('vuln.group/HH_MOD_B.dta')) #Household roster 
ihs5.health <- read_dta(here::here('vuln.group/HH_MOD_D.dta')) #Health module 
hh.spec <- read.csv(here::here('vuln.group/hh.spec.csv')) #special energy requirements for under 2 children

#Energy calculation for men
energy.m <- ihs5.ene %>% mutate(kcal.m = case_when(age <3 ~ (59.512*.[[2]] - 30.4) *.[[6]],
                                              age>=3 & age<10 ~ (22.706*.[[2]] + 504.3) *.[[6]],
                                              age>=10 & age<18 ~ (17.686*.[[2]] + 658.2) *.[[6]],
                                              age>=18 & age<30 ~ (15.057*.[[2]] + 692.2) *.[[6]],
                                              age>=30 & age<60 ~ (11.472*.[[2]] + 873.1) *.[[6]],
                                              age>=60 ~ (11.711*.[[2]] + 587.7) *.[[6]]))

#Energy calculation for women 
energy.m.f <- energy.m %>% mutate(kcal.f = case_when(age <3 ~ (58.317*.[[4]] - 31.1) *.[[8]],
                                                   age>=3 & age<10 ~ (20.315*.[[4]] + 485.9) *.[[8]],
                                                   age>=10 & age<18 ~ (13.384*.[[4]] + 692.6) *.[[8]],
                                                   age>=18 & age<30 ~ (14.818*.[[4]] + 486.6) *.[[8]],
                                                   age>=30 & age<60 ~ (8.126*.[[4]] + 845.6) *.[[8]],
                                                   age>=60 ~ (9.082*.[[2]] + 658.5) *.[[8]]))


#Household roster- Renaming 
ihs5.roster.c <- ihs5.roster %>% 
  rename('sex' = 'hh_b03',     #sex
         'age_y' = 'hh_b05a',  #age 
         'age_m' = 'hh_b05b',  #age 
         'r.head' = 'hh_b04',  #r/ship to household head
         'mother' = 'hh_b19a', #where is the [name's] mother
         'id.mother' = 'hh_b19b')  %>% #if household member, copy id 
  select(case_id, HHID, PID, sex, age_y, age_m, r.head, mother, id.mother)
ihs5.roster.c$age_m[is.na(ihs5.roster.c$age_m)] <- 0
ihs5.roster.c <- ihs5.roster.c %>% mutate(age.m.total = (age_y*12 + age_m))
ihs5.roster.c <- ihs5.roster.c %>% mutate(age.u2 = case_when(age_y < 2 ~ "TRUE"))



#Merge Household demographic data with energy factors
energy.m.f$age <- as.factor(energy.m.f$age)
ihs5.roster.c$age_y <- as.factor(ihs5.roster.c$age_y)

ihs5.roster.c <- merge(x=ihs5.roster.c , y=energy.m.f , by.x='age_y', by.y='age', fill=-9999, all.x = TRUE) %>% 
  arrange(case_id) 

ihs5.roster.c <- ihs5.roster.c %>% select(1:11, 20:31) %>% relocate('age_y', .after=sex)

ihs5.roster.c$sex <- as.factor(ihs5.roster.c$sex)


#Dietary energy requirements for children under 1 

#create variable for under 1 year old categories
ihs5.roster.c <- ihs5.roster.c %>% mutate(age.u1.cat = case_when(age.m.total  < 3 ~ "0-2 months",
                                                                 age.m.total >=3 & age.m.total < 6 ~ "3-5 months",
                                                                 age.m.total  >= 6 & age.m.total  < 9 ~ "6-8 months",
                                                                 age.m.total  >= 9 & age.m.total  < 12 ~ "9-11 months"))
names(hh.spec) [1] <- 'cat'

hh.spec$sex.id <- as.factor(hh.spec$sex.id)

ihs5.roster.c <- merge(x=ihs5.roster.c , y=hh.spec , by.x=c('age.u1.cat', 'sex'), by.y=c('cat', 'sex.id'),
                       fill=-9999, all.x = TRUE) %>% arrange(case_id)


# Finding and assigning hh.lact values ----

#Creating a list of breastfeeding children from which wqe can identify the lactating mothers
breastfeeding_children <- subset(ihs5.roster.c, mother == 1 & age.u2=='TRUE')

ihs5.roster.c$hh.lact <- NA #create h.lact column, fill with NA

for(i in 1:nrow(breastfeeding_children)){ #loop through breastfeeding children
  relevant_PID <- breastfeeding_children$id.mother[i] #pull out data to identify mother
  relevant_HHID <- breastfeeding_children$HHID[i] #pull out data to identify mother
  ihs5.roster.c[ihs5.roster.c$HHID == relevant_HHID & ihs5.roster.c$PID == relevant_PID,]$hh.lact <- 500 #assign breastfeeding mother 500 in hh.lact
}

#Extra energy for pregnancy 

#Extra energy requirements for pregnancy

#Illness
ihs5.preg <- ihs5.health %>% 
  rename('ill1' = 'hh_d05a',
         'ill2' = 'hh_d05b') %>% 
  filter(ill1==28 | ill2==28) %>% 
  select(case_id, HHID, PID, ill1, ill2)

ihs5.roster.c$hh.preg <- NA

for(i in 1:nrow(ihs5.preg)){ #loop through breastfeeding children
  relevant_PID <- ihs5.preg$PID[i] #pull out data to identify mother
  relevant_HHID <- ihs5.preg$HHID[i] #pull out data to identify mother
  ihs5.roster.c[ihs5.roster.c$HHID == relevant_HHID & ihs5.roster.c$PID == relevant_PID,]$hh.preg <- 300 #assign pregnant woman extra energy of 300
}


#Removing the distinction between under 6 and over 6mo lactation - no difference in H.AR ----
ihs5.roster.c <- ihs5.roster.c %>% rename(zn.H.AR.lact = zn.H.AR.lact.less6,
                                          va.H.AR.lact = va.H.AR.lact.less6)
ihs5.roster.c$va.H.AR.lact.more6 <- NULL
ihs5.roster.c$zn.H.AR.lact.more6 <- NULL


#Creating empty columns to populate in for loop ----

ihs5.roster.c$va.H.AR <- NA
ihs5.roster.c$zn.H.AR <- NA
ihs5.roster.c$ener.base <- NA
ihs5.roster.c$total.energy <- NA

print("got to here!")

#For loop to assign H.AR requirements to main column for each nutrient, using male or female values where appropriate

for (i in 1:nrow(ihs5.roster.c)){
  if(ihs5.roster.c$sex[i] == 1){ #this assigns the main ener.base, va.H.AR, and zn.H.AR to be taken from the male values if the person's gender is male
    ihs5.roster.c$ener.base[i] <- ihs5.roster.c$kcal.m[i]
    ihs5.roster.c$va.H.AR[i] <- ihs5.roster.c$va.H.AR.m[i]
    ihs5.roster.c$zn.H.AR[i] <- ihs5.roster.c$zn.H.AR.m[i]
  } else { #this assigns the main ener.base, va.H.AR, and zn.H.AR to be taken from the male values if the person's gender is female
    ihs5.roster.c$ener.base[i] <- ihs5.roster.c$kcal.f[i]
    ihs5.roster.c$va.H.AR[i] <- ihs5.roster.c$va.H.AR.f[i]
    ihs5.roster.c$zn.H.AR[i] <- ihs5.roster.c$zn.H.AR.f[i]
  }
  
  #creating empty lists to be populated depending on the lactation or pregnancy status of the person in question.
  preg_lact_values_zn <- c()
  preg_lact_values_va <- c()
  
  if(!is.na(ihs5.roster.c$hh.lact[i]) | !is.na(ihs5.roster.c$hh.preg[i])){ #selects if the person in question is lactating OR pregnant
    if(!is.na(ihs5.roster.c$hh.lact[i])){ #tests if they're lactating - if so joins lactating zn H.AR and va H.AR values to the list
      preg_lact_values_zn <- c(preg_lact_values_zn, ihs5.roster.c$zn.H.AR.lact[i])
      preg_lact_values_va <- c(preg_lact_values_va, ihs5.roster.c$va.H.AR.lact[i])
    }
    if(!is.na(ihs5.roster.c$hh.preg[i])){ #tests if they're pregnant - if so joins pregnancy zn H.AR and va H.AR values to the list
      preg_lact_values_zn <- c(preg_lact_values_zn, ihs5.roster.c$zn.H.AR.preg[i])
      preg_lact_values_va <- c(preg_lact_values_va, ihs5.roster.c$va.H.AR.preg[i])
    }
    ihs5.roster.c$va.H.AR[i] <- max(preg_lact_values_va) #assigns the highest of the values in the list (i.e. if the person is both pregnant and lactating, the highest H.AR value is assigned) to the central column
    ihs5.roster.c$zn.H.AR[i] <- max(preg_lact_values_zn)
  }
  
  ihs5.roster.c$total.energy[i] <- sum(ihs5.roster.c$ener.base[i], ihs5.roster.c$energyreq.diet.kcals[i], ihs5.roster.c$hh.lact[i], ihs5.roster.c$hh.preg[i], na.rm = T) #Adds the cumulative energy requirements together. Done in for loop to allow sum() use, and na.rm use.
  
}

ihs5.roster.c$afe <- (ihs5.roster.c$total.energy/2254)

ihs5.roster.c$zn.cnd <- (ihs5.roster.c$zn.H.AR/ihs5.roster.c$total.energy)*1000
ihs5.roster.c$va.cnd <- (ihs5.roster.c$va.H.AR/ihs5.roster.c$total.energy)*1000
ihs5.roster.c$zn.cnd[is.nan(ihs5.roster.c$zn.cnd)] <- NA
ihs5.roster.c$va.cnd[is.nan(ihs5.roster.c$va.cnd)] <- NA

# Loop to find household max critical nutrient density ----

#list of household ID's
Household_ID_list <- unique(ihs5.roster.c$HHID)

#new columns to populate
ihs5.roster.c$household_equiv_va <- NA
ihs5.roster.c$household_equiv_zn <- NA
ihs5.roster.c$vulnerable_equiv_va <- NA
ihs5.roster.c$vulnerable_equiv_zn <- NA
ihs5.roster.c$hh.afe <- NA
ihs5.roster.c$vulnerable_equiv_H_AR_va <- NA
ihs5.roster.c$vulnerable_equiv_H_AR_zn <- NA

for (i in 1:length(Household_ID_list)){ #For each unique Household ID
  print(i)
  household_dataset <- subset(ihs5.roster.c, HHID == Household_ID_list[i]) #creates a subset of the overall dataset for just that household
  max_cnd_zn <- max(household_dataset$zn.cnd, na.rm = TRUE) #finds the max zn.cnd for that household
  max_cnd_va <- max(household_dataset$va.cnd, na.rm = TRUE) #finds the max va.cnd for that household
  max_cnd_zn_total_energy <- max(household_dataset[household_dataset$zn.cnd == max_cnd_zn,]$total.energy, na.rm = TRUE) #finds the highest energy of the people who have that highest zn cnd
  max_cnd_va_total_energy <- max(household_dataset[household_dataset$va.cnd == max_cnd_va,]$total.energy, na.rm = TRUE) #finds the highest energy of the people who have that highest va cnd
  vuln_equiv_H_AR_va <- max(household_dataset[household_dataset$va.cnd == max_cnd_va,]$va.H.AR, na.rm = TRUE) #select the va-H-AR for the most vulnerable people in the household
  vuln_equiv_H_AR_zn <- max(household_dataset[household_dataset$zn.cnd == max_cnd_zn,]$zn.H.AR, na.rm = TRUE) #select the zn-H-AR for the most vulnerable people in the household
  total_hh_afe <- sum(household_dataset$afe)
  ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$hh.afe <- total_hh_afe
  ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$vulnerable_equiv_H_AR_va <- vuln_equiv_H_AR_va
  ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$vulnerable_equiv_H_AR_zn <- vuln_equiv_H_AR_zn
  ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$household_equiv_va <- ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$total.energy / max_cnd_va_total_energy #In the overall dataset, for people in households who match the subsetted household in the loop, assigns household_equiv_va to be that persons total energy divided by the person who has the highest va cnd's total energy requirement 
  ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$household_equiv_zn <- ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$total.energy / max_cnd_zn_total_energy #In the overall dataset, for people in households who match the subsetted household in the loop, assigns household_equiv_zn to be that persons total energy divided by the person who has the highest zn cnd's total energy requirement
  ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$vulnerable_equiv_va <- sum(ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$household_equiv_va) #Creates a total of the household_equiv_va column and assigns the total value to be its own column
  ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$vulnerable_equiv_zn <- sum(ihs5.roster.c[ihs5.roster.c$HHID == Household_ID_list[i],]$household_equiv_zn) #Creates a total of the household_equiv_zn column and assigns the total value to be its own column
  
}

ihs5.roster.c.filtered <- ihs5.roster.c %>% select(HHID, vulnerable_equiv_va, vulnerable_equiv_zn, hh.afe, vulnerable_equiv_H_AR_va, vulnerable_equiv_H_AR_zn) %>%
  distinct() #selects the three relevant columns and finds the unique values (i.e. trimming down duplicates in a household to 1 row per household)


#per capita variable 

ihs5.roster.hz <- ihs5.roster %>% mutate(pc = 1)
hh.pc <- aggregate(ihs5.roster.hz$pc, by=list(HHID=ihs5.roster.hz$HHID), FUN=sum)
names(hh.pc)[names(hh.pc) == 'x'] <- 'pc'
ihs5.roster.c.filtered <- merge(x=ihs5.roster.c.filtered, y=hh.pc , by.x='HHID', by.y='HHID', fill=-9999, all.x = TRUE)

#Fix single household factors
ihs5.roster.c.filtered <- ihs5.roster.c.filtered %>% mutate(hh.afe = case_when(pc == 1 ~ 1, pc != 1 ~ hh.afe))

#Done- Archive
write.csv(ihs5.roster.c.filtered, here::here ("vuln.group/hh.eq.factors.csv"))


