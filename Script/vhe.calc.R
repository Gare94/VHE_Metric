
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
roster <- read_dta(here::here('HH_MOD_B.dta')) #Household roster 
ihs5.health <- read_dta(here::here('HH_MOD_D.dta')) #Health module 
h_ar <- read_excel(here::here('paper_VHE/vuln.group/H_AR_va_zn.xlsx')) #Harmonized Average requirements (H-AR) by sex and age group

roster <- roster %>% 
  rename('sex' = 'hh_b03',     #sex
         'age' = 'hh_b05a',  #age in years
         'age_months' = 'hh_b05b',  #age in months
         'r.head' = 'hh_b04',  #r/ship to household head
         'mother' = 'hh_b19a', #where is the [name's] mother
         'id.mother' = 'hh_b19b')  %>% #if household member, copy id 
  select(case_id, HHID, PID, sex, age, age_months, r.head, mother, id.mother)

roster <- roster %>% 
  mutate(age.m.total = age * 12 + ifelse(is.na(age_months), 0, age_months))

roster <- roster %>% mutate(age.u2 = case_when(age.m.total < 24 ~ "TRUE")) 

#Correcting two individuals in the household who were assigned wrong sex
roster$sex[roster$HHID == "038c3a60a6ab4613bd8b95f4c5688be3" & roster$age == 18 & roster$PID == 6] <- 2
roster$sex[roster$HHID == "8b950a3b59b34011b2ebd07c72ad0548" & roster$age == 16 & roster$PID == 3] <- 2

#Energy calculation
#For children under 2, subtract energy assumed to be provided via breastmilk to isolate 
#just energy requirements from complementary foods

roster$kcal <- NA  # Initialize kcal with NA

roster$kcal[roster$age.m.total < 3] <- 0  #assume exclusively breastfed
roster$kcal[roster$age.m.total == 3 & roster$sex == 1] <- 569 - 474
roster$kcal[roster$age.m.total == 4 & roster$sex == 1] <- 608 - 474
roster$kcal[roster$age.m.total == 5 & roster$sex == 1] <- 639 - 474
roster$kcal[roster$age.m.total == 6 & roster$sex == 1] <- 653 - 413
roster$kcal[roster$age.m.total == 7 & roster$sex == 1] <- 680 - 413
roster$kcal[roster$age.m.total == 8 & roster$sex == 1] <- 702 - 413
roster$kcal[roster$age.m.total == 9 & roster$sex == 1] <- 731 - 379
roster$kcal[roster$age.m.total == 10 & roster$sex == 1] <- 752 - 379
roster$kcal[roster$age.m.total == 11 & roster$sex == 1] <- 775 - 379
roster$kcal[roster$age.m.total >= 12 & roster$age.m.total < 24 & roster$sex == 1] <- 948 #/for child 12-24 months, subtract in random subset of children 
#assumed to be breastfeeding according to DHS -- doing this separately with breastfeeding women

roster$kcal <- ifelse(roster$age.m.total < 3, 0, roster$kcal) #assume exclusively breastfed
roster$kcal[roster$age.m.total == 3 & roster$sex == 2] <- 537 - 474
roster$kcal[roster$age.m.total == 4 & roster$sex == 2] <- 571 - 474
roster$kcal[roster$age.m.total == 5 & roster$sex == 2] <- 599 - 474
roster$kcal[roster$age.m.total == 6 & roster$sex == 2] <- 604 - 413
roster$kcal[roster$age.m.total == 7 & roster$sex == 2] <- 629 - 413
roster$kcal[roster$age.m.total == 8 & roster$sex == 2] <- 652 - 413
roster$kcal[roster$age.m.total == 9 & roster$sex == 2] <- 676 - 379
roster$kcal[roster$age.m.total == 10 & roster$sex == 2] <- 694 - 379
roster$kcal[roster$age.m.total == 11 & roster$sex == 2] <- 712 - 379
roster$kcal[roster$age.m.total >= 12 & roster$age.m.total < 24 & roster$sex == 2] <- 865 #/for child 12-24 months, subtract in random subset of children 
#assumed to be breastfeeding according to DHS -- doing this separately with breastfeeding women


roster$kcal[roster$age.m.total >= 24 & roster$age.m.total < 36 & roster$sex == 1] <- 1125
roster$kcal[roster$age.m.total >= 24 & roster$age.m.total < 36 & roster$sex == 2] <- 1050
roster$kcal[roster$age.m.total >= 36 & roster$age.m.total < 48 & roster$sex == 1] <- 1250
roster$kcal[roster$age.m.total >= 36 & roster$age.m.total < 48 & roster$sex == 2] <- 1150
roster$kcal[roster$age.m.total >= 48 & roster$age.m.total < 60 & roster$sex == 1] <- 1350
roster$kcal[roster$age.m.total >= 48 & roster$age.m.total < 60 & roster$sex == 2] <- 1250

roster$kcal[roster$age == 5 & roster$sex == 1] <- 1475
roster$kcal[roster$age == 5 & roster$sex == 2] <- 1335
roster$kcal[roster$age == 6 & roster$sex == 1] <- 1575
roster$kcal[roster$age == 6 & roster$sex == 2] <- 1425
roster$kcal[roster$age == 7 & roster$sex == 1] <- 1700
roster$kcal[roster$age == 7 & roster$sex == 2] <- 1550
roster$kcal[roster$age == 8 & roster$sex == 1] <- 1825
roster$kcal[roster$age == 8 & roster$sex == 2] <- 1700
roster$kcal[roster$age == 9 & roster$sex == 1] <- 1975
roster$kcal[roster$age == 9 & roster$sex == 2] <- 1850
roster$kcal[roster$age == 10 & roster$sex == 1] <- 2150
roster$kcal[roster$age == 10 & roster$sex == 2] <- 2000
roster$kcal[roster$age == 11 & roster$sex == 1] <- 2350
roster$kcal[roster$age == 11 & roster$sex == 2] <- 2150
roster$kcal[roster$age == 12 & roster$sex == 1] <- 2550
roster$kcal[roster$age == 12 & roster$sex == 2] <- 2275
roster$kcal[roster$age == 13 & roster$sex == 1] <- 2775
roster$kcal[roster$age == 13 & roster$sex == 2] <- 2375
roster$kcal[roster$age == 14 & roster$sex == 1] <- 3000
roster$kcal[roster$age == 14 & roster$sex == 2] <- 2450
roster$kcal[roster$age == 15 & roster$sex == 1] <- 3175
roster$kcal[roster$age == 15 & roster$sex == 2] <- 2500
roster$kcal[roster$age == 16 & roster$sex == 1] <- 3325
roster$kcal[roster$age == 16 & roster$sex == 2] <- 2500
roster$kcal[roster$age == 17 & roster$sex == 1] <- 3400
roster$kcal[roster$age == 17 & roster$sex == 2] <- 2500
roster$kcal[roster$age >= 18 & roster$age < 30 & roster$sex == 1] <- 2800 #Malawi NCD study 
roster$kcal[roster$age >= 18 & roster$age < 30 & roster$sex == 2] <- 2300 #2015/16 Malawi DHS
roster$kcal[roster$age >= 30 & roster$age < 60 & roster$sex == 1] <- 2850 #Malawi NCD study
roster$kcal[roster$age >= 30 & roster$age < 60 & roster$sex == 2] <- 2400 #Malawi NCD study
roster$kcal[roster$age >= 60 & roster$sex == 1] <- 2250 #Malawi NCD study
roster$kcal[roster$age >= 60 & roster$sex == 2] <- 2100 #Malawi NCD study


#*If there is a child under 2 in the household, assume there is a lactating women 
#*who requires 330 additional kcal/d if child is under 6 mo and 400 kcal/d 
#*if child is over 6 mo (https://www.dietaryguidelines.gov/)

set.seed(54321)

# Step 1: Identify children under 2
roster <- roster %>%
  mutate(
    childu2 = ifelse(age.m.total < 24, 1, 0)
  )

# Step 2: Precompute random values
n <- nrow(roster)
rand_12_17 <- as.integer(rbinom(n, 1, 0.91)) #91% breastfed- 2015/16 MDHS 
rand_18_23 <- as.integer(rbinom(n, 1, 0.77)) #77% breastfed- 2015/16 MDHS

# Step 3: Assign breastfeeding status using consistent integer types
roster$breastfeeding_child <- if_else(
  roster$age.m.total < 12, 1L,
  if_else(
    roster$age.m.total >= 12 & roster$age.m.total < 18, rand_12_17,
    if_else(
      roster$age.m.total >= 18 & roster$age.m.total < 24, rand_18_23,
      0L
    )
  )
)

# Step 4: Household-level breastfeeding context
roster <- roster %>%
  group_by(HHID) %>%
  mutate(
    numberchildu2 = sum(childu2 == 1, na.rm = TRUE),
    age_youngest_child_months = ifelse(
      numberchildu2 > 0,
      min(age.m.total[childu2 == 1], na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup()

# Ensure PID and id.mother are character
roster <- roster %>%
  mutate(
    PID = as.character(PID),
    id.mother = as.character(id.mother)
  )

# Step 5: Filter and assign kcal per breastfeeding child
bf_children <- roster %>%
  filter(breastfeeding_child == 1, !is.na(id.mother)) %>%
  mutate(
    lact_kcal = case_when(
      age.m.total < 6 ~ 330,
      age.m.total >= 6 & age.m.total < 24 ~ 400,
      TRUE ~ NA_real_
    )
  )

# Step 6: Aggregate kcal per mother (take max kcal if multiple children)
mother_kcal <- bf_children %>%
  group_by(HHID, id.mother) %>%
  summarise(hh.lact = max(lact_kcal, na.rm = TRUE), .groups = "drop")

# Step 7: Join back to roster
roster <- roster %>%
  left_join(mother_kcal, by = c("HHID", "PID" = "id.mother"))

# Step 8: Subtract kcal from breastfeeding children aged 12–23 months
roster <- roster %>%
  mutate(
    kcal = ifelse(
      breastfeeding_child == 1 & age.m.total >= 12 & age.m.total < 24,
      kcal - 346,
      kcal
    )
  )


#Extra energy requirements for pregnancy

#Illness
ihs5.preg <- ihs5.health %>% 
  rename('ill1' = 'hh_d05a',
         'ill2' = 'hh_d05b') %>% 
  filter(ill1==28 | ill2==28) %>% 
  select(case_id, HHID, PID, ill1, ill2)

roster$hh.preg <- NA

for(i in 1:nrow(ihs5.preg)){ #loop through breastfeeding children
  relevant_PID <- ihs5.preg$PID[i] #pull out data to identify mother
  relevant_HHID <- ihs5.preg$HHID[i] #pull out data to identify mother
  roster[roster$HHID == relevant_HHID & roster$PID == relevant_PID,]$hh.preg <- 300 #assign pregnant woman extra energy of 300
}

#Merge the roster with the requirements (H_AR)
h_ar$age <- as.factor(h_ar$age)
roster$age <- as.factor(roster$age)

ihs5.roster.c <- merge(x=roster, y=h_ar, by.x='age', by.y='age', fill=-9999, all.x = TRUE) %>% arrange(HHID)

#Creating empty columns to populate in for loop ----
ihs5.roster.c$va.H.AR <- NA
ihs5.roster.c$zn.H.AR <- NA
ihs5.roster.c$total.energy <- NA

#For loop to assign H.AR requirements to main column for each nutrient, using male or female values where appropriate

for (i in 1:nrow(ihs5.roster.c)){
  if(ihs5.roster.c$sex[i] == 1){ #this assigns the main ener.base, va.H.AR, and zn.H.AR to be taken from the male values if the person's gender is male
    ihs5.roster.c$va.H.AR[i] <- ihs5.roster.c$va.H.AR.m[i]
    ihs5.roster.c$zn.H.AR[i] <- ihs5.roster.c$zn.H.AR.m[i]
  } else { #this assigns the main ener.base, va.H.AR, and zn.H.AR to be taken from the male values if the person's gender is female
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
  
  ihs5.roster.c$total.energy[i] <- sum(ihs5.roster.c$kcal[i], ihs5.roster.c$hh.lact[i], ihs5.roster.c$hh.preg[i], na.rm = T) #Adds the cumulative energy requirements together. Done in for loop to allow sum() use, and na.rm use.
  
}

#Assigning AFE 
ihs5.roster.c$afe <- (ihs5.roster.c$total.energy/2300)

ihs5.roster.c$zn.cnd <- (ihs5.roster.c$zn.H.AR/ihs5.roster.c$total.energy)*1000
ihs5.roster.c$va.cnd <- (ihs5.roster.c$va.H.AR/ihs5.roster.c$total.energy)*1000
ihs5.roster.c$zn.cnd[is.nan(ihs5.roster.c$zn.cnd)] <- NA
ihs5.roster.c$va.cnd[is.nan(ihs5.roster.c$va.cnd)] <- NA

#Adjusting for the breastfeeding in the CND values 
ihs5.roster.c$va.cnd[ihs5.roster.c$age == 1 & ihs5.roster.c$sex == 1] <- 0
ihs5.roster.c$va.cnd[ihs5.roster.c$age == 1 & ihs5.roster.c$sex == 2] <- 0
ihs5.roster.c$zn.cnd[ihs5.roster.c$age == 1 & ihs5.roster.c$sex == 1] <- 4.6
ihs5.roster.c$zn.cnd[ihs5.roster.c$age == 1 & ihs5.roster.c$sex == 2] <- 5.4


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
ihs5.roster.hz <- roster %>% mutate(pc = 1)
hh.pc <- aggregate(ihs5.roster.hz$pc, by=list(HHID=ihs5.roster.hz$HHID), FUN=sum)
names(hh.pc)[names(hh.pc) == 'x'] <- 'pc'
ihs5.roster.c.filtered <- merge(x=ihs5.roster.c.filtered, y=hh.pc , by.x='HHID', by.y='HHID', fill=-9999, all.x = TRUE)

#Fix single household factors
ihs5.roster.c.filtered <- ihs5.roster.c.filtered %>% mutate(hh.afe = case_when(pc == 1 ~ 1, pc != 1 ~ hh.afe))

#Done- Archive
write.csv(ihs5.roster.c.filtered, here::here ("paper_VHE/vuln.group/hh.eq.factors.csv"))
