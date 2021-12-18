# Impute missing ethnicity data for SMI prevalence project

# 1. Read in required packages -------------------------------------------------

library(tidyverse)
library(readxl)
library(mice)
library(ggpubr)
library(openxlsx)
library(parallel)
library(tictoc)

# 2. Read in data and check contents -------------------------------------------


Prev <- read_excel("3. Imputation/2. Input Data/SMI_LR_Imputations_v5.2.xlsx", 
                   sheet = 1) %>% 
  rename(EthGroup = 'EthGroup_2',
         depdecile = 'imd2015_10',
         region = 'region_num',
         num = 'patients',
         SMI = 'SMI',
         SMI_Group = 'Group')

head(Prev) # quick check of variables names and data types

# 3. Clean the data ------------------------------------------------------------

# Factor the columns
Prev$AgeGroup<-as.factor(Prev$AgeGroup)
Prev$depdecile<-as.factor(Prev$depdecile)
Prev$EthGroup<-as.factor(Prev$EthGroup)
Prev$gender<-as.factor(Prev$gender)
Prev$region<-as.factor(Prev$region)
Prev$SMI_Group <- as.factor(Prev$SMI_Group) 

head(Prev) # quick check that variables were factored correctly

# Check contents of 7 columns, and look out for any anomalies
summary(Prev$AgeGroup) # 14:24, 25:49, 50:64, 65+
summary(Prev$depdecile) # 0 to 10
summary(Prev$EthGroup) # Asian/Black/Mixed/Other/White/NA = needs amending
summary(Prev$gender) # Watch out for F, M and U!
summary(Prev$region) # 1 to 10
summary(Prev$num) # Min = 0 and Max = 45410
summary(Prev$SMI_Group) # BPD, NonSMI, PSY, SCH

# 4. Ungroup data and select columns for new dataframe -------------------------

Prev <- as.data.frame(lapply(Prev, rep, Prev$num, na.rm=T))%>%
  select(AgeGroup, depdecile, EthGroup, gender, region, SMI_Group)%>%
  filter(depdecile!= 0) %>% 
  filter(gender != 'U') %>% 
  droplevels() # extra bit of code to drop any unused empty levels in factors

# 9,305,308 rows
# 7 cols = AgeGroup, depdecile, EthGroup, gender, region, SMI, SMI_Group


# 5. Carefully check each column again and it's contents -----------------------

str(Prev, 10)

# Check contents and also check previous anomalies now dealt with
summary(Prev$AgeGroup) # 14:24, 25:49, 50:64, 65+
summary(Prev$depdecile) # 0 is now removed
summary(Prev$EthGroup) # Asian/Black/Mixed/Other/White/NA
summary(Prev$gender) # F, M (U is now removed)
summary(Prev$region) # 1 to 10
summary(Prev$SMI_Group)# BPD 34005/NonSMI9205105/PSY 36019/SCH 30179
summary(Prev$num) # Now unpacked, so no longer column

# Plot all variables in graph and check for anomalies

options(scipen=10000)

Prev %>% 
  select_if(is.factor) %>% 
  gather %>% 
  ggplot(aes(x = value)) + geom_bar(fill="#FF9999", colour="black") +
  facet_wrap(~key, scales = 'free') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 


# 6. Scaled sample Dataset -----------------------------------------------------

# RanPrev <- sample_n(Prev, 2000) # simple random not used

RanPrev <- Prev %>% # very basic cluster sample
  group_by(region, depdecile) %>% 
  sample_frac(0.01) %>% 
  ungroup()

# Check scaled sample

str(RanPrev)
head(RanPrev, 10)
summary(RanPrev$EthGroup) # check ethnicity groupings are correct


# check each variable as a bar plot and look for unusual data points

RanPrev %>% 
  select_if(is.factor) %>% 
  gather %>% 
  ggplot(aes(x = value)) + geom_bar(fill="#FF9999", colour="black") +
  facet_wrap(~key, scales = 'free') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

Ran_plot <- ggplot(RanPrev, aes(x= EthGroup, fill= EthGroup)) + 
  geom_bar(fill="#FF9999", colour="black")


# 7. Prep the data for MICE imputation -----------------------------------------

md.pattern(RanPrev) # Checks variables with missing values

# There is only 1 variable with missing values = Ethnicity

quickpred(RanPrev) # Checks predictors to use with correlation

# MICE has selected Age, IMD, & Region as predictors of ethnicity in the model
# However when Hiral previously ran a log reg, SMI was also a predictor
# Hence Hiral included all variables in the imputation (except the ethgroup)


# 8. Impute data ---------------------------------------------------------------

#Explicitly mention the variables that are used for imputation

# Method One - imputation with parallel computing + MICE

detectCores() # 4 cores in i7 laptop

tic() # run time count begins

imputed_prev <- parlmice(data = RanPrev,
         m = 9, # total imputations
         maxit = 5, # iterations, 5 is default
         cluster.seed = 100, # use cluster.seed when using cores
         n.core = 3, # computing cores = 4 cores - 1 core (you need 1 core free)
         n.imp.core = 3, # imputations per core (3 * 3 = 9 imps)
         pred = quickpred(RanPrev, include = c( # variables for prediction
           "AgeGroup",
           "depdecile",
           "gender",
           "region",
           "SMI_Group")),
         method = 'polyreg') # method used on missing data column

toc() # 6mins 24secs, 24% memory usage with one free core


# If 10% missing use 10 imputations - see Bodner (2008) or White et al (2011)
# seed = set number to ensure reproducibility
# method = default unless there is a strong reason to override which is rare
# maxit (iterations) default = 5


# 9. Check imputed outputs -----------------------------------------------------

summary(imputed_prev) # check 10 imputations

ImpVal <- imputed_prev$imp$EthGroup # check actual imputed values only


# 10. Bind all 10 imputed ethnicity columns ------------------------------------

# by default, C1 is the 1st imputation, not a complete imputation!!

c1 <- complete(imputed_prev) %>% select("EthGroup") 
c2 <- complete(imputed_prev, 2) %>% select("EthGroup")
c3 <- complete(imputed_prev, 3) %>% select("EthGroup")
c4 <- complete(imputed_prev, 4) %>% select("EthGroup")
c5 <- complete(imputed_prev, 5)%>% select("EthGroup")
c6 <- complete(imputed_prev, 6) %>% select("EthGroup")
c7 <- complete(imputed_prev, 7) %>% select("EthGroup")
c8 <- complete(imputed_prev, 8) %>% select("EthGroup")
c9 <- complete(imputed_prev, 9) %>% select("EthGroup")


Eth_Values <- cbind(c1, c2, c3, c4, c5, c6, c7, c8, c9) %>% 
  rename(c1 = 1,
         c2 = 2,
         c3 = 3,
         c4 = 4,
         c5 = 5,
         c6 = 6, 
         c7 = 7, 
         c8 = 8,
         c9 = 9)

# 11 Find the most frequent ethnicity per row ----------------------------------

Eth_Values <- Eth_Values %>% 
  mutate(ImpEth = apply(Eth_Values, 1, function(x) 
    names(which.max(table(x))))) %>% 
  select(ImpEth)


# 12 Bind the original to the imputed by columns (wide)-------------------------

Output_df <- cbind(RanPrev, Eth_Values)

# 13 Save the combined output to file for QA only-------------------------------

Output_Imp <- createWorkbook()
addWorksheet(Output_Imp, "Imputed")
writeData(Output_Imp, sheet = "Imputed", Output_df)
saveWorkbook(Output_Imp, "Sample_Output_Data_v5.2_a.xlsx")

# 14 Save dataset for logistic regression --------------------------------------

df <-Output_df %>% 
  select(ImpEth, AgeGroup, depdecile, gender, region, SMI_Group) %>%
  group_by(ImpEth, AgeGroup, depdecile, gender, region, SMI_Group)%>%
  summarize(count = n()) %>%
  rename("num"="count")

write.csv(df,"Sample_logreg_data_v5.2_a.csv",row.names = F)













