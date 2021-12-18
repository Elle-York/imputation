
# IMPUTATION FOR SMI/PD PREVALENCE (2021)

# Use the test code first! 

# 1. Read in required packages -------------------------------------------------

library(tidyverse)
library(readxl)
library(mice)
library(ggpubr)
library(openxlsx)
library(parallel)
library(tictoc)

# 2. Read in data and check contents -------------------------------------------


prev <- read_excel("3. Imputation/2. Input Data/SMI_LR_Imputations_v5.2.xlsx", 
                   sheet = 1) %>% 
  rename(EthGroup = 'EthGroup_2',
         depdecile = 'imd2015_10',
         region = 'region_num',
         num = 'patients',
         SMI = 'SMI',
         SMI_Group = 'Group')


# 3. Factor the data -----------------------------------------------------------


prev$AgeGroup<-as.factor(prev$AgeGroup)
prev$depdecile<-as.factor(prev$depdecile)
prev$EthGroup<-as.factor(prev$EthGroup)
prev$gender<-as.factor(prev$gender)
prev$region<-as.factor(prev$region)
prev$SMI_Group <- as.factor(prev$SMI_Group) 


# 4. Ungroup data and select columns for new dataframe -------------------------

prev <- as.data.frame(lapply(prev, rep, prev$num, na.rm=T))%>%
  select(AgeGroup, depdecile, EthGroup, gender, region, SMI_Group)%>%
  filter(depdecile!= 0) %>% 
  filter(gender != 'U') %>% 
  droplevels() # extra bit of code to drop any unused empty levels in factors

# 9,305,308 rows
# 6 cols = AgeGroup, depdecile, EthGroup, gender, region, SMI_Group


# 5. Impute data + parallel method for computing -------------------------------

detectCores() # 4 cores in i7 laptop;

tic() # run time count begins

imputed_prev <- parlmice(data = prev,
                         m = 9, # total imputations
                         maxit = 5, # iterations, 5 is default
                         cluster.seed = 100, # use cluster.seed when using cores
                         n.core = 3, # computing cores = 4 cores - 1 core (you need 1 core free)
                         n.imp.core = 3, # imputations per core (3 * 3 = 9 imps)
                         pred = quickpred(prev, include = c( # variables for prediction
                           "AgeGroup",
                           "depdecile",
                           "gender",
                           "region",
                           "SMI_Group")),
                         method = 'polyreg') # method used on missing data column

toc() # run time = 14 hours and 36% memory usage with one free core


# 6. Extract the 9 new imputed ethnicity cols and bind together ----------------

# by default, C1 is the 1st imputation, not a complete imputation

c1 <- complete(imputed_prev) %>% select("EthGroup") 
c2 <- complete(imputed_prev, 2) %>% select("EthGroup")
c3 <- complete(imputed_prev, 3) %>% select("EthGroup")
c4 <- complete(imputed_prev, 4) %>% select("EthGroup")
c5 <- complete(imputed_prev, 5) %>% select("EthGroup")
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

# 7. Find the most frequent ethnicity per row and create 1 new column ----------

Eth_Values <- Eth_Values %>% 
  mutate(ImpEth = apply(Eth_Values, 1, function(x) 
    names(which.max(table(x))))) %>% 
  select(ImpEth)


# 12 Bind original dataset (retain orig. ethnicity) to new imputed column ------

Output_df <- cbind(prev, Eth_Values)

head(Output_df)

# 13. Save final dataset (minus original ethnicity col) ------------------------

df <-Output_df %>% 
  select(AgeGroup, depdecile, gender, region, SMI_Group, ImpEth) %>%
  group_by(AgeGroup, depdecile, gender, region, SMI_Group, ImpEth)%>%
  summarize(count = n()) %>%
  rename("num"="count")

write.csv(df,"Final_data_v5.2.csv",row.names = F)


# 14. Save the combined output to file for QA only -----------------------------

Output_Imp <- createWorkbook()
addWorksheet(Output_Imp, "Imputed")
writeData(Output_Imp, sheet = "Imputed", Output_df)
saveWorkbook(Output_Imp, "QA_Output_Data_v5.2.xlsx")

df <-Output_df %>% 
  select(AgeGroup, depdecile, gender, region, SMI_Group, EthGroup, ImpEth) %>%
  group_by(AgeGroup, depdecile, gender, region, SMI_Group, EthGroup, ImpEth)%>%
  summarize(count = n()) %>%
  rename("num"="count")

write.csv(df,"QA_grouped_data.csv",row.names = F)
















