# Unmet need among former users



# Former Current Never Use loop
# Simplifying Groups


library(tidyverse)
library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(stringr)
library(stringi)
library(jtools)
library(ggplot2)
library(questionr)
library(viridis)
library(xlsx)
library(forcats)
library(data.table)
library(sjlabelled)
#devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)

# v302a ever use

# need v313 and v302a
options(scipen=999)



surveys <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));

surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")

surveys_mini <- surveys %>%
  filter(IRfile!="COIR71FL.DTA") %>%
  filter(IRfile!="ETIR81FL.DTA")  %>%
  filter(IRfile!="PKIR61FL.DTA")  %>%
  filter(StartYear>=2011) 

surveys_mini_info <- surveys_mini %>% select(Country, StartYear, IRfile) %>% rename(SurveyName=IRfile)

results_unmet_former <- setNames(data.frame(matrix(ncol = 2, nrow =0)), c(    "Freq",   "SurveyName"     )) %>%
  mutate( Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))


for (row in 1:nrow(surveys_mini)) {
  women_data <- surveys_mini[row, "IRfile"]
  
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  
  women <- read_dta(women_data, col_select = any_of(c("caseid", "v312", "v313",  "v302a",  "v201", "v149", "v021", "v022", "v023", "v005", "v024", "v190", "v013", "v502", 
                                                      "v525", "v529",  "v602", "v604", "v626a", "v362", "v632", "v632a", "v359", "v360", "v327", "v317", "v008",
                                                      "v3a05", "v3a02", "v3a04", "vcal_1")))
  
  
  if (exists("v302a", women) & exists("v626a", women)) {

  
  women_clean <- women %>% mutate(Status=case_when(v313==1 | v313==2 | v313==3 ~ "Current",
                                                   v302a==1 | v302a==2 | v302a==3 ~ "Former",
                                                   v302a==0 ~ "Never")) %>%
    mutate(sampleweights=v005/100000) %>%
    mutate(Mar_Stat = case_when(v502==1 ~ "Married",
                                v529==0 & v502!=1 ~ "UMSA",
                                v525==0 & v502!=1 ~ "UMNSA",
                                v529>0 & v502!=1 ~ "UMNSA",
                                TRUE~ "MISSING")) %>%
    mutate(Age=case_when(v013==1 | v013==2 ~ "15-24",
                         v013==3 | v013==4 | v013==5 ~ "25-39",
                         v013==6 | v013==7 ~ "40-49")) %>%
    mutate(GroupMarAge=paste(Mar_Stat, Age, sep="_")) %>%
    mutate(group=case_when(v502==1 &  v313!=0 ~ "Married,  Current User",
                           v502==0 & v313!=0  ~ "Never Married,  Current User",
                           v502==2 & v313!=0  ~ "Formerly Married,  Current User",
                           v626a==9 | v602==5 ~ "Infecund, Menopausal",
                           v626a==0  | v626a==8 ~ "Not Married, Not Sexually Active",
                           v502==0 |  v502==2  ~ "Not Married, Sexually Active",
                           v502==1 &  v602==1 & (v604==1 | v604==0) ~ "Married, Wants Child within 2 Years",
                           v502==1 &  v602==1 & v604 >= 2  ~ "Married, Does Not Want Child within 2 Years",
                           v502==1 &  (v602==2 | v602==3)  ~ "Married, Does Not Want Child within 2 Years"  )) %>%
    mutate(Method_Type=case_when(v312==2 | v312==6 | v312==7 | v312==11 ~ "LAPM",
                                 v313==3 ~ "Short Acting Modern",
                                 v313==1 | v313==2 ~ "Traditional")) %>%
    mutate(told_om=case_when(v3a05==1  ~ 1 , v3a05>=0 & v3a05<9 ~ 0 ),
           told_side_eff = case_when(v3a02==1 ~ 1, v3a02==0 ~ 0),
           told_to_do = case_when(v3a04==1 & v3a02==1 ~ 1, v3a02>=0 & v3a02<9 ~ 0 ) ,
           total = told_om + told_side_eff + told_to_do,
           mii= case_when(total==3 ~ 1, total<3 ~ 0),
           sampleweights=v005/100000) %>%
    mutate(time_use =v008 - v317 +1,
           time_use_group=case_when(time_use< 12 ~ "Under a Year",
                                    time_use >=12 & time_use <24 ~ "1 Year",
                                    time_use >=24 & time_use <36 ~ "2 Year",
                                    time_use >=36 & time_use <48 ~ "3 Year",
                                    time_use >=48 & time_use <60 ~ "4 Year",
                                    time_use >=60 ~ "5+ Years")) %>%
    filter(!is.na(v005)) %>%
    mutate(unmet=case_when(v626a==1 | v626a==2 ~ 1,
                           TRUE ~ 0))
  
  

  former <-   women_clean %>% filter(Status=="Former")
  
  unmet_former <- as.data.frame(prop.table(wtd.table(former$unmet, weights = former$sampleweights))) %>%
    filter(Var1==1) %>% mutate(SurveyName=women_data) %>% select(-Var1)
  
  
  results_unmet_former <- bind_rows(results_unmet_former, unmet_former)
  
  }
}

results_unmet_former_clean <- results_unmet_former %>% left_join(surveys_mini_info, by="SurveyName") %>%
  mutate(Unmet=Freq * 100) %>% select(-Freq, -SurveyName)

setwd("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Shiny Input Files 011624")

write.csv(results_unmet_former_clean, "Former Unmet Need.csv", row.names = F, na="")
