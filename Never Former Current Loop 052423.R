

#Note KB 0524423
# Need to look into coding for v302a!




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
devtools::install_github("liamgilbey/ggwaffle")
library(ggwaffle)

# v302a ever use

# need v313 and v302a
options(scipen=999)



n_results <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c(  "Var1",   "Freq"  ,   "SurveyName", "Intention"   )) %>%
  mutate( Var1=as.numeric(Var1),  Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName),  Intention=as.character(Intention))


surveys <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));


surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")

surveys_mini <- surveys 
surveys_mini_info <- surveys_mini %>% select(Country, StartYear, IRfile) %>% rename(SurveyName=IRfile) 

for (row in 1:nrow(surveys_mini)) {
  women_data <- surveys_mini[row, "IRfile"]
  
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  
  women <- read_dta(women_data, col_select = any_of(c("caseid", "v312", "v313",  "v302a",  "v201", "v149", "v021", "v022", "v023", "v005", "v024", "v190", "v013", "v502", "v525", "v529",  "v602", "v604", "v626a", "v362", "v632", "v632a", "v359", "v360",
                                                      "v3a05", "v3a02", "v3a04", "vcal_1")))
  
  if (exists("v302a", women) & !is.na(sum(women$v302a))) {
    
    label <- as.data.frame(attr(women$v302a,"labels")) %>% rownames_to_column()   %>% rename(Var1=2, Intention=1) %>% mutate(Var1=as.numeric(as.character(Var1))) 
    
    n <- as.data.frame(table(women$v302a) )  %>%
      mutate(SurveyName=women_data) %>% mutate(Var1=as.numeric(as.character(Var1))) %>%
      full_join(label, by="Var1")
    
    n_results <- n_results %>% bind_rows(n)
  }
}

# Delete CDIR61FL.DTA CIIR62FL.DTA HTIR61FL.DTA
delete_survey <- c("CDIR61FL.DTA", "CIIR62FL.DTA", "HTIR61FL.DTA")
#$ "CDIR61FL.DTA", "CIIR62FL.DTA", "HTIR61FL.DTA", have no calendar and thus different coding for v302a

survey_keep <- n_results %>% filter(!SurveyName %in% delete_survey) %>% filter(!is.na(SurveyName)) %>% filter(Var1==1) %>% select(SurveyName) 



surveys_mini <- survey_keep  %>% left_join(surveys_mini_info, by="SurveyName")


#################################################################################################################

results_use_status <- setNames(data.frame(matrix(ncol = 3, nrow =0)), c("Var1",    "Freq",   "SurveyName"     )) %>%
  mutate(Var1=as.character(Var1), Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_use_age_status <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c("Var1", "Var2",   "Freq",   "SurveyName"     )) %>%
  mutate(Var1=as.character(Var1), Var2=as.character(Var2),  Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_use_mar_status <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c("Var1", "Var2",   "Freq",   "SurveyName"     )) %>%
  mutate(Var1=as.character(Var1), Var2=as.character(Var2),  Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_use_mar_status <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c("Var1", "Var2",   "Freq",   "SurveyName"     )) %>%
  mutate(Var1=as.character(Var1), Var2=as.character(Var2),  Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_never_group <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c("Group", "Freq", "N",     "SurveyName"     )) %>%
  mutate(Group=as.character(Group),  Freq=as.numeric(Freq),   N=as.numeric(N),  SurveyName=as.character(SurveyName))

results_never_intention <- setNames(data.frame(matrix(ncol = 5, nrow =0)), c("Group", "Intention", "Freq", "N",     "SurveyName"     )) %>%
  mutate(Group=as.character(Group), Intention=as.character(Intention),  Freq=as.numeric(Freq),   N=as.numeric(N),  SurveyName=as.character(SurveyName))

results_never_has_intention <- setNames(data.frame(matrix(ncol = 3, nrow =0)), c("Group",    "Freq",   "SurveyName"     )) %>%
  mutate(Group=as.character(Group), Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_former_group <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c("Group", "Freq", "N",     "SurveyName"     )) %>%
  mutate(Group=as.character(Group),  Freq=as.numeric(Freq),   N=as.numeric(N),  SurveyName=as.character(SurveyName))

results_former_intention <- setNames(data.frame(matrix(ncol = 5, nrow =0)), c("Group", "Intention", "Freq", "N",     "SurveyName"     )) %>%
  mutate(Group=as.character(Group), Intention=as.character(Intention),  Freq=as.numeric(Freq),   N=as.numeric(N),  SurveyName=as.character(SurveyName))

results_former_has_intention <- setNames(data.frame(matrix(ncol = 3, nrow =0)), c("Group",    "Freq",   "SurveyName"     )) %>%
  mutate(Group=as.character(Group), Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_former_why_discontinue <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c("Var1",    "Freq", "WhyDisc",  "SurveyName"     )) %>%
  mutate(Var1=as.character(Var1), Freq=as.numeric(Freq),  WhyDisc=as.character(WhyDisc),  SurveyName=as.character(SurveyName))

results_former_time_use_group  <- setNames(data.frame(matrix(ncol = 3, nrow =0)), c("Time_Since_Use",    "Freq",   "SurveyName"     )) %>%
  mutate(Time_Since_Use=as.character(Time_Since_Use), Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_mii <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c("Method",  "Var",    "Freq", "SurveyName"     )) %>%
  mutate(Method=as.character(Method), Freq=as.numeric(Freq),  Var=as.character(Var),  SurveyName=as.character(SurveyName))

results_current_group <-  setNames(data.frame(matrix(ncol = 3, nrow =0)), c("Group",    "Freq",   "SurveyName"     )) %>%
  mutate(Group=as.character(Group), Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_current_methodmix <-  setNames(data.frame(matrix(ncol = 3, nrow =0)), c("Method",    "Freq",   "SurveyName"     )) %>%
  mutate(Method=as.character(Method), Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_current_source <- setNames(data.frame(matrix(ncol = 3, nrow =0)), c("Source",    "Freq",   "SurveyName"     )) %>%
  mutate(Source=as.character(Source), Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_current_timesince <-  setNames(data.frame(matrix(ncol = 3, nrow =0)), c("Var1",    "Freq",   "SurveyName"     )) %>%
  mutate(Var1=as.character(Var1), Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName))

results_empowerment <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c("Group",  "Decider",    "Freq", "SurveyName"     )) %>%
  mutate(Group=as.character(Group), Freq=as.numeric(Freq),  Decider=as.character(Decider),  SurveyName=as.character(SurveyName))
#################################################################################################################


for (row in 1:nrow(surveys_mini)) {
  women_data <- surveys_mini[row, "SurveyName"]
  
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  
  women <- read_dta(women_data, col_select = any_of(c("caseid", "v312", "v313",  "v302a",  "v201", "v149", "v021", "v022", "v023", "v005", "v024", "v190", "v013", "v502", 
                                                      "v525", "v529",  "v602", "v604", "v626a", "v362", "v632", "v632a", "v359", "v360", "v327", "v317", "v008",
                                                      "v3a05", "v3a02", "v3a04", "vcal_1")))
  
  
  
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
    filter(!is.na(v005))
  
  ###############################################################
  mar_clean <- women_clean %>% filter(v502==1)
  
  # Never Use 
  never_users <- women_clean %>% filter(Status=="Never") 
  
  # Never Users who are married
  never_mar <- never_users %>% filter(v502==1)
  
  # Former Users
  formercal_labels <- as.data.frame(attr(women_clean$v302a,"labels")) %>% rownames_to_column()   %>% rename(v302a=2, time_discontinuation=1) %>% mutate(v302a=as.numeric(as.character(v302a))) 
  
  former_users <- women_clean %>% filter(Status=="Former")  %>% full_join(formercal_labels, by="v302a")

   
    # Former users who used within the calendar (around 6 years)
  former_users_check <- former_users %>% filter(v302a>0) %>% filter(!is.na(v005))
  
  if ( mean(former_users_check$v302a) > 1) {
    former_users_calendar <- former_users %>% filter(v302a==2)
  }
  
  # Former Users who are married
  former_mar <- former_users %>% filter(v502==1)
  
  # Current Use
  current_users <- women_clean %>% filter(Status=="Current")
  
  # Current Users who are married
  current_mar <- current_users %>% filter(v502==1)
  #################################################################
  
  # Labels
  intention_labels <- as.data.frame(attr(women_clean$v362,"labels")) %>% rownames_to_column() %>% rename(Var1=2, Intention=1) %>% mutate(Var1=as.numeric(as.character(Var1))) 
  
  if (exists("v632a", women_clean)) {
    dec_not_use_labels <- as.data.frame(attr(women_clean$v632a,"labels")) %>% rownames_to_column() %>% rename(Var1=2, Decider=1) %>% mutate(Var1=as.numeric(as.character(Var1))) 
  }
  
  dec_to_use_labels <- as.data.frame(attr(women_clean$v632,"labels")) %>% rownames_to_column() %>% rename(Var1=2, Decider=1) %>% mutate(Var1=as.numeric(as.character(Var1))) 
  
  if (exists("v360", women_clean)) {
    why_discontinued_labels <- as.data.frame(attr(women_clean$v360,"labels")) %>% rownames_to_column() %>% rename(Var2=2, WhyDisc=1) %>% mutate(Var2=as.numeric(as.character(Var2))) 
  }
  
  v312_labels <- as.data.frame(attr(women_clean$v312,"labels")) %>% rownames_to_column() %>% rename(Var1=2, Method=1) %>% mutate(Var1=as.numeric(as.character(Var1))) 
  
  
  ####################################################################################################################################################################
  use_status<- as.data.frame(prop.table(wtd.table(women_clean$Status, weights = women_clean$sampleweights))) %>%
    mutate(Var1=case_when(Var1== "Never" ~ "Never Used", 
                          Var1== "Former" ~ "Former User", 
                          Var1== "Current" ~ "Current User")) %>%
    mutate(Var1=fct_relevel(Var1,  "Never Used", "Former User" , "Current User")) %>%
    mutate(SurveyName=women_data)
  
  use_age_status<- as.data.frame(prop.table(wtd.table(as.factor(women_clean$Age), as.factor(women_clean$Status), weights = women_clean$sampleweights)) ) %>%
    mutate(Var2=case_when(Var2== "Never" ~ "Never Used", 
                          Var2== "Former" ~ "Former User", 
                          Var2== "Current" ~ "Current User")) %>%
    mutate(Var2=fct_relevel(Var2,  "Never Used", "Former User" , "Current User")) %>%
    mutate(SurveyName=women_data)
  
  use_mar_status<- as.data.frame(prop.table(wtd.table(as.factor(women_clean$Mar_Stat), as.factor(women_clean$Status), weights = women_clean$sampleweights)) )   %>%
    mutate(Var2=case_when(Var2== "Never" ~ "Never Used", 
                          Var2== "Former" ~ "Former User", 
                          Var2== "Current" ~ "Current User")) %>%
    mutate(Var2=fct_relevel(Var2,  "Never Used", "Former User" , "Current User")) %>%
    mutate(SurveyName=women_data)
  
  ####################################################################################################################################################################
  
  never_group_n <- as.data.frame(table(as.factor(never_users$group))) %>% rename(N=Freq)
  
  never_group <- as.data.frame(prop.table(wtd.table(as.factor(never_users$group), weights = never_users$sampleweights)))   %>%
    left_join(never_group_n, by="Var1") %>% rename(Group=Var1) %>%
    mutate(SurveyName=women_data)
  
 
  ########################################################################################################################
  # Intention to use- two different ways- of each group what percent have an intention to use, then of those with intention to use group distribution
  
  never_intention_n <-  as.data.frame(table(as.factor(never_users$group))) %>% rename(Group=Var1, N=Freq) %>% mutate(N=as.numeric(as.character(N)))
  never_intention_n_total <- as.data.frame(table(sum(never_intention_n$N))) %>% mutate(Group="All Never Users")  %>% 
    rename(N=Var1) %>% select(Group, N) %>% mutate(N=as.numeric(as.character(N))) %>% bind_rows(never_intention_n)
  
  never_intention_group <- as.data.frame(prop.table(wtd.table(as.factor(never_users$group), as.factor(never_users$v362), weights = never_users$sampleweights),1)) %>%
    rename(Group=Var1) %>% mutate(Var1=as.numeric(as.character(Var2))) %>% left_join(intention_labels, by="Var1") %>% select(Group, Intention, Freq)
  never_intention <- as.data.frame(prop.table(wtd.table(as.factor(never_users$v362), weights = never_users$sampleweights))) %>% mutate(Var1=as.numeric(as.character(Var1))) %>% 
    left_join(intention_labels, by="Var1") %>% mutate(Group="All Never Users") %>% select(Group, Intention, Freq) %>% bind_rows(never_intention_group) %>% 
    left_join(never_intention_n_total, by="Group") %>% arrange(Group, Intention) %>%
    mutate(SurveyName=women_data)
  
  never_has_intention <- never_users %>% filter(v362==2)
  never_has_intention_group <- as.data.frame(prop.table(wtd.table(as.factor(never_has_intention$group), weights = never_has_intention$sampleweights)))  %>% rename(Group=Var1)   %>%
    mutate(SurveyName=women_data)
  
  ####################################################################################################################################################################
  
  former_group_n <- as.data.frame(table(as.factor(former_users$group))) %>% rename(N=Freq)
  
  former_group <- as.data.frame(prop.table(wtd.table(as.factor(former_users$group), weights = former_users$sampleweights)))   %>%
    left_join(former_group_n, by="Var1") %>% rename(Group=Var1) %>%
    mutate(SurveyName=women_data)
  
  ########################################################################################################################
  # Intention to use- two different ways- of each group what percent have an intention to use, then of those with intention to use group distribution
  
  former_intention_n <-  as.data.frame(table(as.factor(former_users$group))) %>% rename(Group=Var1, N=Freq) %>% mutate(N=as.numeric(as.character(N)))
  former_intention_n_total <- as.data.frame(table(sum(former_intention_n$N))) %>% mutate(Group="All Never Users")  %>% 
    rename(N=Var1) %>% select(Group, N) %>% mutate(N=as.numeric(as.character(N))) %>% bind_rows(former_intention_n)
  
  former_intention_group <- as.data.frame(prop.table(wtd.table(as.factor(former_users$group), as.factor(former_users$v362), weights = former_users$sampleweights),1)) %>%
    rename(Group=Var1) %>% mutate(Var1=as.numeric(as.character(Var2))) %>% left_join(intention_labels, by="Var1") %>% select(Group, Intention, Freq)
  former_intention <- as.data.frame(prop.table(wtd.table(as.factor(former_users$v362), weights = former_users$sampleweights))) %>% mutate(Var1=as.numeric(as.character(Var1))) %>% 
    left_join(intention_labels, by="Var1") %>% mutate(Group="All Never Users") %>% select(Group, Intention, Freq) %>% bind_rows(former_intention_group) %>% 
    left_join(former_intention_n_total, by="Group") %>% arrange(Group, Intention) %>%
    mutate(SurveyName=women_data)
  
  former_has_intention <- former_users %>% filter(v362==2)
  former_has_intention_group <- as.data.frame(prop.table(wtd.table(as.factor(former_has_intention$group), weights = former_has_intention$sampleweights)))  %>% rename(Group=Var1)   %>%
    mutate(SurveyName=women_data)
  
  ########################################################################################################################
  
  # why discontinued 
  
  if (exists("v360", women_clean) &  mean(former_users_check$v302a) > 1 & women_data!="ETIR61FL.DTA" ) {
  
    why_discontinued_group <- as.data.frame(prop.table(wtd.table(as.factor(former_users_calendar$group), as.factor(former_users_calendar$v360), weights = former_users_calendar$sampleweights),1))
    
    why_discontinued <- as.data.frame(prop.table(wtd.table(as.factor(former_users_calendar$v360), weights = former_users_calendar$sampleweights))) %>%
      rename(Var2=Var1) %>% mutate(Var1="All Former Users") %>% select(Var1, Var2, Freq) %>%
      bind_rows(why_discontinued_group) %>% mutate(Var2=as.numeric(as.character(Var2))) %>% 
      left_join(why_discontinued_labels, by="Var2") %>%
      select(-Var2)   %>%
      mutate(SurveyName=women_data)
    }
  
  #################################################################################################
  # Calendar analysis
  
  if (exists("vcal_1", women_clean)  &  mean(former_users_check$v302a) > 1 & women_data!="ETIR61FL.DTA") {
    
    women_cal <- women_clean %>% filter(Status=="Former") %>% select(caseid,  vcal_1)
    
    # set length of calendar in a local macro
    vcal_len <- as.numeric(nchar(women_cal$vcal_1[1]))
    length_data <- seq( vcal_len, 1, -1)
    oplength_data <- seq( 1, vcal_len,  1)
    
    length_data <- data.frame(length_data, oplength_data)
    
    for (row in 1:nrow(length_data)) {
      var <- paste("vcal1_", length_data[row, "oplength_data"], sep="")
      num <-  paste(length_data[row, "length_data"])
      
      # contraceptive method, non-use, or birth, pregnancy, or termination
      women_cal[[var]] = substr(women_cal$vcal_1, num, num)
    }
    
    women_cal <- women_cal %>% select(-vcal_1) 
    women_cal <- women_cal %>% gather(Variable, Value, 2:ncol(women_cal)) %>% 
      arrange(caseid) %>% separate(Variable, c("Vcal", "Num"), sep="_") %>%
      mutate(Num=as.numeric(as.character(Num))) %>%
      filter(Value!="") %>%
      filter(Value!=" ") %>%
      filter(Value!="0") %>%
      filter(Value!="B") %>%
      filter(Value!="P") %>%
      filter(Value!="T")  
    
    levels(as.factor(women_cal$Value))
    
    women_cal_min <- women_cal %>% group_by(caseid) %>% summarise(Month= min(Num)) %>%
      mutate(Time_Since_Use=case_when(Month< 12 ~ "Under a Year",
                                      Month >=12 & Month <24 ~ "1 Year",
                                      Month >=24 & Month <36 ~ "2 Year",
                                      Month >=36 & Month <48 ~ "3 Year",
                                      Month >=48 & Month <60 ~ "4 Year",
                                      Month >=60 ~ "5+ Years")) %>% 
      select(-Month)
    
    former_time_use <- full_join(former_users, women_cal_min, by="caseid") %>%
      mutate(Time_Since_Use=case_when(!is.na(Time_Since_Use) ~ Time_Since_Use,
                                      is.na(Time_Since_Use)  ~ time_discontinuation)) %>%
      mutate(Time_Since_Use=case_when(Time_Since_Use == "yes, used outside calendar" |  Time_Since_Use == "Yes, used outside calendar" ~ "5+ Years",
                                      Time_Since_Use != "yes, used outside calendar" &  Time_Since_Use !="Yes, used outside calendar" ~ Time_Since_Use))
    
    former_time_use_group <- as.data.frame(prop.table(wtd.table(as.factor(former_time_use$Time_Since_Use), weights = former_time_use$sampleweights))) %>% rename(Time_Since_Use=Var1)   %>%
      mutate(SurveyName=women_data)
    
  }
  
  #################################################################################################
  #################################################################################################
  # Current Use
  
  method_labels <- as.data.frame(attr(women_clean$v312,"labels")) %>% rownames_to_column() %>% rename(Var1=2, Method=1) %>% mutate(Var1=as.numeric(as.character(Var1))) %>% 
    mutate(Method=str_to_title(Method))
  if (exists("mii", women_clean) & !is.na(sum(current_users$mii, na.rm = T)) & sum(current_users$mii, na.rm = T)>0) {
    
  
  
  mii_total <- as.data.frame(prop.table(wtd.table(as.factor(current_users$mii),  weights = current_users$sampleweights))) %>%
    filter(Var1==1) %>% mutate(Method="Total") %>% mutate(Var="Method Information Index") %>% select(Method, Var, Freq)
  
  told_om_total <- as.data.frame(prop.table(wtd.table(as.factor(current_users$told_om),  weights = current_users$sampleweights)))  %>%
    filter(Var1==1) %>% mutate(Method="Total") %>% mutate(Var="Told About Other Methods") %>% select(Method, Var, Freq)
  
  told_side_eff_total <- as.data.frame(prop.table(wtd.table(as.factor(current_users$told_side_eff ),  weights = current_users$sampleweights)))  %>%
    filter(Var1==1) %>% mutate(Method="Total") %>% mutate(Var="Told About Side Effects") %>% select(Method, Var, Freq)
  
  told_to_do_total <- as.data.frame(prop.table(wtd.table(as.factor(current_users$told_to_do ),  weights = current_users$sampleweights)))  %>%
    filter(Var1==1) %>% mutate(Method="Total") %>% mutate(Var="Told What to Do About Side Effects") %>% select(Method, Var, Freq)
  
  

  
  mii_method <-  as.data.frame(prop.table(wtd.table(as.factor(current_users$v312), as.factor(current_users$mii), weights = current_users$sampleweights),1)) %>%
    filter(!is.na(Freq)) %>% filter(Var2==1) %>% mutate(Var="Method Information Index") 
  
  
  told_om_method <-  as.data.frame(prop.table(wtd.table(as.factor(current_users$v312), as.factor(current_users$told_om), weights = current_users$sampleweights),1)) %>%
    filter(!is.na(Freq)) %>% filter(Var2==1) %>% mutate(Var="Told About Other Methods") 
  
  
  told_side_eff_method <-  as.data.frame(prop.table(wtd.table(as.factor(current_users$v312), as.factor(current_users$told_side_eff), weights = current_users$sampleweights),1)) %>%
    filter(!is.na(Freq)) %>% filter(Var2==1) %>% mutate(Var="Told About Side Effects") 
  
  
  told_to_do_method <-  as.data.frame(prop.table(wtd.table(as.factor(current_users$v312), as.factor(current_users$told_to_do), weights = current_users$sampleweights),1)) %>%
    filter(!is.na(Freq)) %>% filter(Var2==1) %>% mutate(Var="Told What to Do About Side Effects") 
  
  method_list <- c(1, 2, 3, 6, 11)
  
  mii_method_all <- bind_rows(mii_method, told_om_method, told_side_eff_method, told_to_do_method) %>% filter(Var1 %in% method_list) %>% 
    mutate(Var1=as.numeric(as.character(Var1))) %>% left_join(method_labels, by="Var1") %>% 
    select(Method, Var, Freq)  %>% bind_rows(mii_total, told_om_total, told_side_eff_total, told_to_do_total) %>%
    mutate(SurveyName=women_data)
  
  }
  
  #################################################################################################
  
  
  current_group <- as.data.frame(prop.table(wtd.table(as.factor(current_users$group), weights = current_users$sampleweights))) %>% 
    rename(Group=Var1) %>%
    mutate(SurveyName=women_data)
  
  #################################################################################################
  
  
  current_method_mix <- as.data.frame(prop.table(wtd.table(as.factor(current_users$v312),  weights = current_users$sampleweights))) %>% 
    mutate(Var1=as.numeric(as.character(Var1))) %>% 
    left_join(method_labels, by="Var1")  %>% select(Method,  Freq)  %>%
    mutate(SurveyName=women_data)
  #################################################################################################
  
  source_labels <- as.data.frame(attr(current_users$v327,"labels")) %>% rownames_to_column()  %>%
    rename(Var1=2, Source=1) %>% mutate(Var1=as.numeric(as.character(Var1))) %>% 
    mutate(Source=str_to_title(Source))
  
  current_source <- as.data.frame(prop.table(wtd.table(as.factor(current_users$v327),  weights = current_users$sampleweights)))   %>% 
    mutate(Var1=as.numeric(as.character(Var1))) %>% 
    left_join(source_labels, by="Var1") %>% select(Source,  Freq)  %>% mutate(Source=case_when(!is.na(Source) ~ Source,
                                                                                               is.na(Source) ~ "Missing")) %>%
    mutate(SurveyName=women_data)
  #################################################################################################

  current_timesince <-  as.data.frame(prop.table(wtd.table(as.factor(current_users$time_use_group),  weights = current_users$sampleweights)))  %>%
    mutate(SurveyName=women_data)
  ########################################################################################################################
  # Empowerment
  if (exists("v632a", women_clean) & !is.na(sum(never_mar$v632a, na.rm = T)) & sum(never_mar$v632a, na.rm = T)>0  ) {
    
    v632a_labels <- as.data.frame(attr(never_mar$v632a,"labels")) %>% rownames_to_column()  %>%
      rename(Var1=2, Decider=1) %>% mutate(Var1=as.numeric(as.character(Var1))) %>% 
      mutate(Decider=str_to_title(Decider))
    
    never_empowerment <- as.data.frame(prop.table(wtd.table(as.factor(never_mar$v632a), weights = never_mar$sampleweights)))    %>%
      mutate(Var1=as.numeric(as.character(Var1))) %>%  left_join(v632a_labels, by="Var1") %>% mutate(Group="Never User") %>%
      select(Group, Decider, Freq)  %>%
      mutate(SurveyName=women_data)
    former_empowerment <- as.data.frame(prop.table(wtd.table(as.factor(former_mar$v632a), weights = former_mar$sampleweights))) %>%
      mutate(Var1=as.numeric(as.character(Var1))) %>%  left_join(v632a_labels, by="Var1") %>% mutate(Group="Former User") %>%
      select(Group, Decider, Freq)  %>%
      mutate(SurveyName=women_data)
    
  }
  
  if (exists("v632", women_clean) & !is.na(sum(current_users$v632, na.rm = T)) & sum(current_users$v632, na.rm = T)>0  ) {
    
    v632_labels <- as.data.frame(attr(current_users$v632,"labels")) %>% rownames_to_column()  %>%
      rename(Var1=2, Decider=1) %>% mutate(Var1=as.numeric(as.character(Var1))) %>% 
      mutate(Decider=str_to_title(Decider))
    
    current_empowerment <- as.data.frame(prop.table(wtd.table(as.factor(current_mar$v632), weights = current_mar$sampleweights))) %>%
      mutate(Var1=as.numeric(as.character(Var1))) %>%  left_join(v632_labels, by="Var1") %>% mutate(Group="Current User") %>%
      select(Group, Decider, Freq)  %>%
      mutate(SurveyName=women_data)
    
  }
  
  
  #################################################################################################
  results_use_status <- bind_rows(results_use_status, use_status)
  results_use_age_status <- bind_rows(results_use_age_status, use_age_status)
  results_use_mar_status <- bind_rows(results_use_mar_status, use_mar_status)
  results_never_group <- bind_rows(results_never_group, never_group)
  results_never_intention <- bind_rows(results_never_intention, never_intention)
  results_never_has_intention <- bind_rows(results_never_has_intention, never_has_intention_group)
  results_former_group <- bind_rows(results_former_group, former_group)
  results_former_intention <- bind_rows(results_former_intention, former_intention)
  results_former_has_intention <- bind_rows(results_former_has_intention, former_has_intention_group)
  
  if (exists("v360", women_clean) &  mean(former_users_check$v302a) > 1 & women_data!="ETIR61FL.DTA") {
    results_former_why_discontinue <- bind_rows(results_former_why_discontinue, why_discontinued)
    
  }
  
  if (exists("vcal_1", women_clean) &  mean(former_users_check$v302a) > 1 & women_data!="ETIR61FL.DTA") {
    results_former_time_use_group <- bind_rows(results_former_time_use_group, former_time_use_group)
    
  }
  
  if (exists("mii", women_clean) & !is.na(sum(current_users$mii, na.rm = T)) & sum(current_users$mii, na.rm = T)>0) {
  results_mii <- bind_rows(results_mii, mii_method_all)
  }
  
  results_current_group <- bind_rows(results_current_group, current_group)
  results_current_methodmix <- bind_rows(results_current_methodmix, current_method_mix)
  results_current_source <- bind_rows(results_current_source, current_source)
  results_current_timesince <- bind_rows(results_current_timesince, current_timesince)
  
  if (exists("v632", women_clean) & !is.na(sum(current_users$v632, na.rm = T)) & sum(current_users$v632, na.rm = T)>0  ) {
    results_empowerment <- bind_rows(results_empowerment, current_empowerment)
  }
  if (exists("v632a", women_clean)  & !is.na(sum(never_mar$v632a, na.rm = T)) & sum(never_mar$v632a, na.rm = T)>0  ) {
    results_empowerment <- bind_rows(results_empowerment, never_empowerment, former_empowerment)
  }
  
  
  ####################################################################################################################################################################
  
}



########################################################################################
########################################################################################
########################################################################################





########################################################################################
use_status_clean <- results_use_status %>% left_join(surveys_mini_info, by="SurveyName")
use_age_status_clean <- results_use_age_status %>% left_join(surveys_mini_info, by="SurveyName") %>%
  mutate(Var2 = fct_relevel(Var2, c( "Never Used", "Former User", "Current User")))
use_mar_status_clean <- results_use_mar_status %>% left_join(surveys_mini_info, by="SurveyName")


never_group_clean <- results_never_group %>% left_join(surveys_mini_info, by="SurveyName")
never_intention_clean <- results_never_intention %>% left_join(surveys_mini_info, by="SurveyName") %>%
  mutate(Intention= str_to_title(Intention)) %>%
  mutate(Intention=case_when(is.na(Intention) ~ "Missing", !is.na(Intention) ~ Intention))

never_has_intention_clean <- results_never_has_intention %>% left_join(surveys_mini_info, by="SurveyName")

former_group_clean <- results_former_group %>% left_join(surveys_mini_info, by="SurveyName")
former_intention_clean <- results_former_intention %>% left_join(surveys_mini_info, by="SurveyName")
former_has_intention_clean <- results_former_has_intention %>% left_join(surveys_mini_info, by="SurveyName")
former_why_discontinue_clean <- results_former_why_discontinue %>% left_join(surveys_mini_info, by="SurveyName") %>%
  mutate(WhyDisc= str_to_title(WhyDisc)) %>% mutate(WhyDisc=case_when(!is.na(WhyDisc) ~ WhyDisc, is.na(WhyDisc) ~ "Missing"))

former_time_use_group_clean <- results_former_time_use_group %>% left_join(surveys_mini_info, by="SurveyName") %>%
  filter(Freq!=0) %>%
  mutate(Time_Since_Use = fct_relevel(Time_Since_Use, c("Under a Year", "1 Year", "2 Year", "3 Year", "4 Year", "5+ Years" )))


levels(as.factor(results_mii$Method))

mii_clean <- results_mii %>% left_join(surveys_mini_info, by="SurveyName") %>% mutate(Method=case_when(Method=="Iud" ~ "IUD",
                                                                                                       Method=="Implants/Norplant" ~ "Implant",
                                                                                                       Method== "Norplant"   ~ "Implant",
                                                                                                       Method=="Injections 3 Month" ~ "Injections" ,
                                                                                                       Method!="Iud" &  Method!="Implants/Norplant" & Method!= "Injections 3 Month" & Method!="Norplant" ~ Method))

current_group_clean <- results_current_group %>% left_join(surveys_mini_info, by="SurveyName") %>%
  mutate(Group=case_when(
    Group=="Married,  Current User" ~ "Married" ,
    Group=="Never Married,  Current User" ~ "Never Married",
    Group== "Formerly Married,  Current User" ~ "Formerly Married")) %>%
  mutate(Group = fct_relevel(Group, c("Married", "Never Married", "Formerly Married")))

current_methodmix_clean <- results_current_methodmix %>% left_join(surveys_mini_info, by="SurveyName") %>% mutate(Method=case_when(Method== "Injections" | Method== "Injection (3 Monthly)" | Method== "Injections 3 Month"  ~ "Injections" ,         
                                                                                                                                   Method== "Injection 1 Months"  | Method=="Injection (Monthly)"  ~ "Injection 1 Months"  ,             
                                                                                                                                   Method==  "Injections 2 Month"   ~ "Injections 2 Month",             
                                                                                                                                   Method== "Standard Days"  | Method=="Standard Days (Sdm)"   | Method=="Standard Days Method"  | Method=="Standard Days Method (Sdm)"  | Method=="Collier"   | Method=="Collier (Cs)"    | Method== "Cycle Beads/Standard Days"  | Method=="Fertility Wheel Calculator"   | Method== "Fixed Days (Collar)"  | Method==  "Fixed Days (String)"  ~ "Standard Days Method", 
                                                                                                                                   Method=="Basal Body Temperature"   | Method==  "Symptothermal" ~ "Basal Body Temperature" ,         
                                                                                                                                   Method=="Billings Method"    | Method== "Mucus/ Billings/ Ovulation"  ~ "Billings Method",              
                                                                                                                                   Method=="Chinese (Monthly) Pill" ~ "Chinese (Monthly) Pill" ,                  
                                                                                                                                   Method== "Diaphragm"  ~ "Diaphragm" ,
                                                                                                                                   Method=="Diaphragm/Foam/Jelly"    | Method=="Foam/Jelly/Tablets/Ovule/Diaphragm" ~ "Diaphragm/Foam/Jelly",
                                                                                                                                   Method=="Foam Or Jelly"  ~ "Foam Or Jelly" ,         
                                                                                                                                   Method=="Emergency Contraception"    ~"Emergency Contraception"  ,        
                                                                                                                                   Method=="Female Condom"   ~ "Female Condom",               
                                                                                                                                   Method== "Female Sterilization"  ~  "Female Sterilization" ,  
                                                                                                                                   Method=="Implant"   | Method==  "Norplant" | Method=="Implants/Norplant" ~ "Implant" ,                                      
                                                                                                                                   Method== "IUD" | Method=="Iud"     ~ "IUD" ,                         
                                                                                                                                   Method== "Lactational Amenorrhea"    | Method==  "Lactational Amenorrhea (Lam)" ~ "Lactational Amenorrhea"   ,     
                                                                                                                                   Method== "Male Condom"  | Method=="Condom"       ~  "Male Condom",              
                                                                                                                                   Method== "Male Sterilization" ~  "Male Sterilization",                                       
                                                                                                                                   Method== "Other Modern Method" | Method== "Oher Modern Method"   ~ "Other Modern Method", 
                                                                                                                                   Method=="Other" ~ "Other",
                                                                                                                                   Method== "Other Traditional Method"   | Method==  "Other Traditional"  ~ "Other Traditional Method" ,     
                                                                                                                                   Method==  "Periodic Abstinence"  | Method==   "Rhythm/Periodic Abstinence"   ~  "Periodic Abstinence" ,          
                                                                                                                                   Method==  "Pill"   ~ "Pill",                           
                                                                                                                                   Method== "Prolonged Breastfeeding"  ~  "Prolonged Breastfeeding" ,         
                                                                                                                                   Method== "Withdrawal"  ~  "Withdrawal" ))


levels(as.factor(current_methodmix_clean$Method))

current_source_clean <- results_current_source %>% left_join(surveys_mini_info, by="SurveyName") %>%
  mutate(Source=case_when(
    Source== "Don't Know"         ~ "Don't Know"  ,                                                                           
    Source== "Government Clinic/Pharmacy"       |  Source==        "Government Clinic/Pharmacy (Hospital/Health Center/Maternity)" ~ "Government Clinic/Pharmacy" ,
    Source== "Government Home/Community Delivery"        ~ "Government Home/Community Delivery"  ,                    
    Source=="Missing"   ~ "Missing"  ,                                                   
    Source== "Ngo"   ~ "NGO",                                                       
    Source== "Other" ~ "Other",                                                      
    Source== "Pharmacy"  ~ "Pharmacy",                                                  
    Source==  "Private Clinic/Delivery"  ~ "Private Clinic/Delivery",                                    
    Source=="Shop, Church, Friend"    |  Source==   "Drug Vendor, Shop, Friend"   ~ "Shop, Church, Friend"))
levels(as.factor(current_source_clean$Source))

current_timesince_clean <- results_current_timesince %>% left_join(surveys_mini_info, by="SurveyName") %>%
  mutate(Var1 = fct_relevel(Var1, c("Under a Year", "1 Year", "2 Year", "3 Year", "4 Year", "5+ Years" ))) %>%
  select(Var1, Freq, SurveyName, Country, StartYear)

empowerment_clean <- results_empowerment %>% left_join(surveys_mini_info, by="SurveyName")%>% mutate(Decider=case_when(!is.na(Decider) ~ Decider, is.na(Decider) ~ "Missing")) %>%
  mutate(Group = fct_relevel(Group, c( "Never User", "Former User", "Current User")))


########################################################################################

setwd("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Shiny Input Files 060123")
write.csv(use_status_clean , "use_status_clean 060123.csv", row.names=F, na="")
write.csv(use_age_status_clean, "use_age_status_clean, 060123.csv", row.names=F, na="")
write.csv(use_mar_status_clean , "use_mar_status_clean 060123.csv", row.names=F, na="")
write.csv(never_group_clean , "never_group_clean 060123.csv", row.names=F, na="")
write.csv(never_intention_clean , "never_intention_clean 060123.csv", row.names=F, na="")
write.csv(never_has_intention_clean , "never_has_intention_clean 060123.csv", row.names=F, na="")
write.csv(former_group_clean , "former_group_clean 060123.csv", row.names=F, na="")
write.csv(former_intention_clean , "former_intention_clean 060123.csv", row.names=F, na="")
write.csv(former_has_intention_clean, "former_has_intention_clean 060123.csv", row.names=F, na="")
write.csv(former_why_discontinue_clean , "former_why_discontinue_clean 060123.csv", row.names=F, na="")
write.csv(former_time_use_group_clean , "former_time_use_group_clean 060123.csv", row.names=F, na="")
write.csv(mii_clean , "mii_clean 060123.csv", row.names=F, na="")
write.csv(current_group_clean, "current_group_clean 060123.csv", row.names=F, na="")
write.csv(current_methodmix_clean, "current_methodmix_clean 060123.csv", row.names=F, na="")
write.csv(current_source_clean , "current_source_clean 060123.csv", row.names=F, na="")
write.csv(current_timesince_clean , "current_timesince_clean 060123.csv", row.names=F, na="")
write.csv(empowerment_clean , "empowerment_clean 060123.csv", row.names=F, na="")



########################################################################################
########################################################################################
########################################################################################

#######################################################################################
country_list <- as.data.frame(levels(as.factor(surveys_mini_info$Country)))  %>% rename(Country=1)

for (row in 1:nrow(country_list)) {
  Country_name <- country_list[row, "Country"]
  
  
  ########################################
  
  use_status_country <- use_status_clean %>% filter(Country==Country_name)  
  use_age_status_country <- use_age_status_clean %>% filter(Country==Country_name)  %>% mutate(recent=max(StartYear))
  use_mar_status_country <- use_mar_status_clean %>% filter(Country==Country_name)   %>% mutate(recent=max(StartYear))
  
  never_group_country <- never_group_clean %>% filter(Country==Country_name)   %>% mutate(recent=max(StartYear))
  never_intention_country <- never_intention_clean %>% filter(Country==Country_name)   %>%
    mutate(recent=max(StartYear)) %>%  filter(recent==StartYear) %>% filter(N>=50) 
  never_has_intention_country <- never_has_intention_clean %>% filter(Country==Country_name)   %>% mutate(recent=max(StartYear))
  
  former_group_country <- former_group_clean %>% filter(Country==Country_name)   %>% mutate(recent=max(StartYear))
  former_intention_country <- former_intention_clean %>% filter(Country==Country_name)   %>%
    mutate(recent=max(StartYear)) %>%  filter(recent==StartYear) %>% filter(N>=50) %>%
    mutate(Intention= str_to_title(Intention))
  former_has_intention_country <- former_has_intention_clean %>% filter(Country==Country_name)   %>% mutate(recent=max(StartYear))
  former_why_discontinue_country <- former_why_discontinue_clean %>% filter(Country==Country_name)  %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% select(-recent)
  former_time_use_group_country <- former_time_use_group_clean %>% filter(Country==Country_name)  %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% select(-recent)
  
  mii_country <- mii_clean %>% filter(Country==Country_name)  %>% mutate(recent=max(StartYear)) %>% 
    filter(recent==StartYear) %>% select(-recent)  %>% group_by(Method) %>% mutate(Max=max(Freq)) %>% ungroup()
  
  current_group_country <- current_group_clean %>% filter(Country==Country_name)
  current_methodmix_country <- current_methodmix_clean %>% filter(Country==Country_name)
  current_source_country <- current_source_clean %>% filter(Country==Country_name)
  current_timesince_country <- current_timesince_clean %>% filter(Country==Country_name) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% select(-recent)

  empowerment_country <- empowerment_clean %>% filter(Country==Country_name) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% select(-recent)
  
  ########################################
  
  
   recent <- use_age_status_country[1, 7]
  
  ########################################
  ########################################
  ########################################
  
  ggplot(use_status_country, aes(x=as.factor(StartYear), y=Freq*100,  fill=Var1, label=paste(round(Freq*100), "%", sep=""))) +
    geom_bar(stat="identity") + 
    geom_text(size = 6, position = position_stack(vjust = 0.5))+
    labs(x="", y="Percent of WRA Population", shape="", fill="", title=Country_name) +
    scale_fill_manual(values=c("Current User"= "#3892C6", "Former User"= "#8DC645", "Never Used"= "#954F72"),
                      guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(text=element_text(size=20, color="black"),
          axis.text =element_text(size=20, color="black"),
          legend.text =element_text(size=23),
          legend.position = "bottom")
  
 ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/UserDistribution", Country_name,".png"), width=8, height=8, units="in")
  
  # ########################################
   ggplot(subset(use_age_status_country, StartYear==recent), aes(x=Var2, y=Freq*100, fill=Var1, label=paste(round(Freq*100), "%", sep=""))) +
     geom_bar(stat="identity") +
   geom_text(size = 6, position = position_stack(vjust = 0.5))+
     labs(x="", y="Percent of WRA Population", shape="", fill="", title=paste(Country_name, ": Age Distribution, ", recent, sep="")) +
    scale_fill_manual(values=c("15-24" = "#0B9444",  "25-39"= "#FCD116", "40-49"= "#3892C6")) +
     theme_bw() +
   theme(text=element_text(size=20, color="black"),
         axis.text =element_text(size=20, color="black"),
         legend.text =element_text(size=23),
         legend.position = "bottom")
   
  ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/UserDistribution_Age_Recent", recent, Country_name,".png"), width=8, height=8, units="in")
  
  # ########################################
  # ########################################
  # ########################################
  
  ggplot(never_group_country, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=paste(round(Freq*100), "%", sep=""))) +
     geom_bar(stat = "identity") +
    geom_text(size = 6, position = position_stack(vjust = 0.5))+
     labs(x="", y="", fill="", title=paste(Country_name, ": Never Users Population Distribution", sep="")) +
    scale_fill_manual(values=c( "Not Married, Sexually Active"= "#0B9444",  "Not Married, Not Sexually Active" = "#8DC645",  "Infecund, Menopausal"= "#954F72",  "Married, Wants Child within 2 Years"= "#1D4E92",   "Married, Does Not Want Child within 2 Years"= "#3892C6")) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE, rev=TRUE)) +
    theme_bw() +
    theme(text=element_text(size=20, color="black"),
          axis.text =element_text(size=20, color="black"),
          legend.text =element_text(size=15),
          legend.position = "bottom")
  # 
  ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/NeverUserDistribution_AllSurveys", Country_name,".png"), width=12, height=8, units="in")
  # 
  # ########################################
   ggplot(never_intention_country, aes(x=Group, y=Freq * 100, fill=Intention, label=paste(round(Freq*100), "%", sep=""))) +
     geom_bar(stat = "identity") +
    geom_text(size = 6, position = position_stack(vjust = 0.5))+
     scale_x_discrete(limits=rev) +
     coord_flip() +
     labs(x="", y="", fill="", title=paste(Country_name, ", ",  recent, ": Never Users\nIntention to Use in Future", sep="")) +
    scale_fill_manual(values=c("Does Not Intend" = "#3892C6",  "Unsure About Use"= "#FCD116", "Use Later"= "#0B9444")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE, rev=TRUE)) +
     theme_bw() +
    theme(text=element_text(size=20, color="black"),
          axis.text =element_text(size=20, color="black"),
          legend.text =element_text(size=20),
          legend.position = "bottom")
  ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/NeverUserIntentionToUse", recent, Country_name,".png"), width=12, height=8, units="in")
  # 
  # ########################################
  never_pie <- never_has_intention_country %>% filter(StartYear==recent)  %>% mutate(values=round(Freq*100)) %>% filter(values>0) %>%
    arrange(-values) 
  never_pie$Group <- reorder(never_pie$Group, never_pie$values)
  never_pie <- never_pie %>% mutate(pos = cumsum(values)- values/2) %>% mutate(labels=case_when(values>=3 ~ values, values< 3 ~ NA_real_)) %>%
    mutate(percent = case_when(values>=3 ~ "%", values< 3 ~ "")) %>% mutate(labels=paste(labels, percent, sep=""))
  
  
  ggplot(never_pie, aes(x="", y=values, fill=Group))+
    geom_bar(width = 1, stat = "identity")+ 
    coord_polar("y", start=0) +
    labs(fill="", title=paste(Country_name, ": Never Users who Intend to Use, ", recent, sep="")) +
    geom_text(aes(x= "", y=pos, label = labels), size=7) +
    scale_fill_manual(values=c( "Not Married, Sexually Active"= "#0B9444",  "Not Married, Not Sexually Active" = "#8DC645",  "Infecund, Menopausal"= "#954F72",  "Married, Wants Child within 2 Years"= "#1D4E92",   "Married, Does Not Want Child within 2 Years"= "#3892C6")) +
    guides(fill = guide_legend(nrow=5,byrow=TRUE,  reverse=TRUE)) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x=element_blank(),
      text=element_text(size=20),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      legend.text =element_text(size=20),
      legend.position = "bottom")

    ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/NeverUsersIntentionDist", recent, Country_name,".png"), width=12, height=8, units="in")
  
  # ########################################
  # ########################################
  # ########################################
   ggplot(former_group_country, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=paste(round(Freq*100), "%", sep=""))) +
     geom_bar(stat = "identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
     labs(x="", y="", fill="", title=paste(Country_name, ": Former Users Population Distribution", sep="")) +
      scale_fill_manual(values=c( "Not Married, Sexually Active"= "#0B9444",  "Not Married, Not Sexually Active" = "#8DC645",  "Infecund, Menopausal"= "#954F72",  "Married, Wants Child within 2 Years"= "#1D4E92",   "Married, Does Not Want Child within 2 Years"= "#3892C6")) +
      guides(fill = guide_legend(nrow=3,byrow=TRUE,  reverse=TRUE)) +
      theme_bw()  +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=15),
            legend.position = "bottom")
  
   ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/FormerUserDistribution_AllSurveys", Country_name,".png"), width=12, height=8, units="in")
    
  # ########################################
  ggplot(former_intention_country, aes(x=Group, y=Freq * 100, fill=Intention, label=paste(round(Freq*100), "%", sep=""))) +
    geom_bar(stat = "identity") +
     geom_text(size = 6, position = position_stack(vjust = 0.5))+
    scale_x_discrete(limits=rev) +
    coord_flip() +
    labs(x="", y="", fill="", title=paste(Country_name, ", ",  recent, ": Former Users\nIntention to Use in Future", sep="")) +
    scale_fill_manual(values=c("Does Not Intend" = "#3892C6",  "Unsure About Use"= "#FCD116", "Use Later"= "#0B9444")) +
     guides(fill=guide_legend(nrow=2,byrow=TRUE, rev=TRUE)) +
     theme_bw() +
     theme(text=element_text(size=20, color="black"),
           axis.text =element_text(size=20, color="black"),
           legend.text =element_text(size=20),
           legend.position = "bottom")
   
  
   ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/FormerUserIntentionToUse", recent, Country_name,".png"), width=12, height=8, units="in")
  
  # ########################################
  
  pie_former <- former_has_intention_country %>% filter(StartYear==recent)  %>% mutate(values=round(Freq*100)) %>% filter(values>0) %>%
    arrange(-values) 
  pie_former$Group <- reorder(pie_former$Group, pie_former$values)
  pie_former <- pie_former %>% mutate(pos = cumsum(values)- values/2) %>% mutate(labels=case_when(values>=3 ~ as.character(values), values< 3 ~ "")) %>%
    mutate(percent = case_when(values>=3 ~ "%", values< 3 ~ "")) %>% mutate(labels=paste(labels, percent, sep=""))
  
  
  ggplot(pie_former, aes(x="", y=values, fill=Group))+
    geom_bar(width = 1, stat = "identity")+ 
    coord_polar("y", start=0) +
    labs(fill="", title=paste(Country_name, ": Former Users who Intend to Use, ", recent, sep="")) +
    geom_text(aes(x= "", y=pos, label = labels), size=7) +
    scale_fill_manual(values=c( "Not Married, Sexually Active"= "#0B9444",  "Not Married, Not Sexually Active" = "#8DC645",  "Infecund, Menopausal"= "#954F72",  "Married, Wants Child within 2 Years"= "#1D4E92",   "Married, Does Not Want Child within 2 Years"= "#3892C6")) +
    guides(fill = guide_legend(nrow=5,byrow=TRUE,  reverse=TRUE)) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x=element_blank(),
      text=element_text(size=20),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      legend.text =element_text(size=20),
      legend.position = "bottom")
  
  
  ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/FormerUsersIntentionDist", recent, Country_name,".png"), width=12, height=8, units="in")
  
  ###########################################################
  former_why_discontinue_country_wide <- former_why_discontinue_country %>% select(Var1, WhyDisc, Freq) %>% rename(Group=Var1) %>%
    mutate(Freq= paste(round(Freq * 100, 1), "%", sep = "")) %>% spread(WhyDisc, Freq)
  
  write.csv(former_why_discontinue_country_wide, paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/FormerUsersWhyDist", recent, Country_name,".csv"), row.names = F, na="")
  
  ###########################################################

  ggplot(former_time_use_group_country, aes(x=Time_Since_Use, y=Freq*100, fill=Time_Since_Use)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=paste(round(Freq*100), "%", sep="")), vjust=-.5, size=6) +
  labs(x="", y="Percent of Former Users", fill="", title=paste(Country_name, ", ",  recent, ": When Did Former Users Last Use Family Planning", sep="")) +
    scale_fill_manual(values=c("Under a Year" = "#F49100" , "1 Year" = "#FCD116", "2 Year"= "#8DC645", "3 Year"= "#0B9444",  "4 Year"= "#3892C6", "5+ Years"= "#1D4E92")) +
    theme_bw() +
    theme(text=element_text(size=20, color="black"),
          axis.text =element_text(size=20, color="black"),
          legend.text =element_text(size=20),
          legend.position = "none")
  
  ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/FormerUsersTimeSinceUse", recent, Country_name,".png"), width=12, height=8, units="in")

  ###########################################################
  ###########################################################
  ###########################################################
  

  ggplot(current_group_country, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=paste(round(Freq*100), "%", sep=""))) +
    geom_bar(stat = "identity") +
    geom_text(size = 6, position = position_stack(vjust = 0.5))+
    labs(x="", y="", fill="", title=paste(Country_name, ": Current Users Population Distribution", sep="")) +
  scale_fill_manual(values=c( "Never Married"= "#8DC645",  "Formerly Married"= "#FCD116",  "Married"= "#3892C6")) +
   theme_bw() +
    theme(text=element_text(size=20, color="black"),
          axis.text =element_text(size=20, color="black"),
          legend.text =element_text(size=20),
          legend.position = "bottom")

 ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/CurrentUserDistribution_AllSurveys", Country_name,".png"), width=12, height=8, units="in")
  
 ###########################################################
 
 ggplot(current_methodmix_country, aes(x=as.factor(StartYear), y=Freq * 100, fill=Method)) +
   geom_bar(stat = "identity") +
   labs(x="", y="", fill="", title=paste(Country_name, ": Current Users Method Mix", sep="")) +
   theme_bw()  +
   theme(text=element_text(size=20, color="black"),
         axis.text =element_text(size=20, color="black"),
         legend.text =element_text(size=15))
 
 ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/CurrentUserMethodMix_AllSurveys", Country_name,".png"), width=12, height=8, units="in")
 
 
 ###########################################################
 ggplot(mii_country, aes(x=Method, y=Freq*100, color=Var, shape=Var, fill=Var)) +
   geom_segment(aes(x=Method, xend=Method, y=0, yend=Max*100), color="black") +
   geom_point(size=7) +
   scale_shape_manual(na.translate = F,
                      values = c(21, 22,   23,  24)) +
   scale_fill_manual(values=c("#F49100", "#FCD116", "#0B9444","#1D4E92")) +
   scale_color_manual(values=c("#F49100", "#FCD116", "#0B9444","#1D4E92")) +
   labs(x="", y="Percent", fill="", color="", shape="", title=paste(Country_name, ", ",  recent, ": Method Information Index", sep="")) +
   coord_flip() +
   guides(color = guide_legend(nrow=2,byrow=TRUE), shape = guide_legend(nrow=2,byrow=TRUE), fill = guide_legend(nrow=2,byrow=TRUE)) +
   theme_bw() +
   theme(text=element_text(size=20, color="black"),
         axis.text =element_text(size=20, color="black"),
         legend.text =element_text(size=20),
         legend.position = "bottom")
 
 ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/MIIMethod", recent, Country_name,".png"), width=12, height=8, units="in")
 
 ###########################################################
 
 ggplot(current_source_country, aes(x=as.factor(StartYear), y=Freq * 100, fill=Source)) +
   geom_bar(stat = "identity") +
   guides(fill = guide_legend(ncol=2,byrow=TRUE)) +
   labs(x="", y="", fill="", title=paste(Country_name, ": Current Users Method Source", sep="")) +
   theme_bw() +
   theme(text=element_text(size=20, color="black"),
         axis.text =element_text(size=20, color="black"),
         legend.text =element_text(size=20),
         legend.position = "bottom")
 
 ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/CurrentUserMethodSource_AllSurveys", Country_name,".png"), width=12, height=8, units="in")
 
 ###########################################################
 
 ggplot(current_timesince_country, aes(x=Var1, y=Freq*100, fill=Var1)) +
   geom_bar(stat="identity") +
   geom_text(aes(label=paste(round(Freq*100), "%", sep="")), vjust=-.5, size=6) +
   labs(x="", y="Percent of Current Users", fill="", title=paste(Country_name, ", ",  recent, ": When Did Current Users Start Using Current Method", sep="")) +
   scale_fill_manual(values=c("Under a Year" = "#F49100" , "1 Year" = "#FCD116", "2 Year"= "#8DC645", "3 Year"= "#0B9444",  "4 Year"= "#3892C6", "5+ Years"= "#1D4E92")) +
   theme_bw() +
   theme(text=element_text(size=20, color="black"),
         axis.text =element_text(size=20, color="black"),
         legend.text =element_text(size=20),
         legend.position = "none")
 
 
 ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/CurrentUsersTimeSinceUse", recent, Country_name,".png"), width=12, height=8, units="in")
 ###########################################################
 ###########################################################
 ###########################################################
 ggplot(empowerment_country, aes(x=Group, y=Freq*100, fill=Decider, label=paste(round(Freq*100), "%", sep=""))) +
   geom_bar(stat="identity") +
   geom_text(size = 6, position = position_stack(vjust = 0.5))+
   scale_fill_manual(values=c("#F49100", "#FCD116", "#0B9444","#3892C6")) +
   labs(x="", y="Percent of Married Women in Each Group", fill="", title=paste(Country_name, ", ",  recent, ": Who Made the Decision to Use or Not Use Family Planning", sep="")) +
   theme_bw() +
   theme(text=element_text(size=20, color="black"),
         axis.text =element_text(size=20, color="black"),
         legend.text =element_text(size=20),
         legend.position = "bottom")
 
 
 ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/Empowerment", recent, Country_name,".png"), width=12, height=8, units="in")
 
 
  }