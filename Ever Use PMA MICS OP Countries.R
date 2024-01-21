# Fp2030 Survey Based Indicators from a PMA

# Kristin Bietsch, PhD
# Track20 Project, Avenir Health

library(dplyr)
library(tidyr)
library(haven)
library(sjlabelled)
library(DHS.rates)
library(questionr)


Burkina <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Burkina Faso/PMA2022_BFP3_HQFQ_v1.0_3Oct2022/PMA2022_BFP3_HQFQ_v1.0_3Oct2022.dta")

Burkina_clean <- Burkina %>%  filter(!is.na(FQweight)) %>%
  mutate(EverUse=case_when(current_user==1 ~ "Current User",
                           fp_ever_used==1 ~ "Former User",
                           fp_ever_used==0 ~ "Never Used")) %>%
  mutate(Unmarried=case_when(FQmarital_status==3 | FQmarital_status==4 | FQmarital_status==5 ~ 1,
                             FQmarital_status==1 | FQmarital_status==2 ~ 0 )) %>%
  mutate(SexActive=case_when(age_at_first_sex== -77 ~ 0,
                             last_time_sex==1 & last_time_sex_value<31 ~ 1,
                             last_time_sex==1 & last_time_sex_value>=31 ~ 0, 
                             last_time_sex==2 & last_time_sex_value<5 ~ 1,
                             last_time_sex==2 & last_time_sex_value>=5 ~ 0,
                             last_time_sex==3 ~ 0,
                             last_time_sex==4 ~ 0)) %>%
  mutate(UMNSA = case_when(Unmarried==1 & SexActive==0 ~ "UMNSA", TRUE ~ "Other")) %>%
  mutate(Unmet=case_when(unmettot== 1 ~ 1,
                         unmettot==0  ~ 0)) %>% 
  mutate(told_se=case_when(fp_side_effects==1 ~ 1, fp_side_effects!=1 ~ 0),
         told_todo_se=case_when(fp_side_effects==1 & fp_side_effects_instructions==1 ~ 1, 
                                                             fp_side_effects==1 & fp_side_effects_instructions==0 ~ 0,
                                                             fp_side_effects!=1 ~ 0),
         told_om = case_when(fp_told_other_methods==1 ~ 1, fp_told_other_methods!=1 ~ 0),
         told_switch=case_when(fp_told_switch==1 ~ 1, fp_told_switch!=1 ~ 0),
        mii=case_when(told_se==1 & told_todo_se==1 & told_om==1 ~ 1,
                                                    told_se!=1 | told_todo_se!=1 | told_om!=1 ~ 0),
        mii_plus= case_when(told_se==1 & told_todo_se==1 & told_om==1 & told_switch==1 ~ 1 ,
                                                          told_se!=1 | told_todo_se!=1 | told_om!=1 | told_switch!=1 ~ 0))




Burkina_never <- Burkina_clean %>% filter(EverUse== "Never Used")
Burkina_former <- Burkina_clean %>% filter(EverUse== "Former User")

Burkina_current_mod <- Burkina_clean %>% filter(EverUse== "Current User") %>% filter(mcp==1) %>% filter(current_methodnum_rc!=14) %>%
  mutate(method=case_when(current_methodnum_rc == 16 ~ 5,
                          current_methodnum_rc != 16 ~ as.numeric(current_methodnum_rc)))
  
table(Burkina_current_mod$current_methodnum_rc)

method_labels <- as.data.frame(attr(Burkina_current_mod$current_methodnum_rc,"labels")) %>% rownames_to_column() %>% rename(Var2=2, Method=1) %>% 
  mutate(Var2=as.numeric(as.character(Var2)),
         Method=as.character(Method)) 


sel <- Burkina_clean %>% select(fp_ever_user, fp_ever_used, current_user, current_method, EverUse,
                                FQmarital_status, age_at_first_sex, last_time_sex, last_time_sex_value, Unmarried, SexActive, UMNSA,
                                Unmet)
sel <- Burkina_current_mod %>% select(mcp, current_methodnum_rc, told_se, told_todo_se,told_om,mii,  fp_side_effects, fp_side_effects_instructions, fp_told_other_methods)
# missing for fp_ever_user, going to use fp_ever_used

Burkina_ever <- as.data.frame(prop.table(wtd.table(x= Burkina_clean$EverUse, weights = Burkina_clean$FQweight))) %>%
  mutate(SurveyName="PMA2022_BFP3_HQFQ_v1.0_3Oct2022.dta",
         Country="Burkina Faso",
         StartYear=2022)

Burkina_never_UMNSA <- as.data.frame(prop.table(wtd.table(x= Burkina_never$UMNSA, weights = Burkina_never$FQweight))) %>%
  mutate(SurveyName="PMA2022_BFP3_HQFQ_v1.0_3Oct2022.dta",
         Country="Burkina Faso",
         StartYear=2022)


Burkina_former_unmet <- as.data.frame(prop.table(wtd.table(x= Burkina_former$Unmet, weights = Burkina_former$FQweight))) %>%
  mutate(SurveyName="PMA2022_BFP3_HQFQ_v1.0_3Oct2022.dta",
         Country="Burkina Faso",
         StartYear=2022) %>% 
  filter(Var1==1) %>% select(-Var1)

Burkina_current_mii <- as.data.frame(prop.table(wtd.table(x= Burkina_current_mod$mii, weights = Burkina_current_mod$FQweight))) %>%
  mutate(SurveyName="PMA2022_BFP3_HQFQ_v1.0_3Oct2022.dta",
         Country="Burkina Faso",
         StartYear=2022) %>% 
  filter(Var1==1) %>% select(-Var1) %>%
  mutate(Method="All")


Burkina_current_mii_method_n <- as.data.frame(table(as.factor(Burkina_current_mod$method))) %>% rename(n=Freq, Var2=Var1) %>% 
  mutate(Var2=as.numeric(as.character(Var2)))
Burkina_current_mii_method <- as.data.frame(prop.table(wtd.table(x= Burkina_current_mod$mii, y=as.factor(Burkina_current_mod$method) , weights = Burkina_current_mod$FQweight),2)) %>% 
  filter(Var1==1) %>% select(-Var1)  %>% 
  mutate(Var2=as.numeric(as.character(Var2)))  %>% left_join(method_labels, by="Var2") %>% left_join(Burkina_current_mii_method_n, by="Var2") %>%
  filter(n>=25) %>%
  mutate(SurveyName="PMA2022_BFP3_HQFQ_v1.0_3Oct2022.dta",
         Country="Burkina Faso",
         StartYear=2022) %>%
  bind_rows(Burkina_current_mii)
  

########################################################################
CDI <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Cote d'Ivoire/PMA2022_CIP2_HQFQ_v1.0_1Aug2022/PMA2022_CIP2_HQFQ_v1.0_1Aug2022.dta")


CDI_clean <- CDI %>%  filter(!is.na(FQweight)) %>%
  mutate(EverUse=case_when(current_user==1 ~ "Current User",
                           fp_ever_used==1 ~ "Former User",
                           fp_ever_used==0 ~ "Never Used")) %>%
  mutate(Unmarried=case_when(FQmarital_status==3 | FQmarital_status==4 | FQmarital_status==5 ~ 1,
                             FQmarital_status==1 | FQmarital_status==2 ~ 0 )) %>%
  mutate(SexActive=case_when(age_at_first_sex== -77 ~ 0,
                             last_time_sex==1 & last_time_sex_value<31 ~ 1,
                             last_time_sex==1 & last_time_sex_value>=31 ~ 0, 
                             last_time_sex==2 & last_time_sex_value<5 ~ 1,
                             last_time_sex==2 & last_time_sex_value>=5 ~ 0,
                             last_time_sex==3 ~ 0,
                             last_time_sex==4 ~ 0)) %>%
  mutate(UMNSA = case_when(Unmarried==1 & SexActive==0 ~ "UMNSA", TRUE ~ "Other")) %>%
  mutate(Unmet=case_when(unmettot== 1 ~ 1,
                         unmettot==0  ~ 0))  %>% 
  mutate(told_se=case_when(fp_side_effects==1 ~ 1, fp_side_effects!=1 ~ 0),
         told_todo_se=case_when(fp_side_effects==1 & fp_side_effects_instructions==1 ~ 1, 
                                fp_side_effects==1 & fp_side_effects_instructions==0 ~ 0,
                                fp_side_effects!=1 ~ 0),
         told_om = case_when(fp_told_other_methods==1 ~ 1, fp_told_other_methods!=1 ~ 0),
         told_switch=case_when(fp_told_switch==1 ~ 1, fp_told_switch!=1 ~ 0),
         mii=case_when(told_se==1 & told_todo_se==1 & told_om==1 ~ 1,
                       told_se!=1 | told_todo_se!=1 | told_om!=1 ~ 0),
         mii_plus= case_when(told_se==1 & told_todo_se==1 & told_om==1 & told_switch==1 ~ 1 ,
                             told_se!=1 | told_todo_se!=1 | told_om!=1 | told_switch!=1 ~ 0))

CDI_never <- CDI_clean %>% filter(EverUse== "Never Used")
CDI_former <- CDI_clean %>% filter(EverUse== "Former User")
CDI_current_mod <- CDI_clean %>% filter(EverUse== "Current User") %>% filter(mcp==1)  %>%
  mutate(method=case_when(current_methodnum_rc == 16 ~ 5,
                          current_methodnum_rc != 16 ~ as.numeric(current_methodnum_rc)))
sel <- CDI_current_mod %>% select(mcp, current_methodnum_rc, told_se, told_todo_se,told_om,mii,  fp_side_effects, fp_side_effects_instructions, fp_told_other_methods)

method_labels <- as.data.frame(attr(CDI_current_mod$current_methodnum_rc,"labels")) %>% rownames_to_column() %>% rename(Var2=2, Method=1) %>% 
  mutate(Var2=as.numeric(as.character(Var2)),
         Method=as.character(Method)) 


sel <- CDI_clean  %>% select(fp_ever_user, fp_ever_used, current_user, current_method, EverUse,
                             FQmarital_status, age_at_first_sex, last_time_sex, last_time_sex_value, Unmarried, SexActive, UMNSA )
# missing for fp_ever_user, going to use fp_ever_used

CDI_ever <- as.data.frame(prop.table(wtd.table(x= CDI_clean$EverUse, weights = CDI_clean$FQweight)))  %>%
  mutate(SurveyName="PMA2022_CIP2_HQFQ_v1.0_1Aug2022.dta",
         Country="Cote d'Ivoire",
         StartYear=2022)


CDI_never_UMNSA <- as.data.frame(prop.table(wtd.table(x= CDI_never$UMNSA, weights = CDI_never$FQweight))) %>%
  mutate(SurveyName="PMA2022_CIP2_HQFQ_v1.0_1Aug2022.dta",
         Country="Cote d'Ivoire",
         StartYear=2022)

CDI_former_unmet <- as.data.frame(prop.table(wtd.table(x= CDI_former$Unmet, weights = CDI_former$FQweight)))  %>%
  mutate(SurveyName="PMA2022_CIP2_HQFQ_v1.0_1Aug2022.dta",
         Country="Cote d'Ivoire",
         StartYear=2022) %>% 
  filter(Var1==1) %>% select(-Var1)


CDI_current_mii <- as.data.frame(prop.table(wtd.table(x= CDI_current_mod$mii, weights = CDI_current_mod$FQweight))) %>%
  mutate(SurveyName="PMA2022_CIP2_HQFQ_v1.0_1Aug2022.dta",
         Country="Cote d'Ivoire",
         StartYear=2022) %>% 
  filter(Var1==1) %>% select(-Var1) %>%
  mutate(Method="All")



CDI_current_mii_method_n <- as.data.frame(table(as.factor(CDI_current_mod$method))) %>% rename(n=Freq, Var2=Var1) %>% 
  mutate(Var2=as.numeric(as.character(Var2)))
CDI_current_mii_method <- as.data.frame(prop.table(wtd.table(x= CDI_current_mod$mii, y=as.factor(CDI_current_mod$method) , weights = CDI_current_mod$FQweight),2))  %>% 
  filter(Var1==1) %>% select(-Var1)  %>% 
  mutate(Var2=as.numeric(as.character(Var2)))  %>% left_join(method_labels, by="Var2") %>% left_join(CDI_current_mii_method_n, by="Var2") %>%
  filter(n>=25)  %>%
  mutate(SurveyName="PMA2022_CIP2_HQFQ_v1.0_1Aug2022.dta",
         Country="Cote d'Ivoire",
         StartYear=2022)  %>%
  bind_rows(CDI_current_mii)



########################################################################
Niger <- read_dta("C:/Users/KristinBietsch/files/PMA2020 Data/Niger/PMA2022_NEP2_HQFQ_v1.0_30Nov2022/PMA2022_NEP2_HQFQ_v1.0_30Nov2022.dta" )


Niger_clean <- Niger %>%  filter(!is.na(FQweight)) %>%
  mutate(EverUse=case_when(current_user==1 ~ "Current User",
                           fp_ever_used==1 ~ "Former User",
                           fp_ever_used==0 ~ "Never Used"))  %>%
  mutate(Unmarried=case_when(FQmarital_status==3 | FQmarital_status==4 | FQmarital_status==5 ~ 1,
                             FQmarital_status==1 | FQmarital_status==2 ~ 0 )) %>%
  mutate(SexActive=case_when(age_at_first_sex== -77 ~ 0,
                             last_time_sex==1 & last_time_sex_value<31 ~ 1,
                             last_time_sex==1 & last_time_sex_value>=31 ~ 0, 
                             last_time_sex==2 & last_time_sex_value<5 ~ 1,
                             last_time_sex==2 & last_time_sex_value>=5 ~ 0,
                             last_time_sex==3 ~ 0,
                             last_time_sex==4 ~ 0)) %>%
  mutate(UMNSA = case_when(Unmarried==1 & SexActive==0 ~ "UMNSA", TRUE ~ "Other")) %>%
  mutate(Unmet=case_when(unmettot== 1 ~ 1,
                         unmettot==0  ~ 0))  %>% 
  mutate(told_se=case_when(fp_side_effects==1 ~ 1, fp_side_effects!=1 ~ 0),
         told_todo_se=case_when(fp_side_effects==1 & fp_side_effects_instructions==1 ~ 1, 
                                fp_side_effects==1 & fp_side_effects_instructions==0 ~ 0,
                                fp_side_effects!=1 ~ 0),
         told_om = case_when(fp_told_other_methods==1 ~ 1, fp_told_other_methods!=1 ~ 0),
         told_switch=case_when(fp_told_switch==1 ~ 1, fp_told_switch!=1 ~ 0),
         mii=case_when(told_se==1 & told_todo_se==1 & told_om==1 ~ 1,
                       told_se!=1 | told_todo_se!=1 | told_om!=1 ~ 0),
         mii_plus= case_when(told_se==1 & told_todo_se==1 & told_om==1 & told_switch==1 ~ 1 ,
                             told_se!=1 | told_todo_se!=1 | told_om!=1 | told_switch!=1 ~ 0))

Niger_never <- Niger_clean %>% filter(EverUse== "Never Used")
Niger_former <- Niger_clean %>% filter(EverUse== "Former User")
Niger_current_mod <- Niger_clean %>% filter(EverUse== "Current User") %>% filter(mcp==1)  %>% filter(current_methodnum_rc!=14) %>%
  mutate(method=case_when(current_methodnum_rc == 16 ~ 5,
                          current_methodnum_rc != 16 ~ as.numeric(current_methodnum_rc)))
sel <- Niger_current_mod %>% select(mcp, current_methodnum_rc, told_se, told_todo_se,told_om,mii,  fp_side_effects, fp_side_effects_instructions, fp_told_other_methods)

method_labels <- as.data.frame(attr(Niger_current_mod$current_methodnum_rc,"labels")) %>% rownames_to_column() %>% rename(Var2=2, Method=1) %>% 
  mutate(Var2=as.numeric(as.character(Var2)),
         Method=as.character(Method)) 



sel <- Niger_clean %>% select(fp_ever_user, fp_ever_used, current_user, current_method, EverUse,
                              FQmarital_status, age_at_first_sex, last_time_sex, last_time_sex_value, Unmarried, SexActive, UMNSA )
# missing for fp_ever_user, going to use fp_ever_used

Niger_ever <- as.data.frame(prop.table(wtd.table(x= Niger_clean$EverUse, weights = Niger_clean$FQweight)))   %>%
  mutate(SurveyName="PMA2022_NEP2_HQFQ_v1.0_30Nov2022.dta",
         Country="Niger",
         StartYear=2022)



Niger_never_UMNSA <- as.data.frame(prop.table(wtd.table(x= Niger_never$UMNSA, weights = Niger_never$FQweight)))  %>%
  mutate(SurveyName="PMA2022_NEP2_HQFQ_v1.0_30Nov2022.dta",
         Country="Niger",
         StartYear=2022)

Niger_former_unmet <- as.data.frame(prop.table(wtd.table(x= Niger_former$Unmet, weights = Niger_former$FQweight)))   %>%
  mutate(SurveyName="PMA2022_NEP2_HQFQ_v1.0_30Nov2022.dta",
         Country="Niger",
         StartYear=2022) %>% 
  filter(Var1==1) %>% select(-Var1)

Niger_current_mii <- as.data.frame(prop.table(wtd.table(x= Niger_current_mod$mii, weights = Niger_current_mod$FQweight)))   %>%
  mutate(SurveyName="PMA2022_NEP2_HQFQ_v1.0_30Nov2022.dta",
         Country="Niger",
         StartYear=2022) %>% 
  filter(Var1==1) %>% select(-Var1)  %>%
  mutate(Method="All")




Niger_current_mii_method_n <- as.data.frame(table(as.factor(Niger_current_mod$method))) %>% rename(n=Freq, Var2=Var1) %>% 
  mutate(Var2=as.numeric(as.character(Var2)))
Niger_current_mii_method <- as.data.frame(prop.table(wtd.table(x= Niger_current_mod$mii, y=as.factor(Niger_current_mod$method) , weights = Niger_current_mod$FQweight),2))  %>% 
  filter(Var1==1) %>% select(-Var1)  %>% 
  mutate(Var2=as.numeric(as.character(Var2)))  %>% left_join(method_labels, by="Var2") %>% left_join(Niger_current_mii_method_n, by="Var2") %>%
  filter(n>=25)  %>%
  mutate(SurveyName="PMA2022_NEP2_HQFQ_v1.0_30Nov2022.dta",
         Country="Niger",
         StartYear=2022)   %>%
  bind_rows(Niger_current_mii)

########################################################
setwd("C:/Users/KristinBietsch/Files/MICS Data")

Togo <- read_sav("Togo MICS6 SPSS Datasets/wm.sav")

library(foreign)
mics1 <-  read.spss("Togo MICS6 SPSS Datasets/wm.sav", to.data.frame= TRUE)
mics1 <- as.data.frame(attr(mics1, "variable.labels"))  %>%  tibble::rownames_to_column()

sel <- Togo %>% select( CP1, CP2, CP3)

Togo_clean <- Togo %>%  
  mutate(EverUse=case_when(CP2==1 ~ "Current User",
                           CP3==1 ~ "Former User",
                           CP3==2 ~ "Never Used")) %>%
  mutate(UMNSA=case_when( MSTATUS==1 ~ "Other",
                          SB1==0 ~ "UMNSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==1 & SB2N<=31 ~ "Other",
                          MSTATUS!=1 & SB1!=0 & SB2U==1 & SB2N>31 ~ "UMNSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==2 & SB2N<=4 ~ "Other",
                          MSTATUS!=1 & SB1!=0 & SB2U==2 & SB2N>4 ~ "UMNSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==3 & SB2N<=0 ~ "Other",
                          MSTATUS!=1 & SB1!=0 & SB2U==3 & SB2N>0 ~ "UMNSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==4 ~ "UMNSA",
                          TRUE ~ "Other"))

Togo_never <- Togo_clean %>% filter(EverUse== "Never Used")


sel <- Togo_clean %>% select( CP1, CP2, CP3, EverUse, MSTATUS, SB1, SB2U, SB2N, UMNSA)

table(Togo$CP3)
attr(Togo$CP3,"labels")

Togo_ever <- as.data.frame(prop.table(wtd.table(x= Togo_clean$EverUse, weights = Togo_clean$wmweight)))   %>%
  mutate(SurveyName="Togo MICS6 SPSS Datasets/wm.sav",
         Country="Togo",
         StartYear=2017)


Togo_never_UMNSA <- as.data.frame(prop.table(wtd.table(x= Togo_never$UMNSA, weights = Togo_never$wmweight)))       %>%
  mutate(SurveyName="Togo MICS6 SPSS Datasets/wm.sav",
         Country="Togo",
         StartYear=2017)

# Togo not doing unmetneed
############################################################################################

surveys <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));
OP_Countries <- c("Benin"  ,  "Burkina Faso" ,   "Cote d'Ivoire" ,   "Guinea",  "Mali" ,   "Mauritania",   "Niger",   "Senegal" , "Togo"  ) 


surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")

surveys_mini <- surveys   %>% filter(Country %in% OP_Countries)
surveys_mini_info <- surveys_mini %>% select(Country, StartYear, IRfile) %>% rename(SurveyName=IRfile) 

n_results <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c(  "Var1",   "Freq"  ,   "SurveyName", "Intention"   )) %>%
  mutate( Var1=as.numeric(Var1),  Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName),  Intention=as.character(Intention))
unmet_results <- setNames(data.frame(matrix(ncol = 4, nrow =0)), c(   "Freq"  ,   "SurveyName", "Country", "StartYear"   )) %>%
  mutate(  Freq=as.numeric(Freq),  SurveyName=as.character(SurveyName),  Country=as.character(Country), StartYear=as.character(StartYear) )


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

survey_keep <- n_results  %>% filter(!is.na(SurveyName)) %>% filter(Var1==1) %>% select(SurveyName) 

surveys_mini <- survey_keep  %>% left_join(surveys_mini_info, by="SurveyName")


#surveys_mini <- surveys_mini[1:3, ]
# # # # # # #
for (row in 1:nrow(surveys_mini)) {
  women_data <- surveys_mini[row, "SurveyName"]
  Country_info <- surveys_mini[row, "Country"]
  year <- surveys_mini[row, "StartYear"]
  
  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  
  women <- read_dta(women_data, col_select = any_of(c("caseid", "v312", "v313",  "v302a",  "v201", "v149", "v021", "v022", "v023", "v005", "v024", "v190", "v013", "v502", 
                                                      "v525", "v529",  "v602", "v604", "v626a", "v362", "v632", "v632a", "v359", "v360", "v327", "v317", "v008",
                                                      "v3a05", "v3a02", "v3a04", "vcal_1")))
  
  
  
  women_clean <- women %>% mutate(Status=case_when(v313==1 | v313==2 | v313==3 ~ "Current",
                                                   v302a==1 | v302a==2 | v302a==3 ~ "Former",
                                                   v302a==0 ~ "Never")) %>%
    mutate(sampleweights=v005/100000) %>%
    mutate(Unmet=case_when(v626a==1 | v626a==2 ~ 1,
                           v626a==0 | v626a==3 | v626a==4 | v626a==7 | v626a==8 | v626a==9  ~ 0)) %>%
    filter(!is.na(v005)) 
  
  former <- women_clean %>% filter(Status=="Former" )

  former_unmet <- as.data.frame(prop.table(wtd.table(x= former$Unmet, weights = former$sampleweights)))   %>%
    mutate(SurveyName=women_data,
           Country=Country_info,
           StartYear=year) %>%
    filter(Var1==1) %>% select(-Var1) %>% mutate(StartYear=as.character(StartYear))
  
  unmet_results <- bind_rows(unmet_results, former_unmet)
    
}



#########################################################################################
OP_ever <- bind_rows(Burkina_ever, CDI_ever, Niger_ever, Togo_ever)
OP_never_umnsa <- bind_rows(Burkina_never_UMNSA, CDI_never_UMNSA, Niger_never_UMNSA, Togo_never_UMNSA)
former_unmet <- bind_rows(Burkina_former_unmet, CDI_former_unmet, Niger_former_unmet) %>% mutate(StartYear=as.character(StartYear)) %>%
  bind_rows(unmet_results) %>% 
  group_by(Country) %>%
  mutate(max=max(StartYear)) %>% filter(max==StartYear) %>% filter(Country!="Togo")

OP_mii <- bind_rows(Burkina_current_mii_method, CDI_current_mii_method, Niger_current_mii_method)

setwd("C:/Users/KristinBietsch/Avenir Health Dropbox/Kristin kbietsch@avenirhealth.org/Avenir Track20/Data Requests/Ouaga Partnership/September 2023 Donors Meeting/Never Former Current Data")
write.csv(OP_ever, "OPPMAMICS083123.csv", row.names=F, na="")
write.csv(OP_never_umnsa, "Never_UMNSA_OPPMAMICS083123.csv", row.names=F, na="")
write.csv(former_unmet, "Former_Unmet_DHSOPPMAMICS083123.csv", row.names=F, na="")
write.csv(OP_mii, "Current_MII_OPPMAMICS083123.csv", row.names=F, na="")
