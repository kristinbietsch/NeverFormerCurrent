# Malawi 2019 MICS


library(dplyr)
library(haven)
library(survey)
require(xlsx)
library(tibble)
library(questionr)
library(reshape2)
library(DHS.rates)
library(forcats)
library(ggplot2)

options(scipen = 999)

setwd("C:/Users/KristinBietsch/Files/MICS Data")


# Malawi 2019

Country <- "Malawi" 
StartYear <- 2019
ISOCode <- 454

women_data <- paste(Country, StartYear, sep=" ")

mics <- read_sav("Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/wm.sav")
sel <- select(mics, SB1, SB2U, SB2N, wmweight) %>% filter(wmweight!=0)

mics_br <- read_sav("Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/bh.sav")

library(foreign)
mics1 <-  read.spss("Malawi MICS6 SPSS/Malawi MICS6 SPSS Datasets/wm.sav", to.data.frame= TRUE)
mics1 <- as.data.frame(attr(mics1, "variable.labels")) %>% rownames_to_column()

# Contraceptive use

attr(mics$CP4A,"labels")
attr(mics$CP4B,"labels")
attr(mics$CP4C,"labels")
attr(mics$CP4D,"labels")
attr(mics$CP4E,"labels")
attr(mics$CP4F,"labels")
attr(mics$CP4G,"labels")
attr(mics$CP4H,"labels")
attr(mics$CP4I,"labels")
attr(mics$CP4J,"labels")
attr(mics$CP4K,"labels")
attr(mics$CP4L,"labels")
attr(mics$CP4M,"labels")
attr(mics$CP4X,"labels")

attr(mics$CP3,"labels")
attr(mics$UN7,"labels")
attr(mics$UN8N,"labels")
attr(mics$UN8U,"labels")

sel <- select(mics, UN7, UN8N, UN8U)

women_clean <- mics %>% 
  filter(MSTATUS!=9) %>%
  mutate(method=case_when(CP4A=="A" ~ "F_Ster", 
                                         CP4B=="B" ~ "M_Ster",
                                         CP4C=="C" ~ "IUD", 
                                         CP4D=="D" ~ "Injectable",
                                         CP4E=="E" ~ "Implant",
                                         CP4F=="F" ~ "Pill",
                                         CP4G=="G" ~ "M_Condom",
                                         CP4H=="H" ~ "F_Condom",
                                         CP4I=="I" ~ "Diaphragm",
                                         CP4J=="J" ~ "Foam/Jelly",
                                         CP4K=="K" ~ "LAM",
                                         CP4L=="L" ~ "Periodic Abstinence/Rhythm",
                                         CP4M=="M" ~ "Withdrawal",
                                         CP4X=="X" ~ "Other",
                                         TRUE ~ "None"),
                        cpr=case_when(method!="None" ~ 1, method=="None" ~ 0))  %>% 
  mutate(pill=case_when(method=="Pill" ~ 1, method!="Pill" ~ 0),
                        condom=case_when(method=="M_Condom" | method=="F_Condom" ~ 1, method!="M_Condom" & method!="F_Condom" ~ 0),
                        fster=case_when(method=="F_Ster" ~ 1, method!="F_Ster" ~ 0),
                        implant=case_when(method=="Implant" ~ 1, method!="Implant" ~ 0),
                        iud=case_when(method=="IUD" ~ 1, method!="IUD" ~ 0),
                        mster=case_when(method=="M_Ster" ~ 1, method!="M_Ster" ~ 0),
                        perabs=case_when(method=="Periodic Abstinence/Rhythm" ~ 1, method!="Periodic Abstinence/Rhythm" ~ 0),
                        withdrawal=case_when(method=="Withdrawal" ~ 1, method!="Withdrawal" ~ 0),
                        diaphram=case_when(method=="Diaphragm" ~ 1, method!="Diaphragm" ~ 0),
                        injectable=case_when(method=="Injectable" ~ 1, method!="Injectable" ~ 0),
                        other_modern=case_when(method=="Foam/Jelly" |  method=="LAM"  ~ 1, method!="Foam/Jelly" & method!="LAM" ~ 0),
                        other_trad=case_when(method=="Other" ~ 1, method!="Other" ~ 0)) %>%
  mutate(Status=case_when(cpr==1 ~ "Current",
                          CP3==1 ~ "Former",
                          CP3==2 ~ "Never")) %>%
  filter(wmweight>0) %>%
  mutate(sampleweights=wmweight) %>%
  mutate(group=case_when( MSTATUS==1 ~ "married",
                          SB1==0 ~ "never sex",
                          MSTATUS!=1 & SB1!=0 & SB2U==1 & SB2N<=31 ~ "UMSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==1 & SB2N>31 ~ "UMNSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==2 & SB2N<=4 ~ "UMSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==2 & SB2N>4 ~ "UMNSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==3 & SB2N<=0 ~ "UMSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==3 & SB2N>0 ~ "UMNSA",
                          MSTATUS!=1 & SB1!=0 & SB2U==4 ~ "UMNSA",
                          TRUE ~ "Missing"),
         mar_umsa=case_when(group=="married" | group=="UMSA" ~ 1,
                            group=="never sex" | group=="UMNSA" ~ 0),
         age_group=WAGE)  %>%
  mutate(Mar_Stat = case_when(group=="married" ~ "Married",
                              group=="UMSA" ~ "UMSA",
                              group=="UMNSA" ~ "UMNSA",
                              TRUE~ "MISSING")) %>%
  mutate(Age=case_when(WAGE==1 | WAGE==2 ~ "15-24",
                       WAGE==3 | WAGE==4 | WAGE==5 ~ "25-39",
                       WAGE==6 | WAGE==7 ~ "40-49")) %>%
  mutate(GroupMarAge=paste(Mar_Stat, Age, sep="_")) %>%
  mutate(Group=case_when(MSTATUS==1 &  Status=="Current" ~ "Married,  Current User",
                         MSTATUS==3 &  Status=="Current"  ~ "Never Married,  Current User",
                         MSTATUS==2 &  Status=="Current"  ~ "Formerly Married,  Current User",
                         UN12B=="B" | UN12C=="C" | UN12D=="D" | UN12E=="E" | UN12E=="H" ~ "Infecund, Menopausal",
                         group  == "UMNSA"  ~ "Not Married, Not Sexually Active",
                         group  == "never sex" ~ "Not Married, Not Sexually Active",
                         group  == "UMSA" ~ "Not Married, Sexually Active",
                         MSTATUS==1 & UN7==8 ~ "Married, Does Not Want Child within 2 Years",
                         MSTATUS==1 &  UN7==1 & UN8N==93 ~ "Married, Wants Child within 2 Years", 
                         MSTATUS==1 &  UN7==1 & UN8U==2 & UN8N<=2~ "Married, Wants Child within 2 Years",
                         MSTATUS==1 &  UN7==1 & UN8U==1 & UN8N<=24~ "Married, Wants Child within 2 Years",
                         MSTATUS==1 &  UN7==2  ~ "Married, Does Not Want Child within 2 Years",
                         MSTATUS==1 &  UN7==1 & UN8U==2 & UN8N>2  ~ "Married, Does Not Want Child within 2 Years" ,
                         MSTATUS==1 &  UN7==1 & UN8U==1 & UN8N>24  ~ "Married, Does Not Want Child within 2 Years" ,
                         MSTATUS==1 &  UN5==1 & UN8N==93 ~ "Married, Wants Child within 2 Years", 
                         MSTATUS==1 &  UN5==1 & UN8U==2 & UN8N<=2~ "Married, Wants Child within 2 Years",
                         MSTATUS==1 &  UN5==1 & UN8U==1 & UN8N<=24~ "Married, Wants Child within 2 Years",
                         MSTATUS==1 &  UN5==2  ~ "Married, Does Not Want Child within 2 Years",
                         MSTATUS==1 &  UN5==1 & UN8U==2 & UN8N>2  ~ "Married, Does Not Want Child within 2 Years" ,
                         MSTATUS==1 &  UN5==1 & UN8U==1 & UN8N>24  ~ "Married, Does Not Want Child within 2 Years" ,
                         MSTATUS==1 & UN5==8 ~ "Married, Does Not Want Child within 2 Years",
                         MSTATUS==1 &  UN5==1 & UN8N==95 ~ "Married, Does Not Want Child within 2 Years",
                         MSTATUS==1 &  UN5==1 & UN8N==96 ~ "Married, Does Not Want Child within 2 Years",
                         MSTATUS==1 &  UN7==1 & UN8N==95 ~ "Married, Does Not Want Child within 2 Years",
                         MSTATUS==1 &  UN7==1 & UN8N==96 ~ "Married, Does Not Want Child within 2 Years",
                         MSTATUS==1 &  UN7==1 & UN8N==99 ~ "Married, Wants Child within 2 Years",
                         MSTATUS==1 &  UN7==1 & UN8N==98 ~ "Married, Does Not Want Child within 2 Years",
                         UN8N==94 | UN7==3 ~ "Infecund, Menopausal")) %>%
  filter(group!="Missing")

sel <- women_clean %>% select(Group, group, Mar_Stat, Status, UN12B, UN12C, UN12D, UN12E, UN12F, UN5, UN7,  UN8N, UN8U)



###############################################################

never_users <- women_clean %>% filter(Status=="Never") 


former_users <- women_clean %>% filter(Status=="Former")  


current_users <- women_clean %>% filter(Status=="Current")

#################################################################
####################################################################################################################################################################
use_status<- as.data.frame(prop.table(wtd.table(women_clean$Status, weights = women_clean$sampleweights))) %>%
  mutate(Var1=case_when(Var1== "Never" ~ "Never Used", 
                        Var1== "Former" ~ "Former User", 
                        Var1== "Current" ~ "Current User")) %>%
  mutate(Var1=fct_relevel(Var1, "Current User",  "Former User" ,  "Never Used")) %>%
  mutate(SurveyName=women_data, Country=Country, StartYear=StartYear)  %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ ""))

use_age_status<- as.data.frame(prop.table(wtd.table(as.factor(women_clean$Age), as.factor(women_clean$Status), weights = women_clean$sampleweights)) ) %>%
  mutate(Var2=case_when(Var2== "Never" ~ "Never Used", 
                        Var2== "Former" ~ "Former User", 
                        Var2== "Current" ~ "Current User")) %>%
  mutate(Var2=fct_relevel(Var2,  "Never Used", "Former User" , "Current User"))  %>%
  mutate(SurveyName=women_data, Country=Country, StartYear=StartYear) %>%
  mutate(Var2 = fct_relevel(Var2, c( "Never Used", "Former User", "Current User")))  %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ "")) 

use_mar_status<- as.data.frame(prop.table(wtd.table(as.factor(women_clean$Mar_Stat), as.factor(women_clean$Status), weights = women_clean$sampleweights)) )   %>%
  mutate(Var2=case_when(Var2== "Never" ~ "Never Used", 
                        Var2== "Former" ~ "Former User", 
                        Var2== "Current" ~ "Current User")) %>%
  mutate(Var2=fct_relevel(Var2,  "Never Used", "Former User" , "Current User"))  %>%
  mutate(SurveyName=women_data, Country=Country, StartYear=StartYear)

####################################################################################################################################################################


never_group_n <- as.data.frame(table(as.factor(never_users$Group))) %>% rename(N=Freq)

never_group <- as.data.frame(prop.table(wtd.table(as.factor(never_users$Group), weights = never_users$sampleweights)))   %>%
  left_join(never_group_n, by="Var1") %>% rename(Group=Var1)  %>%
  mutate(SurveyName=women_data, Country=Country, StartYear=StartYear)   %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ "")) 


####################################################################################################################################################################

former_group_n <- as.data.frame(table(as.factor(former_users$Group))) %>% rename(N=Freq)

former_group <- as.data.frame(prop.table(wtd.table(as.factor(former_users$Group), weights = former_users$sampleweights)))   %>%
  left_join(former_group_n, by="Var1") %>% rename(Group=Var1)  %>%
  mutate(SurveyName=women_data, Country=Country, StartYear=StartYear)  %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ "")) 

########################################################################################################################


current_group <- as.data.frame(prop.table(wtd.table(as.factor(current_users$Group), weights = current_users$sampleweights))) %>% 
  rename(Group=Var1)  %>%
  mutate(SurveyName=women_data, Country=Country, StartYear=StartYear) %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ ""))  %>%
  mutate(Group=case_when(
    Group=="Married,  Current User" ~ "Married" ,
    Group=="Never Married,  Current User" ~ "Never Married",
    Group== "Formerly Married,  Current User" ~ "Formerly Married"))

#################################################################################################

current_method_mix <- as.data.frame(prop.table(wtd.table(as.factor(current_users$method),  weights = current_users$sampleweights))) %>% 
  rename(Method=Var1)  %>% select(Method,  Freq)   %>%
  mutate(SurveyName=women_data, Country=Country, StartYear=StartYear)
#################################################################################################


ggplot(use_status, aes(x=as.factor(StartYear), y=Freq*100,  fill=Var1, label=label)) +
  geom_bar(stat="identity") + 
  geom_text(size = 6, position = position_stack(vjust = 0.5))+
  labs(x="", y="Percent of WRA Population", shape="", fill="", title=Country) +
  coord_flip() +
  scale_fill_manual(values=c("Current User"= "#3892C6", "Former User"= "#8DC645", "Never Used"= "#954F72"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_bw() +
  theme(text=element_text(size=20, color="black"),
        axis.text =element_text(size=20, color="black"),
        legend.text =element_text(size=23),
        legend.position = "bottom")
ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/UserDistribution", women_data,".png"), width=8, height=8, units="in")


ggplot(use_age_status, aes(x=Var2, y=Freq*100, fill=Var1, label=label)) +
  geom_bar(stat="identity") +
  geom_text(size = 6, position = position_stack(vjust = 0.5))+
  labs(x="", y="Percent of WRA Population", shape="", fill="", title=paste(Country, ": Age Distribution, ", StartYear, sep="")) +
  scale_fill_manual(values=c("15-24" = "#0B9444",  "25-39"= "#FCD116", "40-49"= "#3892C6")) +
  theme_bw() +
  theme(text=element_text(size=20, color="black"),
        axis.text =element_text(size=20, color="black"),
        legend.text =element_text(size=23),
        legend.position = "bottom")

ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/UserDistribution_Age_Recent", women_data,".png"), width=8, height=8, units="in")

ggplot(never_group, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=label)) +
  geom_bar(stat = "identity") +
  geom_text(size = 6, position = position_stack(vjust = 0.5))+
  labs(x="", y="", fill="", title=paste(Country, ": Never Users Population Distribution", sep="")) +
  coord_flip() +
  scale_fill_manual(values=c( "Not Married, Sexually Active"= "#0B9444",  "Not Married, Not Sexually Active" = "#8DC645",  "Infecund, Menopausal"= "#954F72",  "Married, Wants Child within 2 Years"= "#1D4E92",   "Married, Does Not Want Child within 2 Years"= "#3892C6")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE, rev=TRUE)) +
  theme_bw() +
  theme(text=element_text(size=20, color="black"),
        axis.text =element_text(size=20, color="black"),
        legend.text =element_text(size=15),
        legend.position = "bottom")

ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/NeverUserDistribution_AllSurveys", women_data,".png"), width=12, height=8, units="in")

ggplot(former_group, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=label)) +
  geom_bar(stat = "identity") +
  geom_text(size = 6, position = position_stack(vjust = 0.5))+
  labs(x="", y="", fill="", title=paste(Country, ": Former Users Population Distribution", sep="")) +
  coord_flip() +
  scale_fill_manual(values=c( "Not Married, Sexually Active"= "#0B9444",  "Not Married, Not Sexually Active" = "#8DC645",  "Infecund, Menopausal"= "#954F72",  "Married, Wants Child within 2 Years"= "#1D4E92",   "Married, Does Not Want Child within 2 Years"= "#3892C6")) +
  guides(fill = guide_legend(nrow=3,byrow=TRUE,  reverse=TRUE)) +
  theme_bw()  +
  theme(text=element_text(size=20, color="black"),
        axis.text =element_text(size=20, color="black"),
        legend.text =element_text(size=15),
        legend.position = "bottom")

ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/FormerUserDistribution_AllSurveys", women_data,".png"), width=12, height=8, units="in")

ggplot(current_group, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=label)) +
  geom_bar(stat = "identity") +
  geom_text(size = 6, position = position_stack(vjust = 0.5))+
  labs(x="", y="", fill="", title=paste(Country, ": Current Users Population Distribution", sep="")) +
  coord_flip() +
  scale_fill_manual(values=c( "Never Married"= "#8DC645",  "Formerly Married"= "#FCD116",  "Married"= "#3892C6")) +
  theme_bw() +
  theme(text=element_text(size=20, color="black"),
        axis.text =element_text(size=20, color="black"),
        legend.text =element_text(size=20),
        legend.position = "bottom")

ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/CurrentUserDistribution_AllSurveys", women_data,".png"), width=12, height=8, units="in")

ggplot(current_method_mix, aes(x=as.factor(StartYear), y=Freq * 100, fill=Method)) +
  geom_bar(stat = "identity", color="white") +
  labs(x="", y="", fill="", title=paste(Country, ": Current Users Method Mix", sep="")) +
  theme_bw()  +
  theme(text=element_text(size=20, color="black"),
        axis.text =element_text(size=20, color="black"),
        legend.text =element_text(size=15))

ggsave(paste("C:/Users/KristinBietsch/files/Track20/Ever Use of Contraception/Former Never Current Graphics 050423/CurrentUserMethodMix_AllSurveys", women_data,".png"), width=12, height=8, units="in")
