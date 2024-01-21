# Kristin Bietsch, PhD
# kristinbietsch@avenirhealth.org
# 06/01/23



library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(jsonlite) 
library(data.table)
library(ggrepel)
library(stringr)
library(plotly)
####################################################

cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE


use_status_clean <- read.csv("data/use_status_clean 011624.csv") %>% arrange(Country) %>%
  mutate(Var1=case_when(Var1=="Current User" ~ "Utilisatrice actuelle",
                        Var1=="Former User" ~ "Ancienne utilisatrice",
                        Var1=="Never Used" ~ "N’a jamais utilisé")) %>%
  mutate(Var1 = fct_relevel(Var1, c( "N’a jamais utilisé", "Ancienne utilisatrice", "Utilisatrice actuelle" )))
use_age_status_clean <- read.csv("data/use_age_status_clean, 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>%
  mutate(Var2=case_when(Var2=="Current User" ~ "Utilisatrice actuelle",
                        Var2=="Former User" ~ "Ancienne utilisatrice",
                        Var2=="Never Used" ~ "N’a jamais utilisé")) %>%
  mutate(Var2 = fct_relevel(Var2, c( "N’a jamais utilisé", "Ancienne utilisatrice", "Utilisatrice actuelle" )))
use_mar_status_clean <- read.csv("data/use_mar_status_clean 011624.csv")
never_group_clean <- read.csv("data/never_group_clean 011624.csv") %>%
  mutate(Group=case_when(Group=="Infecund, Menopausal" ~ "Inféconde, ménopausiques",
                         Group=="Married, Does Not Want Child within 2 Years" ~ "Mariée, ne veut pas d’enfants dans les 2 ans",
                         Group=="Married, Wants Child within 2 Years" ~ "Mariée, veut un enfant dans les 2 ans",
                         Group=="Not Married, Not Sexually Active" ~ "Pas mariée, pas active sexuellement",
                         Group=="Not Married, Sexually Active" ~ "Pas mariée, sexuellement actives"))
never_intention_clean <- read.csv("data/never_intention_clean 011624.csv") %>% group_by(Country) %>%  mutate(recent=max(StartYear)) %>%  filter(recent==StartYear)  %>% ungroup() %>% filter(N>=50) %>%
  mutate(Group = case_when(Group== "All Never Users" ~ "Toutes les\nutilisatrices Jamais", 
                           Group=="Infecund, Menopausal" ~ "Inféconde,\nménopausiques",
                           Group=="Married, Does Not Want Child within 2 Years" ~ "Mariée, ne veut pas\nd’enfants dans les 2 ans",
                           Group=="Married, Wants Child within 2 Years" ~ "Mariée, veut un\nenfant dans les 2 ans",
                           Group=="Not Married, Not Sexually Active" ~ "Pas mariée, pas\nactive sexuellement",
                           Group=="Not Married, Sexually Active" ~ "Pas mariée,\nsexuellement actives")) %>%
  mutate(Group = fct_relevel(Group, c(  "Toutes les\nutilisatrices Jamais" , "Mariée, ne veut pas\nd’enfants dans les 2 ans", "Mariée, veut un\nenfant dans les 2 ans", "Pas mariée,\nsexuellement actives", "Pas mariée, pas\nactive sexuellement", "Inféconde,\nménopausiques"))) %>%
  mutate(Intention=case_when(Intention=="Does Not Intend" ~ "N’a pas l’intention",
                             Intention=="Unsure About Use" ~ "Incertaine quant à l’utilisation",
                             Intention=="Use Later" ~ "Utiliser plus tard" )) %>%
  mutate(Intention = fct_relevel(Intention, c( "N’a pas l’intention" , "Incertaine quant à l’utilisation" , "Utiliser plus tard")))

never_has_intention_clean <- read.csv("data/never_has_intention_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>%
  mutate(Group=case_when(Group=="Infecund, Menopausal" ~ "Inféconde, ménopausiques",
                         Group=="Married, Does Not Want Child within 2 Years" ~ "Mariée, ne veut pas d’enfants dans les 2 ans",
                         Group=="Married, Wants Child within 2 Years" ~ "Mariée, veut un enfant dans les 2 ans",
                         Group=="Not Married, Not Sexually Active" ~ "Pas mariée, pas active sexuellement",
                         Group=="Not Married, Sexually Active" ~ "Pas mariée, sexuellement actives"))
former_group_clean <- read.csv("data/former_group_clean 011624.csv") %>%
  mutate(Group=case_when(Group=="Infecund, Menopausal" ~ "Inféconde, ménopausiques",
                         Group=="Married, Does Not Want Child within 2 Years" ~ "Mariée, ne veut pas d’enfants dans les 2 ans",
                         Group=="Married, Wants Child within 2 Years" ~ "Mariée, veut un enfant dans les 2 ans",
                         Group=="Not Married, Not Sexually Active" ~ "Pas mariée, pas active sexuellement",
                         Group=="Not Married, Sexually Active" ~ "Pas mariée, sexuellement actives"))
former_intention_clean <- read.csv( "data/former_intention_clean 011624.csv") %>% mutate(Intention=str_to_title(Intention)) %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup()  %>%
  mutate(Group = case_when(Group== "All Never Users" ~ "Toutes les\nanciennes utilisatrices", 
                           Group=="Infecund, Menopausal" ~ "Inféconde,\nménopausiques",
                           Group=="Married, Does Not Want Child within 2 Years" ~ "Mariée, ne veut pas\nd’enfants dans les 2 ans",
                           Group=="Married, Wants Child within 2 Years" ~ "Mariée, veut un\nenfant dans les 2 ans",
                           Group=="Not Married, Not Sexually Active" ~ "Pas mariée, pas\nactive sexuellement",
                           Group=="Not Married, Sexually Active" ~ "Pas mariée,\nsexuellement actives")) %>%
  mutate(Group = fct_relevel(Group, c(  "Toutes les\nanciennes utilisatrices" , "Mariée, ne veut pas\nd’enfants dans les 2 ans", "Mariée, veut un\nenfant dans les 2 ans", "Pas mariée,\nsexuellement actives", "Pas mariée, pas\nactive sexuellement", "Inféconde,\nménopausiques"))) %>%
  mutate(Intention=case_when(Intention=="Does Not Intend" ~ "N’a pas l’intention",
                             Intention=="Unsure About Use" ~ "Incertaine quant à l’utilisation",
                             Intention=="Use Later" ~ "Utiliser plus tard" )) %>%
  mutate(Intention = fct_relevel(Intention, c( "N’a pas l’intention" , "Incertaine quant à l’utilisation" , "Utiliser plus tard")))
former_has_intention_clean <- read.csv( "data/former_has_intention_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>%
  mutate(Group=case_when(Group=="Infecund, Menopausal" ~ "Inféconde, ménopausiques",
                         Group=="Married, Does Not Want Child within 2 Years" ~ "Mariée, ne veut pas d’enfants dans les 2 ans",
                         Group=="Married, Wants Child within 2 Years" ~ "Mariée, veut un enfant dans les 2 ans",
                         Group=="Not Married, Not Sexually Active" ~ "Pas mariée, pas active sexuellement",
                         Group=="Not Married, Sexually Active" ~ "Pas mariée, sexuellement actives"))
former_why_discontinue_clean <- read.csv( "data/former_why_discontinue_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>%
  mutate(Var1 = case_when(Var1== "All Former Users" ~ "Toutes les anciennes\nutilisatrices", 
                          Var1=="Infecund, Menopausal" ~ "Inféconde,\nménopausiques",
                          Var1=="Married, Does Not Want Child within 2 Years" ~ "Mariée, ne veut pas\nd’enfants dans\nles 2 ans",
                          Var1=="Married, Wants Child within 2 Years" ~ "Mariée, veut un\nenfant dans les 2 ans",
                          Var1=="Not Married, Not Sexually Active" ~ "Pas mariée, pas\nactive sexuellement",
                          Var1=="Not Married, Sexually Active" ~ "Pas mariée,\nsexuellement actives")) %>%
  mutate(Var1 = fct_relevel(Var1, c(  "Toutes les anciennes\nutilisatrices" , "Mariée, ne veut pas\nd’enfants dans\nles 2 ans", "Mariée, veut un\nenfant dans les 2 ans", "Pas mariée,\nsexuellement actives", "Pas mariée, pas\nactive sexuellement", "Inféconde,\nménopausiques")))  %>%
  mutate(WhyDisc=case_when(WhyDisc=="Became Pregnant" ~ "Est tombée enceinte",
                           WhyDisc=="Wanted To Become Pregnant" ~ "Voulait tomber enceinte",
                           WhyDisc=="Husband Disapproved" ~ "Le mari a refusé",
                           WhyDisc=="Side Effects" ~ "Effets secondaires",
                           WhyDisc=="Access, Availability" ~ "Accès, Disponibilité",
                           WhyDisc=="Wanted More Effective Method" ~ "Voulais une méthode plus efficace",
                           WhyDisc=="Inconvenient To Use" ~ "Peu pratique à utiliser",
                           WhyDisc=="Infrequent Sex, Husband Away" ~ "Sexe peu fréquent, mari absent",
                           WhyDisc=="Cost" ~ "Coût",
                           WhyDisc=="Fatalistic" ~ "Fataliste",
                           WhyDisc=="Difficult Pregnancy, Menopause" ~ "Grossesse difficile, Ménopause",
                           WhyDisc=="Other" ~ "Autre",
                           WhyDisc=="Marital Dissolution" ~ "Dissolution du mariage",
                           WhyDisc=="Don't Know" ~ "Ne sais pas",
                           WhyDisc== "Creating Menstrual Problems"  ~ "Créer des problèmes menstruels",
                           WhyDisc=="Did Not Like Method" ~ "N'a pas aimé la méthode",
                           WhyDisc=="Don't  Mind"  ~ "Ne vous inquiétez pas",
                           WhyDisc== "Gained Weight" ~ "A pris du poids",
                           WhyDisc=="Health Concerns"  ~ "Problèmes de santé",
                           WhyDisc== "Husband Away" ~ "Mari absent",
                           WhyDisc=="Missing" ~ "Manquant",
                           WhyDisc=="Ramadan" ~ "Ramadan",
                           WhyDisc== "Lack Of Privacy To Use"   ~ "Manque de confidentialité à utiliser",
                           WhyDisc== "Lack Of Sexual Satisfaction"   ~ "Manque de satisfaction sexuelle",
                           WhyDisc== "Iud Expelled"  ~ "DIU expulsé",
                           WhyDisc== "Side Effects /Health Concerns"  ~ "Effets secondaires/Problèmes de santé"))


former_time_use_group_clean <- read.csv( "data/former_time_use_group_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>%
  mutate(Time_Since_Use=case_when(Time_Since_Use=="Under a Year" ~ "Moins d’un an",
                                  Time_Since_Use=="1 Year" ~  "1 an",
                                  Time_Since_Use=="2 Year" ~ "2 ans",
                                  Time_Since_Use=="3 Year" ~ "3 ans",
                                  Time_Since_Use=="4 Year" ~ "4 ans",
                                  Time_Since_Use=="5+ Years" ~ "5 ans et plus")) %>%
  mutate(Time_Since_Use = fct_relevel(Time_Since_Use, c("Moins d’un an", "1 an", "2 ans", "3 ans", "4 ans", "5 ans et plus" )))
mii_clean <- read.csv( "data/mii_clean 011624.csv")  %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% 
  filter(recent==StartYear) %>% select(-recent)  %>% ungroup() %>% group_by(Country, Method) %>% mutate(Max=max(Freq)) %>% ungroup() %>%
  mutate(Method=case_when(Method== "Female Sterilization"    ~ "Stérilisation féminine",
                          Method== "Implant"                  ~ "Implant" ,
                          Method== "Injections"               ~ "Injections" ,
                          Method== "IUD"                      ~ "DIU",
                          Method== "Pill"                     ~"Pilule",
                          Method== "Total"                     ~"Total")) %>%
  mutate(Var=case_when(Var=="Method Information Index"     ~ "Index des informations sur la méthode",       
                       Var=="Told About Other Methods"     ~ "Informé sur les autres méthodes",     
                       Var=="Told About Side Effects"           ~ "Informé sur les effets secondaires",
                       Var=="Told What to Do About Side Effects" ~ "Informé sur ce qu'il faut faire en cas d'effets secondaires"))
current_group_clean <- read.csv("data/current_group_clean 011624.csv") %>%
  mutate(Group=case_when(Group=="Formerly Married" ~ "Anciennement mariée",
                         Group=="Married" ~ "Mariée",
                         Group=="Never Married" ~ "Jamais mariée")) %>%
  mutate(Group = fct_relevel(Group, c( "Jamais mariée", "Mariée",  "Anciennement mariée")))
current_methodmix_clean <- read.csv( "data/current_methodmix_clean 011624.csv") %>%
  mutate(Method=case_when(Method== "Basal Body Temperature"   ~ "Température corporelle basale" ,
                          Method== "Billings Method"          ~ "Méthode de facturation" ,
                          Method== "Chinese (Monthly) Pill"   ~ "Pilule (mensuelle) chinoise",
                          Method== "Diaphragm"                ~ "Diaphragme",
                          Method== "Diaphragm/Foam/Jelly"     ~ "Membrane/Mousse/Gelée",
                          Method== "Emergency Contraception"  ~ "Contraception d'urgence" ,
                          Method== "Female Condom"            ~ "Préservatif féminin",
                          Method== "Female Sterilization"    ~ "Stérilisation féminine",
                          Method== "Foam Or Jelly"            ~ "Mousse ou gelée",
                          Method== "Implant"                  ~ "Implant" ,
                          Method== "Injection 1 Months"       ~ "Injection 1 Mois" ,
                          Method== "Injections"               ~ "Injections" ,
                          Method== "Injections 2 Month"       ~ "Injections 2 Mois",
                          Method== "IUD"                      ~ "DIU",
                          Method== "Lactational Amenorrhea"   ~ "Aménorrhée de lactation",
                          Method== "Male Condom"             ~ "Préservatif masculin",
                          Method== "Male Sterilization"       ~ "Stérilisation masculine",
                          Method== "Other"                    ~ "Autre" ,
                          Method== "Other Modern Method"      ~ "Autre méthode moderne",
                          Method== "Other Traditional Method" ~ "Autre méthode traditionnelle" ,
                          Method== "Periodic Abstinence"      ~ "Abstinence périodique" ,
                          Method== "Pill"                     ~"Pilule",
                          Method== "Prolonged Breastfeeding"  ~ "Allaitement prolongé",
                          Method== "Standard Days Method"    ~ "Méthode des jours standards",
                          Method== "Withdrawal"   ~ "Retrait" ))
current_source_clean <- read.csv("data/current_source_clean 011624.csv") %>%
  mutate(Source=case_when(Source=="Don't Know"  ~  "Ne sais pas"  ,                  
                          Source=="Government Clinic/Pharmacy" ~  "Clinique gouvernementale/Pharmacie" ,     
                          Source=="Government Home/Community Delivery" ~ "Livraison gouvernementale à domicile/communautaire",
                          Source=="Missing"   ~       "Manquant"   ,            
                          Source=="NGO"    ~    "ONG" ,                       
                          Source=="Other" ~  "Autre"  ,                        
                          Source=="Pharmacy"   ~   "Pharmacie",                     
                          Source=="Private Clinic/Delivery" ~ "Clinique privée/Accouchement"   ,    
                          Source=="Shop, Church, Friend" ~ "Magasin, église, ami"))
current_timesince_clean <- read.csv( "data/current_timesince_clean 011624.csv")  %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>%
  mutate(Var1=case_when(Var1=="Under a Year" ~ "Moins d’un an",
                        Var1=="1 Year" ~  "1 an",
                        Var1=="2 Year" ~ "2 ans",
                        Var1=="3 Year" ~ "3 ans",
                        Var1=="4 Year" ~ "4 ans",
                        Var1=="5+ Years" ~ "5 ans et plus")) %>%
  mutate(Var1 = fct_relevel(Var1, c("Moins d’un an", "1 an", "2 ans", "3 ans", "4 ans", "5 ans et plus" )))
empowerment_clean <- read.csv( "data/empowerment_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup()

############################################


###########################################################################################################################################

ui <-
  navbarPage("Track20 Utilisation de contraceptifs actuelle, ancienne ou jamais", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cerulean"),
             tags$head(
               tags$style(HTML("

      .navbar .navbar-nav {font-size: 20px; 
      } 
            .navbar-header { width:100% }
                   .navbar-brand { width: 100% }
       .navbar-default .navbar-brand { font-size: 30px; } 
      .selectize-input {
        height: 60px;
        width: 800px;
        font-size: 16pt;
        padding-top: 5px;
      }
      

    "))
             ),
             tabPanel("Répartition des femmes en âge de procréer",
                      fluidPage(
                        mainPanel(
                          fluidRow(h4("En général, notre évaluation des programmes de planification familiale se concentre sur la prévalence de la contraception et les utilisatrices actuelles de contraceptifs : quelle proportion de la population utilise des contraceptifs, les méthodes qu’elles utilisent, leurs caractéristiques démographiques, etc.")),
                          fluidRow(h4("Cependant, ces données ne reflètent pas entièrement les succès et les limites des programmes de planning familial. L’étude des expériences et des intentions des femmes qui n’utilisent pas actuellement la contraception (celles qui l’ont utilisée dans le passé et celles qui ne l’ont jamais utilisée) peut aider à mettre en évidence les opportunités et les obstacles pour les programmes de planning familial.")),
                          fluidRow(h4("Les femmes commencent et arrêtent la contraception pour un large éventail de raisons. Cela signifie qu'à un moment donné, certaines femmes qui ont été atteintes avec succès et dont les besoins ont été satisfaits par le programme dans le passé, ces femmes peuvent ne pas utiliser actuellement de méthode. Cela peut être particulièrement vrai dans les pays où la contraception est largement utilisée pour espacer les naissances plutôt que pour les limiter. Ces anciennes utilisatrices, associées aux utilisatrices actuelles, permettent de dresser un tableau plus complet de la portée du programme ainsi que de la disponibilité et de l’acceptabilité de la contraception.")),
                          fluidRow(h4("Au fur et à mesure que les programmes de planning familial se développent, nous nous attendons à ce que le nombre d’utilisatrices actuelles et anciennes augmente. Chacun de ces trois groupes a une importance programmatique, mais les questions que nous posons à chacun sont différentes.")),
                          fluidRow(h4("Le nouvel outil de Track20 s'appelle « l'Outil des Utilisatrices Actuelle, Ancienne, Jamais » et il est conçu pour aider les utilisatrices à comprendre comment les femmes sont réparties entre ces trois groupes, comment cela a changé au fil du temps et ce que chaque groupe dit des succès et des limites du programme de planification familiale dans un pays donné.")),
                          fluidRow(h4(strong("Pour utiliser cet outil, commencer par choisir un pays. Déplacez-vous entre les quatre onglets situés en haut de la page, en choisissant différentes questions pour explorer les données. Si vous souhaitez télécharger tous les graphiques, cliquez sur le bouton de téléchargement ci-dessus."))),
                          fluidRow(h3("Choisir un pays"),
                            selectInput("Country", 
                                      label = "",
                                      choices = unique(use_status_clean$Country),
                                      selected = ""))),
                        br(),
                        fluidRow(downloadButton( 'plot.download', 'Télécharger les graphiques et les tableaux')),
                        hr(),
                        h3("Quelle proportion de femmes a déjà eu recours à la contraception ? Les utilisatrices actuelles ou les anciennes utilisatrices constituent-elles le plus grand pourcentage de femmes ?"),
                        h4("Y a-t-il une plus grande proportion de femmes utilisatrices (actuelles + anciennes) ou jamais utilisatrices ? Y a-t-il une plus grande proportion d’utilisatrices anciennes ou actuelles ? Si une grande proportion de femmes sont d'anciennes utilisatrices, cela suggère que les contraceptifs sont utilisés pour espacer les naissances, et une part importante de femmes n'ayant pas encore atteint la taille idéale de leur famille. Cela pourrait également suggérer un problème de programme si les femmes abandonnent en raison de l'insatisfaction. Nous explorerons ces problèmes dans l'onglet précédent. Si deux enquêtes ou plus sont disponibles, avez-vous constaté une augmentation de l'utilisation actuelle ou antérieure ?"), 
                        fluidRow( plotOutput("use_status", inline = TRUE)),
                        hr(),
                        h3("Quelle est la répartition par âge des femmes en âge de procréer ? La plupart des utilisatrices « Jamais » sont-elles de jeunes femmes ?"),
                        h4("La plupart des utilisatrices « Jamais » sont-elles de jeunes femmes ? Nous nous attendons à ce que les jeunes femmes qui n'ont jamais eu de relations sexuelles n'aient jamais utilisé de contraception. Pour les femmes de plus de 40 ans, celles qui n'ont jamais utilisé ou ont utilisé une méthode sont moins susceptibles de commencer à utiliser une méthode car l'infertilité augmente avec l'âge."),
                        fluidRow( plotOutput("use_age_status", inline = TRUE)),   
                        hr(),
                        h3("Note : Certaines enquêtes démographiques et de santé n’interrogent que les femmes femmes jamais mariées")
                      )),
             tabPanel("Utilisatrices Jamais",
                      fluidPage(mainPanel(  h3(strong("Utilisatrices jamais– Les femmes qui n’ont jamais utilisé de méthode contraceptive au cours de leur vie (peuvent devenir des utilisatrices actuelles).")),
                                            h4("Les femmes qui n’ont jamais utilisé de contraceptifs peuvent nous aider à comprendre les obstacles potentiels à l’utilisation des contraceptifs et l’ampleur de la croissance potentielle. Comprendre qui compose ce groupe, si elle peut avoir un besoin actuel ou futur de contraception et si elle a l’intention de l’utiliser à l’avenir peut aider à concevoir des programmes qui répondent à leurs besoins - les femmes qui n’ont jamais utilisé de contraception ont probablement besoin de messages différents et ont des obstacles différents à l’utilisation de ceux des femmes qui ont déjà utilisé la contraception."),
                                            h3("Choisissez une question pour explorer les données"),
                                            selectInput("neverselect", label = h3(""),
                                                        choices = list("Quelle proportion des utilisatrices jamais peuvent avoir besoin d'une contraception en ce moment", 
                                                                       "Quelle proportion utilisatrices jamais ayant l'intention d'utiliser à l'avenir",
                                                                       "Quelle est la répartition des utilisatrices jamais ayant l'intention d'utiliser à l'avenir ?")),
                                            h4(textOutput("never_text")), 
                                            plotOutput("neverplot", inline = TRUE))) ,
                      h3("Note : Certaines enquêtes démographiques et de santé n’interrogent que les femmes femmes jamais mariées")),
             tabPanel("Utilisatrices Ancienne",
                      fluidPage(mainPanel(  h3(strong("Utilisatrices ancienne– Femmes qui ont déjà utilisé une méthode de contraception, mais qui ne l’utilisent plus (peuvent devenir des utilisatrices actuelles)")),
                                            h4("Les anciennes utilisatrices peuvent nous aider à comprendre les succès et les problèmes éventuels du programme de planning familial. Ces femmes ont choisi d’utiliser une méthode mais l’ont abandonnée - comprendre les raisons de leur abandon et leurs intentions futures d’utilisation peut aider à indiquer où le programme peut avoir besoin d’apporter des changements pour aider à retenir les utilisatrices de contraceptifs ou rendre la reprise après une interruption planifiée aussi facile que possible."),
                                            h3("Choisissez une question pour explorer les données"),
                                            selectInput("formerselect", label = h3(""),
                                                        choices = list("Quelle proportion utilisatrices ancienne peuvent avoir besoin d'une contraception en ce moment ?",
                                                                       "Pourquoi les utilisatrices ancienne ont-elles cessé d'utiliser la planification familiale ?",
                                                                       "Quand les utilisatrices ancienne ont-elles cessé d'utiliser la planification familiale ?",
                                                                       "Quelle proportion utilisatrices ancienne ayant l'intention d'utiliser à l'avenir ?",
                                                                       "Quelle est la répartition des utilisatrices ancienne ayant l'intention d'utiliser à l'avenir ?")),
                                            h4(textOutput("former_text")), 
                                            fluidRow(  plotOutput("formerplot", inline = TRUE))
                      )),
                      h3("Note : Certaines enquêtes démographiques et de santé n’interrogent que les femmes femmes jamais mariées")
             ),
             tabPanel("Utilisatrices Actuelle",
                      fluidPage(mainPanel(   h3(strong("Utilisatrices actuelle- Les femmes qui utilisent actuellement une méthode de contraception pour éviter une grossesse (peuvent devenir des utilisatrices ancienne)")),
                                             h4("Grâce aux utilisatrices actuelle, nous pouvons savoir qui en fait usage, quelles sont les méthodes disponibles, la qualité des services et de la source, entre autres, ce qui nous aide à mieux comprendre les services fournis et les clients dont les besoins sont satisfaits."),
                                             h3("Choisissez une question pour explorer les données"),
                                             fluidRow( selectInput("currentselect", label = h3(""),
                                                                   choices =list("Quelle est la répartition des utilisatrices actuelle ?", 
                                                                                 "Quelles sont les méthodes les plus populaires ?",
                                                                                 "Depuis combien de temps les femmes utilisent-elles leur méthode ?",
                                                                                 "Où accèdent-elles aux méthodes ?",
                                                                                 "Sont-elles informées des autres méthodes et des effets secondaires ?"))),
                                             h4(textOutput("current_text")), 
                                             fluidRow(  plotOutput("currentplot", inline = TRUE)),
                                             hr(),   br()
                      )),
                      h3("Note : Certaines enquêtes démographiques et de santé n’interrogent que les femmes femmes jamais mariées"))
  )




server <- function(input, output, session) {
  
  # USE STATUS
  
  use_status_Data = reactiveVal(use_status_clean %>% mutate(key = 1:nrow(use_status_clean )))
  
  use_status_country <- reactive({
    use_status_sel <- use_status_Data()  %>% filter(Country %in% input$Country)    %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ ""))
    use_status_sel
  })
  
  
  use_status_plot<-reactive({
    
    use_status_df <- use_status_country()
    
    use_status_p <-      ggplot(use_status_df, aes(x=as.factor(StartYear), y=Freq*100,  fill=Var1, label=label)) +
      geom_bar(stat="identity") + 
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      labs(x="", y="Pourcentage de la population de FAP", shape="", fill="", title=input$Country) +
      coord_flip() +
      scale_fill_manual(values=c("Utilisatrice actuelle"= "#3892C6", "Ancienne utilisatrice"= "#8DC645", "N’a jamais utilisé"= "#954F72"),
                        guide = guide_legend(reverse = TRUE)) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=23),
            legend.position = "bottom")
  })
  
  
  output$use_status <-renderPlot({
    
    print(use_status_plot())
    
  }, height = 600 ,width = 800)
  
  
  # USE AGE
  use_age_status_Data = reactiveVal(use_age_status_clean %>% mutate(key = 1:nrow(use_age_status_clean )))  
  
  use_age_status_country	 <- reactive({	
    use_age_status_sel <- use_age_status_Data()  %>% filter(Country %in% input$Country)    %>%
      mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ "")) 
    use_age_status_sel	
  })
  
  use_age_status_plot<-reactive({
    
    use_age_status_df <- use_age_status_country()
    
    use_age_status_p <-  ggplot(use_age_status_df, aes(x=Var2, y=Freq*100, fill=Var1, label=label)) +
      geom_bar(stat="identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      labs(x="", y="Pourcentage de la population de FAP", shape="", fill="", title=paste(input$Country, ": Distribution d’âge, ", mean(use_age_status_df$StartYear), sep="")) +
      scale_fill_manual(values=c("15-24" = "#0B9444",  "25-39"= "#FCD116", "40-49"= "#3892C6")) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=23),
            legend.position = "bottom")
  })
  
  
  output$use_age_status <-renderPlot({
    
    print(use_age_status_plot())
    
  }, height = 600 ,width = 800)
  
  # NEVER USE GROUP 
  never_group_Data = reactiveVal(never_group_clean %>% mutate(key = 1:nrow(never_group_clean )))
  
  never_group_country	 <- reactive({	
    never_group_sel <- never_group_Data()  %>% filter(Country %in% input$Country)     %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ "")) 
    never_group_sel	 
  })
  
  never_group_plot<-reactive({
    
    never_group_df <- never_group_country()
    
    never_group_p <-    
      ggplot(never_group_df, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=label)) +
      geom_bar(stat = "identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      labs(x="", y="", fill="", title=paste(input$Country, ": Répartition de la population\ndes personnes n’ayant jamais utilisé", sep="")) +
      coord_flip() +
      scale_fill_manual(values=c( "Pas mariée, sexuellement actives"= "#0B9444",  "Pas mariée, pas active sexuellement" = "#8DC645",  "Inféconde, ménopausiques"= "#954F72",  "Mariée, veut un enfant dans les 2 ans"= "#1D4E92",   "Mariée, ne veut pas d’enfants dans les 2 ans"= "#3892C6")) +
      guides(fill=guide_legend(nrow=3,byrow=TRUE, rev=TRUE)) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=15),
            legend.position = "bottom")
  })
  
  output$never_group <-renderPlot({
    
    print(never_group_plot())
    
  }, height = 600 ,width = 800)
  
  # NEVER INTENTION
  never_intention_Data = reactiveVal(never_intention_clean %>% mutate(key = 1:nrow(never_intention_clean )))
  
  never_intention_country	 <- reactive({	
    never_intention_sel <- never_intention_Data()  %>% filter(Country %in% input$Country)    %>% mutate(label=case_when(Freq *100 >=5 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 5 ~ "")) 
    never_intention_sel	
  })
  
  never_intention_plot<-reactive({
    
    never_intention_df <- never_intention_country()
    
    never_intention_p <-      ggplot(never_intention_df, aes(x=Group, y=Freq * 100, fill=Intention, label=label)) +
      geom_bar(stat = "identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      scale_x_discrete(limits=rev) +
      coord_flip() +
      labs(x="", y="", fill="", title=paste(input$Country, ", ",  mean(never_intention_df$StartYear), ": Intention d’utilisation\nà l’avenir d’utilisatrices jamais", sep="")) +
      scale_fill_manual(values=c("N’a pas l’intention" = "#3892C6",  "Incertaine quant à l’utilisation"= "#FCD116", "Utiliser plus tard"= "#0B9444")) +
      guides(fill=guide_legend(nrow=2,byrow=TRUE, rev=TRUE)) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=20),
            legend.position = "bottom") 
  })
  
  output$never_intention <-renderPlot({
    
    print(never_intention_plot())
    
  }, height = 600 ,width = 800)
  
  # NEVER HAS INTENTION
  never_has_intention_Data = reactiveVal(never_has_intention_clean %>% mutate(key = 1:nrow(never_has_intention_clean )))
  
  never_has_intention_country	 <- reactive({	
    never_has_intention_sel <- never_has_intention_Data()  %>% filter(Country %in% input$Country)    	
    never_has_intention_sel	
  })
  
  never_has_intention_plot<-reactive({
    
    never_has_intention_df <- never_has_intention_country()
    
    never_pie <- never_has_intention_df %>%  mutate(values=round(Freq*100)) %>% filter(values>0) %>%
      arrange(-values) 
    never_pie$Group <- reorder(never_pie$Group, never_pie$values)
    never_pie <- never_pie %>% mutate(pos = cumsum(values)- values/2) %>% mutate(labels=case_when(values>=3 ~ as.character(values), values< 3 ~ "")) %>%
      mutate(percent = case_when(values>=3 ~ "%", values< 3 ~ "")) %>% mutate(labels=paste(labels, percent, sep=""))
    
    
    never_has_intention_p <-     ggplot(never_pie, aes(x="", y=values, fill=Group))+
      geom_bar(width = 1, stat = "identity")+ 
      coord_polar("y", start=0) +
      labs(fill="", title=paste(input$Country, ": Utilisatrices jamais qui\nont l’intention d’utiliser, ", mean(never_pie$StartYear), sep="")) +
      geom_text(aes(x= "", y=pos, label = labels), size=7) +
      scale_fill_manual(values=c( "Pas mariée, sexuellement actives"= "#0B9444",  "Pas mariée, pas active sexuellement" = "#8DC645",  "Inféconde, ménopausiques"= "#954F72",  "Mariée, veut un enfant dans les 2 ans"= "#1D4E92",   "Mariée, ne veut pas d’enfants dans les 2 ans"= "#3892C6")) +
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
  })
  
  output$never_has_intention <-renderPlot({
    
    print(never_has_intention_plot())
    
  }, height = 600 ,width = 800)
  
  
  # FORMER GROUP
  former_group_Data = reactiveVal(former_group_clean %>% mutate(key = 1:nrow(former_group_clean )))
  
  former_group_country	 <- reactive({	
    former_group_sel <- former_group_Data()  %>% filter(Country %in% input$Country)    	  %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ "")) 
    former_group_sel	 
  })
  
  former_group_plot<-reactive({
    
    former_group_df <- former_group_country()
    
    
    
    former_group_p <-  ggplot(former_group_df, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=label)) +
      geom_bar(stat = "identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      labs(x="", y="", fill="", title=paste(input$Country, ": Répartition de la population\ndes anciennes utilisatrices", sep="")) +
      coord_flip() +
      scale_fill_manual(values=c( "Pas mariée, sexuellement actives"= "#0B9444",  "Pas mariée, pas active sexuellement" = "#8DC645",  "Inféconde, ménopausiques"= "#954F72",  "Mariée, veut un enfant dans les 2 ans"= "#1D4E92",   "Mariée, ne veut pas d’enfants dans les 2 ans"= "#3892C6")) +
      guides(fill = guide_legend(nrow=3,byrow=TRUE,  reverse=TRUE)) +
      theme_bw()  +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=15),
            legend.position = "bottom")
  })
  
  output$former_group <-renderPlot({
    
    print(former_group_plot())
    
  }, height = 600 ,width = 800)
  
  
  # FORMER INTENTION
  former_intention_Data = reactiveVal(former_intention_clean %>% mutate(key = 1:nrow(former_intention_clean )))
  
  former_intention_country	 <- reactive({	
    former_intention_sel <- former_intention_Data()  %>% filter(Country %in% input$Country)    	 %>% mutate(label=case_when(Freq *100 >=5 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 5 ~ "")) 
    former_intention_sel	 
  })
  
  former_intention_plot<-reactive({
    
    former_intention_df <- former_intention_country()
    
    
    former_intention_p <-   ggplot(former_intention_df, aes(x=Group, y=Freq * 100, fill=Intention, label=label)) +
      geom_bar(stat = "identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      scale_x_discrete(limits=rev) +
      coord_flip() +
      labs(x="", y="", fill="", title=paste(input$Country, ", ",  mean(former_intention_df$StartYear), ": Anciennes utilisatrices\nIntention d’utiliser à l’avenir", sep="")) +
      scale_fill_manual(values=c("N’a pas l’intention" = "#3892C6",  "Incertaine quant à l’utilisation"= "#FCD116", "Utiliser plus tard"= "#0B9444")) +
      guides(fill=guide_legend(nrow=2,byrow=TRUE, rev=TRUE)) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=20),
            legend.position = "bottom")
    
  })
  
  output$former_intention <-renderPlot({
    
    print(former_intention_plot())
    
  }, height = 600 ,width = 800)
  
  
  # FORMER HAS INTENTION
  
  former_has_intention_Data = reactiveVal(former_has_intention_clean %>% mutate(key = 1:nrow(former_has_intention_clean )))
  
  former_has_intention_country	 <- reactive({	
    former_has_intention_sel <- former_has_intention_Data()  %>% filter(Country %in% input$Country)    	
    former_has_intention_sel	
  })
  
  
  former_has_intention_plot<-reactive({
    
    former_has_intention_df <- former_has_intention_country()
    
    pie_former <- former_has_intention_df %>%  mutate(values=round(Freq*100)) %>% filter(values>0) %>%
      arrange(-values) 
    pie_former$Group <- reorder(pie_former$Group, pie_former$values)
    pie_former <- pie_former %>% mutate(pos = cumsum(values)- values/2) %>% mutate(labels=case_when(values>=3 ~ as.character(values), values< 3 ~ "")) %>%
      mutate(percent = case_when(values>=3 ~ "%", values< 3 ~ "")) %>% mutate(labels=paste(labels, percent, sep=""))
    
    
    former_has_intention_p <-   ggplot(pie_former, aes(x="", y=values, fill=Group))+
      geom_bar(width = 1, stat = "identity")+ 
      coord_polar("y", start=0) +
      labs(fill="", title=paste(input$Country, ": Utilisatrices ancienne ayant\nl’intention d’utiliser, ", mean(pie_former$StartYear), sep="")) +
      geom_text(aes(x= "", y=pos, label = labels), size=7) +
      scale_fill_manual(values=c( "Pas mariée, sexuellement actives"= "#0B9444",  "Pas mariée, pas active sexuellement" = "#8DC645",  "Inféconde, ménopausiques"= "#954F72",  "Mariée, veut un enfant dans les 2 ans"= "#1D4E92",   "Mariée, ne veut pas d’enfants dans les 2 ans"= "#3892C6")) +
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
    
  })
  
  output$former_has_intention <-renderPlot({
    
    print(former_has_intention_plot())
    
  }, height = 600 ,width = 800)
  
  
  
  
  
  
  # Former Why Discontinue
  former_why_discontinue_Data = reactiveVal(former_why_discontinue_clean %>% mutate(key = 1:nrow(former_why_discontinue_clean )))
  
  former_why_discontinue_country	 <- reactive({	
    former_why_discontinue_sel <- former_why_discontinue_Data()  %>% filter(Country %in% input$Country)  %>% select(Var1, WhyDisc, Freq) %>% rename(Group=Var1) %>%
      mutate(Freq= paste(round(Freq * 100, 1), "%", sep = "")) %>% spread(WhyDisc, Freq)  	
    former_why_discontinue_sel	 
  })
  
  output$former_why_discontinue_table <- renderDataTable(former_why_discontinue_country())
  
  
  
  
  # FORMER WHY DISCONTINUE Plot
  
  former_whydispie_Data =  reactiveVal(former_why_discontinue_clean %>% mutate(key = 1:nrow(former_why_discontinue_clean )))
  
  former_whydispie_country	 <- reactive({	
    former_whydispie_sel <- former_whydispie_Data()  %>% filter(Country %in% input$Country)    	
    former_whydispie_sel	
  })
  
  
  former_whydispie_plot<-reactive({
    
    former_whydispie_df <- former_whydispie_country()
    
    pie_formerdis <- former_whydispie_df %>%   mutate(values=round(Freq*100))  %>% filter(Freq>0) %>%
      arrange(Var1, WhyDisc) %>% group_by(Var1)  %>% mutate(pos = 100- (cumsum(Freq)- Freq/2)*100) %>%
      mutate(labels=case_when(values>=5 ~ as.character(values), values< 5 ~ "")) %>%
      mutate(percent = case_when(values>=5 ~ "%", values< 5 ~ "")) %>% mutate(labels=paste(labels, percent, sep=""))
    
    
    former_whydis_p <-   ggplot(pie_formerdis , aes(x="",  y=Freq*100, fill=WhyDisc))+
      geom_bar(width = 1, stat = "identity")+ 
      cp +
      facet_wrap(~ Var1, scales = "free") +
      labs(fill="", title=paste(input$Country, ", " , mean(pie_formerdis$StartYear) , ": Pourquoi les utilisatrices anciennes\nont-elles arrêté la contraception ?", sep="")) +
      geom_text(aes(x= "", y=pos, label = labels), size=4) +
      guides(fill = guide_legend( reverse=TRUE)) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        text=element_text(size=20),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        legend.text =element_text(size=15),
        strip.background = element_blank(),
        aspect.ratio = 1) 
    
  })
  
  output$former_whydispie <-renderPlot({
    
    print(former_whydispie_plot())
    
  }, height = 600 ,width = 800)
  
  # Former Time Use Group
  
  former_time_use_group_Data = reactiveVal(former_time_use_group_clean %>% mutate(key = 1:nrow(former_time_use_group_clean )))
  
  former_time_use_group_country	 <- reactive({	
    former_time_use_group_sel <- former_time_use_group_Data()  %>% filter(Country %in% input$Country)    	
    former_time_use_group_sel	 
  })
  
  former_time_use_group_plot<-reactive({
    
    former_time_use_group_df <- former_time_use_group_country()
    
    
    
    former_time_use_group_p <-    ggplot(former_time_use_group_df, aes(x=Time_Since_Use, y=Freq*100, fill=Time_Since_Use)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=paste(round(Freq*100), "%", sep="")), vjust=-.5, size=6) +
      labs(x="", y="Pourcentage d’anciennes utilisatrices", fill="", title=paste(input$Country, ", ",  mean(former_time_use_group_df$StartYear), ": Quand les utilisatrices anciennes ont-elles\nutilisé la planification familiale pour la dernière fois ?", sep="")) +
      scale_fill_manual(values=c("Moins d’un an" = "#F49100" , "1 an" = "#FCD116", "2 ans"= "#8DC645", "3 ans"= "#0B9444",  "4 ans"= "#3892C6", "5 ans et plus" = "#1D4E92")) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=20),
            legend.position = "none")
  })
  
  output$former_time_use_group <-renderPlot({
    
    print(former_time_use_group_plot())
    
  }, height = 600 ,width = 800)
  
  # Current Group
  current_group_Data = reactiveVal(current_group_clean %>% mutate(key = 1:nrow(current_group_clean )))
  
  current_group_country	 <- reactive({	
    current_group_sel <- current_group_Data()  %>% filter(Country %in% input$Country)   %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ ""))   	
    current_group_sel	 
  })
  
  current_group_plot<-reactive({
    
    current_group_df <- current_group_country()
    
    current_group_p <-   ggplot(current_group_df, aes(x=as.factor(StartYear), y=Freq * 100, fill=Group, label=label)) +
      geom_bar(stat = "identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      labs(x="", y="", fill="", title=paste(input$Country, ": Distribution de la population\ndes utilisatrices actuelle", sep="")) +
      coord_flip() +
      scale_fill_manual(values=c( "Jamais mariée"= "#8DC645",  "Anciennement mariée"= "#FCD116",  "Mariée"= "#3892C6")) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=20),
            legend.position = "bottom")
  })
  
  output$current_group <-renderPlot({
    
    print(current_group_plot())
    
  }, height = 600 ,width = 800)
  
  # Current Method Mix
  current_methodmix_Data = reactiveVal(current_methodmix_clean %>% mutate(key = 1:nrow(current_methodmix_clean )))
  
  current_methodmix_country	 <- reactive({	
    current_methodmix_sel <- current_methodmix_Data()  %>% filter(Country %in% input$Country)    	
    current_methodmix_sel	 
  })
  
  current_methodmix_plot<-reactive({
    
    current_methodmix_df <- current_methodmix_country()
    
    current_methodmix_p <- ggplot(current_methodmix_df, aes(x=as.factor(StartYear), y=Freq * 100, fill=Method)) +
      geom_bar(stat = "identity", color="white") +
      labs(x="", y="", fill="", title=paste(input$Country, ": Gamme de méthodes des utilisatrices actuelle", sep="")) +
      theme_bw()  +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=15))
    
  })
  
  output$current_methodmix <-renderPlot({
    
    print(current_methodmix_plot())
    
  }, height = 600 ,width = 800)
  
  # MII
  mii_Data = reactiveVal(mii_clean %>% mutate(key = 1:nrow(mii_clean )))
  
  mii_country	 <- reactive({	
    mii_sel <- mii_Data()  %>% filter(Country %in% input$Country)    	
    mii_sel	 
  })
  
  mii_plot<-reactive({
    
    mii_df <- mii_country()
    
    mii_p <- ggplot(mii_df, aes(x=Method, y=Freq*100, color=Var, shape=Var, fill=Var)) +
      geom_segment(aes(x=Method, xend=Method, y=0, yend=Max*100), color="black") +
      geom_point(size=7) +
      scale_shape_manual(na.translate = F,
                         values = c(21, 22,   23,  24)) +
      scale_fill_manual(values=c("#F49100", "#FCD116", "#0B9444","#1D4E92")) +
      scale_color_manual(values=c("#F49100", "#FCD116", "#0B9444","#1D4E92")) +
      labs(x="", y="Pour cent", fill="", color="", shape="", title=paste(input$Country, ", ",   mean(mii_df$StartYear), ": Indice d’information sur les méthodes", sep="")) +
      coord_flip() +
      guides(color = guide_legend(nrow=4,byrow=TRUE), shape = guide_legend(nrow=4,byrow=TRUE), fill = guide_legend(nrow=4,byrow=TRUE)) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=20),
            legend.position = "bottom")
    
  })
  
  output$mii <-renderPlot({
    
    print(mii_plot())
    
  }, height = 600 ,width = 800)
  
  
  # Current Source
  current_source_Data = reactiveVal(current_source_clean %>% mutate(key = 1:nrow(current_source_clean )))
  
  current_source_country	 <- reactive({	
    current_source_sel <- current_source_Data()  %>% filter(Country %in% input$Country)  %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ ""))    	
    current_source_sel	 
  })
  
  current_source_plot<-reactive({
    
    current_source_df <- current_source_country()
    
    current_source_p <-  
      ggplot(current_source_df, aes(x=as.factor(StartYear), y=Freq * 100, fill=Source, label=label)) +
      geom_bar(stat = "identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      guides(fill = guide_legend(ncol=2,byrow=TRUE)) +
      labs(x="", y="", fill="", title=paste(input$Country, ": Source de la méthode des utilisatrices actuelle", sep="")) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=20),
            legend.position = "bottom")
    
  })
  
  output$current_source <-renderPlot({
    
    print(current_source_plot())
    
  }, height = 600 ,width = 800)
  
  # Current Time Since
  current_timesince_Data = reactiveVal(current_timesince_clean %>% mutate(key = 1:nrow(current_timesince_clean )))
  
  current_timesince_country	 <- reactive({	
    current_timesince_sel <- current_timesince_Data()  %>% filter(Country %in% input$Country)   	
    current_timesince_sel	 
  })
  
  current_timesince_plot<-reactive({
    
    current_timesince_df <- current_timesince_country()
    
    current_timesince_p <-  ggplot(current_timesince_df, aes(x=Var1, y=Freq*100, fill=Var1)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=paste(round(Freq*100), "%", sep="")), vjust=-.5, size=6) +
      labs(x="", y="Pourcentage d’utilisatrices actuelles", fill="", title=paste(input$Country, ", ",   mean(current_timesince_df$StartYear), ":\nQuand les utilisatrices actuelle ont-elles commencé à utiliser\nla méthode actuelle ?", sep="")) +
      scale_fill_manual(values=c("Moins d’un an" = "#F49100" , "1 an" = "#FCD116", "2 ans"= "#8DC645", "3 ans"= "#0B9444",  "4 ans"= "#3892C6", "5 ans et plus" = "#1D4E92")) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=20),
            legend.position = "none")
    
    
  })
  
  output$current_timesince <-renderPlot({
    
    print(current_timesince_plot())
    
  }, height = 600 ,width = 800)
  
  # Empowerment
  empowerment_Data = reactiveVal(empowerment_clean %>% mutate(key = 1:nrow(empowerment_clean )))
  
  empowerment_country	 <- reactive({	
    empowerment_sel <- empowerment_Data()  %>% filter(Country %in% input$Country)    %>%
      mutate(Group = fct_relevel(Group, c( "Never User", "Former User", "Current User"))) 	
    empowerment_sel	 
  })
  
  empowerment_plot<-reactive({
    
    empowerment_df <- empowerment_country()
    
    empowerment_p <-   ggplot(empowerment_df, aes(x=Group, y=Freq*100, fill=Decider, label=paste(round(Freq*100), "%", sep=""))) +
      geom_bar(stat="identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      scale_fill_manual(values=c("#F49100", "#FCD116", "#0B9444","#3892C6", "#1D4E92")) +
      guides(fill = guide_legend(ncol=2,byrow=TRUE)) +
      labs(x="", y="Percent of Married Women in Each Group", fill="", title=paste(input$Country, ", ",  mean(empowerment_df$StartYear), ":\nWho Made the Decision About Use", sep="")) +
      theme_bw() +
      theme(text=element_text(size=20, color="black"),
            axis.text =element_text(size=20, color="black"),
            legend.text =element_text(size=20),
            legend.position = "bottom")
    
  })
  
  output$empowerment <-renderPlot({
    
    print(empowerment_plot())
    
  }, height = 600 ,width = 800)
  
  
  
  
  neverplotchoice <- reactive({
    if ( "Quelle proportion des utilisatrices jamais peuvent avoir besoin d'une contraception en ce moment" %in% input$neverselect) return( never_group_plot())
    if ("Quelle proportion utilisatrices jamais ayant l'intention d'utiliser à l'avenir" %in% input$neverselect) return( never_intention_plot())
    if (  "Quelle est la répartition des utilisatrices jamais ayant l'intention d'utiliser à l'avenir ?" %in% input$neverselect) return(never_has_intention_plot())
  })
  
  output$neverplot <- renderPlot({   
    print(neverplotchoice())
  } , height = 600 , width = 800)
  
  
  nevertextchoice <- reactive({
    if ( "Quelle proportion des utilisatrices jamais peuvent avoir besoin d'une contraception en ce moment" %in% input$neverselect) return( "Les femmes mariées qui ne veulent pas d’enfant dans les deux prochaines années et les femmes célibataires sexuellement actives peuvent avoir besoin d’une contraception pour éviter une grossesse.  Les femmes qui ne sont pas mariées et qui ne sont pas sexuellement actives et celles qui sont mariées et qui veulent un enfant bientôt n’ont peut-être pas besoin de contraception maintenant mais peuvent bénéficier d’informations sur la planification familiale. Les femmes infécondes ou ménopausées qui n’ont jamais utilisé de contraceptifs ont peu de chances de le faire à l’avenir.")
    if ( "Quelle proportion utilisatrices jamais ayant l'intention d'utiliser à l'avenir" %in% input$neverselect) return("Pour chaque groupe, nous pouvons examiner qui a besoin de programmes (celles qui ont l’intention d’utiliser le planning familial à l’avenir) par rapport à qui a besoin d’informations sur les avantages du planning familial (celles qui ne sont pas sûres ou qui n’ont pas l’intention d’utiliser le planning familial à l’avenir). Si une grande partie des femmes non mariées (principalement des jeunes) n’ont pas l’intention de recourir à la planification familiale à l’avenir, il est peut-être nécessaire d’élargir la portée de l’action auprès des jeunes." )
    if (  "Quelle est la répartition des utilisatrices jamais ayant l'intention d'utiliser à l'avenir ?" %in% input$neverselect) return("D’un point de vue programmatique, il est utile de savoir quelle est la population la plus importante parmi ceux qui ont l’intention d’utiliser les contraceptifs à l’avenir, en particulier si ce groupe a un besoin actuel non satisfait.")
  })
  
  output$never_text <- renderText({ 
    nevertextchoice()
  })
  
  

  formerplotchoice <- reactive({
    if ( "Quelle proportion utilisatrices ancienne peuvent avoir besoin d'une contraception en ce moment ?" %in% input$formerselect) return( former_group_plot())
    if ("Pourquoi les utilisatrices ancienne ont-elles cessé d'utiliser la planification familiale ?" %in% input$formerselect) return( former_whydispie_plot())
    if ( "Quand les utilisatrices ancienne ont-elles cessé d'utiliser la planification familiale ?" %in% input$formerselect) return( former_time_use_group_plot())
    if ( "Quelle proportion utilisatrices ancienne ayant l'intention d'utiliser à l'avenir ?" %in% input$formerselect) return( former_intention_plot())
    if ( "Quelle est la répartition des utilisatrices ancienne ayant l'intention d'utiliser à l'avenir ?" %in% input$formerselect) return( former_has_intention_plot())
  })
  
  output$formerplot <- renderPlot({   
    print(formerplotchoice())
  } , height = 600 , width = 800)
  
  formertextchoice <- reactive({
    if ("Quelle proportion utilisatrices ancienne peuvent avoir besoin d'une contraception en ce moment ?" %in% input$formerselect) return( "Les femmes mariées qui ne veulent pas d’enfant dans les deux prochaines années et les femmes célibataires sexuellement actives peuvent avoir besoin de la contraception pour éviter une grossesse.  Les femmes qui ne sont pas mariées et qui ne sont pas sexuellement actives, ainsi que celles qui sont mariées et qui veulent un enfant bientôt, n’ont peut-être pas besoin de contraception maintenant, mais elles pourraient vouloir redevenir des utilisatrices de planification familiale à l’avenir. Les femmes infécondes ou ménopausées qui ont déjà eu recours à la contraception ont peu de chances de le faire à l’avenir.")
    if ( "Pourquoi les utilisatrices ancienne ont-elles cessé d'utiliser la planification familiale ?" %in% input$formerselect) return("Dans un programme axé sur l’espacement, nous nous attendons à ce que de nombreuses femmes cessent d’utiliser une méthode contraceptive parce qu’elles veulent tomber enceintes. Les femmes dont le risque de grossesse diminue (rapports sexuels peu fréquents, dissolution du mariage et ménopause) sont également susceptibles d’arrêter d’utiliser une méthode. Si une grande partie des femmes cessent d’utiliser une méthode parce qu’elles sont tombées enceintes alors qu’elles l’utilisaient, cela suggère des taux élevés d’échec de la méthode. Si une grande proportion d’anciennes utilisatrices cessent d’utiliser une méthode en raison des effets secondaires (et ne commencent pas une nouvelle méthode), cela suggère un accès limité à un large éventail de méthodes et à l’information.")
    if ( "Quand les utilisatrices ancienne ont-elles cessé d'utiliser la planification familiale ?" %in% input$formerselect) return( "Si la plupart des anciennes utilisatrices ont cessé d’utiliser la contraception récemment, cela peut suggérer qu’elles ont cessé pour tomber enceintes. Une concentration d’anciennes utilisatrices qui ont arrêté d’utiliser il y a plus de 5 ans suggère que de nombreuses anciennes utilisatrices peuvent penser qu’elles ont un risque réduit de grossesse non planifiée ou qu’elles veulent tomber enceintes. Une grande proportion d’anciennes utilisatrices qui n’ont pas eu recours à la contraception au cours des cinq dernières années peut donner à penser qu’elles ont eu une expérience négative avec les programmes de planning familial.")
    if ( "Quelle proportion utilisatrices ancienne ayant l'intention d'utiliser à l'avenir ?" %in% input$formerselect) return( "Pour chaque groupe, nous pouvons déterminer qui a besoin d’un programme (ceux qui ont l’intention d’utiliser la contraception à l’avenir) et qui a besoin d’informations sur les avantages de la planification familiale (celles qui ne sont pas sûres ou qui n’ont pas l’intention de l’utiliser à l’avenir).")
    if ( "Quelle est la répartition des utilisatrices ancienne ayant l'intention d'utiliser à l'avenir ?" %in% input$formerselect) return("D’un point de vue programmatique, il est utile de savoir quelle est la population la plus importante parmi celles qui ont l’intention d’utiliser la contraception à l’avenir, en particulier si ce groupe a un besoin actuel non satisfait.")
  })
  
  output$former_text <- renderText({ 
    formertextchoice()
  })
  
  

  currentplotchoice <- reactive({
    if ( "Quelle est la répartition des utilisatrices actuelle ?" %in% input$currentselect) return( current_group_plot())
    if ( "Quelles sont les méthodes les plus populaires ?" %in% input$currentselect) return( current_methodmix_plot())
    if ( "Depuis combien de temps les femmes utilisent-elles leur méthode ?" %in% input$currentselect) return( current_timesince_plot())
    if (  "Où accèdent-elles aux méthodes ?" %in% input$currentselect) return( current_source_plot())
    if ( "Sont-elles informées des autres méthodes et des effets secondaires ?" %in% input$currentselect) return( mii_plot())
  })
  
  output$currentplot <- renderPlot({   
    print(currentplotchoice())
  } , height = 600 , width = 800)
  
  
  currenttextchoice <- reactive({
    if ( "Quelle est la répartition des utilisatrices actuelle ?" %in% input$currentselect) return("Les femmes mariées et non mariées sont-elles des utilisatrices actuelles ? Si deux enquêtes ou plus sont disponibles, la répartition a-t-elle évolué dans le temps ?" )
    if ( "Quelles sont les méthodes les plus populaires ?" %in% input$currentselect) return( "Quelles sont les méthodes contraceptives les plus utilisées ? S’agit-il de méthodes à court terme ou à long terme ? Si deux enquêtes ou plus sont disponibles, la répartition a-t-elle évolué dans le temps ?" )
    if ( "Depuis combien de temps les femmes utilisent-elles leur méthode ?" %in% input$currentselect) return( "Dans les pays où l’utilisation des contraceptifs connaît une croissance spectaculaire, nous nous attendons à ce qu’un plus grand nombre de femmes utilisent de nouvelles méthodes. De même, si les changements de méthode sont nombreux, on peut s’attendre à ce que les femmes utilisent leur méthode actuelle pendant une période plus courte.")
    if (  "Où accèdent-elles aux méthodes ?" %in% input$currentselect) return("La plupart des utilisatrices reçoivent-elles leurs méthodes d’une source publique ou privée ? Si deux enquêtes ou plus sont disponibles, la répartition a-t-elle changé au fil du temps ?")
    if ( "Sont-elles informées des autres méthodes et des effets secondaires ?" %in% input$currentselect) return("Les utilisatrices reçoivent-elles des informations clés lorsqu’elles commencent à utiliser une méthode ? Cela varie selon la méthode ?")
  })
  
  
  output$current_text <- renderText({ 
    currenttextchoice()
  })
  
  
  # Download the plots
  output$plot.download = downloadHandler(
    filename = 'plot.zip',
    content = function( file){
      
      # Set temporary working directory
      owd <- setwd( tempdir())
      on.exit( setwd( owd))
      
      # Save the histograms (a loop can be used here for a bunch of plots)
      ggsave('use_status.jpg', plot = use_status_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('use_age_status.jpg', plot = use_age_status_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('never_group.jpg', plot = never_group_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('never_intention.jpg', plot = never_intention_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('never_has_intention.jpg', plot = never_has_intention_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('former_group.jpg', plot = former_group_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('former_intention.jpg', plot = former_intention_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('former_has_intention.jpg', plot = former_has_intention_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('former_time_use_group.jpg', plot = former_time_use_group_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('former_why_discontinue.jpg', plot = former_whydispie_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
       ggsave('mii.jpg', plot = mii_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('current_group.jpg', plot = current_group_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('current_methodmix.jpg', plot = current_methodmix_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('current_source.jpg', plot = current_source_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      ggsave('current_timesince.jpg', plot = current_timesince_plot(), bg = "white", device = "jpg", width=12, height=8, units="in")
      write.csv(former_why_discontinue_country(), "former_why_discontinue.csv", row.names = F, na="")
      
      # Zip them up
      zip( file, c(   'use_status.jpg',
                      'use_age_status.jpg',
                      'never_group.jpg',
                      'never_intention.jpg',
                      'never_has_intention.jpg',
                      'former_group.jpg',
                      'former_intention.jpg',
                      'former_has_intention.jpg',
                      'former_time_use_group.jpg',
                      'former_why_discontinue.jpg',
                      'mii.jpg',
                      'current_group.jpg',
                      'current_methodmix.jpg',
                      'current_source.jpg',
                      'current_timesince.jpg',
                      "former_why_discontinue.csv"))
    }
  )
  
  
  
}

shinyApp(ui = ui, server = server)