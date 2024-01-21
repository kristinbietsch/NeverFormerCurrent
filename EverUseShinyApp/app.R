# Kristin Bietsch, PhD
# kristinbietsch@avenirhealth.org
# 06/01/23
# Updated Inputs 01/20/24


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


use_status_clean <- read.csv("data/use_status_clean 011624.csv") %>% arrange(Country)
use_age_status_clean <- read.csv("data/use_age_status_clean, 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup()
use_mar_status_clean <- read.csv("data/use_mar_status_clean 011624.csv")
never_group_clean <- read.csv("data/never_group_clean 011624.csv")
never_intention_clean <- read.csv("data/never_intention_clean 011624.csv") %>% group_by(Country) %>%  mutate(recent=max(StartYear)) %>%  filter(recent==StartYear)  %>% ungroup() %>% filter(N>=50) %>%
  mutate(Group = case_when(Group== "All Never Users" ~ "All Never Users", 
                           Group==  "Infecund, Menopausal" ~ "Infecund,\nMenopausal", 
                           Group== "Not Married, Not Sexually Active" ~"Not Married,\nNot Sexually Active",
                           Group==  "Not Married, Sexually Active" ~ "Not Married,\nSexually Active" ,
                           Group== "Married, Wants Child within 2 Years" ~ "Married, Wants\nChild within 2 Years",
                           Group== "Married, Does Not Want Child within 2 Years" ~"Married, Does Not Want\nChild within 2 Years"))
never_has_intention_clean <- read.csv("data/never_has_intention_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup()
former_group_clean <- read.csv("data/former_group_clean 011624.csv")
former_intention_clean <- read.csv( "data/former_intention_clean 011624.csv") %>% mutate(Intention=str_to_title(Intention)) %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>%
  mutate(Group = case_when(Group== "All Never Users" ~ "All Former Users", 
                           Group==  "Infecund, Menopausal" ~ "Infecund,\nMenopausal", 
                           Group== "Not Married, Not Sexually Active" ~"Not Married,\nNot Sexually Active",
                           Group==  "Not Married, Sexually Active" ~ "Not Married,\nSexually Active" ,
                           Group== "Married, Wants Child within 2 Years" ~ "Married, Wants\nChild within 2 Years",
                           Group== "Married, Does Not Want Child within 2 Years" ~"Married, Does Not Want\nChild within 2 Years"))
former_has_intention_clean <- read.csv( "data/former_has_intention_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup()
former_why_discontinue_clean <- read.csv( "data/former_why_discontinue_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>% 
  mutate(Var1=case_when(Var1=="All Former Users" ~ "All Former\nUsers",
                        Var1==   "Infecund, Menopausal"  ~ "Infecund,\nMenopausal",
                        Var1== "Married, Does Not Want Child within 2 Years" ~  "Married,\nDoes Not Want\nChild within 2 Years" ,
                        Var1=="Married, Wants Child within 2 Years"  ~ "Married,\nWants Child\nwithin 2 Years",
                        Var1== "Not Married, Not Sexually Active"  ~ "Not Married,\nNot Sexually Active",
                        Var1== "Not Married, Sexually Active"  ~ "Not Married,\nSexually Active"))
former_time_use_group_clean <- read.csv( "data/former_time_use_group_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup() %>%
  mutate(Time_Since_Use = fct_relevel(Time_Since_Use, c("Under a Year", "1 Year", "2 Year", "3 Year", "4 Year", "5+ Years" )))
mii_clean <- read.csv( "data/mii_clean 011624.csv")  %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% 
  filter(recent==StartYear) %>% select(-recent)  %>% ungroup() %>% group_by(Country, Method) %>% mutate(Max=max(Freq)) %>% ungroup()
current_group_clean <- read.csv("data/current_group_clean 011624.csv") 
current_methodmix_clean <- read.csv( "data/current_methodmix_clean 011624.csv")
current_source_clean <- read.csv("data/current_source_clean 011624.csv")
current_timesince_clean <- read.csv( "data/current_timesince_clean 011624.csv")  %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup()
empowerment_clean <- read.csv( "data/empowerment_clean 011624.csv") %>% group_by(Country) %>% mutate(recent=max(StartYear)) %>% filter(recent==StartYear) %>% ungroup()

############################################


###########################################################################################################################################

ui <-
  navbarPage("Track20 Current, Former, and Never Use of Contraception", collapsible = TRUE, inverse = TRUE, theme = shinytheme("cerulean"),
             tags$head(
               tags$style(HTML("

      .navbar .navbar-nav {font-size: 20px; 
      } 
           .navbar-header { width:100% }
                   .navbar-brand { width: 100% }
       .navbar-default .navbar-brand { font-size: 30px; } 
      .selectize-input {
        height: 60px;
        width: 600px;
        font-size: 16pt;
        padding-top: 5px;
      }
      

    "))
             ),
             tabPanel("Distribution of Women of Reproductive Age",
                      fluidPage(
                        mainPanel(
                          fluidRow(h4("Generally, our assessment of family planning programs focuses on contraceptive prevalence and current contraceptive users: what proportion of the population are using, the methods they are using, their demographic characteristics, etc.")),
                          fluidRow(h4("However, this data doesn’t fully capture the successes and limitations of family planning programs. Exploring the experiences and intentions of women who are not currently using contraception (those who have in the past and those who have never used) can help point to opportunities and barriers for family planning programs.")),
                          fluidRow(h4("Women cycle on and off contraception for a wide range of reasons, meaning that at any given time, some women who have been successfully reached and had their needs met by the program may not currently be using a method. This may be especially true in countries where contraception is largely used for spacing rather than limiting. These former users, when looked at along with current users, help paint a more complete picture of the reach of the program and the availability and acceptability of contraception.")),
                          fluidRow(h4("As family planning programs expand, we expect current users and former users to grow. Each of these three groups has programmatic importance, but the questions we ask differ.")),
                          fluidRow(h4("Track20’s new “Current, Former, Never Tool” is designed to help users understand how women are distributed across these three groups, how that has changed over time, and what each group says about the successes and limitations of the family planning program in a given country.")),
                          fluidRow(h4(strong("To use this tool, start by choosing a country.  Move between the four tabs located at the top of the page, choosing different questions to explore the data.  If you would like to download all the graphics, click the download button above."))),
                          fluidRow(h3("Choose a Country"),
                            selectInput("Country", 
                                      label = "",
                                      choices = unique(use_status_clean$Country),
                                      selected = ""))),
                        br(),
                        fluidRow(downloadButton( 'plot.download', 'Download Plots and Tables')),
                        hr(),
                        h3("What proportion of women have ever used contraception? Are a larger percentage of women current or former users? "),
                        h4("Are a larger proportion of women ever users (current + former) or never users?  Are a larger proportion of ever users former or current?  If a large proportion of women are former users, it suggests contraceptive use for spacing births with a sizable share of women not yet at their ideal family size.  It could also suggest a program issue if women are discontinuing because of dissatisfaction.  We will explore these issues in the former tab.  If two or more surveys are available, have you seen more growth in current use or former use?"), 
                        fluidRow( plotOutput("use_status", inline = TRUE)),
                        hr(),
                        h3("What is the age distribution of women of reproductive age?  Are most never users young women?"),
                        h4("Are most never user young women?  We expect young women who have never had sex to have never used contraception.  For women over 40, those who are never users or former users are less likely to start using a method as infecundability rises with age."),
                        fluidRow( plotOutput("use_age_status", inline = TRUE)),   
                        hr(),
                        h3("Note: Some Demographic and Health Surveys only interview ever married women")
                      )),
             tabPanel("Never Users",
                      fluidPage(mainPanel(  h3(strong("Never Users – Women who have never in their lives used a method of contraception (can become current users)")),
                                            h4("Never Users can help us understand potential barriers to contraceptive use and the scale of potential growth. Understanding who makes up this group, whether they may have a current or future need for contraception, and whether they intend to use in the future can help in designing programs to meet their needs – women who have never used contraception likely need different messaging and have different barriers to use than those who have formerly used."),
                                            h3("Choose a Question to Explore the Data"),
                                            selectInput("neverselect", label = h3(""),
                                                        choices = list("What proportion of never users may have a need for contraception right now?", 
                                                                       "What is the distribution of never users who intend to use in the future?",
                                                                       "What proportion of never users intend to use in the future?")),
                                            h4(textOutput("never_text")), 
                                            plotOutput("neverplot", inline = TRUE))) ,
                      h3("Note: Some Demographic and Health Surveys only interview ever married women")),
             tabPanel("Former Users",
                      fluidPage(mainPanel(  h3(strong("Former Users – Women who previously used a method of contraception, but are not currently using (can become current users)")),
                                            h4("Former Users can help us understand successes and possible issues within the family planning program. These women once chose to use a method but have discontinued – understanding the reasons for their discontinuation and their future intentions to use can help indicate where the program may need to make changes to help retain contraceptive users or make uptake after a planned break as easy as possible."),
                                            h3("Choose a Question to Explore the Data"),
                                            selectInput("formerselect", label = h3(""),
                                                        choices = list("What proportion of former users may have a need for contraception right now?",
                                                                       "Why did former users stop using family planning?",
                                                                       "When did former users stop using family planning?",
                                                                       "What is the distribution of former users who intend to use in the future?",
                                                                       "What proportion of former users intend to use in the future?")),
                                            h4(textOutput("former_text")), 
                                            fluidRow(  plotOutput("formerplot", inline = TRUE))
                      )),
                      h3("Note: Some Demographic and Health Surveys only interview ever married women")
             ),
             tabPanel("Current Users",
                      fluidPage(mainPanel(   h3(strong("Current Users- Women who are currently using a method of contraception to prevent pregnancy (can become former users)")),
                                             h4("From Current Users we can understand who is using, what methods are available, service quality, and source among other things - helping to improve our understanding of the services being provided and clients whose needs are being met."),
                                             h3("Choose a Question to Explore the Data"),
                                             fluidRow( selectInput("currentselect", label = h3(""),
                                                                   choices =list("What is the distribution of current users?", 
                                                                                 "What methods are most popular?",
                                                                                 "How long have women used their method?",
                                                                                 "Where are they accessing methods?",
                                                                                 "Are they informed about other methods and side effects?"))),
                                             h4(textOutput("current_text")), 
                                             fluidRow(  plotOutput("currentplot", inline = TRUE)),
                                             hr(),   br()
                      )),
                      h3("Note: Some Demographic and Health Surveys only interview ever married women"))
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
      labs(x="", y="Percent of WRA Population", shape="", fill="", title=input$Country) +
      coord_flip() +
      scale_fill_manual(values=c("Current User"= "#3892C6", "Former User"= "#8DC645", "Never Used"= "#954F72"),
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
      mutate(Var2 = fct_relevel(Var2, c( "Never Used", "Former User", "Current User")))  %>% mutate(label=case_when(Freq *100 >=3 ~paste(round(Freq*100), "%", sep=""),  Freq *100 < 3 ~ "")) 
    use_age_status_sel	
  })
  
  use_age_status_plot<-reactive({
    
    use_age_status_df <- use_age_status_country()
    
    use_age_status_p <-  ggplot(use_age_status_df, aes(x=Var2, y=Freq*100, fill=Var1, label=label)) +
      geom_bar(stat="identity") +
      geom_text(size = 6, position = position_stack(vjust = 0.5))+
      labs(x="", y="Percent of WRA Population", shape="", fill="", title=paste(input$Country, ": Age Distribution, ", mean(use_age_status_df$StartYear), sep="")) +
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
      labs(x="", y="", fill="", title=paste(input$Country, ": Never Users Population Distribution", sep="")) +
      coord_flip() +
      scale_fill_manual(values=c( "Not Married, Sexually Active"= "#0B9444",  "Not Married, Not Sexually Active" = "#8DC645",  "Infecund, Menopausal"= "#954F72",  "Married, Wants Child within 2 Years"= "#1D4E92",   "Married, Does Not Want Child within 2 Years"= "#3892C6")) +
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
      labs(x="", y="", fill="", title=paste(input$Country, ", ",  mean(never_intention_df$StartYear), ": Never Users\nIntention to Use in Future", sep="")) +
      scale_fill_manual(values=c("Does Not Intend" = "#3892C6",  "Unsure About Use"= "#FCD116", "Use Later"= "#0B9444")) +
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
      labs(fill="", title=paste(input$Country, ": Never Users who Intend to Use, ", mean(never_pie$StartYear), sep="")) +
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
      labs(x="", y="", fill="", title=paste(input$Country, ": Former Users Population Distribution", sep="")) +
      coord_flip() +
      scale_fill_manual(values=c( "Not Married, Sexually Active"= "#0B9444",  "Not Married, Not Sexually Active" = "#8DC645",  "Infecund, Menopausal"= "#954F72",  "Married, Wants Child within 2 Years"= "#1D4E92",   "Married, Does Not Want Child within 2 Years"= "#3892C6")) +
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
      labs(x="", y="", fill="", title=paste(input$Country, ", ",  mean(former_intention_df$StartYear), ": Former Users\nIntention to Use in Future", sep="")) +
      scale_fill_manual(values=c("Does Not Intend" = "#3892C6",  "Unsure About Use"= "#FCD116", "Use Later"= "#0B9444")) +
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
      labs(fill="", title=paste(input$Country, ": Former Users who Intend to Use, ", mean(pie_former$StartYear), sep="")) +
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
      labs(fill="", title=paste(input$Country, ": Why Did Former Users Discontinue, ", mean(pie_formerdis$StartYear), sep="")) +
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
      labs(x="", y="Percent of Former Users", fill="", title=paste(input$Country, ", ",  mean(former_time_use_group_df$StartYear), ": When Did Former Users Last Use Family Planning", sep="")) +
      scale_fill_manual(values=c("Under a Year" = "#F49100" , "1 Year" = "#FCD116", "2 Year"= "#8DC645", "3 Year"= "#0B9444",  "4 Year"= "#3892C6", "5+ Years"= "#1D4E92")) +
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
      labs(x="", y="", fill="", title=paste(input$Country, ": Current Users Population Distribution", sep="")) +
      coord_flip() +
      scale_fill_manual(values=c( "Never Married"= "#8DC645",  "Formerly Married"= "#FCD116",  "Married"= "#3892C6")) +
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
      labs(x="", y="", fill="", title=paste(input$Country, ": Current Users Method Mix", sep="")) +
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
      labs(x="", y="Percent", fill="", color="", shape="", title=paste(input$Country, ", ",   mean(mii_df$StartYear), ": Method Information Index", sep="")) +
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
      labs(x="", y="", fill="", title=paste(input$Country, ": Current Users Method Source", sep="")) +
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
    current_timesince_sel <- current_timesince_Data()  %>% filter(Country %in% input$Country)  %>%
      mutate(Var1 = fct_relevel(Var1, c("Under a Year", "1 Year", "2 Year", "3 Year", "4 Year", "5+ Years" )))   	
    current_timesince_sel	 
  })
  
  current_timesince_plot<-reactive({
    
    current_timesince_df <- current_timesince_country()
    
    current_timesince_p <-  ggplot(current_timesince_df, aes(x=Var1, y=Freq*100, fill=Var1)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=paste(round(Freq*100), "%", sep="")), vjust=-.5, size=6) +
      labs(x="", y="Percent of Current Users", fill="", title=paste(input$Country, ", ",   mean(current_timesince_df$StartYear), ":\nWhen Did Current Users Start Using Current Method", sep="")) +
      scale_fill_manual(values=c("Under a Year" = "#F49100" , "1 Year" = "#FCD116", "2 Year"= "#8DC645", "3 Year"= "#0B9444",  "4 Year"= "#3892C6", "5+ Years"= "#1D4E92")) +
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
    if ( "What proportion of never users may have a need for contraception right now?" %in% input$neverselect) return( never_group_plot())
    if ( "What proportion of never users intend to use in the future?" %in% input$neverselect) return( never_intention_plot())
    if ( "What is the distribution of never users who intend to use in the future?" %in% input$neverselect) return(never_has_intention_plot())
  })
  
  output$neverplot <- renderPlot({   
    print(neverplotchoice())
  } , height = 600 , width = 800)
  
  
  nevertextchoice <- reactive({
    if ( "What proportion of never users may have a need for contraception right now?" %in% input$neverselect) return( "Married women who do not want a child in the next two years and unmarried, sexually active women may have a need for contraception to avoid pregnancy.  Women who are not married and not sexually active and those who are married and want a child soon may not need contraception now but may benefit from family planning information.  Infecund or menopausal women who have never used are unlikely to become users in the future.")
    if ( "What proportion of never users intend to use in the future?" %in% input$neverselect) return("For each group, we can look at who needs programming (those who intend to use in the future) versus who needs information about the benefits of family planning (those who are unsure or do not intend to use in the future).  If a large proportion of not married women (who are predominately young women) do not intend to use in the future, expanded outreach to youth maybe needed." )
    if ( "What is the distribution of never users who intend to use in the future?" %in% input$neverselect) return("Programmatically, it is beneficial to know which population is largest among those who intended to use in the future, especially if that group has a current unmet need.")
  })
  
  output$never_text <- renderText({ 
    nevertextchoice()
  })
  
  

  formerplotchoice <- reactive({
    if ( "What proportion of former users may have a need for contraception right now?" %in% input$formerselect) return( former_group_plot())
    if ( "Why did former users stop using family planning?" %in% input$formerselect) return( former_whydispie_plot())
    if ( "When did former users stop using family planning?" %in% input$formerselect) return( former_time_use_group_plot())
    if ( "What proportion of former users intend to use in the future?" %in% input$formerselect) return( former_intention_plot())
    if ( "What is the distribution of former users who intend to use in the future?" %in% input$formerselect) return( former_has_intention_plot())
  })
  
  output$formerplot <- renderPlot({   
    print(formerplotchoice())
  } , height = 600 , width = 800)
  
  formertextchoice <- reactive({
    if ( "What proportion of former users may have a need for contraception right now?" %in% input$formerselect) return( "Married women who do not want a child in the next two years and unmarried, sexually active women may have a need for contraception to avoid pregnancy.  Women who are not married and not sexually active and those who are married and want a child soon may not need contraception now but may want to become family planning users again in the future.  Infecund or menopausal women who have formerly used are unlikely to become users in the future.")
    if ( "Why did former users stop using family planning?" %in% input$formerselect) return("In a program focused on spacing, we expect many women to stop using a contraceptive method because they want to become pregnant.  Women with decreased risk of pregnancy (infrequent sex, marital dissolution, and menopause) are also likely to stop using a method.  If a large share of women discontinues because they pregnant while using a method, this suggests high rates of method failure.  If a large proportion of former users stop using due to side effects (and do not start a new method), it suggests a limited access to wide arrange of methods and information.")
    if ( "When did former users stop using family planning?" %in% input$formerselect) return( "If most former users stopped using recently, it may suggest they stopped using to become pregnant.  A concentration of former users who stopped using 5+ years ago suggests many former users may assume they have a decreased risk of unplanned pregnancy or want to become pregnant.  One concern with a large proportion of former users who have not used in the last 5 years is that they had a negative experience with family planning programs.")
    if ( "What proportion of former users intend to use in the future?" %in% input$formerselect) return( "For each group, we can look at who needs programming (those who intend to use in the future) versus who needs information about the benefits of family planning (those who are unsure or do not intend to use in the future).")
    if ( "What is the distribution of former users who intend to use in the future?" %in% input$formerselect) return("Programmatically, it is beneficial to know which population is largest among those who intended to use in the future, especially if that group has a current unmet need.")
  })
  
  output$former_text <- renderText({ 
    formertextchoice()
  })
  
  

  currentplotchoice <- reactive({
    if ( "What is the distribution of current users?" %in% input$currentselect) return( current_group_plot())
    if (  "What methods are most popular?" %in% input$currentselect) return( current_methodmix_plot())
    if ( "How long have women used their method?" %in% input$currentselect) return( current_timesince_plot())
    if ( "Where are they accessing methods?" %in% input$currentselect) return( current_source_plot())
    if ( "Are they informed about other methods and side effects?" %in% input$currentselect) return( mii_plot())
  })
  
  output$currentplot <- renderPlot({   
    print(currentplotchoice())
  } , height = 600 , width = 800)
  
  
  currenttextchoice <- reactive({
    if ( "What is the distribution of current users?" %in% input$currentselect) return("Are married and unmarried women current users?  If two or more surveys are available, has the distribution changed over time?" )
    if (  "What methods are most popular?" %in% input$currentselect) return( "Which contraceptive methods are most widely used?  Are they short-term or long-term methods?  If two or more surveys are available, has the distribution changed over time?" )
    if ( "How long have women used their method?" %in% input$currentselect) return( "In countries with dramatic growth in contraceptive use, we expect more women to be new users of methods.  Also, if there is a lot of method switching, we might expect women have used their current method for a shorter period.")
    if ( "Where are they accessing methods?" %in% input$currentselect) return("Are most users receiving their methods from a public or private source?  If two or more surveys are available, has the distribution changed over time?")
    if ( "Are they informed about other methods and side effects?" %in% input$currentselect) return("Are users receiving key information when starting a method?  Does this vary by method?")
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