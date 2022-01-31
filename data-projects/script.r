# ============================
# 
#  DARES MEETING DATA
#
# =============================
# Authors: AIDIR Anas, THIERY Valentin

# ---- SOFTWARE MANAGEMENT ----
# Install the new packages
## (UNCOMENT TO INSTALL ON THE FIRST RUN)

# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("data.table")
# install.packages("dyplr")
# install.packages("tidyr")

#.........................
# Updates existing libraries
#.........................
# Change to TRUE if you want to update
#update.packages(ask = FALSE)

# Load the libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(dplyr)
library(tidyr)

# ---- DATA LOAD ----
# Load the data
## (PLEASE PUT THE INITIAL DATA FILES (3 files) IN THE SAME FOLDER AS THIS SCRIPT)
load("Taxonomy.RData")
ojv1 <- get(load("OJV level 1.RData"))
ojv2 <- get(load("OJV level 2.RData"))
str(ojv1)
str(ojv2)
str(taxo)

# ===== DATA PREP =====
ojv1 <- as.data.table(ojv1)
colnames(ojv1) <- c("country_OJV1", "skill_1", "share_1", "year_OJV1")
ojv2 <- as.data.table(ojv2)
colnames(ojv2) <- c("country_OJV2", "skill_2", "share_2", "year_OJV2")
ojv1 %>% as.data.frame()
ojv2 %>% as.data.frame()
taxo <- as.data.table(taxo)
taxo %>% as.data.frame()

## Test 1
# Getting full OJV1 with a merge of taxo

OJV1_1 <- merge(taxo, ojv1,
                by.x = "skill1",
                by.y = "skill_1",
                all.x = TRUE, all.y = TRUE, allow.cartesian=TRUE
)
## GOOOOOD !!!!

# Getting full OJV2 with a merge of taxo

OJV2_1 <- merge(taxo, ojv2,
                by.x = "skill2",
                by.y = "skill_2",
                all.x = TRUE, all.y = TRUE, allow.cartesian=TRUE
)
## GOOOOOD !!!!

data <- merge(OJV1_1, OJV2_1,
              by.x = c("skill1","skill2","country_OJV1","year_OJV1"),
              by.y = c("skill1","skill2","country_OJV2","year_OJV2"),
              all.x = TRUE, all.y = TRUE, allow.cartesian=TRUE
)
# Data is now the full table

# ===== SHINY APP =====
# ---- DASHBOARD VISUAL ----
## 1 ## Define the menu content

menu <- dashboardSidebar(
  # What's inside the sidebar
  sidebarMenu(
    menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
    menuItem("Market evolution", tabName = "#", icon = icon("chart-line"), startExpanded = TRUE,
             menuSubItem("Evolution of skills (1st level)", tabName = "page1P1", icon = icon("chevron-right")),
             menuSubItem("Evolution of skills (2nd level)", tabName = "page1P2", icon = icon("chevron-right"))
    ),
    menuItem("Shares per country & year", tabName = "#", icon = icon("chart-bar"), startExpanded = TRUE,
             menuSubItem("Skills shares (1st level)", tabName = "page2P1", icon = icon("chevron-right")),
             menuSubItem("Skills shares (2nd level)", tabName = "page2P2", icon = icon("chevron-right"))
    ),
    menuItem("Range by Country & Year", tabName = "page3", icon = icon("exchange-alt"), startExpanded = TRUE,
             menuSubItem("Range (1st level)", tabName = "page3P1", icon = icon("chevron-right")),
             menuSubItem("Range (2nd level)", tabName = "page3P2", icon = icon("chevron-right"))
             ),
    menuItem("In or out? A scope", tabName = "page4", icon = icon("layer-group"), startExpanded = TRUE,
             menuSubItem("A scope (1st level)", tabName = "page4P1", icon = icon("chevron-right")),
             menuSubItem("A scope (2nd level)", tabName = "page4P2", icon = icon("chevron-right"))
             ),
    menuItem("Going further", tabName = "page5", icon = icon("lightbulb"))
    # Name of the tab, ID of the tab matching body content later, icon that needs to appear
  )
)

## 2 ## Fix and write all the visual contents and interactions of the page body

body <- dashboardBody(
  tabItems(
    # PAGE INTRODUCTION
    tabItem(tabName = "intro",
            h2("Introduction to the Dares dashboard"),
            column(width=4,
                   # A static valueBox
                   valueBox(4, "Countries", width=NULL, icon = icon("globe-europe"), color = "navy"),
                   # A static valueBox
                   valueBox(3, "Years", width=NULL, icon = icon("calendar-alt"), color = "orange"),
                   # A static valueBox
                   valueBox("An infinite", "number of possibilities", width=NULL, color = "black"),
                   # A static valueBox
                   valueBox("The future", "of European citizens", width=NULL, icon = icon("euro-sign"), color = "purple"),
            ),
            column(width=8,
                   box(
                     title = "The context", width = NULL, background = "purple",
                     HTML("You work as a data analyst at the Direction de l'Animation de la recherche, des Études et des Statistiques (DARES) of the French Ministry of Labour.<br>
                     <br>
                     Next week, Élisabeth Borne, the French Minister of Labour, has a meeting with the Ministers of Labour of Belgium, Luxembourg, and Germany.<br>
                     They will talk about developing a new strategy on training, reskilling, and upskilling for the unemployed who lost their jobs because of the COVID-19 crisis.<br>
                     <br>
                     Your chief asks you to prepare a dashboard for Élisabeth Borne allowing to analyze skills requirements on the labour market in France, Belgium, Luxembourg, and Germany.
                      ")
                   ),
                   box(
                     title = "About the dataset", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("The dataset is a simulated database on skills requirement in online job vacancies.<br>
                          <br>
                          It covers <b>Belgium, France, Germany, and Luxembourg</b>.<br>
                          The vacancies have been web scrapped in <b>2019, 2020, and 2021</b>.<br>
                          <b>1st level</b> represents the core families of skills and <b>2nd level</b> is the children/relatives values.<br>
                          <br>
                          Composed of 3 files, he needs to be formated in order to get best results and sense for further processings.<br>
                          Aka THE GOAL OF THIS DASHBOARD.")
                   ) # End Box
            ), # End column
    ), # End tabItem intro
    
    ##### PAGE 1       
    
    # PAGE 1 P1
    tabItem(tabName = "page1P1",
            h2("Evolution of skills (1st level)"),
            column(width = 12,
                   box(
                     title = "About the graph", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("The main objective of this graph was to highlight the evolutionary trends of the countries under review over the last 3 years to see
                          if there are cross-border exchange mechanisms available to meet constantly changing labour requirements.<br>
                          <br>
                          This first graph allows us to observe the evolution by major family of skills, the share of importance of some skills according
                          to the different job offers proposed on the market.<br>
                          ")
                   ) # End box
            ), ## End column
            column(width = 12,
                   box(width = 2,
                       selectInput(inputId = "P1P1_Skill1",
                                   label = strong("Choose a Skill to analyse"),
                                   choices = unique(ojv1$skill_1),
                                   selected = "Baseline skills"
                       )
                   ), # End box
                   box(width = 10,
                       plotOutput(outputId = "P1P1") # output$P1P1
                   )
            ) ## End column
    ), # End tabItem page 1 P1
    # PAGE 1 P2
    tabItem(tabName = "page1P2",
            h2("Evolution of skills (2nd level)"),
            column(width = 12,
                   box(
                     title = "About the graph", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("The main objective of this graph was to highlight the evolutionary trends of the countries under review over the last 3 years to see
                          if there are cross-border exchange mechanisms available to meet constantly changing labour requirements.<br>
                          <br>
                          This second graph then shows the changes for the children of the core family of skills seen previously.<br>
                          This graph contains more detailed and specific skills.<br>
                          ")
                   ) # End box
            ), ## End column
            column(width = 12,
                   box(width = 2,
                       selectInput(inputId = "P1P2_Skill2",
                                   label = strong("Choose a Skill to analyse"),
                                   choices = unique(ojv2$skill_2),
                                   selected = "Baseline skills"
                       )
                   ), # End box
                   box(width = 10,
                       plotOutput(outputId = "P1P2") # output$P1P2
                   )
            ) ## End column
    ), # End tabItem page 1 P2
    
    ##### PAGE 2    
    
    # PAGE 2 P1
    tabItem(tabName = "page2P1",
            h2("Shares per country and year (1st level)"),
            column(width = 12,
                   box(
                     title = "About the graph", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("The aim of these histograms is to be able to compare, by year and by country, the categories of skills most frequently 
                     requested in ascending order according to the published offers.<br>
                     <br>
                     This first histogram therefore allows you to choose a year and a country and to observe the distribution of the most in-demand level 1 skills.<br>
                          ")
                   ) # End box
            ), ## End column
            column(width = 12,
                   box(width = 2,
                       selectInput(inputId = "P2P1_Year",
                                   label = strong("Choose a Year"),
                                   choices = unique(ojv1$year_OJV1),
                                   selected = ""
                       ),
                       selectInput(inputId = "P2P1_Country",
                                   label = strong("Choose a Country"),
                                   choices = unique(ojv1$country_OJV1),
                                   selected = ""
                       )
                   ), # End box
                   box(width = 10,
                       plotOutput(outputId = "P2P1") # output$P2P1
                   )
            ) ## End column
    ), # End tabItem page 2 P1
    # PAGE 2 P2
    tabItem(tabName = "page2P2",
            h2("Shares per country and year (2nd level)"),
            column(width = 12,
                   box(
                     title = "About the graph", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("The aim of these histograms is to be able to compare, by year and by country, the categories of skills most frequently 
                     requested in ascending order according to the published offers.<br>
                     <br>
                     This second histogram allows us to choose a year and a country and to visualise the distribution of level 2 skills that are most in demand.<br>
                          ")
                   ) # End box
            ), ## End column
            column(width = 12,
                   box(width = 2,
                       selectInput(inputId = "P2P2_Year",
                                   label = strong("Choose a Year"),
                                   choices = unique(ojv2$year_OJV2),
                                   selected = ""
                       ),
                       selectInput(inputId = "P2P2_Country",
                                   label = strong("Choose a Country"),
                                   choices = unique(ojv2$country_OJV2),
                                   selected = ""
                       )
                   ), # End box
                   box(width = 10,
                       plotOutput(outputId = "P2P2") # output$P2P2
                   )
            ) ## End column
    ), # End tabItem page 2 P2
    
## PAGE 3    
    
    # PAGE 3 P1
    tabItem(tabName = "page3P1",
            h2("Range by Country and Year (1st level)"),
            column(width = 12,
                   box(
                     title = "About the graph", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("The charts are the same as the previous 'Shares per country & year' tab.<br>
                          This version simply allows you to add granularity to see the selections more precisely.<br>
                          <br>
                          It is difficult to read the values in the case of Skills Level 2.<br>
                          Thanks to the selector, it is possible to make appear values included in an interval to obtain a better visibility.<br>
                          ")
                   ) # End box
            ), ## End column
            column(width = 12,
                   box(width = 2,
                       selectInput(inputId = "P3P1_Year",
                                   label = strong("Choose a Year"),
                                   choices = unique(ojv2$year_OJV2),
                                   selected = ""
                       ),
                       selectInput(inputId = "P3P1_Country",
                                   label = strong("Choose a Country"),
                                   choices = unique(ojv2$country_OJV2),
                                   selected = ""
                       ),
                       sliderInput("P3P1_Range", "Range the % of share:",
                                   min = 0, max = 100, value = c(0,100))
                   ), # End box
                   box(width = 10,
                       plotOutput(outputId = "P3P1") # output$P3P1
                   )
            )
    ), # End tabItem page 3 P1
    # PAGE 3 P2
    tabItem(tabName = "page3P2",
            h2("Range by Country and Year (2nd level)"),
            column(width = 12,
                  box(
                    title = "About the graph", width = NULL, solidHeader = TRUE, status = "warning",
                    HTML("The charts are the same as the previous 'Shares per country & year' tab.<br>
                          This version simply allows you to add granularity to see the selections more precisely.<br>
                          <br>
                          It is difficult to read the values in the case of Skills Level 2.<br>
                          Thanks to the selector, it is possible to make appear values included in an interval to obtain a better visibility.<br>
                          ")
                  ) # End box
            ), ## End column
            column(width = 12,
                  box(width = 2,
                      selectInput(inputId = "P3P2_Year",
                                  label = strong("Choose a Year"),
                                  choices = unique(ojv2$year_OJV2),
                                  selected = ""
                      ),
                      selectInput(inputId = "P3P2_Country",
                                  label = strong("Choose a Country"),
                                  choices = unique(ojv2$country_OJV2),
                                  selected = ""
                      ),
                      sliderInput("P3P2_Range", "Range the % of share:",
                                  min = 0, max = 100, value = c(0,100))
                      ), # End box
                  box(width = 10,
                      plotOutput(outputId = "P3P2") # output$P3P2
                  )
            )
    ), # End tabItem page 3 P2

##### PAGE 4    
    
    # PAGE 4 P1
    tabItem(tabName = "page4P1",
            h2("In or Out? A scope (1st level)"),
            column(width = 12,
                   box(
                     title = "About the graph", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("This last graph is intended for an annualised cross-country comparison of skills beyond a certain demand threshold.<br>
                          This allows us to identify the skills that are most commonly required at the European level (here 4 countries, so part of Europe).<br>
                          <br>
                          The scaling is useful here to define the specific demand threshold we want to observe.<br>
                          <br>
                          In the case where all countries have a demand in this segment, we can deduce that the skill is not solely specific to a local market trend but rather a common trait.<br>
                          ")
                   ) # End box
            ), ## End column
            column(width = 12,
                   box(width = 2,
                       selectInput(inputId = "P4P1_Year",
                                   label = strong("Choose a Year"),
                                   choices = unique(ojv1$year_OJV1),
                                   selected = ""
                       ),
                       sliderInput("P4P1_Range", "Range the % of share:",
                                   min = 0, max = 100, value = c(0,100))
                   ), # End box
                   box(width = 10,
                       plotOutput(outputId = "P4P1") # output$P4P1
                   )
            ) ## End column
    ), # End tabItem page 4 P1
    # PAGE 4 P2
    tabItem(tabName = "page4P2",
            h2("In or Out? A scope (2nd level)"),
            column(width = 12,
                   box(
                     title = "About the graph", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("This last graph is intended for an annualised cross-country comparison of skills beyond a certain demand threshold.<br>
                          This allows us to identify the skills that are most commonly required at the European level (here 4 countries, so part of Europe).<br>
                          <br>
                          The scaling is useful here to define the specific demand threshold we want to observe.<br>
                          <br>
                          In the case where all countries have a demand in this segment, we can deduce that the skill is not solely specific to a local market trend but rather a common trait.<br>
                          ")
                   ) # End box
            ), ## End column
            column(width = 12,
                   box(width = 2,
                       selectInput(inputId = "P4P2_Year",
                                   label = strong("Choose a Year"),
                                   choices = unique(ojv2$year_OJV2),
                                   selected = ""
                       ),
                       sliderInput("P4P2_Range", "Range the % of share:",
                                   min = 0, max = 100, value = c(0,100))
                   ), # End box
                   box(width = 10,
                       plotOutput(outputId = "P4P2") # output$P4P2
                   )
            ) ## End column
    ), # End tabItem page 4 P2
    
    
    # PAGE 5
    tabItem(tabName = "page5",
            h2("Going further"),
            column(width = 12,
                   box(
                     title = "What to do next?", width = NULL, solidHeader = TRUE, status = "warning",
                     HTML("To go further, we could consider an improvement of the system's capabilities :<br><br>
                          <b>- To improve the readability of the links between skills 1 and skills 2.</b><br>
                          By grouping the 2 fields and unifying them, it would be easier to understand/observe when skill 1 families are more present than others.<br>
                          EX :<br>
                          Baseline Skills - Basic digital skills;<br>
                          Baseline Skills - Basic literacy skills;<br>
                          Etc<br>
                          <br>
                          In particular in the case of an observation of shares on Skills 2. That could be useful to rank the skills/skills families.<br><br>
                          <b>- To improve the visualisation, it could be interesting to split the graphs into bars according to 4 colours for each share level.</b><br>
                          Imagine :<br>
                          - 0 > 1st quartile = one color,<br>
                          - 1st quartile > 2nd quartile = another color,<br>
                          - etc<br>
                          This would make it easier to see the importance of different families in the rendered graphs (case of the 'Shares per country & year').
                          <br>
                          <br>
                          <b>- To improve lisibility of the data points especially on the 1st section of graphs.</b><br>
                          Being able to evaluate the Geom_Point() values on each lines via a Package called GGPLOTY to make them interactive on hover.<br>
                          <br>
                          
                          For a time concern and due to the workload constraints (internship, etc) we didn't went further into thoses aspects in this version.<br>
                          <br>
                          We spent already a loooooot of time on this work. (I would say around 28/30 hours of work)<br>
                          <br>
                          Shiny isn't that complicated, but it takes time to fully understand how it works and to create plots that can be adapted according to imput values, fixing all the small details, adapting visuals, readability, etc<br>
                          <br>
                          We hope, you've enjoyed it.<br>
                          Regards<br>
                          ")
                   ) # End box
            ) ## End column
    ) # End tabItem page 5
  ) ## End tabItemS
) # End Body

## 3 ## Group the visual parts together and name it

ui <- dashboardPage(skin = "purple", # Color of the dashboard
                    dashboardHeader(title = "Skills in a pandemic", # Title
                                    dropdownMenu(type = "message",
                                                 messageItem(
                                                   from = "Valentin THIERY",
                                                   message = "Student MSc DSOB - BSB"
                                                 ),
                                                 messageItem(
                                                   from = "Anas AIDIR",
                                                   message = "Student MSc DSOB - BSB"
                                                 ),
                                                 messageItem(
                                                   from = "Help",
                                                   message = "Internet is key.",
                                                   icon = icon("life-ring")
                                                 )
                                    ),
                                    dropdownMenu(type = "notification",
                                                 notificationItem(
                                                   text = "Due for 25th April 2021",
                                                   icon = icon("exclamation-triangle"),
                                                   status = "warning"
                                                 ),
                                                 notificationItem(
                                                   text = "Done!",
                                                   icon("truck"),
                                                   status = "success"
                                                 )
                                    )
                    ),
                    menu, # Sidebar
                    body # Tabs
)
# Now let's calculate the functions
# ---- DASHBOARD FUNCTIONS ----
server <- function(input, output) {
  
  ## TAB PAGE1
  
  output$P1P1 <- renderPlot({
    
    data_P1plot1 <- ojv1 %>% filter(ojv1$skill_1 == input$P1P1_Skill1) # Store the recomputed variable for rendering later with GGPLOT
    ggplot(data_P1plot1, aes(x = year_OJV1, y = share_1, group = country_OJV1, color = country_OJV1)) + geom_point() + geom_line() + labs(title = paste("Market evolution for Skill (Level 1) - ", input$P1P1_Skill1), x = "Year", y = "Share (%)", color="Countries") + scale_y_continuous(breaks = seq(0, 100, 2)) + theme_light()
    
  })
  
  output$P1P2 <- renderPlot({
    
    data_P1plot2 <- ojv2 %>% filter(ojv2$skill_2 == input$P1P2_Skill2) # Store the recomputed variable for rendering later with GGPLOT
    ggplot(data_P1plot2, aes(x = year_OJV2, y = share_2, group = country_OJV2, color = country_OJV2)) + geom_point() + geom_line() + labs(title = paste("Market evolution for Skill (Level 2) - ", input$P1P2_Skill2), x = "Year", y = "Share (%)", color="Countries") + scale_y_continuous(breaks = seq(0, 100, 2)) + theme_light()
    
  })
  
  ## TAB PAGE2
  
  output$P2P1 <- renderPlot({
    
    data_P2plot1 <- ojv1 %>% filter(ojv1$country_OJV1 == input$P2P1_Country & ojv1$year_OJV1 == input$P2P1_Year)
    data_P2plot1 <- within(data_P2plot1, 
                           Position <- factor(data_P2plot1$share_1, 
                                              levels=names(sort(table(share_1), 
                                                                increasing=TRUE))))
    ggplot(data_P2plot1, aes(x = reorder(skill_1, share_1), y = share_1)) + geom_bar(stat = "identity") + scale_y_continuous(breaks = seq(0, 100, 5)) + theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5)) + labs(title = paste("Skills shares per country & year (Level 1) - In ", input$P2P1_Country, " in ", input$P2P1_Year),x = "Skills",y = "Share (%)", color="Countries") + guides(fill=guide_legend(keyheight=0.9, default.unit="inch"))
    
  })
  
  output$P2P2 <- renderPlot({
    
    data_P2plot2 <- ojv2 %>% filter(ojv2$country_OJV2 == input$P2P2_Country & ojv2$year_OJV2 == input$P2P2_Year)
    data_P2plot2 <- within(data_P2plot2, 
                           Position <- factor(data_P2plot2$share_1, 
                                              levels=names(sort(table(share_2), 
                                                                increasing=TRUE))))
    ggplot(data_P2plot2, aes(x = reorder(skill_2, share_2), y = share_2)) + geom_bar(stat = "identity") + scale_y_continuous(breaks = seq(0, 100, 5)) + theme(axis.text.x=element_text(angle=70,hjust=0.5,vjust=0.5)) + labs(title = paste("Skills shares per country & year (Level 2) - In ", input$P2P2_Country, " in ", input$P2P2_Year),x = "Skills",y = "Share (%)", color="Countries") + guides(fill=guide_legend(keyheight=0.9, default.unit="inch"))
    
  })
  
  ## TAB PAGE3
  
  output$P3P1 <- renderPlot({
    
    data_P3plot1 <- ojv1 %>% filter(ojv1$country_OJV1 == input$P3P1_Country & ojv1$year_OJV1 == input$P3P1_Year & ojv1$share_1 >= input$P3P1_Range[1] & ojv1$share_1 <= input$P3P1_Range[2])
    data_P3plot1 <- within(data_P3plot1, 
                           Position <- factor(data_P3plot1$share_1, 
                                              levels=names(sort(table(share_1), 
                                                                increasing=TRUE))))
    ggplot(data_P3plot1, aes(x = reorder(skill_1, share_1), y = share_1)) + geom_bar(stat = "identity") + scale_y_continuous(breaks = seq(0, 100, 5)) + theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5)) + labs(title = paste("Range per country & year (Level 1) - In ", input$P3P1_Country, " in ", input$P3P1_Year),x = "Skills",y = "Share (%)", color="Countries") + guides(fill=guide_legend(keyheight=0.9, default.unit="inch"))
    
  })
  
  output$P3P2 <- renderPlot({
    
    data_P3plot2 <- ojv2 %>% filter(ojv2$country_OJV2 == input$P3P2_Country & ojv2$year_OJV2 == input$P3P2_Year & ojv2$share_2 >= input$P3P2_Range[1] & ojv2$share_2 <= input$P3P2_Range[2])
    data_P3plot2 <- within(data_P3plot2, 
                           Position <- factor(data_P3plot2$share_2, 
                                              levels=names(sort(table(share_2), 
                                                                increasing=TRUE))))
    ggplot(data_P3plot2, aes(x = reorder(skill_2, share_2), y = share_2)) + geom_bar(stat = "identity") + scale_y_continuous(breaks = seq(0, 100, 5)) + theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5)) + labs(title = paste("Range per country & year (Level 2) - In ", input$P3P2_Country, " in ", input$P3P2_Year),x = "Skills",y = "Share (%)", color="Countries") + guides(fill=guide_legend(keyheight=0.9, default.unit="inch"))
    
  })
  
  ## TAB PAGE4
  
  output$P4P1 <- renderPlot({
    
    data_P4plot1 <- ojv1 %>% filter(ojv1$share_1 >= input$P4P1_Range[1] & ojv1$share_1 <= input$P4P1_Range[2] & ojv1$year_OJV1 == input$P4P1_Year)
    data_P4plot1 <- within(data_P4plot1, Position <- factor(data_P4plot1$share_1, levels=names(sort(table(data_P4plot1$share_1), increasing=TRUE))))
    
    ggplot(data_P4plot1, aes(fill = data_P4plot1$country_OJV1, y = data_P4plot1$share_1, x = data_P4plot1$skill_1)) + geom_bar(position="stack", stat="identity") + scale_y_continuous(breaks = seq(0, 500, 10)) + theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5)) + labs(title = paste("Cumulated shares of the countries (Level 1) - In", input$P4P1_Year), x = "Skills", y = "Share per country (%)", fill = "Countries")
    
  })
  
  output$P4P2 <- renderPlot({
    
    data_P4plot2 <- ojv2 %>% filter(ojv2$share_2 >= input$P4P2_Range[1] & ojv2$share_2 <= input$P4P2_Range[2] & ojv2$year_OJV2 == input$P4P2_Year)
    data_P4plot2 <- within(data_P4plot2, Position <- factor(data_P4plot2$share_2, levels=names(sort(table(data_P4plot2$share_2), increasing=TRUE))))
    
    ggplot(data_P4plot2, aes(fill = data_P4plot2$country_OJV2, y = data_P4plot2$share_2, x = data_P4plot2$skill_2)) + geom_bar(position="stack", stat="identity") + scale_y_continuous(breaks = seq(0, 500, 10)) + theme(axis.text.x=element_text(angle=60,hjust=0.5,vjust=0.5)) + labs(title = paste("Cumulated shares of the countries (Level 2) - In", input$P4P2_Year), x = "Skills", y = "Share per country (%)", fill = "Countries")
    
  })
  
}

# ---- THE APP LAUNCHER ----
# Loading all the previous parts in the app

shinyApp(ui, server)

# ===== THE END =====
