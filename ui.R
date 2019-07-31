library(shiny)
library(tidyverse)
library(shinydashboard)
library(plotly)

#read the data
cancerData<-read_csv("C:\\Users\\X-Gong\\Documents\\ST558\\Project3\\Five Year Cancer Survival Rates in USA.csv",col_types = cols(
  `Survival Rate` = col_double(),
  Year = col_double(),
  Race = col_factor(),
  Gender = col_factor(),
  `Cancer Type` = col_factor()
))

shinyUI(dashboardPage(skin = "red",
              dashboardHeader(title = "Cancer in USA"),
              
              dashboardSidebar(sidebarMenu(
                  menuItem("About",tabName = "about"),
                  menuItem("Data exploration",tabName = "dataexp"),
                  menuItem("Modeling",tabName = "model"),
                  menuItem("PCA Analysis",tabName = "pca"),
                  menuItem("Data resources",tabName = "data")
              )),
              
              dashboardBody(
                  tabItems(
                      tabItem(tabName = "about",
                              fluidRow(
                                  h3("Five-year Cancer Survival Rates in America"),
                                  br(),
                                  br(),
                                  column(6,
                                     box(background = "red",width = 50,
                                         h5("Cancer survival rates or survival statistics tell you 
                                            the percentage of people who survive a certain type of 
                                            cancer for a specific amount of time. Cancer statistics 
                                            often use an overall five-year survival rate.")
                                         ),
                                     box(background = "red",width = 50,
                                         h5("We used five-year survival rates from the 1970s (1970-77) to 2007-2013 here. 
                                             Overall we see that survival rates from all cancers have increased from 50 to 67 percent. 
                                             This results from the combination of both early detection and improved treatment."),
                                         a("Link for more available information", href="https://ourworldindata.org/cancer")
                                         )
                                         
                                         ),
                                  column(6,
                                    box(background = "blue",width = 50,
                                        h5("This app invloves three modeling methods,which are PCA analysis,random forest and boosting.
                                           Regarding PCA analysis, it helps look for linear combinations of variables that accounts for as 
                                           much variability as possible. With respect to supervised learning models,random forest(extended
                                           version of bagging) inputs tree plots showing the classification of variables. In addition,boosting 
                                           simulated here for making predictions of `five-year survival rate` based on four predictors that 
                                           are year,race,gender and cancer type" )
                                          
                                          
                                    )
                                    
                                  )
                                     )
                              ),
                      
                      tabItem(tabName = "dataexp",
                              fluidRow(
                               h3("Common Numerical and Graphical Summaries"),
                               column(3,
                                     selectInput("Vars","Predictor Variables",choices = 
                                                   as.factor(colnames(cancerData)[-1]))
                               ),
                               tabsetPanel(
                                 tabPanel("Numerical Summaries",
                                          column(9,
                                            tableOutput("num")
                                            )
                                 ),
                                   
                                   #another tab
                                  tabPanel("Graphical Summaries",
                                            column(9,
                                                   plotlyOutput("hist"),
                                                   downloadButton("downloadPlot","Download Plot"),
                                                   downloadButton("downloadData","Download Data")
                                                   
                                                   
                                            )
                                            
                                   )
                                 
                                  
                               )
                              )
                      ),
                      #tabs for models
                      tabItem(tabName = "model",
                           fluidRow(
                           column(6,
                                 # numericInput(),
                                 # selectizeInput(),
                                 splitLayout(
                                   checkboxGroupInput("treeVar","Variables for tree", choices = list("Year","Gender","Race","Cancer Type"="Cancer.Type"),
                                                      selected =c("Year","Gender","Race","Cancer Type"="Cancer.Type")),
                                   sliderInput("bst","number of trees for boosting",min = 1000,max = 10000,
                                               value = 5000,step = 1000)
                                 )
                                  
                                 
                                 ),
                           column(12,
                                  tabsetPanel(
                                  tabPanel("Random Forest",
                                           plotOutput("rf")
                                                        ),
                                  tabPanel("Boosting",
                                           
                                           sliderInput("Year","Year",min=2019,max = 2029,value = 2019,step = 1),
                                           selectInput("Race","Race",choices = list("White","Black")),
                                           selectInput("Gender","Gender",choices = list("females","males")),
                                           selectInput("Cancer Type","Cancer Type",choices = levels(cancerData$`Cancer Type`)),
                                           
                                           box(background = "red",width = 50,
                                            uiOutput("bstVars"),
                                            br(),
                                            uiOutput("bstTrees"),
                                            br(),
                                            textOutput("bst")
                                           )
                                           
                                            
                                               )
                                  )
                             
                           )
                           )
                      ),
                      
                      #PCA analysis
                      tabItem("pca",
                              fluidRow(
                                h3("PCA analysis"),
                                column(9,
                                       checkboxGroupInput("variables","Variables to choose for PCA",selected=c("Year","Race"), choices = 
                                                            c("Year","Race","Gender","Cancer Type"="Cancer.Type")),
                                       #biplots
                                       plotOutput("biplot")
                                       
              
                                )
                              )
                        
                      ),
                      
                      #data resources
                      tabItem("data",
                              fluidRow(
                              h3("Data used for analysis"),
                              column(12,
                                downloadButton("download","Download Data"),
                                br(),
                                br(),
                                dataTableOutput("data")
                                 
                                )
                              )
                        
                      )
                      
                              
                              
                              )
                              
                         )
                     
                  
              )
)
   


