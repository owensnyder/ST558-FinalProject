
## load packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(Lahman)
library(tidyverse)



myBatting <- Batting %>% filter(yearID >= "2009") # & AB > 200)

dashboardPage(skin="blue",

## title for app
dashboardHeader(title = "Modeling Baseball Data with R", titleWidth = 700),
## create UI tabs
dashboardSidebar(sidebarMenu(
  menuItem("About", tabName = "Tab1"),
  menuItem("Data", tabName = "Tab2"),
  menuItem("Data Exploration and Summaries", tabName = "Tab3"),
  menuItem("Modeling", tabName = "Tab4")
              )),
## dashboardBody() to create each tab or body element 
dashboardBody(
tabItems(

## start Tab1 ##
tabItem(tabName = "Tab1",
  column(4,
  h1("Brief Introduction"),
## box 1
  box(width=12,
    h4("This app will look at some of the best and most popular Major League Baseball (MLB) 
       players during the 2021 season. If you navigate through the tabs, you will be able to 
       see numerical summaries and graphical displays, a modeling section, and a simple data 
       page where you will be able to filter the data and save it if you please. note: filtering data"))),

## box 2
  column(4,
    h1("The Data"),
    box(width = 12,
    h4("The data I used comes from the Lahman database. There are many different datasets within this 
       database/package and I decided to use the Batting dataset. More information regarding the 
       Lahman database and the Batting data set can be found at the links below."))),

## box 3
  column(4,
    h1("Links"),
    box(width = 12,
    h4("The links below provide some information regarding the Lahman package in R, 
       the offcial MLB website, and Sean Lahman's personal wesbite for more information 
       regarding himself"),
      h4(uiOutput("cranLink")),
      h4(uiOutput("mlbLink")),
      h4(uiOutput("seanLahmanLink")))),
                          
## Add a baseball picture
  div(img(src="https://www.google.com/search?q=mlb+image&source=lnms&tbm=isch&sa=X&ved=2ahUKEwiG3IiXwZf5AhXgFlkFHUI4ArgQ_AUoAXoECAEQAw&biw=1423&bih=641&dpr=2#imgrc=iYMO1kZaUs2SwM", 
        height = '500px'), style="text-align: center;")
                          
),
                  
#### start tab 2 ####
tabItem(tabName = "Tab2",

## choose a baseball players from the list
column(6,
  h4("You can subset the data by players below:"),
    selectInput(inputId = "players",
    label = "Choose a player or all the players to view",
      choices = c("All Players", 
                  "Giancarlo Stanton", 
                  "Jose Altuve", 
                  "Nelson Cruz",  
                  "Mookie Betts",
                  "Bryce Harper")
)),


## select some variables
column(5,
  h4("Try selecting some variables of your choice! (multiple choices allowed)"),
    selectInput(inputId = "variables",
    label = "Choose variables to include (can't ignore first 5 categorical variables)",
      choices = names(myBatting)[-c(1:5)], multiple = TRUE)
),

## create a download button
downloadButton(outputId = "baseballData", label = "Download the data!"),

# Show the data frame
dataTableOutput("dataOutput"),
  h3("Breif explanation of variables"),


# Provide variables explanation
column(8,box(width = 16,
  h6("playerID: unique player ID number."),
  h6("yearID: year the data is from"),
  h6("stint: stint of time"),
  h6("teamID: abbreviation of team "),
  h6("lgID: abbreviation of league -- AL = American League, NL = National League"),
  h6("G: number of games."),
  h6("AB: number of at bats"), height = 300
)
)
#,

## end of tab2
),

## third tab: data exploration

## need to filter data for each summary and plot though...



tabItem(tabName = "Tab3",
fluidRow(
    box(width = 5,
      h4("You can filter the data by popular players"),
      selectInput(inputId = "playerDataSelect",
                  label = "defualts to All data in given time frame (i.e the years I decided on)",
                  choices = c("All Players", 
                              "Giancarlo Stanton", 
                              "Jose Altuve", 
                              "Nelson Cruz",  
                              "Mookie Betts",
                              "Bryce Harper"),
                  multiple = FALSE
      )
  ),
  
  # Select Variables
  box(width = 12,
      h4("Select a variable to change the plots and summaries!"),
      selectInput(inputId = "playerVars",
                  label = "Choose variables to summarize:",
                  choices = names(myBatting)[-c(1:5)], multiple = FALSE
      )
  ),
  
  ## UI for summary stats 
  column(width = 9,
         br(),
         h4("Numerical Summary for Given Statistic:"),
         box(width = 15,
             skin="black",
             tableOutput("dataTable"))),
  
  ## UI for plot choice
    box(width = 10,
      h4("You can change the type of plots below:"),
      radioButtons(inputId = "plotChoice",
      label = "",
      choices = c("Box Plot", "Scaterplot"),
      selected = "Box Plot"),
        plotOutput("trialPlots"))




)

), ## end tab 3

tabItem((tabName = "Tab4"),
        tabsetPanel(
          tabPanel("Modeling Info",
                   #fluidRow(
                            h1("Linear Regression"),
                            box(width = 12,
                            h4("Linear regression accomplishes this by learning a model that best fits the 
                                 linear relationship between the predictor and response variables.
                                 The model is fit by minimizing the sum of squared residuals (difference between 
                                 the observed and predicted responses).")
                            
                            #h1("Regression Tree"),
                           # box(width = 12,
                              #  h4("text goes here"))
        
        ), # end box 1
        h1("Reg Tree"),
        box(width = 12,
            h4("text goes here")),
        
        h1("Random Forest"),
        box(width = 12,
            h4("text goes here"))
        ),
        ## start model fitting tab 
        tabPanel("Model Fitting",
                 fluidRow(
          column(width = 4,
                 box(width = 12, background = "aqua",
                     sliderInput("splitSize", "What percent of the training/test set do you want?",
                                 min = 0.10, max = 1.0, value = 0.80, step = 0.10))),
          
          box(width = 12, h3("The Response Variable"),
                            h4("NOTE: the default response variable is Home Runs, however, 
                               the user will be able to pick which predictor variables they want to use."),
              h5("This reponse variable of Home Runs will be used across all three models.")),
          
          box(width = 12,
              selectInput(inputId = 'respVar',
                          label = "Select response variable",
                          choices = c("HR")
              )
          ),
          
          box(width = 12,
              selectInput(inputId = 'predVars',
                          label = "Please select variables for all models (multiple choices):",
                          choices = c("G", "AB","X2B","X3B","RBI","SB","BB","SO","IBB",
                                      "HBP","SF", "GIDP"), multiple = TRUE
              ),
              h6("The slections defualt to all variables. Note that this may take longer to
                 build models opposed to using fewer variables.")
          ),
          ## create the run button
          box(width = 12,
              h3("Click the button to run all three models"),
              actionButton("runModelButton", "Click Here!"),
                 ),
         column(width = 9,
                br(),
                
                
                # Summary
                box(width = 12,
                    column(10,
                           strong(h4("Summary for Multiple Linear Regression")),
                           verbatimTextOutput("mlrSummary"))),
                    box(width = 12,
                    column(10,
                           strong(h4("Summary for Regression Tree")),
                           verbatimTextOutput("regTreeSummary"))),
                    box(width = 12,
                    column(10,
                           h4("Summary for Random Forest"),
                           verbatimTextOutput("rfSummary"))
                    ),
                box(width = 12,
                    column(10, h4("Fit Statistics: RMSE"),
                           tableOutput("RMSE")))
               # )

))),
tabPanel("Prediction",
         fluidRow(
           column(4, 
                  box(width = 12,
                      title = "Enter values for variables that will predict Home Runs",
                      numericInput(inputId = "predGames",
                                   label = "Number of Games (49-162)",
                                   value = 162,
                                   min = 49, max = 162),
                      numericInput(inputId = "predAB",
                                   label = "Number of At Bats (201-684)",
                                   value = 300,
                                   min = 201, max = 684),
                      numericInput(inputId = "predX2B",
                                   label = "Number of Doubles (5-56)",
                                   value = 25,
                                   min = 5, max = 56),
                      numericInput(inputId = "predX3B",
                                   label = "Number of Triples (0-15)",
                                   value = 3,
                                   min = 0, max = 15),
                      numericInput(inputId = "predRBI",
                                   label = "Number of RBIs (15-138)",
                                   value = 100,
                                   min = 15, max = 138),
                      numericInput(inputId = "predSB",
                                   label = "Number of Stolen Bases (0-62)",
                                   value = 25,
                                   min = 0, max = 62),
                      numericInput(inputId = "predBB",
                                   label = "Number of Walks (5-143)",
                                   value = 80,
                                   min = 5, max = 143),
                      numericInput(inputId = "predSO",
                                   label = "Number of Strikeouts (26-219)",
                                   value = 125,
                                   min = 26, max = 219),
                      numericInput(inputId = "predIBB",
                                   label = "Number of Intentional Walks (0-29)",
                                   value = 8,
                                   min = 0, max = 29),
                      numericInput(inputId = "predHBP",
                                   label = "Number of Hit by Pitch (0-30)",
                                   value = 15,
                                   min = 0, max = 30),
                      numericInput(inputId = "predSF",
                                   label = "Number of Sacrifice Flies (0-15)",
                                   value = 5,
                                   min = 0, max = 15),
                      numericInput(inputId = "predGIDP",
                                   label = "Number of Grounded into Double Plays (0-31)",
                                   value = 14,
                                   min = 0, max = 31)
                      ),
                  column(8,
                         box(width = 10, background = "blue",
                             h3("Prediction for Home Runs (HR)"),
                             tableOutput(outputId = "predValsTable")))
         ))

))))))## top parens 