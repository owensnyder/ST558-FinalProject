
## load packages
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(Lahman)

dashboardPage(skin = "blue",

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
      choices = c("All Players", "Aaron Judge", "Jose Altuve", 
                  "Juan Soto",  "Mookie Betts")
)),


## select some variables
column(5,
  h4("Try selecting some variables of your choice! (multiple choices allowed)"),
    selectInput(inputId = "variables",
    label = "Choose variables to include (can't ignore first 5 categorical variables)",
      choices = names(Batting)[-c(1:5)], multiple = TRUE)
),

## create a download button
downloadButton(outputId = "baseballData", label = "Download the data!"),

# Show the data frame
dataTableOutput("dataOutput"),
  h3("Breif explanation of variables"),


# Provide variables explanation
column(8,box(width = 50,
  h6("playerID: unique player ID number."),
  h6("yearID: year the data is from"),
  h6("stint: stint of time"),
  h6("teamID: abbreviation of team "),
  h6("lgID: abbreviation of league -- AL = American League, NL = National League"),
  h6("G: number of games."),
  h6("AB: number of at bats"), height = 300
)
),


## end of tab2
),

## third tab: data exploration

## need to filter data for each summary and plot though...



tabItem(tabName = "Tab3",
fluidRow(
  #plotOutput(
box(width = 10,
    h4("You can change the type of plots below:"),
    radioButtons(inputId = "plotChoice",
    label = "",
    choices = c("Box Plot", "Scaterplot"),
    selected = "Box Plot"),
    plotOutput("trialPlots")
)
  
)
#)
)



                  )))