
## load packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(Lahman)
library(tidyverse)



myBatting <- Batting %>% filter(yearID > 2012 & yearID < 2018) %>% filter(HR > 5 & AB > 200)

dashboardPage(skin="blue",
              
              ## title for app
              dashboardHeader(title = "Modeling Baseball Data with R", titleWidth = 700),
              ## create UI tabs
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "Tab1"),
                menuItem("Data Exploration", tabName = "Tab2"),
                menuItem("Modeling", tabName = "Tab3"),
                menuItem("Data", tabName = "Tab4")
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
                                     h4("This app will look at data from seasons 2013-2017 of Major League Baseball. The goal of analyzing this data is to focus on the number of home runs that are hit throughout the seasons. By analyzing this data, we hope to predict the number of home runs a player would likely hit based on given baseball statistics (NOTE: a description of these statistics I used can be found on the 'Data' page. Now, if you navigate through the tabs, you will be able to see numerical summaries and graphical displays, a modeling section with a predictions tab, and a simple data page where you will be able to filter the data and save it if you please."))),
                          
                          ## box 2
                          column(4,
                                 h1("The Data"),
                                 box(width = 12,
                                     h4("The data I used comes from the Lahman database. There are many different datasets within this database/package and I decided to use the Batting dataset. More information regarding the Lahman database and the Batting data set can be found at the links below."))),
                          
                          ## box 3
                          column(4,
                                 h1("Links"),
                                 box(width = 12,
                                     h4("The links below provide some information regarding the Lahman package in 
                                     R, the offcial MLB website, and Sean Lahman's personal wesbite for more 
                                        information regarding himself."),
                                     h4("Note that the information regarding the different regression 
                                        models was taken from our ST558 course notes."),
                                     h4(uiOutput("cranLink")),
                                     h4(uiOutput("mlbLink")),
                                     h4(uiOutput("seanLahmanLink")))),
                          
                          ## Add a baseball picture
                          #img(src = "julio-rodriguez.jpeg",align="left", height = "30%", width = "30%")
                          imageOutput(outputId = "MLBpicture")
                          #div(img(src="https://www.google.com/search?q=mlb+image&source=lnms&tbm=isch&sa=X&ved=2ahUKEwiG3IiXwZf5AhXgFlkFHUI4ArgQ_AUoAXoECAEQAw&biw=1423&bih=641&dpr=2#imgrc=iYMO1kZaUs2SwM", 
                          #  height = '500px'), style="text-align: center;")
                          
                  ),
                  
  ######################################## TAB 4: DATA PAGE #############################
                  
                  ## NOTE ##
                  ## i realize this tab should be at the bottom, however I built the app 
                  ##layout with a Data page before everything
                  ## because i thought it made more sense. But i ended up changing the tab name numbers around
                  tabItem(tabName = "Tab4",
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
                                 h4("Try selecting some variables of your choice! 
                                    You can choose as many as you'd like."),
                                 selectInput(inputId = "variables",
                                             label = "Choose variables to include 
                                             (cannot exlcude first 5 categorical variables)",
                                             choices = names(myBatting)[-c(1:5)], multiple = TRUE)
                          ),
                          
                          ## create a download button
                          downloadButton(outputId = "baseballData", label = "Download the data!"),
                          
                          ## data table output
                          dataTableOutput("dataOutput"),
                          h3("An Explanation of the Variables"),
                          
                          
                          ## explain variables
                          column(12,box(width = 10,
                                        h6("playerID: unique player ID number."),
                                        h6("yearID: year the data is from"),
                                        h6("stint: stint of time"),
                                        h6("teamID: abbreviation of team "),
                                        h6("lgID: abbreviation of league -- 
                                           AL = American League, NL = National League"),
                                        h6("G: number of games."),
                                        h6("AB: number of at bats"),
                                        h6("X2B: number of doubles"),
                                        h6("X3B: number of triples"),
                                        h6("RBI: number of Runs Batted In"),
                                        h6("SB: number of stolen bases"),
                                        h6("BB: number of walks"),
                                        h6("SO: number of strikeouts"),
                                        h6("IBB: number of intentional walks"),
                                        h6("HBP: number of times hit by pitch"),
                                        h6("SF: number of sacrifice flies"),
                                        h6("GIDP: number of grounded into double plays"),
                                        height = 450
                          )
                          )
                          
                          
                          ## end of tab2 ###
                  ),
                  
    ######################################## TAB 2: Data Exploration  ####################################
tabItem(tabName = "Tab2",
  fluidRow(
      box(width = 5,
      h4("You can filter the data by Year (yearID)"),
      selectInput(inputId = "playerDataSelect",
      label = "Note: defaults to all data, i.e. all years available",
                                            choices = c("All Years", 
                                                        "2013", 
                                                        "2014", 
                                                        "2015",  
                                                        "2016",
                                                        "2017"),
                                            multiple = FALSE
                                )
                            ),
                            
                            # Select Variables
                            box(width = 12,
                                h4("Select a variable to change the plots and summaries!"),
                                selectInput(inputId = "playerVars",
                                            label = "Choose variables to summarize:",
                                            choices = names(myBatting)[-c(1:5)], multiple = FALSE)
                            ),
                            
                            ## UI for summary stats 
                            column(width = 9,
                                   br(),
                                   h4("Numerical Summary for Chosen Statistic"),
                                   box(width = 15,
                                       skin="black",
                                       tableOutput("dataTable"))),
                            
                            ## UI for plot choice
                            box(width = 10,
                                h4("You can change the type of plots below"),
                                radioButtons(inputId = "plotChoice",
                                             label = "",
                                             choices = c("Box Plot", "Scatterplot", "Histogram"),
                                             selected = "Box Plot"),
                                plotOutput("trialPlots"))
                          )
                          
                  ), ###### end tab 2 #########
                  
                  tabItem((tabName = "Tab3"),
                          tabsetPanel(
                            tabPanel("Modeling Info",
                                     #fluidRow(
                                     h1(" Multiple Linear Regression"),
                                     box(width = 12,
                                         h4("Multiple Linear Regression (MLR) is another supervised learning method and is an extension of Simple Linear Regression (SLR). We can expand MLR in many ways. For example, one might want to include many more variables than you would in SLR while including higher order terms. The goal is to model a relationship between two or more predictor variables and a response variable. Below is an example of a MLR model with two predictor variables."),
                                         h4(withMathJax(helpText("$$Y_i=\\beta_0+\\beta_1x_{1i}+\\beta_2x_{2i}+\\beta_3x_{1i}x_{2i}  +...+ E_i$$"))),
                                         h4(strong("Benefits:")),
                                         h4("There are many benefits to MLR. For example, MLR is very simple to understand compared to other complex models. When looking at an output, we will be able to clearly undertsand the influence that interactions have on each other. Furthermore, because MLR is generally simple, efficiency/computation time is not usually compromised with these models. Also, we tend to have an easier time using predictions for MLR models."),
                                         h4(strong("Drawbacks:")),
                                         h4("While MLR has many advantages, there are some disadvantages that must be acknowledged. First, there are many assumptions that need to be met/assumed. Also, MLR is looking at linear relationships between the response and predictor variables, which may not be optimal all of the time. For example, there may be instaces where our variables could be better fit by different methods and the MLR model fails to provide a good fit.")
                                         

                                     ), # end box 1
                                     h1("Regression Tree"),
                                     box(width = 12,
                                         h4("Regression Trees are one of the many tree-based methods that live in the world of supervised learning. We use a Regression Tree when our goal is to predict a continous response. This is different to its counterpart, Classification Trees where you want to classify(predict) group memebership. Futhermore, for a given region we usually use the mean of the observations as predictiosn. Now, the main process for Regression Trees goes something like this:"),
                                         h4("(1) Pick the splits by using recursive binary 
                                         splitting. But note that this is a very tedious and 'greedy' algorithm. 
                                            Then, for every possible value of each predictor we find the 
                                            Residual Sum of Squares and try to minimize it. Thus,"),
                                         h4(withMathJax(helpText("$$R_1(j,s) = {{x|x_j < s}} , R_2(j,s) = {{x|x_j \\ge s}}$$"))),
                                         h4("(2) Then, once your split is chosen, you repeat the 
                                            same process for the second split."),
                                         h4("(3) You then want to grow a large tree, i.e. a tree with many nodes"),
                                         h4("(4) Then you will prune the tree using 'cost-complexity pruning'. 
                                            This is done so that we do not overfit the data!"),
                                         h4(strong("Benefits:")),
                                         h4("Now, there are many benefits to using Regression Tree, or trees in general. First, they are very easy to understand and interpret the output. Plots in the form of tree plots can really help with this. Also, predictors do not need to be scaled, no statistical assumptions are necessary, and we have built-in variable selection."),
                                         h4(strong("Drawbacks:")),
                                         h4("However, we do experience some drawbacks when working with trees. For exmaple, small changes in our data at hand can have a big affect on the output/tree. And, unfortunately, the algorithms can be computationally intensive and we usually need to prune the trees")
                                         
                                     ),
                                     
                                     h1("Random Forest"),
                                     box(width = 12,
                                         h4("Finally, we will discuss Random Forests. This method actually uses some of the same ideas as Bagging which requires Bootstrapping. So what is Bootstrapping? This method is used throuough statistics and is when we either resample from the data (non-parametric method) or from a fitted model (parametric method)."),
                                         h4("Now, we will use Random Forest (and the other tree-based methods) when prediction is more important that interpretation. In fact, we will be able to average across the fitted trees and decrease the variance for an individual tree fit."),
                                         h4("Thus, for Random Forests, we will create multiple trees from bootstrapped samples and avergae the results. It is important to note that a kewy difference of the Random Forest method is that we do not use all of the predictors and we use a random subset of predictors for each bootstrap sample."),
                                         h4("Now, for predictors, we are going to usually use this 
                                            method for Classification Trees:"),
                                         h4(withMathJax(helpText("$$ m = \\sqrt{p}$$"))),
                                         h4("And use this method for Regression Trees:"),
                                         h4(withMathJax(helpText("$$ m = p/3$$"))),
                                         h4("Also to note, if we have:"),
                                         h4(withMathJax(helpText("$$ m = p$$"))),
                                         h4("then we have bagging."),
                                         h4(strong("Benefits:")),
                                         h4("Now, as mentioned above, the greatest aspect of Random Forests 
                                            is that we greeatly improve on prediction."),
                                         h4(strong("Drawbacks:")),
                                         h4("On the other hand, we lose intepretability when using 
                                            Random Forest models.")
                                         
                                     )),
                            ## start model fitting tab 
                            tabPanel("Model Fitting",
                                     fluidRow(
                                       column(width = 4,
                                              box(width = 12, #background = "blue",
                                                  sliderInput("splitSize", "What percent of the 
                                                              training/test set do you want?",
                                                              min = 0.10, max = 1.0, value = 0.80, step = 0.10))),
                                       
                                       box(width = 12, h3("The Response Variable"),
                                           h4("NOTE: the default response variable is Home Runs, however, 
                               the user will be able to pick which predictor variables they want to use."),
                                           h5("This reponse variable of Home Runs will be used across all three models.")),
                                       ## select response variable
                                       box(width = 12,
                                           selectInput(inputId = 'respVar',
                                                       label = "Select response variable",
                                                       choices = c("HR")
                                           )
                                       ),
                                       ## select predictor variables
                                       box(width = 12,
                                           selectInput(inputId = 'predVars',
                                                       label = "Please select variables for all models (multiple choices):",
                                                       choices = c("G", "AB","X2B","X3B","RBI","SB","BB","SO","IBB",
                                                                   "HBP","SF", "GIDP"), multiple = TRUE
                                           ),
                                           h6("The slections defualt to all variables. 
                                           Note that this may take longer tobuild models opposed to 
                                              using fewer variables.")
                                       ),
                                       
                                       ## select CV folds!
                                       box(width = 10,
                                           h4("All models are run using Repeated Cross Validation. 
                                           You are able to customize how many folds you prefer when building 
                                              your model."),
                                           sliderInput("cvFold", "Select Number of Fold for Cross Validation:",
                                                       min = 1, max = 10, value = 5, step = 1)
                                       ),
                                       
                                       ## create the run button
                                       box(width = 12,
                                           h3("Click the button to run all three models!"),
                                           actionButton("runModelButton", "Click Here!"),
                                       ),
                                       #column(width = 9,
                                       # br(),
                                       
                                       
                                       ## create UI outputs for each model summary!
                                       box(width = 12,
                                           column(10,
                                                  h4(strong("Summary for Multiple Linear Regression")),
                                                  verbatimTextOutput("mlrSummary"))),
                                       box(width = 12,
                                           column(10,
                                                  h4(strong("Summary for Regression Tree")),
                                                  verbatimTextOutput("regTreeSummary"))),
                                       box(width = 12,
                                           column(10,
                                                  h4(strong("Summary for Random Forest")),
                                                  verbatimTextOutput("rfSummary"))
                                       ),
                                       
                                       
                                       column(9, 
                                              box(width = 12,
                                                  h3("RMSE on Training Data"),
                                                  h5("Below, you will find the minimum RMSE output f
                                                  or each model that you built. 
                             The minimum was taken for each respective model RMSE output and the 
                             best model will be dtermined by the lowest RMSE of these three below. "),
                                                  ),
                                              box(width = 12, solidHeader = TRUE,
                                                  title = "Training Data Outputs",
                                                  h4(strong("Multiple Linear Regression")),
                                                  verbatimTextOutput("mlrRMSEoutput"),
                                                  h4(strong("Regression Tree")),
                                                  verbatimTextOutput("regTreeRMSEoutput"),
                                                  h4(strong("Random Forest")),
                                                  verbatimTextOutput("rndmForestRMSEoutput")))
                                       
                                       
                                     )),
                            ##### prediction tab #######
                            tabPanel("Prediction",
                                     fluidRow(
                                       column(4,
                                              br(),
                                              
                                              ## select model for prediction
                                              box(width = 12,
                                                  h4("Click a button to chose a model!"),
                                                  radioButtons(inputId = "chooseModel",
                                                               label = "",
                                                               choices = c("Multiple Linear Regression", 
                                                                           "Regression Tree", 
                                                                           "Random Forest"),
                                                               selected = "Multiple Linear Regression")
                                              ),
                                              ## create another button for predictions
                                              box(width = 12,
                                                  h4("Click the button to predict!"),
                                                  actionButton("predButton", "Click Here!!")
                                              ),
                                              ## now we want to set up our variables for prediction
                                              ## note the ranges for each variable
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
                                             ### and the number is..... 
                                              column(width = 9,
                                                     h3("The predicted number of Home Runs is"),
                                                     verbatimTextOutput("HRpredictions")))
                                     ))
                            
                          ))
                  
                  
                  
                )))## top parens
