library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyverse)
library(caret)
library(Lahman)
#library(dashboardthemes)



## THIS IS THE DATA!!!
myBatting <- Batting %>% filter(yearID > 2012 & yearID < 2018) %>% filter(HR > 5 & AB > 200)

shinyServer(function(input, output, session) {
  
######################################## TAB 1 -- About page ######################################
  
## create links
  output$cranLink <- renderUI({
    tagList("CRAN documentation regarding the Lahman package", a("can be found here", 
            href="https://cran.r-project.org/web/packages/Lahman/Lahman.pdf"))
  })
  output$mlbLink <- renderUI({
    tagList("Major League Baseball official website", a("can be found here", href="https://www.mlb.com/"))
  })
  output$seanLahmanLink <- renderUI({
    tagList("Sean Lahman's offical wesbite", a("can be found here", href="https://www.seanlahman.com/"))
  })
  
## output MLB image
output$MLBpicture <- renderImage({
   list(src = "../mlbpic.jpeg", width = "350", height = "300")
  }, deleteFile = FALSE)  

  
  
############################################## TAB 4 -- Data outputs ########################

## create player names
playerChoice <- reactive({
  switch(input$players,
         "All Players" = "All",
         "Giancarlo Stanton" = "stantmi03",
         "Jose Altuve" = "altuvjo01",
         "Nelson Cruz" =  "cruzne02",
         "Mookie Betts" = "bettsmo01",
         "Bryce Harper" = "harpebr03")
})

## full data tables
## these are just for the popular players that i figured someone would maybe want to see
output$dataOutput <- renderDataTable({
  findPlayer <- playerChoice()
  if (findPlayer == "All"){
    myBatting %>% select(playerID, yearID, stint, teamID, lgID, input$variables) %>%
      datatable()
  } else{
    myBatting %>%
      filter(playerID==findPlayer) %>% select(playerID, yearID, stint, teamID, lgID, input$variables) %>%
      datatable()
  }
})

#csvData <- renderDataTable({
 # findPlayer <- playerChoice()
 # if (findPlayer == "All"){
 #   myBatting %>% select(playerID, yearID, stint, teamID, lgID, input$variables) %>%
 #     datatable()
 # } else{
 #   myBatting %>%
 #     filter(playerID==findPlayer) %>% select(playerID, yearID, stint, teamID, lgID, input$variables) %>%
 #     datatable()
 # }
#})


#output$baseballData<- downloadHandler(
 # filename = "LahmanBaseballData.csv",
 # content = function(file) {
 ##   write.csv(myLahmanfile, row.names = FALSE)
 # }
#)


########################################## TAB 2 -- Data Exploration  #########################################

#tab3data <- reactive({
 # if ("G" %in% input$variables) return(batting$G)

 ## create a reactable new data set for plots and summaries 
## this will be for All Years, and years 2013-2017
subData <- reactive({
  if(input$playerDataSelect == "All Years"){
    return(myBatting %>% select(yearID,lgID, HR,input$playerVars))
    
  } else if (input$playerDataSelect=="2013"){
    otherYears1 <- myBatting %>%
      filter(yearID == "2013") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherYears1)
    
  } else if(input$playerDataSelect=="2014"){
    otherYears2 <- myBatting %>%
      filter(yearID == "2014") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherYears2)
    
  }else if(input$playerDataSelect=="2015"){
    otherYears3 <- myBatting %>%
      filter(yearID == "2015") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherYears3)
    
  }else if(input$playerDataSelect=="2016"){
    otherYears4 <- myBatting %>%
      filter(yearID == "2016") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherYears4)
    
  }else if(input$playerDataSelect=="2017"){
    otherYears5 <- myBatting %>%
      filter(yearID == "2017") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherYears5)
  }
})

## Create summary table
## stats include minimum, maximum, mean, and standard deviation
## these will change upon a click from the user!
output$dataTable <- renderTable({
  setStat <- quo(!!sym(input$playerVars))  
  summaryStat <- subData() %>%
    select(!!setStat) %>%
    summarise(Minimum = min(!!setStat),  Mean = mean(!!setStat, na.rm=TRUE),
            Maximum = max(!!setStat), StdDeviation = sd(!!setStat))
  summaryStat
})


#### putting download button code here ###
## need subData() to be built first
output$baseballData<- downloadHandler(
  filename = "LahmanBaseballData.csv",
  content = function(file) {
  write.csv(myBatting,file, row.names = FALSE)
 }
)

#output$table_tag3 <- reactive({
#  summary(!!sym(input$playerVars))
#})

## Create plots. Variables will be changed per the user's request. Some will stay fixed.
## boxplot
Outputplot <- reactive({
  if (input$plotChoice=="Box Plot"){
    plotCycle <- ggplot(data = subData(), aes(x = lgID, y = subData()[,input$playerVars], fill = lgID)) +
      geom_boxplot() + geom_jitter() + ggtitle("Boxplot of", input$playerVars) +
      labs(x = "League (AL/NL)", y = input$playerVars)
    ## scatterplot
  } else if (input$plotChoice=="Scatterplot") {
    plotCycle <- ggplot(data = subData(), aes(x = HR, y = subData()[,input$playerVars])) + 
      geom_point(aes(color = yearID)) + geom_smooth(method = "lm", col = "red") + 
      ggtitle("Scatterplot of HR and", input$playerVars) + 
      labs(x = "Home Runs", y = input$playerVars)
    ## histogram
  } else if (input$plotChoice=="Histogram"){
    plotCycle <- ggplot(data = subData(), aes(x = subData()[,input$playerVars])) + 
      geom_histogram(bins = 25, fill = "lightblue") + 
      ggtitle("Histogram of", input$playerVars) + 
      labs(x = input$playerVars)
  }
  return(plotCycle)
})

## output the plots
output$trialPlots <- renderPlot({
  Outputplot()
  })


######################################## TAB 4 (MODELING) ###############################

## Training-Testing data split
## also make it so that a user can access it via a slider value
dataSplit <- reactive({
  set.seed(558)
  trainIndex <- createDataPartition(myBatting$HR, p = input$splitSize, list = FALSE)
  myBattingTrain <- myBatting[trainIndex, ]
  myBattingTest <- myBatting[-trainIndex, ]
  return(list(trainData=myBattingTrain, testData=myBattingTest))
})

## this is the formula for MLR 
## build formula for all variables selected or only some variables selected
myFormula.mlr <- reactive({
  if (length(input$predVars)==0){
    return(formula(paste0(input$respVar,'~','(G + AB + X2B + X3B + RBI + SB + BB + SO + IBB + HBP + SF + GIDP)^2')))
  } else{
    n <- length(input$predVars)
    formulaBuild1 <- paste0(input$predVars,c(rep("+", n-1), ""))
    formulaBuild1 <- paste0(formulaBuild1, collapse = "")
    return(formula(paste0(input$respVar, '~', formulaBuild1)))
  }
})

## this formula is used for regression trees and random forests
## build formula for all variables selected or only some variables selected
myFormula <- reactive({
  if (length(input$predVars)==0){
    return(formula(paste0(input$respVar,'~','G + AB + X2B + X3B + RBI + SB + BB + SO + IBB + HBP + SF + GIDP')))
  } else{
    n <- length(input$predVars)
    formulaBuild <- paste0(input$predVars,c(rep("*", n-1), ""))
    forumulaBuild <- paste0(formulaBuild, collapse = "")
    return(formula(paste0(input$respVar, '~', forumulaBuild)))
  }
})

## FIT THE MLR MODEL
mlrFit <- eventReactive(input$runModelButton,{
fit.mlr <- train(myFormula(), data = dataSplit()[["trainData"]],
            method = "lm",
            preProcess = c("center", "scale"),
            trControl = trainControl(method = "repeatedcv", number = input$cvFold, repeats = 3))
return(fit.mlr)
})

## FIT THE REGRESSION TREE MODEL 
regTreeFit <- eventReactive(input$runModelButton,{
  fit.regTree <- train(myFormula(), data = dataSplit()[["trainData"]], method = "rpart",
        trControl = trainControl(method = "repeatedcv", number = input$cvFold, repeats = 3),
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(cp = seq(input$minCP, input$maxCP, input$stepSeq)))
  return(fit.regTree)
})

## FIT THE RANDOM FOREST MODEL 
rndmForest <- eventReactive(input$runModelButton,{
  fit.rf <- train(myFormula(), data = dataSplit()[["trainData"]], method = "rf",
                       trControl = trainControl(method = "repeatedcv", number = input$cvFold, repeats = 3),
                       preProcess = c("center", "scale"),
                       tuneGrid = data.frame(mtry = input$mtryMIN:input$mtryMAX))
  return(fit.rf)
})



## summary for MLR
output$mlrSummary <- renderPrint({
  if (input$runModelButton){
    summary(mlrFit())
  }
})

## summary for Regression Tree
output$regTreeSummary <- renderPrint({
  if (input$runModelButton){
    regTreeFit()
  }
})

## summary fro Random Forest
output$rfSummary <- renderPrint({
  if (input$runModelButton){
    rndmForest()
  }
})

## get RMSE
#output$RMSE <- renderTable({
 # rmsemlr <- mlrFit()$results$RMSE
 # rmseRegTree <- regTreeFit()$results$RMSE
  #rmserrf <- rndmForest()$results$RMSE
  

## now we want to find each minimum RMSE value for each model and format it correctly/nicely
## for MLR RMSE
output$mlrRMSEoutput <- renderPrint({
  mlrFit <- mlrFit()
  mlrRMSE <- mlrFit()$results["RMSE"] %>% min() %>% as.data.frame()
  colnames(mlrRMSE) <- "RMSE"
  return(mlrRMSE)
})
## for Reg Tree RMSE
output$regTreeRMSEoutput <- renderPrint({
  regTreeFit <- regTreeFit()
  regtreeRMSE <- regTreeFit()$results["RMSE"] %>% min() %>% as.data.frame()
  colnames(regtreeRMSE) <- "RMSE"
  return(regtreeRMSE)
})

## for Random Forest RMSE
output$rndmForestRMSEoutput <- renderPrint({
  rndmForest <- rndmForest()
  rndmforestRMSE <- rndmForest()$results["RMSE"] %>% min() %>% as.data.frame()
  colnames(rndmforestRMSE) <- "RMSE"
  return(rndmforestRMSE)
})

########## PREDICTION TAB ##########

#allPredvals <- eventReactive(input$predButton, {
 # #newData <- dataSplit()[["testData"]]
  #Games <- input$predGames
#  AtBats <- input$predAB
 # Doubles <- input$predX2B
#  Triples <- input$predX3B
 # RBIs <- input$predRBI
  #StolenBases <- input$predSB
  #Walks <- input$predBB
  #StrikeOuts <- input$predSO
  #IntentionalWalks <- input$predIBB
  #HitByPitch <- input$predHBP
  #SacFly <- input$predSF
  #GroundedDP <- input$predGIDP
  

#### this didnt work ^^

## establish predictor variables in terms of the test data set 
output$HRpredictions <- eventReactive(input$predButton,{
  predData <- dataSplit()[["testData"]]
  predData$G <- input$predGames
  predData$AB <- input$predAB
  predData$X2B <- input$predX2B
  predData$X3B <- input$predX3B
  predData$RBI <- input$predRBI
  predData$SB <- input$predSO
  predData$BB <- input$predBB
  predData$SO <- input$predSO
  predData$IBB <- input$predIBB
  predData$HBP <- input$predHBP
  predData$SF <- input$predSF
  predData$GIDP <- input$predGIDP
  
  ## create if/else for each button to return a prediction
  ## for MLR
  if (input$chooseModel=="Multiple Linear Regression"){
    mlrPrediction <- predict(mlrFit(), newdata = predData)
    mlrPrediction <- mlrPrediction[1] ## access first element, all we need
    return(as.numeric(mlrPrediction))}
  ## for Reg Tree
   else if (input$chooseModel=="Regression Tree"){
     regTreePrediction <- predict(regTreeFit(), newdata = predData)
     regTreePrediction <- regTreePrediction[1]
      return(as.numeric(regTreePrediction))}
  ## for Random Forest
   else if (input$chooseModel=="Random Forest")
     rndmForestPrediction <-  predict(rndmForest(), newdata = predData)
     rndmForestPrediction <- rndmForestPrediction[1]
      return(as.numeric(rndmForestPrediction))

})



##end
})
## so many parens...
