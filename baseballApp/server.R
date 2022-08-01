library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyverse)
library(caret)
library(Lahman)
library(dashboardthemes)



## this will be used for melding, maybe for everything??
myBatting <- Batting %>% filter(yearID > 2012 & yearID < 2018) %>% filter(HR > 5 & AB > 200)

shinyServer(function(input, output, session) {
  
#### tab 1 ####
  
## create links
  output$cranLink <- renderUI({
    tagList("CRAN documentation regarding the Lahman package", a("can be found here", 
            href="https://cran.r-project.org/web/packages/Lahman/Lahman.pdf"))
  })
  output$mlbLink <- renderUI({
    tagList("Major League Baseball official webaite", a("can be found here", href="https://www.mlb.com/"))
  })
  output$seanLahmanLink <- renderUI({
    tagList("Sean Lahman's offical wesbite", a("can be found here", href="https://www.seanlahman.com/"))
  })
  

#### tab 2 ####
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

#output$dataTable <- renderDataTable()

output$baseballData<- downloadHandler(
  filename = "LahmanBaseballData.csv",
  content = function(file) {
    write.csv(findPlayer(), file, row.names = FALSE)
  }
)


#### tab 3 ####

#tab3data <- reactive({
 # if ("G" %in% input$variables) return(batting$G)

 ## create a reactable new data set for plots and summaries 


Inputdata <- reactive({
  if(input$playerDataSelect == "All Players"){
    return(myBatting %>% select(yearID,lgID, HR,input$playerVars))
    
  } else if (input$playerDataSelect=="Nelson Cruz"){
    otherPlayers1 <- myBatting %>%
      filter(playerID == "cruzne02") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherPlayers1)
    
  } else if(input$playerDataSelect=="Jose Altuve"){
    otherPlayers2 <- myBatting %>%
      filter(playerID == "altuvjo01") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherPlayers2)
    
  }else if(input$playerDataSelect=="Giancarlo Stanton"){
    otherPlayers3 <- myBatting %>%
      filter(playerID == "stantmi03") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherPlayers3)
    
  }else if(input$playerDataSelect=="Bryce Harper"){
    otherPlayers4 <- myBatting %>%
      filter(playerID == "harpebr03") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherPlayers4)
    
  }else if(input$playerDataSelect=="Mookie Betts"){
    otherPlayers5 <- myBatting %>%
      filter(playerID == "bettsmo01") %>% select(yearID,lgID,HR,input$playerVars)
    return(otherPlayers5)
  }
})

# Create summary table

output$dataTable <- renderTable({
  setStat <- quo(!!sym(input$playerVars))  
  summaryStat <- Inputdata() %>%
    select(!!setStat) %>%
    summarise(Minimum = min(!!setStat),  Mean = mean(!!setStat, na.rm=TRUE),
            Maximum = max(!!setStat), StdDeviation = sd(!!setStat))
  summaryStat
})

#output$table_tag3 <- reactive({
#  summary(!!sym(input$playerVars))
#})


Outputplot <- reactive({
  if (input$plotChoice=="Box Plot"){
    plotCycle <- ggplot(data = Inputdata(), aes(x = lgID, y = Inputdata()[,input$playerVars], fill = lgID)) +
      geom_boxplot() + geom_jitter() + ggtitle("Boxplot of", input$playerVars) +
      labs(x = "League (AL/NL)", y = input$playerVars)
    
  } else {
    plotCycle <- ggplot(data = Inputdata(), aes(x = HR, y = Inputdata()[,input$playerVars])) + 
      geom_point(aes(color = yearID)) + geom_smooth(method = "lm", col = "red") + 
      ggtitle("Scatterplot of HR and", input$playerVars) + 
      labs(x = "Home Runs", y = input$playerVars)
  }
  return(plotCycle)
})


### MAYBVE TAKE OFF THE  GEOM_JITTER IN BOXPLOT?!?!


## output the plots
output$trialPlots <- renderPlot({
  Outputplot()
  })


## tab 4 (modeling) ## 

# Train/Test data split
Splitdata <- reactive({
  set.seed(558)
  trainIndex <- createDataPartition(myBatting$HR, p = input$splitSize, list = FALSE)
  myBattingTrain <- myBatting[trainIndex, ]
  myBattingTest <- myBatting[-trainIndex, ]
  return(list(trainData=myBattingTrain, testData=myBattingTest))
})

## this is the formula for MLR 
myFormula.mlr <- reactive({
  if (length(input$predVars)==0){
    return(formula(paste0(input$respVar,'~','(G + AB + X2B + X3B + RBI + SB + BB + SO + IBB + HBP + SF + GIDP)^2')))
  } else{
    n <- length(input$predVars)
    formulaBuild1 <- paste0(input$predVars,c(rep("+", n-1), ""))
    formulaBuild1 <- paste0(formulaBuild1, collapse = "")
    #temp <- paste0(temp^2)
    return(formula(paste0(input$respVar, '~', formulaBuild1)))
  }
})

## this formula is used for regression trees and random forests
myFormula <- reactive({
  if (length(input$predVars)==0){
    return(formula(paste0(input$respVar,'~','G + AB + X2B + X3B + RBI + SB + BB + SO + IBB + HBP + SF + GIDP')))
  } else{
    n <- length(input$predVars)
    formulaBuild <- paste0(input$predVars,c(rep("+", n-1), ""))
    forumulaBuild <- paste0(formulaBuild, collapse = "")
    #temp <- paste0(temp^2)
    return(formula(paste0(input$respVar, '~', forumulaBuild)))
  }
})

## FIT THE MLR MODEL
mlrFit <- eventReactive(input$runModelButton,{
fit.mlr <- train(myFormula(), data = Splitdata()[["trainData"]],
            method = "lm",
            preProcess = c("center", "scale"),
            trControl = trainControl(method = "repeatedcv", number = input$cvFold, repeats = 3))
return(fit.mlr)
})

## FIT THE REGRESSION TREE MODEL 
regTreeFit <- eventReactive(input$runModelButton,{
  fit.regTree <- train(myFormula(), data = Splitdata()[["trainData"]], method = "rpart",
        trControl = trainControl(method = "repeatedcv", number = input$cvFold, repeats = 3),
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
  return(fit.regTree)
})

## FIT THE RANDOM FOREST MODEL 
rndmForest <- eventReactive(input$runModelButton,{
  fit.rf <- train(myFormula(), data = Splitdata()[["trainData"]], method = "rf",
                       trControl = trainControl(method = "repeatedcv", number = input$cvFold, repeats = 3),
                       preProcess = c("center", "scale"),
                       tuneGrid = data.frame(mtry = 1:5))
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
 # rmse.mlr <- mlrFit()$results$RMSE
 # rmse.regTree <- regTreeFit()$results$RMSE
  #rmse.rf <- rndmForest()$results$RMSE
  

## now we want to find each minimum RMSE value for each model
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
 # #newData <- Splitdata()[["testData"]]
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
  predData <- Splitdata()[["testData"]]
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
