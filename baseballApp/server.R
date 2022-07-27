library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyverse)
library(caret)
library(Lahman)


## this will be used for melding, maybe for everything??
myBatting <- Batting %>% filter(yearID > "2020" & AB > 200)

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
         "Aaron Judge" = "judgeaa01",
         "Jose Altuve" = "altuvjo01",
         "Juan Soto" =  "sotoju01",
         "Mookie Betts" = "bettsmo01")
})

## full data tables
output$dataOutput <- renderDataTable({
  findPlayer <- playerChoice()
  if (findPlayer == "All"){
    myBatting %>% #select(playerID, yearID, stint, teamID, lgID, input$variables) %>%
      datatable()
  } else{
    myBatting %>%
      filter(playerID==findPlayer) %>% #select(playerID, yearID, stint, teamID, lgID, input$variables) %>%
      datatable()
  }
})

#### tab 3 ####


## boxplot and scatterplot
output$trialPlots <- renderPlot({"trailPlots"
#reactive({
  if (input$plotChoice =="Box Plot"){
     ggplot(data = myBatting, aes(x = lgID, y = HR, fill = lgID)) + geom_boxplot() + geom_jitter()
    
  } else{
  ggplot(data = myBatting, aes(x = G, y = HR)) + geom_point() + geom_smooth(method = lm, col = "red")
  
  }
 
#})
})




})



