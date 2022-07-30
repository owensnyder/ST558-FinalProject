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

#tab3data <- reactive({
 # if ("G" %in% input$variables) return(batting$G)

 ## create a reactable new data set for plots and summaries 


Inputdata <- reactive({
  if(input$playerDataSelect == "All Players"){
    return(myBatting %>% select(lgID, HR,input$playerVars))
    
  } else if (input$playerDataSelect=="Aaron Judge"){
    otherPlayers1 <- myBatting %>%
      filter(playerID == "judgeaa01") %>% select(lgID,HR,input$playerVars)
    return(otherPlayers1)
    
  } else if(input$playerDataSelect=="Jose Altuve"){
    otherPlayers2 <- myBatting %>%
      filter(playerID == "altuvjo01") %>% select(lgID,HR,input$playerVars)
    return(otherPlayers2)
    
  }else if(input$playerDataSelect=="Juan Soto"){
    otherPlayers3 <- myBatting %>%
      filter(playerID == "sotoju01") %>% select(lgID,HR,input$playerVars)
    return(otherPlayers3)
    
  }
  
})


# Create summary table
## this works!!
output$dataTable <- renderTable({
  setStat <- quo(!!sym(input$playerVars))  
  summaryStat <- Inputdata() %>%
    select(!!setStat) %>%
    summarise(Min = min(!!setStat),  Mean = mean(!!setStat, na.rm=T),
            Max = max(!!setStat))
  summaryStat
})

#output$table_tag3 <- reactive({
#  summary(!!sym(input$playerVars))
#})





Outputplot <- reactive({
  if (input$plotChoice=="Box Plot"){
    plotCycle <- ggplot(data = Inputdata(), aes(x = lgID, y = Inputdata()[,input$playerVars], fill = lgID)) +
      geom_boxplot() + geom_jitter()
    
  } else {
    plotCycle <- ggplot(data = Inputdata(), aes(x = HR, y = Inputdata()[,input$playerVars])) + 
      geom_point() + geom_smooth(method = "lm")
  }
  return(plotCycle)
})





# Output the plot
output$trialPlots <- renderPlot({
  Outputplot()
  })

})

