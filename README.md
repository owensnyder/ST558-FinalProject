# ST558-FinalProject

This app looks at data from the 2013-2017 Major League Baseball (MLB) seasons. The main goal of this analysis and exploration is to get a better idea of how many home runs were hit during this time period and how well can we predict the amount of home runs that were hit. The data comes from Sean Lahman’s `Lahman` package/database where anyone can pull datasets of current and past MLB data.

When you jump into the app, you will see an “About” page where you will encounter a brief description of the app and various website links where you can learn more about the tools used to create this app. Next, you will see a “Data Exploration” page where you can create summary statistics with their corresponding plots based on which season or variable you want to analyze. The plots will be boxplots, scatterplots, and histograms. After getting familiar with the data, you will go to the “Modeling” page and run three different models on the training data. In fact, you will be able to choose the proportion of training/test data to use and the number of folds for Cross Validation. The better model will eventually be determined by RMSE. After that, you will go to the “Prediction” tab to predict the number of home runs based on user-input and type of model. Finally, you can customize your own data table and download it to your device. Note, I have included a few popular players that you can filter through on that page.


Before running the app, make sure that you have these apps installed into your R session. These were the necessary packages needed for the creation of this app.

`install.packages(c("shiny", "shinydashboard", "DT", "ggplot2", "Lahman", "tidyverse", "caret"))`


You can run this code to access my app!

`shiny::runGitHub("owensnyder/ST558-FinalProject", subdir = "baseballApp/")`
