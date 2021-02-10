#
# This is a Shiny web application (http://shiny.rstudio.com/) for comparing multiple
# sets of data to a single ground-truth dataset.
#
# Author:   William Aue <william.aue@parallaxresearch.org>
#
# Copyright (C) 2021 Parallax Advanced Research

library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(validate)

source("support_functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),

    # Application title
    titlePanel("Evaluating TAILOR Prediction Accuracy"),
    
    fluidRow(
        column(3,
               wellPanel(
                   h4("File Inputs"),
                   p(HTML("Select the input files. Each file should be a .CSV file containing a column with a subject number, 
                   named <em>subj</em>, and their corresponding prediction, named <em>prediction</em>.")),
                   p("When you have uploaded the necessary files, press 'Submit' to run the analysis. Clicking on the 'CSV' button under the output table will download the results to a CSV file."),
                   p("To see a demonstration of the output table, click the 'demonstration' box and click 'Submit'."),
                   fileInput("ground_truth", "Choose Ground-Truth File",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                   fileInput("team1_pred", "Choose predictions file for Team 1",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                   fileInput("team2_pred", "Choose predictions file for Team 2",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                   fileInput("team3_pred", "Choose predictions file for Team 3",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                   fileInput("team4_pred", "Choose predictions file for Team 4",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                   fileInput("team5_pred", "Choose predictions file for Team 5",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                   checkboxInput("demo", "Demonstration", FALSE),
                   actionButton(
                       inputId = "submit_loc",
                       label = "Submit"
                   ),
                   p(HTML("<p style='font-size:11px'>Developed by <a href='mailto:william.aue@parallaxresearch.org'>Dr. William Aue</a> 
                   with support provided by Wright State University through a collaborative agreement awarded to Dr. Ion Juvina by the 
                   Defense Advanced Research Projects Agency (DARPA) under agreement number HR00111990066.</p>")),
                   p(HTML("<p style='font-size:11px'>The source code for this Shiny application is available on <a href='https://github.com/williamaue/evaluating-prediction-accuracy'>GitHub</a>.</p>")),
                   )
               ),
        column(6,
               DT::dataTableOutput("diffTable"),
               p(HTML("
               <p>MAE: Mean absolute error</p>
               <p>RMSE: Root Mean Squared Error</p>
               <p>Pearson: Pearson Correlation</p>
               <p>Spearman: Spearman Correlation</p>
               <p>Intercept/Beta: Values from a regression for predicting the observed data given the predictions.</p>
                      "))
               )
    )
)


server <- function(input, output) {
    observeEvent(
        eventExpr = input[["submit_loc"]],
        handlerExpr = {
            diffVals <- reactive({
                err_txt = NA # Placeholder for an error message, if needed.
                
                demo <- input$demo
                if(demo == FALSE){
                    observations.file <- input$ground_truth
                    observations.dat <- read.csv(observations.file$datapath, header = TRUE)
                    team_list = NA
                    
                    if(is.null(input$team1_pred) == FALSE){
                        team1_predictions.file <- input$team1_pred
                        team1_predictions.dat <- read.csv(team1_predictions.file$datapath, header = TRUE)
                        team_list = c(team_list, "team1")
                    }
                    
                    if(is.null(input$team2_pred) == FALSE){
                        team2_predictions.file <- input$team2_pred
                        team2_predictions.dat <- read.csv(team2_predictions.file$datapath, header = TRUE)
                        team_list = c(team_list, "team2")
                    }
                    
                    if(is.null(input$team3_pred) == FALSE){
                        team3_predictions.file <- input$team3_pred
                        team3_predictions.dat <- read.csv(team3_predictions.file$datapath, header = TRUE)
                        team_list = c(team_list, "team3")
                    }

                    if(is.null(input$team4_pred) == FALSE){
                        team4_predictions.file <- input$team4_pred
                        team4_predictions.dat <- read.csv(team4_predictions.file$datapath, header = TRUE)
                        team_list = c(team_list, "team4")
                    }

                    if(is.null(input$team5_pred) == FALSE){
                        team5_predictions.file <- input$team5_pred
                        team5_predictions.dat <- read.csv(team5_predictions.file$datapath, header = TRUE)
                        team_list = c(team_list, "team5")
                    }

                } else {

                    observations.dat <- simulate_ground_truth()
                    team1_predictions.dat <- simulate_predicted(observations.dat)
                    team2_predictions.dat <- simulate_predicted(observations.dat)
                    team3_predictions.dat <- simulate_predicted(observations.dat)
                    team4_predictions.dat <- simulate_predicted(observations.dat)
                    team5_predictions.dat <- simulate_predicted(observations.dat)
                    team_list = c("team1","team2","team3","team4","team5")
                }
                
                # Create an empty table to take in the results of the calculations
                table_out <- data.frame(array(NA, c(5,7)))
                names(table_out) <- c("Team", "MAE", "RMSE", "Pearson", "Spearman", "Intercept", "Beta")

                # List the team names corresponding to the uploaded files
                team_list = na.omit(team_list)

                # Loop over the team list and calculate accuracy, updating the results table along the way.
                for(teamIdx in 1:length(team_list)){
                    # Concatenate the team number and assign it to the predictions variable
                    predictions.dat <- get(paste(team_list[teamIdx], "_predictions.dat", sep = ""))
                    
                    # Check that there are the right number of columns in the predictions file.
                    if(ncol(predictions.dat) > 2){
                        err_txt <- "There are too many columns in the predictions file for Team 1. There should only be two columns."
                    }

                    # Check that there is the number of rows in the predictions file match the number of rows.
                    if(nrow(observations.dat) != nrow(predictions.dat)){
                        err_txt <- "The number of data points in the predictions file do not match the number of predictions in the ground-truth file for Team 1."
                    }
                    
                    # Check that there is a prediction for each subject.
                    if(sum(is.na(match(observations.dat$subj, predictions.dat$subj))) > 0){
                        err_txt = "There is a discrepancy in the subject numbers in the predictions file and the ground-truth file for Team 1."
                    }
                    
                    # Combine the Team data with the ground-truth matching by subject number ("subj")
                    comparison.dat <- left_join(observations.dat, predictions.dat, by ="subj")
                    
                    # Update the table with the team's information
                    table_out$Team[teamIdx] = team_list[teamIdx]
                    table_out$MAE[teamIdx] <- round(mae(comparison.dat$prediction - comparison.dat$observed), 3)
                    table_out$RMSE[teamIdx] <- round(rmse(comparison.dat$prediction - comparison.dat$observed), 3)
                    table_out$Pearson[teamIdx] <- round(cor(comparison.dat$prediction, comparison.dat$observed, method = "pearson"), 3)
                    table_out$Spearman[teamIdx] <- round(cor(comparison.dat$prediction, comparison.dat$observed, method = "spearman"), 3)
                    table_out$Intercept[teamIdx] <- round(coef(lm(observed ~ prediction, data = comparison.dat))["(Intercept)"], 3)
                    table_out$Beta[teamIdx] <- round(coef(lm(observed ~ prediction, data = comparison.dat))["prediction"], 3)
                }
                print(err_txt)
                table_out
            })

            output$diffTable = DT::renderDataTable(
                { diffVals() },
                extensions = 'Buttons',
                
                options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    buttons = c('csv')
                ),
                
                class = "display"
            )
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)