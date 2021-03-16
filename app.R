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
                   selectInput("data_type","Which type of data are being evaluated?", c("Performance","Differential expression")),
                   fileInput("ground_truth", "Choose Ground-Truth File",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                   fileInput("predictions", "Choose a predictions file",
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
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
               htmlOutput("tableInfo")
               )
    )
)


server <- function(input, output) {
    observeEvent(
        eventExpr = input[["submit_loc"]],
        handlerExpr = {
            diffVals <- reactive({
                
                if(input$data_type == "Performance"){
                    err_txt = NA # Placeholder for an error message, if needed.
                        
                        # Import the ground truth file
                        observations.file <- input$ground_truth
                        observations.dat <- read.csv(observations.file$datapath, header = TRUE)
                        
                        # Import the file containing the predictions for each team
                        predictions.file <- input$predictions
                        predictions.dat <- read.csv(predictions.file$datapath, header = TRUE)
                        
                        # Generate a list of the teams that we have predictions for
                        team_list = unique(predictions.dat$team)
                    
                        # Create an empty table to take in the results of the calculations
                        table_out <- data.frame(array(NA, c(5,8)))
                        names(table_out) <- c("Team", "n", "MAE", "RMSE", "Pearson", "Spearman", "Intercept", "Beta")
                    
                    # Loop over the team list and calculate accuracy, updating the results table along the way.
                    for(teamIdx in 1:length(team_list)){
                        # Concatenate the team number and assign it to the predictions variable
                        predictions.temp <- predictions.dat %>% filter(team == team_list[teamIdx])
                        
                        # Check that there are the right number of columns in the predictions file.
                        if(ncol(predictions.temp) > 3){
                            err_txt[1] <- paste("There are too many columns in the predictions file for ", team_list[teamIdx], ".", sep = "")
                        }
                        
                        # Check that there is the number of rows in the predictions file match the number of rows.
                        if(nrow(observations.dat) != nrow(predictions.temp)){
                            err_txt[2] <- paste("The number of data points in the predictions file do not match the number of 
                            predictions in the ground-truth file for ", team_list[teamIdx] ,".", sep = "")
                        }
                        
                        # Check that there is a prediction for each subject.
                        if(sum(is.na(match(observations.dat$subj, predictions.temp$subj))) > 0){
                            err_txt[3] = paste("There is a discrepancy in the subject numbers in the predictions file and the 
                            ground-truth file for ", team_list[teamIdx] ,".", sep = "")
                        }
                        
                        # Combine the Team data with the ground-truth matching by subject number ("subj")
                        comparison.dat <- left_join(observations.dat, predictions.temp, by ="subj")
                        
                        # Update the table with the team's information
                        table_out$Team[teamIdx] = team_list[teamIdx]
                        table_out$n = length(unique(predictions.temp$subj))
                        table_out$MAE[teamIdx] <- round(mae(comparison.dat$prediction - comparison.dat$observed), 3)
                        table_out$RMSE[teamIdx] <- round(rmse(comparison.dat$prediction - comparison.dat$observed), 3)
                        table_out$Pearson[teamIdx] <- round(cor(comparison.dat$prediction, comparison.dat$observed, method = "pearson"), 3)
                        table_out$Spearman[teamIdx] <- round(cor(comparison.dat$prediction, comparison.dat$observed, method = "spearman"), 3)
                        table_out$Intercept[teamIdx] <- round(coef(lm(observed ~ prediction, data = comparison.dat))["(Intercept)"], 3)
                        table_out$Beta[teamIdx] <- round(coef(lm(observed ~ prediction, data = comparison.dat))["prediction"], 3)
                    }
                    print(err_txt)
                    table_out
                }
            })
            
            tableInfo <- reactive({
                if(input$data_type == "Performance"){
                    HTML("
                    <p>n: Number of unique subject predictions provided by a team</p>
                    <p>MAE: Mean absolute error</p>
                    <p>RMSE: Root Mean Squared Error</p>
                    <p>Pearson: Pearson Correlation</p>
                    <p>Spearman: Spearman Correlation</p>
                    <p>Intercept/Beta: Values from a regression for predicting the observed data given the predictions.</p>")
                }
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
            output$tableInfo = renderText({tableInfo()})
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)