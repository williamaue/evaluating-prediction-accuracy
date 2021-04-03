
# This is a Shiny web application (http://shiny.rstudio.com/) for comparing multiple
# sets of data to a single ground-truth dataset.
#
# Author:   William Aue <william.aue@parallaxresearch.org>
#
# Copyright (C) 2021 Parallax Advanced Research

library(shiny)
library(shinythemes)
library(tidyverse)
library(shinycssloaders)
library(caret)
library(e1071)

source("support_functions.R")
mirna_list.dat <- read_csv("mirna_list.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),

    # Application title
    titlePanel("Evaluating TAILOR Prediction Accuracy"),
    fluidRow(style='padding-left:25px; padding-right:25px',
        sidebarLayout(
               sidebarPanel(width = 4, 
                   h4("File Inputs"),
                   p("When you have uploaded the necessary files, press 'Submit' to run the analysis. Clicking on the 'CSV' button under 
                     the output table will download the results to a CSV file."),
                   
                   p("Please note, the differential expression analysis takes a couple of minutes to complete, depending on the amount of data."),
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
                   p(HTML("For a differential expression analysis, does the <em>expressed</em> column include directionality of the 
                          differential expression where  1 = upregulated, -1 = downregulated, and 0 = not differentially expressed?")),
                   checkboxInput("isMulticlass","Yes, it is directional.", FALSE),
                   actionButton(
                       inputId = "submit_loc",
                       label = "Submit"
                   ),
                   p(HTML("<p style='font-size:11px'>Developed by <a href='mailto:william.aue@parallaxresearch.org'>Dr. William Aue</a> 
                   with support provided by Wright State University through a collaborative agreement awarded to Dr. Ion Juvina by the 
                   Defense Advanced Research Projects Agency (DARPA) under agreement number HR00111990066.</p>")),
                   p(HTML("<p style='font-size:11px'>The source code for this Shiny application is available on 
                          <a href='https://github.com/williamaue/evaluating-prediction-accuracy'>GitHub</a>.</p>"))
                   ),
        mainPanel(width = 8,
            tabsetPanel(
                id = "tabs",
                type = "tabs",
                tabPanel("Instructions",
                         p(HTML("
                         <h3>Performance predictions</h3>
                         <h4 style='padding-right: 20px; padding-bottom: 20px; padding-left: 20px'><p>For performance predictions, each file should be a .CSV file. The <b>ground truth</b> file should contain a column named <em>subj</em> with a subject number and a column with the subject's performance called <em>observed</em>. The <b>predictions</b> file should contain a column named <em>team</em> with the team name team, a column named <em>subj</em> containing a number, and a column named <em>prediction</em> 
                         containing subject's predicted performance.</p></h4>
                         
                         <h3>Differential Expression</h3>
                         <h4 style='padding-right: 20px; padding-bottom: 20px; padding-left: 20px'><p>For differential expression, each file should be a .CSV file containing the a team name in a column named <em>team</em>, a subject number column named <em>subj</em>, a column of miNRA labels named <em>mirna</em>, and the nature of the differential expression in a column called <em>expressed</em>. The predictions can be either binary 1 = differentially expressed or 0 = not differentially expressed, or directional where  1 = upregulated, -1 = downregulated, and 0 = not differentially expressed. Any miRNA not listed in the prediction or ground truth files are assumed to be not differentially expressed.</p></h4>"))),
                tabPanel("Analysis",
                         withSpinner(DT::dataTableOutput("diffTable"), color = "#080051", type = 6),
                         htmlOutput("tableInfo"),
                         HTML("<p>Sample data files to test these analyses can be downloaded from <a href='https://github.com/williamaue/evaluating-prediction-accuracy/tree/master/sample-data'>GitHub </a>.</p>")
                         )
               )
        )
    )
)
)


server <- function(input, output, session) {
    output$diffTable = NULL
    observeEvent(
        eventExpr = input[["submit_loc"]],
        handlerExpr = {
            updateTabsetPanel(session, "tabs",
                              selected = "Analysis"
            )
            
            diffVals <- reactive({
                
                if(input$data_type == "Performance"){
                    err_txt = NA # Placeholder for an error message, if needed.
                        
                        # Import the ground truth file
                        observations.file <- input$ground_truth
                        observations.dat <- read_csv(observations.file$datapath) #, header = TRUE)
                        
                        # Import the file containing the predictions for each team
                        predictions.file <- input$predictions
                        predictions.dat <- read_csv(predictions.file$datapath) #, header = TRUE)
                        
                        # Generate a list of the teams that we have predictions for
                        team_list = unique(predictions.dat$team)
                    
                        # Create an empty table to take in the results of the calculations
                        table_out <- data.frame(array(NA, c(5,8)))
                        names(table_out) <- c("Team", "n", "MAE", "RMSE", "Pearson", "Spearman", "Intercept", "Beta")
                    
                    # Loop over the team list and calculate accuracy, updating the results table along the way.
                    for(teamIdx in 1:length(team_list)){
                        # Concatenate the team number and assign it to the predictions variable
                        predictions.temp = predictions.dat %>% filter(team == team_list[teamIdx])
                        
                        # # Check that there are the right number of columns in the predictions file.
                        # if(ncol(predictions.temp) > 3){
                        #     err_txt[1] = paste("There are too many columns in the predictions file for ", team_list[teamIdx], ".", sep = "")
                        # }
                        # 
                        # # Check that there is the number of rows in the predictions file match the number of rows.
                        # if(nrow(observations.dat) != nrow(predictions.temp)){
                        #     err_txt[2] = paste("The number of data points in the predictions file do not match the number of 
                        #     predictions in the ground-truth file for ", team_list[teamIdx] ,".", sep = "")
                        # }
                        # 
                        # # Check that there is a prediction for each subject.
                        # if(sum(is.na(match(observations.dat$subj, predictions.temp$subj))) > 0){
                        #     err_txt[3] = paste("There is a discrepancy in the subject numbers in the predictions file and the 
                        #     ground-truth file for ", team_list[teamIdx] ,".", sep = "")
                        # }
                        
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
                    # print(err_txt)
                    table_out
                    
                } else if(input$data_type == "Differential expression") {
                    # Import the ground truth file
                    observations.file <- input$ground_truth
                    observations.dat <- read_csv(observations.file$datapath) #, header = TRUE)

                    # Import the file containing the predictions for each team
                    predictions.file <- input$predictions
                    predictions.dat <- read_csv(predictions.file$datapath) #, header = TRUE)
                    
                    # Generate a list of the teams that we have predictions for
                    team_list = unique(predictions.dat$team)

                    for(teamIdx in 1:length(team_list)){
                        print(paste("Starting ", team_list[teamIdx]))
                        # Select the specific team to analyze
                        team_dat.temp = predictions.dat %>%
                            filter(team == team_list[teamIdx])
                        # Select the subject within the team to analyze
                        subject_list = unique(team_dat.temp$subject)
                        for(subjIdx in 1:length(subject_list)){
                            print(paste("Starting subject ", subject_list[subjIdx]))
                            # Pull out the ground truth data for the subject
                            observations_subject.temp = observations.dat %>%
                                filter(subject == subject_list[subjIdx])
                            
                            # Expand the ground truth to include all miRNA using the miRNA list sourced earlier
                            # Create an empty dataframe for the subject with all miRNA listed as not expressed
                            # and a 0 direction
                            observations_subject_expanded.temp <- tibble(
                                subject = rep(subject_list[subjIdx], nrow(mirna_list.dat)),
                                mirna = mirna_list.dat$mirna_list,
                                expressed_obs = rep(0, nrow(mirna_list.dat)),
                                # direction_obs = rep(0, nrow(mirna_list.dat)),
                            )
                            
                            # Fill in the expanded array with the ground truth
                            observations_subject_expanded.temp[match(
                                observations_subject.temp$mirna, 
                                observations_subject_expanded.temp$mirna), 
                                "expressed_obs"] = observations_subject.temp$expressed
                            
                            # Pull out the predictions for the subject
                            pred_subj_dat.temp = team_dat.temp %>%
                                filter(subject == subject_list[subjIdx])
                            
                            # Expand the prediction to include all miRNA using the miRNA list sourced earlier
                            # Create an empty dataframe for the subject with all miRNA listed as not expressed
                            # and a 0 direction
                            pred_subj_dat_expanded.temp <- tibble(
                                subject = rep(subject_list[subjIdx], nrow(mirna_list.dat)),
                                team = team_list[teamIdx],
                                mirna = mirna_list.dat$mirna_list,
                                # expressed_obs = rep(0, nrow(mirna_list.dat))
                                expressed_pred = rep(0, nrow(mirna_list.dat))
                            )
                            
                            # Fill in the expanded array with the team's prediction
                            pred_subj_dat_expanded.temp[
                                match(pred_subj_dat.temp$mirna, 
                                      pred_subj_dat_expanded.temp$mirna), "expressed_pred"] = 
                                pred_subj_dat.temp$expressed
                            
                            # Combine the two objects by subject number and mirna
                            pred_subj_dat_expanded_combined = left_join(
                                pred_subj_dat_expanded.temp, 
                                observations_subject_expanded.temp, 
                                by = c("subject", "mirna"), 
                                keep = FALSE
                            )
                            
                            # Convert the observed and predicted variables for direction in to factors and label the factor levels. 
                            # This is a necessary step for Caret to be able to calculate the confusion matrix
                            if(input$isMulticlass == TRUE){
                                pred_subj_dat_expanded_combined_caret = pred_subj_dat_expanded_combined %>%
                                    mutate(
                                        expressed_obs_f = factor(
                                            expressed_obs,
                                            levels = c(-1, 0, 1),
                                            labels = c("down_regulated", "not_diffExp","up_regulated")
                                        ),
                                        expressed_pred_f = factor(
                                            expressed_pred,
                                            levels = c(-1, 0, 1),
                                            labels = c("down_regulated", "not_diffExp","up_regulated")
                                        )
                                    )
                            } else if(input$isMulticlass == FALSE){
                                pred_subj_dat_expanded_combined_caret = pred_subj_dat_expanded_combined %>%
                                    mutate(
                                        expressed_obs_f = factor(
                                            expressed_obs,
                                            levels = c(0, 1),
                                            labels = c("not_diffExp","diffExp")
                                        ),
                                        expressed_pred_f = factor(
                                            expressed_pred,
                                            levels = c(0, 1),
                                            labels = c("not_diffExp","diffExp")
                                        )
                                    )
                            }

                            if(input$isMulticlass == TRUE){caretPositive = NULL} else if(input$isMulticlass == FALSE){caretPositive = "diffExp"}
                            results_confusionMatrix.out = 
                                confusionMatrix(
                                    data = pred_subj_dat_expanded_combined_caret$expressed_pred_f,
                                    reference = pred_subj_dat_expanded_combined_caret$expressed_obs_f,
                                    positive = caretPositive
                                )
                            
                            # Overall results
                            results_overall.temp = tibble(
                                subject = subject_list[subjIdx],
                                team = team_list[teamIdx]
                            ) 
                            results_overall.temp = cbind(results_overall.temp, as_tibble_row(results_confusionMatrix.out$overall))
                            
                            # Class-level results
                            results_byClass.temp = as_tibble(results_confusionMatrix.out$byClass, rownames = "class_label")
                            results_byClass.temp["class_label"] = str_replace_all(results_byClass.temp$class_label, "Class: ", "")
                            if(input$isMulticlass == TRUE){
                                results_byClass_wide.temp = results_byClass.temp %>%
                                    pivot_wider(
                                        names_from = class_label,
                                        values_from = Sensitivity:`Balanced Accuracy`,
                                        names_sep = "."
                                    )
                                
                            } else if(input$isMulticlass == FALSE){
                                results_byClass_wide.temp = results_byClass.temp %>%
                                    pivot_wider(
                                        names_from = class_label,
                                        values_from = value,
                                        names_sep = "."
                                    )
                            }
                            
                            results_byClass_wide.temp = cbind(
                                tibble(
                                    subject = subject_list[subjIdx],
                                    team = team_list[teamIdx]
                                ),
                                results_byClass_wide.temp
                            )
                            
                            if(subjIdx == 1 & teamIdx == 1){
                                raw_predictions.dat = pred_subj_dat_expanded_combined_caret
                                results_byClass_wide.dat = results_byClass_wide.temp
                                results_overall.dat = results_overall.temp
                            } else {
                                raw_predictions.dat = rbind(raw_predictions.dat, pred_subj_dat_expanded_combined_caret)
                                results_byClass_wide.dat = rbind(results_byClass_wide.dat, results_byClass_wide.temp)
                                results_overall.dat = rbind(results_overall.dat, results_overall.temp)
                            }
                            print(paste("Finished subject ", subject_list[subjIdx]))
                        }
                        print(paste("Finished ", team_list[teamIdx]))
                    }
                    
                    if(input$isMulticlass == TRUE){
                        
                        results_byClass.sum = results_byClass_wide.dat %>%
                            group_by(team) %>%
                            summarize(across(`Sensitivity.down_regulated`:`Balanced Accuracy.up_regulated`, mean)) %>%
                            select("team",
                                   # "Precision.down_regulated","Precision.not_diffExp","Precision.up_regulated",
                                   "Sensitivity.down_regulated","Sensitivity.not_diffExp", "Sensitivity.up_regulated",
                                   "Specificity.down_regulated","Specificity.not_diffExp","Specificity.up_regulated",
                                   "Balanced Accuracy.down_regulated","Balanced Accuracy.not_diffExp","Balanced Accuracy.up_regulated",
                                   "F1.down_regulated","F1.not_diffExp","F1.up_regulated")
                        
                        results_overall.sum = results_overall.dat %>%
                            group_by(team) %>%
                            summarize(across(`Accuracy`:`McnemarPValue`, mean)) %>%
                            select("team","Accuracy")
                        
                        table_out = left_join(results_overall.sum, results_byClass.sum, by = "team") %>%
                            rename(Accuracy.overall = "Accuracy") %>%
                            mutate(across(Accuracy.overall:F1.up_regulated, round, 3))
                   
                         } else if(input$isMulticlass == FALSE){
                             
                        results_byClass.sum = results_byClass_wide.dat %>%
                            group_by(team) %>%
                            summarize(across(`Sensitivity`:`Balanced Accuracy`, mean, na.rm = TRUE)) %>%
                            select("team", "Sensitivity","Specificity","Balanced Accuracy","F1")
                        
                        results_overall.sum = results_overall.dat %>%
                            group_by(team) %>%
                            summarize(across(`Accuracy`:`McnemarPValue`, mean, na.rm = TRUE)) %>%
                            select("team","Accuracy")
                        
                        table_out = left_join(results_overall.sum, results_byClass.sum, by = "team") %>%
                            rename(Accuracy.overall = "Accuracy") %>%
                            mutate(across(Accuracy.overall:F1, round, 3))
                        
                    }
                    table_out
                }
            })
            
            tableInfo <- reactive({
                if(input$data_type == "Performance"){
                    HTML("
                    <br><p>n: Number of unique subject predictions provided by a team</p>
                    <p>MAE: Mean absolute error</p>
                    <p>RMSE: Root Mean Squared Error</p>
                    <p>Pearson: Pearson Correlation</p>
                    <p>Spearman: Spearman Correlation</p>
                    <p>Intercept/Beta: Values from a regression for predicting the observed data given the predictions.</p>")
                } else if(input$data_type == "Differential expression"){
                    HTML("
                    <br><p>Performance metrics are calculated using the <em>confusionMatrix</em> function in the Caret R package 
                    (<a href='https://topepo.github.io/caret'>https://topepo.github.io/caret</a>). Suppose a 2x2 table with notation:</p>
                    <style>
                    table, th, td {
                    border: 1px solid black;
                    }
                    th, td {
                    padding: 10px;
                    }
                    </style>
                    <table>
                    <thead>
                      <tr>
                        <th>Predicted</th>
                        <th colspan='2'>Reference</th>
                      </tr>
                    </thead>
                    <tbody>
                      <tr>
                        <td></td>
                        <td>Present<br></td>
                        <td>Absent<br></td>
                      </tr>
                      <tr>
                        <td>Present<br></td>
                        <td>A</td>
                        <td>B</td>
                      </tr>
                      <tr>
                        <td>Absent</td>
                        <td>C</td>
                        <td>D</td>
                      </tr>
                    </tbody>
                    </table>
                        <br><p>The formulas used here are:</p>
                        <p>Sensitivity/Recall = A/(A+C)</p>
                        <p>Specificity = D/(B+D)</p>
                        <p>Balanced Accuracy = (sensitivity+specificity)/2</p>
                        <p>F1 = (1+beta^2)*precision*recall/((beta^2 * precision)+recall) where beta = 1 and precision = A/(A+B).</p>
                    ")
                }
            })

            output$diffTable = DT::renderDataTable({
                diffVals()
                },
                extensions = 'Buttons',
                
                options = list(
                    paging = TRUE,
                    searching = TRUE,
                    fixedColumns = TRUE,
                    autoWidth = TRUE,
                    ordering = TRUE,
                    dom = 'tB',
                    pageLength = 30,
                    buttons = c('csv')
                ),
                class = "display")
            
            output$tableInfo = renderText({
                tableInfo()
                })
        }
    )}



# Run the application 
options(shiny.reactlog=TRUE) 
shinyApp(ui = ui, server = server)
