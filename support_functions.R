# Return the Root Mean Squared Error given a set of difference scores
rmse <- function(error){
  sqrt(mean(error^2, na.rm = TRUE))
}

# Return the Mean Absolute Error given a set of difference scores
mae <- function(error){
  mean(abs(error), na.rm = TRUE)
}

simulate_ground_truth <- function(){
  # Make sample data
  observed_data <- data.frame(array(NA, c(100,2)))
  names(observed_data) <- c("subj","observed")
  observed_data$subj <- 1:nrow(observed_data)
  observed_data$observed <- rnorm(nrow(observed_data),.5,.15)
  return(observed_data)
}

simulate_predicted <- function(ground_truth_data){
  predicted_data <- data.frame(array(NA, dim(ground_truth_data)))
  names(predicted_data) <- c("subj","prediction")
  predicted_data$subj = 1:nrow(predicted_data)
  predicted_data$prediction = rnorm(nrow(predicted_data), mean(ground_truth_data$observed), sd(ground_truth_data$observed))
  return(predicted_data)
}

simulate_predicted_multipleTeams <- function(num_teams = 5, ground_truth_data){
  for(i in 1:num_teams){
    sim_teams.temp = simulate_predicted(ground_truth_data)
    sim_teams.temp$team = paste("team",i, sep = "")
    if(i == 1){sim_teams.out = sim_teams.temp} else {sim_teams.out = rbind(sim_teams.out, sim_teams.temp)}
  }
  return(sim_teams.out)
}