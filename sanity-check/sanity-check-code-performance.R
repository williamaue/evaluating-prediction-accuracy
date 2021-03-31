simulate_ground_truth <- function(){
  # Make sample data
  observed_data <- data.frame(array(NA, c(100,2)))
  names(observed_data) <- c("subj","observed")
  observed_data$subj <- 1:nrow(observed_data)
  observed_data$observed <- rnorm(nrow(observed_data),.5,.15)
  return(observed_data)
}


simulate_predicted_multipleTeams <- function(num_teams = 5, ground_truth_data){
  for(i in 1:num_teams){
    sim_teams.temp = simulate_predicted(ground_truth_data)
    sim_teams.temp$team = paste("team",i, sep = "")
    if(i == 1){sim_teams.out = sim_teams.temp} else {sim_teams.out = rbind(sim_teams.out, sim_teams.temp)}
  }
  return(sim_teams.out)
}


ground_truth.dat <- simulate_ground_truth()
write_csv(ground_truth.dat, "sanity-check/sanity-check-performance-ground-truth.csv")

perfect_team.dat <- data.frame(array(NA, c(nrow(ground_truth.dat), 3)))
names(perfect_team.dat) <- c("subj", "team", "prediction")
perfect_team.dat$subj = 1:nrow(perfect_team.dat)
perfect_team.dat$team = "perfect_team"
perfect_team.dat$prediction = ground_truth.dat$observed

all_zeros.dat <- data.frame(array(NA, c(nrow(ground_truth.dat), 3)))
names(all_zeros.dat) <- c("subj", "team", "prediction")
all_zeros.dat$subj = 1:nrow(all_zeros.dat)
all_zeros.dat$team = "all_zeros"
all_zeros.dat$prediction = 0

ground_truth.split = split(ground_truth.dat, c("matches","mismatches"))
ground_truth.split$mismatches$observed = rnorm(
  length(ground_truth.split$mismatches$observed), 
  mean(ground_truth.split$mismatches$observed)*.5, 
  sd(ground_truth.split$mismatches$observed)
  )

half_correct.dat <- data.frame(array(do.call(rbind, ground_truth.split)))
rownames(half_correct.dat) <- NULL
names(half_correct.dat) <- c("subj", "prediction")
half_correct.dat$team = "half_correct"
half_correct.dat <- half_correct.dat[,c("subj","team","prediction")]

little_off.dat <- data.frame(array(NA, c(nrow(ground_truth.dat), 3)))
names(little_off.dat) <- c("subj", "team", "prediction")
little_off.dat$subj = 1:nrow(little_off.dat)
little_off.dat$team = "off_by_a_little"
little_off.dat$prediction = ground_truth.dat$observed * runif(nrow(ground_truth.dat), .5, .6)


way_off.dat <- data.frame(array(NA, c(nrow(ground_truth.dat), 3)))
names(way_off.dat) <- c("subj", "team", "prediction")
way_off.dat$subj = 1:nrow(way_off.dat)
way_off.dat$team = "off_by_a_lot"
way_off.dat$prediction = ground_truth.dat$observed * runif(nrow(ground_truth.dat), .1, .2)

sanity_check.dat <- rbind(perfect_team.dat, all_zeros.dat, half_correct.dat, little_off.dat, way_off.dat)
write_csv(sanity_check.dat, "sanity-check/sanity-check-performance-teams.csv")
# perfect_team.dat$prediction = rnorm(nrow(perfect_team.dat), mean(ground_truth.dat$observed), sd(ground_truth.dat$observed))
