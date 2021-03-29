######### Binary ##########
nMirna = 30
mini_mirna_list = mirna_list.dat$mirna_list[sample(length(mirna_list.dat$mirna_list), nMirna*2)]
mini_mirna_list.split = split(mini_mirna_list, c("TP","FP"))
mirna_present = mini_mirna_list.split$TP
mirna_notpresent = mini_mirna_list.split$FP

sanity_check_ground_truth_binary_30 = tibble(
  "subject" = rep("1", nMirna),
  "team" = rep("team1", nMirna),
  "mirna" = mirna_present,
  "expressed" = 1
)
write_csv(sanity_check_ground_truth_binary_30, "sanity-check/sanity-check-ground-truth_binary_30.csv")


nSubj = 40
sanity_check_binary.dat = NA
for(j in 1:nSubj){
  sanity_check_binary.temp = tibble(
    "subject" = rep(j, nMirna),
    "team" = rep(paste("team_allTP"), nMirna),
    "mirna" = mirna_present,
    "expressed" = rep(1, nMirna)
  )
  sanity_check_binary.dat = rbind(sanity_check_binary.dat, sanity_check_binary.temp)
}

for(j in 1:nSubj){
  sanity_check_binary.temp = tibble(
    "subject" = rep(j, nMirna*.5),
    "team" = rep(paste("team_halfTP"), nMirna*.5),
    "mirna" = sample(mirna_present, nMirna*.5),
    "expressed" = rep(1, nMirna*.5)
  )
  sanity_check_binary.dat = rbind(sanity_check_binary.dat, sanity_check_binary.temp)
}

for(j in 1:nSubj){
  sanity_check_binary.temp = tibble(
    "subject" = rep(j, nMirna),
    "team" = rep(paste("team_allFP"), nMirna),
    "mirna" = mirna_notpresent,
    "expressed" = rep(1, nMirna)
  )
  sanity_check_binary.dat = rbind(sanity_check_binary.dat, sanity_check_binary.temp)
}

for(j in 1:nSubj){
  sanity_check_binary.temp = tibble(
    "subject" = rep(j, nMirna*.5),
    "team" = rep(paste("team_halfFP"), nMirna*.5),
    "mirna" = sample(mirna_notpresent, nMirna*.5),
    "expressed" = rep(1, nMirna*.5)
  )
  sanity_check_binary.dat = rbind(sanity_check_binary.dat, sanity_check_binary.temp)
}

for(j in 1:nSubj){
  sanity_check_binary.temp = tibble(
    "subject" = rep(j, nMirna),
    "team" = rep(paste("team_halfTP-halfFP"), nMirna),
    "mirna" = c(sample(mirna_present, nMirna*.5), sample(mirna_notpresent, nMirna*.5)),
    "expressed" = c(rep(1, nMirna*.5), rep(1, nMirna*.5))
  )
  sanity_check_binary.dat = rbind(sanity_check_binary.dat, sanity_check_binary.temp)
}

for(j in 1:nSubj){
  sanity_check_binary.temp = tibble(
    "subject" = rep(j, nMirna),
    "team" = rep(paste("team_halfTP-halfFN"), nMirna),
    "mirna" = mirna_present,
    "expressed" = c(rep(1, nMirna*.5), rep(0, nMirna*.5))
  )
  sanity_check_binary.dat = rbind(sanity_check_binary.dat, sanity_check_binary.temp)
}

for(j in 1:nSubj){
  sanity_check_binary.temp = tibble(
    "subject" = rep(j, nMirna),
    "team" = rep(paste("team_allFN"), nMirna),
    "mirna" = mirna_present,
    "expressed" = rep(0, nMirna)
  )
  sanity_check_binary.dat = rbind(sanity_check_binary.dat, sanity_check_binary.temp)
}

write_csv(sanity_check_binary.dat %>% drop_na(), "sanity-check/sanity-check-binary-teams.csv")

########## Multiclass ##########

nMirna = 40
mini_mirna_list = mirna_list.dat$mirna_list[sample(length(mirna_list.dat$mirna_list), nMirna*2)]
mini_mirna_list.split = split(mini_mirna_list, c("TP","FP"))
mirna_present = mini_mirna_list.split$TP
mirna_present.split = split(mirna_present, c("upReg","downReg"))
mirna_notpresent = mini_mirna_list.split$FP
mirna_notpresent.split = split(mirna_notpresent, c("upReg","downReg"))

sanity_check_ground_truth_multiclass_30 = tibble(
  "subject" = rep("1", nMirna),
  "team" = rep("team1", nMirna),
  "mirna" = mirna_present,
  "expressed" = c(rep(1, nMirna*.5), rep(-1, nMirna*.5))
)
write_csv(sanity_check_ground_truth_multiclass_30, "sanity-check/sanity-check-ground-truth_multiclass_30.csv")

nSubj = 1
sanity_check_multiclass.dat = NA
for(j in 1:nSubj){
  sanity_check_multiclass.temp = tibble(
    "subject" = rep(j, nMirna),
    "team" = rep(paste("team_allTP"), nMirna),
    "mirna" = mirna_present,
    "expressed" = c(rep(1, nMirna*.5), rep(-1, nMirna*.5))
  )
  sanity_check_multiclass.dat = rbind(sanity_check_multiclass.dat, sanity_check_multiclass.temp)
}

for(j in 1:nSubj){
  sanity_check_multiclass.temp = tibble(
    "subject" = rep(j, nMirna*.5),
    "team" = rep(paste("team_halfTP"), nMirna*.5),
    "mirna" = c(sample(mirna_present.split$upReg, nMirna*.25), sample(mirna_present.split$downReg, nMirna*.25)),
    "expressed" = c(rep(1, nMirna*.25), rep(-1, nMirna*.25))
  )
  sanity_check_multiclass.dat = rbind(sanity_check_multiclass.dat, sanity_check_multiclass.temp)
}

for(j in 1:nSubj){
  sanity_check_multiclass.temp = tibble(
    "subject" = rep(j, nMirna),
    "team" = rep(paste("team_allFP"), nMirna),
    "mirna" = mirna_present,
    "expressed" = c(rep(-1, nMirna*.5), rep(1, nMirna*.5))
  )
  sanity_check_multiclass.dat = rbind(sanity_check_multiclass.dat, sanity_check_multiclass.temp)
}

# for(j in 1:nSubj){
#   sanity_check_multiclass.temp = tibble(
#     "subject" = rep(j, nMirna*.5),
#     "team" = rep(paste("team_halfFP"), nMirna*.5),
#     "mirna" = c(sample(mirna_present.split$upReg, nMirna*.25), sample(mirna_present.split$downReg, nMirna*.25)),
#     "expressed" = c(rep(-1, nMirna*.25), rep(1, nMirna*.25))
#   )
#   sanity_check_multiclass.dat = rbind(sanity_check_multiclass.dat, sanity_check_multiclass.temp)
# }
# 
# for(j in 1:nSubj){
#   sanity_check_multiclass.temp = tibble(
#     "subject" = rep(j, nMirna),
#     "team" = rep(paste("team_halfTP-halfFP"), nMirna),
#     "mirna" = c(sample(mirna_present, nMirna*.5), sample(mirna_notpresent, nMirna*.5)),
#     "expressed" = c(rep(1, nMirna*.5), rep(1, nMirna*.5))
#   )
#   sanity_check_multiclass.dat = rbind(sanity_check_multiclass.dat, sanity_check_multiclass.temp)
# }
# 
# for(j in 1:nSubj){
#   sanity_check_multiclass.temp = tibble(
#     "subject" = rep(j, nMirna),
#     "team" = rep(paste("team_halfTP-halfFN"), nMirna),
#     "mirna" = mirna_present,
#     "expressed" = c(rep(1, nMirna*.5), rep(0, nMirna*.5))
#   )
#   sanity_check_multiclass.dat = rbind(sanity_check_multiclass.dat, sanity_check_multiclass.temp)
# }
# 
# for(j in 1:nSubj){
#   sanity_check_multiclass.temp = tibble(
#     "subject" = rep(j, nMirna),
#     "team" = rep(paste("team_allFN"), nMirna),
#     "mirna" = mirna_present,
#     "expressed" = rep(0, nMirna)
#   )
#   sanity_check_multiclass.dat = rbind(sanity_check_multiclass.dat, sanity_check_multiclass.temp)
# }

write_csv(sanity_check_multiclass.dat %>% drop_na(), "sanity-check/sanity-check-multiclass-teams.csv")


