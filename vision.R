#set up
#Paths will need to be changed on experimental machine

#directories

#loading both options
# datasets <- list.files("C:/Users/simonha/OneDrive - University of Glasgow/research/experiment_3/analysis_3/R3/experiment_3/vision_data", full.names = F)
# At home

datasets <- list.files("C:/Users/hanza/OneDrive - University of Glasgow/research/experiment_3/analysis_3/R3/experiment_3/vision_data", full.names = F)
#go accuracy will be added

vision_dataset <- tibble(participant = character(),
                       session = character(),
                       stim_location = character(),
                       Reaction.corr = numeric(),
                       trial_no = numeric()
)

len <- length(datasets)

for (d in 1:len) {
  df <- read_csv(paste("vision_data/",datasets[d], sep="")) %>%
    filter(!is.na(stim_location)) %>%
    mutate(trial_no = row_number()) %>%
    mutate(participant = as.character(participant),
           session = as.character(session),
           stim_location = as.character(stim_location),
           Reaction.corr = as.numeric(Reaction.corr)) %>%
    select(participant, session, stim_location, Reaction.corr, trial_no)
  
  vision_dataset <- full_join(vision_dataset, df)
}

filtering_vision <- vision_dataset %>%
  filter(! participant %in% c(105, 115, 128, 168, 155, 153)) %>%
  mutate(age_group = ifelse(participant > 140, 1, 0))

calc_visio <- filtering_vision %>%
  group_by(participant, age_group) %>%
  summarise(mean_acc = mean(Reaction.corr))

t.test(filter(calc_visio, age_group == 0) %>% pull(mean_acc), filter(calc_visio, age_group == 1) %>% pull(mean_acc), paired=FALSE)

cohensD(baseline_rt ~ age_group, data = core_stats)

