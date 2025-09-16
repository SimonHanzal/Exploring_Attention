#set up
#Paths will need to be changed on experimental machine

#directories

#loading both options
# datasets <- list.files("C:/Users/simonha/OneDrive - University of Glasgow/research/experiment_3/analysis_3/R3/experiment_3/data", full.names = F)

# at home
datasets <- list.files("C:/Users/hanza/OneDrive - University of Glasgow/research/experiment_3/analysis_3/R3/experiment_3/data", full.names = F)

#go accuracy will be added

full_dataset <- tibble(participant = character(),
                       session = character(),
                       trial_no = numeric(),
                       titration = numeric(),
                       StimulusList = numeric(),
                       stimulus = character(),
                       is_correct = numeric(),
                       key_resp.rt = numeric(),
                       block_increment = numeric(),
                       overall_accuracy = numeric(),
                       nogo_accuracy = numeric(),
                       go_accuracy = numeric(),
)

len <- length(datasets)

for (d in 1:len) {
  df <- read_csv(paste("data/",datasets[d], sep="")) %>%
    filter(is.na(WelcomeScreen.started)) %>%
    filter(is.na(StartClock.started)) %>%
    #filter(!is.na(stimulus) | ) %>%
    mutate(trial_no = row_number()) %>%
    mutate(StimulusList = str_sub(StimulusList, -5, -5)) %>%
    mutate(stimulus = as.character(stimulus),
           titration = as.numeric(titration),
           StimulusList = as.numeric(StimulusList),
           is_correct = as.numeric(is_correct),
           key_resp.rt = as.numeric(key_resp.rt),
           overall_accuracy = as.numeric(overall_accuracy),
           nogo_accuracy = as.numeric(nogo_accuracy),
           go_accuracy = as.numeric(go_accuracy),
           participant = as.character(participant),
           session = as.character(session),
           trial_no = as.numeric(trial_no)) %>%
    select(participant, session, trial_no, titration, StimulusList, stimulus, is_correct, key_resp.rt, block_increment, overall_accuracy, nogo_accuracy, go_accuracy)
    
  full_dataset <- full_join(full_dataset, df)
}
