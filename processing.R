## Loading in----

full_demographics <- read_csv("data/demographics3.csv")

# Main----

# Trimming
trim_full_dataset <- full_dataset %>%
  filter( !(stimulus %in% c(3,6)), !is.na(key_resp.rt)) %>%
  mutate(age_group = ifelse(participant > 130, 1,0)) %>%
  group_by(age_group) %>%
  mutate(mean_rt = mean(key_resp.rt), sd_rt = sd(key_resp.rt)) %>%
  mutate(anticipation = ifelse(key_resp.rt < 0.150, 1, 0),
         deviation = ifelse(key_resp.rt > (mean_rt + 3*sd_rt), 1, 0)) %>%
  filter(anticipation == 0, deviation == 0) %>%
  ungroup()

# RT calculation
calc_rt <- trim_full_dataset %>%
  filter( !(stimulus %in% c(3,6)), !is.na(key_resp.rt)) %>%
  group_by(StimulusList, participant) %>%
  summarise(mean_rt = mean(key_resp.rt), sd_rt = sd(key_resp.rt), trial_no = n(), mean_log_rt = mean(log(key_resp.rt)), sd_log_rt = sd(log(key_resp.rt)))

# Only the precalculated stats
calc_stats <- full_dataset %>%
  filter(!is.na(block_increment)) %>%
  group_by(StimulusList, participant) %>%
  summarise(mean_nogo_a = mean(nogo_accuracy), mean_go_a = mean(go_accuracy), mean_all_a = mean(overall_accuracy), mean_increment = mean(block_increment)) %>%
  inner_join(calc_rt, by=c("participant","StimulusList"))

calc_blocks <- full_dataset %>%
  filter(!is.na(block_increment)) %>%
  group_by(StimulusList) %>%
  summarise(mean_nogo_a = mean(nogo_accuracy), mean_go_a = mean(go_accuracy), mean_all_a = mean(overall_accuracy), mean_increment = mean(block_increment))

just_stats <- full_dataset %>%
  filter(!is.na(block_increment))

# Seems to consider the end of titration only
titration_calculation <- just_stats %>%
  filter(StimulusList == 8) %>%
  group_by(participant) %>%
  mutate(number = row_number(), max = max(number)) %>%
  filter(number > max-5) %>%
  group_by(participant) %>%
  summarise(titration_accuracy = mean(overall_accuracy, na.rm = TRUE), titration_nogo_accuracy = mean(nogo_accuracy, na.rm = TRUE), titration_go_accuracy = mean(go_accuracy, na.rm = TRUE))

# Appendix 1 titration----
full_dataset_extract <- trim_full_dataset %>%
  filter(! participant %in% c(105, 115, 128, 168, 155, 153)) %>%
  mutate(is_na_nest_row = +(lead(!is.na(block_increment), default = FALSE)), is_increment = +(!is.na(block_increment))) %>%
  mutate(real_titration = lag(titration), real_stimulus = lag(StimulusList))

full_datset_rts <- trim_full_dataset %>%
  group_by(StimulusList, participant, titration) %>%
  summarise(mean_rt = mean(key_resp.rt), sd_rt = sd(key_resp.rt), trial_no = n(),
            mean_log_rt = mean(log(key_resp.rt)), sd_log_rt = sd(log(key_resp.rt)))

full_dataset_other_stats <- full_dataset_extract %>%
  filter(is_increment == TRUE) %>%
  mutate(titration = real_titration, StimulusList = real_stimulus) %>%
  select(participant, titration, StimulusList, block_increment, overall_accuracy, nogo_accuracy, go_accuracy)

psychophysics_comparison <- full_datset_rts %>%
  inner_join(full_dataset_other_stats, by=c("participant", "titration", "StimulusList"))

psychophysics_time_summary <- psychophysics_comparison %>%
  mutate(age_group = ifelse(participant > 140, 1, 0)) %>%
  group_by(age_group, participant, titration, StimulusList) %>%
  summarise(mean_increment = mean(block_increment), mean_accuracy = mean(overall_accuracy), mean_nogo = mean(nogo_accuracy), mean_log_rt = mean(mean_log_rt), mean_rt = mean(mean_rt)) %>%
  filter(StimulusList > 4)

psychophysics_comparison_summary <- psychophysics_comparison %>%
  mutate(age_group = ifelse(participant > 140, 1, 0)) %>%
  group_by(age_group, participant, StimulusList) %>%
  summarise(mean_increment = mean(block_increment), mean_accuracy = mean(overall_accuracy), mean_nogo = mean(nogo_accuracy), mean_rt = mean(mean_rt), mean_log_rt = mean(mean_log_rt)) %>%
  filter(StimulusList > 4) %>%
  mutate(StimulusList = recode(
    StimulusList,
    `5` = 1,
    `6` = 2,
    `7` = 3,
    `8` = 4,
    `9` = 5)) %>%
  mutate(age_group = factor(recode(
   age_group,
  `0` = "young",
 `1` = "older")))

psychophysics_comparison_overall <- psychophysics_comparison %>%
  filter(StimulusList > 4) %>%
  filter(StimulusList < 9) %>%
  mutate(age_group = ifelse(participant > 140, 1, 0)) %>%
  group_by(age_group, participant) %>%
  summarise(trial_no = sum(trial_no), mean_increment = mean(block_increment), mean_accuracy = mean(overall_accuracy), mean_nogo = mean(nogo_accuracy), mean_log_rt = mean(mean_log_rt), mean_rt = mean(mean_rt), sd_rt = sd(mean_rt))

psychophysics_comparison_overall %>%
  group_by(age_group) %>%
  summarise(mean = mean(mean_nogo), sd = sd(mean_nogo), min = min(mean_nogo), max = max(mean_nogo))

t.test(filter(psychophysics_comparison_overall, age_group == 0) %>% pull(mean_nogo), filter(psychophysics_comparison_overall, age_group == 1) %>% pull(mean_nogo), paired=FALSE)

t.test(filter(psychophysics_comparison_overall, age_group == 0) %>% pull(mean_accuracy), filter(psychophysics_comparison_overall, age_group == 1) %>% pull(mean_accuracy), paired=FALSE)

psychophysics_comparison_overall %>%
  group_by(age_group) %>%
  summarise(mean = mean(trial_no), sd = sd(trial_no), min = min(trial_no), max = max(trial_no))

psychophysics_comparison_overall %>%
  group_by(age_group) %>%
  summarise(mean = mean(mean_increment), sd = sd(mean_increment), min = min(mean_increment), max = max(mean_increment))

full_rts_summary <- full_dataset_extract %>%
  filter( !(stimulus %in% c(3,6)), !is.na(key_resp.rt)) %>%
  filter( key_resp.rt>0.150) %>%
  filter(StimulusList > 4) %>%
  filter(StimulusList < 9) %>%
  mutate(age_group = ifelse(participant > 140, 1, 0)) %>%
  group_by(age_group) %>%
  summarise(mean_rt = mean(key_resp.rt), sd_rt = sd(key_resp.rt), min_rt=min(key_resp.rt), max_rt=max(key_resp.rt),
            mean_log_rt = mean(log(key_resp.rt)), sd_log_rt = sd(log(key_resp.rt)))




psychophysics_comparison_pse <- psychophysics_comparison %>%
  filter(StimulusList > 4) %>%
  filter(StimulusList < 9) %>%
  mutate(age_group = ifelse(participant > 140, 1, 0)) %>%
  group_by(block_increment, age_group) %>%
  summarise(trial_no = sum(trial_no), mean_accuracy = mean(overall_accuracy), mean_nogo = mean(nogo_accuracy), mean_log_rt = mean(mean_log_rt), mean_rt = mean(mean_rt)) %>%
  mutate(age_group = factor(recode(
    age_group,
    `0` = "young",
    `1` = "older"), levels=c("young","older")))

# Preparation for plots etc..
psychophysics_comparison_summary_no9 <- psychophysics_comparison_summary %>%
  filter(StimulusList != 5) %>%
  #  mutate(StimulusList = recode(
  #    StimulusList,
  #    `5` = 1,
  #    `6` = 2,
  #    `7` = 3,
  #    `8` = 4)) %>%
  mutate(age_group = factor(recode(
    age_group,
    `0` = "young",
    `1` = "older")))

psychophysics_median <- psychophysics_comparison_summary_no9 %>%
  filter(StimulusList == 1) %>%
  group_by() %>%
  mutate(median = median(mean_accuracy), split_group = case_when(mean_accuracy >= median ~ "better", mean_accuracy < median ~ "worse")) %>%
  select(participant, split_group) %>%
  inner_join(psychophysics_comparison_summary_no9, by = "participant")

