library(psych)

demo_addition <- full_demographics %>%
  mutate(age_group = ifelse(age > 50, 1, 0),
         imi_3R = (8-imi_3), imi_4R = (8-imi_4), imi_9R = (8-imi_9), imi_12R = (8-imi_12),
         IMI_i = imi_1+imi_2+imi_3R+imi_4R+imi_5+imi_6+imi_7,
         IMI_e = imi_8+imi_9R+imi_10+imi_11+imi_12R,
         IMI_f = imi_13+imi_14+imi_15+imi_16+imi_17+imi_18+imi_19,
         IMI_all = IMI_i+IMI_e+IMI_f,
         mfi_2R = (6-mfi_2),mfi_5R = (6-mfi_5),mfi_9R = (6-mfi_9),mfi_10R = (6-mfi_10),
         mfi_13R = (6-mfi_13),mfi_14R = (6-mfi_14),mfi_16R = (6-mfi_16),mfi_17R = (6-mfi_17),
         mfi_18R = (6-mfi_18), mfi_19R = (6-mfi_19),
         mfi_gf=mfi_1+mfi_5R+mfi_12+mfi_16R,
         mfi_pf=mfi_2R+mfi_8+mfi_14R+mfi_20,
         mfi_ra=mfi_3+mfi_6+mfi_10R+mfi_17R,
         mfi_rm=mfi_4+mfi_9R+mfi_15+mfi_18R,
         mfi_mf=mfi_7+mfi_11+mfi_13R+mfi_19R,
         mfi_all=mfi_gf+mfi_pf+mfi_ra+mfi_rm+mfi_mf,
         moca=moca_1+moca_2+moca_3+moca_4+moca_5+moca_6+moca_7+moca_8+moca_9+moca_10+
              moca_11+moca_12+moca_13+moca_14+moca_15+moca_16+moca_17+moca_18+moca_19+moca_20+
              moca_21+moca_22+moca_23+moca_24+moca_25+moca_26+moca_27+moca_28+moca_29+moca_30+moca_31) %>%
  filter(! participant %in% c(105, 115, 128, 168, 155, 153))

chi_square_table <- demo_addition %>%
  select(participant, age_group, mot_1:mot_10) %>%
  filter(! participant %in% c(105, 115, 128, 168, 155, 153))

demo_clean <- demo_addition %>%
  select(participant:vision, vas_m, age_group, IMI_i:IMI_all, mfi_gf:moca) %>%
  mutate(participant = as.character(participant))

imi_i_alpha <- select(demo_addition, imi_1,imi_2,imi_3R,imi_4R,imi_5,imi_6,imi_7)
imi_e_alpha <- select(demo_addition, imi_8,imi_9R,imi_10,imi_11,imi_12R)
imi_f_alpha <- select(demo_addition, imi_13,imi_14,imi_15,imi_16,imi_17,imi_18,imi_19)

c_alpha_imi_i <- psych::alpha(imi_i_alpha)$total$raw_alpha
c_alpha_imi_e <- psych::alpha(imi_e_alpha)$total$raw_alpha
c_alpha_imi_f <- psych::alpha(imi_f_alpha)$total$raw_alpha

mfi_gf_alpha = select(demo_addition, mfi_1,mfi_5R,mfi_12,mfi_16R)
mfi_pf_alpha = select(demo_addition, mfi_2R,mfi_8,mfi_14R,mfi_20)
mfi_ra_alpha = select(demo_addition, mfi_3,mfi_6,mfi_10R,mfi_17R)
mfi_rm_alpha = select(demo_addition, mfi_4,mfi_9R,mfi_15,mfi_18R)
mfi_mf_alpha = select(demo_addition, mfi_7,mfi_11,mfi_13R,mfi_19R)

c_alpha_mfi_gf <- psych::alpha(mfi_gf_alpha)$total$raw_alpha
c_alpha_mfi_pf <- psych::alpha(mfi_pf_alpha)$total$raw_alpha
c_alpha_mfi_ra <- psych::alpha(mfi_ra_alpha)$total$raw_alpha
c_alpha_mfi_rm <- psych::alpha(mfi_rm_alpha)$total$raw_alpha
c_alpha_mfi_mf <- psych::alpha(mfi_mf_alpha)$total$raw_alpha


# age_group, PSE_increment, IMI_1, IMI_2, IMI_3, MFI_1, MFI_2, MFI_3, MFI_4, MFI_5, 
# motivation_accuracy, titration_accuracy, motivation_improvement, baseline_rt, baseline_nogo, chi_square_table

pse_increment <- calc_stats %>%
  filter(StimulusList == 9) %>%
  ungroup() %>%
  mutate(PSE_increment = mean_increment, participant = as.character(participant)) %>% # calculate better
  select(participant, PSE_increment)

motivation_accuracy <- calc_stats %>%
  filter(StimulusList == 9) %>%
  ungroup() %>%
  mutate(motivation_accuracy = mean_all_a, participant = as.character(participant)) %>%
  select(motivation_accuracy, participant)

motivation_nogo_accuracy <- calc_stats %>%
  filter(StimulusList == 9) %>%
  ungroup() %>%
  mutate(motivation_nogo_accuracy = mean_nogo_a, participant = as.character(participant)) %>%
  select(motivation_nogo_accuracy, participant)

motivation_go_accuracy <- calc_stats %>%
  filter(StimulusList == 9) %>%
  ungroup() %>%
  mutate(motivation_go_accuracy = mean_go_a, participant = as.character(participant)) %>%
  select(motivation_go_accuracy, participant)


baseline_rt <- calc_stats %>%
  filter(StimulusList == 0) %>%
  ungroup() %>%
  mutate(baseline_rt = mean_log_rt, participant = as.character(participant), baseline_plot_rt = mean_rt) %>% #mean_log_rt
  select(baseline_rt, participant, baseline_plot_rt)

motivation_rt <- calc_stats %>%
  filter(StimulusList == 9) %>%
  ungroup() %>%
  mutate(motivation_rt = mean_log_rt, participant = as.character(participant), motivation_plot_rt= mean_rt) %>%
  select(motivation_rt, participant, motivation_plot_rt)

titration_rt <- calc_stats %>%
  filter(StimulusList == 8) %>%
  ungroup() %>%
  mutate(titration_rt = mean_log_rt, participant = as.character(participant), titration_plot_rt= mean_rt) %>%
  select(titration_rt, participant, titration_plot_rt)

titration_go_accuracy <- calc_stats %>%
  filter(StimulusList == 8) %>%
  ungroup() %>%
  mutate(titration_go_accuracy = mean_go_a, participant = as.character(participant)) %>%
  select(titration_go_accuracy, participant)

baseline_nogo <- calc_stats %>%
  filter(StimulusList == 0) %>%
  ungroup() %>%
  mutate(baseline_nogo = mean_nogo_a, participant = as.character(participant)) %>%
  select(baseline_nogo, participant)

core_stats <- pse_increment %>%
  inner_join(motivation_accuracy, by="participant") %>%
  inner_join(motivation_go_accuracy, by="participant") %>%
  inner_join(titration_calculation, by="participant") %>%
  inner_join(motivation_nogo_accuracy, by="participant") %>%
  inner_join(baseline_rt, by="participant") %>%
  inner_join(baseline_nogo, by="participant") %>%
  inner_join(titration_rt, by="participant") %>%
  inner_join(motivation_rt, by="participant") %>%
  inner_join(demo_clean, by="participant")

# Exclusions

cut_offs <- core_stats %>%
  mutate(age_group = ifelse(age > 30, 1 ,0)) %>%
  group_by(age_group) %>%
  summarise(mean_moca = mean(moca), sd_moca = sd(moca), minimum = mean_moca - 2*sd_moca)

sleep_cut_offs <- core_stats %>%
  mutate(age_group = ifelse(age > 30, 1 ,0)) %>%
  group_by(age_group) %>%
  summarise(mean_sleep = mean(sleep), sd_sleep = sd(sleep), minimum = mean_sleep - 2*sd_sleep)
# MOCA cut-off is bellow 24 for both groups, but 21 was pre-registed. Yet:
#128, #153, #155. Furthermore - 128 was clearly confused, 155 had clear visual
#defficiency reflected in questionnaire answers and visual field test
#153 Did not understand the questionnaire, and was quite old at 96
#168 -> caffeine was at 6, which is way too high
#105 -> sleep was at 4, under the defined criteria
#115 -> Suspiciously low performance, but could be ok to include

core_stats <- core_stats %>%
  filter(! participant %in% c(105, 115, 128, 168, 155, 153))# remove participants

core_stats %>%
  group_by(age_group) %>%
  summarise(mean = mean(age), sd = sd(age), min = min(age), max = max(age))

core_stats %>%
  summarise(mean = mean(sleep), sd = sd(sleep), min = min(sleep), max = max(sleep))

core_stats %>%
  summarise(mean = mean(caffeine), sd = sd(caffeine), min = min(caffeine), max = max(caffeine))

core_stats %>%
  group_by(age_group) %>%
  summarise(mean = mean(moca), sd = sd(moca), min = min(moca), max = max(moca))

sex <- core_stats %>%
  count(age_group, gender)
sex

core_stats %>%
  count(handedness)

core_stats %>%
  count(smoker)
# Description----

rts <- trim_full_dataset %>%
  select(participant, key_resp.rt)

# How many were there before
before_rts <- full_dataset %>%
  filter( !(stimulus %in% c(3,6)), !is.na(key_resp.rt)) %>%
  summarise(number = n ())

# How many trials were removed as anticipation and deviation
removals <- full_dataset %>%
  filter( !(stimulus %in% c(3,6)), !is.na(key_resp.rt)) %>%
  mutate(age_group = ifelse(participant > 130, 1,0)) %>%
  group_by(age_group) %>%
  mutate(mean_rt = mean(key_resp.rt), sd_rt = sd(key_resp.rt)) %>%
  mutate(anticipation = ifelse(key_resp.rt < 0.150, 1, 0),
         deviation = ifelse(key_resp.rt > (mean_rt + 3*sd_rt), 1, 0)) %>%
  summarise(anticipations = sum(anticipation), deviations = sum(deviation))


skew(rts$key_resp.rt)

ggplot(rts, aes(key_resp.rt)) + geom_histogram(binwidth = 0.01)

log_rts <- trim_full_dataset %>%
  mutate(key_resp.rt = log(key_resp.rt)) %>%
  select(participant, key_resp.rt)

skew(log_rts$key_resp.rt)
  
ggplot(log_rts, aes(key_resp.rt)) + geom_histogram(binwidth = 0.01)

# How about moca scores?
test_moca <- t.test(moca ~ as.factor(age_group),core_stats)
test_moca
cohensD(moca ~ age_group, data = core_stats)
