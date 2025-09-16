# age_group, PSE_increment, IMI_1, IMI_2, IMI_3, MFI_1, MFI_2, MFI_3, MFI_4, MFI_5, 
# motivation_accuracy, titration_accuracy, motivation_improvement, baseline_rt, baseline_nogo, chi_square_table

# Preparation

library(ez)
library(ggpubr)
library(ggsignif)
library(lsr)
set.seed(2025)

library(BayesFactor)

core_stats %>%
  group_by(age_group) %>%
  summarise(mean = mean(IMI_i), sd = sd(IMI_i))

std <- function(x) sd(x)/sqrt(length(x))

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(baseline_nogo), se = std(baseline_nogo))

# Titration window length

  full_datset_compare <- full_dataset %>%
  filter(StimulusList > 4) %>%
  filter(StimulusList < 9) %>%
  mutate(age_group = ifelse(participant > 140, 1, 0)) %>%
  group_by(age_group, participant) %>%
  summarise(mean_rt = mean(key_resp.rt), sd_rt = sd(key_resp.rt), trial_no = n(),
            mean_log_rt = mean(log(key_resp.rt)), sd_log_rt = sd(log(key_resp.rt)))
  
full_datset_compare_rt <- full_dataset %>%
  filter(!(stimulus %in% c(3,6)), !is.na(key_resp.rt)) %>%
  filter(StimulusList > 4) %>%
  filter(StimulusList < 9) %>%
  mutate(age_group = ifelse(participant > 140, 1, 0)) %>%
  group_by(age_group, participant) %>%
  summarise(mean_rt = mean(key_resp.rt), sd_rt = sd(key_resp.rt), trial_no = n(),
            mean_log_rt = mean(log(key_resp.rt)), sd_log_rt = sd(log(key_resp.rt)))
  
  full_datset_stats <- full_datset_compare %>%
    group_by(age_group) %>%
    summarise(min = min(trial_no), max = max(trial_no), mean = mean(trial_no), sd = sd(trial_no))


t.test(filter(full_datset_compare, age_group == 0) %>% pull(trial_no), filter(full_datset_compare, age_group == 1) %>% pull(trial_no), paired=FALSE)

cohensD(trial_no ~ age_group, data = full_datset_compare)

t.test(filter(full_datset_compare_rt, age_group == 0) %>% pull(mean_log_rt), filter(full_datset_compare_rt, age_group == 1) %>% pull(mean_log_rt), paired=FALSE)

cohensD(mean_log_rt ~ age_group, data = full_datset_compare_rt)

# Average window length

win_datset_stats <- psychophysics_comparison_overall %>%
  group_by(age_group) %>%
  summarise(min = min(mean_increment), max = max(mean_increment), mean = mean(mean_increment), sd = sd(mean_increment))

t.test(filter(psychophysics_comparison_overall, age_group == 0) %>% pull(mean_increment), filter(psychophysics_comparison_overall, age_group == 1) %>% pull(mean_increment), paired=FALSE)

cohensD(mean_increment ~ age_group, data = psychophysics_comparison_overall)

# Baseline testing----

#one-sided t-test: age_group x baseline_nogo. one-sided t-test: age_group x baseline_r
test_1 <- t.test(baseline_nogo ~ as.factor(age_group),core_stats, alternative="less")
test_1

cohensD(baseline_nogo ~ age_group, data = core_stats)

test_1rt <- t.test(baseline_rt ~ as.factor(age_group), core_stats, alternative="less")
test_1rt

cohensD(baseline_rt ~ age_group, data = core_stats)

# Bayes follow-up
btest_1rt <- ttestBF(formula = baseline_rt ~ age_group,data = core_stats)
btest_1rt


test_2 <- t.test(PSE_increment ~ as.factor(age_group),core_stats, alternative="greater")
test_2


cohensD(PSE_increment ~ age_group, data = core_stats)

# Subjective Motivation----
test_3i <- t.test(IMI_i ~ as.factor(age_group),core_stats, alternative="less" )
test_3i

cohensD(IMI_i ~ age_group, data = core_stats)

test_3e <- t.test(IMI_e ~ as.factor(age_group),core_stats, alternative="less")
test_3e

cohensD(IMI_e ~ age_group, data = core_stats)

test_3f <- t.test(IMI_f ~ as.factor(age_group),core_stats, alternative="less")
test_3f

cohensD(IMI_f ~ age_group, data = core_stats)

test_3 <- t.test(IMI_all ~ as.factor(age_group),core_stats, alternative="less")
test_3

test_4b <- t.test(vas_m ~ as.factor(age_group),core_stats, alternative="greater")
test_4b

cohensD(vas_m ~ age_group, data = core_stats)

# Motivation effects----
anova_stats <- core_stats %>%
  select(participant, age_group, motivation_accuracy, titration_accuracy) %>%
  pivot_longer(cols=motivation_accuracy:titration_accuracy, names_to = "block", values_to="all_accuracy") %>%
  mutate(age_group = as.factor(age_group),
         block = as.factor(block),
         participant = as.factor(participant))

test_4a <- ezANOVA(
  anova_stats
  , dv = all_accuracy
  , wid = participant
  , within = block
  , between = age_group
  
)
test_4a

btest_4a <- anovaBF(formula = all_accuracy ~ block * age_group,
  data = anova_stats,
  whichModels="all",
  whichRandom="participant",
  rscaleFixed="medium"
)
btest_4a

max(btest_4a)

btest_4a/max(btest_4a)

btest_4a[6] / btest_4a[2]

anova_rt_stats <- core_stats %>%
  select(participant, age_group, motivation_rt, titration_rt) %>%
  pivot_longer(cols=motivation_rt:titration_rt, names_to = "block", values_to="all_rt") %>%
  mutate(age_group = as.factor(age_group),
         block = as.factor(block),
         participant = as.factor(participant))

test_4e <- ezANOVA(
  anova_rt_stats
  , dv = all_rt
  , wid = participant
  , within = block
  , between = age_group
  
)
test_4e

anova_nogo_stats <- core_stats %>%
  select(participant, age_group, motivation_nogo_accuracy, titration_nogo_accuracy) %>%
  pivot_longer(cols=titration_nogo_accuracy:motivation_nogo_accuracy, names_to = "block", values_to="all_nogo") %>%
  mutate(age_group = as.factor(age_group),
         block = as.factor(block),
         participant = as.factor(participant))

test_4ee <- ezANOVA(
  anova_nogo_stats
  , dv = all_nogo
  , wid = participant
  , within = block
  , between = age_group
  
)
test_4ee

btest_4ee <- anovaBF(formula = all_nogo ~ block * age_group,
                    data = anova_nogo_stats,
                    whichModels="all",
                    whichRandom="participant",
                    rscaleRandom="medium" 
)
btest_4ee

max(btest_4ee)

# btest_4ee/max(btest_4ee)
# try the theory driven one
btest_4ee[6] / btest_4ee[2]


# Median Titration with Bayes----
psychophysics_median %>%
  filter(StimulusList == 1) %>%
  summarise(mean =mean(mean_accuracy), sd = sd(mean_accuracy))

psychophysics_median %>%
  filter(StimulusList == 1) %>%
  group_by(split_group) %>%
  summarise(mean =mean(mean_accuracy), sd = sd(mean_accuracy))


model <- lm(mean_accuracy ~ split_group*StimulusList, psychophysics_median)
summary(model)

## effect size
r_sq_adj <- summary(model)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2

model <- lm(mean_nogo ~ split_group*StimulusList, psychophysics_median)
summary(model)

## effect size
r_sq_adj <- summary(model)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2

model <- lm(mean_log_rt ~ split_group*StimulusList, psychophysics_median)
summary(model)

## effect size
r_sq_adj <- summary(model)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2

BF_nogo_regression_all <- generalTestBF(
  formula = mean_nogo ~ split_group + StimulusList + split_group:StimulusList,
  data = psychophysics_median
)

BF_nogo_regression_all


BF_rt_regression_all <- generalTestBF(
  formula = mean_log_rt ~ split_group + StimulusList + split_group:StimulusList,
  data = psychophysics_median
)

BF_rt_regression_all

model <- lm(mean_increment ~ split_group*StimulusList, psychophysics_median)
summary(model)

## effect size
r_sq_adj <- summary(model)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2

## effect size
r_sq_adj <- summary(model)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2


test_4a <- ezANOVA(
  anova_stats
  , dv = all_accuracy
  , wid = participant
  , within = block
  , between = age_group
  
)
test_4a

btest_4a <- anovaBF(formula = all_accuracy ~ block * age_group,
                    data = anova_stats,
                    whichModels="all",
                    whichRandom="participant",
                    rscaleFixed="medium"
)
btest_4a

max(btest_4a)

# When age is factored in, it still turns out okay
# Posthocs

# Replace metric: mean_log_rt, mean_nogo, mean_accuracy 

t.test(filter(psychophysics_median, StimulusList == 1 & split_group == "better") %>% pull(mean_nogo),
       filter(psychophysics_median, StimulusList == 1 & split_group == "worse") %>% pull(mean_nogo), paired=FALSE)

t.test(filter(psychophysics_median, StimulusList == 2 & split_group == "better") %>% pull(mean_nogo),
       filter(psychophysics_median, StimulusList == 2 & split_group == "worse") %>% pull(mean_nogo), paired=FALSE)

t.test(filter(psychophysics_median, StimulusList == 3 & split_group == "better") %>% pull(mean_nogo),
       filter(psychophysics_median, StimulusList == 3 & split_group == "worse") %>% pull(mean_nogo), paired=FALSE)

t.test(filter(psychophysics_median, StimulusList == 4 & split_group == "better") %>% pull(mean_nogo),
       filter(psychophysics_median, StimulusList == 4 & split_group == "worse") %>% pull(mean_nogo), paired=FALSE)

# Chi-square----

# 5) A Pearson’s chi-square test: age_group x chi_square_table
preferences <- chi_square_table %>%
  pivot_longer(cols=mot_1:mot_7, names_to = "motivation_type", values_to = "present") %>%
  group_by(age_group, motivation_type) %>%
  summarise(number = sum(present)) %>%
  pivot_wider(names_from = age_group, values_from = number) %>%
  mutate(young=as.numeric(`0`), older=as.numeric(`1`), motivation_type=as.factor(motivation_type)) %>%
  select(-`0`,-`1`)

preferences_long <- preferences %>%
  pivot_longer(young:older, names_to = "age_group", values_to = "number") %>%
  mutate(age_group = as.factor(age_group), number = as.factor(number)) %>%
  as.data.frame()

test_5 <- chisq.test(preferences$young, preferences$older)
test_5


sqrt((test_5[["statistic"]] / 50)/1)

# Regressions----

# 6) multiple linear regression: age_group x PSE_increment x MFI
test_6 <- lm(PSE_increment ~ age_group*mfi_all, core_stats)
summary(test_6)

test_6 <- lm(PSE_increment ~ age_group*mfi_gf, core_stats)
summary(test_6)

test_6 <- lm(PSE_increment ~ age_group*mfi_pf, core_stats)
summary(test_6)

test_6 <- lm(PSE_increment ~ age_group*mfi_ra, core_stats)
summary(test_6)

test_6 <- lm(PSE_increment ~ age_group*mfi_rm, core_stats)
summary(test_6)

test_6 <- lm(PSE_increment ~ age_group*mfi_mf, core_stats)
summary(test_6)

test_6e <- lm(baseline_nogo ~ age_group*mfi_all, core_stats)
summary(test_6e)

test_6e <- lm(motivation_accuracy ~ age_group*mfi_all, core_stats)
summary(test_6e)

test_6e <- lm(titration_accuracy ~ age_group*mfi_all, core_stats)
summary(test_6e)

core_stats %>%
  group_by(age_group) %>%
  summarise(mean_all = mean(mfi_all), sd_all = sd(mfi_all),
            mean_gf = mean(mfi_gf), sd_gf = sd(mfi_gf),
            mean_pf = mean(mfi_pf), sd_pf = sd(mfi_pf),
            mean_ra = mean(mfi_ra), sd_ra = sd(mfi_ra),
            mean_rm = mean(mfi_rm), sd_rm = sd(mfi_rm),
            mean_mf = mean(mfi_mf), sd_mf = sd(mfi_mf))

# 7) multiple linear regression: age_group x MFI x IMI
test_7 <- lm(IMI_all ~ age_group*mfi_all, core_stats)
summary(test_7)

## effect size
r_sq_adj <- summary(test_7)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2

# d prime----
dprime_full_stats <- full_dataset %>%
  filter(StimulusList > 7) %>%
  mutate(age_group = ifelse(participant > 130, 1,0)) %>%
  group_by(age_group) %>%
  mutate(mean_rt = mean(key_resp.rt, na.rm = TRUE),
         sd_rt = sd(key_resp.rt, na.rm = TRUE),
         na = if_any(key_resp.rt, is.na),
         anticipation = ifelse(key_resp.rt < 0.150 & na == FALSE, 1, 0),
         deviation = ifelse(key_resp.rt > (mean_rt + 3*sd_rt) & na == FALSE, 1, 0)
         ) %>%
  filter(anticipation != 1, deviation != 1) %>%
  mutate(trial_type = case_when(
    is_correct == 1 & na == FALSE ~ "Hit",
    is_correct == 0 & na == TRUE ~ "Miss",
    is_correct == 1 & na == TRUE ~ "Correct_Rejection",
    is_correct == 0 & na == FALSE ~ "False_Alarm",
    .default = "Problem")
  )  %>%
  filter(trial_type != "Problem") %>%
  group_by(participant, StimulusList) %>%
  mutate(trial_max = max(trial_no) - 124) %>%
  filter(StimulusList == 9 | (StimulusList == 8 & trial_no > trial_max ))
  
  
dprime_stats <- dprime_full_stats %>%
  group_by(participant, StimulusList, trial_type) %>%
  summarise(number = n()) %>%
  ungroup() %>%
  complete(participant, StimulusList, trial_type) %>%
  mutate(number = replace_na(number, 0))

dprime_stats_wider <- dprime_stats %>%
  pivot_wider(names_from = trial_type, values_from = number)
  


library(psycho)

indices <- psycho::dprime(
  n_hit = dprime_stats_wider$Hit,
  n_fa = dprime_stats_wider$False_Alarm,
  n_miss = dprime_stats_wider$Miss,
  n_cr = dprime_stats_wider$Correct_Rejection,
  adjusted = TRUE
)

dprimes <- dprime_stats_wider %>%
  mutate(dprime = indices$dprime) %>%
  mutate(age_group = ifelse(participant < 130, "young", "older")) %>%
  mutate(StimulusList = ifelse(StimulusList == 8, "titrated", "motivated")) %>%
  mutate(participant = as.factor(participant)) %>%
  select(participant, StimulusList, age_group, dprime)

dprimes %>%
  group_by(StimulusList, age_group) %>%
  summarise(mean_d = mean(dprime), sd_d = sd(dprime))

anova_SD <- ezANOVA(
  dprimes,
  dv = dprime,
  wid = participant,
  within = StimulusList,
  between = age_group
  
)
anova_SD
