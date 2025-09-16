### Appendix 1 plots ----

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

fig_2_D <- ggplot(psychophysics_comparison_summary_no9, aes(x = StimulusList, y = mean_rt*1000, colour = as.factor(age_group))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participant), alpha = 0.5) +
  geom_smooth(method = "lm", aes(fill = as.factor(age_group))) +
  theme_minimal() +
  labs(x = "Titration block", y = "RT (ms)", color = "Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))# +
#ylim(400,850)

model <- lm(mean_log_rt ~ age_group*StimulusList, psychophysics_comparison_summary_no9)
summary(model)

#ggplot(psychophysics_comparison_summary, aes(x = StimulusList, y = mean_accuracy, colour = as.factor(age_group))) +
#  geom_point() +
#  geom_line(aes(group = participant)) +
#  geom_smooth(method = "lm")

fig_2_C <- ggplot(psychophysics_comparison_summary_no9, aes(x = StimulusList, y = mean_nogo*100, colour = as.factor(age_group))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participant), alpha = 0.5) +
  geom_smooth(method = "lm", aes(fill = as.factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titration block", y = "Nogo Accuracy (%)", color = "Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  ylim(30,100)

model <- lm(mean_nogo ~ age_group*StimulusList, psychophysics_comparison_summary_no9)
summary(model)

model <- lm(mean_accuracy ~ age_group*StimulusList, psychophysics_comparison_summary_no9)
summary(model)



fig_2_F <- ggplot(psychophysics_comparison_summary_no9, aes(x = StimulusList, y = mean_increment*1000, colour = as.factor(age_group))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participant), alpha = 0.5) +
  geom_smooth(method = "lm", aes(fill = as.factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titration block", y = "Response window length (ms)", color = "Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  ylim(300,1850)

model <- lm(mean_increment ~ age_group*StimulusList, psychophysics_comparison_summary_no9)
summary(model)

# Stats
summary_rt <- full_datset_compare_rt %>%
  group_by(age_group) %>%
  summarise(mean = mean(mean_rt), sd = mean(sd_rt))
 

summary_nogo <- full_dataset %>%
  filter(!is.na(block_increment)) %>%
  mutate(age_group = ifelse(participant > 130, 1,0)) %>%
  group_by(age_group) %>%
  summarise(mean_nogo_a = mean(nogo_accuracy), sd_nogo_a = sd(nogo_accuracy))

summary_nogo <- full_datset_compare_rt %>%
  group_by(age_group) %>%
  summarise(mean = mean(mean_nogo), sd = mean(sd_rt))


fig_2_G <- ggplot(psychophysics_comparison_summary_no9, aes(x = StimulusList, y = mean_accuracy*100, colour = as.factor(age_group))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participant), alpha = 0.5) +
  geom_smooth(method = "lm", aes(fill = as.factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titration block", y = "Overall Accuracy (%)", color = "Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  ylim(80,100)

# This may only be three plots
 
p6 <- ggarrange(fig_2_G, fig_2_C, fig_2_D, fig_2_F, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"),common.legend = TRUE, legend = "right")
#ggsave(p, filename = 'plots/fig2_n.png', dpi = 300, type = 'cairo', width = 12, height = 18, bg = "white", units = 'in')
p6

fig_11_A <- ggplot(core_stats, aes(x = (motivation_accuracy - titration_accuracy)*100, y = vas_m, colour=factor(age_group, levels=c(0,1), labels=c("young", "older")))) +
  geom_point() +
  geom_line(aes(group = participant)) +
  geom_smooth(method = "lm", aes(fill = as.factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Accuracy improvement (%)", y = "VAS Monetary motivation", colour="Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))

fig_11_B <- ggplot(core_stats, aes(x = (motivation_accuracy - titration_accuracy)*100, y = IMI_i, colour=factor(age_group, levels=c(0,1), labels=c("young", "older")))) +
  geom_point() +
  geom_line(aes(group = participant)) +
  geom_smooth(method = "lm", aes(fill = as.factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Accuracy improvement (%)", y = "IMI - interest", colour="Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))

fig_11_C <- ggplot(core_stats, aes(x = (motivation_accuracy - titration_accuracy)*100, y = IMI_f, colour=factor(age_group, levels=c(0,1), labels=c("young", "older")))) +
  geom_point() +
  geom_line(aes(group = participant)) +
  geom_smooth(method = "lm", aes(fill = as.factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Accuracy improvement (%)", y = "IMI - value", colour="Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))

fig_11_D <- ggplot(core_stats, aes(x = (motivation_accuracy - titration_accuracy)*100, y = IMI_e, colour=factor(age_group, levels=c(0,1), labels=c("young", "older")))) +
  geom_point() +
  geom_line(aes(group = participant)) +
  geom_smooth(method = "lm", aes(fill = as.factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Accuracy improvement (%)", y = "IMI - effort", colour="Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))

p <- ggarrange(fig_11_A, fig_11_B, fig_11_C, fig_11_D, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"),common.legend = TRUE, legend = "bottom")
p
##ggsave(p, filename = 'plots/fig11.png', dpi = 300, type = 'cairo', width = 12, height = 12, bg = "white", units = 'in')

#ggsave(fig_11_A, filename = 'plots/fig12.png', dpi = 300, type = 'cairo', width = 6, height = 6, bg = "white", units = 'in')


# Appendix 2 trait measure links----
model<- lm((motivation_accuracy - titration_accuracy) ~ IMI_i*age_group, core_stats)
summary(model)

model<- lm((motivation_accuracy - titration_accuracy) ~ IMI_f*age_group, core_stats)
summary(model)

model<- lm((motivation_accuracy - titration_accuracy) ~ IMI_e*age_group, core_stats)
summary(model)



furter_test <- core_stats %>%
  inner_join(psychophysics_comparison_overall, by=c("participant","age_group"))

model<- lm(mean_log_rt ~ age_group*IMI_i, furter_test)
summary(model)

model<- lm(mean_log_rt ~ age_group*IMI_e, furter_test)
summary(model)

model<- lm(mean_log_rt ~ age_group*IMI_f, furter_test)
summary(model)



model<- lm(mean_accuracy ~ age_group*IMI_i, furter_test)
summary(model)

model<- lm(mean_accuracy ~ age_group*IMI_e, furter_test)
summary(model)

model<- lm(mean_accuracy ~ age_group*IMI_f, furter_test)
summary(model)


model<- lm(PSE_increment ~ age_group*IMI_i, furter_test)
summary(model)

## effect size
r_sq_adj <- summary(model)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2

fig_8_A <- ggplot(furter_test, aes(x = PSE_increment*1000, y = IMI_i, colour=factor(age_group, levels=c(0,1), labels=c("young", "older"))), ) +
  geom_point() +
  geom_line(aes(group = participant)) +
  geom_smooth(method = "lm", aes(fill = factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titrated window length (ms)", y = "IMI - Interest", colour="Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))


model<- lm(PSE_increment ~ age_group*IMI_e, furter_test)
summary(model)

## effect size
r_sq_adj <- summary(model)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2

fig_8_B <- ggplot(furter_test, aes(x = PSE_increment*1000, y = IMI_e, colour=factor(age_group, levels=c(0,1), labels=c("young", "older")))) +
  geom_point() +
  geom_line(aes(group = participant)) +
  geom_smooth(method = "lm", aes(fill = factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titrated window length (ms)", y = "IMI - Effort", colour="Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))


model<- lm(PSE_increment ~ age_group*IMI_f, furter_test)
summary(model)

## effect size
r_sq_adj <- summary(model)$adj.r.squared
f_2 <- r_sq_adj/(1-r_sq_adj)
f_2

fig_8_C <- ggplot(furter_test, aes(x = PSE_increment*1000, y = IMI_f, colour=factor(age_group, levels=c(0,1), labels=c("young", "older")))) +
  geom_point() +
  geom_line(aes(group = participant)) +
  geom_smooth(method = "lm", aes(fill = factor(age_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titrated window length (ms)", y = "IMI - Value", colour="Age group") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))

p7 <- ggarrange(fig_8_A, fig_8_C, fig_8_B, ncol = 3, nrow = 1, labels = c("A", "B", "C"),common.legend = TRUE, legend = "bottom")
#ggsave(p, filename = 'plots/fig8.png', dpi = 300, type = 'cairo', width = 18, height = 6, bg = "white", units = 'in')

p7
