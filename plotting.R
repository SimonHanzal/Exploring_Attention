# Plotting----

## Custom geom----


GeomSplitViolin <- ggplot2::ggproto(
  "GeomSplitViolin",
  ggplot2::GeomViolin,
  draw_group = function(self,
                        data,
                        ...,
                        # add the nudge here
                        nudge = 0,
                        draw_quantiles = NULL) {
    data <- transform(data,
                      xminv = x - violinwidth * (x - xmin),
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1, "group"]
    newdata <- plyr::arrange(transform(data,
                                       x = if (grp %% 2 == 1) xminv else xmaxv),
                             if (grp %% 2 == 1) y else -y)
    newdata <- rbind(newdata[1, ],
                     newdata,
                     newdata[nrow(newdata), ],
                     newdata[1, ])
    newdata[c(1, nrow(newdata)-1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
    
    # now nudge them apart
    newdata$x <- ifelse(newdata$group %% 2 == 1,
                        newdata$x - nudge,
                        newdata$x + nudge)
    
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      
      quantiles <- ggplot2:::create_quantile_segment_frame(data,
                                                           draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)),
                         setdiff(names(data), c("x", "y")),
                         drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- ggplot2::GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin",
                       grid::grobTree(ggplot2::GeomPolygon$draw_panel(newdata, ...),
                                      quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin",
                       ggplot2::GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function(mapping = NULL,
                              data = NULL,
                              stat = "ydensity",
                              position = "identity",
                              # nudge param here
                              nudge = 0,
                              ...,
                              draw_quantiles = NULL,
                              trim = TRUE,
                              scale = "area",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  
  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = stat,
                 geom = GeomSplitViolin,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(trim = trim,
                               scale = scale,
                               # don't forget the nudge
                               nudge = nudge,
                               draw_quantiles = draw_quantiles,
                               na.rm = na.rm,
                               ...))
}


## Fig1----

fig_N3A <- ggplot(psychophysics_median, aes(x = StimulusList, y = mean_accuracy*100, colour = as.factor(split_group))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participant), alpha = 0.5) +
  geom_smooth(method = "lm", aes(fill = as.factor(split_group))) +
  theme_minimal() +
  labs(x = "Titration block", y = "Overall accuracy (%)", color = "Performance group") +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  ylim(80,100)

fig_N3B <- ggplot(psychophysics_median, aes(x = StimulusList, y = mean_nogo*100, colour = as.factor(split_group))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participant), alpha = 0.5) +
  geom_smooth(method = "lm", aes(fill = as.factor(split_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titration block", y = "Nogo Accuracy (%)", color = "Baseline performance") +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  ylim(30,100)

fig_N3C <- ggplot(psychophysics_median, aes(x = StimulusList, y = mean_rt*1000, colour = as.factor(split_group))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participant), alpha = 0.5) +
  geom_smooth(method = "lm", aes(fill = as.factor(split_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titration block", y = "Mean reaction time (ms)", color = "Baseline performance") +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))

fig_N3D <- ggplot(psychophysics_median, aes(x = StimulusList, y = mean_increment*1000, colour = as.factor(split_group))) +
  geom_point(alpha = 0.5) +
  geom_line(aes(group = participant), alpha = 0.5) +
  geom_smooth(method = "lm", aes(fill = as.factor(split_group))) +
  scale_color_viridis_d(option="E") +
  theme_minimal() +
  labs(x = "Titration block", y = "Response window length (ms)", color = "Baseline performance") +
  scale_colour_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))



p1 <- ggarrange(fig_N3A, fig_N3B, fig_N3C, fig_N3D, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"),common.legend = TRUE, legend = "bottom"
)

p1

## Fig2----

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(baseline_nogo), se = std(baseline_nogo))


fig_2_A <- ggplot(plot_means, aes(x = factor(age_group, levels = c(0,1), labels= c("young", "older")),
                                  y = baseline_nogo*100, fill = factor(age_group, levels = c(0,1), labels=c("young", "older")), colour = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  #  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.2, height = 0) +
  # geom_boxplot(alpha = 0.6) +
  labs(x = "Age groups", y = "Baseline Nogo Accuracy (%)", colour = "Age groups") +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  guides(fill="none") +
  geom_point(aes(y = mean*100), size  = 3) +
  geom_errorbar(aes(ymin  = (mean - se)*100,
                    ymax  = (mean + se)*100),
                width = 0.3,
                size  = 1.2) +
  ylim(0,105) +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  geom_signif(comparisons = list(c("young", "older")), 
              map_signif_level=TRUE, textsize = 4, color = "black")

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(baseline_plot_rt), se = std(baseline_plot_rt))

fig_2_B <- ggplot(plot_means, aes(x = factor(age_group, levels = c(0,1), labels= c("young", "older")),
                                  y = baseline_plot_rt*1000, fill = factor(age_group, levels = c(0,1), labels=c("young", "older")), colour = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  #  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.2, height = 0) +
  # geom_boxplot(alpha = 0.6) +
  labs(x = "Age groups", y = "Baseline RT (ms)", colour = "Age groups") +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  guides(fill="none") +
  geom_point(aes(y = mean*1000), size  = 3) +
  geom_errorbar(aes(ymin  = (mean - se)*1000,
                    ymax  = (mean + se)*1000),
                width = 0.3,
                size  = 1.2) +
  ylim(400,850) +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  geom_signif(comparisons = list(c("young", "older")), 
              map_signif_level=TRUE, test.args=list(alternative = "less"), textsize = 4, color = "black")

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(PSE_increment), se = std(PSE_increment))

fig_2_E <- ggplot(plot_means, aes(x = factor(age_group, levels = c(0,1), labels= c("young", "older")),
                                  y = PSE_increment*1000, fill = factor(age_group, levels = c(0,1), labels=c("young", "older")), colour = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  #  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.2, height = 0) +
  # geom_boxplot(alpha = 0.6) +
  labs(x = "Age groups", y = "Titrated Window Length (ms)", colour = "Age groups") +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  guides(fill="none") +
  geom_point(aes(y = mean*1000), size  = 3) +
  geom_errorbar(aes(ymin  = (mean - se)*1000,
                    ymax  = (mean + se)*1000),
                width = 0.3,
                size  = 1.2) +
  ylim(300,1850) +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  geom_signif(comparisons = list(c("young", "older")), 
              map_signif_level=TRUE, textsize = 4, color = "black")


p2 <- ggarrange(fig_2_A, fig_2_B, fig_2_E, ncol = 3, nrow = 1, labels = c("A", "B", "C"),common.legend = TRUE, legend = "bottom")

p2



## Fig3----

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(IMI_all), se = std(IMI_all))

fig_5_0 <- ggplot(plot_means, aes(x = factor(age_group, levels = c(0,1), labels= c("young", "older")),
                                  y = IMI_all, fill = factor(age_group, levels = c(0,1), labels=c("young", "older")), colour = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  #  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.2, height = 0) +
  # geom_boxplot(alpha = 0.6) +
  labs(x = "Age groups", y = "IMI - Baseline Motivation", colour = "Age groups") +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  guides(fill="none") +
  geom_point(aes(y = mean), size  = 3) +
  geom_errorbar(aes(ymin  = mean - se,
                    ymax  = mean + se),
                width = 0.3,
                size  = 1.2) +
  ylim(20,135) +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  geom_signif(comparisons = list(c("young", "older")), 
              map_signif_level=TRUE, test="t.test", test.args=list(alternative = "less", var.equal = FALSE, paired=FALSE), textsize = 4, color = "black")

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(IMI_i), se = std(IMI_i))

fig_5_A <- ggplot(plot_means, aes(x = factor(age_group, levels = c(0,1), labels= c("young", "older")),
                                  y = IMI_i, fill = factor(age_group, levels = c(0,1), labels=c("young", "older")), colour = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  #  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.2, height = 0) +
  # geom_boxplot(alpha = 0.6) +
  labs(x = "Age groups", y = "IMI - Interest", colour = "Age groups") +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  guides(fill="none") +
  geom_point(aes(y = mean), size  = 3) +
  geom_errorbar(aes(ymin  = mean - se,
                    ymax  = mean + se),
                width = 0.3,
                size  = 1.2) +
  ylim(0,50) +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  geom_signif(comparisons = list(c("young", "older")), 
              map_signif_level=TRUE, textsize = 4, color = "black")

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(IMI_f), se = std(IMI_f))

fig_5_B <- ggplot(plot_means, aes(x = factor(age_group, levels = c(0,1), labels= c("young", "older")),
                                  y = IMI_f, fill = factor(age_group, levels = c(0,1), labels=c("young", "older")), colour = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  #  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.2, height = 0) +
  # geom_boxplot(alpha = 0.6) +
  labs(x = "Age groups", y = "IMI - Value", colour = "Age groups") +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  guides(fill="none") +
  geom_point(aes(y = mean), size  = 3) +
  geom_errorbar(aes(ymin  = mean - se,
                    ymax  = mean + se),
                width = 0.3,
                size  = 1.2) +
  ylim(0,50) +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  geom_signif(comparisons = list(c("young", "older")), 
              map_signif_level=TRUE, textsize = 4, color = "black")

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(IMI_e), se = std(IMI_e))

fig_5_C <- ggplot(plot_means, aes(x = factor(age_group, levels = c(0,1), labels= c("young", "older")),
                                  y = IMI_e, fill = factor(age_group, levels = c(0,1), labels=c("young", "older")), colour = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  #  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.2, height = 0) +
  # geom_boxplot(alpha = 0.6) +
  labs(x = "Age groups", y = "IMI - Effort", colour = "Age groups") +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  guides(fill="none") +
  geom_point(aes(y = mean), size  = 3) +
  geom_errorbar(aes(ymin  = mean - se,
                    ymax  = mean + se),
                width = 0.3,
                size  = 1.2) +
  ylim(0,45) +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  geom_signif(comparisons = list(c("young", "older")), 
              map_signif_level=TRUE, textsize = 4, color = "black")

plot_means <- core_stats %>%
  group_by(age_group) %>%
  mutate(mean = mean(vas_m), se = std(vas_m))

fig_5_D <- ggplot(plot_means, aes(x = factor(age_group, levels = c(0,1), labels= c("young", "older")),
                                  y = vas_m, fill = factor(age_group, levels = c(0,1), labels=c("young", "older")), colour = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  #  geom_violin(alpha = 0.4) +
  geom_jitter(width = 0.2, height = 0) +
  # geom_boxplot(alpha = 0.6) +
  labs(x = "Age groups", y = "VAS Monetary motivation", colour = "Age groups") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  guides(fill="none") +
  geom_point(aes(y = mean), size  = 3) +
  geom_errorbar(aes(ymin  = mean - se,
                    ymax  = mean + se),
                width = 0.3,
                size  = 1.2) +
  ylim(0,105) +
  theme(text=element_text(size=16), axis.text=element_text(size=14)) +
  guides(colour = "none") +
  geom_signif(comparisons = list(c("young", "older")), 
              map_signif_level=TRUE, textsize = 4, color = "black")

p3 <- ggarrange(fig_5_A, fig_5_B, fig_5_C, fig_5_D, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"))

p3


## Fig4----
plot_means <- anova_stats %>%
  group_by(age_group, block) %>%
  mutate(mean = mean(all_accuracy), se = std(all_accuracy))

fig_4_A <- ggplot(plot_means, aes(x = as.factor(block), y = as.numeric(all_accuracy)*100, fill = factor(age_group, levels = c(0,1), labels= c("young", "older")))) +
  geom_split_violin(alpha = 0.55, adjust  = 0.8, colour="white") +
  geom_line(aes(colour = as.factor(age_group), group = as.character(participant)), alpha = 0.6) +
  #  geom_boxplot(alpha = 0.4, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0, aes(colour=age_group)) +
  labs(x = "Block", y = "Overall Accuracy (%)", fill = "Age group") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels=c("titration_accuracy" = "End of Titration", "motivation_accuracy" = "Motivation Block"), limits=rev) +
  geom_point(aes(y = mean*100, colour=age_group), size  = 3, position = position_dodge(0.5), width = 0.6) +
  geom_errorbar(aes(ymin  = (mean - se)*100,
                    ymax  = (mean + se)*100,
                    colour=age_group),
                width = 0.3,
                size  = 1.2,
                position = position_dodge(0.5)) +
  guides(colour = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))


plot_means <- anova_nogo_stats %>%
  group_by(age_group, block) %>%
  mutate(mean = mean(all_nogo), se = std(all_nogo))

fig_4_B <- ggplot(plot_means, aes(x = as.factor(block), y = as.numeric(all_nogo)*100, fill = factor(age_group, levels = c(0,1), labels= c("young", "older")))) +
  geom_split_violin(alpha = 0.55, adjust  = 0.8, colour="white") +
  geom_line(aes(colour = as.factor(age_group), group = as.character(participant)), alpha = 0.6) +
  #  geom_boxplot(alpha = 0.4, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0, aes(colour=age_group)) +
  labs(x = "Block", y = "Nogo Accuracy (%)", fill = "Age group") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels=c("titration_nogo_accuracy" = "End of Titration", "motivation_nogo_accuracy" = "Motivation Block"), limits=rev) +
  geom_point(aes(y = mean*100, colour=age_group), size  = 3, position = position_dodge(0.5), width = 0.6) +
  geom_errorbar(aes(ymin  = (mean - se)*100,
                    ymax  = (mean + se)*100,
                    colour=age_group),
                width = 0.3,
                size  = 1.2,
                position = position_dodge(0.5)) +
  guides(colour = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))


# This needs to be added to again show natural RTs
plot_rt_stats <- core_stats %>%
  select(participant, age_group, motivation_plot_rt, titration_plot_rt) %>%
  pivot_longer(cols=motivation_plot_rt:titration_plot_rt, names_to = "block", values_to="all_rt") %>%
  mutate(age_group = as.factor(age_group),
         block = as.factor(block),
         participant = as.factor(participant))

plot_means <- plot_rt_stats %>%
  group_by(age_group, block) %>%
  mutate(mean = mean(all_rt), se = std(all_rt))

fig_4_C <- ggplot(plot_means, aes(x = as.factor(block), y = as.numeric(all_rt)*1000, fill = factor(age_group, levels = c(0,1), labels= c("young", "older")))) +
  geom_split_violin(alpha = 0.55, adjust  = 0.8, colour="white") +
  geom_line(aes(colour = as.factor(age_group), group = as.character(participant)), alpha = 0.6) +
  #  geom_boxplot(alpha = 0.4, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0, aes(colour=age_group)) +
  labs(x = "Block", y = "RT (ms)", fill = "Age group") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1", guide="none") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels=c("titration_plot_rt" = "End of Titration", "motivation_plot_rt" = "Motivation Block"), limits=rev) +
  geom_point(aes(y = mean*1000, colour=age_group), size  = 3, position = position_dodge(0.5), width = 0.6) +
  geom_errorbar(aes(ymin  = (mean - se)*1000,
                    ymax  = (mean + se)*1000,
                    colour=age_group),
                width = 0.3,
                size  = 1.2,
                position = position_dodge(0.5)) +
  guides(colour = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))


plot_means <- dprimes %>%
  group_by(age_group, StimulusList) %>%
  mutate(mean = mean(dprime), se = std(dprime))

fig_SD <- ggplot(plot_means, aes(x = as.factor(StimulusList), y = as.numeric(dprime), fill = factor(age_group, level=c("young", "older"), labels = c("young", "older")), colour = factor(age_group, level=c("young", "older"), labels = c("young", "older")))) +
  #  geom_boxplot(alpha = 0.4, varwidth = TRUE, width = 0.45, outlier.shape = NA, coef = 0, aes(colour=age_group)) +
  labs(x = "Block", y = "d prime", fill = "Age group") +
  theme_minimal() +
  scale_colour_brewer(palette = "Set1", guide="none", labels=c("Young","Older")) +
  scale_fill_brewer(palette = "Set1", labels=c("Young","Older")) +
  scale_x_discrete(labels=c("titrated" = "End of Titration", "motivated" = "Motivation Block"), limits=rev) +
  # scale_x_discrete(levels = c("Titrated","Motivated"), labels=c("Titrated","Motivated")) +
  geom_split_violin(alpha = 0.55, adjust  = 0.8, colour="white") +
  geom_line(aes(colour = factor(age_group, level=c("young", "older"), labels = c("young", "older")), group = as.character(participant)), alpha = 0.6) +
  geom_point(aes(y = mean), size  = 3, position = position_dodge(0.5), width = 0.6) +
  geom_errorbar(aes(ymin  = (mean - se),
                    ymax  = (mean + se),
  ),
  width = 0.3,
  size  = 1.2,
  position = position_dodge(0.5)) +
  guides(colour = "none") +
  theme(text=element_text(size=16), axis.text=element_text(size=14))


p4 <- ggarrange(fig_4_A, fig_4_B, fig_SD, fig_4_C, ncol = 2, nrow = 2, labels = c("A", "B", "C", "D"),common.legend = TRUE, legend = "bottom")

p4


## Fig5----

fig_6 <- ggplot(core_stats, aes(x = IMI_all, y = mfi_all, color = factor(age_group, levels = c(0,1), labels=c("young", "older")))) +
  geom_point()+
  geom_smooth(method = "lm", aes(fill = factor(age_group))) +
  labs(x = "IMI - Total ", y = "MFI - Total", color = "Age") +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  theme_minimal() +
  theme(text=element_text(size=16), axis.text=element_text(size=14))

p5 <- fig_6


