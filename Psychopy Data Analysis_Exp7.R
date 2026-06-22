install.packages("Rmisc")
library(Rmisc) 
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("rstatix")
library(rstatix)
library(dplyr)

SONAsubID <- c(
  401:410,412,413,427:434,436:441,443,445:449,451:458,460:467,469:471,473:478,
  480:489,491:494,496:499,500,701:704,706:723,725:727,729:731,733:741,743:747,
  749:766,768:771,774:782,784:791,1001:1004,1006:1044,1046:1068,1070:1083)
allSubID <- SONAsubID

ratesDat <- data.frame(matrix(NA, length(allSubID) * 8, 6))
colnames(ratesDat) <- c('Subject', 'nam_unn', 'RS_RT_N', 'N', 'nRespRight', 'PropRight')

dprimeTable <- data.frame(matrix(NA, length(allSubID) * 4, 7))
colnames(dprimeTable) <- c('Subject', 'nam_unn', 'RS_RT_N', 'Hits', 'FAs', 'dprime', 'criterion')

subCt <- 0

library(dplyr)

for (s in 1:length(allSubID)) {
  fName <- paste('C:/Users/jgove/OneDrive/Desktop/Nameable and Unnameable Objects/data shaping/Formatted data/participantAp', allSubID[s], '.csv', sep = "")
  dat <- read.table(fName, sep = ',', header = TRUE)
  
  subCt <- subCt + 1
  
  dat$correct <- as.factor(dat$key_resp_9.corr)
  dat$nam_unn <- as.factor(dat$nam_unn)
  dat$RS_RT_N <- as.factor(dat$rs_rt_n)
  dat$old <- as.factor(dat$intact)
  
  condCt <- 0
  RS_RT_N_lvls <- levels(dat$RS_RT_N)
  old_lvls <- levels(dat$old)
  nam_unn_levels <- levels(dat$nam_unn)
  keys_levels <- levels(dat$key_resp_9.keys)
  
  for (oj in 1:length(nam_unn_levels)) {
    for (i in 1:length(RS_RT_N_lvls)) {
      for (k in 1:length(old_lvls)){
        condCt <- condCt + 1
        
        ratesDat$Subject[subCt * 8 + condCt - 8] <- allSubID[s]
        ratesDat$nam_unn[subCt * 8 + condCt - 8] <- nam_unn_levels[oj]
        ratesDat$RS_RT_N[subCt * 8 + condCt - 8] <- RS_RT_N_lvls[i]
        ratesDat$Old[subCt * 8 + condCt - 8] <- old_lvls[k]
        
        filt_dat <- filter(filter(dat, nam_unn == nam_unn_levels[oj]), RS_RT_N == RS_RT_N_lvls[i], old == old_lvls[k])
        
        ratesDat$N[subCt * 8 + condCt - 8] <- nrow(filt_dat)
        ratesDat$nRespRight[subCt * 8 + condCt - 8] <- sum(filt_dat$key_resp_9.keys == "right")
        
        ratesDat$PropRight[subCt * 8 + condCt - 8] <- (ratesDat$nRespRight[subCt * 8 + condCt - 8]+.5) / (ratesDat$N[subCt * 8 + condCt - 8]+1)
        
        #if(old_lvls[k] == "right"){  # Corrected the condition here
        #dprimeTable$Subject[(subCt - 1) * 6 + oj] <- allSubID[s]  # Corrected the indexing here
        #dprimeTable$nam_unn[(subCt - 1) * 6 + oj] <- nam_unn_levels[oj]  # Corrected the indexing here
        #dprimeTable$RS_RT_N[(subCt - 1) * 6 + oj] <- RS_RT_N_lvls[i]  # Corrected the indexing here
      }
    }
  }
}

library(tidyr)
df <- ratesDat %>% pivot_wider(id_cols = c("Subject", "nam_unn", "RS_RT_N"), names_from = "Old", values_from = "PropRight")

dprimeTable$Subject <- df$Subject
dprimeTable$nam_unn <- df$nam_unn
dprimeTable$RS_RT_N <- df$RS_RT_N
dprimeTable$Hits <- df$right
dprimeTable$FAs <- df$left
dprimeTable$dprime <- qnorm(df$right) - qnorm(df$left)
dprimeTable$criterion <- (df$right + df$left)*-.5


dprimeTable$nam_unn <- as.factor(dprimeTable$nam_unn)


SE_dprime <- summarySE(dprimeTable, measurevar = "dprime", groupvars = c("nam_unn", "RS_RT_N"))
SE_HITS <- summarySE(dprimeTable, measurevar = "Hits", groupvars = c("nam_unn", "RS_RT_N"))
SE_FA <- summarySE(dprimeTable, measurevar = "FAs", groupvars = c("nam_unn", "RS_RT_N"))

print(ratesDat)

print(paste("subCt:", subCt, "condCt:", condCt, "index:", subCt * 8 + condCt - 8))


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  # black, gold, light blue, green, yellow, blue, dark orange, pink

library(ggplot2)  # Make sure to load ggplot2 package before running this part

#SCATTER PLOTS
# Calculate SEM using summarySE
data_summary3 <- summarySE(dprimeTable, measurevar = "dprime", groupvars = c("nam_unn", "RS_RT_N"))

# Create a jittered Group variable for consistent positioning with increased spacing
set.seed(123)  # For reproducibility
spacing_factor <- 0.6  # Increased spacing factor to create more separation
dprimeTable <- dprimeTable %>%
  mutate(nam_unn_jit = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor + jitter(rep(0, n()), amount = 0.1))

# Create the scatter plot with individual points, means, and error bars
dodge <- position_dodge(width = 0.75)

plotALL3 <- ggplot(dprimeTable, aes(x = nam_unn_jit, y = dprime, color = RS_RT_N)) +
  geom_jitter(size = 3, alpha = 0.7) +
  stat_summary(aes(x = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, y = dprime, color = RS_RT_N), 
               fun = mean, geom = "point", shape = 18, size = 4, position = dodge, color = "black") +
  geom_errorbar(data = data_summary3, aes(x = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, ymin = dprime - se, ymax = dprime + se, color = RS_RT_N),
                width = 0.2, position = dodge, color = "black") +
  scale_x_continuous(breaks = c(2, 4), labels = c("Meaningful", "Abstract")) +  # Adjusted breaks to match new spacing
  scale_color_manual(name = NULL, values = c(cbbPalette[2], cbbPalette[8]), labels = c("Restudy", "Retrieval")) +
  xlab("Stimulus Type") +
  ylab("Visual Recall (d')") +
  ggtitle("Memory Performance Exp. 7") +
  theme(plot.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        legend.spacing.y = unit(1.5, "cm"),
        legend.key.height = unit(1.5, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) +
  coord_cartesian(xlim = c(0.75, 5), ylim = c(-1, 3.7))  # Adjusted xlim to accommodate increased spacing

# Overlay the line plot on the scatter plot
final_plotALL3 <- plotALL3 +
  geom_line(data = dprimeTable, aes(x = nam_unn_jit, y = dprime, group = interaction(Subject, nam_unn)), 
            alpha = 0.5, color = "grey")

print(final_plotALL3)


library(dplyr)
library(rstatix)

anova_dprime_dat <- dprimeTable %>%
  dplyr::mutate(
    Subject = factor(Subject),
    
    StimType = factor(
      nam_unn,
      levels = c("nam", "unn"),
      labels = c("Meaningful", "Abstract")
    ),
    
    StudyCond = factor(
      RS_RT_N,
      levels = c("ApRS", "ApRT"),
      labels = c("Restudy", "Retrieval")
    )
  ) %>%
  dplyr::select(Subject, StimType, StudyCond, dprime)

anova_dprime_res <- rstatix::anova_test(
  data = anova_dprime_dat,
  dv = dprime,
  wid = Subject,
  within = c(StimType, StudyCond)
)

anova_dprime_tab <- rstatix::get_anova_table(anova_dprime_res)

print(anova_dprime_tab, width = Inf)

desc_dprime <- anova_dprime_dat %>%
  dplyr::group_by(StimType, StudyCond) %>%
  dplyr::summarise(
    N  = dplyr::n(),
    M  = mean(dprime, na.rm = TRUE),
    SD = sd(dprime, na.rm = TRUE),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

print(desc_dprime)

##Hits and FAs

anova_hits_dat <- dprimeTable %>%
  dplyr::mutate(
    Subject = factor(Subject),
    
    StimType = factor(
      nam_unn,
      levels = c("nam", "unn"),
      labels = c("Meaningful", "Abstract")
    ),
    
    StudyCond = factor(
      RS_RT_N,
      levels = c("ApRS", "ApRT"),
      labels = c("Restudy", "Retrieval")
    )
  ) %>%
  dplyr::select(Subject, StimType, StudyCond, Hits)

anova_hits_res <- rstatix::anova_test(
  data = anova_hits_dat,
  dv = Hits,
  wid = Subject,
  within = c(StimType, StudyCond)
)

anova_hits_tab <- rstatix::get_anova_table(anova_hits_res)

print(anova_hits_tab, width = Inf)


anova_fa_dat <- dprimeTable %>%
  dplyr::mutate(
    Subject = factor(Subject),
    
    StimType = factor(
      nam_unn,
      levels = c("nam", "unn"),
      labels = c("Meaningful", "Abstract")
    ),
    
    StudyCond = factor(
      RS_RT_N,
      levels = c("ApRS", "ApRT"),
      labels = c("Restudy", "Retrieval")
    )
  ) %>%
  dplyr::select(Subject, StimType, StudyCond, FAs)

anova_fa_res <- rstatix::anova_test(
  data = anova_fa_dat,
  dv = FAs,
  wid = Subject,
  within = c(StimType, StudyCond)
)

anova_fa_tab <- rstatix::get_anova_table(anova_fa_res)

print(anova_fa_tab, width = Inf)


# ============================
# HITS PLOT — Exp. 7
# ============================

data_summary_hits <- summarySE(
  dprimeTable,
  measurevar = "Hits",
  groupvars = c("nam_unn", "RS_RT_N")
)

plot_hits <- ggplot(dprimeTable, aes(x = nam_unn_jit, y = Hits, color = RS_RT_N)) +
  geom_jitter(size = 3, alpha = 0.7) +
  
  stat_summary(
    aes(
      x = as.numeric(nam_unn) * 2 +
        (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor,
      y = Hits,
      color = RS_RT_N
    ),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    position = dodge,
    color = "black"
  ) +
  
  geom_errorbar(
    data = data_summary_hits,
    aes(
      x = as.numeric(nam_unn) * 2 +
        (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor,
      ymin = Hits - se,
      ymax = Hits + se,
      color = RS_RT_N
    ),
    width = 0.2,
    position = dodge,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(2, 4),
    labels = c("Meaningful", "Abstract")
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c(cbbPalette[2], cbbPalette[8]),
    labels = c("Restudy", "Retrieval")
  ) +
  
  xlab("Stimulus Type") +
  ylab("Hits") +
  ggtitle("Hits Exp. 7") +
  
  theme(
    plot.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    
    legend.text = element_text(size = 30),
    legend.title = element_blank(),
    legend.position = "right",
    legend.spacing.y = unit(1.0, "cm"),
    legend.key.height = unit(1.4, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
  ) +
  
  coord_cartesian(xlim = c(0.75, 5), ylim = c(0, 1))

final_plot_hits <- plot_hits +
  geom_line(
    data = dprimeTable,
    aes(
      x = nam_unn_jit,
      y = Hits,
      group = interaction(Subject, nam_unn)
    ),
    alpha = 0.5,
    color = "grey"
  )

print(final_plot_hits)


# ============================
# FALSE ALARM PLOT — Exp. 7
# ============================

data_summary_fa <- summarySE(
  dprimeTable,
  measurevar = "FAs",
  groupvars = c("nam_unn", "RS_RT_N")
)

plot_fa <- ggplot(dprimeTable, aes(x = nam_unn_jit, y = FAs, color = RS_RT_N)) +
  geom_jitter(size = 3, alpha = 0.7) +
  
  stat_summary(
    aes(
      x = as.numeric(nam_unn) * 2 +
        (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor,
      y = FAs,
      color = RS_RT_N
    ),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4,
    position = dodge,
    color = "black"
  ) +
  
  geom_errorbar(
    data = data_summary_fa,
    aes(
      x = as.numeric(nam_unn) * 2 +
        (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor,
      ymin = FAs - se,
      ymax = FAs + se,
      color = RS_RT_N
    ),
    width = 0.2,
    position = dodge,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(2, 4),
    labels = c("Meaningful", "Abstract")
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c(cbbPalette[2], cbbPalette[8]),
    labels = c("Restudy", "Retrieval")
  ) +
  
  xlab("Stimulus Type") +
  ylab("False Alarms") +
  ggtitle("False Alarms Exp. 7") +
  
  theme(
    plot.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    
    legend.text = element_text(size = 30),
    legend.title = element_blank(),
    legend.position = "right",
    legend.spacing.y = unit(1.0, "cm"),
    legend.key.height = unit(1.4, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
  ) +
  
  coord_cartesian(xlim = c(0.75, 5), ylim = c(0, 1))

final_plot_fa <- plot_fa +
  geom_line(
    data = dprimeTable,
    aes(
      x = nam_unn_jit,
      y = FAs,
      group = interaction(Subject, nam_unn)
    ),
    alpha = 0.5,
    color = "grey"
  )

print(final_plot_fa)



##ANOVA
dprimeTable$Subject <- factor(dprimeTable$Subject)
dprimeTable$nam_unn <- factor(dprimeTable$nam_unn, levels = c("nam", "unn"))
dprimeTable$RS_RT_N <- factor(dprimeTable$RS_RT_N, levels = c("ApRS", "ApRT"))

Recall.anova2 <- aov(dprime ~ Subject + nam_unn * RS_RT_N, data = dprimeTable)
summary(Recall.anova2)


## ============================
## RESULTS STATS FOR WRITEUP
## ============================ DISREGARD

library(dplyr)

## Relabel factors for readable output
dprimeTable <- dprimeTable %>%
  mutate(
    Subject = factor(Subject),
    StimulusType = factor(nam_unn,
                          levels = c("nam", "unn"),
                          labels = c("Meaningful", "Abstract")),
    StudyCondition = factor(RS_RT_N,
                            levels = c("ApRS", "ApRT"),
                            labels = c("Restudy", "Retrieval Practice"))
  )

## ----------------------------
## Descriptive statistics
## ----------------------------

desc_dprime <- dprimeTable %>%
  dplyr::group_by(StimulusType, StudyCondition) %>%
  dplyr::summarise(
    N  = dplyr::n(),
    M  = mean(dprime, na.rm = TRUE),
    SD = sd(dprime, na.rm = TRUE),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

print(desc_dprime)

## Collapsed across stimulus type
desc_dprime_collapsed <- dprimeTable %>%
  dplyr::group_by(StudyCondition) %>%
  dplyr::summarise(
    N  = dplyr::n(),
    M  = mean(dprime, na.rm = TRUE),
    SD = sd(dprime, na.rm = TRUE),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

print(desc_dprime_collapsed)
print(desc_dprime_collapsed)

## ----------------------------
## Proper repeated-measures ANOVA
## ----------------------------
anova_dprime_dat %>%
  dplyr::count(Subject, StimType, StudyCond) %>%
  dplyr::filter(n > 1)
dprimeTable %>%
  dplyr::count(Subject, nam_unn, RS_RT_N) %>%
  dplyr::filter(n > 1)
head(
  dprimeTable[dprimeTable$Subject == 401, ],
  20
)

anova_dprime_dat2 <- anova_dprime_dat %>%
  dplyr::distinct(
    Subject,
    StimType,
    StudyCond,
    dprime,
    .keep_all = TRUE
  )

anova_dprime_dat2 %>%
  dplyr::count(Subject, StimType, StudyCond) %>%
  dplyr::filter(n > 1)

anova_dprime_res <- rstatix::anova_test(
  data = anova_dprime_dat2,
  dv = dprime,
  wid = Subject,
  within = c(StimType, StudyCond)
)

anova_dprime_tab <- rstatix::get_anova_table(anova_dprime_res)

print(anova_dprime_tab, width = Inf)

desc_dprime <- anova_dprime_dat2 %>%
  dplyr::group_by(StimType, StudyCond) %>%
  dplyr::summarise(
    N  = dplyr::n(),
    M  = mean(dprime, na.rm = TRUE),
    SD = sd(dprime, na.rm = TRUE),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

print(desc_dprime)

t_by_stim <- anova_dprime_dat2 %>%
  tidyr::pivot_wider(
    names_from = StudyCond,
    values_from = dprime
  ) %>%
  dplyr::group_by(StimType) %>%
  dplyr::summarise(
    t = unname(t.test(Retrieval, Restudy, paired = TRUE)$statistic),
    df = unname(t.test(Retrieval, Restudy, paired = TRUE)$parameter),
    p = t.test(Retrieval, Restudy, paired = TRUE)$p.value,
    M_Restudy = mean(Restudy, na.rm = TRUE),
    SD_Restudy = sd(Restudy, na.rm = TRUE),
    M_Retrieval = mean(Retrieval, na.rm = TRUE),
    SD_Retrieval = sd(Retrieval, na.rm = TRUE),
    .groups = "drop"
  )

print(t_by_stim)











library(dplyr)
library(rstatix)

anova_dprime_dat <- dprimeTable %>%
  dplyr::mutate(
    Subject = factor(Subject),
    
    nam_unn = dplyr::recode(as.character(nam_unn),
                            "nam" = "Meaningful",
                            "unn" = "Abstract",
                            .default = as.character(nam_unn)),
    
    RS_RT_N = dplyr::recode(as.character(RS_RT_N),
                            "ApRS" = "Restudy",
                            "ApRT" = "Retrieval",
                            "RS"   = "Restudy",
                            "RT"   = "Retrieval",
                            .default = as.character(RS_RT_N)),
    
    StimType  = factor(nam_unn, levels = c("Meaningful", "Abstract")),
    StudyCond = factor(RS_RT_N, levels = c("Restudy", "Retrieval"))
  ) %>%
  dplyr::select(Subject, StimType, StudyCond, dprime) %>%
  dplyr::filter(is.finite(dprime), !is.na(StimType), !is.na(StudyCond))

anova_dprime_res <- rstatix::anova_test(
  data = anova_dprime_dat,
  dv = dprime,
  wid = Subject,
  within = c(StimType, StudyCond)
)

anova_dprime_res

anova_dprime_tab <- rstatix::get_anova_table(anova_dprime_res)

print(anova_dprime_tab, width = Inf)

## ----------------------------
## Paired t-tests:
## Retrieval Practice vs Restudy
## ----------------------------

## Overall paired comparison collapsed across stimulus type
dprime_collapsed <- dprimeTable %>%
  group_by(Subject, StudyCondition) %>%
  summarise(dprime = mean(dprime, na.rm = TRUE), .groups = "drop")

wide_collapsed <- dprime_collapsed %>%
  tidyr::pivot_wider(
    names_from = StudyCondition,
    values_from = dprime
  )

t_overall <- t.test(
  wide_collapsed$`Retrieval Practice`,
  wide_collapsed$Restudy,
  paired = TRUE
)

print(t_overall)

## By stimulus type
wide_by_stim <- dprimeTable %>%
  select(Subject, StimulusType, StudyCondition, dprime) %>%
  tidyr::pivot_wider(
    names_from = StudyCondition,
    values_from = dprime
  )

t_by_stim <- wide_by_stim %>%
  group_by(StimulusType) %>%
  summarise(
    t = t.test(`Retrieval Practice`, Restudy, paired = TRUE)$statistic,
    df = t.test(`Retrieval Practice`, Restudy, paired = TRUE)$parameter,
    p = t.test(`Retrieval Practice`, Restudy, paired = TRUE)$p.value,
    M_Restudy = mean(Restudy, na.rm = TRUE),
    SD_Restudy = sd(Restudy, na.rm = TRUE),
    M_Retrieval = mean(`Retrieval Practice`, na.rm = TRUE),
    SD_Retrieval = sd(`Retrieval Practice`, na.rm = TRUE),
    .groups = "drop"
  )

print(t_by_stim)

## ----------------------------
## Optional: export tables
## ----------------------------

write.csv(desc_dprime,
          "Exp7_dprime_descriptives_by_condition.csv",
          row.names = FALSE)

write.csv(desc_dprime_collapsed,
          "Exp7_dprime_descriptives_collapsed.csv",
          row.names = FALSE)

write.csv(t_by_stim,
          "Exp7_retrieval_vs_restudy_ttests_by_stimtype.csv",
          row.names = FALSE)
# =========================
# MISSING RESPONSE SUMMARY: WORDMIX1 APERTURE HALF
# =========================
library(dplyr)
library(ggplot2)

missing_summary <- data.frame(
  Subject = numeric(),
  n_missing = numeric(),
  total_trials = numeric(),
  prop_missing = numeric()
)

for (s in 1:length(allSubID)) {
  
  fName <- paste(
    'C:/Users/jgove/OneDrive/Desktop/Nameable and Unnameable Objects/data shaping/Formatted data/participantAp',
    allSubID[s],
    '.csv',
    sep = ""
  )
  
  dat <- read.csv(fName, stringsAsFactors = FALSE)
  
  # count only actual experimental trials
  trial_rows <- !is.na(dat$nam_unn) & !is.na(dat$rs_rt_n)
  
  total_trials <- sum(trial_rows)
  
  # missing responses = NA or "None" in key_resp_9.keys
  n_missing <- sum(
    trial_rows &
      (is.na(dat$key_resp_9.keys) | dat$key_resp_9.keys == "None")
  )
  
  prop_missing <- n_missing / total_trials
  
  missing_summary <- rbind(
    missing_summary,
    data.frame(
      Subject = allSubID[s],
      n_missing = n_missing,
      total_trials = total_trials,
      prop_missing = prop_missing
    )
  )
}

print(missing_summary)
summary(missing_summary$n_missing)

ggplot(missing_summary, aes(x = n_missing)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    color = "black",
    fill = "steelblue"
  ) +
  xlab("Number of Missing Responses") +
  ylab("Number of Participants") +
  ggtitle("wordMix1 Aperture Half Missing Responses by Participant") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18)
  )


##original plots
ggplot(data = SE_dprime,
       aes(x = nam_unn, y = dprime, fill = RS_RT_N)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymax = dprime + se, ymin = dprime - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("nam_unn", labels = c("Nameable", "Abstract")) +
  scale_fill_manual(name = "Condition", values = c(cbbPalette[2], cbbPalette[8]),
                    labels = c("Restudy", "Retrieval")) +
  ylab("Visual Recall (d')") +
  ggtitle("APERTURE Recall") +
  theme_bw() +
  theme(plot.title = element_text(size = 30)) +
  theme(axis.text = element_text(size = 30)) +
  theme(axis.title.x = element_text(size = 30)) +
  theme(axis.title.y = element_text(size = 30)) +
  theme(legend.text = element_text(size = 30)) +
  theme(legend.title = element_text(size = 30)) 


ggplot(data = SE_HITS,
       aes(x = nam_unn, y = Hits, fill = RS_RT_N)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = Hits + se, ymin = Hits - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Nameable", "Unnameable")) + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) + 
  ylab("HITS") +
  ggtitle("Aperture Paradigm HITS") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()

ggplot(data = SE_FA,
       aes(x = nam_unn, y = FAs, fill = RS_RT_N)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = FAs + se, ymin = FAs - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Nameable", "Unnameable")) + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) + 
  ylab("FAs") +
  ggtitle("Aperture Paradigm FAs") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()

ggplot(data = SE_FA,
       aes(x = nam_unn, y = FAs, fill = RS_RT_N)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = FAs + se, ymin = FAs - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Nameable", "Unnameable")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) + 
  ylab("FAs") +
  ggtitle("FAs") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()  
  
ggplot(data = SE_HITS_recall,
       aes(x = Group, y = Hits_recall, fill = Obj_Sce)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = Hits_recall + se, ymin = Hits_recall - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Young Adults", "Older Adults")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[8], cbbPalette[3]),
                    labels = c("Objects", "Scenes")) + 
  ylab("Recall HITS") +
  ggtitle("Recall HITS") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()

ggplot(data = SE_FA_recall,
       aes(x = Group, y = FAs_recall, fill = Obj_Sce)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = FAs_recall + se, ymin = FAs_recall - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Young Adults", "Older Adults")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[8], cbbPalette[3]),
                    labels = c("Objects", "Scenes")) + 
  ylab("Recall FAs") +
  ggtitle("Recall FAs") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()

ggplot(data = SE_Yules_Studied_Omit,
        aes(x = Group, y = Yules_Studied, fill = Obj_Sce)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = Yules_Studied + se, ymin = Yules_Studied - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Young Adults", "Older Adults")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[8], cbbPalette[3]),
                    labels = c("Objects", "Scenes")) + 
  ylab("Yule's Q Studied") +
  ggtitle("Yule's Q Studied") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()
    
ggplot(data = SE_Yules_Unstudied_Omit,
        aes(x = Group, y = Yules_Unstudied, fill = Obj_Sce)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = Yules_Unstudied + se, ymin = Yules_Unstudied - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Young Adults", "Older Adults")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[8], cbbPalette[3]),
                    labels = c("Objects", "Scenes")) + 
  ylab("Yule's Q Unstudied") +
  ggtitle("Yule's Q Unstudied") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()   

ggplot(data = SE_a_studied,
        aes(x = Group, y = a_studied, fill = Obj_Sce)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = a_studied + se, ymin = a_studied - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Young Adults", "Older Adults")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[8], cbbPalette[3]),
                      labels = c("Objects", "Scenes")) + 
  ylab("Prop Recall-Recog") +
  ggtitle("Prop Recall-Recog") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw() 

ggplot(data = SE_c_studied,
        aes(x = Group, y = c_studied, fill = Obj_Sce)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = c_studied + se, ymin = c_studied - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Group", labels = c("Young Adults", "Older Adults")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[8], cbbPalette[3]),
                    labels = c("Objects", "Scenes")) + 
  ylab("Prop Recall-Not-Recog") +
  ggtitle("Prop Recall-Not-Recog") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()  
    
write.csv(ratesDat, file=paste("C:/Users/jgove/OneDrive/Desktop/ratesDat_word.csv", sep=""),row.names = F)
write.csv(pivot_df, file=paste("C:/Users/jgove/OneDrive/Desktop/pivotdf.csv", sep=""),row.names = F)

write.csv(dprimeTable, file=paste("C:/Users/jgove/OneDrive/Desktop/dPrimeTable_WORDMIX_Aperture.csv", sep=""),row.names = F)
write.csv(ContingenciesTable, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/ContingenciesTable-9-23.csv", sep=""),row.names = F)

write.csv(SE_dprime_recog, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/SE_dprime_recog.csv", sep=""),row.names = F)
write.csv(SE_dprime_recall, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/SE_dprime_recall.csv", sep=""),row.names = F)
write.csv(SE_HITS_recog, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/SE_HITS_recog.csv", sep=""),row.names = F)
write.csv(SE_HITS_recall, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/SE_HITS_recall.csv", sep=""),row.names = F)
write.csv(SE_FA_recog, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/SE_FA_recog.csv", sep=""),row.names = F)
write.csv(SE_FA_recall, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/SE_FA_recall.csv", sep=""),row.names = F)
write.csv(SE_Yules_Studied_Omit, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/SE_Yules_Studied_Omit-6-9.csv", sep=""),row.names = F)
write.csv(SE_Yules_Unstudied_Omit, file=paste("C:/Users/jgove/OneDrive/Desktop/Object Recall/data-shaping/SE_Yules_Unstudied_Omit-6-9.csv", sep=""),row.names = F)



boxplot(dprimeTable$dprime_recog~interaction(dprimeTable$Group,dprimeTable$Obj_Sce), 
        main = "Recognition dPrime",
        xlab = "Obj_Sce by Group",
        ylab = "d'",
        names = c("YA Obj","OA Obj","YA Sce","OA Sce"))

boxplot(dprimeTable$dprime_recall~interaction(dprimeTable$Group,dprimeTable$Obj_Sce), 
        main = "Recall Visual Learning Score",
        xlab = "Obj_Sce by Group",
        ylab = "Visual Learning Score",
        names = c("YA Obj","OA Obj","YA Sce","OA Sce"))

boxplot(dprimeTable$Hits_recog~interaction(dprimeTable$Group,dprimeTable$Obj_Sce), 
        main = "Recognition HITS",
        xlab = "Obj_Sce by Group",
        ylab = "HITS",
        names = c("YA Obj","OA Obj","YA Sce","OA Sce"))

boxplot(dprimeTable$FAs_recog~interaction(dprimeTable$Group,dprimeTable$Obj_Sce), 
        main = "Recognition FAs",
        xlab = "Obj_Sce by Group",
        ylab = "FAs",
        names = c("YA Obj","OA Obj","YA Sce","OA Sce"))

boxplot(dprimeTable$Hits_recall~interaction(dprimeTable$Group,dprimeTable$Obj_Sce), 
        main = "Recall HITS",
        xlab = "Obj_Sce by Group",
        ylab = "HITS",
        names = c("YA Obj","OA Obj","YA Sce","OA Sce"))

boxplot(dprimeTable$FAs_recall~interaction(dprimeTable$Group,dprimeTable$Obj_Sce), 
        main = "Recall FAs",
        xlab = "Obj_Sce by Group",
        ylab = "FAs",
        names = c("YA Obj","OA Obj","YA Sce","OA Sce"))

 
ggplot(dprimeTable, aes(x = Obj_Sce, y = dprime_recog, fill = Obj_Sce)) +
  geom_bar(position = position_dodge(),stat = "identity", color = "black") + 
  scale_x_discrete("Image Type", labels = c("Objects", "Scenes")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[4], cbbPalette[2]),
                    labels = c("Objects", "Scenes")) +
  guides(fill=FALSE)+
  ylab("d'") + 
  ggtitle("Individual Subject Recognition Performance") +  
  theme(text = element_text(size=20)) +
  facet_wrap(~Subject, ncol = 5, labeller = "label_both") 


ggplot(dprimeTable, aes(x = Obj_Sce, y = dprime_recall, fill = Obj_Sce)) +
  geom_bar(position = position_dodge(),stat = "identity", color = "black") + 
  scale_x_discrete("Image Type", labels = c("Objects", "Scenes")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[8], cbbPalette[3]),
                    labels = c("Objects", "Scenes")) +
  guides(fill=FALSE)+
  ylab("Visual Learning Score") + 
  ggtitle("Individual Subject Recall Performance") +  
  theme(text = element_text(size=20)) +
  facet_wrap(~Subject, ncol = 5, labeller = "label_both") 

#Recog.anova <- aov(dprimeTable$dprime_recog~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(Recog.anova)

#library(tidyr)
dprime_wide <- dprimeTable %>% pivot_wider(id_cols = c("Subject", "nam_unn"), names_from = "RS_RT_N", values_from = "dprime")

dprime_wide$RS_RT <- dprime_wide$RT - dprime_wide$RS

diff_wide <- dprime_wide %>% pivot_wider(id_cols = c("Subject"), names_from = "nam_unn", values_from = "RS_RT")

diff_wide$avgRSRT <- (diff_wide$nam + diff_wide$unn)/2

t_test_result <- t.test(diff_wide$avgRSRT, mu = 0, alternative = "two.sided")
print(t_test_result)


t_test_RSRT <- t.test(diff_wide$nam, diff_wide$unn, alternative = "two.sided", mu = 0, paired = FALSE)
print(t_test_RSRT)

#Recall.anova <- aov(dprimeTable$dprime_recall~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(Recall.anova)

#RecogF.anova <- aov(dprimeTable$FAs_recog~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(RecogF.anova)

#RecogH.anova <- aov(dprimeTable$Hits_recog~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(RecogH.anova)

#RecallH.anova <- aov(dprimeTable$Hits_recall~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(RecallH.anova)

#RecallF.anova <- aov(dprimeTable$FAs_recall~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(RecallF.anova)

#YulesStudied.anova <- aov(dprimeTable$Yules_Studied~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(YulesStudied.anova)

#YulesUnstudied.anova <- aov(dprimeTable$Yules_Unstudied~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(YulesUnstudied.anova)

##ANOVAS REGULAR
Recall.anova <- aov(dprimeTable$dprime~dprimeTable$nam_unn*dprimeTable$RS_RT_N)
summary(Recall.anova)

RecallH.anova <- aov(dprimeTable$Hits~dprimeTable$nam_unn*dprimeTable$RS_RT_N)
summary(RecallH.anova)

RecallF.anova <- aov(dprimeTable$FAs~dprimeTable$nam_unn*dprimeTable$RS_RT_N)
summary(RecallF.anova)

## t-tests filtering
Nam_RS = filter(dprimeTable, nam_unn == "nam" & RS_RT_N == "ApRS")
Nam_RT = filter(dprimeTable, nam_unn == "nam" & RS_RT_N == "ApRT")
Unn_RS = filter(dprimeTable, nam_unn == "unn" & RS_RT_N == "ApRS")
Unn_RT = filter(dprimeTable, nam_unn == "unn" & RS_RT_N == "ApRT")

Nam_RSH = filter(dprimeTable, nam_unn == "nam" & RS_RT_N == "RS")
Nam_RTH = filter(dprimeTable, nam_unn == "nam" & RS_RT_N == "RT")
Unn_RSH = filter(dprimeTable, nam_unn == "unn" & RS_RT_N == "RS")
Unn_RTH = filter(dprimeTable, nam_unn == "unn" & RS_RT_N == "RT")

Nam_RSF = filter(dprimeTable, nam_unn == "nam" & RS_RT_N == "RS")
Nam_RTF = filter(dprimeTable, nam_unn == "nam" & RS_RT_N == "RT")
Unn_RSF = filter(dprimeTable, nam_unn == "unn" & RS_RT_N == "RS")
Unn_RTF = filter(dprimeTable, nam_unn == "unn" & RS_RT_N == "RT")

##d'

t.test(Nam_RS$dprime, Nam_RT$dprime, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RS$dprime, Unn_RT$dprime, alternative = "two.sided", mu = 0, paired = FALSE)


##Hits

t.test(Nam_RSH$Hits, Nam_RTH$Hits, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RSH$Hits, Unn_RTH$Hits, alternative = "two.sided", mu = 0, paired = FALSE)


##FAs

t.test(Nam_RSF$FAs, Nam_RTF$FAs, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RSF$FAs, Unn_RTF$FAs, alternative = "two.sided", mu = 0, paired = FALSE)







#mixed ANOVAS
Recog.aov <- anova_test(
  data = dprimeTable, dv = dprime_recog, wid = Subject,
  between = Group, within = Obj_Sce
)
get_anova_table(Recog.aov)

Recall.aov <- anova_test(
  data = dprimeTable, dv = dprime_recall, wid = Subject,
  between = Group, within = Obj_Sce
)
get_anova_table(Recall.aov)

RecogHITS.aov <- anova_test(
  data = dprimeTable, dv = Hits_recog, wid = Subject,
  between = Group, within = Obj_Sce
)
get_anova_table(RecogHITS.aov)

RecogFAS.aov <- anova_test(
  data = dprimeTable, dv = FAs_recog, wid = Subject,
  between = Group, within = Obj_Sce
)
get_anova_table(RecogFAS.aov)

RecallHITS.aov <- anova_test(
  data = dprimeTable, dv = Hits_recall, wid = Subject,
  between = Group, within = Obj_Sce
)
get_anova_table(RecallHITS.aov)

RecallFAS.aov <- anova_test(
  data = dprimeTable, dv = FAs_recall, wid = Subject,
  between = Group, within = Obj_Sce
)
get_anova_table(RecallFAS.aov)


SYules.aov <- anova_test(
  data = dprimeTable, dv = Yules_Studied, wid = Subject,
  between = Group, within = Obj_Sce
)
get_anova_table(SYules.aov)

UYules.aov <- anova_test(
  data = dprimeTable, dv = Yules_Unstudied, wid = Subject,
  between = Group, within = Obj_Sce
)
get_anova_table(UYules.aov)

## t-tests from Ross et al
#objRatesDat = filter(ratesDat, Obj_Sce == 1)
#oldObjRatesDat = filter(objRatesDat, Old == 1)
#newObjRatesDat = filter(objRatesDat, Old == 0)
#t.test(oldObjRatesDat$Prop, newObjRatesDat$Prop, alternative = "two.sided", mu = 0, paired = TRUE)
#mean(oldObjRatesDat$Prop)
#mean(newObjRatesDat$Prop)

#sceRatesDat = filter(ratesDat, Obj_Sce == 2)
#oldSceRatesDat = filter(sceRatesDat, Old == 1)
#newSceRatesDat = filter(sceRatesDat, Old == 0)
#t.test(oldSceRatesDat$Prop, newSceRatesDat$Prop, alternative = "two.sided", mu = 0, paired = TRUE)
#mean(oldSceRatesDat$Prop)
#mean(newSceRatesDat$Prop)

#library(psych)
#objYuleDat = matrix(c(sum(oldObjRatesDat$nRespOld), (sum(oldObjRatesDat$N)-sum(oldObjRatesDat$nRespOld)), sum(newObjRatesDat$nRespOld), (sum(newObjRatesDat$N)-sum(newObjRatesDat$nRespOld))), ncol = 2, byrow=TRUE)
#YuleBonett(objYuleDat,1)

#sceYuleDat = matrix(c(sum(oldSceRatesDat$nRespOld), (sum(oldSceRatesDat$N)-sum(oldSceRatesDat$nRespOld)), sum(newSceRatesDat$nRespOld), (sum(newSceRatesDat$N)-sum(newSceRatesDat$nRespOld))), ncol = 2, byrow=TRUE)
#YuleBonett(sceYuleDat,1)

## t-tests Screen size & compensation (MODIFY SUBJECTS TO RUN)
SonaDPrimeObj = filter(dprimeTable, Group == 1 & Obj_Sce == 1)
PaidDPrimeObj = filter(dprimeTable, Group == 0 & Obj_Sce == 1)
t.test(SonaDPrimeObj$dprime_recog, PaidDPrimeObj$dprime_recog, alternative = "two.sided", mu = 0, paired = FALSE)

t.test(SonaDPrimeObj$dprime_recall, PaidDPrimeObj$dprime_recall, alternative = "two.sided", mu = 0, paired = FALSE)


SonaDPrimeSce = filter(dprimeTable, Group == 1 & Obj_Sce == 2)
PaidDPrimeSce = filter(dprimeTable, Group == 0 & Obj_Sce == 2)
t.test(SonaDPrimeSce$dprime_recog, PaidDPrimeSce$dprime_recog, alternative = "two.sided", mu = 0, paired = FALSE)

t.test(SonaDPrimeSce$dprime_recall, PaidDPrimeSce$dprime_recall, alternative = "two.sided", mu = 0, paired = FALSE)

## t-tests word frequencies
SE_50_O <- summarySE(Frequencies, measurevar="50_O")
SE_50_S <- summarySE(Frequencies, measurevar="50_S")
SE_20_O <- summarySE(Frequencies, measurevar="20_O")
SE_20_S <- summarySE(Frequencies, measurevar="20_S")

t.test(Frequencies$`50_O`, Frequencies$`50_S`, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Frequencies$`20_O`, Frequencies$`20_S`, alternative = "two.sided", mu = 0, paired = FALSE)

## t-tests effect of study CHECK GROUP NUMBERS
Correct_studiedOA = filter(ratesDat, Group == 1 & Old == 1)
Correct_studiedYA = filter(ratesDat, Group == 0 & Old == 1)
Correct_unstudiedOA = filter(ratesDat, Group == 1 & Old == 0)
Correct_unstudiedYA = filter(ratesDat, Group == 0 & Old == 0)

t.test(Correct_studiedYA$Prop, Correct_unstudiedYA$Prop, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Correct_studiedYA$Prop_Recall_Correct, Correct_unstudiedYA$Prop_Recall_Correct, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Correct_studiedOA$Prop, Correct_unstudiedOA$Prop, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Correct_studiedOA$Prop_Recall_Correct, Correct_unstudiedOA$Prop_Recall_Correct, alternative = "two.sided", mu = 0, paired = FALSE)

## t-test scenes old vs young
Scenes_YA = filter(dprimeTable, Group == 1 & Obj_Sce == 2)
Scenes_OA = filter(dprimeTable, Group == 0 & Obj_Sce == 2)
t.test(Scenes_YA$dprime_recall, Scenes_OA$dprime_recall, alternative = "two.sided", mu = 0, paired = FALSE)

# t-test obj vs scenes in old and young
Scenes_YA = filter(dprimeTable, Group == 0 & Obj_Sce == 2)
Scenes_OA = filter(dprimeTable, Group == 1 & Obj_Sce == 2)
Objects_YA = filter(dprimeTable, Group == 0 & Obj_Sce == 1)
Objects_OA = filter(dprimeTable, Group == 1 & Obj_Sce == 1)
t.test(Scenes_YA$dprime_recall, Objects_YA$dprime_recall, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Scenes_OA$dprime_recall, Objects_OA$dprime_recall, alternative = "two.sided", mu = 0, paired = FALSE)

#for STATS class
mean(dprimeTable$dprime_recall)
sd(dprimeTable$dprime_recall)

hist(dprimeTable$dprime_recall)
desc(dprimeTable$dprime_recall)
