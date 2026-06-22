# Load libraries
library(tidyverse)
library(Rmisc)
library(ggplot2)

# List of participant IDs
SONAsubID <- c(551,552,554,555, 603:608, 610:613, 615:618, 620:632,634:642,644:647, 649:659, 661:663, 665, 666, 670:672)

# Initialize output dataframe
ratesDat <- data.frame(matrix(NA, length(SONAsubID) * 6, 6))
colnames(ratesDat) <- c('participant', 'nam_unn', 'rs_rt_n', 'N', 'nCorrect', 'PropCorrect')

subCt <- 0

# Loop over participants
for (s in 1:length(SONAsubID)) {
  fName <- paste0("C:/Users/jgove/OneDrive/Desktop/Nameable and Unnameable Objects/data shaping/STACKED/stacked_", SONAsubID[s], ".csv")
  
  if (!file.exists(fName)) next  # Skip if file not found
  
  dat <- read.csv(fName)
  
  # Prepare columns
  dat$nam_unn <- as.factor(dat$nam_unn)
  dat$rs_rt_n <- as.factor(dat$rs_rt_n)
  dat$isCorrect <- as.logical(dat$isCorrect)
  dat$isCorrect[is.na(dat$isCorrect)] <- FALSE  # Treat NA as incorrect
  
  subCt <- subCt + 1
  condCt <- 0
  
  # Loop over conditions
  for (n in levels(dat$nam_unn)) {
    for (r in levels(dat$rs_rt_n)) {
      condCt <- condCt + 1
      rowIdx <- (subCt - 1) * 6 + condCt
      
      filt_dat <- dat %>% filter(nam_unn == n, rs_rt_n == r)
      
      ratesDat$participant[rowIdx] <- SONAsubID[s]
      ratesDat$nam_unn[rowIdx] <- n
      ratesDat$rs_rt_n[rowIdx] <- r
      ratesDat$N[rowIdx] <- nrow(filt_dat)
      ratesDat$nCorrect[rowIdx] <- sum(filt_dat$isCorrect == TRUE)  # Count TRUEs
      
      # Adjusted Wald accuracy (handle edge cases)
      ratesDat$PropCorrect[rowIdx] <- (ratesDat$nCorrect[rowIdx] + 0.5) / (ratesDat$N[rowIdx] + 1)
    }
  }
}

# Summary stats
SE_accuracy <- summarySE(ratesDat, measurevar = "PropCorrect", groupvars = c("nam_unn", "rs_rt_n"))

# Plotting
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# ===== Exp 5: Scatter-style accuracy plot (PropCorrect) + aligned subject lines =====

library(dplyr)
library(tidyr)
library(ggplot2)
library(Rmisc)
library(grid)

# ----------------------------
# 0) Clean + standardize labels
# ----------------------------

acc5 <- ratesDat %>%
  mutate(
    participant = as.character(participant),
    
    nam_unn = trimws(as.character(nam_unn)),
    nam_unn = recode(
      nam_unn,
      "nam"        = "Meaningful",
      "unn"        = "Abstract",
      "Nameable"   = "Meaningful",
      "Meaningful" = "Meaningful",
      "Abstract"   = "Abstract",
      .default = nam_unn
    ),
    
    rs_rt_n = trimws(as.character(rs_rt_n)),
    rs_rt_n = recode(
      rs_rt_n,
      "N"         = "Novel",
      "RS"        = "Restudy",
      "RT"        = "Retrieval",
      "Novel"     = "Novel",
      "Restudy"   = "Restudy",
      "Retrieval" = "Retrieval",
      .default = rs_rt_n
    ),
    
    PropCorrect = as.numeric(PropCorrect)
  ) %>%
  filter(
    nam_unn %in% c("Meaningful", "Abstract"),
    rs_rt_n %in% c("Novel", "Restudy", "Retrieval"),
    is.finite(PropCorrect)
  ) %>%
  mutate(
    nam_unn = factor(nam_unn, levels = c("Meaningful", "Abstract")),
    rs_rt_n = factor(rs_rt_n, levels = c("Novel", "Restudy", "Retrieval"))
  )

# ----------------------------
# 1) Colors
# ----------------------------

cbbPalette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

cond_cols3 <- c(
  "Novel"     = cbbPalette[4],
  "Restudy"   = cbbPalette[2],
  "Retrieval" = cbbPalette[8]
)

# ----------------------------
# 2) Spacing controls
# ----------------------------

set.seed(123)

meaningful_center <- .9
abstract_center   <- 2.1

spacing_factor <- 0.3
jitter_amount  <- 0.08

# ------------------------------------------
# 3) Original condition summary table
# ------------------------------------------

SE_acc5 <- summarySE(
  acc5,
  measurevar = "PropCorrect",
  groupvars = c("nam_unn", "rs_rt_n")
)

# ------------------------------------------
# 4) X positions + constant subject jitter
# ------------------------------------------

base_x5 <- acc5 %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Meaningful",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(rs_rt_n),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

subj_offsets5 <- base_x5 %>%
  distinct(participant, nam_unn) %>%
  mutate(
    subj_jit = jitter(rep(0, n()), amount = jitter_amount)
  )

plot_x5 <- base_x5 %>%
  left_join(subj_offsets5, by = c("participant", "nam_unn")) %>%
  mutate(
    x_plot = x_mean + subj_jit
  )

SE_acc5_x <- SE_acc5 %>%
  mutate(
    nam_unn = factor(nam_unn, levels = levels(acc5$nam_unn)),
    rs_rt_n = factor(rs_rt_n, levels = levels(acc5$rs_rt_n)),
    stim_center = ifelse(
      nam_unn == "Meaningful",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(rs_rt_n),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

# ------------------------------------------
# 5) Plot
# ------------------------------------------

p5_acc <- ggplot() +
  
  geom_point(
    data = plot_x5,
    aes(x = x_plot, y = PropCorrect, color = rs_rt_n),
    size = 3,
    alpha = 0.7
  ) +
  
  geom_line(
    data = plot_x5 %>% arrange(nam_unn, participant, rs_rt_n),
    aes(
      x = x_plot,
      y = PropCorrect,
      group = interaction(participant, nam_unn)
    ),
    alpha = 0.35,
    color = "grey"
  ) +
  
  geom_point(
    data = SE_acc5_x,
    aes(x = x_mean, y = PropCorrect),
    inherit.aes = FALSE,
    shape = 18,
    size = 4,
    color = "black"
  ) +
  
  geom_errorbar(
    data = SE_acc5_x,
    aes(
      x = x_mean,
      ymin = PropCorrect - se,
      ymax = PropCorrect + se
    ),
    inherit.aes = FALSE,
    width = 0.12,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(meaningful_center, abstract_center),
    labels = c("Meaningful", "Abstract"),
    limits = c(0.45, 2.55)
  ) +
  
  scale_color_manual(
    name = NULL,
    values = cond_cols3,
    breaks = c("Novel", "Restudy", "Retrieval")
  ) +
  
  ylab("Percent Correct") +
  xlab("Stimulus Type") +
  ggtitle("Memory Performance Exp. 5") +
  
  theme_bw() +
  theme(
    plot.title = element_text(size = 30),
    
    axis.text = element_text(size = 30),
    
    axis.title.x = element_text(
      size = 30,
      margin = margin(t = 10)
    ),
    
    axis.title.y = element_text(
      size = 30,
      margin = margin(r = 10)
    ),
    
    legend.text = element_text(size = 30),
    legend.title = element_blank(),
    
    legend.spacing.y = unit(1, "cm"),
    legend.key.height = unit(1.2, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p5_acc)
print(SE_acc5)

# Collapse accuracy across nam/unn
collapsed_5 <- ratesDat %>%
  dplyr::group_by(participant, rs_rt_n) %>%
  dplyr::summarise(
    PropCorrect = mean(PropCorrect, na.rm = TRUE),
    .groups = "drop"
  )

# Means and SDs collapsed across nam/unn
desc_5 <- collapsed_5 %>%
  dplyr::group_by(rs_rt_n) %>%
  dplyr::summarise(
    N = dplyr::n(),
    M_PropCorrect = mean(PropCorrect, na.rm = TRUE),
    SD_PropCorrect = sd(PropCorrect, na.rm = TRUE),
    .groups = "drop"
  )

desc_5

# Wide format for paired t-tests
wide_5 <- collapsed_5 %>%
  tidyr::pivot_wider(
    id_cols = participant,
    names_from = rs_rt_n,
    values_from = PropCorrect
  )

# Paired t-tests collapsed across nam/unn
t.test(wide_5$RS, wide_5$RT, paired = TRUE)
t.test(wide_5$RT, wide_5$N,  paired = TRUE)
t.test(wide_5$RS, wide_5$N,  paired = TRUE)

# Testing effect = Retrieval Practice minus Restudy, separately by stim type
testing_effect_5 <- ratesDat %>%
  dplyr::filter(rs_rt_n %in% c("RS", "RT")) %>%
  tidyr::pivot_wider(
    id_cols = c(participant, nam_unn),
    names_from = rs_rt_n,
    values_from = PropCorrect
  ) %>%
  dplyr::mutate(
    testing_effect = RT - RS
  )

# Means and SDs of testing effect by stim type
testing_effect_desc_5 <- testing_effect_5 %>%
  dplyr::group_by(nam_unn) %>%
  dplyr::summarise(
    N = dplyr::n(),
    M_testing_effect = mean(testing_effect, na.rm = TRUE),
    SD_testing_effect = sd(testing_effect, na.rm = TRUE),
    .groups = "drop"
  )

testing_effect_desc_5

# Wide format for Abstract vs Meaningful testing effect comparison
testing_effect_wide_5 <- testing_effect_5 %>%
  tidyr::pivot_wider(
    id_cols = participant,
    names_from = nam_unn,
    values_from = testing_effect
  )

# Check column names first
colnames(testing_effect_wide_5)

# If labels are nam/unn:
t.test(
  testing_effect_wide_5$unn,
  testing_effect_wide_5$nam,
  paired = TRUE
)

# If labels are Abstract/Meaningful instead, use:
# t.test(
#   testing_effect_wide_5$Abstract,
#   testing_effect_wide_5$Meaningful,
#   paired = TRUE
# )

# =========================
# MISSING RESPONSE SUMMARY: EXP 5
# =========================
library(dplyr)
library(ggplot2)

missing_summary <- data.frame(
  participant = numeric(),
  n_missing = numeric(),
  total_trials = numeric(),
  prop_missing = numeric()
)

for (s in 1:length(SONAsubID)) {
  
  fName <- paste0(
    "C:/Users/jgove/OneDrive/Desktop/Nameable and Unnameable Objects/data shaping/STACKED/redo/stacked_",
    SONAsubID[s],
    ".csv"
  )
  
  if (!file.exists(fName)) next
  
  dat <- read.csv(fName, stringsAsFactors = FALSE)
  
  # count only actual experimental trials
  trial_rows <- !is.na(dat$nam_unn) & !is.na(dat$rs_rt_n)
  
  total_trials <- sum(trial_rows)
  
  # missing responses = NA in isCorrect
  n_missing <- sum(
    trial_rows & is.na(dat$isCorrect)
  )
  
  prop_missing <- n_missing / total_trials
  
  missing_summary <- rbind(
    missing_summary,
    data.frame(
      participant = SONAsubID[s],
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
  ggtitle("Exp. 5 Missing Responses by Participant") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18)
  )


library(dplyr)
library(rstatix)

library(dplyr)
library(rstatix)
library(tidyr)

# =========================================================
# ANOVA for accuracy
# =========================================================
anova_acc_dat <- ratesDat %>%
  mutate(
    participant = factor(participant),
    
    rs_rt_n = recode(as.character(rs_rt_n),
                     "N"  = "Novel",
                     "RS" = "Restudy",
                     "RT" = "Retrieval",
                     .default = as.character(rs_rt_n)),
    
    StudyCond = factor(rs_rt_n, levels = c("Novel", "Restudy", "Retrieval")),
    
    StimType = factor(
      nam_unn,
      levels = levels(factor(nam_unn)),
      labels = c("Meaningful", "Abstract")
    )
  ) %>%
  select(participant, StimType, StudyCond, PropCorrect) %>%
  filter(is.finite(PropCorrect), !is.na(StimType), !is.na(StudyCond))

anova_acc_res <- anova_test(
  data = anova_acc_dat,
  dv = PropCorrect,
  wid = participant,
  within = c(StimType, StudyCond)
)

# full output: includes Mauchly + sphericity corrections
print(anova_acc_res)

# compact ANOVA table
anova_acc_tab <- get_anova_table(anova_acc_res)
print(anova_acc_tab, width = Inf)

# =========================================================
# Post-hoc paired t-tests for accuracy
# =========================================================
posthoc_acc <- anova_acc_dat %>%
  group_by(StimType) %>%
  pairwise_t_test(
    PropCorrect ~ StudyCond,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE
  )

# effect sizes
eff_posthoc_acc <- anova_acc_dat %>%
  group_by(StimType) %>%
  cohens_d(
    PropCorrect ~ StudyCond,
    paired = TRUE
  )

# merge t-tests with effect sizes
posthoc_acc_full <- posthoc_acc %>%
  left_join(
    eff_posthoc_acc %>%
      select(StimType, group1, group2, effsize, magnitude),
    by = c("StimType", "group1", "group2")
  )

print(posthoc_acc_full, width = Inf)
print(SE_accuracy)


ggplot(data = SE_accuracy,
       aes(x = nam_unn, y = PropCorrect, fill = rs_rt_n)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = PropCorrect - se, ymax = PropCorrect + se),
                width = 0.2, position = position_dodge(0.9), color = "black") +
  scale_x_discrete("Stimulus Type", labels = c("Meaningful", "Abstract")) +
  scale_fill_manual(name = "Condition", values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) +
  ylab("Percent Correct") +
  ggtitle("Memory Performance Exp 5") +
  theme_bw() +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))






# Prepare for ANOVA
correct_wide_long <- correct_wide %>%
  select(participant, nam_unn, RS_N, RT_N) %>%
  pivot_longer(cols = c(RS_N, RT_N), names_to = "Condition", values_to = "AccuracyDiff") %>%
  mutate(
    Condition = recode(Condition, RS_N = "Restudy", RT_N = "Retrieval"),
    StimType = ifelse(nam_unn == "Nameable", "Nameable", "Abstract")
  )

# Convert to factors
correct_wide_long <- correct_wide_long %>%
  mutate(across(c(participant, Condition, StimType), as.factor))

Accuracy.anova <- aov(AccuracyDiff ~ Condition * nam_unn, data = correct_wide_long)
summary(Accuracy.anova)

## t-tests filtering
Nam_RS = filter(correct_wide_long, nam_unn == "nam" & Condition == "Restudy")
Nam_RT = filter(correct_wide_long, nam_unn == "nam" & Condition == "Retrieval")
Unn_RS = filter(correct_wide_long, nam_unn == "unn" & Condition == "Restudy")
Unn_RT = filter(correct_wide_long, nam_unn == "unn" & Condition == "Retrieval")

##d'
t.test(Nam_RS$AccuracyDiff, Nam_RT$AccuracyDiff, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RS$AccuracyDiff, Unn_RT$AccuracyDiff, alternative = "two.sided", mu = 0, paired = FALSE)
