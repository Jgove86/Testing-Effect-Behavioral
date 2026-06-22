install.packages("Rmisc")
library(Rmisc) 
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("rstatix")
library(rstatix)
library(dplyr)

SONAsubID <- c(101,104:105,107,110,112,114,116,118,120,123:125,127,130,134,135,137,141,143,145:146,148,150,152,154:157,159:161,163:170,171,173:177,179:188)
allSubID <- SONAsubID

ratesDat <- data.frame(matrix(NA, length(allSubID) * 12, 6))
colnames(ratesDat) <- c('Subject', 'nam_unn', 'RS_RT_N', 'N', 'nRespRight', 'PropRight')

dprimeTable <- data.frame(matrix(NA, length(allSubID) * 6, 7))
colnames(dprimeTable) <- c('Subject', 'nam_unn', 'RS_RT_N', 'Hits', 'FAs', 'dprime', 'criterion')

subCt <- 0

library(dplyr)

for (s in 1:length(allSubID)) {
  fName <- paste('C:/Users/jgove/OneDrive/Desktop/Nameable and Unnameable Objects/data shaping/Formatted dataparticipant_study', allSubID[s], '.csv', sep = "")
  dat <- read.table(fName, sep = ',', header = TRUE)
  
  subCt <- subCt + 1
  
  dat$correct <- as.factor(dat$key_resp_2.corr)
  dat$nam_unn <- as.factor(dat$nam_unn)
  dat$RS_RT_N <- as.factor(dat$rs_rt_n)
  dat$old <- as.factor(dat$intact)
  
  condCt <- 0
  RS_RT_N_lvls <- levels(dat$RS_RT_N)
  old_lvls <- levels(dat$old)
  nam_unn_levels <- levels(dat$nam_unn)
  keys_levels <- levels(dat$key_resp_2.keys)
  
  for (oj in 1:length(nam_unn_levels)) {
    for (i in 1:length(RS_RT_N_lvls)) {
      for (k in 1:length(old_lvls)){
        condCt <- condCt + 1
        
        ratesDat$Subject[subCt * 12 + condCt - 12] <- allSubID[s]
        ratesDat$nam_unn[subCt * 12 + condCt - 12] <- nam_unn_levels[oj]
        ratesDat$RS_RT_N[subCt * 12 + condCt - 12] <- RS_RT_N_lvls[i]
        ratesDat$Old[subCt * 12 + condCt - 12] <- old_lvls[k]
        
        filt_dat <- filter(filter(dat, nam_unn == nam_unn_levels[oj]), RS_RT_N == RS_RT_N_lvls[i], old == old_lvls[k])
        
        ratesDat$N[subCt * 12 + condCt - 12] <- nrow(filt_dat)
        ratesDat$nRespRight[subCt * 12 + condCt - 12] <- sum(filt_dat$key_resp_2.keys == "right")
        
        ratesDat$PropRight[subCt * 12 + condCt - 12] <- (ratesDat$nRespRight[subCt * 12 + condCt - 12]+.5) / (ratesDat$N[subCt * 12 + condCt - 12]+1)
        
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

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  # black, gold, light blue, green, yellow, blue, dark orange, pink

# ===== Scatter-style plot: points + subject lines + mean + SE =====

library(dplyr)
library(ggplot2)
library(Rmisc)
library(grid)

# ---- recode to pretty labels + enforce plotting order ----
dprimeTable <- dprimeTable %>%
  mutate(
    nam_unn = recode(nam_unn, nam = "Nameable", unn = "Abstract"),
    RS_RT_N = recode(RS_RT_N, N = "Novel", RS = "Restudy", RT = "Retrieval"),
    nam_unn = factor(nam_unn, levels = c("Nameable", "Abstract")),
    RS_RT_N = factor(RS_RT_N, levels = c("Novel", "Restudy", "Retrieval"))
  ) %>%
  filter(is.finite(dprime))

# ---- mean + SE ----
SE_dprime <- summarySE(
  dprimeTable,
  measurevar = "dprime",
  groupvars = c("nam_unn", "RS_RT_N")
)

# ---- spacing controls ----
set.seed(123)

meaningful_center <- 1.00
abstract_center   <- 2.00

spacing_factor <- 0.20
jitter_amount  <- 0.06

# ---- participant x positions ----
dprime_base <- dprimeTable %>%
  mutate(
    stim_center = ifelse(nam_unn == "Nameable",
                         meaningful_center,
                         abstract_center),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

# ---- one jitter offset per subject within each stimulus type ----
subj_offsets <- dprime_base %>%
  distinct(Subject, nam_unn) %>%
  mutate(subj_jit = jitter(rep(0, n()), amount = jitter_amount))

# ---- apply same jitter to all three condition points ----
dprime_plot <- dprime_base %>%
  left_join(subj_offsets, by = c("Subject", "nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

# ---- mean and SE x positions ----
SE_dprime_plot <- SE_dprime %>%
  mutate(
    stim_center = ifelse(nam_unn == "Nameable",
                         meaningful_center,
                         abstract_center),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

# ---- colors ----
cbbPalette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

# ---- plot ----
p <- ggplot() +
  
  geom_point(
    data = dprime_plot,
    aes(x = x_plot, y = dprime, color = RS_RT_N),
    size = 3,
    alpha = 0.7
  ) +
  
  geom_line(
    data = dprime_plot %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = dprime, group = interaction(Subject, nam_unn)),
    alpha = 0.35,
    color = "grey"
  ) +
  
  geom_point(
    data = SE_dprime_plot,
    aes(x = x_mean, y = dprime),
    inherit.aes = FALSE,
    shape = 18,
    size = 4,
    color = "black"
  ) +
  
  geom_errorbar(
    data = SE_dprime_plot,
    aes(x = x_mean, ymin = dprime - se, ymax = dprime + se),
    inherit.aes = FALSE,
    width = 0.10,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(meaningful_center, abstract_center),
    labels = c("Meaningful", "Abstract")
  ) +
  
  scale_color_manual(
    name = NULL,
    values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
    breaks = c("Novel", "Restudy", "Retrieval")
  ) +
  
  xlab("Stimulus Type") +
  ylab("Visual Recognition (d')") +
  ggtitle("Memory Performance Exp. 1") +
  
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

print(p)

##COLLAPSED T TESTS
# Collapse dprime across nam/unn for each subject and condition
dprime_collapsed <- dprimeTable %>%
  dplyr::group_by(Subject, RS_RT_N) %>%
  dplyr::summarise(
    dprime = mean(dprime, na.rm = TRUE),
    .groups = "drop"
  )

dprime_wide <- dprime_collapsed %>%
  tidyr::pivot_wider(
    id_cols = Subject,
    names_from = RS_RT_N,
    values_from = dprime
  )

t.test(dprime_wide$RS, dprime_wide$RT, paired = TRUE)
t.test(dprime_wide$RT, dprime_wide$N,  paired = TRUE)
t.test(dprime_wide$RS, dprime_wide$N,  paired = TRUE)

# Print results
t_restudy_retrieval
t_retrieval_novel
t_restudy_novel

dprime_desc <- dprime_collapsed %>%
  dplyr::group_by(RS_RT_N) %>%
  dplyr::summarise(
    N = dplyr::n(),
    M = mean(dprime, na.rm = TRUE),
    SD = sd(dprime, na.rm = TRUE),
    .groups = "drop"
  )

dprime_desc


fa_collapsed <- dprimeTable %>%
  dplyr::group_by(Subject, RS_RT_N) %>%
  dplyr::summarise(
    FAs = mean(FAs, na.rm = TRUE),
    .groups = "drop"
  )

fa_wide <- fa_collapsed %>%
  tidyr::pivot_wider(
    id_cols = Subject,
    names_from = RS_RT_N,
    values_from = FAs
  )

# Paired t-tests
t.test(fa_wide$RS, fa_wide$RT, paired = TRUE)
t.test(fa_wide$RT, fa_wide$N,  paired = TRUE)
t.test(fa_wide$RS, fa_wide$N,  paired = TRUE)

fa_desc <- fa_collapsed %>%
  dplyr::group_by(RS_RT_N) %>%
  dplyr::summarise(
    N = dplyr::n(),
    M = mean(FAs, na.rm = TRUE),
    SD = sd(FAs, na.rm = TRUE),
    .groups = "drop"
  )

fa_desc

# =========================
# MISSING RESPONSE SUMMARY: EXP 1
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
    'C:/Users/jgove/OneDrive/Desktop/Nameable and Unnameable Objects/data shaping/Formatted dataparticipant_study',
    allSubID[s],
    '.csv',
    sep = ""
  )
  
  dat <- read.csv(fName, stringsAsFactors = FALSE)
  
  # count only real experimental trials
  trial_rows <- !is.na(dat$nam_unn) & !is.na(dat$rs_rt_n)
  
  total_trials <- sum(trial_rows)
  
  # missing responses = NA or "None" in key_resp_2.keys
  n_missing <- sum(
    trial_rows &
      (is.na(dat$key_resp_2.keys) | dat$key_resp_2.keys == "None")
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
  ggtitle("Exp. 1 Missing Responses by Participant") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18)
  )


## 3x2 ANOVA

library(dplyr)
library(rstatix)

anova_dat <- dprimeTable %>%
  mutate(
    Subject = factor(Subject),
    StimType = factor(nam_unn, levels = c("Nameable", "Abstract")),
    StudyCond = factor(RS_RT_N, levels = c("Novel", "Restudy", "Retrieval"))
  ) %>%
  select(Subject, StimType, StudyCond, dprime) %>%
  filter(is.finite(dprime), !is.na(StimType), !is.na(StudyCond))

anova_res <- anova_test(
  data = anova_dat,
  dv = dprime,
  wid = Subject,
  within = c(StimType, StudyCond)
)

anova_tab <- get_anova_table(anova_res)
print(anova_res, width = Inf)
print(anova_tab, width = Inf)

## POSTHOC t-tests
posthoc_dprime <- anova_dat %>%
  group_by(StimType) %>%
  pairwise_t_test(
    dprime ~ StudyCond,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE
  )

## EFFECT SIZES
eff_posthoc_dprime <- anova_dat %>%
  group_by(StimType) %>%
  cohens_d(
    dprime ~ StudyCond,
    paired = TRUE,
    var.equal = FALSE
  )

## Merge them
posthoc_dprime_full <- posthoc_dprime %>%
  left_join(
    eff_posthoc_dprime %>%
      select(StimType, group1, group2, effsize, magnitude),
    by = c("StimType", "group1", "group2")
  )

print(posthoc_dprime_full, width = Inf)

cell_means <- dplyr::summarise(
  dplyr::group_by(anova_dat, StimType, StudyCond),
  Mean = mean(dprime, na.rm = TRUE),
  SD   = sd(dprime, na.rm = TRUE),
  N    = dplyr::n(),
  .groups = "drop"
)

print(cell_means)
print(SE_dprime)
print(SE_HITS)
print(SE_FA)

library(dplyr)
library(rstatix)

library(dplyr)
library(rstatix)

library(dplyr)
library(rstatix)
library(tidyr)

# =========================================================
# ANOVA for HITS
# =========================================================
anova_hits_dat <- dprimeTable %>%
  mutate(
    Subject = factor(Subject),
    StimType = factor(nam_unn, levels = c("Nameable", "Abstract")),
    StudyCond = factor(RS_RT_N, levels = c("Novel", "Restudy", "Retrieval"))
  ) %>%
  select(Subject, StimType, StudyCond, Hits) %>%
  filter(is.finite(Hits), !is.na(StimType), !is.na(StudyCond))

anova_hits_res <- anova_test(
  data = anova_hits_dat,
  dv = Hits,
  wid = Subject,
  within = c(StimType, StudyCond)
)

# full output: includes Mauchly + sphericity corrections
print(anova_hits_res)

# compact ANOVA table
anova_hits_tab <- get_anova_table(anova_hits_res)
print(anova_hits_tab, width = Inf)

# =========================================================
# Post-hoc paired t-tests for HITS
# =========================================================
posthoc_hits <- anova_hits_dat %>%
  group_by(StimType) %>%
  pairwise_t_test(
    Hits ~ StudyCond,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE
  )

# effect sizes for HITS post hocs
eff_posthoc_hits <- anova_hits_dat %>%
  group_by(StimType) %>%
  cohens_d(
    Hits ~ StudyCond,
    paired = TRUE
  )

# merged table
posthoc_hits_full <- posthoc_hits %>%
  left_join(
    eff_posthoc_hits %>%
      select(StimType, group1, group2, effsize, magnitude),
    by = c("StimType", "group1", "group2")
  )

print(posthoc_hits_full, width = Inf)

SE_HITS <- summarySE(plot_dat,
                     measurevar="Hits",
                     groupvars=c("nam_unn","RS_RT_N"))
# =========================================================
# ANOVA for FAs
# =========================================================
anova_fa_dat <- dprimeTable %>%
  mutate(
    Subject = factor(Subject),
    StimType = factor(nam_unn, levels = c("Nameable", "Abstract")),
    StudyCond = factor(RS_RT_N, levels = c("Novel", "Restudy", "Retrieval"))
  ) %>%
  select(Subject, StimType, StudyCond, FAs) %>%
  filter(is.finite(FAs), !is.na(StimType), !is.na(StudyCond))

anova_fa_res <- anova_test(
  data = anova_fa_dat,
  dv = FAs,
  wid = Subject,
  within = c(StimType, StudyCond)
)

# full output: includes Mauchly + sphericity corrections
print(anova_fa_res)

# compact ANOVA table
anova_fa_tab <- get_anova_table(anova_fa_res)
print(anova_fa_tab, width = Inf)

# =========================================================
# Post-hoc paired t-tests for FAs
# =========================================================
posthoc_fa <- anova_fa_dat %>%
  group_by(StimType) %>%
  pairwise_t_test(
    FAs ~ StudyCond,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE
  )

# effect sizes for FA post hocs
eff_posthoc_fa <- anova_fa_dat %>%
  group_by(StimType) %>%
  cohens_d(
    FAs ~ StudyCond,
    paired = TRUE
  )

# merged table
posthoc_fa_full <- posthoc_fa %>%
  left_join(
    eff_posthoc_fa %>%
      select(StimType, group1, group2, effsize, magnitude),
    by = c("StimType", "group1", "group2")
  )

print(posthoc_fa_full, width = Inf)


# ===== Scatter-style HITS + FA plots: points + subject lines + mean + SE =====

library(dplyr)
library(ggplot2)
library(Rmisc)
library(grid)

# ---- recode to pretty labels + enforce plotting order ----
dprimeTable <- dprimeTable %>%
  mutate(
    nam_unn = recode(nam_unn, nam = "Nameable", unn = "Abstract"),
    RS_RT_N = recode(RS_RT_N, N = "Novel", RS = "Restudy", RT = "Retrieval"),
    nam_unn = factor(nam_unn, levels = c("Nameable", "Abstract")),
    RS_RT_N = factor(RS_RT_N, levels = c("Novel", "Restudy", "Retrieval"))
  )

# ---- summary tables: means + SE ----
SE_HITS <- summarySE(
  dprimeTable,
  measurevar = "Hits",
  groupvars = c("nam_unn", "RS_RT_N")
)

SE_FA <- summarySE(
  dprimeTable,
  measurevar = "FAs",
  groupvars = c("nam_unn", "RS_RT_N")
)

# ---- spacing controls ----
set.seed(123)

meaningful_center <- 1.00
abstract_center   <- 2.00

spacing_factor <- 0.20
jitter_amount  <- 0.06

# ---- participant x positions ----
base_x <- dprimeTable %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Nameable",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

# ---- one jitter offset per subject within each stimulus type ----
subj_offsets <- base_x %>%
  distinct(Subject, nam_unn) %>%
  mutate(subj_jit = jitter(rep(0, n()), amount = jitter_amount))

# ---- apply same jitter to all condition points for each subject/stimulus type ----
plot_x <- base_x %>%
  left_join(subj_offsets, by = c("Subject", "nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

# ---- summary x positions for HITS ----
SE_HITS_x <- SE_HITS %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Nameable",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

# ---- summary x positions for FAs ----
SE_FA_x <- SE_FA %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Nameable",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

# ---- colors ----
cbbPalette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

cond_colors <- c(
  "Novel"     = cbbPalette[4],
  "Restudy"   = cbbPalette[2],
  "Retrieval" = cbbPalette[8]
)

# =========================
# PLOT 1: HITS
# =========================

p_hits <- ggplot() +
  
  geom_point(
    data = plot_x,
    aes(x = x_plot, y = Hits, color = RS_RT_N),
    size = 3,
    alpha = 0.7
  ) +
  
  geom_line(
    data = plot_x %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = Hits, group = interaction(Subject, nam_unn)),
    alpha = 0.35,
    color = "grey"
  ) +
  
  geom_point(
    data = SE_HITS_x,
    aes(x = x_mean, y = Hits),
    inherit.aes = FALSE,
    shape = 18,
    size = 4,
    color = "black"
  ) +
  
  geom_errorbar(
    data = SE_HITS_x,
    aes(x = x_mean, ymin = Hits - se, ymax = Hits + se),
    inherit.aes = FALSE,
    width = 0.10,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(meaningful_center, abstract_center),
    labels = c("Meaningful", "Abstract")
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  scale_color_manual(
    name = NULL,
    values = cond_colors,
    breaks = c("Novel", "Restudy", "Retrieval")
  ) +
  
  ylab("HITS") +
  xlab("Stimulus Type") +
  ggtitle("HITS Exp. 1") +
  
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

print(p_hits)

# =========================
# PLOT 2: FAs
# =========================

p_fa <- ggplot() +
  
  geom_point(
    data = plot_x,
    aes(x = x_plot, y = FAs, color = RS_RT_N),
    size = 3,
    alpha = 0.7
  ) +
  
  geom_line(
    data = plot_x %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = FAs, group = interaction(Subject, nam_unn)),
    alpha = 0.35,
    color = "grey"
  ) +
  
  geom_point(
    data = SE_FA_x,
    aes(x = x_mean, y = FAs),
    inherit.aes = FALSE,
    shape = 18,
    size = 4,
    color = "black"
  ) +
  
  geom_errorbar(
    data = SE_FA_x,
    aes(x = x_mean, ymin = FAs - se, ymax = FAs + se),
    inherit.aes = FALSE,
    width = 0.10,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(meaningful_center, abstract_center),
    labels = c("Meaningful", "Abstract")
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  scale_color_manual(
    name = NULL,
    values = cond_colors,
    breaks = c("Novel", "Restudy", "Retrieval")
  ) +
  
  ylab("FAs") +
  xlab("Stimulus Type") +
  ggtitle("FAs Exp. 1") +
  
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

print(p_fa)


##BAR PLOTS

ggplot(data = SE_dprime,
       aes(x = nam_unn, y = dprime, fill = RS_RT_N)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymax = dprime + se, ymin = dprime - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete(labels = c("Nameable", "Abstract")) +
  scale_fill_manual(name = "Condition", values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) +
  ylab("Visual Recognition (d')") +
  ggtitle("Memory Performance Exp. 1") +
  theme_bw() +  # Place this first
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
  scale_x_discrete("Group", labels = c("Nameable", "Abstract")) + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(name = "Condition", values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) + 
  ylab("HITS") +
  ggtitle("HITS Exp. 1") +
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
  scale_x_discrete("Group", labels = c("Nameable", "Abstract")) + 
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_fill_manual(name = "Condition", values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) + 
  ylab("FAs") +
  ggtitle("FAs Exp. 1") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()


write.csv(ratesDat, file=paste("C:/Users/jgove/OneDrive/Desktop/ratesDat_4_1.csv", sep=""),row.names = F)
write.csv(pivot_df, file=paste("C:/Users/jgove/OneDrive/Desktop/pivotdf.csv", sep=""),row.names = F)

write.csv(dprimeTable, file=paste("C:/Users/jgove/OneDrive/Desktop/dPrimeTableExp1.csv", sep=""),row.names = F)
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


###MINUS NOVEL FOR SCATTERPLOTS
library(dplyr)
library(tidyr)
library(ggplot2)
library(Rmisc)


# ---- 0) Standardize labels (works whether your input is N/RS/RT or Novel/Restudy/Retrieval; nam/unn or Nameable/Abstract) ----
dprimeTable2 <- dprimeTable %>%
  mutate(
    nam_unn = as.character(nam_unn),
    RS_RT_N = as.character(RS_RT_N),
    
    nam_unn = recode(nam_unn, "nam" = "Nameable", "unn" = "Abstract", .default = nam_unn),
    nam_unn = factor(nam_unn, levels = c("Nameable","Abstract")),
    
    RS_RT_N = recode(RS_RT_N,
                     "N"="Novel","RS"="Restudy","RT"="Retrieval",
                     "ApRS"="Restudy","ApRT"="Retrieval",
                     .default = RS_RT_N),
    RS_RT_N = factor(RS_RT_N, levels = c("Novel","Restudy","Retrieval"))
  ) %>%
  filter(RS_RT_N %in% c("Novel","Restudy","Retrieval"))

# ---- 1) Wide form so we can compute differences vs Novel ----
wide_all <- dprimeTable2 %>%
  select(Subject, nam_unn, RS_RT_N, dprime, Hits, FAs) %>%
  pivot_wider(
    id_cols = c(Subject, nam_unn),
    names_from = RS_RT_N,
    values_from = c(dprime, Hits, FAs),
    values_fn = mean
  ) %>%
  mutate(
    dprime_RS_N = dprime_Restudy   - dprime_Novel,
    dprime_RT_N = dprime_Retrieval - dprime_Novel,
    Hits_RS_N   = Hits_Restudy     - Hits_Novel,
    Hits_RT_N   = Hits_Retrieval   - Hits_Novel,
    FAs_RS_N    = FAs_Restudy      - FAs_Novel,
    FAs_RT_N    = FAs_Retrieval    - FAs_Novel
  )

# ---- 2) Long forms (two “conditions”: Restudy-Novel vs Retrieval-Novel) ----
dprime_long <- wide_all %>%
  select(Subject, nam_unn, dprime_RS_N, dprime_RT_N) %>%
  pivot_longer(cols = c(dprime_RS_N, dprime_RT_N),
               names_to = "Contrast", values_to = "dprime") %>%
  mutate(
    Contrast = recode(Contrast,
                      dprime_RS_N = "Restudy - Novel",
                      dprime_RT_N = "Retrieval - Novel"),
    Contrast = factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))
  ) %>%
  filter(is.finite(dprime))

Hits_long <- wide_all %>%
  select(Subject, nam_unn, Hits_RS_N, Hits_RT_N) %>%
  pivot_longer(cols = c(Hits_RS_N, Hits_RT_N),
               names_to = "Contrast", values_to = "Hits") %>%
  mutate(
    Contrast = recode(Contrast,
                      Hits_RS_N = "Restudy - Novel",
                      Hits_RT_N = "Retrieval - Novel"),
    Contrast = factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))
  ) %>%
  filter(is.finite(Hits))

FAs_long <- wide_all %>%
  select(Subject, nam_unn, FAs_RS_N, FAs_RT_N) %>%
  pivot_longer(cols = c(FAs_RS_N, FAs_RT_N),
               names_to = "Contrast", values_to = "FAs") %>%
  mutate(
    Contrast = recode(Contrast,
                      FAs_RS_N = "Restudy - Novel",
                      FAs_RT_N = "Retrieval - Novel"),
    Contrast = factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))
  ) %>%
  filter(is.finite(FAs))

# ---- 3) Summaries for mean + SE ----
SE_dprime_long <- summarySE(dprime_long, measurevar = "dprime", groupvars = c("nam_unn", "Contrast"))
SE_HITS_long   <- summarySE(Hits_long,   measurevar = "Hits",   groupvars = c("nam_unn", "Contrast"))
SE_FA_long     <- summarySE(FAs_long,    measurevar = "FAs",    groupvars = c("nam_unn", "Contrast"))

# ---- 4) X positions + CONSTANT subject jitter (do separately for each long df) ----
set.seed(123)
spacing_factor <- 0.35
jitter_amount  <- 0.08

# ---- dprime plot data ----
dprime_plot <- dprime_long %>%
  mutate(
    nam_num  = as.numeric(factor(nam_unn, levels = c("Nameable","Abstract"))),
    cond_num = as.numeric(factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )
dprime_offsets <- dprime_plot %>%
  distinct(Subject, nam_unn) %>%
  mutate(subj_jit = jitter(rep(0, n()), amount = jitter_amount))
dprime_plot <- dprime_plot %>%
  left_join(dprime_offsets, by = c("Subject","nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

SE_dprime_plot <- SE_dprime_long %>%
  mutate(
    nam_num  = as.numeric(factor(nam_unn, levels = c("Nameable","Abstract"))),
    cond_num = as.numeric(factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

# ---- Hits plot data ----
Hits_plot <- Hits_long %>%
  mutate(
    nam_num  = as.numeric(factor(nam_unn, levels = c("Nameable","Abstract"))),
    cond_num = as.numeric(factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )
Hits_offsets <- Hits_plot %>%
  distinct(Subject, nam_unn) %>%
  mutate(subj_jit = jitter(rep(0, n()), amount = jitter_amount))
Hits_plot <- Hits_plot %>%
  left_join(Hits_offsets, by = c("Subject","nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

SE_Hits_plot <- SE_HITS_long %>%
  mutate(
    nam_num  = as.numeric(factor(nam_unn, levels = c("Nameable","Abstract"))),
    cond_num = as.numeric(factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

# ---- FAs plot data ----
FAs_plot <- FAs_long %>%
  mutate(
    nam_num  = as.numeric(factor(nam_unn, levels = c("Nameable","Abstract"))),
    cond_num = as.numeric(factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )
FAs_offsets <- FAs_plot %>%
  distinct(Subject, nam_unn) %>%
  mutate(subj_jit = jitter(rep(0, n()), amount = jitter_amount))
FAs_plot <- FAs_plot %>%
  left_join(FAs_offsets, by = c("Subject","nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

SE_FAs_plot <- SE_FA_long %>%
  mutate(
    nam_num  = as.numeric(factor(nam_unn, levels = c("Nameable","Abstract"))),
    cond_num = as.numeric(factor(Contrast, levels = c("Restudy - Novel","Retrieval - Novel"))),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

# ---- 5) Colors ----
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
contrast_cols <- c("Restudy - Novel"   = cbbPalette[2],
                   "Retrieval - Novel" = cbbPalette[8])

# ---- 6) Plots ----

# d' (minus Novel)
p_diff_dprime <- ggplot() +
  geom_point(data = dprime_plot, aes(x = x_plot, y = dprime, color = Contrast),
             size = 3, alpha = 0.7) +
  geom_line(data = dprime_plot %>% arrange(nam_unn, Subject, Contrast),
            aes(x = x_plot, y = dprime, group = interaction(Subject, nam_unn)),
            alpha = 0.35, color = "grey") +
  geom_point(data = SE_dprime_plot, aes(x = x_mean, y = dprime),
             inherit.aes = FALSE, shape = 18, size = 4, color = "black") +
  geom_errorbar(data = SE_dprime_plot, aes(x = x_mean, ymin = dprime - se, ymax = dprime + se),
                inherit.aes = FALSE, width = 0.12, color = "black") +
  scale_x_continuous(breaks = c(1,2), labels = c("Nameable","Abstract")) +
  scale_color_manual(name = "Contrast", values = contrast_cols) +
  ylab("d' (Condition - Novel)") +
  ggtitle("Memory Benefit Relative to Novel") +
  theme_bw()
print(p_diff_dprime)

# Hits (minus Novel)
p_diff_hits <- ggplot() +
  geom_point(data = Hits_plot, aes(x = x_plot, y = Hits, color = Contrast),
             size = 3, alpha = 0.7) +
  geom_line(data = Hits_plot %>% arrange(nam_unn, Subject, Contrast),
            aes(x = x_plot, y = Hits, group = interaction(Subject, nam_unn)),
            alpha = 0.35, color = "grey") +
  geom_point(data = SE_Hits_plot, aes(x = x_mean, y = Hits),
             inherit.aes = FALSE, shape = 18, size = 4, color = "black") +
  geom_errorbar(data = SE_Hits_plot, aes(x = x_mean, ymin = Hits - se, ymax = Hits + se),
                inherit.aes = FALSE, width = 0.12, color = "black") +
  scale_x_continuous(breaks = c(1,2), labels = c("Nameable","Abstract")) +
  scale_color_manual(name = "Contrast", values = contrast_cols) +
  ylab("Hits (Condition - Novel)") +
  ggtitle("Hits Change Relative to Novel") +
  theme_bw()
print(p_diff_hits)

# FAs (minus Novel) with auto y-limits to avoid clipping
fa_top <- max(FAs_plot$FAs, SE_FAs_plot$FAs + SE_FAs_plot$se, na.rm = TRUE) * 1.05
fa_bot <- min(0, min(FAs_plot$FAs, na.rm = TRUE))

p_diff_fas <- ggplot() +
  geom_point(data = FAs_plot, aes(x = x_plot, y = FAs, color = Contrast),
             size = 3, alpha = 0.7) +
  geom_line(data = FAs_plot %>% arrange(nam_unn, Subject, Contrast),
            aes(x = x_plot, y = FAs, group = interaction(Subject, nam_unn)),
            alpha = 0.35, color = "grey") +
  geom_point(data = SE_FAs_plot, aes(x = x_mean, y = FAs),
             inherit.aes = FALSE, shape = 18, size = 4, color = "black") +
  geom_errorbar(data = SE_FAs_plot, aes(x = x_mean, ymin = FAs - se, ymax = FAs + se),
                inherit.aes = FALSE, width = 0.12, color = "black") +
  scale_x_continuous(breaks = c(1,2), labels = c("Nameable","Abstract")) +
  scale_color_manual(name = "Contrast", values = contrast_cols) +
  coord_cartesian(ylim = c(fa_bot, fa_top)) +
  ylab("FAs (Condition - Novel)") +
  ggtitle("False Alarm Change Relative to Novel") +
  theme_bw()
print(p_diff_fas)



Recall.anovaN <- aov(dprime_long$dprime~dprime_long$nam_unn*dprime_long$RS_RT_N)
summary(Recall.anovaN)

RecallH.anovaN <- aov(dprime_longH$Hits~dprime_longH$nam_unn*dprime_longH$RS_RT_N)
summary(RecallH.anovaN)

RecallF.anovaN <- aov(dprime_longFA$FAs~dprime_longFA$nam_unn*dprime_longFA$RS_RT_N)
summary(RecallF.anovaN)

##ANOVAS REGULAR
Recall.anova <- aov(dprimeTable$dprime~dprimeTable$nam_unn*dprimeTable$RS_RT_N)
summary(Recall.anova)

RecallH.anova <- aov(dprimeTable$Hits~dprimeTable$nam_unn*dprimeTable$RS_RT_N)
summary(RecallH.anova)

RecallF.anova <- aov(dprimeTable$FAs~dprimeTable$nam_unn*dprimeTable$RS_RT_N)
summary(RecallF.anova)



## t-tests filtering
Nam_RS = filter(dprime_long, nam_unn == "nam" & RS_RT_N == "RS_N")
Nam_RT = filter(dprime_long, nam_unn == "nam" & RS_RT_N == "RT_N")
Unn_RS = filter(dprime_long, nam_unn == "unn" & RS_RT_N == "RS_N")
Unn_RT = filter(dprime_long, nam_unn == "unn" & RS_RT_N == "RT_N")

Nam_RSH = filter(dprime_longH, nam_unn == "nam" & RS_RT_N == "RS_N")
Nam_RTH = filter(dprime_longH, nam_unn == "nam" & RS_RT_N == "RT_N")
Unn_RSH = filter(dprime_longH, nam_unn == "unn" & RS_RT_N == "RS_N")
Unn_RTH = filter(dprime_longH, nam_unn == "unn" & RS_RT_N == "RT_N")

Nam_RSF = filter(dprime_longFA, nam_unn == "nam" & RS_RT_N == "RS_N")
Nam_RTF = filter(dprime_longFA, nam_unn == "nam" & RS_RT_N == "RT_N")
Unn_RSF = filter(dprime_longFA, nam_unn == "unn" & RS_RT_N == "RS_N")
Unn_RTF = filter(dprime_longFA, nam_unn == "unn" & RS_RT_N == "RT_N")

##d'

t.test(Nam_RS$dprime, Nam_RT$dprime, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RS$dprime, Unn_RT$dprime, alternative = "two.sided", mu = 0, paired = FALSE)


##Hits

t.test(Nam_RSH$Hits, Nam_RTH$Hits, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RSH$Hits, Unn_RTH$Hits, alternative = "two.sided", mu = 0, paired = FALSE)


##FAs

t.test(Nam_RSF$FAs, Nam_RTF$FAs, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RSF$FAs, Unn_RTF$FAs, alternative = "two.sided", mu = 0, paired = FALSE)


##SCATTER PLOTS

# Calculate SEM using summarySE
data_summary <- summarySE(dprime_long, measurevar = "dprime", groupvars = c("nam_unn", "RS_RT_N"))
#data_summary$se_M <- SE_dprime_recallCM$se_M

# Create a jittered Group variable for consistent positioning
set.seed(123)  # For reproducibility
spacing_factor <- 0.4  # Increase this factor to create more spacing
dprime_long <- dprime_long %>%
  mutate(nam_unn_jit = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor + jitter(rep(0, n()), amount = 0.1))

# Create the scatter plot with individual points, means, and error bars
dodge <- position_dodge(width = 0.75)

plot3 <- ggplot(dprime_long, aes(x = nam_unn_jit, y = dprime, color = RS_RT_N)) +
  geom_jitter(size = 3, alpha = 0.7) +
  stat_summary(aes(x = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, y = dprime, color = RS_RT_N), 
               fun = mean, geom = "point", shape = 18, size = 4, position = dodge, color = "black") +
  geom_errorbar(data = data_summary, aes(x = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, ymin = dprime - se, ymax = dprime + se, color = RS_RT_N),
                width = 0.2, position = dodge, color = "black") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Nam", "Unn")) +
  scale_color_manual(name = "Image Type", values = c(cbbPalette[5], cbbPalette[8]), labels = c("Restudy", "Retrieval")) +
  xlab("Stimuli Type") +  # Set the x-axis label explicitly
  ylab("Visual Recall (d')") +
  ggtitle("Recall Performance") +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) +
  coord_cartesian(xlim = c(0.5, 2.5), ylim = c(-3, 3)) 

# Overlay the line plot on the scatter plot
final_plot2 <- plot3 +
  geom_line(data = dprime_long, aes(x = nam_unn_jit, y = dprime, group = interaction(Subject, nam_unn)), 
            alpha = 0.5, color = "grey")

print(final_plot2)

# Calculate SEM for Hits using summarySE
data_summary_hits <- summarySE(dprime_longH, measurevar = "Hits", groupvars = c("nam_unn", "RS_RT_N"))

# Create a jittered Group variable for consistent positioning
set.seed(123)  # For reproducibility
spacing_factor <- 0.4  # Increase this factor to create more spacing
dprime_longH <- dprime_longH %>%
  mutate(nam_unn_jit = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor + jitter(rep(0, n()), amount = 0.1))

# Create the scatter plot with individual points, means, and error bars for Hits
dodge <- position_dodge(width = 0.75)

plot_hits <- ggplot(dprime_longH, aes(x = nam_unn_jit, y = Hits, color = RS_RT_N)) +
  geom_jitter(size = 3, alpha = 0.7) +
  stat_summary(aes(x = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, y = Hits, color = RS_RT_N), 
               fun = mean, geom = "point", shape = 18, size = 4, position = dodge, color = "black") +
  geom_errorbar(data = data_summary_hits, aes(x = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, ymin = Hits - se, ymax = Hits + se, color = RS_RT_N),
                width = 0.2, position = dodge, color = "black") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Nam", "Unn")) +
  scale_color_manual(name = "Image Type", values = c(cbbPalette[5], cbbPalette[8]), labels = c("Restudy", "Retrieval")) +
  xlab("Stimuli Type") +
  ylab("Hits") +
  ggtitle("Hits Performance") +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) +
  coord_cartesian(xlim = c(0.5, 2.5), ylim = c(-.5, .75)) 

# Overlay the line plot on the scatter plot
final_plot_hits <- plot_hits +
  geom_line(data = dprime_longH, aes(x = nam_unn_jit, y = Hits, group = interaction(Subject, nam_unn)), 
            alpha = 0.5, color = "grey")

print(final_plot_hits)


# Calculate SEM for FAs using summarySE
data_summary_fas <- summarySE(dprime_longFA, measurevar = "FAs", groupvars = c("nam_unn", "RS_RT_N"))

# Create a jittered Group variable for consistent positioning
set.seed(123)  # For reproducibility
spacing_factor <- 0.4  # Increase this factor to create more spacing
dprime_longFA <- dprime_longFA %>%
  mutate(nam_unn_jit = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor + jitter(rep(0, n()), amount = 0.1))

# Create the scatter plot with individual points, means, and error bars for FAs
dodge <- position_dodge(width = 0.75)

plot_fas <- ggplot(dprime_longFA, aes(x = nam_unn_jit, y = FAs, color = RS_RT_N)) +
  geom_jitter(size = 3, alpha = 0.7) +
  stat_summary(aes(x = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, y = FAs, color = RS_RT_N), 
               fun = mean, geom = "point", shape = 18, size = 4, position = dodge, color = "black") +
  geom_errorbar(data = data_summary_fas, aes(x = as.numeric(nam_unn) + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, ymin = FAs - se, ymax = FAs + se, color = RS_RT_N),
                width = 0.2, position = dodge, color = "black") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Nam", "Unn")) +
  scale_color_manual(name = "Image Type", values = c(cbbPalette[5], cbbPalette[8]), labels = c("Restudy", "Retrieval")) +
  xlab("Stimuli Type") +
  ylab("False Alarms (FAs)") +
  ggtitle("False Alarm Performance") +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) +
  coord_cartesian(xlim = c(0.5, 2.5), ylim = c(-.5, .75)) 

# Overlay the line plot on the scatter plot
final_plot_fas <- plot_fas +
  geom_line(data = dprime_longFA, aes(x = nam_unn_jit, y = FAs, group = interaction(Subject, nam_unn)), 
            alpha = 0.5, color = "grey")

print(final_plot_fas)


## SCATTER PLOTS BEFORE SUBTRACTION

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
  scale_x_continuous(breaks = c(2, 4), labels = c("Nam", "Unn")) +  # Adjusted breaks to match new spacing
  scale_color_manual(name = "Image Type", values = c(cbbPalette[6], cbbPalette[5], cbbPalette[8]), labels = c("Novel", "Restudy", "Retrieval")) +
  xlab("Stimuli Type") +
  ylab("Visual Recall (d')") +
  ggtitle("Recall Performance") +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) +
  coord_cartesian(xlim = c(0.75, 5), ylim = c(-1, 3.7))  # Adjusted xlim to accommodate increased spacing

# Overlay the line plot on the scatter plot
final_plotALL3 <- plotALL3 +
  geom_line(data = dprimeTable, aes(x = nam_unn_jit, y = dprime, group = interaction(Subject, nam_unn)), 
            alpha = 0.5, color = "grey")

print(final_plotALL3)

# Calculate SEM for Hits using summarySE
data_summary_hits_before <- summarySE(dprimeTable, measurevar = "Hits", groupvars = c("nam_unn", "RS_RT_N"))

# Create a jittered Group variable for consistent positioning with increased spacing
set.seed(123)  # For reproducibility
spacing_factor <- 0.6  # Increased spacing factor to create more separation
dprimeTable <- dprimeTable %>%
  mutate(nam_unn_jit = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor + jitter(rep(0, n()), amount = 0.1))

# Create the scatter plot with individual points, means, and error bars for Hits
dodge <- position_dodge(width = 0.75)

plot_hits_before <- ggplot(dprimeTable, aes(x = nam_unn_jit, y = Hits, color = RS_RT_N)) +
  geom_jitter(size = 3, alpha = 0.7) +
  stat_summary(aes(x = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, y = Hits, color = RS_RT_N), 
               fun = mean, geom = "point", shape = 18, size = 4, position = dodge, color = "black") +
  geom_errorbar(data = data_summary_hits_before, aes(x = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, ymin = Hits - se, ymax = Hits + se, color = RS_RT_N),
                width = 0.2, position = dodge, color = "black") +
  scale_x_continuous(breaks = c(2, 4), labels = c("Nam", "Unn")) +  # Adjusted breaks to match new spacing
  scale_color_manual(name = "Image Type", values = c(cbbPalette[6], cbbPalette[5], cbbPalette[8]), labels = c("Novel", "Restudy", "Retrieval")) +
  xlab("Stimuli Type") +
  ylab("Hits") +
  ggtitle("Hits Performance Before Subtraction") +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) +
  coord_cartesian(xlim = c(0.75, 5), ylim = c(0, 1))  # Adjusted xlim to accommodate increased spacing

# Overlay the line plot on the scatter plot
final_plot_hits_before <- plot_hits_before +
  geom_line(data = dprimeTable, aes(x = nam_unn_jit, y = Hits, group = interaction(Subject, nam_unn)), 
            alpha = 0.5, color = "grey")

print(final_plot_hits_before)


# Calculate SEM for FAs using summarySE
data_summary_fas_before <- summarySE(dprimeTable, measurevar = "FAs", groupvars = c("nam_unn", "RS_RT_N"))

# Create a jittered Group variable for consistent positioning with increased spacing
set.seed(123)  # For reproducibility
spacing_factor <- 0.6  # Increased spacing factor to create more separation
dprimeTable <- dprimeTable %>%
  mutate(nam_unn_jit = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor + jitter(rep(0, n()), amount = 0.1))

# Create the scatter plot with individual points, means, and error bars for FAs
dodge <- position_dodge(width = 0.75)

plot_fas_before <- ggplot(dprimeTable, aes(x = nam_unn_jit, y = FAs, color = RS_RT_N)) +
  geom_jitter(size = 3, alpha = 0.7) +
  stat_summary(aes(x = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, y = FAs, color = RS_RT_N), 
               fun = mean, geom = "point", shape = 18, size = 4, position = dodge, color = "black") +
  geom_errorbar(data = data_summary_fas_before, aes(x = as.numeric(nam_unn) * 2 + (as.numeric(factor(RS_RT_N)) - 1.5) * spacing_factor, ymin = FAs - se, ymax = FAs + se, color = RS_RT_N),
                width = 0.2, position = dodge, color = "black") +
  scale_x_continuous(breaks = c(2, 4), labels = c("Nam", "Unn")) +  # Adjusted breaks to match new spacing
  scale_color_manual(name = "Image Type", values = c(cbbPalette[6], cbbPalette[5], cbbPalette[8]), labels = c("Novel", "Restudy", "Retrieval")) +
  xlab("Stimuli Type") +
  ylab("False Alarms (FAs)") +
  ggtitle("False Alarm Performance Before Subtraction") +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) +
  coord_cartesian(xlim = c(0.75, 5), ylim = c(0, 1))  # Adjusted xlim to accommodate increased spacing

# Overlay the line plot on the scatter plot
final_plot_fas_before <- plot_fas_before +
  geom_line(data = dprimeTable, aes(x = nam_unn_jit, y = FAs, group = interaction(Subject, nam_unn)), 
            alpha = 0.5, color = "grey")

print(final_plot_fas_before)
