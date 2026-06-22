install.packages("Rmisc")
library(Rmisc) 
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("rstatix")
library(rstatix)
library(dplyr)

SONAsubID <- c(201:230,232:241,243:262)
allSubID <- SONAsubID

ratesDat <- data.frame(matrix(NA, length(allSubID) * 8, 6))
colnames(ratesDat) <- c('Subject', 'nam_unn', 'RS_RT_N', 'N', 'nRespRight', 'PropRight')

dprimeTable <- data.frame(matrix(NA, length(allSubID) * 4, 7))
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
        
        ratesDat$Subject[subCt * 8 + condCt - 8] <- allSubID[s]
        ratesDat$nam_unn[subCt * 8 + condCt - 8] <- nam_unn_levels[oj]
        ratesDat$RS_RT_N[subCt * 8 + condCt - 8] <- RS_RT_N_lvls[i]
        ratesDat$Old[subCt * 8 + condCt - 8] <- old_lvls[k]
        
        filt_dat <- filter(filter(dat, nam_unn == nam_unn_levels[oj]), RS_RT_N == RS_RT_N_lvls[i], old == old_lvls[k])
        
        ratesDat$N[subCt * 8 + condCt - 8] <- nrow(filt_dat)
        ratesDat$nRespRight[subCt * 8 + condCt - 8] <- sum(filt_dat$key_resp_2.keys == "right")
        
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

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  # black, gold, light blue, green, yellow, blue, dark orange, pink

library(ggplot2)  # Make sure to load ggplot2 package before running this part


# =========================================================
# EXP 2 CLEAN PLOTS
#   1) dprime
#   2) Hits
#   3) False Alarms
# =========================================================

library(dplyr)
library(ggplot2)
library(Rmisc)
library(grid)

# =========================================================
# CLEAN + STANDARDIZE DATA
# =========================================================

dprimeTable2 <- dprimeTable %>%
  mutate(
    Subject = factor(Subject),
    
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
    
    RS_RT_N = trimws(as.character(RS_RT_N)),
    RS_RT_N = recode(
      RS_RT_N,
      "RS"        = "Restudy",
      "RT"        = "Retrieval",
      "ApRS"      = "Restudy",
      "ApRT"      = "Retrieval",
      "Restudy"   = "Restudy",
      "Retrieval" = "Retrieval",
      .default = RS_RT_N
    )
  ) %>%
  filter(
    nam_unn %in% c("Meaningful", "Abstract"),
    RS_RT_N %in% c("Restudy", "Retrieval")
  ) %>%
  mutate(
    nam_unn = factor(nam_unn, levels = c("Meaningful", "Abstract")),
    RS_RT_N = factor(RS_RT_N, levels = c("Restudy", "Retrieval"))
  )

# =========================================================
# SUMMARY TABLES
# =========================================================

SE_dprime <- summarySE(
  dprimeTable2,
  measurevar = "dprime",
  groupvars = c("nam_unn", "RS_RT_N")
)

SE_hits <- summarySE(
  dprimeTable2,
  measurevar = "Hits",
  groupvars = c("nam_unn", "RS_RT_N")
)

SE_fa <- summarySE(
  dprimeTable2,
  measurevar = "FAs",
  groupvars = c("nam_unn", "RS_RT_N")
)

# =========================================================
# X POSITIONS
# =========================================================

set.seed(123)

meaningful_center <- 1.00
abstract_center   <- 1.70

spacing_factor <- 0.2
jitter_amount  <- 0.06

plot_dat <- dprimeTable2 %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Meaningful",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

# =========================================================
# CONSTANT SUBJECT JITTER
# =========================================================

subj_offsets <- plot_dat %>%
  distinct(Subject, nam_unn) %>%
  mutate(
    subj_jit = jitter(rep(0, n()), amount = jitter_amount)
  )

plot_dat <- plot_dat %>%
  left_join(subj_offsets, by = c("Subject", "nam_unn")) %>%
  mutate(
    x_plot = x_mean + subj_jit
  )

# =========================================================
# SUMMARY TABLE X POSITIONS
# =========================================================

SE_dprime_plot <- SE_dprime %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Meaningful",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

SE_hits_plot <- SE_hits %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Meaningful",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

SE_fa_plot <- SE_fa %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Meaningful",
      meaningful_center,
      abstract_center
    ),
    cond_num = as.numeric(RS_RT_N),
    x_mean = stim_center + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

# =========================================================
# COLORS
# =========================================================

cond_cols <- c(
  "Restudy"   = "#E69F00",
  "Retrieval" = "#CC79A7"
)

# =========================================================
# SHARED THEME
# =========================================================

big_theme <- theme_bw() +
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

# =========================================================
# PLOT 1: DPRIME
# =========================================================

p_dprime <- ggplot() +
  
  geom_point(
    data = plot_dat,
    aes(x = x_plot, y = dprime, color = RS_RT_N),
    size = 3,
    alpha = 0.7
  ) +
  
  geom_line(
    data = plot_dat %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = dprime, group = interaction(Subject, nam_unn)),
    alpha = 0.35,
    color = "grey"
  ) +
  
  geom_point(
    data = SE_dprime_plot,
    aes(x = x_mean, y = dprime),
    shape = 18,
    size = 5,
    color = "black"
  ) +
  
  geom_errorbar(
    data = SE_dprime_plot,
    aes(x = x_mean, ymin = dprime - se, ymax = dprime + se),
    width = 0.10,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(meaningful_center, abstract_center),
    labels = c("Meaningful", "Abstract")
  ) +
  
  scale_color_manual(
    name = NULL,
    values = cond_cols,
    breaks = c("Restudy", "Retrieval")
  ) +
  
  ylab("d'") +
  xlab("Stimulus Type") +
  ggtitle("Memory Performance Exp. 2") +
  
  big_theme

print(p_dprime)

# =========================================================
# PLOT 2: HITS
# =========================================================

p_hits <- ggplot() +
  
  geom_point(
    data = plot_dat,
    aes(x = x_plot, y = Hits, color = RS_RT_N),
    size = 3,
    alpha = 0.7
  ) +
  
  geom_line(
    data = plot_dat %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = Hits, group = interaction(Subject, nam_unn)),
    alpha = 0.35,
    color = "grey"
  ) +
  
  geom_point(
    data = SE_hits_plot,
    aes(x = x_mean, y = Hits),
    shape = 18,
    size = 5,
    color = "black"
  ) +
  
  geom_errorbar(
    data = SE_hits_plot,
    aes(x = x_mean, ymin = Hits - se, ymax = Hits + se),
    width = 0.10,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(meaningful_center, abstract_center),
    labels = c("Meaningful", "Abstract")
  ) +
  
  scale_color_manual(
    name = NULL,
    values = cond_cols,
    breaks = c("Restudy", "Retrieval")
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  ylab("HITS") +
  xlab("Stimulus Type") +
  ggtitle("HITS Exp. 2") +
  
  big_theme

print(p_hits)

# =========================================================
# PLOT 3: FALSE ALARMS
# =========================================================

p_fa <- ggplot() +
  
  geom_point(
    data = plot_dat,
    aes(x = x_plot, y = FAs, color = RS_RT_N),
    size = 3,
    alpha = 0.7
  ) +
  
  geom_line(
    data = plot_dat %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = FAs, group = interaction(Subject, nam_unn)),
    alpha = 0.35,
    color = "grey"
  ) +
  
  geom_point(
    data = SE_fa_plot,
    aes(x = x_mean, y = FAs),
    shape = 18,
    size = 5,
    color = "black"
  ) +
  
  geom_errorbar(
    data = SE_fa_plot,
    aes(x = x_mean, ymin = FAs - se, ymax = FAs + se),
    width = 0.10,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(meaningful_center, abstract_center),
    labels = c("Meaningful", "Abstract")
  ) +
  
  scale_color_manual(
    name = NULL,
    values = cond_cols,
    breaks = c("Restudy", "Retrieval")
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  ylab("FAs") +
  xlab("Stimulus Type") +
  ggtitle("FAs Exp. 2") +
  
  big_theme

print(p_fa)





# ===== Scatter-style versions of your 3 plots (d', HITS, FAs) for the 200s sample =====
# - Individual subject points
# - Mean (black diamond) + SE bars (black)
# - Subject-connecting lines that align perfectly (constant subject jitter)
# - Works whether your labels are nam/unn + RS/RT or already pretty

library(dplyr)
library(ggplot2)
library(Rmisc)

# ---- 0) Standardize labels + factor order ----
dprimeTable2 <- dprimeTable %>%
  mutate(
    nam_unn = as.character(nam_unn),
    RS_RT_N = as.character(RS_RT_N),
    
    # nam/unn -> Nameable/Abstract if needed
    nam_unn = recode(nam_unn, "nam" = "Nameable", "unn" = "Abstract", .default = nam_unn),
    nam_unn = factor(nam_unn, levels = c("Nameable","Abstract")),
    
    # RS/RT -> Restudy/Retrieval if needed
    RS_RT_N = recode(RS_RT_N, "RS" = "Restudy", "RT" = "Retrieval", .default = RS_RT_N),
    RS_RT_N = factor(RS_RT_N, levels = c("Restudy","Retrieval"))
  ) %>%
  filter(!is.na(nam_unn), !is.na(RS_RT_N))

# ---- 1) Summary tables (means + SE) ----
SE_dprime2 <- summarySE(dprimeTable2, measurevar = "dprime", groupvars = c("nam_unn","RS_RT_N"))
SE_HITS2   <- summarySE(dprimeTable2, measurevar = "Hits",   groupvars = c("nam_unn","RS_RT_N"))
SE_FA2     <- summarySE(dprimeTable2, measurevar = "FAs",    groupvars = c("nam_unn","RS_RT_N"))


library(dplyr)
library(ggplot2)
library(Rmisc)

# 1) Inspect what's really in the columns BEFORE recoding
cat("nam_unn values:\n"); print(sort(unique(as.character(dprimeTable$nam_unn))))
cat("RS_RT_N values:\n"); print(sort(unique(as.character(dprimeTable$RS_RT_N))))
cat("dprime finite count:\n"); print(sum(is.finite(dprimeTable$dprime)))

# 2) Robust recode (handles both code-style and label-style)
dprimeTable2 <- dprimeTable %>%
  mutate(
    nam_raw  = trimws(as.character(nam_unn)),
    cond_raw = trimws(as.character(RS_RT_N)),
    
    nam_unn = recode(nam_raw,
                     "nam"="Nameable", "unn"="Abstract",
                     "Nameable"="Meaningful", "Abstract"="Abstract",
                     .default = nam_raw),
    
    RS_RT_N = recode(cond_raw,
                     "RS"="Restudy", "RT"="Retrieval",
                     "Restudy"="Restudy", "Retrieval"="Retrieval",
                     .default = cond_raw)
  )

# 3) Show anything that DIDN'T map cleanly
cat("Unmapped nam_unn values (will cause NAs later):\n")
print(dprimeTable2 %>% filter(!(nam_unn %in% c("Meaningful","Abstract"))) %>% distinct(nam_unn))

cat("Unmapped RS_RT_N values (will cause NAs later):\n")
print(dprimeTable2 %>% filter(!(RS_RT_N %in% c("Restudy","Retrieval"))) %>% distinct(RS_RT_N))

# 4) Keep only the two categories + two conditions we expect, and drop non-finite d'
dprimeTable2 <- dprimeTable2 %>%
  filter(nam_unn %in% c("Meaningful","Abstract"),
         RS_RT_N %in% c("Restudy","Retrieval"),
         is.finite(dprime)) %>%
  mutate(
    nam_unn = factor(nam_unn, levels = c("Meaningful","Abstract")),
    RS_RT_N = factor(RS_RT_N, levels = c("Restudy","Retrieval"))
  )

cat("Rows after cleaning:\n"); print(nrow(dprimeTable2))
stopifnot(nrow(dprimeTable2) > 0)

# 5) Summary stats
SE_dprime2 <- summarySE(dprimeTable2, measurevar="dprime", groupvars=c("nam_unn","RS_RT_N"))

# 6) X positions + constant subject jitter
set.seed(123)
spacing_factor <- 0.35
jitter_amount  <- 0.08

base_x <- dprimeTable2 %>%
  mutate(
    nam_num  = as.numeric(nam_unn),
    cond_num = as.numeric(RS_RT_N),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

subj_offsets <- base_x %>%
  distinct(Subject, nam_unn) %>%
  mutate(subj_jit = jitter(rep(0, nrow(.)), amount = jitter_amount))

plot_x <- base_x %>%
  left_join(subj_offsets, by = c("Subject","nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

SE_plot <- SE_dprime2 %>%
  mutate(
    nam_unn = factor(nam_unn, levels = c("Meaningful","Abstract")),
    RS_RT_N = factor(RS_RT_N, levels = c("Restudy","Retrieval")),
    nam_num  = as.numeric(nam_unn),
    cond_num = as.numeric(RS_RT_N),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

# 7) Plot (should NOT be empty)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cond_cols  <- c("Restudy" = cbbPalette[2], "Retrieval" = cbbPalette[8])

p_dprime <- ggplot() +
  geom_point(
    data = plot_x,
    aes(x = x_plot, y = dprime, color = RS_RT_N),
    size = 3, alpha = 0.7
  ) +
  geom_line(
    data = plot_x %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = dprime, group = interaction(Subject, nam_unn)),
    alpha = 0.35, color = "grey"
  ) +
  geom_point(
    data = SE_plot,
    aes(x = x_mean, y = dprime),
    inherit.aes = FALSE,
    shape = 18, size = 4, color = "black"
  ) +
  geom_errorbar(
    data = SE_plot,
    aes(x = x_mean, ymin = dprime - se, ymax = dprime + se),
    inherit.aes = FALSE,
    width = 0.12, color = "black"
  ) +
  scale_x_continuous(breaks = c(1,2), labels = c("Meaningful","Abstract")) +
  scale_color_manual(name = "Condition", values = cond_cols) +
  ylab("Visual Recall (d')") +
  xlab("Stimulus Type") +
  ggtitle("Recall Performance") +
  theme_bw() +
  theme(
    plot.title   = element_text(size = 30),
    axis.text    = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.text  = element_text(size = 30),
    legend.title = element_text(size = 30),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_dprime)
print(SE_dprime)
print(SE_HITS)
print(SE_FA)
# =========================
# MISSING RESPONSE SUMMARY: EXP 2
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
  
  # count only actual experimental trials
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
  ggtitle("Exp. 2 Missing Responses by Participant") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18)
  )






library(dplyr)
library(rstatix)
library(tidyr)

# =========================================================
# ANOVA for dprime (Version #2: 2 x 2)
# =========================================================
anova_dprime_dat <- dprimeTable %>%
  mutate(
    Subject = factor(Subject),
    
    nam_unn = recode(as.character(nam_unn),
                     "nam" = "Meaningful",
                     "unn" = "Abstract",
                     .default = as.character(nam_unn)),
    
    RS_RT_N = recode(as.character(RS_RT_N),
                     "ApRS" = "Restudy",
                     "ApRT" = "Retrieval",
                     "RS"   = "Restudy",
                     "RT"   = "Retrieval",
                     .default = as.character(RS_RT_N)),
    
    StimType  = factor(nam_unn, levels = c("Meaningful", "Abstract")),
    StudyCond = factor(RS_RT_N, levels = c("Restudy", "Retrieval"))
  ) %>%
  select(Subject, StimType, StudyCond, dprime) %>%
  filter(is.finite(dprime), !is.na(StimType), !is.na(StudyCond))

anova_dprime_res <- anova_test(
  data = anova_dprime_dat,
  dv = dprime,
  wid = Subject,
  within = c(StimType, StudyCond)
)
anova_dprime_res
anova_dprime_tab <- get_anova_table(anova_dprime_res)
print(anova_dprime_tab, width = Inf)

# =========================================================
# Post-hoc paired t-tests for dprime + effect sizes
# =========================================================
posthoc_dprime <- anova_dprime_dat %>%
  group_by(StimType) %>%
  pairwise_t_test(
    dprime ~ StudyCond,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE
  )

eff_posthoc_dprime <- anova_dprime_dat %>%
  group_by(StimType) %>%
  cohens_d(
    dprime ~ StudyCond,
    paired = TRUE
  )

posthoc_dprime_full <- posthoc_dprime %>%
  left_join(
    eff_posthoc_dprime %>%
      select(StimType, group1, group2, effsize, magnitude),
    by = c("StimType", "group1", "group2")
  )

print(posthoc_dprime_full, width = Inf)


# =========================================================
# ANOVA for Hits (Version #2: 2 x 2)
# =========================================================
anova_hits_dat <- dprimeTable %>%
  mutate(
    Subject = factor(Subject),
    
    nam_unn = recode(as.character(nam_unn),
                     "nam" = "Meaningful",
                     "unn" = "Abstract",
                     .default = as.character(nam_unn)),
    
    RS_RT_N = recode(as.character(RS_RT_N),
                     "ApRS" = "Restudy",
                     "ApRT" = "Retrieval",
                     "RS"   = "Restudy",
                     "RT"   = "Retrieval",
                     .default = as.character(RS_RT_N)),
    
    StimType  = factor(nam_unn, levels = c("Meaningful", "Abstract")),
    StudyCond = factor(RS_RT_N, levels = c("Restudy", "Retrieval"))
  ) %>%
  select(Subject, StimType, StudyCond, Hits) %>%
  filter(is.finite(Hits), !is.na(StimType), !is.na(StudyCond))

anova_hits_res <- anova_test(
  data = anova_hits_dat,
  dv = Hits,
  wid = Subject,
  within = c(StimType, StudyCond)
)

print(anova_hits_res)
anova_hits_tab <- get_anova_table(anova_hits_res)
print(anova_hits_tab, width = Inf)

# =========================================================
# Post-hoc paired t-tests for Hits + effect sizes
# =========================================================
posthoc_hits <- anova_hits_dat %>%
  group_by(StimType) %>%
  pairwise_t_test(
    Hits ~ StudyCond,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE
  )

eff_posthoc_hits <- anova_hits_dat %>%
  group_by(StimType) %>%
  cohens_d(
    Hits ~ StudyCond,
    paired = TRUE
  )

posthoc_hits_full <- posthoc_hits %>%
  left_join(
    eff_posthoc_hits %>%
      select(StimType, group1, group2, effsize, magnitude),
    by = c("StimType", "group1", "group2")
  )

print(posthoc_hits_full, width = Inf)


# =========================================================
# ANOVA for FAs (Version #2: 2 x 2)
# =========================================================
anova_fa_dat <- dprimeTable %>%
  mutate(
    Subject = factor(Subject),
    
    nam_unn = recode(as.character(nam_unn),
                     "nam" = "Meaningful",
                     "unn" = "Abstract",
                     .default = as.character(nam_unn)),
    
    RS_RT_N = recode(as.character(RS_RT_N),
                     "ApRS" = "Restudy",
                     "ApRT" = "Retrieval",
                     "RS"   = "Restudy",
                     "RT"   = "Retrieval",
                     .default = as.character(RS_RT_N)),
    
    StimType  = factor(nam_unn, levels = c("Meaningful", "Abstract")),
    StudyCond = factor(RS_RT_N, levels = c("Restudy", "Retrieval"))
  ) %>%
  select(Subject, StimType, StudyCond, FAs) %>%
  filter(is.finite(FAs), !is.na(StimType), !is.na(StudyCond))

anova_fa_res <- anova_test(
  data = anova_fa_dat,
  dv = FAs,
  wid = Subject,
  within = c(StimType, StudyCond)
)

print(anova_fa_res)
anova_fa_tab <- get_anova_table(anova_fa_res)
print(anova_fa_tab, width = Inf)

# =========================================================
# Post-hoc paired t-tests for FAs + effect sizes
# =========================================================
posthoc_fa <- anova_fa_dat %>%
  group_by(StimType) %>%
  pairwise_t_test(
    FAs ~ StudyCond,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE
  )

eff_posthoc_fa <- anova_fa_dat %>%
  group_by(StimType) %>%
  cohens_d(
    FAs ~ StudyCond,
    paired = TRUE
  )

posthoc_fa_full <- posthoc_fa %>%
  left_join(
    eff_posthoc_fa %>%
      select(StimType, group1, group2, effsize, magnitude),
    by = c("StimType", "group1", "group2")
  )

print(posthoc_fa_full, width = Inf)
# =========================
# PLOT B: HITS
# =========================
# --- Summary stats for Hits and FAs (means + SE across subjects) ---
SE_HITS2 <- summarySE(dprimeTable2, measurevar = "Hits", groupvars = c("nam_unn","RS_RT_N"))
SE_FA2   <- summarySE(dprimeTable2, measurevar = "FAs",  groupvars = c("nam_unn","RS_RT_N"))

# --- X positions for Hits/FAs summary layers (match SE_plot exactly) ---
SE_HITS_plot <- SE_HITS2 %>%
  mutate(
    nam_unn = factor(nam_unn, levels = c("Meaningful","Abstract")),
    RS_RT_N = factor(RS_RT_N, levels = c("Restudy","Retrieval")),
    nam_num  = as.numeric(nam_unn),
    cond_num = as.numeric(RS_RT_N),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

SE_FA_plot <- SE_FA2 %>%
  mutate(
    nam_unn = factor(nam_unn, levels = c("Meaningful","Abstract")),
    RS_RT_N = factor(RS_RT_N, levels = c("Restudy","Retrieval")),
    nam_num  = as.numeric(nam_unn),
    cond_num = as.numeric(RS_RT_N),
    x_mean   = nam_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

p_hits <- ggplot() +
  geom_point(
    data = plot_x,
    aes(x = x_plot, y = Hits, color = RS_RT_N),
    size = 3, alpha = 0.7
  ) +
  geom_line(
    data = plot_x %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = Hits, group = interaction(Subject, nam_unn)),
    alpha = 0.35, color = "grey"
  ) +
  geom_point(
    data = SE_HITS_plot,
    aes(x = x_mean, y = Hits),
    inherit.aes = FALSE,
    shape = 18, size = 4, color = "black"
  ) +
  geom_errorbar(
    data = SE_HITS_plot,
    aes(x = x_mean, ymin = Hits - se, ymax = Hits + se),
    inherit.aes = FALSE,
    width = 0.12, color = "black"
  ) +
  scale_x_continuous(breaks = c(1,2), labels = c("Meaningful","Abstract")) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_manual(name = "Condition", values = cond_cols) +
  ylab("HITS") +
  xlab("Stimulus Type") +
  ggtitle("HITS") +
  theme_bw() +
  theme(
    plot.title   = element_text(size = 30),
    axis.text    = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.text  = element_text(size = 30),
    legend.title = element_text(size = 30),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_hits)

# =========================
# PLOT C: FAs (auto y-limit so it never clips)
# =========================
fa_top <- max(plot_x$FAs, SE_FA_plot$FAs + SE_FA_plot$se, na.rm = TRUE) * 1.05
fa_bot <- min(0, min(plot_x$FAs, na.rm = TRUE))

p_fa <- ggplot() +
  geom_point(
    data = plot_x,
    aes(x = x_plot, y = FAs, color = RS_RT_N),
    size = 3, alpha = 0.7
  ) +
  geom_line(
    data = plot_x %>% arrange(nam_unn, Subject, RS_RT_N),
    aes(x = x_plot, y = FAs, group = interaction(Subject, nam_unn)),
    alpha = 0.35, color = "grey"
  ) +
  geom_point(
    data = SE_FA_plot,
    aes(x = x_mean, y = FAs),
    inherit.aes = FALSE,
    shape = 18, size = 4, color = "black"
  ) +
  geom_errorbar(
    data = SE_FA_plot,
    aes(x = x_mean, ymin = FAs - se, ymax = FAs + se),
    inherit.aes = FALSE,
    width = 0.12, color = "black"
  ) +
  scale_x_continuous(breaks = c(1,2), labels = c("Meaningful","Abstract")) +
  coord_cartesian(ylim = c(fa_bot, fa_top)) +
  scale_color_manual(name = "Condition", values = cond_cols) +
  ylab("FAs") +
  xlab("Stimulus Type") +
  ggtitle("FAs") +
  theme_bw() +
  theme(
    plot.title   = element_text(size = 30),
    axis.text    = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    axis.title.y = element_text(size = 30),
    legend.text  = element_text(size = 30),
    legend.title = element_text(size = 30),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_fa)





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
  ggtitle("Recall Performance") +
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
  coord_cartesian(ylim = c(0.5, 1)) +
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) + 
  ylab("HITS") +
  ggtitle("HITS") +
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
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[2], cbbPalette[8]),
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

write.csv(dprimeTable, file=paste("C:/Users/jgove/OneDrive/Desktop/dPrimeTableExp2.csv", sep=""),row.names = F)
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
Nam_RS = filter(dprimeTable, nam_unn == "nam" & RS_RT_N == "RS")
Nam_RT = filter(dprimeTable, nam_unn == "nam" & RS_RT_N == "RT")
Unn_RS = filter(dprimeTable, nam_unn == "unn" & RS_RT_N == "RS")
Unn_RT = filter(dprimeTable, nam_unn == "unn" & RS_RT_N == "RT")

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
