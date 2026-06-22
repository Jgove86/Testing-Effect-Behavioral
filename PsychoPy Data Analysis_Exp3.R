install.packages("Rmisc")
library(Rmisc) 
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("rstatix")
library(rstatix)
library(dplyr)

SONAsubID <- c(301:310,312:314,316:324,326:328,331,333:353,355:357,359:364,366:371)
allSubID <- SONAsubID

ratesDat <- data.frame(matrix(NA, length(allSubID) * 12, 6))
colnames(ratesDat) <- c('participant', 'nam_unn', 'rs_rt_n', 'N', 'nRespRight', 'PropRight')

dprimeTable <- data.frame(matrix(NA, length(allSubID) * 6, 7))
colnames(dprimeTable) <- c('participant', 'nam_unn', 'rs_rt_n', 'Hits', 'FAs', 'dprime', 'criterion')

subCt <- 0

library(dplyr)

for (s in 1:length(allSubID)) {
  fName <- paste('C:/Users/jgove/OneDrive/Desktop/Nameable and Unnameable Objects/data shaping/participant', allSubID[s], '.csv', sep = "")
  dat <- read.table(fName, sep = ',', header = TRUE)
  
  subCt <- subCt + 1
  
  dat$correct <- as.factor(dat$key_resp_14.corr)
  dat$nam_unn <- as.factor(dat$nam_unn)
  dat$rs_rt_n <- as.factor(dat$rs_rt_n)
  dat$old <- as.factor(dat$intact)

  
  condCt <- 0
  rs_rt_n_lvls <- levels(dat$rs_rt_n)
  old_lvls <- levels(dat$old)
  nam_unn_levels <- levels(dat$nam_unn)
  keys_levels <- levels(dat$key_resp_14.keys)
  
  for (oj in 1:length(nam_unn_levels)) {
    for (i in 1:length(rs_rt_n_lvls)) {
      for (k in 1:length(old_lvls)){
        condCt <- condCt + 1
        
        ratesDat$participant[subCt * 12 + condCt - 12] <- allSubID[s]
        ratesDat$nam_unn[subCt * 12 + condCt - 12] <- nam_unn_levels[oj]
        ratesDat$rs_rt_n[subCt * 12 + condCt - 12] <- rs_rt_n_lvls[i]
        ratesDat$Old[subCt * 12 + condCt - 12] <- old_lvls[k]
        
        filt_dat <- filter(filter(dat, nam_unn == nam_unn_levels[oj]), rs_rt_n == rs_rt_n_lvls[i], old == old_lvls[k])
        
        ratesDat$N[subCt * 12 + condCt - 12] <- nrow(filt_dat)
        ratesDat$nRespRight[subCt * 12 + condCt - 12] <- sum(filt_dat$key_resp_14.keys == "right")
        
        ratesDat$PropRight[subCt * 12 + condCt - 12] <- (ratesDat$nRespRight[subCt * 12 + condCt - 12]+.5) / (ratesDat$N[subCt * 12 + condCt - 12]+1)
        
        #if(old_lvls[k] == "right"){  # Corrected the condition here
        #dprimeTable$participant[(subCt - 1) * 6 + oj] <- allSubID[s]  # Corrected the indexing here
        #dprimeTable$nam_unn[(subCt - 1) * 6 + oj] <- nam_unn_levels[oj]  # Corrected the indexing here
        #dprimeTable$rs_rt_n[(subCt - 1) * 6 + oj] <- rs_rt_n_lvls[i]  # Corrected the indexing here
      }
    }
  }
}

library(tidyr)
df <- ratesDat %>% pivot_wider(id_cols = c("participant", "nam_unn", "rs_rt_n"), names_from = "Old", values_from = "PropRight")

dprimeTable$participant <- df$participant
dprimeTable$nam_unn <- df$nam_unn
dprimeTable$rs_rt_n <- df$rs_rt_n
dprimeTable$Hits <- df$right
dprimeTable$FAs <- df$left
dprimeTable$dprime <- qnorm(df$right) - qnorm(df$left)
dprimeTable$criterion <- (df$right + df$left)*-.5


dprimeTable$nam_unn <- as.factor(dprimeTable$nam_unn)


SE_dprime <- summarySE(dprimeTable, measurevar = "dprime", groupvars = c("nam_unn", "rs_rt_n"))
SE_HITS <- summarySE(dprimeTable, measurevar = "Hits", groupvars = c("nam_unn", "rs_rt_n"))
SE_FA <- summarySE(dprimeTable, measurevar = "FAs", groupvars = c("nam_unn", "rs_rt_n"))

print(ratesDat)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")  # black, gold, light blue, green, yellow, blue, dark orange, pink

library(ggplot2)  # Make sure to load ggplot2 package before running this part

# ===== Exp 3 dprime plot: points + mean + SE + aligned participant lines =====

library(dplyr)
library(ggplot2)
library(Rmisc)
library(grid)

# --- 0) Inspect what Exp3 actually contains ---
cat("RAW nam_unn unique:\n")
print(sort(unique(trimws(as.character(dprimeTable$nam_unn)))))

cat("\nRAW rs_rt_n unique:\n")
print(sort(unique(trimws(as.character(dprimeTable$rs_rt_n)))))

# --- 1) Clean + robustly map labels ---
d3 <- dprimeTable %>%
  mutate(
    participant = as.character(participant),
    
    nam_raw  = tolower(trimws(as.character(nam_unn))),
    cond_raw = tolower(trimws(as.character(rs_rt_n))),
    
    nam_unn = case_when(
      nam_raw %in% c("nam", "nameable", "meaningful") ~ "Meaningful",
      nam_raw %in% c("unn", "unnameable", "abstract", "mandarin") ~ "Mandarin",
      TRUE ~ as.character(nam_unn)
    ),
    
    rs_rt_n = case_when(
      cond_raw %in% c("n", "novel") ~ "Novel",
      cond_raw %in% c("rs", "restudy", "aprs") ~ "Restudy",
      cond_raw %in% c("rt", "retrieval", "aprt") ~ "Retrieval",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(is.finite(dprime)) %>%
  filter(!is.na(rs_rt_n), !is.na(nam_unn))

# Force Meaningful on the left, Mandarin on the right
d3$nam_unn <- factor(d3$nam_unn, levels = c("Meaningful", "Mandarin"))
d3$rs_rt_n <- factor(d3$rs_rt_n, levels = c("Novel", "Restudy", "Retrieval"))
d3$rs_rt_n <- droplevels(d3$rs_rt_n)

if (nrow(d3) == 0) {
  stop("After mapping, d3 has 0 rows. Check printed RAW rs_rt_n values above.")
}

cat("\nLevels used in plot:\n")
print(levels(d3$nam_unn))
print(levels(d3$rs_rt_n))

# --- 2) Mean + SE ---
SE_d3_d <- summarySE(
  d3,
  measurevar = "dprime",
  groupvars = c("nam_unn", "rs_rt_n")
)

# --- 3) Original 3-condition spacing controls ---
set.seed(123)

meaningful_center <- .8
mandarin_center   <- 2.2

spacing_factor <- 0.3
jitter_amount  <- 0.08

# --- 4) X positions + constant participant jitter ---
base_x3 <- d3 %>%
  mutate(
    stim_center = ifelse(
      nam_unn == "Meaningful",
      meaningful_center,
      mandarin_center
    ),
    cond_num = as.numeric(rs_rt_n),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

subj_offsets3 <- base_x3 %>%
  distinct(participant, nam_unn) %>%
  mutate(
    subj_jit = jitter(rep(0, n()), amount = jitter_amount)
  )

plot_x3 <- base_x3 %>%
  left_join(subj_offsets3, by = c("participant", "nam_unn")) %>%
  mutate(
    x_plot = x_mean + subj_jit
  )

SE_d3_d_x <- SE_d3_d %>%
  mutate(
    nam_unn = factor(nam_unn, levels = levels(d3$nam_unn)),
    rs_rt_n = factor(rs_rt_n, levels = levels(d3$rs_rt_n)),
    
    stim_center = ifelse(
      nam_unn == "Meaningful",
      meaningful_center,
      mandarin_center
    ),
    cond_num = as.numeric(rs_rt_n),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

cat("\nNA x_plot:", sum(is.na(plot_x3$x_plot)), "of", nrow(plot_x3), "\n")

# --- 5) Colors ---
cbbPalette <- c(
  "#000000", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

cond_cols3 <- c(
  "Novel"     = cbbPalette[4],
  "Restudy"   = cbbPalette[2],
  "Retrieval" = cbbPalette[8]
)

# --- 6) Plot ---
p3_dprime <- ggplot() +
  
  geom_point(
    data = plot_x3,
    aes(x = x_plot, y = dprime, color = rs_rt_n),
    size = 3,
    alpha = 0.7
  ) +
  
  geom_line(
    data = plot_x3 %>% arrange(nam_unn, participant, rs_rt_n),
    aes(x = x_plot, y = dprime, group = interaction(participant, nam_unn)),
    alpha = 0.35,
    color = "grey"
  ) +
  
  geom_point(
    data = SE_d3_d_x,
    aes(x = x_mean, y = dprime),
    inherit.aes = FALSE,
    shape = 18,
    size = 4,
    color = "black"
  ) +
  
  geom_errorbar(
    data = SE_d3_d_x,
    aes(x = x_mean, ymin = dprime - se, ymax = dprime + se),
    inherit.aes = FALSE,
    width = 0.12,
    color = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(meaningful_center, mandarin_center),
    labels = c("Meaningful", "Mandarin"),
    limits = c(0.35, 2.65)
  ) +
  
  scale_color_manual(
    name = NULL,
    values = cond_cols3,
    breaks = c("Novel", "Restudy", "Retrieval")
  ) +
  
  ylab("Visual Recognition (d')") +
  xlab("Stimulus Type") +
  ggtitle("Memory Performance Exp. 3") +
  
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

print(p3_dprime)

# Collapse across nam/unn
collapsed_3 <- dprimeTable %>%
  dplyr::group_by(participant, rs_rt_n) %>%
  dplyr::summarise(
    dprime = mean(dprime, na.rm = TRUE),
    Hits   = mean(Hits, na.rm = TRUE),
    FAs    = mean(FAs, na.rm = TRUE),
    .groups = "drop"
  )

# Means and SDs
desc_3 <- collapsed_3 %>%
  dplyr::group_by(rs_rt_n) %>%
  dplyr::summarise(
    N = dplyr::n(),
    M_dprime = mean(dprime, na.rm = TRUE),
    SD_dprime = sd(dprime, na.rm = TRUE),
    M_Hits = mean(Hits, na.rm = TRUE),
    SD_Hits = sd(Hits, na.rm = TRUE),
    M_FAs = mean(FAs, na.rm = TRUE),
    SD_FAs = sd(FAs, na.rm = TRUE),
    .groups = "drop"
  )

desc_3

wide_3 <- collapsed_3 %>%
  tidyr::pivot_wider(
    id_cols = participant,
    names_from = rs_rt_n,
    values_from = c(dprime, Hits, FAs)
  )

# dprime
t.test(wide_3$dprime_RS, wide_3$dprime_RT, paired = TRUE)
t.test(wide_3$dprime_RT, wide_3$dprime_N,  paired = TRUE)
t.test(wide_3$dprime_RS, wide_3$dprime_N,  paired = TRUE)

# Hits
t.test(wide_3$Hits_RS, wide_3$Hits_RT, paired = TRUE)
t.test(wide_3$Hits_RT, wide_3$Hits_N,  paired = TRUE)
t.test(wide_3$Hits_RS, wide_3$Hits_N,  paired = TRUE)

# False alarms
t.test(wide_3$FAs_RS, wide_3$FAs_RT, paired = TRUE)
t.test(wide_3$FAs_RT, wide_3$FAs_N,  paired = TRUE)
t.test(wide_3$FAs_RS, wide_3$FAs_N,  paired = TRUE)


##MISSING SUMMARY

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

missing_summary <- data.frame(
  participant = numeric(),
  n_missing = numeric(),
  total_trials = numeric(),
  prop_missing = numeric()
)

for (s in 1:length(allSubID)) {
  
  fName <- paste(
    'C:/Users/jgove/OneDrive/Desktop/Nameable and Unnameable Objects/data shaping/participant',
    allSubID[s],
    '.csv',
    sep = ""
  )
  
  dat <- read.csv(fName, stringsAsFactors = FALSE)
  
  # count real trials using the response column from Exp 3
  total_trials <- sum(!is.na(dat$key_resp_14.keys))
  
  # missing responses are typically NA or "None"
  n_missing <- sum(
    is.na(dat$key_resp_14.keys) |
      dat$key_resp_14.keys == "None"
  )
  
  prop_missing <- n_missing / total_trials
  
  missing_summary <- rbind(
    missing_summary,
    data.frame(
      participant = allSubID[s],
      n_missing = n_missing,
      total_trials = total_trials,
      prop_missing = prop_missing
    )
  )
}

print(missing_summary)

str(missing_summary)
print(missing_summary)
summary(missing_summary$n_missing)
unique(missing_summary$n_missing)


ggplot(missing_summary, aes(x = n_missing)) +
  geom_histogram(
    binwidth = 2,
    color = "black",
    fill = "steelblue"
  ) +
  xlab("Number of Missing Responses") +
  ylab("Number of Participants") +
  ggtitle("Distribution of Missing Responses Across Participants") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )



library(dplyr)
library(rstatix)
library(tidyr)

# =========================================================
# ANOVA for dprime
# =========================================================
anova_dat <- d3 %>%
  mutate(
    participant = factor(participant),
    StimType = factor(
      nam_unn,
      levels = levels(factor(nam_unn)),
      labels = c("Meaningful", "Mandarin")
    ),
    StudyCond = factor(
      rs_rt_n,
      levels = c("Novel", "Restudy", "Retrieval")
    )
  ) %>%
  select(participant, StimType, StudyCond, dprime) %>%
  filter(is.finite(dprime), !is.na(StimType), !is.na(StudyCond))

anova_res <- anova_test(
  data = anova_dat,
  dv = dprime,
  wid = participant,
  within = c(StimType, StudyCond)
)

# full output: ANOVA + Mauchly + sphericity corrections
print(anova_res)

# compact ANOVA table
anova_tab <- get_anova_table(anova_res)
print(anova_tab, width = Inf)

# =========================================================
# POST-HOC PAIRED T-TESTS for dprime
# =========================================================
posthoc_dprime <- anova_dat %>%
  group_by(StimType) %>%
  pairwise_t_test(
    dprime ~ StudyCond,
    paired = TRUE,
    p.adjust.method = "bonferroni",
    detailed = TRUE
  )

eff_posthoc_dprime <- anova_dat %>%
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
print(SE_dprime)
print(SE_HITS)
print(SE_FA)

# =========================================================
# ANOVA for HITS
# =========================================================
anova_hits_dat <- d3 %>%
  mutate(
    participant = factor(participant),
    StimType = factor(
      nam_unn,
      levels = levels(factor(nam_unn)),
      labels = c("Meaningful", "Mandarin")
    ),
    StudyCond = factor(
      rs_rt_n,
      levels = c("Novel", "Restudy", "Retrieval")
    )
  ) %>%
  select(participant, StimType, StudyCond, Hits) %>%
  filter(is.finite(Hits), !is.na(StimType), !is.na(StudyCond))

anova_hits_res <- anova_test(
  data = anova_hits_dat,
  dv = Hits,
  wid = participant,
  within = c(StimType, StudyCond)
)

print(anova_hits_res)

anova_hits_tab <- get_anova_table(anova_hits_res)
print(anova_hits_tab, width = Inf)

# =========================================================
# POST-HOC PAIRED T-TESTS for HITS
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
# ANOVA for FAs
# =========================================================
anova_fa_dat <- d3 %>%
  mutate(
    participant = factor(participant),
    StimType = factor(
      nam_unn,
      levels = levels(factor(nam_unn)),
      labels = c("Meaningful", "Mandarin")
    ),
    StudyCond = factor(
      rs_rt_n,
      levels = c("Novel", "Restudy", "Retrieval")
    )
  ) %>%
  select(participant, StimType, StudyCond, FAs) %>%
  filter(is.finite(FAs), !is.na(StimType), !is.na(StudyCond))

anova_fa_res <- anova_test(
  data = anova_fa_dat,
  dv = FAs,
  wid = participant,
  within = c(StimType, StudyCond)
)

print(anova_fa_res)

anova_fa_tab <- get_anova_table(anova_fa_res)
print(anova_fa_tab, width = Inf)

# =========================================================
# POST-HOC PAIRED T-TESTS for FAs
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

# ---- Plot A2: Hits ----

SE_d3_h <- summarySE(
  d3,
  measurevar = "Hits",
  groupvars = c("nam_unn", "rs_rt_n")
)

SE_d3_h_x <- SE_d3_h %>%
  mutate(
    nam_unn = factor(nam_unn, levels = levels(d3$nam_unn)),
    rs_rt_n = factor(rs_rt_n, levels = levels(d3$rs_rt_n)),
    stim_center = ifelse(nam_unn == "Meaningful",
                         meaningful_center,
                         mandarin_center),
    cond_num = as.numeric(rs_rt_n),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

p3_hits <- ggplot() +
  geom_point(
    data = plot_x3,
    aes(x = x_plot, y = Hits, color = rs_rt_n),
    size = 3, alpha = 0.7
  ) +
  geom_line(
    data = plot_x3 %>% arrange(nam_unn, participant, rs_rt_n),
    aes(x = x_plot, y = Hits, group = interaction(participant, nam_unn)),
    alpha = 0.35, color = "grey"
  ) +
  geom_point(
    data = SE_d3_h_x,
    aes(x = x_mean, y = Hits),
    inherit.aes = FALSE,
    shape = 18, size = 4, color = "black"
  ) +
  geom_errorbar(
    data = SE_d3_h_x,
    aes(x = x_mean, ymin = Hits - se, ymax = Hits + se),
    inherit.aes = FALSE,
    width = 0.12, color = "black"
  ) +
  scale_x_continuous(
    breaks = c(meaningful_center, mandarin_center),
    labels = c("Meaningful", "Mandarin"),
    limits = c(0.45, 2.55)
  ) +
  scale_color_manual(
    name = NULL,
    values = cond_cols3,
    breaks = c("Novel", "Restudy", "Retrieval")
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  ylab("HITS") +
  xlab("Stimulus Type") +
  ggtitle("HITS Exp. 3") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    axis.title.x = element_text(size = 30, margin = margin(t = 10)),
    axis.title.y = element_text(size = 30, margin = margin(r = 10)),
    legend.text = element_text(size = 30),
    legend.title = element_blank(),
    legend.spacing.y = unit(1, "cm"),
    legend.key.height = unit(1.2, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p3_hits)


# ---- Plot A3: FAs ----

SE_d3_fa <- summarySE(
  d3,
  measurevar = "FAs",
  groupvars = c("nam_unn", "rs_rt_n")
)

SE_d3_fa_x <- SE_d3_fa %>%
  mutate(
    nam_unn = factor(nam_unn, levels = levels(d3$nam_unn)),
    rs_rt_n = factor(rs_rt_n, levels = levels(d3$rs_rt_n)),
    stim_center = ifelse(nam_unn == "Meaningful",
                         meaningful_center,
                         mandarin_center),
    cond_num = as.numeric(rs_rt_n),
    x_mean = stim_center + (cond_num - 2) * spacing_factor
  )

fa_top3 <- max(plot_x3$FAs, SE_d3_fa_x$FAs + SE_d3_fa_x$se, na.rm = TRUE) * 1.05
fa_bot3 <- min(0, min(plot_x3$FAs, na.rm = TRUE))

p3_fas <- ggplot() +
  geom_point(
    data = plot_x3,
    aes(x = x_plot, y = FAs, color = rs_rt_n),
    size = 3, alpha = 0.7
  ) +
  geom_line(
    data = plot_x3 %>% arrange(nam_unn, participant, rs_rt_n),
    aes(x = x_plot, y = FAs, group = interaction(participant, nam_unn)),
    alpha = 0.35, color = "grey"
  ) +
  geom_point(
    data = SE_d3_fa_x,
    aes(x = x_mean, y = FAs),
    inherit.aes = FALSE,
    shape = 18, size = 4, color = "black"
  ) +
  geom_errorbar(
    data = SE_d3_fa_x,
    aes(x = x_mean, ymin = FAs - se, ymax = FAs + se),
    inherit.aes = FALSE,
    width = 0.12, color = "black"
  ) +
  scale_x_continuous(
    breaks = c(meaningful_center, mandarin_center),
    labels = c("Meaningful", "Mandarin"),
    limits = c(0.45, 2.55)
  ) +
  scale_color_manual(
    name = NULL,
    values = cond_cols3,
    breaks = c("Novel", "Restudy", "Retrieval")
  ) +
  coord_cartesian(ylim = c(fa_bot3, fa_top3)) +
  ylab("FAs") +
  xlab("Stimulus Type") +
  ggtitle("FAs Exp. 3") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30),
    axis.text = element_text(size = 30),
    axis.title.x = element_text(size = 30, margin = margin(t = 10)),
    axis.title.y = element_text(size = 30, margin = margin(r = 10)),
    legend.text = element_text(size = 30),
    legend.title = element_blank(),
    legend.spacing.y = unit(1, "cm"),
    legend.key.height = unit(1.2, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p3_fas)

# -------------------------------------------------------------------
# B) MINUS NOVEL: (Restudy-Novel) and (Retrieval-Novel) scatter plots
# -------------------------------------------------------------------
wide3 <- d3 %>%
  select(participant, nam_unn, rs_rt_n, dprime, Hits, FAs) %>%
  pivot_wider(
    id_cols = c(participant, nam_unn),
    names_from = rs_rt_n,
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

dprime_long3 <- wide3 %>%
  select(participant, nam_unn, dprime_RS_N, dprime_RT_N) %>%
  pivot_longer(cols=c(dprime_RS_N, dprime_RT_N), names_to="contrast", values_to="dprime") %>%
  mutate(
    contrast = recode(contrast, dprime_RS_N="Restudy - Novel", dprime_RT_N="Retrieval - Novel"),
    contrast = factor(contrast, levels=c("Restudy - Novel","Retrieval - Novel"))
  ) %>%
  filter(is.finite(dprime))

Hits_long3 <- wide3 %>%
  select(participant, nam_unn, Hits_RS_N, Hits_RT_N) %>%
  pivot_longer(cols=c(Hits_RS_N, Hits_RT_N), names_to="contrast", values_to="Hits") %>%
  mutate(
    contrast = recode(contrast, Hits_RS_N="Restudy - Novel", Hits_RT_N="Retrieval - Novel"),
    contrast = factor(contrast, levels=c("Restudy - Novel","Retrieval - Novel"))
  ) %>%
  filter(is.finite(Hits))

FAs_long3 <- wide3 %>%
  select(participant, nam_unn, FAs_RS_N, FAs_RT_N) %>%
  pivot_longer(cols=c(FAs_RS_N, FAs_RT_N), names_to="contrast", values_to="FAs") %>%
  mutate(
    contrast = recode(contrast, FAs_RS_N="Restudy - Novel", FAs_RT_N="Retrieval - Novel"),
    contrast = factor(contrast, levels=c("Restudy - Novel","Retrieval - Novel"))
  ) %>%
  filter(is.finite(FAs))

SE_dprime_long3 <- summarySE(dprime_long3, measurevar="dprime", groupvars=c("nam_unn","contrast"))
SE_hits_long3   <- summarySE(Hits_long3,   measurevar="Hits",   groupvars=c("nam_unn","contrast"))
SE_fa_long3     <- summarySE(FAs_long3,    measurevar="FAs",    groupvars=c("nam_unn","contrast"))

# x positions + constant participant jitter for minus-novel plots
set.seed(123)
contrast_cols <- c("Restudy - Novel"=cbbPalette[2], "Retrieval - Novel"=cbbPalette[8])

base_minus3 <- dprime_long3 %>%
  mutate(
    stim_num = as.numeric(nam_unn),
    cond_num = as.numeric(contrast),
    x_mean   = stim_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )
off_minus3 <- base_minus3 %>%
  distinct(participant, nam_unn) %>%
  mutate(subj_jit = jitter(rep(0, n()), amount = jitter_amount))
plot_minus_d <- base_minus3 %>%
  left_join(off_minus3, by=c("participant","nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

base_minus_h <- Hits_long3 %>%
  mutate(
    stim_num = as.numeric(nam_unn),
    cond_num = as.numeric(contrast),
    x_mean   = stim_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  ) %>%
  left_join(off_minus3, by=c("participant","nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

base_minus_fa <- FAs_long3 %>%
  mutate(
    stim_num = as.numeric(nam_unn),
    cond_num = as.numeric(contrast),
    x_mean   = stim_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  ) %>%
  left_join(off_minus3, by=c("participant","nam_unn")) %>%
  mutate(x_plot = x_mean + subj_jit)

SE_minus_d <- SE_dprime_long3 %>%
  mutate(
    stim_num = as.numeric(nam_unn),
    cond_num = as.numeric(contrast),
    x_mean   = stim_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )
SE_minus_h <- SE_hits_long3 %>%
  mutate(
    stim_num = as.numeric(nam_unn),
    cond_num = as.numeric(contrast),
    x_mean   = stim_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )
SE_minus_fa <- SE_fa_long3 %>%
  mutate(
    stim_num = as.numeric(nam_unn),
    cond_num = as.numeric(contrast),
    x_mean   = stim_num + (cond_num - mean(unique(cond_num))) * spacing_factor
  )

# ---- Plot B1: d' minus Novel ----
p3_minus_d <- ggplot() +
  geom_point(data=plot_minus_d, aes(x=x_plot, y=dprime, color=contrast), size=3, alpha=0.7) +
  geom_line(data=plot_minus_d %>% arrange(nam_unn, participant, contrast),
            aes(x=x_plot, y=dprime, group=interaction(participant, nam_unn)),
            alpha=0.35, color="grey") +
  geom_point(data=SE_minus_d, aes(x=x_mean, y=dprime),
             inherit.aes=FALSE, shape=18, size=4, color="black") +
  geom_errorbar(data=SE_minus_d, aes(x=x_mean, ymin=dprime-se, ymax=dprime+se),
                inherit.aes=FALSE, width=0.12, color="black") +
  scale_x_continuous(breaks=1:length(levels(d3$nam_unn)), labels=stim_labels) +
  scale_color_manual(name="Contrast", values=contrast_cols) +
  ylab("d' (Condition - Novel)") +
  ggtitle("Memory Performance (Minus Novel) Exp 3") +
  theme_bw()
print(p3_minus_d)

# ---- Plot B2: Hits minus Novel ----
p3_minus_h <- ggplot() +
  geom_point(data=base_minus_h, aes(x=x_plot, y=Hits, color=contrast), size=3, alpha=0.7) +
  geom_line(data=base_minus_h %>% arrange(nam_unn, participant, contrast),
            aes(x=x_plot, y=Hits, group=interaction(participant, nam_unn)),
            alpha=0.35, color="grey") +
  geom_point(data=SE_minus_h, aes(x=x_mean, y=Hits),
             inherit.aes=FALSE, shape=18, size=4, color="black") +
  geom_errorbar(data=SE_minus_h, aes(x=x_mean, ymin=Hits-se, ymax=Hits+se),
                inherit.aes=FALSE, width=0.12, color="black") +
  scale_x_continuous(breaks=1:length(levels(d3$nam_unn)), labels=stim_labels) +
  scale_color_manual(name="Contrast", values=contrast_cols) +
  ylab("Hits (Condition - Novel)") +
  ggtitle("Hits (Minus Novel) Exp 3") +
  theme_bw()
print(p3_minus_h)

# ---- Plot B3: FAs minus Novel (auto ylim) ----
fa_top3m <- max(base_minus_fa$FAs, SE_minus_fa$FAs + SE_minus_fa$se, na.rm=TRUE) * 1.05
fa_bot3m <- min(0, min(base_minus_fa$FAs, na.rm=TRUE))

p3_minus_fa <- ggplot() +
  geom_point(data=base_minus_fa, aes(x=x_plot, y=FAs, color=contrast), size=3, alpha=0.7) +
  geom_line(data=base_minus_fa %>% arrange(nam_unn, participant, contrast),
            aes(x=x_plot, y=FAs, group=interaction(participant, nam_unn)),
            alpha=0.35, color="grey") +
  geom_point(data=SE_minus_fa, aes(x=x_mean, y=FAs),
             inherit.aes=FALSE, shape=18, size=4, color="black") +
  geom_errorbar(data=SE_minus_fa, aes(x=x_mean, ymin=FAs-se, ymax=FAs+se),
                inherit.aes=FALSE, width=0.12, color="black") +
  scale_x_continuous(breaks=1:length(levels(d3$nam_unn)), labels=stim_labels) +
  scale_color_manual(name="Contrast", values=contrast_cols) +
  coord_cartesian(ylim=c(fa_bot3m, fa_top3m)) +
  ylab("FAs (Condition - Novel)") +
  ggtitle("FAs (Minus Novel) Exp 3") +
  theme_bw()
print(p3_minus_fa)








##original plots


ggplot(data = SE_dprime,
       aes(x = nam_unn, y = dprime, fill = rs_rt_n)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymax = dprime + se, ymin = dprime - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Stimulus Type", labels = c("Meaningful", "Mandarin")) +
  scale_fill_manual(name = "Condition", values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) +
  scale_y_continuous(name = "Visual Recognition (d')", limits = c(0, 2)) +
  ggtitle("Memory Performance Exp. 3") +
  theme_bw() +  # Place this first
  theme(plot.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))


ggplot(data = SE_HITS,
       aes(x = nam_unn, y = Hits, fill = rs_rt_n)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = Hits + se, ymin = Hits - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Stimulus Type", labels = c("Meaningful", "Mandarin")) + 
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_manual(name = "Condition", values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) + 
  ylab("HITS") +
  ggtitle("HITS Exp 3") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()

ggplot(data = SE_FA,
       aes(x = nam_unn, y = FAs, fill = rs_rt_n)) + 
  geom_bar(position = position_dodge(), stat = "identity", color = "black") + 
  geom_errorbar(aes(ymax = FAs + se, ymin = FAs - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Stimulus Type", labels = c("Meaningful", "Mandarin")) + 
  coord_cartesian(ylim = c(0, 0.5)) +
  scale_fill_manual(name = "Condition", values = c(cbbPalette[4], cbbPalette[2], cbbPalette[8]),
                    labels = c("Novel", "Restudy", "Retrieval")) + 
  ylab("FAs") +
  ggtitle("FAs Exp 3") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
  theme_bw()

   
write.csv(ratesDat, file=paste("C:/Users/jgove/OneDrive/Desktop/ratesDat_4_1.csv", sep=""),row.names = F)
write.csv(pivot_df, file=paste("C:/Users/jgove/OneDrive/Desktop/pivotdf.csv", sep=""),row.names = F)

write.csv(dprimeTable, file=paste("C:/Users/jgove/OneDrive/Desktop/dPrimeTable-4-17.csv", sep=""),row.names = F)
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
  ggtitle("Individual participant Recognition Performance") +  
  theme(text = element_text(size=20)) +
  facet_wrap(~participant, ncol = 5, labeller = "label_both") 


ggplot(dprimeTable, aes(x = Obj_Sce, y = dprime_recall, fill = Obj_Sce)) +
  geom_bar(position = position_dodge(),stat = "identity", color = "black") + 
  scale_x_discrete("Image Type", labels = c("Objects", "Scenes")) + 
  scale_fill_manual(name = "Image Type", values = c(cbbPalette[8], cbbPalette[3]),
                    labels = c("Objects", "Scenes")) +
  guides(fill=FALSE)+
  ylab("Visual Learning Score") + 
  ggtitle("Individual participant Recall Performance") +  
  theme(text = element_text(size=20)) +
  facet_wrap(~participant, ncol = 5, labeller = "label_both") 

#Recog.anova <- aov(dprimeTable$dprime_recog~dprimeTable$Group*dprimeTable$Obj_Sce)
#summary(Recog.anova)

#library(tidyr)
dprime_wide <- dprimeTable %>% pivot_wider(id_cols = c("participant", "nam_unn"), names_from = "rs_rt_n", values_from = "dprime")

dprime_wide$RS_RT <- dprime_wide$RT - dprime_wide$RS

diff_wide <- dprime_wide %>% pivot_wider(id_cols = c("participant"), names_from = "nam_unn", values_from = "RS_RT")

diff_wide$avgRSRT <- (diff_wide$nam + diff_wide$unn)/2

t_test_result <- t.test(diff_wide$avgRSRT, mu = 0, alternative = "two.sided")
print(t_test_result)


t_test_RSRT <- t.test(diff_wide$nam, diff_wide$unn, alternative = "two.sided", mu = 0, paired = FALSE)
print(t_test_RSRT)

##Minus novel
dprime_wide <- dprimeTable %>% pivot_wider(id_cols = c("participant", "nam_unn"), names_from = "rs_rt_n", values_from = "dprime")

dprime_wide$RS_N <- dprime_wide$RS - dprime_wide$N
dprime_wide$RT_N <- dprime_wide$RT - dprime_wide$N

dprime_wideH <- dprimeTable %>% pivot_wider(id_cols = c("participant", "nam_unn"), names_from = "rs_rt_n", values_from = "Hits")

dprime_wideH$RS_N <- dprime_wideH$RS - dprime_wideH$N
dprime_wideH$RT_N <- dprime_wideH$RT - dprime_wideH$N

dprime_wideFA <- dprimeTable %>% pivot_wider(id_cols = c("participant", "nam_unn"), names_from = "rs_rt_n", values_from = "FAs")

dprime_wideFA$RS_N <- dprime_wideFA$RS - dprime_wideFA$N
dprime_wideFA$RT_N <- dprime_wideFA$RT - dprime_wideFA$N

dprime_long <- dprime_wide %>% 
  pivot_longer(cols = c(RS_N, RT_N), names_to = "rs_rt_n", values_to = "dprime")
dprime_longH <- dprime_wideH %>% 
  pivot_longer(cols = c(RS_N, RT_N), names_to = "rs_rt_n", values_to = "Hits")
dprime_longFA <- dprime_wideFA %>% 
  pivot_longer(cols = c(RS_N, RT_N), names_to = "rs_rt_n", values_to = "FAs")

SE_dprime_long <- summarySE(dprime_long, measurevar = "dprime", groupvars = c("nam_unn", "rs_rt_n"))
SE_HITS_long <- summarySE(dprime_longH, measurevar = "Hits", groupvars = c("nam_unn", "rs_rt_n"))
SE_FA_long <- summarySE(dprime_longFA, measurevar = "FAs", groupvars = c("nam_unn", "rs_rt_n"))

ggplot(data = SE_dprime_long,
       aes(x = nam_unn, y = dprime, fill = rs_rt_n)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymax = dprime + se, ymin = dprime - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete("Stimulus Type", labels = c("Meaningful", "Mandarin")) +
  scale_fill_manual("Condition", values = c(cbbPalette[2], cbbPalette[8]),
                    labels = c("Restudy", "Retrieval")) +
  ylab("Visual Recognition (d')") +
  ggtitle("Memory Performance-Minus Novel Exp 3") +
  theme(plot.title = element_text(size=20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20))
theme_bw()

ggplot(data = SE_HITS_long,
       aes(x = nam_unn, y = Hits, fill = rs_rt_n)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymax = Hits + se, ymin = Hits - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete(labels = c("Nameable", "Unnameable")) +
  scale_fill_manual(values = c(cbbPalette[5], cbbPalette[8]),
                    labels = c("Restudy", "Retrieval")) +
  ylab("Hits") +
  ggtitle("Recognition Performance") +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme_bw()

ggplot(data = SE_FA_long,
       aes(x = nam_unn, y = FAs, fill = rs_rt_n)) +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymax = FAs + se, ymin = FAs - se),
                position = position_dodge(.9), color = "black", width = .2) +
  scale_x_discrete(labels = c("Nameable", "Unnameable")) +
  scale_fill_manual(values = c(cbbPalette[5], cbbPalette[8]),
                    labels = c("Restudy", "Retrieval")) +
  ylab("FAs") +
  ggtitle("Recognition Performance") +
  theme(plot.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title.x = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(legend.title = element_text(size = 20)) +
  theme_bw()




Recall.anovaN <- aov(dprime_long$dprime~dprime_long$nam_unn*dprime_long$rs_rt_n)
summary(Recall.anovaN)

RecallH.anovaN <- aov(dprime_longH$Hits~dprime_longH$nam_unn*dprime_longH$rs_rt_n)
summary(RecallH.anovaN)

RecallF.anovaN <- aov(dprime_longFA$FAs~dprime_longFA$nam_unn*dprime_longFA$rs_rt_n)
summary(RecallF.anovaN)

## t-tests filtering
Nam_RS = filter(dprime_long, nam_unn == "nam" & rs_rt_n == "RS_N")
Nam_RT = filter(dprime_long, nam_unn == "nam" & rs_rt_n == "RT_N")
Unn_RS = filter(dprime_long, nam_unn == "unn" & rs_rt_n == "RS_N")
Unn_RT = filter(dprime_long, nam_unn == "unn" & rs_rt_n == "RT_N")

Nam_RSH = filter(dprime_longH, nam_unn == "nam" & rs_rt_n == "RS_N")
Nam_RTH = filter(dprime_longH, nam_unn == "nam" & rs_rt_n == "RT_N")
Unn_RSH = filter(dprime_longH, nam_unn == "unn" & rs_rt_n == "RS_N")
Unn_RTH = filter(dprime_longH, nam_unn == "unn" & rs_rt_n == "RT_N")

Nam_RSF = filter(dprime_longFA, nam_unn == "nam" & rs_rt_n == "RS_N")
Nam_RTF = filter(dprime_longFA, nam_unn == "nam" & rs_rt_n == "RT_N")
Unn_RSF = filter(dprime_longFA, nam_unn == "unn" & rs_rt_n == "RS_N")
Unn_RTF = filter(dprime_longFA, nam_unn == "unn" & rs_rt_n == "RT_N")

##d'

t.test(Nam_RS$dprime, Nam_RT$dprime, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RS$dprime, Unn_RT$dprime, alternative = "two.sided", mu = 0, paired = FALSE)


##Hits

t.test(Nam_RSH$Hits, Nam_RTH$Hits, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RSH$Hits, Unn_RTH$Hits, alternative = "two.sided", mu = 0, paired = FALSE)


##FAs

t.test(Nam_RSF$FAs, Nam_RTF$FAs, alternative = "two.sided", mu = 0, paired = FALSE)
t.test(Unn_RSF$FAs, Unn_RTF$FAs, alternative = "two.sided", mu = 0, paired = FALSE)







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

#mixed ANOVAS
Recog.aov <- anova_test(
  data = dprimeTable, dv = dprime_recog, wid = participant,
  between = Group, within = Obj_Sce
)
get_anova_table(Recog.aov)

Recall.aov <- anova_test(
  data = dprimeTable, dv = dprime_recall, wid = participant,
  between = Group, within = Obj_Sce
)
get_anova_table(Recall.aov)

RecogHITS.aov <- anova_test(
  data = dprimeTable, dv = Hits_recog, wid = participant,
  between = Group, within = Obj_Sce
)
get_anova_table(RecogHITS.aov)

RecogFAS.aov <- anova_test(
  data = dprimeTable, dv = FAs_recog, wid = participant,
  between = Group, within = Obj_Sce
)
get_anova_table(RecogFAS.aov)

RecallHITS.aov <- anova_test(
  data = dprimeTable, dv = Hits_recall, wid = participant,
  between = Group, within = Obj_Sce
)
get_anova_table(RecallHITS.aov)

RecallFAS.aov <- anova_test(
  data = dprimeTable, dv = FAs_recall, wid = participant,
  between = Group, within = Obj_Sce
)
get_anova_table(RecallFAS.aov)


SYules.aov <- anova_test(
  data = dprimeTable, dv = Yules_Studied, wid = participant,
  between = Group, within = Obj_Sce
)
get_anova_table(SYules.aov)

UYules.aov <- anova_test(
  data = dprimeTable, dv = Yules_Unstudied, wid = participant,
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

## t-tests Screen size & compensation (MODIFY participantS TO RUN)
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
