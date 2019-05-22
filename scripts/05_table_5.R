#
# Delib. in Kirkuk
# Attitudes, Opinions etc. split by ethnicity
# 
# 1. list-wise delete missing only on the current wave
# 2. list-wise delete missing on any of the 2 waves (Table 5)
# 

# set directory
setwd(githubdir)
setwd("kirkuk/")

# Load libs 
library(tidyr)
library(dplyr)
library(broom)

# Read in the data
source("scripts/01_recode.R")

# 1. subset by ethnicity; list-wise delete missing only on the current wave
# --------------------------------------------------------------------------

t4_vars <- c("local_trust", "central_trust", "fed", "ind_kirkuk", "ca_3", "cb_5")

t4_ethnic <- all_dat[, c(t4_vars, "cond", "ethnicity", "wave")] %>%
    group_by(ethnicity, cond, wave) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))

# Get Condition/Wave concat
t4_ethnic$cond <- paste0(t4_ethnic$cond, t4_ethnic$wave)

t4_ethnic_t <- t4_ethnic %>%
    gather(key = var_name, value = value, 3:8) %>% 
    spread_(key = "cond", value = "value") %>%
    filter(!is.na(ethnicity) & ethnicity != "Other") %>%
    filter(var_name != "wave") %>%
    arrange(var_name)

t4_ethnic_t$diff_delib       <- t4_ethnic_t$delib2 - t4_ethnic_t$delib1
t4_ethnic_t$diff_delib_info  <- t4_ethnic_t$delib_info2 - t4_ethnic_t$delib_info1

wave_wise_res <- t4_ethnic_t[, c("ethnicity", "var_name", "control1", "delib1", "delib2", "delib_info1", "delib_info2", "diff_delib", "diff_delib_info")]

# Pooled t1
# -----------------------------
ethnic_t1_res <- all_dat[, c(t4_vars, "ethnicity", "wave")] %>%
    group_by(ethnicity, wave) %>%
    summarise_all(funs(mean(., na.rm = TRUE))) %>%
    filter(wave == 1) %>%
    filter(!is.na(ethnicity) & ethnicity != "Other") %>%
    select(-wave)

ethnic_t1_res <- melt(ethnic_t1_res)


# Table 5: list-wise delete missing on any of the 2 waves
# ----------------------------------------------------------

# Convenient subsets
ewall_dat <- subset(wall_dat, ethnicity_t1 %in% c("Arab", "Kurd", "Turkman"))

arab_d_set  <- ewall_dat[ewall_dat$cond_t1 == "delib" & ewall_dat$ethnicity_t1 == "Arab", ]
arab_di_set <- ewall_dat[ewall_dat$cond_t1 == "delib_info"  & ewall_dat$ethnicity_t1 == "Arab", ]

kurd_d_set  <- ewall_dat[ewall_dat$cond_t1 == "delib"  & ewall_dat$ethnicity_t1 == "Kurd", ]
kurd_di_set <- ewall_dat[ewall_dat$cond_t1 == "delib_info"  & ewall_dat$ethnicity_t1 == "Kurd", ]

turk_d_set  <- ewall_dat[ewall_dat$cond_t1 == "delib"  & ewall_dat$ethnicity_t1 == "Turkman", ]
turk_di_set <- ewall_dat[ewall_dat$cond_t1 == "delib_info"  & ewall_dat$ethnicity_t1 == "Turkman", ]

# List of tab 4 vars
tab_4_t2 <- paste0(c("ethnocentrism", "ca_3", "fed", "ind_kirkuk", "local_trust", "central_trust", "cb_5"), "_t2")
tab_4_t1 <- paste0(c("ethnocentrism", "ca_3", "fed", "ind_kirkuk", "local_trust", "central_trust", "cb_5"), "_t1")

# 5a: Arab
# ----------------

# No NA on either wave/List-wise deletion estimates
arab_res_nona <- data.frame(var_name = NA, t1_delib_nona = NA, t2_delib_nona = NA, t1_di_nona = NA, t2_di_nona = NA)

for (i in 1:length(tab_4_t2)) {
    
    nona_delib      <- !is.na(arab_d_set[, tab_4_t1[i]]) & !is.na(arab_d_set[, tab_4_t2[i]])
    nona_delib_info <- !is.na(arab_di_set[, tab_4_t1[i]]) & !is.na(arab_di_set[, tab_4_t2[i]])

    t1_delib <- mean(arab_d_set[, tab_4_t1[i]][nona_delib], na.rm = T)
    t2_delib <- mean(arab_d_set[, tab_4_t2[i]][nona_delib], na.rm = T)

    t1_delib_i <- mean(arab_di_set[,  tab_4_t1[i]][nona_delib_info], na.rm = T)
    t2_delib_i <- mean(arab_di_set[,  tab_4_t2[i]][nona_delib_info], na.rm = T)

    arab_res_nona[i, ] <- c(tab_4_t2[i], t1_delib, t2_delib, t1_delib_i, t2_delib_i)

}

# Arab P-values

diff_delib  <- arab_d_set[, tab_4_t2] - arab_d_set[, tab_4_t1]
diff_delib  <- subset(diff_delib, select = tab_4_t2)
res_delib   <- do.call(rbind, lapply(diff_delib, function(x) tidy(t.test(x, mu = 0))))
names(res_delib) <- paste0(names(res_delib), "_d")
res_delib$var_name <- gsub("_t2", "", rownames(res_delib))

diff_delib_info  <- arab_di_set[, tab_4_t2] - arab_di_set[, tab_4_t1]
diff_delib_info  <- subset(diff_delib_info, select = tab_4_t2)
res_delib_info   <- do.call(rbind, lapply(diff_delib_info, function(x) tidy(t.test(x, mu = 0))))
names(res_delib_info) <- paste0(names(res_delib_info), "_di")
res_delib_info$var_name_di <- gsub("_t2", "", rownames(res_delib_info))

arab_res <- res_delib %>%
    left_join(res_delib_info, by = c("var_name" =  "var_name_di"))

arab_res <- arab_res[, c("var_name", "estimate_d", "p.value_d", "estimate_di", "p.value_di")]

arab_res_all <- arab_res %>%
    mutate(var_name = paste0(var_name, "_t2")) %>%
    left_join(arab_res_nona)

# 5b. Kurds
# -----------------

# No NA on either wave/List-wise deletion estimates
kurd_res_nona <- data.frame(var_name = NA, t1_delib_nona = NA, t2_delib_nona = NA, t1_di_nona = NA, t2_di_nona = NA)

for (i in 1:length(tab_4_t2)) {
    
    nona_delib      <- !is.na(kurd_d_set[, tab_4_t1[i]]) & !is.na(kurd_d_set[, tab_4_t2[i]])
    nona_delib_info <- !is.na(kurd_di_set[, tab_4_t1[i]]) & !is.na(kurd_di_set[, tab_4_t2[i]])

    t1_delib <- mean(kurd_d_set[, tab_4_t1[i]][nona_delib], na.rm = T)
    t2_delib <- mean(kurd_d_set[, tab_4_t2[i]][nona_delib], na.rm = T)

    t1_delib_i <- mean(kurd_di_set[,  tab_4_t1[i]][nona_delib_info], na.rm = T)
    t2_delib_i <- mean(kurd_di_set[,  tab_4_t2[i]][nona_delib_info], na.rm = T)

    kurd_res_nona[i, ] <- c(tab_4_t2[i], t1_delib, t2_delib, t1_delib_i, t2_delib_i)

}

# kurd P-values

diff_delib  <- kurd_d_set[, tab_4_t2] - kurd_d_set[, tab_4_t1]
diff_delib  <- subset(diff_delib, select = tab_4_t2)
res_delib   <- do.call(rbind, lapply(diff_delib, function(x) tidy(t.test(x, mu = 0))))
names(res_delib) <- paste0(names(res_delib), "_d")
res_delib$var_name <- gsub("_t2", "", rownames(res_delib))

diff_delib_info  <- kurd_di_set[, tab_4_t2] - kurd_di_set[, tab_4_t1]
diff_delib_info  <- subset(diff_delib_info, select = tab_4_t2)
res_delib_info   <- do.call(rbind, lapply(diff_delib_info, function(x) tidy(t.test(x, mu = 0))))
names(res_delib_info) <- paste0(names(res_delib_info), "_di")
res_delib_info$var_name_di <- gsub("_t2", "", rownames(res_delib_info))

kurd_res <- res_delib %>%
    left_join(res_delib_info, by = c("var_name" =  "var_name_di"))

kurd_res <- kurd_res[, c("var_name", "estimate_d", "p.value_d", "estimate_di", "p.value_di")]

kurd_res_all <- kurd_res %>%
    mutate(var_name = paste0(var_name, "_t2")) %>%
    left_join(kurd_res_nona)

# 5c. Turkoman
# -----------------

# No NA on either wave/List-wise deletion estimates
turk_res_nona <- data.frame(var_name = NA, t1_delib_nona = NA, t2_delib_nona = NA, t1_di_nona = NA, t2_di_nona = NA)

for (i in 1:length(tab_4_t2)) {
    
    nona_delib      <- !is.na(turk_d_set[, tab_4_t1[i]]) & !is.na(turk_d_set[, tab_4_t2[i]])
    nona_delib_info <- !is.na(turk_di_set[, tab_4_t1[i]]) & !is.na(turk_di_set[, tab_4_t2[i]])

    t1_delib <- mean(turk_d_set[, tab_4_t1[i]][nona_delib], na.rm = T)
    t2_delib <- mean(turk_d_set[, tab_4_t2[i]][nona_delib], na.rm = T)

    t1_delib_i <- mean(turk_di_set[,  tab_4_t1[i]][nona_delib_info], na.rm = T)
    t2_delib_i <- mean(turk_di_set[,  tab_4_t2[i]][nona_delib_info], na.rm = T)

    turk_res_nona[i, ] <- c(tab_4_t2[i], t1_delib, t2_delib, t1_delib_i, t2_delib_i)

}

# turk P-values

diff_delib  <- turk_d_set[, tab_4_t2] - turk_d_set[, tab_4_t1]
diff_delib  <- subset(diff_delib, select = tab_4_t2)
res_delib   <- do.call(rbind, lapply(diff_delib, function(x) tidy(t.test(x, mu = 0))))
names(res_delib) <- paste0(names(res_delib), "_d")
res_delib$var_name <- gsub("_t2", "", rownames(res_delib))

diff_delib_info  <- turk_di_set[, tab_4_t2] - turk_di_set[, tab_4_t1]
diff_delib_info  <- subset(diff_delib_info, select = tab_4_t2)
res_delib_info   <- do.call(rbind, lapply(diff_delib_info, function(x) tidy(t.test(x, mu = 0))))
names(res_delib_info) <- paste0(names(res_delib_info), "_di")
res_delib_info$var_name_di <- gsub("_t2", "", rownames(res_delib_info))

turk_res <- res_delib %>%
    left_join(res_delib_info, by = c("var_name" =  "var_name_di"))

turk_res <- turk_res[, c("var_name", "estimate_d", "p.value_d", "estimate_di", "p.value_di")]

turk_res_all <- turk_res %>%
    mutate(var_name = paste0(var_name, "_t2")) %>%
    left_join(turk_res_nona)

res_all <- rbind(cbind(arab_res_all, ethnicity = "Arab"),
                 cbind(kurd_res_all, ethnicity = "Kurd"),
                 cbind(turk_res_all, ethnicity = "Turkman"))

# Merge
res_all$var_name <- gsub("_t2", "", res_all$var_name)

res_all <- res_all %>% 
          left_join(ethnic_t1_res, by = c("var_name" = "variable", "ethnicity" = "ethnicity"))

res_all <- res_all %>%
    arrange(var_name)

res_all <- res_all[, c("var_name", "ethnicity", "value",
                       "t1_delib_nona", "t2_delib_nona", "estimate_d", "p.value_d",
                       "t1_di_nona", "t2_di_nona", "estimate_di", "p.value_di" )]

write.csv(res_all, file = "tabs/04_table_5.csv", row.names = F)

# Inverse Propensity Score Weighting
# Compare Change in D to DI. 
# ---------------------------------------

# Subset on D and DI
dat_d_di <- subset(all_dat, cond %in% c("delib", "delib_info"))

# Create dummy vectors for missing
dat_d_di$nona_age             <- nona(dat_d_di$age)
dat_d_di$age_miss             <- is.na(dat_d_di$age)

dat_d_di$nona_lived_in_kirkuk_1 <- nona(dat_d_di$lived_in_kirkuk == 1)
dat_d_di$nona_lived_in_kirkuk_2 <- nona(dat_d_di$lived_in_kirkuk == 2)
dat_d_di$lived_in_kirkuk_miss <- is.na(dat_d_di$lived_in_kirkuk)

dat_d_di$nona_sunni <- nona(dat_d_di$sunni == 1)
dat_d_di$relig_miss <- is.na(dat_d_di$sunni)

# Dependent Variable so that it is clear
dat_d_di$d_or_di <- I(dat_d_di$cond == "delib")

# Vector of covariates
covars <- c("nona_age", "age_miss", 
            "female", "kirkuk", 
            "nona_lived_in_kirkuk_1", "nona_lived_in_kirkuk_2", "lived_in_kirkuk_miss",
            "nona_relig", "relig_miss")

d_di_reg <- glm(d_or_di ~  nona_age +
                           age_miss + 
                           female + 
                           kirkuk + 
                           nona_lived_in_kirkuk_1 + 
                           nona_lived_in_kirkuk_2 + 
                           lived_in_kirkuk_miss + 
                           nona_sunni + 
                           relig_miss, 
                           data = dat_d_di[dat_d_di$wave == 1, ],
                           family = "binomial")

prs_df <- data.frame(pr_score = predict(d_di_reg, type = "response"),
                     d_or_di = d_di_reg$model$d_or_di)
