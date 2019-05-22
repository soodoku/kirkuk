#
# Delib. in Kirkuk
# Attitudes, Opinions etc.
# 
# 1. list-wise delete missing only on the current wave
# 2. list-wise delete missing on any of the 2 waves (Table 3, 4, C1)
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

# All the vars
trust_me   <- paste0(rep(c("regard", "trust", "reason"), each = 3), c("_a", "_k", "_t"))
b2   <- paste0("b2_", 1:9)
ca   <- paste0("ca_", 1:3)
cb   <- paste0("cb_", 1:8)
d2   <- c(paste0("d2_", 1:7),  "pol_efficacy")
d3   <- c(paste0("d3_", 1:14), "trust_index")
d4   <- paste0("d4_", letters[1:8])
d5   <- paste0("d5_", 1:13)
d6a  <- c(paste0("d6a_", 1:5), "soc_delib_capacity")
d6b  <- c(paste0("d6b_", 1:7), "own_delib_capacity")

indices <- c("fed", "ind_kirkuk", "local_trust", "central_trust")

# Recode/Only 1st priority
all_dat[, d5] <- all_dat[, d5] == 1

# List of vars
att_cond_wave <- c(trust_me, b2, ca, cb, d2, d3, d5, d6a, d6b, indices, "cond", "wave")

# 1. list-wise delete missing only on the current wave
# ------------------------------------------------------

att_op <- 
    all_dat[, att_cond_wave] %>%
    group_by(cond, wave) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))

# Get Condition/Wave concat
att_op$cond <- paste0(att_op$cond, att_op$wave)

# Transpose
att_op_t <- att_op %>%
    gather(key = var_name, value = value, 2:85) %>% 
    spread_(key = "cond", value = "value")

att_op_t$diff_delib       <- att_op_t$delib2 - att_op_t$delib1
att_op_t$diff_delib_info  <- att_op_t$delib_info2 - att_op_t$delib_info1

# Read in Labels
lab_cats <- read.csv("data/qno_lab_cats.csv")

wave_wise_res <- att_op_t %>%
    left_join(lab_cats, by = "var_name") %>%
    filter(var_name != "wave") 

# Pooled t1
# -----------------------------

att_op_t1 <- all_dat[, att_cond_wave] %>%
    select(-cond) %>%
    filter(wave == 1) %>%
    group_by(wave) %>%
    summarise_all(funs(mean(., na.rm = TRUE))) %>%
    select(-wave)

att_op_t1_t <- as.data.frame(t(att_op_t1))
att_op_t1_t$var_name <- rownames(att_op_t1_t)

t1_res <- att_op_t1_t %>%
    left_join(lab_cats, by = "var_name") %>%
    rename(t1_pooled = V1) %>%
    select(-c(lab, cat, tab_order))

# Ethnocentrism (uses wide data)
# -------------------------------
round(mean(wall_dat$ethnocentrism_t1, na.rm = T), 3)
round(mean(wall_dat$ethnocentrism_t1[wall_dat$cond_t1 == "control"], na.rm = T), 3)
round(mean(wall_dat$ethnocentrism_t1[wall_dat$cond_t1 == "delib"], na.rm = T), 3)
round(mean(wall_dat$ethnocentrism_t2[wall_dat$cond_t1 == "delib"], na.rm = T), 3)

round(mean(wall_dat$ethnocentrism_t1[wall_dat$cond_t1 == "delib_info"], na.rm = T), 3)
round(mean(wall_dat$ethnocentrism_t2[wall_dat$cond_t1 == "delib_info"], na.rm = T), 3)

with(wall_dat[wall_dat$cond_t1 == "delib", ], t.test(ethnocentrism_t2, ethnocentrism_t1, paired = TRUE))
with(wall_dat[wall_dat$cond_t1 == "delib_info", ], t.test(ethnocentrism_t2, ethnocentrism_t1, paired = TRUE))

# 2. Table 3: list-wise delete missing on any one of the waves
# -------------------------------------------------------

tee_1 <-  paste0(lab_cats[, 1], "_t1")
tee_2 <-  paste0(lab_cats[, 1], "_t2")

# No NA on either wave/List-wise deletion estimates
res_nona <- data.frame(var_name = NA, t1_delib_nona = NA, t2_delib_nona = NA, t1_di_nona = NA, t2_di_nona = NA)

for (i in 1:length(tee_1)) {
    
    nona_delib      <- !is.na(wall_dat[wall_dat$cond_t1 == "delib", tee_1[i]]) & !is.na(wall_dat[wall_dat$cond_t1 == "delib", tee_2[i]])
    nona_delib_info <- !is.na(wall_dat[wall_dat$cond_t1 == "delib_info", tee_1[i]]) & !is.na(wall_dat[wall_dat$cond_t1 == "delib_info", tee_2[i]])

    t1_delib <- mean(wall_dat[wall_dat$cond_t1 == "delib", tee_1[i]][nona_delib], na.rm = T)
    t2_delib <- mean(wall_dat[wall_dat$cond_t1 == "delib", tee_2[i]][nona_delib], na.rm = T)

    t1_delib_i <- mean(wall_dat[wall_dat$cond_t1 == "delib_info", tee_1[i]][nona_delib_info], na.rm = T)
    t2_delib_i <- mean(wall_dat[wall_dat$cond_t1 == "delib_info", tee_2[i]][nona_delib_info], na.rm = T)

    res_nona[i, ] <- c(lab_cats[i, 1], t1_delib, t2_delib, t1_delib_i, t2_delib_i)

}

# No NA p-values & estimate of diff.
# -----------------------------------
diff_delib  <- wall_dat[wall_dat$cond_t1 == "delib", tee_2] - wall_dat[wall_dat$cond_t1 == "delib", tee_1]
diff_delib  <- subset(diff_delib, select = !(names(diff_delib) %in% c(paste0(d4, "_t2"), paste0(d5[10:13], "_t2"))))
res_delib   <- do.call(rbind, lapply(diff_delib, function(x) tidy(t.test(x, mu = 0))))
names(res_delib) <- paste0(names(res_delib), "_d")
res_delib$var_name <- gsub("_t2", "", rownames(res_delib))

diff_delib_info  <- wall_dat[wall_dat$cond_t1 == "delib_info", tee_2] - wall_dat[wall_dat$cond_t1 == "delib_info", tee_1]
diff_delib_info  <- subset(diff_delib_info, select = !(names(diff_delib_info) %in% c(paste0(d4, "_t2"), paste0(d5[10:13], "_t2"))))
res_delib_info   <- do.call(rbind, lapply(diff_delib_info, function(x) tidy(t.test(x, mu = 0))))
names(res_delib_info) <- paste0(names(res_delib_info), "_di")
res_delib_info$var_name_di <- gsub("_t2", "", rownames(res_delib_info))

# Select cols
res_delib      <- res_delib[, c("estimate_d", "p.value_d", "var_name")]
res_delib_info <- res_delib_info[, c("estimate_di", "p.value_di", "var_name_di")]

# Append
all_res <- t1_res %>%
    left_join(res_nona, by = "var_name") %>%
    left_join(res_delib, by = "var_name") %>%
    left_join(res_delib_info, by = c("var_name" =  "var_name_di")) %>%
    left_join(wave_wise_res, by = "var_name") %>%
    filter(! (var_name %in% c("d6b_5", "d6b_6")))


all_res <- all_res[, c("var_name", "lab", "cat", "tab_order", "t1_pooled", 
                       "t1_delib_nona", "t2_delib_nona", "estimate_d", "p.value_d",
                       "t1_di_nona", "t2_di_nona", "estimate_di", "p.value_di",
                       "delib1", "delib2", "diff_delib",
                       "delib_info1", "delib_info2", "diff_delib_info")]

all_res <- all_res[order(all_res$tab_order), ]

write.csv(all_res, file = "tabs/03_table_3_4_append_c1.csv", row.names = F)
