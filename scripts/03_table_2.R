#
# Delib. in Kirkuk
# Table 2
# 

# set directory
setwd(githubdir)
setwd("kirkuk/")

# Load libs 
library(tidyr)
library(dplyr)
library(reshape2)
library(broom)

# Read in the data
source("scripts/01_recode.R")

# Table 2: Knowledge
# ----------------------
know <- paste0("know", 1:5)

know_all <- all_dat[, c(know, "know", "cond", "wave")] %>%
    group_by(cond, wave) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))

# Get Condition/Wave concat
know_all$cond <- paste0(know_all$cond, know_all$wave)

# Transpose
know_all_t <- know_all %>%
    gather(key = var_name, value = value, 2:8) %>% 
    spread_(key = names(know_all)[1], value = "value") %>%
    filter(var_name != "wave")

know_all_t$diff_delib       <- know_all_t$delib2 - know_all_t$delib1
know_all_t$diff_delib_info  <- know_all_t$delib_info2 - know_all_t$delib_info1

# Pooled t1
know_t1_pooled <- all_dat[, c(know, "know", "wave")] %>%
    group_by(wave) %>%
    filter(wave == 1) %>%
    summarise_all(funs(mean(., na.rm = TRUE))) %>%
    melt(variable.name = "var_name",
    	 value.name = "t1_pooled")

# Merge t1 pooled and other results
know_all <- know_t1_pooled %>%
    left_join(know_all_t) %>%
    filter(var_name != "wave")

# p-values (no missing issue as missing = 0)
# ---------------------------------------------

tee_1 <-  paste0(c(know, "know"), "_t1")
tee_2 <-  paste0(c(know, "know"),"_t2")

diff_delib  <- wall_dat[wall_dat$cond_t1 == "delib", tee_2] - wall_dat[wall_dat$cond_t1 == "delib", tee_1]
diff_delib  <- subset(diff_delib, select = tee_2)
res_delib   <- do.call(rbind, lapply(diff_delib, function(x) tidy(t.test(x, mu = 0))))
names(res_delib) <- paste0(names(res_delib), "_d")
res_delib$var_name <- gsub("_t2", "", rownames(res_delib))
res_delib <- subset(res_delib, select = c("var_name", "estimate_d", "p.value_d"))

diff_delib_info  <- wall_dat[wall_dat$cond_t1 == "delib_info", tee_2] - wall_dat[wall_dat$cond_t1 == "delib_info", tee_1]
diff_delib_info  <- subset(diff_delib_info, select = tee_2)
res_delib_info   <- do.call(rbind, lapply(diff_delib_info, function(x) tidy(t.test(x, mu = 0))))
names(res_delib_info) <- paste0(names(res_delib_info), "_di")
res_delib_info$var_name_di <- gsub("_t2", "", rownames(res_delib_info))
res_delib_info <- subset(res_delib_info, select = c("var_name_di", "estimate_di", "p.value_di"))

tab_2 <- know_all %>% 
    left_join(res_delib) %>%
    left_join(res_delib_info, by = c("var_name" =  "var_name_di"))

tab_2_col_order <- c("var_name", "t1_pooled", "control1",
                     "delib1", "delib2", "diff_delib", "p.value_d", "estimate_d",
                     "delib_info1", "delib_info2", "diff_delib_info", "p.value_di", "estimate_di")               

tab_2 <- tab_2[, tab_2_col_order]

write.csv(tab_2, file = "tabs/02_table_2_know.csv", row.names = F)
