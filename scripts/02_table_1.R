#
# Delib. in Kirkuk
# Table 1
# 

# set directory
setwd(githubdir)
setwd("kirkuk/")

# Load libs 
library(tidyr)
library(dplyr)
library(broom)
library(reshape2)

# Read in the data
source("scripts/01_recode.R")

# Subset on t1
t1_dat <- subset(all_dat, wave == 1)

socio_dem_l <- c("age", "female", "kirkuk", "life_in_kirkuk", "shia", "sunni", "kurd", "arab", "turk", "cross_ethnic")
d4   <- paste0("d4_", letters[1:8])

# Means
socio_dem <- t1_dat[, c(socio_dem_l, d4, "cond")] %>%
    group_by(cond) %>%
    summarise_all(funs(mean(., na.rm = TRUE)))

# Transpose
socio_dem_t <- socio_dem %>%
    gather(key = var_name, value = value, 2:19) %>% 
    spread_(key = names(socio_dem)[1], value = 'value')

socio_dem_t$diff_d_c   <- socio_dem_t$delib - socio_dem_t$control
socio_dem_t$diff_di_d  <- socio_dem_t$delib_info - socio_dem_t$delib

# Pooled T1
socio_dem_t1_pooled <- t1_dat[, c(socio_dem_l, d4)] %>%
    summarise_all(funs(mean(., na.rm = TRUE))) %>%
    melt(variable.name = "var_name",
    	 value.name = "t1_pooled")

# Merge t1 pooled and other results
socio_dem_all <- socio_dem_t1_pooled %>%
    left_join(socio_dem_t)

# formal tests for DC
dc <- subset(t1_dat, cond %in% c("delib", "control"))
pdc <- do.call(rbind, lapply(dc[, c(socio_dem_l, d4)], function(x) tidy(t.test(x ~ dc$cond))))
pdc$var_name <- rownames(pdc)

# formal tests for DIC
dic <- subset(t1_dat, cond %in% c("delib_info", "delib"))
pdic <- do.call(rbind, lapply(dic[, c(socio_dem_l, d4)], function(x) tidy(t.test(x ~ dic$cond))))
pdic$var_name <- rownames(pdic)

# Just add p-vals for now
socio_dem_all_p <- socio_dem_all %>%
    left_join(pdc[, c("var_name", "p.value")]) %>%
    left_join(pdic[, c("var_name", "p.value")], by = c("var_name" = "var_name"))

# Write
write.csv(socio_dem_all_p, file = "tabs/01_table_1_sociodem.csv", row.names = F)
