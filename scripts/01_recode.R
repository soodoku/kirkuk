#
# Recode Kirkuk Data
# 

# set directory
setwd(githubdir)
setwd("kirkuk/")

# Load libs 
#library(readxl)
library(car)
library(goji)
library(dplyr)

# Read in the data
#read_excel("data/Raw Data-Final.xlsx")

t1    <- read.csv("data/sheet_1.csv")
t2_t3 <- read.csv("data/sheet_2.csv")

# Changes column names to lowercase
names(t1)    <- tolower(names(t1))
names(t2_t3) <- tolower(names(t2_t3))

# Combine the data
t1$wave    <- 1
t2_t3$wave <- 2

all_dat <- rbind(t1, t2_t3)

# Design
# ----------
all_dat$cond  <- NA
all_dat$cond[all_dat$id.manually %in% all_dat$id.manually[all_dat$dater == "18-Apr-17"]] <- "delib"
all_dat$cond[all_dat$id.manually %in% all_dat$id.manually[all_dat$dater == "03-May-17" | all_dat$dater == "05/03/17"]] <- "delib_info"
all_dat$cond[is.na(all_dat$cond)] <- "control"

# So 1 person vanished in DI. We will nuke that
table(all_dat$id.manually[all_dat$cond == "delib_info"])
# IDs 156 and 178
all_dat <- subset(all_dat, ! (id.manually %in% c(156)))

# Sociodem
# --------------

# Age 
all_dat$age     <- all_dat$a1

# Sex
all_dat$female  <- all_dat$a2 == "Female"

# Born
all_dat$born    <- all_dat$a3
all_dat$kirkuk  <- all_dat$born == "Karkuk"

# All my life – 1; Less than 1 year – 2; Less than 5 years – 3; Less than 10 years – 4; Refused/NA – 9
# The response options are technically incorrect. 
all_dat$lived_in_kirkuk <- car::recode(all_dat$a4, "1 = 1; 4 = 2; 3 = 3; 2 = 4; 9 = NA") 
all_dat$life_in_kirkuk  <- all_dat$lived_in_kirkuk == 1

# Religion
# Sunni – 1; Shia – 2; Catholic – 3; Other ______________________ – 4; I do not consider myself belonging to any religion – 5
# (Don’t know / Refused) – 9
all_dat$religion       <- car::recode(all_dat$a5, "1 = 'Sunni'; 2 = 'Shia'; 3 = 'Catholic'; 4 = 'Other'; 5 ='Atheist or Agnostic'; 9 = NA")

# Create dummies
all_dat$shia <- all_dat$religion == "Shia"
all_dat$sunni <- all_dat$religion == "Sunni"

# Kurd – 1; Arab – 2; Turkman – 3; Other ______________________ – 4; (Don’t know / Refused) – 9
all_dat$ethnicity   <- car::recode(all_dat$a6, "1 = 'Kurd'; 2 = 'Arab'; 3 = 'Turkman'; 4 = 'Other'; 9 = NA")

# Create dummies
all_dat$kurd <- all_dat$ethnicity == "Kurd"
all_dat$arab <- all_dat$ethnicity == "Arab"
all_dat$turk <- all_dat$ethnicity == "Turkman"

# Let's make it easy on ourselves and add ethnicity to t2_t3 also
# -----------------------------------------------------------------
all_dat$ethnicity[all_dat$wave == 2] <- 
    all_dat$ethnicity[match(all_dat$id.manually[all_dat$wave == 2], all_dat$id.manually[all_dat$wave == 1])]

# Ethnic Identity
all_dat$ethnic_id   <- car::recode(all_dat$a7, "1 = 'Iraqi first'; 2 = 'Kurdish first'; 3 = 'Arab first'; 4 = 'Turkoman first';
	                                            5 = 'Kurdish only'; 6 = 'Turkoman Only'; 7 = 'Arab only'; 8 = 'Other'; 9 = NA")

# A8. How often do you have contact with members of other ethnic groups?
# Never – 1; Seldom – 2; Regularly – 3; Often – 4; Very often – 5
all_dat$cross_ethnic   <- all_dat$a8

# A9 = Group Attitudes (0 = Negative, 1 = Positive)
# ---------------------------------------------------
# Naming convention: .a = arab, .k = kurd, .t = turkoman

to_vars   <- paste0(rep(c("regard", "trust", "reason"), each = 3), c("_a", "_k", "_t"))
from_vars <- c("a9a.a", "a9a.k", "a9a.t", "a9b.a", "a9b.k", "a9b.t", "a9c.a", "a9c.k", "a9c.t")
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "99 = NA"))) 

# Validation spot check 
table(all_dat$a9a.a, all_dat$regard_a, useNA = "always")

# Indices 
all_dat$avg_a <- rowMeans(all_dat[, c("regard_a", "trust_a", "reason_a")], na.rm = T)
all_dat$avg_k <- rowMeans(all_dat[, c("regard_k", "trust_k", "reason_k")], na.rm = T)
all_dat$avg_t <- rowMeans(all_dat[, c("regard_t", "trust_t", "reason_t")], na.rm = T)

# B1 = Satisfaction w/ Kirkuk Gov. (1 = Dissatisfied, 5 = Satisfied)
# -----------------------
all_dat$b1r <- zero1(car::recode(all_dat$b1, "9 = NA"))

# B2 = Evaluation of Gov. (1 = Strongly disagree, 5 = Strongly agree)
# b26 has 88 also
# ---------------------------
from_vars <- paste0("b2.", 1:9)
to_vars   <- paste0("b2_", 1:9)
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "c(88, 99) = NA")))

# Validation spot check 
table(all_dat$b2.1, all_dat$b2_1, useNA = "always")

# B3 (Biggest problem facing kirkuk)
# ----------------------------
table(all_dat$b3) # 12 is other but no string
table(all_dat$b4a) # 7 is other but no string
table(all_dat$b4b) 

# CA, B (Options; 0 = strongly oppose, 10 = strongly support)
# ------------------------------
from_vars <- paste0("ca.", 1:3)
to_vars   <- paste0("ca_", 1:3)
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "99 = NA")))

# Validation spot check 
table(all_dat$ca.1, all_dat$ca_1, useNA = "always")

from_vars <- paste0("cb.", 1:8)
to_vars   <- paste0("cb_", 1:8)
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "99 = NA; 100 = 10"))) 

# Validation spot check 
table(all_dat$cb.1, all_dat$cb_1, useNA = "always")

# Build indices using items from c1a and c1b

# Status quo
# A governorate under the authority of the federal government
# What matters most is how the country as a whole develops.
all_dat$fed <- with(all_dat, rowMeans(cbind(ca_1, cb_1, cb_8), na.rm = T))

# c1a2: A federal region in its own right, with an extended range of powers.
# c1b: It is important that Kirkuk should have as many powers as possible.
# c1b: It is important that decisions that only affect Kirkuk are made in Kirkuk (free from excessive influence from Bagdad or the KRG). 
# c1b: It is important that Kirkuk develops its own political identity.
# c1b: It is important that Kirkuk has equal standing within Iraq.
all_dat$ind_kirkuk <- with(all_dat, rowMeans(cbind(ca_2, cb_2, cb_3, cb_6, cb_7), na.rm = T))

# D1 = interest in politics (1 = Not at all; 4 = very)
# -------------------
all_dat$polint <- zero1(car::recode(all_dat$d1, "9 = NA"))

# D2 = pol. efficacy (0 = not at all, 10 = extremely so)
# 5, 6, 7 need to be reverse coded. But reverse coding leads to -ve correlation w/ others.
# Correlation b/w 1 and 5, 6, 7 super low. Generally 5, 6, 7 seem a f*** fest
# ---------------------------------------
from_vars <- paste0("d2.", 1:7)
to_vars   <- paste0("d2_", 1:7)
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "99 = NA")))

#from_vars <- paste0("d2.", 5:7)
#to_vars   <- paste0("d2_", 5:7)
#all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "99 = NA"), 10, 0))

# Validation spot check 
table(all_dat$d2.1, all_dat$d2_1, useNA = "always")

# Pol. Efficacy Index
all_dat$pol_efficacy <- rowMeans(all_dat[, to_vars])

# D3 = Trust in Gov. + Leaders (0 = No trust, 10 = Completely)
# ----------------------------
from_vars <- paste0("d3.", 1:14)
to_vars   <- paste0("d3_", 1:14)
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "99 = NA"))) 

# Kurds Trust: 
 # Governor (d3_1), KRG (d3_5), Prov. Council (d3_3), Dep. Gov. (d3_2)

all_dat$local_trust <- with(all_dat, rowMeans(cbind(d3_1, d3_2, d3_3, d3_5), na.rm = T))

# Central 
  # The Council of Representatives (d3_6), The Federal Government (d3_4), d3_7: The judiciary, 
  # d3_9: The army, 
all_dat$central_trust <- with(all_dat, rowMeans(cbind(d3_4, d3_6, d3_7, d3_9), na.rm = T))

# General or ambiguous: The police (d3_8), Politicians (d3_10), Political parties (d3_11),
# The Independent High Electoral Commission (d3_12), The media (d3_13), d3_14: UNAMI (United Nations Iraq) 

# Validation spot check 
table(all_dat$d3.1, all_dat$d3_1, useNA = "always")

# Trust Index
all_dat$trust_index <- rowMeans(all_dat[, to_vars])

# D4 = Political Action (1 = Yes, 2 = NO)
# -----------------------------
from_vars <- paste0("d4", letters[1:8])
to_vars   <- paste0("d4_", letters[1:8])
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) car::recode(x, "9 = NA; 2 = 0")) 

# Validation spot check 
table(all_dat$d4a, all_dat$d4_a, useNA = "always")

# D5 = Goals (1 = Most important, 5 = Least important; not choosing = not in top 5)
# -----------------------------------
# Analyze as proportion choosing most important (flipped scaled, so = 1 now)
# d5.5 has entry = 41; one or two people ranking all 13
from_vars <- paste0("d5.", 1:13)
to_vars   <- paste0("d5_", 1:13)
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(ifelse(x > 5, NA, x), 5, 1)) 

# Validation spot check
table(all_dat$d5.5, all_dat$d5_5, useNA = "always")

# Delib. capacity (1 = Not at all, 5 = Extremely)
# ------------------------------------------
from_vars <- paste0("d6a.", 1:5)
to_vars   <- paste0("d6a_", 1:5)
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "99 = NA")))

# Validation spot check
table(all_dat$d6a.1, all_dat$d6a_1, useNA = "always")

# Index for society's delib. capacity
all_dat$soc_delib_capacity <- rowMeans(all_dat[, to_vars])

from_vars <- paste0("d6b.", 1:7)
to_vars   <- paste0("d6b_", 1:7)
all_dat[, to_vars] <- lapply(all_dat[, from_vars], function(x) zero1(car::recode(x, "99 = NA"))) 

# Issues w/ items
# 1. d6b_6 is a weird q: How valid would you say your opinion is in any situation?
# cor(all_dat[, to_vars], use = "na.or.complete") w/ other items also low
# 2. d6b_5 should be reverse coded: How unsettled do you feel when someone challenges your views during a conversation?
# But positively correlated w/ other items. So sigh.  
all_dat$d6b_5 <- zero1(all_dat$d6b_5, 1, 0)

# Validation spot check
table(all_dat$d6b.1, all_dat$d6b_1, useNA = "always")

# Index for own's delib. capacity
# Take out 6 for sure and 5 also as responses seem weird
all_dat$own_delib_capacity <- rowMeans(all_dat[, to_vars[c(1:4, 7)]])

# Own - Society
all_dat$own_soc_diff_delib_capacity <- all_dat$own_delib_capacity - all_dat$soc_delib_capacity

# Knowledge
# -----------------
all_dat$know1 <- nona(all_dat$e1 == 3)
all_dat$know2 <- nona(all_dat$e2 == 4)
all_dat$know3 <- nona(all_dat$e3 == 3)
all_dat$know4 <- nona(all_dat$e4 == 2)
all_dat$know5 <- nona(all_dat$e5 == 2)

all_dat$know  <- with(all_dat, rowMeans(cbind(know1, know2, know3, know4, know5)))


# Ethnocentrism
# -------------------------

# Long to wide
wall_dat <- all_dat %>% 
  filter(wave == "1") %>%
  left_join(., filter(all_dat, wave == "2"), by = "id.manually", suffix = c("_t1", "_t2"))

# Ethnocentrism
# ------------------
kurd <- !is.na(wall_dat$ethnicity_t1) & wall_dat$ethnicity_t1 == "Kurd"
arab <- !is.na(wall_dat$ethnicity_t1) & wall_dat$ethnicity_t1 == "Arab"
turk <- !is.na(wall_dat$ethnicity_t1) & wall_dat$ethnicity_t1 == "Turkman"

wall_dat$ethnocentrism_t1 <- NA 
wall_dat$ethnocentrism_t1[kurd] <- with(wall_dat[kurd, ], avg_k_t1 - rowMeans(cbind(avg_a_t1, avg_t_t1)))
wall_dat$ethnocentrism_t1[arab] <- with(wall_dat[arab, ], avg_a_t1 - rowMeans(cbind(avg_k_t1, avg_t_t1)))
wall_dat$ethnocentrism_t1[turk] <- with(wall_dat[turk, ], avg_t_t1 - rowMeans(cbind(avg_a_t1, avg_k_t1)))

wall_dat$ethnocentrism_t2 <- NA 
wall_dat$ethnocentrism_t2[kurd] <- with(wall_dat[kurd, ], avg_k_t2 - rowMeans(cbind(avg_a_t2, avg_t_t2)))
wall_dat$ethnocentrism_t2[arab] <- with(wall_dat[arab, ], avg_a_t2 - rowMeans(cbind(avg_k_t2, avg_t_t2)))
wall_dat$ethnocentrism_t2[turk] <- with(wall_dat[turk, ], avg_t_t2 - rowMeans(cbind(avg_a_t2, avg_k_t2)))
