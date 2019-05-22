#
# Kirkuk
# 

# set directory
setwd(githubdir)
setwd("kirkuk/")

# Read in the data
t1 <- read.csv("data/sheet_1.csv")
t2_t3 <- read.csv("data/sheet_2.csv")

names(t1)    <- tolower(names(t1))
names(t2_t3) <- tolower(names(t2_t3))

# D
# ----------
t2_delib      <- subset(t2_t3, dater == "18-Apr-17")
t1_delib      <- t1[intersect(t1$id.manually, t2_delib$id.manually), ]

# DI 
# -------------
#  129 to 178 (or IDs 141 to 190); n = 50
t1_delib_info <- subset(t1, dater == "05/03/17")
# n = 49
t2_delib_info <- subset(t2_t3, dater == "03-May-17")

# length(unique(t1_delib_info$id.manually))
# [1] 50
# > length(unique(t2_delib_info$id.manually))
# [1] 48

# No Treat 
# ------------
control       <- subset(t1, !(t1$id.manually %in% intersect(t1$id.manually, t2_delib$id.manually)))
control       <- subset(control, !(control$id.manually %in% intersect(control$id.manually, t1_delib_info$id.manually)))

# Recode
# --------------
# Q.C1a, Q.C1b
t1$cb.1r    <- car:recode(t1$cb.1, "99 = NA") 
t2_t3$cb.1r <- car:recode(t2_t3$cb.1, "99 = NA")

# Spot checking results
mean(t2_delib$e1 == 3)
