#load package
pacman::p_load(haven, tidyverse, readstata13, stats,
               lmtest, sandwich, clubSandwich, stargazer, estimatr, 
               broom, huxtable)

data <- read_dta("C:/Users/Owner/Indian-cycling-B/replication data/113676-V1/dlhs-reg-data.dta")

data_fil <- data %>% 
  filter(
    !is.na(bihar)
  )

demographic <- c("sc", "st", "obc", "hindu", "muslim")
household <- c("hhheadschool", "hhheadmale", "land", "bpl", "media", "electricity")
village <- c("middle", "bank", "postoff", "lcurrpop")
dist <- c("busdist", "towndist", "railwaydist", "hqdist")

#Model 1
a1 <- lm_robust(enrollment_secschool ~ treat1_female_bihar +
              treat1_female + treat1_bihar + female_bihar + treat1 +
              female + bihar, 
              data = data_fil, 
              weights = hhwt,
              clusters = village)

# Model 2
a2 <- lm_robust(as.formula(paste("enrollment_secschool ~ treat1_female_bihar + treat1_female + 
               treat1_bihar + female_bihar + treat1 + female + bihar +", 
               paste(demographic, collapse = "+"))),
               data = data_fil,
               weights = hhwt,
               clusters = village)

# Model 3
a3 <- lm_robust(as.formula(paste("enrollment_secschool ~ treat1_female_bihar + treat1_female + 
               treat1_bihar + female_bihar + treat1 + female + bihar +", 
               paste(c(demographic, household), collapse = "+"))),
               data = data_fil,
               weights = hhwt,
               clusters = village)

# Model 4
a4 <- lm_robust(as.formula(paste("enrollment_secschool ~ treat1_female_bihar + treat1_female + 
               treat1_bihar + female_bihar + treat1 + female + bihar +", 
               paste(c(demographic, household, village, dist), collapse = "+"))),
               data = data_fil,
               weights = hhwt,
               clusters = village)

#replicate table 2
huxreg(a1, a2, a3, a4)

stargazer(a1, a2, a3, a4, type="text", align=TRUE, digits=3, 
          dep.var.labels = "Enroll")


