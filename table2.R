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
tab2 <- huxreg(a1, a2, a3, a4, coefs = c("Treat*female*Bihar" = "treat1_female_bihar",
                                 "Treat*female" = "treat1_female",
                                 "Treat*Bihar" = "treat1_bihar",
                                 "Female*Bihar" = "female_bihar",
                                 "Treat" = "treat1",
                                 "Female" = "female",
                                 "Bihar" = "bihar",
                                 "Constant" = "(Intercept)"),
       omit_coefs = c("sc", "st", "obc", "hindu", "muslim", 
                      "hhheadschool", "hhheadmale", "land", "bpl", "media", "electricity",
                      "middle", "bank", "postoff", "lcurrpop",
                      "busdist", "towndist", "railwaydist", "hqdist"),
       stars=NULL,
       digits=3
)

demorow <- c("Demographic controls", "NO", "YES", "YES", "YES")
hhrow <- c("HH socioeconomic controls", "NO", "NO", "YES", "YES")
vilrow <- c("Village level controls", "NO", "NO", "NO", "YES")

demorow <- data.frame(demorow)
hhrow <- data.frame(hhrow)
vilrow <- data.frame(vilrow)

# transpose
demorow <- t(demorow)
hhrow <- t(hhrow)
vilrow <- t(vilrow)

# convert to data frame
demorow <- data.frame(demorow)
hhrow <- data.frame(hhrow)
vilrow <- data.frame(vilrow)

rows <- bind_rows(demorow, hhrow, vilrow)
colnames(rows) <- colnames(tab2) 
tab2_rows <- rbind(tab2, rows)
class(tab2)







, hhrow, vilrow

