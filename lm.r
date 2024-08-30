### Package
# Work Flow
library(here)
library(estimatr)
library(readr)
library(openxlsx)
# Cleaning
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)


df <- tibble(
  y = seq(1, 10, 1),
  x_1 = rnorm(10),
  x_2 = rnorm(10),
  x_3 = rnorm(10)
)

list_covs <- c("x_1", "x_2", "x_3")

lm_fnc <- function(list_covs, df) {

  covs <- paste(list_covs, collapse = " + ")
  formula_i <- paste0("y ~", covs)

  lm_output <- lm_robust(as.formula(formula_i), data = df)

  return(lm_output)
}

lm_fnc(c("x_2", "x_3"), df)
lm_fnc(c("x_1", "x_2"), df)
lm_fnc(c("x_1", "x_2"), df)


