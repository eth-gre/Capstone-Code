# ECOM30002/90002 Econometrics 2
# Semester 2, 2026
# Capstone Project Sample R Code: Data Processing and Analysis

# This code is based on an illustrative research question examining the
# effect of the number of siblings on school attendance.
#
# The research design, variable construction, and empirical models are
# deliberately simplified and do not meet the requirements of the Capstone
# Project. In particular, the example should not be treated as a suitable
# research question or empirical strategy for your own project.
#
# The sole purpose of this code is to illustrate some basic data-processing
# and econometric procedures using the provided data set.
#
# This script contains two independent sections:
#   - Section 1 uses one survey wave as cross-sectional data.
#   - Section 2 uses all four survey waves as panel data.
# Use only the section relevant to your project. The two sections are
# independent and each starts from the original data files. You may delete
# or ignore the section that is not relevant to your analysis.
#
# You must adapt the data files, variables, data-processing decisions,
# sample restrictions, and empirical models to your own research question.
# You must understand every line of code that you submit.


# Clear workspace
rm(list = ls())


# Install and load packages

# Install each package once if it is not already installed.
# Remove the # at the beginning of a line to install that package.
# install.packages("AER")        # for instrumental-variable estimation
# install.packages("stargazer")  # for formatted tables
# install.packages("plm")        # for panel-data models

library(AER)
library(stargazer)
library(plm)

# Section 1: Cross-sectional analysis using one survey wave

# The variable round identifies the survey wave:
#   - round = 1: Wave 1, 2008–2009
#   - round = 2: Wave 2, 2010–2011
#   - round = 3: Wave 3, 2012–2013
#   - round = 4: Wave 4, 2014–2015
# Select one wave and exclude observations from the other three waves.

# Load data files

# Household Member Roster
upd4_hh_b <- read.csv("data/upd4_hh_b.csv")
# Variables used from this file:
# UPI:    Uniform Panel Identifier
# round:  Survey wave: NPSY1, NPSY2, NPSY3 or NPSY4
# r_hhid: Household identifier
# r_id:   Individual identifier within the household
# hb_04:  Age

# Education
upd4_hh_c <- read.csv("data/upd4_hh_c.csv")
# Variables used from this file:
# UPI:    Uniform Panel Identifier
# round:  Survey wave: NPSY1, NPSY2, NPSY3 or NPSY4
# r_hhid: Household identifier
# r_id:   Individual identifier within the household
# hc_05:  Currently attending school


# Select one survey wave

# Choose the survey wave for the cross-sectional analysis:
# 1 = NPSY1, 2 = NPSY2, 3 = NPSY3, 4 = NPSY4
wave_for_use <- 4

# Remove observations from the other survey waves
upd4_hh_b <- upd4_hh_b[upd4_hh_b$round == wave_for_use, ]
upd4_hh_c <- upd4_hh_c[upd4_hh_c$round == wave_for_use, ]


# Create the working data frame

# Obtain the unique individual identifiers in the selected wave
id <- unique(upd4_hh_b$UPI)

# Number of individuals in the selected wave
N <- length(id)

# Create a data frame containing one row for each individual
df <- data.frame(id)

# Initialise the variables to be added to the working data frame
df$hhid <- NA              # Household identifier
df$age <- NA               # Individual's age
df$school <- NA            # Whether the individual is currently attending school
df$school_age <- NA        # Indicator for whether the individual is of school age
df$other_children_no <- NA # Number of other household members aged 15 or below

# Combine variables from the two data files

# For each individual, locate the corresponding observation in each
# original data file and copy the required variables into df
for (i in 1:N) {
  
  # Individual identifier
  id_i <- df$id[i]
  
  # Locate individual i in the Household Member Roster
  row_b <- which(upd4_hh_b$UPI == id_i)
  
  # Obtain the household identifier
  hhid_i <- upd4_hh_b$r_hhid[row_b]
  
  # Locate the same observation in the Education file
  row_c <- which(upd4_hh_c$UPI == id_i & upd4_hh_c$r_hhid == hhid_i)
  
  # Copy the required variables into the working data frame
  df$hhid[i] <- hhid_i
  df$age[i] <- upd4_hh_b$hb_04[row_b]
  df$school[i] <- upd4_hh_c$hc_05[row_c]
  
  # Identify whether the individual is aged between 7 and 15
  if (is.na(df$age[i])) {
    df$school_age[i] <- NA
  } else if (df$age[i] >= 7 & df$age[i] <= 15) {
    df$school_age[i] <- 1
  } else {
    df$school_age[i] <- 0
  }
  
  # Obtain the ages of other members of the same household
  other_ages <- upd4_hh_b$hb_04[
    upd4_hh_b$r_hhid == hhid_i &
      upd4_hh_b$UPI != id_i
  ]
  
  # Count other household members aged 15 or below
  df$other_children_no[i] <- sum(other_ages <= 15)
  
}

# Retain individuals of school age
df <- df[df$school_age == 1, ]

# Convert school attendance into a dummy variable
# 1 = currently attending school
# 0 = not currently attending school
df$school[df$school == "YES"] <- 1
df$school[df$school == "NO"] <- 0
df$school[df$school == ""] <- NA
df$school <- as.numeric(as.character(df$school))

# Remove observations with missing values in the variables used in the analysis
df <- df[complete.cases(df[, c("school", "age", "other_children_no")]), ]


# Produce summary statistics

# Select and rename the variables included in the regression
df_summary <- df[, c("school", "other_children_no", "age")]

colnames(df_summary) <- c(
  "School attendance",
  "Other children in household",
  "Age"
)

# Number of observations in the analysis sample
N_analysis <- nrow(df_summary)

# Produce the summary statistics table
stargazer(
  df_summary,
  type = "text",
  title = "Table 1: Summary Statistics",
  summary.stat = c("mean", "sd", "min", "max"),
  digits = 3,
  notes = paste(
    "The analysis sample contains",
    N_analysis,
    "observations."
  ),
  notes.append = FALSE
)


# Estimate the OLS model

eq_ols <- lm(school ~ other_children_no + age, data = df)

# Calculate heteroskedasticity-robust standard errors
se_ols <- sqrt(diag(vcovHC(eq_ols, type = "HC1")))

# Produce the regression results table
stargazer(
  eq_ols,
  type = "text",
  title = "Table 2: OLS Regression Results",
  dep.var.labels = "School attendance",
  covariate.labels = c(
    "Other children in household",
    "Age"
  ),
  se = list(se_ols),
  digits = 3,
  notes = "Heteroskedasticity-robust standard errors are reported in parentheses.",
  notes.append = TRUE
)



# Section 2: Panel-data analysis using all four survey waves

# This section starts again from the original data files and does not use
# any data frames created in Section 1. Retain observations from all four
# survey waves for the panel-data analysis.

# Clear workspace
rm(list = ls())


# Load data files

# Household Member Roster
upd4_hh_b <- read.csv("upd4_hh_b.csv")
# Variables used from this file:
# UPI:    Uniform Panel Identifier
# round:  Survey wave: NPSY1, NPSY2, NPSY3 or NPSY4
# r_hhid: Household identifier
# r_id:   Individual identifier within the household
# hb_04:  Age

# Education
upd4_hh_c <- read.csv("upd4_hh_c.csv")
# Variables used from this file:
# UPI:    Uniform Panel Identifier
# round:  Survey wave: NPSY1, NPSY2, NPSY3 or NPSY4
# r_hhid: Household identifier
# r_id:   Individual identifier within the household
# hc_05:  Currently attending school


# Create the working panel data frame

# Obtain the unique individual-wave combinations
id_wave <- unique(upd4_hh_b[, c("UPI", "round")])

# Number of individual-wave observations
NT <- nrow(id_wave)

# Create a data frame containing one row for each individual in each wave
df <- data.frame(
  id = id_wave$UPI,
  wave = id_wave$round
)

# Initialise the variables to be added to the working data frame
df$hhid <- NA              # Household identifier
df$age <- NA               # Individual's age
df$school <- NA            # Whether the individual is currently attending school
df$school_age <- NA        # Indicator for whether the individual is of school age
df$other_children_no <- NA # Number of other household members aged 15 or below


# Combine variables from the two data files

# For each individual-wave observation, locate the corresponding observation
# in each original data file and copy the required variables into df
for (it in 1:NT) {
  
  # Individual identifier and survey wave
  id_i <- df$id[it]
  wave_i <- df$wave[it]
  
  # Locate individual i in wave t in the Household Member Roster
  row_b <- which(
    upd4_hh_b$UPI == id_i &
      upd4_hh_b$round == wave_i
  )
  
  # Obtain the household and within-household identifiers
  hhid_i <- upd4_hh_b$r_hhid[row_b]
  
  # Locate the same observation in the Education file
  row_c <- which(
    upd4_hh_c$UPI == id_i &
      upd4_hh_c$round == wave_i &
      upd4_hh_c$r_hhid == hhid_i
  )
  
  # Copy the required variables into the working data frame
  df$hhid[it] <- hhid_i
  df$age[it] <- upd4_hh_b$hb_04[row_b]
  df$school[it] <- upd4_hh_c$hc_05[row_c]
  
  # Identify whether the individual is aged between 7 and 15
  if (is.na(df$age[it])) {
    df$school_age[it] <- NA
  } else if (df$age[it] >= 7 & df$age[it] <= 15) {
    df$school_age[it] <- 1
  } else {
    df$school_age[it] <- 0
  }
  
  # Obtain the ages of other members of the same household and survey wave
  other_ages <- upd4_hh_b$hb_04[
    upd4_hh_b$round == wave_i &
      upd4_hh_b$r_hhid == hhid_i &
      upd4_hh_b$UPI != id_i
  ]
  
  # Count other household members aged 15 or below
  df$other_children_no[it] <- sum(other_ages <= 15)
}

# Retain individuals of school age
df <- df[df$school_age == 1, ]

# Convert school attendance into a dummy variable
# 1 = currently attending school
# 0 = not currently attending school
df$school[df$school == "YES"] <- 1
df$school[df$school == "NO"] <- 0
df$school[df$school == ""] <- NA
df$school <- as.numeric(as.character(df$school))

# Remove observations with missing values in the variables used in the analysis
df <- df[complete.cases(df[, c("school", "age", "other_children_no")]), ]


# Select survey waves and construct the balanced analysis panel

# Select the survey waves to include
waves_for_use <- c(1, 2, 3)

# Retain observations from the selected survey waves
df <- df[df$wave %in% waves_for_use, ]

# Obtain the unique individual-wave combinations remaining in the sample
id_wave <- unique(df[, c("id", "wave")])

# Count the number of selected waves in which each individual appears
wave_count <- aggregate(
  wave ~ id,
  data = id_wave,
  FUN = length
)

# Identify individuals who appear in every selected wave
balanced_id <- wave_count$id[
  wave_count$wave == length(waves_for_use)
]

# Retain individuals who appear in every selected wave
df <- df[df$id %in% balanced_id, ]

# Sort observations by individual and survey wave
df <- df[order(df$id, df$wave), ]

# Reset the row numbers
rownames(df) <- NULL

# Check the dimensions of the balanced panel
N_panel <- length(unique(df$id))
T_panel <- length(unique(df$wave))
NT_panel <- nrow(df)


# Produce summary statistics

# Select and rename the variables included in the regression
df_summary <- df[, c("school", "other_children_no", "age")]

colnames(df_summary) <- c(
  "School attendance",
  "Other children in household",
  "Age"
)

# Number of individuals, waves and individual-wave observations
N_panel <- length(unique(df$id))
T_panel <- length(unique(df$wave))
NT_panel <- nrow(df)

# Produce the summary statistics table
stargazer(
  df_summary,
  type = "text",
  title = "Table 1: Summary Statistics",
  summary.stat = c("mean", "sd", "min", "max"),
  digits = 3,
  notes = paste(
    "The balanced panel contains",
    N_panel,
    "individuals observed over",
    T_panel,
    "survey waves, giving",
    NT_panel,
    "individual-wave observations."
  ),
  notes.append = FALSE
)


# Estimate pooled OLS and fixed-effects models

# Declare the panel-data structure
dtp <- pdata.frame(
  df,
  index = c("id", "wave")
)

# Pooled OLS
eq_pooled <- plm(
  school ~ other_children_no + age,
  data = dtp,
  model = "pooling"
)

# Individual fixed effects
eq_individual_fe <- plm(
  school ~ other_children_no + age,
  data = dtp,
  model = "within",
  effect = "individual"
)

# Time fixed effects
eq_time_fe <- plm(
  school ~ other_children_no + age,
  data = dtp,
  model = "within",
  effect = "time"
)

# Individual and time fixed effects
eq_twoway_fe <- plm(
  school ~ other_children_no + age,
  data = dtp,
  model = "within",
  effect = "twoways"
)


# Calculate heteroskedasticity-robust standard errors clustered by individual
se_pooled <- sqrt(diag(vcovHC(
  eq_pooled,
  method = "arellano",
  type = "HC1",
  cluster = "group"
)))

se_individual_fe <- sqrt(diag(vcovHC(
  eq_individual_fe,
  method = "arellano",
  type = "HC1",
  cluster = "group"
)))

se_time_fe <- sqrt(diag(vcovHC(
  eq_time_fe,
  method = "arellano",
  type = "HC1",
  cluster = "group"
)))

se_twoway_fe <- sqrt(diag(vcovHC(
  eq_twoway_fe,
  method = "arellano",
  type = "HC1",
  cluster = "group"
)))


# Produce the regression results table
stargazer(
  eq_pooled,
  eq_individual_fe,
  eq_time_fe,
  eq_twoway_fe,
  type = "text",
  title = "Table 2: Panel Regression Results",
  column.labels = c(
    "Pooled OLS",
    "Individual FE",
    "Time FE",
    "Two-way FE"
  ),
  dep.var.labels = "School attendance",
  covariate.labels = c(
    "Other children in household",
    "Age"
  ),
  se = list(
    se_pooled,
    se_individual_fe,
    se_time_fe,
    se_twoway_fe
  ),
  add.lines = list(
    c("Individual fixed effects", "No", "Yes", "No", "Yes"),
    c("Time fixed effects", "No", "No", "Yes", "Yes"),
    c("Individuals", N_panel, N_panel, N_panel, N_panel),
    c("Survey waves", T_panel, T_panel, T_panel, T_panel)
  ),
  digits = 3,
  notes = paste(
    "Heteroskedasticity-robust standard errors clustered by individual",
    "are reported in parentheses."
  ),
  notes.append = TRUE
)


