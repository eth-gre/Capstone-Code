# === Load libraries
library(AER)
library(stargazer)
library(dplyr)



# === Load the data across all CSVs

# Dependent/outcome
df_views_on_violence = read.csv("data/upd4_hh_y.csv")

# Variable of interest
df_individual_education = read.csv("data/upd4_hh_c.csv")

# Controls
df_personal_information = read.csv("data/upd4_hh_b.csv")
df_location = read.csv("data/upd4_hh_a.csv")
df_expenditure = read.csv("data/upd4_hh_l.csv")

df_assets = read.csv("data/upd4_hh_m.csv")
df_assets <- df_assets %>% filter(round == TARGET_ROUND)




# === Extract the columns we actually want

# These are on a HOUSEHOLD level (UPHI)
df_views_on_violence <- df_views_on_violence %>% select(UPHI, round, r_hhid, r_id, hy_02_a, hy_02_b, hy_02_c, hy_02_d, hy_02_e, hy_02_f, hy_02_g, hy_02_h)
df_location <- df_location %>% select(UPHI, round, r_hhid, urban_label = urb_rur)
df_expenditure <- df_expenditure %>% select(UPHI, round, r_hhid, expense = hl_02)


# These are on an INDIVIDUAL level (UPI)
df_individual_education <- df_individual_education %>% select(UPI, round, r_hhid, r_id, grade_label = hc_07, educ_cost = hc_28_8)
df_personal_information <- df_personal_information %>% select(UPI, round, r_hhid, r_id, sex_label = hb_02, age = hb_04, marriage_label = hb_19)

# Clean up the types (R thinks this is a number, silly R)
df_views_on_violence$r_hhid <- as.character(df_views_on_violence$r_hhid)


# === Create derived columns

# They 'support' violence if they answer yes to ANY of the questions
df_views_on_violence <- df_views_on_violence %>% mutate(supports_violence = as.integer(if_any(c(hy_02_a, hy_02_b, hy_02_c, hy_02_d, hy_02_e, hy_02_f, hy_02_g, hy_02_h), ~ .x == ' YES')))

# Education is provided as a label and must be converted to years for the regression
# Many of them are a lil bit ambiguous, soooo sorry to anyone who did a diploma??
# They get an 'NA' string which gets filtered out later
educ_years_map <- c(
  "PP" = 0,
  "ADULT" = NA,
  
  "D1" = 1, "D2" = 2, "D3" = 3, "D4" = 4, "D5" = 5, "D6" = 6, "D7" = 7, "D8" = 8,
  "F1" = 9, "F2" = 10, "F3" = 11, "F4" = 12, "F5" = 13, "F6" = 14,
  "U1" = 13, "U2" = 14, "U3" = 15, "U4" = 16, "U5&+" = 17,
  
  "OSC" = NA,
  "MS+ COURSE" = NA,
  "'O'+ COURSE" = NA,
  "'A'+ COURSE" = NA,
  "DIPLOMA" = NA
)

df_individual_education$years_educ <- educ_years_map[df_individual_education$grade_label]

# Map variables to a 1 or 0
df_personal_information <- df_personal_information %>% mutate(is_female = as.integer(sex_label == 'FEMALE'))
df_personal_information <- df_personal_information %>% mutate(is_polygamous = as.integer(marriage_label == 'POLYGAMOUS MARRIED'))
df_location <- df_location %>% mutate(is_urban = as.integer(urban_label == 'URBAN'))

# For household expenditure, group and sum (then drop the individual expenses to help with the merge)
df_expenditure <- df_expenditure %>% group_by(UPHI, round, r_hhid) %>% mutate(total_expenses = sum(expense, na.rm = TRUE)) %>% ungroup()
df_expenditure <- df_expenditure %>% select(UPHI, round, r_hhid, total_expenses)
df_expenditure <- df_expenditure %>% mutate(total_expenses = total_expenses / 1000)

# Normalise education cost too
df_individual_education <- df_individual_education %>% mutate(educ_cost = educ_cost / 1000)


# === Filter the data down

# Make everything round 1
TARGET_ROUND <- 1

df_views_on_violence <- df_views_on_violence %>% filter(round == TARGET_ROUND)
df_individual_education <- df_individual_education %>% filter(round == TARGET_ROUND)

# The individual needs and education label
df_individual_education <- df_individual_education %>% filter(!years_educ == 'NA')



# === Merge all of the CSVs together (on an individual level)
df <- df_individual_education %>% left_join(df_views_on_violence, by = c("round", "r_hhid", "r_id"))
df <- df %>% left_join(df_personal_information, by = c("round", "r_hhid", "r_id"))
df <- df %>% left_join(df_location, by = c("round", "r_hhid", "UPHI"), relationship = "many-to-many")
df <- df %>% left_join(df_expenditure, by = c("round", "r_hhid", "UPHI"), relationship = "many-to-many")


# Then kill off all the rows that didn't merge or are unwanted
df <- df %>% filter(!supports_violence == 'NA')
df <- df %>% filter(!is.na(total_expenses) & total_expenses > 0)
df <- df %>% filter(is_female == 1)

df <- distinct(df)


# Compute the final derived net educ column
df <- df %>% mutate(expenses_net_educ = total_expenses - educ_cost)


# === Simple OLS regression
ols <- lm(supports_violence ~ years_educ + age + is_urban + total_expenses + is_polygamous, data = df)
se_ols <- sqrt(diag(vcovHC(ols, type = "HC1")))

stargazer(
  ols,
  type = "latex",
  title = "Table 1: OLS Regression Results",
  dep.var.labels = "Support of intimate partner violence",
  covariate.labels = c(
    "Formal education (years)",
    "Age (years)",
    "Urban (1/0)",
    "Total expenses (1000TSh)",
    "Polygamous (1/0)"
  ),
  se = list(se_ols),
  digits = 3,
  notes = "Heteroskedasticity-robust standard errors are reported in parentheses.",
  notes.append = TRUE
)


# === Summary stats
col <- "supports_violence"
df %>% summarise(min = min(.data[[col]]), mean = mean(.data[[col]]), max = max(.data[[col]]), sd = sd(.data[[col]]))
