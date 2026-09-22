# Credit Card Pricing and Product Features in the U.S. Market
# Reproducible R workflow
# Author: Peter Sarpong
#
# Data: CFPB Terms of Credit Card Plans Survey, July 1-Dec. 31, 2025
# This script assumes the official CFPB workbook is in the working directory.
# Change data_file below if your downloaded filename differs.

library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)

data_file <- "CFPB_Credit_Card_Analytics_Project.xlsx"

cfpb <- read_excel(
  data_file,
  sheet = "TCCP Survey",
  skip = 9
)

stopifnot(nrow(cfpb) == 663)

cfpb_analysis <- cfpb %>%
  mutate(
    secured = if_else(`Secured Card` == "Yes", 1, 0),
    intro_apr = if_else(`Introductory APR Offered?` == "Yes", 1, 0),
    balance_transfer = if_else(`Balance Transfer Offered?` == "Yes", 1, 0),
    cashback = if_else(grepl("Cashback", Rewards), 1, 0, missing = 0),
    travel_rewards = if_else(grepl("Travel-related", Rewards), 1, 0, missing = 0),
    other_rewards = if_else(grepl("Other rewards", Rewards), 1, 0, missing = 0),
    apr_varies_credit = case_when(
      `Purchase APR Vary by Credit Tier` == "Yes" ~ 1,
      `Purchase APR Vary by Credit Tier` == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    periodic_fee = if_else(!is.na(`Periodic Fee Type`), 1, 0),
    no_score = grepl("No credit score", `Targeted Credit Tiers`, ignore.case = TRUE),
    poor = grepl("619", `Targeted Credit Tiers`),
    good = grepl("620", `Targeted Credit Tiers`) | grepl("719", `Targeted Credit Tiers`),
    great = grepl("720", `Targeted Credit Tiers`),
    number_credit_tiers = no_score + poor + good + great,
    credit_tier_count = factor(number_credit_tiers, levels = c(1, 2, 3, 4)),
    apr_620_719_raw = case_when(
      `Purchase APR Vary by Credit Tier` == "Yes" ~ `Purchase APR good`,
      `Purchase APR Vary by Credit Tier` == "No" ~ `Purchase APR median`,
      TRUE ~ NA_real_
    ),
    # 9.99 represents the imported form of CFPB's numeric sentinel 999.
    apr_620_719_clean = if_else(
      !is.na(apr_620_719_raw) & apr_620_719_raw != 9.99,
      apr_620_719_raw,
      NA_real_
    ),
    apr_620_719_clean_pct = apr_620_719_clean * 100
  )

model3_data <- cfpb_analysis %>%
  select(
    apr_620_719_clean_pct,
    `Institution Name`,
    secured, intro_apr, balance_transfer,
    cashback, travel_rewards, other_rewards,
    apr_varies_credit, periodic_fee,
    credit_tier_count,
    `Purchase APR min`, `Purchase APR max`
  ) %>%
  filter(
    !is.na(apr_620_719_clean_pct),
    !is.na(`Institution Name`),
    !is.na(secured),
    !is.na(intro_apr),
    !is.na(balance_transfer),
    !is.na(cashback),
    !is.na(travel_rewards),
    !is.na(other_rewards),
    !is.na(apr_varies_credit),
    !is.na(periodic_fee),
    !is.na(credit_tier_count)
  )

stopifnot(nrow(model3_data) == 620)
stopifnot(dplyr::n_distinct(model3_data$`Institution Name`) == 172)

model3 <- lm(
  apr_620_719_clean_pct ~
    secured + intro_apr + balance_transfer +
    cashback + travel_rewards + other_rewards +
    apr_varies_credit + periodic_fee +
    credit_tier_count,
  data = model3_data
)

summary(model3)

# Institution-clustered covariance matrix and coefficient tests.
cluster_vcov <- vcovCL(
  model3,
  cluster = ~ `Institution Name`,
  type = "HC1"
)
cluster_results <- coeftest(model3, vcov. = cluster_vcov)
print(cluster_results)

# Cluster-based 95% confidence intervals using G - 1 degrees of freedom.
G <- n_distinct(model3_data$`Institution Name`)
crit_t <- qt(0.975, df = G - 1)
cluster_ci <- cbind(
  Estimate = coef(model3),
  SE = sqrt(diag(cluster_vcov)),
  CI_low = coef(model3) - crit_t * sqrt(diag(cluster_vcov)),
  CI_high = coef(model3) + crit_t * sqrt(diag(cluster_vcov))
)
print(cluster_ci)

# Diagnostics.
print(bptest(model3))
print(resettest(model3, power = 2:3, type = "fitted"))

# Sensitivity 1: exclude the three 0% standardized APR observations.
model_zero_sens_data <- model3_data %>%
  filter(apr_620_719_clean_pct != 0)

model_zero_sens <- update(model3, data = model_zero_sens_data)
zero_vcov <- vcovCL(
  model_zero_sens,
  cluster = ~ `Institution Name`,
  type = "HC1"
)
print(coeftest(model_zero_sens, vcov. = zero_vcov))

# Sensitivity 2: exclude observations where standardized APR exceeds
# the plan's reported maximum purchase APR. This is an audit sensitivity;
# the observations are not labeled as errors and remain in the primary model.
range_sens_data <- model3_data %>%
  mutate(
    purchase_apr_max_pct = `Purchase APR max` * 100,
    above_reported_max =
      !is.na(purchase_apr_max_pct) &
      apr_620_719_clean_pct > purchase_apr_max_pct
  ) %>%
  filter(!above_reported_max)

model_range_sens <- update(model3, data = range_sens_data)
range_vcov <- vcovCL(
  model_range_sens,
  cluster = ~ `Institution Name`,
  type = "HC1"
)
print(coeftest(model_range_sens, vcov. = range_vcov))

cat("\nPrimary N:", nrow(model3_data),
    "\nPrimary institutions:", G,
    "\nZero-exclusion N:", nrow(model_zero_sens_data),
    "\nRange-sensitivity N:", nrow(range_sens_data), "\n")
