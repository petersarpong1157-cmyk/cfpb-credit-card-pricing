# Install packages once if needed
install.packages(c("readxl", "dplyr", "ggplot2", "tidyr"))

# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
cfpb <- read_excel(
  "CFPB_Credit_Card_Analytics_Project.xlsx",
  sheet = "TCCP Survey",
  skip = 9
)

dim(cfpb)

names(cfpb)[1:20]

head(cfpb[, 1:6])

library(dplyr)

key_vars <- c(
  "Institution Name",
  "Product Name",
  "Secured Card",
  "Targeted Credit Tiers",
  "Purchase APR Offered?",
  "Purchase APR Vary by Credit Tier",
  "Purchase APR min",
  "Purchase APR median",
  "Purchase APR max",
  "Introductory APR Offered?",
  "Balance Transfer Offered?",
  "Periodic Fee Type",
  "Annual Fee",
  "Monthly Fee",
  "Weekly Fee",
  "Rewards",
  "Late Fees?"
)

# Check that every variable exists
key_vars %in% names(cfpb)

# Show only these variables
cfpb_key <- cfpb %>%
  select(all_of(key_vars))

dim(cfpb_key)

# Check data types
str(cfpb_key)

# Purchase APR audit
cfpb_key %>%
  summarise(
    total_cards = n(),
    apr_available = sum(!is.na(`Purchase APR median`)),
    apr_missing = sum(is.na(`Purchase APR median`)),
    
    min_apr = min(`Purchase APR median`, na.rm = TRUE),
    median_apr = median(`Purchase APR median`, na.rm = TRUE),
    mean_apr = mean(`Purchase APR median`, na.rm = TRUE),
    max_apr = max(`Purchase APR median`, na.rm = TRUE)
  )

apr_problem <- cfpb_key %>%
  filter(
    !is.na(`Purchase APR min`),
    !is.na(`Purchase APR median`),
    !is.na(`Purchase APR max`),
    `Purchase APR median` < `Purchase APR min` |
      `Purchase APR median` > `Purchase APR max`
  ) %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR min`,
    `Purchase APR median`,
    `Purchase APR max`
  )

nrow(apr_problem)

apr_problem

cfpb_key %>%
  filter(`Purchase APR median` > 1) %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR min`,
    `Purchase APR median`,
    `Purchase APR max`
  )

cfpb_clean <- cfpb_key %>%
  mutate(
    # Flag problematic APR observations
    apr_problem = case_when(
      is.na(`Purchase APR median`) ~ "Missing",
      
      !is.na(`Purchase APR min`) &
        `Purchase APR median` < `Purchase APR min` ~ "Invalid",
      
      !is.na(`Purchase APR max`) &
        `Purchase APR median` > `Purchase APR max` ~ "Invalid",
      
      TRUE ~ "Valid"
    ),
    
    # Clean APR used for analysis
    purchase_apr = if_else(
      apr_problem == "Valid",
      `Purchase APR median`,
      NA_real_
    ),
    
    # Convert decimal APR to percentage points
    purchase_apr_pct = purchase_apr * 100
  )

table(cfpb_clean$apr_problem)

summary(cfpb_clean$purchase_apr_pct)

cfpb_clean %>%
  summarise(
    total_records = n(),
    valid_apr = sum(apr_problem == "Valid"),
    invalid_apr = sum(apr_problem == "Invalid"),
    missing_apr = sum(apr_problem == "Missing")
  )

library(ggplot2)

ggplot(
  cfpb_clean %>% filter(!is.na(purchase_apr_pct)),
  aes(x = purchase_apr_pct)
) +
  geom_histogram(
    binwidth = 2,
    boundary = 0
  ) +
  labs(
    title = "Distribution of Purchase APRs",
    subtitle = "CFPB Terms of Credit Card Plans Survey, July–December 2025",
    x = "Median Purchase APR (%)",
    y = "Number of Credit Card Plans"
  ) +
  theme_minimal()

cfpb_clean %>%
  filter(!is.na(purchase_apr_pct)) %>%
  summarise(
    n = n(),
    mean = mean(purchase_apr_pct),
    sd = sd(purchase_apr_pct),
    min = min(purchase_apr_pct),
    q1 = quantile(purchase_apr_pct, 0.25),
    median = median(purchase_apr_pct),
    q3 = quantile(purchase_apr_pct, 0.75),
    max = max(purchase_apr_pct)
  )

cfpb_clean %>%
  filter(purchase_apr_pct == 0) %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR Offered?`,
    `Purchase APR min`,
    `Purchase APR median`,
    `Purchase APR max`,
    `Introductory APR Offered?`
  )

cfpb_clean %>%
  filter(purchase_apr_pct == 0) %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR Offered?`,
    `Purchase APR min`,
    `Purchase APR median`,
    `Purchase APR max`,
    `Introductory APR Offered?`
  ) %>%
  print(width = Inf)

table(cfpb_clean$`Secured Card`, useNA = "ifany")

cfpb_clean %>%
  filter(!is.na(purchase_apr_pct)) %>%
  group_by(`Secured Card`) %>%
  summarise(
    n = n(),
    mean_apr = mean(purchase_apr_pct),
    median_apr = median(purchase_apr_pct),
    sd_apr = sd(purchase_apr_pct),
    min_apr = min(purchase_apr_pct),
    max_apr = max(purchase_apr_pct)
  )


# Introductory APR
table(
  cfpb_clean$`Introductory APR Offered?`,
  useNA = "ifany"
)

# Balance transfer
table(
  cfpb_clean$`Balance Transfer Offered?`,
  useNA = "ifany"
)

# Rewards
table(
  cfpb_clean$Rewards,
  useNA = "ifany"
)

# Credit-tier variation
table(
  cfpb_clean$`Purchase APR Vary by Credit Tier`,
  useNA = "ifany"
)

# Periodic fee type
table(
  cfpb_clean$`Periodic Fee Type`,
  useNA = "ifany"
)

cfpb_clean %>%
  summarise(
    annual_fee_nonmissing =
      sum(!is.na(`Annual Fee`)),
    
    monthly_fee_nonmissing =
      sum(!is.na(`Monthly Fee`)),
    
    weekly_fee_nonmissing =
      sum(!is.na(`Weekly Fee`)),
    
    annual_fee_min =
      min(`Annual Fee`, na.rm = TRUE),
    
    annual_fee_median =
      median(`Annual Fee`, na.rm = TRUE),
    
    annual_fee_max =
      max(`Annual Fee`, na.rm = TRUE)
  )


##Step 8 — Construct the analysis variables
cfpb_analysis <- cfpb_clean %>%
  mutate(
    # Secured card
    secured = if_else(`Secured Card` == "Yes", 1, 0),
    
    # Introductory APR
    intro_apr = if_else(
      `Introductory APR Offered?` == "Yes", 1, 0
    ),
    
    # Balance transfer
    balance_transfer = if_else(
      `Balance Transfer Offered?` == "Yes", 1, 0
    ),
    
    # Rewards information
    rewards_reported = if_else(
      !is.na(Rewards), 1, 0
    ),
    
    cashback = if_else(
      grepl("Cashback", Rewards), 1, 0,
      missing = 0
    ),
    
    travel_rewards = if_else(
      grepl("Travel-related", Rewards), 1, 0,
      missing = 0
    ),
    
    other_rewards = if_else(
      grepl("Other rewards", Rewards), 1, 0,
      missing = 0
    ),
    
    # Purchase APR varies by credit tier
    apr_varies_credit = case_when(
      `Purchase APR Vary by Credit Tier` == "Yes" ~ 1,
      `Purchase APR Vary by Credit Tier` == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Any periodic fee reported
    periodic_fee = if_else(
      !is.na(`Periodic Fee Type`), 1, 0
    )
  )

cfpb_analysis %>%
  summarise(
    total = n(),
    
    secured = sum(secured == 1, na.rm = TRUE),
    
    intro_apr = sum(intro_apr == 1, na.rm = TRUE),
    
    balance_transfer =
      sum(balance_transfer == 1, na.rm = TRUE),
    
    rewards_reported =
      sum(rewards_reported == 1, na.rm = TRUE),
    
    cashback =
      sum(cashback == 1, na.rm = TRUE),
    
    travel_rewards =
      sum(travel_rewards == 1, na.rm = TRUE),
    
    other_rewards =
      sum(other_rewards == 1, na.rm = TRUE),
    
    apr_varies_credit =
      sum(apr_varies_credit == 1, na.rm = TRUE),
    
    periodic_fee =
      sum(periodic_fee == 1, na.rm = TRUE)
  )

cfpb_analysis %>%
  summarise(
    total = n(),
    secured = sum(secured == 1, na.rm = TRUE),
    intro_apr = sum(intro_apr == 1, na.rm = TRUE),
    balance_transfer = sum(balance_transfer == 1, na.rm = TRUE),
    rewards_reported = sum(rewards_reported == 1, na.rm = TRUE),
    cashback = sum(cashback == 1, na.rm = TRUE),
    travel_rewards = sum(travel_rewards == 1, na.rm = TRUE),
    other_rewards = sum(other_rewards == 1, na.rm = TRUE),
    apr_varies_credit = sum(apr_varies_credit == 1, na.rm = TRUE),
    periodic_fee = sum(periodic_fee == 1, na.rm = TRUE)
  ) %>%
  print(width = Inf)


##Step 9 — APR by card characteristics
library(dplyr)
library(tidyr)

apr_comparison <- cfpb_analysis %>%
  filter(!is.na(purchase_apr_pct)) %>%
  select(
    purchase_apr_pct,
    secured,
    intro_apr,
    balance_transfer,
    rewards_reported,
    cashback,
    travel_rewards,
    other_rewards,
    apr_varies_credit,
    periodic_fee
  ) %>%
  pivot_longer(
    cols = -purchase_apr_pct,
    names_to = "characteristic",
    values_to = "present"
  ) %>%
  filter(!is.na(present)) %>%
  group_by(characteristic, present) %>%
  summarise(
    n = n(),
    mean_apr = mean(purchase_apr_pct),
    median_apr = median(purchase_apr_pct),
    sd_apr = sd(purchase_apr_pct),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(present == 1, "Yes", "No")
  ) %>%
  select(
    characteristic,
    group,
    n,
    mean_apr,
    median_apr,
    sd_apr
  )

apr_comparison %>%
  mutate(
    across(
      c(mean_apr, median_apr, sd_apr),
      ~ round(.x, 2)
    )
  ) %>%
  print(n = Inf)

##Step 10 — Test the unadjusted differences
variables <- c(
  "secured",
  "intro_apr",
  "balance_transfer",
  "rewards_reported",
  "cashback",
  "travel_rewards",
  "other_rewards",
  "apr_varies_credit",
  "periodic_fee"
)

test_results <- lapply(variables, function(v) {
  
  temp <- cfpb_analysis %>%
    filter(
      !is.na(purchase_apr_pct),
      !is.na(.data[[v]])
    )
  
  t_result <- t.test(
    purchase_apr_pct ~ temp[[v]],
    data = temp
  )
  
  w_result <- wilcox.test(
    purchase_apr_pct ~ temp[[v]],
    data = temp,
    exact = FALSE
  )
  
  data.frame(
    characteristic = v,
    mean_no = mean(
      temp$purchase_apr_pct[temp[[v]] == 0]
    ),
    mean_yes = mean(
      temp$purchase_apr_pct[temp[[v]] == 1]
    ),
    mean_difference = mean(
      temp$purchase_apr_pct[temp[[v]] == 1]
    ) -
      mean(
        temp$purchase_apr_pct[temp[[v]] == 0]
      ),
    t_p_value = t_result$p.value,
    wilcox_p_value = w_result$p.value
  )
})

test_results <- bind_rows(test_results)

test_results %>%
  mutate(
    across(
      c(mean_no, mean_yes, mean_difference),
      ~ round(.x, 2)
    ),
    t_p_value = signif(t_p_value, 4),
    wilcox_p_value = signif(wilcox_p_value, 4)
  ) %>%
  print(n = Inf)


test_results_clean <- test_results %>%
  mutate(
    mean_no = round(mean_no, 2),
    mean_yes = round(mean_yes, 2),
    mean_difference = round(mean_difference, 2),
    t_p_value = round(t_p_value, 4),
    wilcox_p_value = round(wilcox_p_value, 4)
  )

View(test_results_clean)
test_results_clean


##Step 11 — Multiple regression
model1 <- lm(
  purchase_apr_pct ~
    secured +
    intro_apr +
    balance_transfer +
    cashback +
    travel_rewards +
    other_rewards +
    apr_varies_credit +
    periodic_fee,
  data = cfpb_analysis
)

summary(model1)

nobs(model1)

confint(model1)

##Step 12 — Check regression assumptions
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

install.packages("car")   # only if not already installed
library(car)

vif(model1)

install.packages("lmtest")   # only if needed
library(lmtest)

bptest(model1)

resettest(model1)

##Step 13 — Robust standard errors
install.packages("sandwich")   # only if needed

library(sandwich)
library(lmtest)

robust_model1 <- coeftest(
  model1,
  vcov = vcovHC(model1, type = "HC3")
)

robust_model1

robust_se <- sqrt(
  diag(vcovHC(model1, type = "HC3"))
)

robust_ci <- cbind(
  Estimate = coef(model1),
  Robust_SE = robust_se,
  Lower_95 = coef(model1) - 1.96 * robust_se,
  Upper_95 = coef(model1) + 1.96 * robust_se
)

round(robust_ci, 3)

influence_check <- data.frame(
  observation = as.numeric(names(cooks.distance(model1))),
  cooks_d = cooks.distance(model1),
  leverage = hatvalues(model1),
  studentized_residual = rstudent(model1)
)

influence_check %>%
  arrange(desc(cooks_d)) %>%
  head(10)

max(cooks.distance(model1))

sum(cooks.distance(model1) > 4 / nobs(model1))


model_data <- model.frame(model1)

model_rows <- as.numeric(rownames(model_data))

influence_products <- data.frame(
  original_row = model_rows,
  cooks_d = cooks.distance(model1),
  leverage = hatvalues(model1),
  studentized_residual = rstudent(model1)
) %>%
  arrange(desc(cooks_d)) %>%
  slice_head(n = 10) %>%
  left_join(
    cfpb_analysis %>%
      mutate(original_row = row_number()) %>%
      select(
        original_row,
        `Institution Name`,
        `Product Name`,
        purchase_apr_pct,
        secured,
        intro_apr,
        balance_transfer,
        cashback,
        travel_rewards,
        other_rewards,
        apr_varies_credit,
        periodic_fee
      ),
    by = "original_row"
  )

influence_products %>%
  print(width = Inf)

View(influence_products)

as.data.frame(influence_products)

influence_products %>%
  select(
    original_row,
    `Institution Name`,
    `Product Name`,
    purchase_apr_pct,
    cooks_d,
    leverage,
    studentized_residual
  ) %>%
  as.data.frame()

cook_cutoff <- 4 / nobs(model1)

keep_obs <- cooks.distance(model1) <= cook_cutoff

sum(!keep_obs)
sum(keep_obs)

model1_sensitivity <- lm(
  formula(model1),
  data = model.frame(model1)[keep_obs, ]
)

coeftest(
  model1_sensitivity,
  vcov = vcovHC(model1_sensitivity, type = "HC3")
)

nobs(model1_sensitivity)

##Step 14 — Examine targeted credit tiers
table(
  cfpb_analysis$`Targeted Credit Tiers`,
  useNA = "ifany"
)

cfpb_analysis %>%
  count(`Targeted Credit Tiers`, sort = TRUE) %>%
  as.data.frame()

cfpb_analysis <- cfpb_analysis %>%
  mutate(
    targets_no_score =
      grepl(
        "No credit score",
        `Targeted Credit Tiers`
      ),
    
    targets_619_or_less =
      grepl(
        "619 or less",
        `Targeted Credit Tiers`
      ),
    
    targets_620_719 =
      grepl(
        "620 to 719",
        `Targeted Credit Tiers`
      ),
    
    targets_720_plus =
      grepl(
        "720 or greater",
        `Targeted Credit Tiers`
      )
  )

cfpb_analysis %>%
  summarise(
    no_score =
      sum(targets_no_score, na.rm = TRUE),
    
    score_619_or_less =
      sum(targets_619_or_less, na.rm = TRUE),
    
    score_620_719 =
      sum(targets_620_719, na.rm = TRUE),
    
    score_720_plus =
      sum(targets_720_plus, na.rm = TRUE)
  )

##Step 15 — Check credit-tier overlap
tier_overlap <- cfpb_analysis %>%
  summarise(
    no_score_and_619 =
      sum(targets_no_score & targets_619_or_less),
    
    no_score_and_620 =
      sum(targets_no_score & targets_620_719),
    
    no_score_and_720 =
      sum(targets_no_score & targets_720_plus),
    
    score_619_and_620 =
      sum(targets_619_or_less & targets_620_719),
    
    score_619_and_720 =
      sum(targets_619_or_less & targets_720_plus),
    
    score_620_and_720 =
      sum(targets_620_719 & targets_720_plus)
  )

tier_overlap

cfpb_analysis <- cfpb_analysis %>%
  mutate(
    number_credit_tiers =
      as.integer(targets_no_score) +
      as.integer(targets_619_or_less) +
      as.integer(targets_620_719) +
      as.integer(targets_720_plus)
  )

table(cfpb_analysis$number_credit_tiers)

cfpb_analysis %>%
  filter(!is.na(purchase_apr_pct)) %>%
  group_by(number_credit_tiers) %>%
  summarise(
    n = n(),
    mean_apr = mean(purchase_apr_pct),
    median_apr = median(purchase_apr_pct),
    sd_apr = sd(purchase_apr_pct),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(mean_apr, median_apr, sd_apr),
      ~ round(.x, 2)
    )
  )

##Step 16 — Add credit-tier breadth as categories
cfpb_analysis <- cfpb_analysis %>%
  mutate(
    credit_tier_count = factor(
      number_credit_tiers,
      levels = c(1, 2, 3, 4)
    )
  )

model2 <- lm(
  purchase_apr_pct ~
    secured +
    intro_apr +
    balance_transfer +
    cashback +
    travel_rewards +
    other_rewards +
    apr_varies_credit +
    periodic_fee +
    credit_tier_count,
  data = cfpb_analysis
)

summary(model2)

nobs(model2)

robust_model2 <- coeftest(
  model2,
  vcov = vcovHC(model2, type = "HC3")
)

robust_model2

resettest(model2)

data.frame(
  Model = c("Model 1", "Model 2"),
  R_squared = c(
    summary(model1)$r.squared,
    summary(model2)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(model1)$adj.r.squared,
    summary(model2)$adj.r.squared
  ),
  AIC = c(
    AIC(model1),
    AIC(model2)
  )
)

install.packages("multiwayvcov")  # only if needed

library(multiwayvcov)
library(lmtest)

cluster_vcov <- cluster.vcov(
  model2,
  cfpb_analysis$`Institution Name`[
    as.numeric(rownames(model.frame(model2)))
  ]
)

cluster_model2 <- coeftest(
  model2,
  vcov = cluster_vcov
)

cluster_model2
##Fix the institution clustering
# Create model dataset explicitly
model2_data <- cfpb_analysis %>%
  filter(!is.na(purchase_apr_pct)) %>%
  select(
    purchase_apr_pct,
    `Institution Name`,
    secured,
    intro_apr,
    balance_transfer,
    cashback,
    travel_rewards,
    other_rewards,
    apr_varies_credit,
    periodic_fee,
    credit_tier_count
  ) %>%
  filter(complete.cases(.))

nrow(model2_data)

length(unique(model2_data$`Institution Name`))

model2_cluster <- lm(
  purchase_apr_pct ~
    secured +
    intro_apr +
    balance_transfer +
    cashback +
    travel_rewards +
    other_rewards +
    apr_varies_credit +
    periodic_fee +
    credit_tier_count,
  data = model2_data
)

nobs(model2_cluster)

coef(model2_cluster)

cluster_vcov <- cluster.vcov(
  model2_cluster,
  model2_data$`Institution Name`
)

cluster_model2 <- coeftest(
  model2_cluster,
  vcov = cluster_vcov
)

cluster_model2

length(unique(model2_data$`Institution Name`))

bptest(model2_cluster)

##Step 18 — Sensitivity check for the four 0% APR cards
model_zero_sensitivity_data <- model2_data %>%
  filter(purchase_apr_pct > 0)

nrow(model_zero_sensitivity_data)

model_zero_sensitivity <- lm(
  purchase_apr_pct ~
    secured +
    intro_apr +
    balance_transfer +
    cashback +
    travel_rewards +
    other_rewards +
    apr_varies_credit +
    periodic_fee +
    credit_tier_count,
  data = model_zero_sensitivity_data
)

zero_cluster_vcov <- cluster.vcov(
  model_zero_sensitivity,
  model_zero_sensitivity_data$`Institution Name`
)

zero_cluster_results <- coeftest(
  model_zero_sensitivity,
  vcov = zero_cluster_vcov
)

zero_cluster_results

summary(model_zero_sensitivity)$r.squared
summary(model_zero_sensitivity)$adj.r.squared

##Step 19 — Build the final regression table in R
cluster_results_matrix <- as.matrix(cluster_model2)

final_regression_table <- data.frame(
  Variable = rownames(cluster_results_matrix),
  Estimate = cluster_results_matrix[, 1],
  Clustered_SE = cluster_results_matrix[, 2],
  t_value = cluster_results_matrix[, 3],
  p_value = cluster_results_matrix[, 4],
  row.names = NULL
)

final_regression_table <- final_regression_table %>%
  mutate(
    CI_Lower = Estimate - 1.96 * Clustered_SE,
    CI_Upper = Estimate + 1.96 * Clustered_SE
  ) %>%
  select(
    Variable,
    Estimate,
    Clustered_SE,
    CI_Lower,
    CI_Upper,
    t_value,
    p_value
  )

final_regression_table %>%
  mutate(
    across(
      c(Estimate, Clustered_SE, CI_Lower, CI_Upper, t_value),
      ~ round(.x, 3)
    ),
    p_value = round(p_value, 4)
  ) %>%
  as.data.frame()

G <- length(unique(model2_data$`Institution Name`))

critical_t <- qt(
  0.975,
  df = G - 1
)

G
critical_t

final_regression_table <- final_regression_table %>%
  mutate(
    CI_Lower = Estimate - critical_t * Clustered_SE,
    CI_Upper = Estimate + critical_t * Clustered_SE
  )

##Step 20 — Update CIs and create the final coefficient plot
final_regression_table <- final_regression_table %>%
  mutate(
    CI_Lower = Estimate - critical_t * Clustered_SE,
    CI_Upper = Estimate + critical_t * Clustered_SE
  )

final_regression_table %>%
  mutate(
    across(
      c(Estimate, Clustered_SE, CI_Lower, CI_Upper, t_value),
      ~ round(.x, 3)
    ),
    p_value = round(p_value, 4)
  ) %>%
  as.data.frame()

plot_data <- final_regression_table %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Variable_Label = case_when(
      Variable == "secured" ~ "Secured card",
      Variable == "intro_apr" ~ "Introductory APR offered",
      Variable == "balance_transfer" ~ "Balance transfer offered",
      Variable == "cashback" ~ "Cashback rewards",
      Variable == "travel_rewards" ~ "Travel rewards",
      Variable == "other_rewards" ~ "Other rewards",
      Variable == "apr_varies_credit" ~ "APR varies by credit tier",
      Variable == "periodic_fee" ~ "Periodic fee reported",
      Variable == "credit_tier_count2" ~ "Targets 2 credit tiers",
      Variable == "credit_tier_count3" ~ "Targets 3 credit tiers",
      Variable == "credit_tier_count4" ~ "Targets 4 credit tiers",
      TRUE ~ Variable
    )
  )

library(ggplot2)

ggplot(
  plot_data,
  aes(
    x = Estimate,
    y = reorder(Variable_Label, Estimate)
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"  ) +
  geom_errorbar(
    aes(
      xmin = CI_Lower,
      xmax = CI_Upper
    ),
    width = 0.2
  ) +
  geom_point(size = 2.5) +
  labs(
    title = "Adjusted Associations Between Credit Card Features and Purchase APR",
    subtitle = "OLS estimates with standard errors clustered by institution",
    x = "Estimated difference in purchase APR (percentage points)",
    y = NULL,
    caption = "N = 599 credit card plans; 174 institutions. Bars show 95% confidence intervals."
  ) +
  theme_minimal(base_size = 12)

##Step 21 — Create the final descriptive table
descriptive_table <- model2_data %>%
  summarise(
    `Credit card plans` = n(),
    `Institutions` = n_distinct(`Institution Name`),
    
    `Purchase APR, mean (%)` =
      mean(purchase_apr_pct),
    
    `Purchase APR, SD` =
      sd(purchase_apr_pct),
    
    `Purchase APR, median (%)` =
      median(purchase_apr_pct),
    
    `Purchase APR, min (%)` =
      min(purchase_apr_pct),
    
    `Purchase APR, max (%)` =
      max(purchase_apr_pct),
    
    `Secured cards, n` =
      sum(secured == 1),
    
    `Introductory APR offered, n` =
      sum(intro_apr == 1),
    
    `Balance transfer offered, n` =
      sum(balance_transfer == 1),
    
    `Cashback rewards, n` =
      sum(cashback == 1),
    
    `Travel rewards, n` =
      sum(travel_rewards == 1),
    
    `Other rewards, n` =
      sum(other_rewards == 1),
    
    `APR varies by credit tier, n` =
      sum(apr_varies_credit == 1),
    
    `Periodic fee reported, n` =
      sum(periodic_fee == 1)
  )

as.data.frame(descriptive_table)

tier_table <- model2_data %>%
  count(credit_tier_count) %>%
  mutate(
    Percent = round(100 * n / sum(n), 1)
  )

as.data.frame(tier_table)


##Step 22 — Produce Table 1 directly in R
table1 <- data.frame(
  Characteristic = c(
    "Credit card plans",
    "Institutions",
    "Purchase APR, mean (SD), %",
    "Purchase APR, median, %",
    "Purchase APR range, %",
    "Secured card",
    "Introductory APR offered",
    "Balance transfer offered",
    "Cashback rewards reported",
    "Travel rewards reported",
    "Other rewards reported",
    "APR varies by credit tier",
    "Periodic fee reported",
    "Targets 1 credit tier",
    "Targets 2 credit tiers",
    "Targets 3 credit tiers",
    "Targets 4 credit tiers"
  ),
  
  Value = c(
    "599",
    "174",
    sprintf("%.2f (%.2f)",
            mean(model2_data$purchase_apr_pct),
            sd(model2_data$purchase_apr_pct)),
    sprintf("%.2f",
            median(model2_data$purchase_apr_pct)),
    sprintf("%.2f–%.2f",
            min(model2_data$purchase_apr_pct),
            max(model2_data$purchase_apr_pct)),
    
    sprintf("%d (%.1f%%)",
            sum(model2_data$secured == 1),
            mean(model2_data$secured == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model2_data$intro_apr == 1),
            mean(model2_data$intro_apr == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model2_data$balance_transfer == 1),
            mean(model2_data$balance_transfer == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model2_data$cashback == 1),
            mean(model2_data$cashback == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model2_data$travel_rewards == 1),
            mean(model2_data$travel_rewards == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model2_data$other_rewards == 1),
            mean(model2_data$other_rewards == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model2_data$apr_varies_credit == 1),
            mean(model2_data$apr_varies_credit == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model2_data$periodic_fee == 1),
            mean(model2_data$periodic_fee == 1) * 100),
    
    sprintf("%d (%.1f%%)", 95, 95/599*100),
    sprintf("%d (%.1f%%)", 269, 269/599*100),
    sprintf("%d (%.1f%%)", 145, 145/599*100),
    sprintf("%d (%.1f%%)", 90, 90/599*100)
  )
)

as.data.frame(table1)

##Step 24 — Audit APR outcome against credit-tier variation
cfpb %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR Vary by Credit Tier`,
    `Purchase APR no score`,
    `Purchase APR poor`,
    `Purchase APR good`,
    `Purchase APR great`,
    `Purchase APR min`,
    `Purchase APR median`,
    `Purchase APR max`
  ) %>%
  group_by(`Purchase APR Vary by Credit Tier`) %>%
  summarise(
    n = n(),
    
    median_available =
      sum(!is.na(`Purchase APR median`)),
    
    no_score_available =
      sum(!is.na(`Purchase APR no score`)),
    
    poor_available =
      sum(!is.na(`Purchase APR poor`)),
    
    good_available =
      sum(!is.na(`Purchase APR good`)),
    
    great_available =
      sum(!is.na(`Purchase APR great`)),
    
    .groups = "drop"
  ) %>%
  as.data.frame()

cfpb %>%
  filter(
    `Purchase APR Vary by Credit Tier` == "Yes",
    !is.na(`Purchase APR median`)
  ) %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR no score`,
    `Purchase APR poor`,
    `Purchase APR good`,
    `Purchase APR great`,
    `Purchase APR min`,
    `Purchase APR median`,
    `Purchase APR max`
  ) %>%
  head(20) %>%
  as.data.frame()

cfpb_analysis <- cfpb_analysis %>%
  mutate(
    apr_620_719 = case_when(
      `Purchase APR Vary by Credit Tier` == "Yes" ~
        cfpb$`Purchase APR good`,
      
      `Purchase APR Vary by Credit Tier` == "No" ~
        `Purchase APR median`,
      
      TRUE ~ NA_real_
    ),
    
    apr_620_719_pct = apr_620_719 * 100
  )

cfpb_analysis %>%
  summarise(
    total = n(),
    available = sum(!is.na(apr_620_719_pct)),
    missing = sum(is.na(apr_620_719_pct)),
    mean = mean(apr_620_719_pct, na.rm = TRUE),
    sd = sd(apr_620_719_pct, na.rm = TRUE),
    median = median(apr_620_719_pct, na.rm = TRUE),
    min = min(apr_620_719_pct, na.rm = TRUE),
    max = max(apr_620_719_pct, na.rm = TRUE)
  ) %>%
  as.data.frame()

table(
  cfpb_analysis$`Purchase APR Vary by Credit Tier`,
  is.na(cfpb_analysis$apr_620_719_pct),
  useNA = "ifany"
)

cfpb_analysis %>%
  filter(
    !is.na(apr_620_719_pct),
    apr_620_719_pct < 0 |
      apr_620_719_pct > 100
  ) %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR Vary by Credit Tier`,
    apr_620_719_pct
  ) %>%
  as.data.frame()

##Step 25 — Validate and clean the standardized 620–719 APR
cfpb %>%
  filter(
    `Institution Name` == "Bank Of America, National Association",
    `Product Name` %in% c(
      "CTA Rate Smart Card",
      "MTA Rate Smart Card"
    )
  ) %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR Vary by Credit Tier`,
    `Purchase APR min`,
    `Purchase APR median`,
    `Purchase APR max`,
    `Purchase APR no score`,
    `Purchase APR poor`,
    `Purchase APR good`,
    `Purchase APR great`
  ) %>%
  as.data.frame()

cfpb_analysis <- cfpb_analysis %>%
  mutate(
    apr_620_719_problem = case_when(
      is.na(apr_620_719) ~ "Missing",
      apr_620_719 < 0 ~ "Invalid",
      apr_620_719 > 1 ~ "Invalid",
      TRUE ~ "Valid"
    ),
    
    apr_620_719_clean = if_else(
      apr_620_719_problem == "Valid",
      apr_620_719,
      NA_real_
    ),
    
    apr_620_719_clean_pct =
      apr_620_719_clean * 100
  )

table(
  cfpb_analysis$apr_620_719_problem,
  useNA = "ifany"
)

cfpb_analysis %>%
  summarise(
    total = n(),
    valid = sum(apr_620_719_problem == "Valid"),
    invalid = sum(apr_620_719_problem == "Invalid"),
    missing = sum(apr_620_719_problem == "Missing"),
    
    mean = mean(apr_620_719_clean_pct, na.rm = TRUE),
    sd = sd(apr_620_719_clean_pct, na.rm = TRUE),
    median = median(apr_620_719_clean_pct, na.rm = TRUE),
    min = min(apr_620_719_clean_pct, na.rm = TRUE),
    max = max(apr_620_719_clean_pct, na.rm = TRUE)
  ) %>%
  as.data.frame()

##Step 26 — Re-estimate the primary model with the standardized outcome
model3_data <- cfpb_analysis %>%
  filter(!is.na(apr_620_719_clean_pct)) %>%
  select(
    apr_620_719_clean_pct,
    `Institution Name`,
    secured,
    intro_apr,
    balance_transfer,
    cashback,
    travel_rewards,
    other_rewards,
    apr_varies_credit,
    periodic_fee,
    credit_tier_count
  ) %>%
  filter(complete.cases(.))

nrow(model3_data)
length(unique(model3_data$`Institution Name`))

model3 <- lm(
  apr_620_719_clean_pct ~
    secured +
    intro_apr +
    balance_transfer +
    cashback +
    travel_rewards +
    other_rewards +
    apr_varies_credit +
    periodic_fee +
    credit_tier_count,
  data = model3_data
)

summary(model3)

cluster_vcov3 <- cluster.vcov(
  model3,
  model3_data$`Institution Name`
)

cluster_model3 <- coeftest(
  model3,
  vcov = cluster_vcov3
)

cluster_model3

bptest(model3)
resettest(model3)

data.frame(
  N = nobs(model3),
  Institutions =
    length(unique(model3_data$`Institution Name`)),
  R_squared =
    summary(model3)$r.squared,
  Adjusted_R_squared =
    summary(model3)$adj.r.squared,
  AIC = AIC(model3)
)

##Step 27 — Calculate final confidence intervals
G3 <- length(unique(model3_data$`Institution Name`))
critical_t3 <- qt(0.975, df = G3 - 1)

cluster3_matrix <- as.matrix(cluster_model3)

final_model3_table <- data.frame(
  Variable = rownames(cluster3_matrix),
  Estimate = cluster3_matrix[, 1],
  Clustered_SE = cluster3_matrix[, 2],
  t_value = cluster3_matrix[, 3],
  p_value = cluster3_matrix[, 4],
  row.names = NULL
) %>%
  mutate(
    CI_Lower = Estimate - critical_t3 * Clustered_SE,
    CI_Upper = Estimate + critical_t3 * Clustered_SE
  ) %>%
  select(
    Variable,
    Estimate,
    Clustered_SE,
    CI_Lower,
    CI_Upper,
    t_value,
    p_value
  )

G3
critical_t3

final_model3_table %>%
  mutate(
    across(
      c(Estimate, Clustered_SE, CI_Lower, CI_Upper, t_value),
      ~ round(.x, 3)
    ),
    p_value = ifelse(
      p_value < .001,
      "<0.001",
      sprintf("%.3f", p_value)
    )
  ) %>%
  as.data.frame()

table1_model3 <- model3_data %>%
  summarise(
    Plans = n(),
    Institutions = n_distinct(`Institution Name`),
    
    Mean_APR = mean(apr_620_719_clean_pct),
    SD_APR = sd(apr_620_719_clean_pct),
    Median_APR = median(apr_620_719_clean_pct),
    Min_APR = min(apr_620_719_clean_pct),
    Max_APR = max(apr_620_719_clean_pct),
    
    Secured_n = sum(secured == 1),
    Intro_APR_n = sum(intro_apr == 1),
    Balance_Transfer_n = sum(balance_transfer == 1),
    Cashback_n = sum(cashback == 1),
    Travel_n = sum(travel_rewards == 1),
    Other_Rewards_n = sum(other_rewards == 1),
    APR_Varies_n = sum(apr_varies_credit == 1),
    Periodic_Fee_n = sum(periodic_fee == 1)
  )

as.data.frame(table1_model3)

model3_data %>%
  count(credit_tier_count) %>%
  mutate(
    Percent = round(100 * n / sum(n), 1)
  ) %>%
  as.data.frame()

##Step 28 — Create the final Table 1
table1_final <- data.frame(
  Characteristic = c(
    "Credit card plans",
    "Institutions",
    "Standardized purchase APR, mean (SD), %",
    "Standardized purchase APR, median, %",
    "Standardized purchase APR range, %",
    "Secured card",
    "Introductory APR offered",
    "Balance transfer offered",
    "Cashback rewards reported",
    "Travel rewards reported",
    "Other rewards reported",
    "APR varies by credit tier",
    "Periodic fee reported",
    "Targets 1 credit tier",
    "Targets 2 credit tiers",
    "Targets 3 credit tiers",
    "Targets 4 credit tiers"
  ),
  
  Value = c(
    nrow(model3_data),
    length(unique(model3_data$`Institution Name`)),
    
    sprintf(
      "%.2f (%.2f)",
      mean(model3_data$apr_620_719_clean_pct),
      sd(model3_data$apr_620_719_clean_pct)
    ),
    
    sprintf(
      "%.2f",
      median(model3_data$apr_620_719_clean_pct)
    ),
    
    sprintf(
      "%.2f–%.2f",
      min(model3_data$apr_620_719_clean_pct),
      max(model3_data$apr_620_719_clean_pct)
    ),
    
    sprintf("%d (%.1f%%)",
            sum(model3_data$secured == 1),
            mean(model3_data$secured == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model3_data$intro_apr == 1),
            mean(model3_data$intro_apr == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model3_data$balance_transfer == 1),
            mean(model3_data$balance_transfer == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model3_data$cashback == 1),
            mean(model3_data$cashback == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model3_data$travel_rewards == 1),
            mean(model3_data$travel_rewards == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model3_data$other_rewards == 1),
            mean(model3_data$other_rewards == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model3_data$apr_varies_credit == 1),
            mean(model3_data$apr_varies_credit == 1) * 100),
    
    sprintf("%d (%.1f%%)",
            sum(model3_data$periodic_fee == 1),
            mean(model3_data$periodic_fee == 1) * 100),
    
    sprintf("%d (%.1f%%)", 93, 93/620*100),
    sprintf("%d (%.1f%%)", 292, 292/620*100),
    sprintf("%d (%.1f%%)", 145, 145/620*100),
    sprintf("%d (%.1f%%)", 90, 90/620*100)
  )
)

as.data.frame(table1_final)

##Update figure 1
plot_data3 <- final_model3_table %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Variable_Label = case_when(
      Variable == "secured" ~ "Secured card",
      Variable == "intro_apr" ~ "Introductory APR offered",
      Variable == "balance_transfer" ~ "Balance transfer offered",
      Variable == "cashback" ~ "Cashback rewards",
      Variable == "travel_rewards" ~ "Travel rewards",
      Variable == "other_rewards" ~ "Other rewards",
      Variable == "apr_varies_credit" ~ "APR varies by credit tier",
      Variable == "periodic_fee" ~ "Periodic fee reported",
      Variable == "credit_tier_count2" ~ "Targets 2 credit tiers",
      Variable == "credit_tier_count3" ~ "Targets 3 credit tiers",
      Variable == "credit_tier_count4" ~ "Targets 4 credit tiers",
      TRUE ~ Variable
    )
  )

ggplot(
  plot_data3,
  aes(
    x = Estimate,
    y = reorder(Variable_Label, Estimate)
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(xmin = CI_Lower, xmax = CI_Upper),
    width = 0.2
  ) +
  geom_point(size = 2.5) +
  labs(
    title = "Adjusted Associations Between Credit Card Features and Purchase APR",
    subtitle = "95% CIs based on standard errors clustered by institution",
    x = "Estimated difference in standardized purchase APR (percentage points)",
    y = NULL,
    caption = "N = 620 credit card plans; 172 institutions. Reference for credit-tier count = 1 tier."
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "CFPB_APR_Final_Coefficient_Plot.png",
  width = 9,
  height = 6,
  dpi = 300
)

##Step 30 — Figure 2: Distribution of standardized purchase APR
library(ggplot2)

mean_apr3 <- mean(model3_data$apr_620_719_clean_pct)
median_apr3 <- median(model3_data$apr_620_719_clean_pct)

figure2 <- ggplot(
  model3_data,
  aes(x = apr_620_719_clean_pct)
) +
  geom_histogram(
    binwidth = 2,
    boundary = 0,
    color = "white"
  ) +
  geom_vline(
    xintercept = mean_apr3,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = median_apr3,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = mean_apr3,
    y = Inf,
    label = paste0("Mean = ", round(mean_apr3, 2), "%"),
    vjust = 1.8,
    hjust = 1.05,
    size = 3.5
  ) +
  annotate(
    "text",
    x = median_apr3,
    y = Inf,
    label = paste0("Median = ", round(median_apr3, 2), "%"),
    vjust = 3.5,
    hjust = -0.05,
    size = 3.5
  ) +
  labs(
    title = "Distribution of Standardized Purchase APR",
    subtitle = "Credit card plans in the primary analytical sample",
    x = "Standardized purchase APR (%)",
    y = "Number of credit card plans",
    caption = "N = 620 credit card plans from 172 institutions."
  ) +
  theme_minimal(base_size = 12)

figure2

ggsave(
  "Figure_2_APR_Distribution.png",
  plot = figure2,
  width = 9,
  height = 6,
  dpi = 300
)

##Step 31 — Figure 3: APR by number of targeted credit tiers
tier_plot_data <- model3_data %>%
  mutate(
    tier_group = factor(
      credit_tier_count,
      levels = c("1", "2", "3", "4"),
      labels = c(
        "1 tier",
        "2 tiers",
        "3 tiers",
        "4 tiers"
      )
    )
  )

figure3 <- ggplot(
  tier_plot_data,
  aes(
    x = tier_group,
    y = apr_620_719_clean_pct
  )
) +
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.25,
    size = 1.2
  ) +
  labs(
    title = "Standardized Purchase APR by Number of Targeted Credit Tiers",
    subtitle = "Unadjusted distributions across credit card plans",
    x = "Number of targeted credit tiers",
    y = "Standardized purchase APR (%)",
    caption = "N = 620 plans. Groups contain 93, 292, 145, and 90 plans, respectively."
  ) +
  theme_minimal(base_size = 12)

figure3

ggsave(
  "Figure_3_APR_by_Credit_Tiers.png",
  plot = figure3,
  width = 9,
  height = 6,
  dpi = 300
)

##Step 32A — Check the zero-APR observations
zero_apr_cases <- model3_data %>%
  filter(apr_620_719_clean_pct == 0)

nrow(zero_apr_cases)

zero_apr_cases %>%
  select(
    `Institution Name`,
    apr_620_719_clean_pct,
    secured,
    intro_apr,
    balance_transfer,
    travel_rewards,
    periodic_fee,
    credit_tier_count
  ) %>%
  as.data.frame()

model3_nozero <- model3_data %>%
  filter(apr_620_719_clean_pct > 0)

model3_sensitivity <- lm(
  apr_620_719_clean_pct ~
    secured +
    intro_apr +
    balance_transfer +
    cashback +
    travel_rewards +
    other_rewards +
    apr_varies_credit +
    periodic_fee +
    credit_tier_count,
  data = model3_nozero
)

cluster_vcov_sensitivity <- cluster.vcov(
  model3_sensitivity,
  model3_nozero$`Institution Name`
)

cluster_sensitivity <- coeftest(
  model3_sensitivity,
  vcov. = cluster_vcov_sensitivity
)

nrow(model3_nozero)
length(unique(model3_nozero$`Institution Name`))

as.data.frame(cluster_sensitivity)

# Add the reported minimum and maximum APRs to Model 3
# using the same source rows used to construct the analysis dataset.

names(model3_data)

names(cfpb)[grepl("Purchase APR", names(cfpb))]

##Step 33 — Reconstruct and audit the standardized APR
cfpb_apr_audit <- cfpb %>%
  mutate(
    # Construct the same standardized APR used for Model 3
    apr_620_719 = case_when(
      `Purchase APR Vary by Credit Tier` == "Yes" ~ `Purchase APR good`,
      `Purchase APR Vary by Credit Tier` == "No"  ~ `Purchase APR median`,
      TRUE ~ NA_real_
    ),
    
    # Treat impossible decimal APR values as invalid
    apr_620_719_clean = case_when(
      is.na(apr_620_719) ~ NA_real_,
      apr_620_719 < 0 ~ NA_real_,
      apr_620_719 > 1 ~ NA_real_,
      TRUE ~ apr_620_719
    ),
    
    apr_620_719_clean_pct = apr_620_719_clean * 100,
    
    # Check whether standardized APR falls outside
    # the reported overall purchase APR range
    apr_range_check = case_when(
      is.na(apr_620_719_clean) ~ "Missing/Invalid",
      is.na(`Purchase APR min`) | is.na(`Purchase APR max`) ~
        "Range unavailable",
      apr_620_719_clean < `Purchase APR min` ~ "Below minimum",
      apr_620_719_clean > `Purchase APR max` ~ "Above maximum",
      TRUE ~ "Within range"
    )
  )

cfpb_apr_audit %>%
  count(apr_range_check) %>%
  as.data.frame()

cfpb_apr_audit %>%
  filter(apr_range_check %in% c("Below minimum", "Above maximum")) %>%
  select(
    `Institution Name`,
    `Product Name`,
    `Purchase APR Vary by Credit Tier`,
    `Purchase APR good`,
    `Purchase APR min`,
    `Purchase APR median`,
    `Purchase APR max`,
    apr_620_719,
    apr_range_check
  ) %>%
  as.data.frame()

cfpb_apr_audit %>%
  summarise(
    Total = n(),
    Valid_APR = sum(!is.na(apr_620_719_clean)),
    Missing_or_Invalid = sum(is.na(apr_620_719_clean)),
    Mean_APR = mean(apr_620_719_clean_pct, na.rm = TRUE),
    Median_APR = median(apr_620_719_clean_pct, na.rm = TRUE),
    Min_APR = min(apr_620_719_clean_pct, na.rm = TRUE),
    Max_APR = max(apr_620_719_clean_pct, na.rm = TRUE)
  ) %>%
  as.data.frame()

names(cfpb)[grepl(
  "Targeted Credit|Secured Card|Introductory APR|Balance Transfer Offered|Periodic Fee Type|Rewards",
  names(cfpb)
)]
table(cfpb_apr_audit$apr_range_check, useNA = "ifany")


##Step 34 — Exclude the 11 range-inconsistent records and rerun Model 3
library(dplyr)
library(lmtest)
library(multiwayvcov)

# Rebuild predictors directly from the original CFPB rows
range_sensitivity_data <- cfpb_apr_audit %>%
  mutate(
    secured = case_when(
      `Secured Card` == "Yes" ~ 1,
      `Secured Card` == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    
    intro_apr = case_when(
      `Introductory APR Offered?` == "Yes" ~ 1,
      `Introductory APR Offered?` == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    
    balance_transfer = case_when(
      `Balance Transfer Offered?` == "Yes" ~ 1,
      `Balance Transfer Offered?` == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    
    cashback = if_else(
      grepl("Cashback", Rewards),
      1, 0,
      missing = 0
    ),
    
    travel_rewards = if_else(
      grepl("Travel-related", Rewards),
      1, 0,
      missing = 0
    ),
    
    other_rewards = if_else(
      grepl("Other rewards", Rewards),
      1, 0,
      missing = 0
    ),
    
    apr_varies_credit = case_when(
      `Purchase APR Vary by Credit Tier` == "Yes" ~ 1,
      `Purchase APR Vary by Credit Tier` == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    
    periodic_fee = if_else(
      !is.na(`Periodic Fee Type`),
      1, 0
    ),
    
    target_no_score = if_else(
      grepl("No credit score", `Targeted Credit Tiers`,
            ignore.case = TRUE),
      1, 0,
      missing = 0
    ),
    
    target_619 = if_else(
      grepl("619", `Targeted Credit Tiers`),
      1, 0,
      missing = 0
    ),
    
    target_620_719 = if_else(
      grepl("620", `Targeted Credit Tiers`),
      1, 0,
      missing = 0
    ),
    
    target_720 = if_else(
      grepl("720", `Targeted Credit Tiers`),
      1, 0,
      missing = 0
    ),
    
    number_credit_tiers =
      target_no_score +
      target_619 +
      target_620_719 +
      target_720,
    
    credit_tier_count = factor(
      number_credit_tiers,
      levels = c(1, 2, 3, 4)
    )
  ) %>%
  
  # Exclude the 11 records identified by the range audit
  filter(apr_range_check != "Above maximum") %>%
  
  # Keep valid standardized APR observations
  filter(!is.na(apr_620_719_clean_pct)) %>%
  
  # Keep complete observations for the same Model 3 predictors
  filter(
    complete.cases(
      secured,
      intro_apr,
      balance_transfer,
      cashback,
      travel_rewards,
      other_rewards,
      apr_varies_credit,
      periodic_fee,
      credit_tier_count
    )
  )

nrow(range_sensitivity_data)

length(unique(
  range_sensitivity_data$`Institution Name`
))

table(
  range_sensitivity_data$credit_tier_count,
  useNA = "ifany"
)

model3_range_sensitivity <- lm(
  apr_620_719_clean_pct ~
    secured +
    intro_apr +
    balance_transfer +
    cashback +
    travel_rewards +
    other_rewards +
    apr_varies_credit +
    periodic_fee +
    credit_tier_count,
  data = range_sensitivity_data
)

cluster_vcov_range <- cluster.vcov(
  model3_range_sensitivity,
  range_sensitivity_data$`Institution Name`
)

cluster_range <- coeftest(
  model3_range_sensitivity,
  vcov. = cluster_vcov_range
)

cluster_range

summary(model3_range_sensitivity)$r.squared
summary(model3_range_sensitivity)$adj.r.squared
