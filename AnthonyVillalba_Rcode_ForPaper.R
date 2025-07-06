# Load necessary libraries
library(readxl)
library(openxlsx)
library(tidyr)
library(dplyr)
library(plm)
library(lmtest)
library(stargazer)
library(car)
library(ggplot2)

# Load datasets (same for both approaches)
file1 <- read_excel("P_Data_Extract_From_World_Development_Indicators.xlsx")
file2 <- read_excel("CBDC_tracker_dataver2.xlsx")
file3 <- read_excel("API_SL.UEM.TOTL.ZS_DS2_en_excel_v2_76179.xlsx")
file4 <- read_excel("world_data_gdp_internet.xlsx")
file5 <- read_excel("bankdeposit_data.xlsx")
file6 <- read_excel("zscore_bank.xlsx")
file7 <- read_excel("advanced_emerging_economies.xlsx")
file8 <- read_excel("non_performing_loans.xlsx")

#-------------------------------------------------------------------------------
# DATA CLEANING
#-------------------------------------------------------------------------------

clean_file1 <- function(df) {
  df %>%
    select(-`Series Code`) %>%
    pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Bank_Liquidity_pct") %>%
    mutate(Bank_Liquidity_pct = ifelse(Bank_Liquidity_pct == "..", NA, Bank_Liquidity_pct))
}

clean_file3 <- function(df) {
  df %>%
    select(-c(`1960`:`2009`), -`Indicator Code`) %>%
    pivot_longer(cols = '2010':'2023', names_to = "Year", values_to = "Unemployment_pct")
}

clean_file4 <- function(df) {
  df %>%
    select(-`Series Code`, -`Country Name`) %>%
    rename_with(~ sub(" .*", "", .x), starts_with("20")) %>%
    pivot_longer(cols = `2010`:`2023`, names_to = "Year", values_to = "Value") %>%
    filter(`Series Name` %in% c("Individuals using the Internet (% of population)", "GDP per capita (constant 2015 US$)")) %>%
    pivot_wider(names_from = `Series Name`, values_from = "Value") %>%
    rename(Internet_Users_pct = `Individuals using the Internet (% of population)`,
           GDP_Per_Capita = `GDP per capita (constant 2015 US$)`) %>%
    mutate(across(c(Internet_Users_pct, GDP_Per_Capita), ~ ifelse(.x == "..", NA, .x)))
}

clean_long_file <- function(df, series_names, new_names) {
  df %>%
    pivot_longer(cols = `2010`:`2021`, names_to = "Year", values_to = "Value") %>%
    filter(`Series Name` %in% series_names) %>%
    pivot_wider(names_from = `Series Name`, values_from = "Value") %>%
    rename(!!new_names[1] := !!series_names[1], !!new_names[2] := !!series_names[2]) %>%
    mutate(across(all_of(new_names), ~ ifelse(.x == "..", NA, .x)))
}

# Apply cleaning functions
file1 <- clean_file1(file1)
file3 <- clean_file3(file3)
file4 <- clean_file4(file4)
file5_long <- clean_long_file(file5, 
                              c("Bank deposits to GDP (%)", "Deposit money banks assets to GDP (%)"),
                              c("Bank_deposit_pct", "Deposit_to_GDP_pct"))
file6_long <- clean_long_file(file6,
                              c("Bank Z-score", "Central bank assets to GDP (%)"),
                              c("Bank_Zscore", "Bank_Assets_to_GDP_pct"))
file7 <- file7 %>% select(-`Country Name`) %>% filter(!is.na(`Economic Level`))
file8 <- file8 %>%
  pivot_longer(cols = `2010`:`2023`, names_to = "Year", values_to = "NPL") %>%
  mutate(NPL = ifelse(NPL == "..", NA, NPL))

# Merge all datasets
merged_data <- file1 %>%
  left_join(file3, by = c("Country Code", "Year")) %>%
  left_join(file4, by = c("Country Code", "Year")) %>%
  left_join(file5_long, by = c("Country Code", "Year")) %>%
  left_join(file6_long, by = c("Country Code", "Year")) %>%
  left_join(file8, by = c("Country Code", "Year")) %>%
  left_join(file7, by = "Country Code") %>% 
  left_join(file2, by = "Country Code") %>%
  rename(CBDC_Adoption = Status) %>%
  mutate(CBDC_Adoption = ifelse(CBDC_Adoption == "Cancelled", "No_Adoption", CBDC_Adoption)) %>%
  select(-`Country Name.y`, -`Country Name.x`) %>%
  mutate(across(c(Bank_Liquidity_pct, GDP_Per_Capita, Bank_deposit_pct, Unemployment_pct,
                  Deposit_to_GDP_pct, Internet_Users_pct, Bank_Assets_to_GDP_pct, Bank_Zscore, NPL), as.numeric)) %>%
  filter(!is.na(`Country`), !is.na(`Bank_Liquidity_pct`), !is.na(Bank_deposit_pct),
         !is.na(Bank_Zscore), !is.na(Internet_Users_pct)) %>%
  mutate(CBDC_Adoption = case_when(
    Year < `Announcement Year` ~ "No_Adoption",
    TRUE ~ CBDC_Adoption
  )) %>%
  as.data.frame()

#-------------------------------------------------------------------------------
# APPROACH 1: FACTORIAL/CATEGORICAL
#-------------------------------------------------------------------------------

data_factorial <- merged_data %>%
  mutate(
    CBDC_Adoption = case_when(
      CBDC_Adoption == "No_Adoption" ~ "No_Adoption",
      CBDC_Adoption %in% c("Research", "Proof of concept") ~ "Early_Stage",
      CBDC_Adoption %in% c("Pilot", "Launched") ~ "Advanced_Stage",
      TRUE ~ NA_character_
    ),
    CBDC_Adoption = factor(CBDC_Adoption, 
                           levels = c("No_Adoption", "Early_Stage", "Advanced_Stage"))
  ) %>%
  select(-`Series Name`, -`Indicator Name`, -`Country Name`)

# Save cleaned data
write.xlsx(data_factorial, "cleaned_data_factorial.xlsx", overwrite = TRUE)

# Regression functions country and time fixed effects
run_fe_model <- function(formula, data) {
  plm(formula, data = data, effect = "twoways", model = "within", index = c("Country", "Year"))
}

get_robust_se <- function(model) {
  sqrt(diag(vcovHC(model, type = "HC1")))
}

# Regression functions Country fixed effects
run_ce_model <- function(formula, data) {
  plm(formula, data = data, model = "within", index = c("Country", "Year"))
}

# Model formulas for factorial approach
formula_f1 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita
formula_f2 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct
formula_f3 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct
formula_f4 <- Bank_Liquidity_pct ~ CBDC_Adoption*GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct

# Run models
model_f1 <- run_fe_model(formula_f1, data_factorial)
model_f2 <- run_fe_model(formula_f2, data_factorial)
model_f3 <- run_fe_model(formula_f3, data_factorial)
model_f4 <- run_fe_model(formula_f4, data_factorial)

# Get robust standard errors
robust_se_f1 <- get_robust_se(model_f1)
robust_se_f2 <- get_robust_se(model_f2)
robust_se_f3 <- get_robust_se(model_f3)
robust_se_f4 <- get_robust_se(model_f4)

# Create results table
stargazer(
  model_f1, model_f2, model_f3, model_f4,
  order = c("CBDC_Adoption", "GDP_Per_Capita", "Deposit_to_GDP_pct", "Bank_Assets_to_GDP_pct"),
  se = list(robust_se_f1, robust_se_f2, robust_se_f3, robust_se_f4),
  type = "text",
  title = "Fixed-Effects Regression Results (Factorial CBDC Adoption)",
  dep.var.labels = "Bank Liquidity (%)",
  out = "regression_results_factorial.html"
)

#-------------------------------------------------------------------------------
# APPROACH 2: INDEX APPROACH (0-4)
#-------------------------------------------------------------------------------

data_index <- merged_data %>%
  mutate(
    CBDC_Adoption = case_when(
      CBDC_Adoption == "No_Adoption" ~ 0,
      CBDC_Adoption == "Research" ~ 1,
      CBDC_Adoption == "Proof of concept" ~ 2,
      CBDC_Adoption == "Pilot" ~ 3,
      CBDC_Adoption == "Launched" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-`Series Name`, -`Indicator Name`, -`Country Name`)

# Save cleaned data
write.xlsx(data_index, "cleaned_data_index.xlsx", overwrite = TRUE)

# Model formulas for index approach
formula_i1 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita
formula_i2 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct
formula_i3 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct
formula_i4 <- Bank_Liquidity_pct ~ CBDC_Adoption*GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct

# Run models
model_i1 <- run_fe_model(formula_i1, data_index)
model_i2 <- run_fe_model(formula_i2, data_index)
model_i3 <- run_fe_model(formula_i3, data_index)
model_i4 <- run_fe_model(formula_i4, data_index)

# Get robust standard errors
robust_se_i1 <- get_robust_se(model_i1)
robust_se_i2 <- get_robust_se(model_i2)
robust_se_i3 <- get_robust_se(model_i3)
robust_se_i4 <- get_robust_se(model_i4)

# Create results table
stargazer(
  model_i1, model_i2, model_i3, model_i4,
  order = c("CBDC_Adoption", "GDP_Per_Capita", "Deposit_to_GDP_pct", "Bank_Assets_to_GDP_pct"),
  se = list(robust_se_i1, robust_se_i2, robust_se_i3, robust_se_i4),
  type = "text",
  title = "Fixed-Effects Regression Results (Index CBDC Adoption 0-4)",
  dep.var.labels = "Bank Liquidity (%)",
  out = "regression_results_index.html"
)

#-------------------------------------------------------------------------------
# VISUALIZATION
#-------------------------------------------------------------------------------

# Plot for factorial approach
ggplot(data_factorial, aes(x = GDP_Per_Capita, y = Bank_Liquidity_pct, color = CBDC_Adoption)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Bank Liquidity by CBDC Adoption Stage (Factorial)",
       x = "GDP Per Capita",
       y = "Bank Liquidity (%)",
       color = "CBDC Stage") +
  theme_minimal()

# Plot for index approach
ggplot(data_index, aes(x = CBDC_Adoption, y = Bank_Liquidity_pct)) +
  geom_boxplot() +
  labs(title = "Bank Liquidity by CBDC Adoption Index (0-4)",
       x = "CBDC Adoption Index",
       y = "Bank Liquidity (%)") +
  theme_minimal()

#-------------------------------------------------------------------------------
# ECONOMIC LEVEL ANALYSIS
#-------------------------------------------------------------------------------

# Split data
data_advanced <- subset(data_index, `Economic Level` == "Advanced")
data_emerging <- subset(data_index, `Economic Level` == "Emerging/Developing")

# Advanced economies model
model_adv <- run_fe_model(Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct +
                            Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct, data_advanced)
robust_se_adv <- get_robust_se(model_adv)

# Emerging economies model
model_emg <- run_fe_model(Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct +
                            Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct, data_emerging)
robust_se_emg <- get_robust_se(model_emg)

# Compare results
stargazer(
  model_adv, model_emg,
  se = list(robust_se_adv, robust_se_emg),
  type = "text",
  title = "CBDC Impact by Economic Level (Factorial Approach)",
  column.labels = c("Advanced Economies", "Emerging Economies"),
  out = "economic_level_comparisonfact.html"
)

# Advanced economies model with interaction
model_adv_int <- run_fe_model(Bank_Liquidity_pct ~ CBDC_Adoption*GDP_Per_Capita + Deposit_to_GDP_pct +
                            Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct, data_advanced)
robust_se_adv_int <- get_robust_se(model_adv_int)

# Emerging economies model with interaction
model_emg_int <- run_fe_model(Bank_Liquidity_pct ~ CBDC_Adoption*GDP_Per_Capita + Deposit_to_GDP_pct +
                            Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct, data_emerging)
robust_se_emg_int <- get_robust_se(model_emg_int)

# Compare results
stargazer(
  model_adv_int, model_emg_int,
  se = list(robust_se_adv_int, robust_se_emg_int),
  type = "text",
  title = "CBDC Impact by Economic Level (Factorial Approach)",
  column.labels = c("Advanced Economies", "Emerging Economies"),
  out = "economic_level_comparisonindex.html"
)

#-------------------------------------------------------------------------------
# Bank Zscore as Y
#-------------------------------------------------------------------------------
# Model formulas 
formula_z1 <- Bank_Zscore ~ CBDC_Adoption + GDP_Per_Capita
formula_z2 <- Bank_Zscore ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct
formula_z3 <- Bank_Zscore ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct
formula_z4 <- Bank_Zscore ~ CBDC_Adoption*GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct

# Run models
model_z1 <- run_fe_model(formula_z1, data_index)
model_z2 <- run_fe_model(formula_z2, data_index)
model_z3 <- run_fe_model(formula_z3, data_index)
model_z4 <- run_fe_model(formula_z4, data_index)

# Get robust standard errors
robust_se_z1 <- get_robust_se(model_z1)
robust_se_z2 <- get_robust_se(model_z2)
robust_se_z3 <- get_robust_se(model_z3)
robust_se_z4 <- get_robust_se(model_z4)

# Create results table
stargazer(
  model_z1, model_z2, model_z3, model_z4,
  order = c("CBDC_Adoption", "GDP_Per_Capita", "Deposit_to_GDP_pct", "Bank_Assets_to_GDP_pct"),
  se = list(robust_se_z1, robust_se_z2, robust_se_z3, robust_se_z4),
  type = "text",
  title = "Fixed-Effects Regression Results (Index CBDC Adoption 0-4)",
  dep.var.labels = "Bank Zscore",
  out = "regression_BankZscore.html"
)

# Advanced economies model
model_adv_z <- run_fe_model(Bank_Zscore ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct +
                            Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct, data_advanced)
robust_se_adv_z <- get_robust_se(model_adv_z)

# Emerging economies model
model_emg_z <- run_fe_model(Bank_Zscore ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct +
                            Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct, data_emerging)
robust_se_emg_z <- get_robust_se(model_emg_z)

# Compare results
stargazer(
  model_adv_z, model_emg_z,
  se = list(robust_se_adv_z, robust_se_emg_z),
  type = "text",
  title = "CBDC Impact by Economic Level (Factorial Approach)",
  column.labels = c("Advanced Economies", "Emerging Economies"),
  out = "economic_level_BankZscore.html"
)

#-------------------------------------------------------------------------------
# Only Within Effects Model
#-------------------------------------------------------------------------------

# Model formulas for index approach
formula_c1 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita
formula_c2 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct
formula_c3 <- Bank_Liquidity_pct ~ CBDC_Adoption + GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct
formula_c4 <- Bank_Liquidity_pct ~ CBDC_Adoption*GDP_Per_Capita + Deposit_to_GDP_pct + Bank_Assets_to_GDP_pct + Bank_deposit_pct + Unemployment_pct + Internet_Users_pct

# Run models
model_c1 <- run_ce_model(formula_c1, data_index)
model_c2 <- run_ce_model(formula_c2, data_index)
model_c3 <- run_ce_model(formula_c3, data_index)
model_c4 <- run_ce_model(formula_c4, data_index)

# Get robust standard errors
robust_se_c1 <- get_robust_se(model_c1)
robust_se_c2 <- get_robust_se(model_c2)
robust_se_c3 <- get_robust_se(model_c3)
robust_se_c4 <- get_robust_se(model_c4)

# Create results table
stargazer(
  model_c1, model_c2, model_c3, model_c4,
  order = c("CBDC_Adoption", "GDP_Per_Capita", "Deposit_to_GDP_pct", "Bank_Assets_to_GDP_pct"),
  se = list(robust_se_c1, robust_se_c2, robust_se_c3, robust_se_c4),
  type = "text",
  title = "Country Fixed-Effects Regression Results (Index CBDC Adoption 0-4)",
  dep.var.labels = "Bank Liquidity (%)",
  out = "regression_withinEffects.html"
)
