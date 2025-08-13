# Packages needed===============================================================
library(readr)
library(dplyr)
library(tidyr)
#===============================================================================

# MEASUREMENT DATA CLEANING=====================================================

# Climate Awareness-------------------------------------------------------------
# Read the data
aware <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/climate_aware.csv")

# Remove the last row
aware <- head(aware, -1)

# Remove the entity and year columns
aware <- subset(aware, select = c(-Year, -Entity))

# Rename the remaining columns
## "Code" -> "ISO3"
aware <- aware %>%
  rename(ISO3 = Code)

## "Believe climate ..." -> "Awareness"
aware <- aware %>% 
  rename(Awareness = `Believe climate change is a serious threat to humanity`)

# Save the cleaned data
write_csv(aware, "climate_aware_cleaned.csv")
#-------------------------------------------------------------------------------

# Expenditure on Environment Protections ---------------------------------------
# Read the data
expenditure <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/env_expend.csv")

# Remove irrelevant column`
expenditure <- subset(expenditure, select = c(ISO3, Indicator, `2021`))

expenditure <- expenditure %>%
  rename(Expenditure = `2021`)

# Further cleaning:
expenditure <- expenditure %>%
  filter(Indicator == "Expenditure on environment protection") %>%
  group_by(ISO3) %>%
  summarise(
    Total_Expenditure = sum(Expenditure, na.rm = TRUE),
    Sources_Count = sum(!is.na(Expenditure)),
    Total_Expenditure = ifelse(Sources_Count == 0, NA, Total_Expenditure),
    .groups = "drop"
  )

# Remove the NA counts and rename the column
expenditure <- expenditure %>% 
  filter(!is.na(ISO3)) %>%
  select(-Sources_Count) %>%
  rename(Expenditure = Total_Expenditure) 

# Save the cleaned data
write_csv(expenditure, "env_expenditure_cleaned.csv")
#-------------------------------------------------------------------------------

# Policy Effectiveness ---------------------------------------------------------
# Bioclimatic Ecosystem Resilience
ber <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/effectiveness(env_performance)/BER.csv") %>%
  select(iso, BER.raw.2020) %>%
  rename(ISO3 = iso) %>%
  rename(Bioclimatic = BER.raw.2020)
write_csv(ber, "ber_cleaned.csv")

# Carbon dioxide emissions
cda <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/effectiveness(env_performance)/CDA.csv") %>%
  select(iso, CDA.raw.2022) %>%
  rename(ISO3 = iso) %>%
  rename(CO2 = CDA.raw.2022)
write_csv(cda, "cda_cleaned.csv")

# F-gases
fga <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/effectiveness(env_performance)/FGA.csv") %>%
  select(iso, FGA.raw.2022) %>%
  rename(ISO3 = iso) %>%
  rename(`F-gases` = FGA.raw.2022)
write_csv(fga, "fga_cleaned.csv")

# Nitrous oxide
nxa <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/effectiveness(env_performance)/NXA.csv") %>%
  select(iso, NXA.raw.2022) %>%
  rename(ISO3 = iso) %>%
  rename(NO = NXA.raw.2022)
write_csv(nxa, "nxa_cleaned.csv")

# Species habitat
shi <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/effectiveness(env_performance)/SHI.csv") %>%
  select(iso, SHI.raw.2022) %>%
  rename(ISO3 = iso) %>%
  rename(Habitat = SHI.raw.2022)
write_csv(shi, "shi_cleaned.csv")

#-------------------------------------------------------------------------------

# Public Demand of More Government Actions -------------------------------------
# Read the dataset
more_actions <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/more_actions.csv")

# Cleaning
more_actions <- more_actions %>% 
  filter(!Entity == "World") %>%
  select(Code, `Government action on climate`) %>% ## select relevant columns
  rename(ISO3 = Code) %>%
  rename(Insufficient = `Government action on climate`)

# Save the cleaned data
write_csv(more_actions, "more_actions_cleaned.csv")
# ------------------------------------------------------------------------------

# Political Participation ------------------------------------------------------
# Read the data
participation <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/poli_participation.csv")

# Cleaning
participation <- participation %>%
  filter(!is.na(Code)) %>% ## exclude rows of regions (their country code is NA)
  filter(Year == 2024) %>% ## keep latest data only
  select(Code, `Political participation`) %>% ## keep relevant variables
  rename(ISO3 = Code) %>% 
  rename(Participation = `Political participation`) %>%
  slice(-165) ## row 165 is the world average

# save the cleaned data
write_csv(participation, "poli_participation_cleaned.csv")
#-------------------------------------------------------------------------------

# Distribution of Political Power ----------------------------------------------
# Read the data
power <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/poli_power.csv")

# Cleaning
power <- power %>%
  filter(!is.na(Code)) %>% 
  filter(Year == 2024) %>%
  filter(!Code == "OWID_WRL") %>%
  select(Code, `Equality of political power across social groups (central estimate)`) %>%
  rename(ISO3 = Code) %>%
  rename(PoliEqual = `Equality of political power across social groups (central estimate)`)

# Save the cleaned data
write_csv(power, "poli_equal_cleaned.csv")
# ------------------------------------------------------------------------------

# Number of Policies by Country ------------------------------------------------
# Read the data
policy <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/policy_rec.csv")

# Count the number of policies for each country
policy_counts <- policy %>% 
  count(country_iso, name = "Policies") %>%
  rename(ISO3 = country_iso) 

# Save the cleaned data
write_csv(policy_counts, "policy_counts_cleaned.csv")
# ------------------------------------------------------------------------------

# Support of Environmental Policy ----------------------------------------------
# Read the data
support <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/policy_support.csv")

# Cleaning
support <- support %>%
  filter(!Entity == "World") %>%
  select(Code, `Support political action on climate`) %>%
  rename(ISO3 = Code) %>%
  rename(Support = `Support political action on climate`)

# Save the cleaned data
write_csv(support, "policy_support_cleaned.csv")
# ------------------------------------------------------------------------------

# Power Concentration ----------------------------------------------------------
# Read the data
central <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/power_central.csv")

# Cleaning
central <- central %>%
  filter(!is.na(Code)) %>%
  filter(!Entity == "World") %>%
  filter(Year == 2024) %>%
  select(Code, `Presidentialism index (central estimate)`) %>%
  rename(ISO3 = Code) %>%
  rename(PowerCentral = `Presidentialism index (central estimate)`)

# Save the cleaned data
write_csv(central, "power_central_cleaned.csv")
# ------------------------------------------------------------------------------

#===============================================================================


# ROBUSTNESS CHECK DATA CLEANING================================================

# Environmental Accounting (Binary)---------------------------------------------
envacc <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/robustness/environmental_accounting_raw.csv")
latent_factors <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/2_Modeling/latent_factors.csv")

envacc <- envacc %>%
  select(Code, `15.9.1 - Countries with integrated biodiversity values into national accounting and reporting systems, defined as implementation of the System of Environmental-Economic Accounting  (1 = YES; 0 = NO) - ER_BDY_SEEA - No breakdown`) %>%
  rename(ISO3 = Code) %>%
  rename(Env_Account = `15.9.1 - Countries with integrated biodiversity values into national accounting and reporting systems, defined as implementation of the System of Environmental-Economic Accounting  (1 = YES; 0 = NO) - ER_BDY_SEEA - No breakdown`) %>%
  full_join(latent_factors, by = "ISO3") %>%
  select(ISO3, Env_Account) %>%
  mutate(Env_Account = replace_na(Env_Account, 0))

write_csv(envacc, "environmental_accounting_cleaned.csv")
# ------------------------------------------------------------------------------

# Forest Area ------------------------------------------------------------------
forest <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/robustness/forest_area_raw.csv")

forest <- forest %>%
  filter(!is.na(Code), Year == 2020) %>%
  select(Code, `Share of land covered by forest`) %>%
  rename(ISO3 = Code) %>%
  rename(Forest = `Share of land covered by forest`)

write_csv(forest, "forest_area_cleaned.csv")
# ------------------------------------------------------------------------------

# Sustainable companies --------------------------------------------------------
sc <- sustainable_companies_raw <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/a_raw/robustness/sustainable_companies_raw.csv")

sc <- sc %>% 
  filter(!is.na(Code), Year == 2022) %>% 
  select(Code,`12.6.1 - Number of companies publishing sustainability reports with disclosure by dimension, by level of requirement (Number) - EN_SCP_FRMN - No breakdown` ) %>%
  rename(ISO3 = Code) %>%
  rename(Sus_Comp = `12.6.1 - Number of companies publishing sustainability reports with disclosure by dimension, by level of requirement (Number) - EN_SCP_FRMN - No breakdown`) %>% 
  full_join(latent_factors, by = "ISO3") %>%
  select(ISO3, Sus_Comp) %>%
  mutate(Sus_Comp = replace_na(Sus_Comp, 0))

write_csv(sc, "sustaimable_companies.csv")
# ------------------------------------------------------------------------------

#===============================================================================


# DATA AGGREGATION==============================================================

# Measurement Data---------------------------------------------------------------
ber <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/ber_cleaned.csv")

cda <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/cda_cleaned.csv")

aware <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/climate_aware_cleaned.csv")

expend <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/env_expenditure_cleaned.csv")

fga <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/fga_cleaned.csv")

more <-  read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/more_actions_cleaned.csv")

nxa <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/nxa_cleaned.csv")

equal <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/poli_equal_cleaned.csv")

particip <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/poli_participation_cleaned.csv")

policy <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/policy_counts_cleaned.csv")

support <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/policy_support_cleaned.csv")

central <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/power_central_cleaned.csv")

shi <- read.csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/shi_cleaned.csv")

measurement_data <- ber %>%
  full_join(aware, by = "ISO3") %>%
  full_join(cda, by = "ISO3") %>%
  full_join(central, by = "ISO3") %>%
  full_join(equal, by = "ISO3") %>%
  full_join(expend, by = "ISO3") %>%
  full_join(fga, by = "ISO3") %>%
  full_join(more, by = "ISO3") %>%
  full_join(nxa, by = "ISO3") %>%
  full_join(particip, by = "ISO3") %>%
  full_join(policy, by = "ISO3") %>%
  full_join(shi, by = "ISO3") %>%
  full_join(support, by = "ISO3")
measurement_data <- measurement_data[1:220,]
write_csv(measurement_data, "measurement_items_aggregate.csv")
# ------------------------------------------------------------------------------

# Robustness Data --------------------------------------------------------------
environmental_accounting<- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/environmental_accounting_cleaned.csv")

forest <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/forest_area_cleaned.csv")

sc <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/b_cleaned/sustaimable_companies.csv")

latent_factors <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/2_Modeling/latent_factors.csv")

## aggregation
robustness_data <- latent_factors %>%
  select(ISO3, EAI_P, EAI_D) %>%
  full_join(environmental_accounting, by = "ISO3") %>%
  full_join(forest, by = "ISO3") %>%
  full_join(pm25, by = "ISO3") %>%
  full_join(sc, by = "ISO3")

## further cleaning
robustness_data <- robustness_data [1:220,] %>%
  mutate(Forest = as.numeric(scale(Forest))) %>%
  mutate(PM25 = as.numeric(scale(PM25))) %>%
  mutate(Sus_Comp = as.numeric(scale(Sus_Comp)))

write_csv(robustness_data, "robustness_check.csv")



