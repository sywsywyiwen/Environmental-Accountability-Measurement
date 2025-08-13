# Packages =====================================================================
library(readr)
library(dplyr)
library(lavaan)
library(psych)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(blavaan)
library(ggpubr)
library(purrr)
# ==============================================================================

# Measurement ==================================================================
# Import the measurement dataset & standardize
data <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/c_aggregate/measurement_items_aggregate.csv")
data[,-1] <- scale(data[,-1])

# Measurement Breakdown
## Panel A: Achievement
bcfa.a <- bcfa(
  "f1 =~ CO2 + NO + F.gases + Bioclimatic + Habitat",
  data = data,
  n.chains = 2,
  burnin = 4000,
  sample = 6000,
  target = "stan",
  mcmcfile = FALSE
)
summary(bcfa.a)

prob <- scale(lavPredict(bcfa.a, method = "regression"))
data <- cbind(data, as.data.frame(prob)) %>%
  rename(Env_Prob = f1)

## Panel B: Government Quality
bcfa.b <- bcfa(
  "f2 =~ Expenditure + Policies + Env_Prob",
  data = data,
  n.chains = 2,
  burnin = 4000,
  sample = 6000,
  target = "stan",
  mcmcfile = FALSE
)
summary(bcfa.b)

geq <- scale(lavPredict(bcfa.b, method = "regression"))
data <- cbind(data, as.data.frame(geq)) %>%
  rename(GEQ = f2)

## Panel C: Public Interests
bcfa.c <- bcfa(
  "f3 =~ Insufficient + Awareness + Support",
  data = data,
  n.chains = 2,
  burnin = 4000,
  sample = 6000,
  target = "stan",
  mcmcfile = FALSE
)
summary(bcfa.c)

pi <- scale(lavPredict(bcfa.c, method = "regression"))
data <- cbind(data, as.data.frame(pi)) %>%
  rename(PI = f3)

## Panel D: Public Power
bcfa.d <- bcfa(
  "f4 =~ 1*PoliEqual + Participation + PowerCentral",
  data = data,
  n.chains = 2,
  burnin = 3000,
  sample = 5000,
  target = "stan",
  mcmcfile = FALSE
)
summary(bcfa.d)

pp <- scale(lavPredict(bcfa.d, method = "regression"))
data <- cbind(data, as.data.frame(pp)) %>%
  rename(PP = f4)

## Environmental Accountability Index (Participation Model)
bcfa.e <- bcfa(
  "f5 =~ GEQ + PI + PP",
  data = data,
  n.chains = 2,
  burnin = 4000,
  sample = 6000,
  target = "stan",
  mcmcfile = FALSE
)
summary(bcfa.e)
EAI_p <- scale(lavPredict(bcfa.e, method = "regression"))
data <- cbind(data, as.data.frame(EAI_p)) %>%
  rename(EAI_P = f5) 

## Environmental Accountability Index (Delegation Model)
bcfa.f <- bcfa(
  "f6 =~ Expenditure + Policies + Env_Prob + Insufficient",
  data = data,
  n.chains = 2,
  burnin = 3000,
  sample = 6000,
  target = "stan",
  mcmcfile = FALSE
)
summary(bcfa.f)
EAI_d <- scale(lavPredict(bcfa.f, method = "regression"))
data <- cbind(data, as.data.frame(EAI_d)) %>%
  rename(EAI_D = f6)

# Create the latent factors data
latent_data <- data %>%
  select(ISO3, Env_Prob, GEQ, PI, PP, EAI_P, EAI_D)
write_csv(latent_data, "latent_factors.csv")

latent_data <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/2_Modeling/latent_factors.csv")

# Summarize two indices
summary(latent_data$EAI_P)
summary(latent_data$EAI_D)

# ==============================================================================



# Visualizations ===============================================================

# Distribution -----------------------------------------------------------------
# EAI under participation model
## Density plot
ep_den <- ggplot(latent_data, aes(x = EAI_P)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Panel A. EA-P Index Distribution", 
       x = "", 
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, face = "bold")) 

## Boxplot
ep_box <- ggplot(latent_data, aes(x = EAI_P, y = -0.01)) +
  geom_boxplot(fill = "darkgrey", alpha = 0.5, width = 0.02) +
  scale_y_continuous(limits = c(-0.05, NA)) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

## Combine
ep_distribution <- ep_den + ep_box +  plot_layout(ncol = 1, heights = c(10, 1))

# EAI under participation model
## Density plot
ed_den <- ggplot(latent_data, aes(x = EAI_D)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "Panel B. EA-D Index Distribution", 
       x = "", 
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, face = "bold")) 

## Boxplot
ed_box <- ggplot(latent_data, aes(x = EAI_D, y = -0.01)) +
  geom_boxplot(fill = "darkgrey", alpha = 0.5, width = 0.02) +
  scale_y_continuous(limits = c(-0.05, NA)) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

## Combine
ed_distribution <- ed_den + ed_box +  plot_layout(ncol = 1, heights = c(10, 1))

# Present
wrap_plots(ep_distribution, ed_distribution, ncol = 2)
# ------------------------------------------------------------------------------

# Geographical Illustration ----------------------------------------------------
# Create the data for map
map_data <- latent_data

# Load world map (country polygons)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge EAI with world map data
world_EAI <- world %>% 
  left_join(map_data, by = c("iso_a3_eh" = "ISO3"))

# Create the EAI maps
EAI_P_map <- ggplot(world_EAI) + 
  geom_sf(aes(fill = EAI_P), color = "white", size = 0.2) + 
  scale_fill_viridis_c(option = "viridis", name = "Index") + 
  labs(title = "Panel A. EA-P Index") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "bottom"
  )

# Create the EAI maps
EAI_D_map <- ggplot(world_EAI) + 
  geom_sf(aes(fill = EAI_D), color = "white", size = 0.2) + 
  scale_fill_viridis_c(option = "viridis", name = "Index") + 
  labs(title = "Panel B. EA-D Index") + 
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "bottom"
  )

EAI_P_map / EAI_D_map
# ------------------------------------------------------------------------------

# ==============================================================================



# ROBUSTNESS CHECK =============================================================

# Import the data for robustness check
robust <- read_csv("~/Desktop/MSc@LSE/SRM/MY-Thesis/Data & Analysis/A_Measurement/1_Data/c_aggregate/robustness_check.csv")

# modeling the associations
summary(glm(Env_Account ~ EAI_P, data = robust, family = "binomial"))

summary(lm(Forest ~ EAI_P, data = robust))

summary(lm(Sus_Comp ~ EAI_P, data = robust))

summary(glm(Env_Account ~ EAI_D, data = robust, family = "binomial"))

summary(lm(Forest ~ EAI_D, data = robust))

summary(lm(Sus_Comp ~ EAI_D, data = robust))

# ==============================================================================



# DISCUSSIONS ==================================================================

# Discussion preparations ------------------------------------------------------
# Differences in scores
latent_data$EAI_Diff <- latent_data$EAI_P - latent_data$EAI_D

# Ranks
latent_data$rank_p <- rank(-latent_data$EAI_P, ties.method = "min")
latent_data$rank_d <- rank(-latent_data$EAI_D, ties.method = "min")
latent_data$rank_diff <- latent_data$rank_p - latent_data$rank_d

# Filter countries with prominent differences
dif_data <- latent_data %>%
  filter(abs(rank_diff) >= 55)

PD_data <- dif_data %>%
  filter(rank_diff < 0) ## EA-P>EA-D

DP_data <- dif_data %>%
  filter(rank_diff > 0) ## EA-D>EA-P
# ------------------------------------------------------------------------------

# Source of Inconsistency ------------------------------------------------------
# test the differences
t.test(PD_data$PP, DP_data$PP)
t.test(PD_data$Env_Prob, DP_data$Env_Prob)
t.test(PD_data$GEQ, DP_data$GEQ)

# ------------------------------------------------------------------------------

# Adjustment -------------------------------------------------------------------
# adjusted EA-P
latent_data$EA_P_a1 <- as.numeric(scale(latent_data$GEQ - 
                                          0.741*latent_data$PI + 
                                          1*latent_data$PP))
latent_data$EA_P_a1.5 <- as.numeric(scale(latent_data$GEQ - 
                                            0.741*latent_data$PI + 
                                            0.5*latent_data$PP))
latent_data$EA_P_a2 <- as.numeric(scale(latent_data$GEQ - 
                                          0.741*latent_data$PI + 
                                          0.25*latent_data$PP))

compare_data <- latent_data %>%
  filter(!is.na(EA_P_a1))

compare_data$rank_a1 <- rank(-compare_data$EA_P_a1, ties.method = "min")
compare_data$rank_a1.5 <- rank(-compare_data$EA_P_a1.5, ties.method = "min")
compare_data$rank_a2 <- rank(-compare_data$EA_P_a2, ties.method = "min")

compare_data$rank_diff_a1 <- compare_data$rank_a1 - compare_data$rank_d
compare_data$rank_diff_a1.5 <- compare_data$rank_a1.5 - compare_data$rank_d
compare_data$rank_diff_a2 <- compare_data$rank_a2 - compare_data$rank_d

sum(compare_data$rank_diff >= 55)
sum(compare_data$rank_diff_a1 >= 55)
sum(compare_data$rank_diff_a1.5 >= 55)
sum(compare_data$rank_diff_a2 >= 55)

summary(lm(EAI_P ~ EAI_D, data = compare_data))
summary(lm(EA_P_a1 ~ EAI_D, data = compare_data))
summary(lm(EA_P_a1.5 ~ EAI_D, data = compare_data))
summary(lm(EA_P_a2 ~ EAI_D, data = compare_data))
# ------------------------------------------------------------------------------

# ==============================================================================










