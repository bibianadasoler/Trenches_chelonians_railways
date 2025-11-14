library(dplyr)
library(glmmTMB)
library(performance)

general_data <- read.csv(here::here("SM2_trench_trials.csv"), sep = ";")
measurements_data <- read_excel("SM1_individuals_measurements.xlsx") %>% 
  mutate(individual = as.factor(individual))

type2_data <- general_data %>%
  filter(type == 2) %>%
  mutate(trench_use = ifelse(exit_way == "trench", 1, 0),
         individual = as.factor(individual),
         species = as.factor(species) ) %>%
  left_join(measurements_data, by = c("individual")) %>%
  select(individual, species.x, trench_use, carapace_length) %>%
  rename(species = species.x)


# GLMM BINOMIAL
model <- glmmTMB(trench_use ~ species + carapace_length + (1 | individual), data = type2_data, 
               family = binomial(link = "logit"))

summary(model)
check_model(model)
check_residuals(model)
sjPlot::tab_model(model, transform = "exp", file = "SM2_model_table.doc")

