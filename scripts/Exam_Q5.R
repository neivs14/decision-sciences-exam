#### PREGUNTA 5: Strategic Analysis and Model Application ####

###############################
# 1. Setup
###############################

library(tidyverse)
library(fixest)
library(caret)

# Cargar datos limpios
data <- read_csv("data/processed/world_bank_clean_renamed.csv")

nrow(data)
cat(min(data$year), "-", max(data$year))


###############################
# 2. Re-entrenar modelo predictivo (Q2)
###############################

#Preparar datos para modelo
model_data <- data %>%
  select(CO2_total, GDP, Population, Energy_use_per_capita, 
         Renewable_energy_pct, Urban_population_pct, Industry_value_added,
         country, year, region, income) %>%
  drop_na(CO2_total, GDP, Population, Energy_use_per_capita)

#Train/test split (mismo seed que Q2)
set.seed(222)
train_index <- createDataPartition(model_data$CO2_total, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

#Modelo 5: Fixed Effects (Country + Year), que fue el mejor de Q2
predictive_model <- feols(CO2_total ~ GDP + Population + Energy_use_per_capita + 
                            Renewable_energy_pct + Urban_population_pct + 
                            Industry_value_added | country + year,
                          data = train_data)

cat("\n=== MODELO PREDICTIVO (Q2) ===\n")
print(summary(predictive_model))



###############################
# 3. Re-entrenar clasificador (Q4)
###############################

#Crear target variable (usando Q4)
co2_trends <- data %>%
  filter(year >= 2015) %>%
  mutate(
    period = case_when(
      year >= 2015 & year <= 2017 ~ "initial",
      year >= 2021 & year <= 2023 ~ "final",
      TRUE ~ "middle"
    )
  ) %>%
  filter(period %in% c("initial", "final")) %>%
  group_by(country, region, income, period) %>%
  summarise(avg_CO2 = mean(CO2_total, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = avg_CO2) %>%
  mutate(
    CO2_change_pct = ((final - initial) / initial) * 100,
    is_reducing = ifelse(CO2_change_pct < -5, 1, 0)
  ) %>%
  drop_na(initial, final)

#Preparar features
features <- data %>%
  filter(year >= 2015 & year <= 2023) %>%
  group_by(country, region, income) %>%
  summarise(
    avg_GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
    avg_renewable_pct = mean(Renewable_energy_pct, na.rm = TRUE),
    avg_energy_per_capita = mean(Energy_use_per_capita, na.rm = TRUE),
    avg_urban_pct = mean(Urban_population_pct, na.rm = TRUE),
    avg_CO2_per_capita = mean(CO2_per_capita, na.rm = TRUE),
    avg_CO2_intensity = mean(CO2_intensity_GDP, na.rm = TRUE),
    avg_population = mean(Population, na.rm = TRUE),
    avg_GDP_growth = mean(GDP_growth, na.rm = TRUE),
    .groups = "drop"
  )

#Dataset para la clasificación
classification_data <- co2_trends %>%
  select(country, region, income, is_reducing) %>%
  left_join(features, by = c("country", "region", "income")) %>%
  drop_na()

#Convertir target a factor
classification_data$is_reducing <- factor(classification_data$is_reducing, 
                                          levels = c(0, 1),
                                          labels = c("Not_Reducing", "Reducing"))

#Train/test split
set.seed(123)
class_train_idx <- createDataPartition(classification_data$is_reducing, 
                                       p = 0.80, list = FALSE)
class_train <- classification_data[class_train_idx, ]
class_test <- classification_data[-class_train_idx, ]

#Feature columns
feature_cols <- c("avg_GDP_per_capita", "avg_renewable_pct", 
                  "avg_energy_per_capita", "avg_urban_pct",
                  "avg_CO2_per_capita", "avg_CO2_intensity",
                  "avg_population", "avg_GDP_growth")

#Entrenar clasificador
formula_str <- paste("is_reducing ~", paste(feature_cols, collapse = " + "))
classifier_model <- glm(as.formula(formula_str), 
                        data = class_train, 
                        family = binomial(link = "logit"))

cat("\n=== CLASIFICADOR (Q4) ===\n")
print(summary(classifier_model)$coefficients)


###############################
# 4. Escenarios de inversión en renewable energy
###############################

#Seleccionar países para análisis (últimos 3 años de datos)
scenario_countries <- data %>%
  filter(year >= 2021) %>%
  group_by(country, region, income) %>%
  summarise(
    current_renewable = mean(Renewable_energy_pct, na.rm = TRUE),
    current_CO2 = mean(CO2_total, na.rm = TRUE),
    GDP = mean(GDP, na.rm = TRUE),
    Population = mean(Population, na.rm = TRUE),
    Energy_use_per_capita = mean(Energy_use_per_capita, na.rm = TRUE),
    Urban_population_pct = mean(Urban_population_pct, na.rm = TRUE),
    Industry_value_added = mean(Industry_value_added, na.rm = TRUE),
    GDP_per_capita = mean(GDP_per_capita, na.rm = TRUE),
    GDP_growth = mean(GDP_growth, na.rm = TRUE),
    CO2_per_capita = mean(CO2_per_capita, na.rm = TRUE),
    CO2_intensity = mean(CO2_intensity_GDP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na()

cat("\n=== PAÍSES PARA ANÁLISIS ===\n")
cat("Total países:", nrow(scenario_countries), "\n")

#Definir escenarios de inversión (aumentar renewable % en 5, 10, 15, 20 puntos)
investment_scenarios <- c(5, 10, 15, 20)

cat("\n=== ESCENARIOS DE INVERSIÓN ===\n")
cat("Aumentos en renewable energy %:", paste(investment_scenarios, collapse = ", "), "\n")




###############################
# 5. Simular impacto en CO2 (usando test_data de Q2)
###############################

#Usar test_data que ya conoce el modelo
#Agregar año más reciente por país
scenario_countries <- test_data %>%
  group_by(country, region, income) %>%
  filter(year == max(year)) %>%  #Último año disponible
  ungroup() %>%
  mutate(
    current_renewable = Renewable_energy_pct,
    current_CO2 = CO2_total
  )

cat("\n=== PAÍSES PARA ANÁLISIS ===\n")
cat("Total países:", n_distinct(scenario_countries$country), "\n")

#Función para simular
simulate_investment <- function(base_data, renewable_increase) {
  
  sim_data <- base_data %>%
    mutate(
      renewable_original = Renewable_energy_pct,
      renewable_increase_pct = renewable_increase
    )
  
  #Predicción antes
  pred_before <- predict(predictive_model, newdata = sim_data)
  
  #Predicción después (aumentar renewable)
  sim_data$Renewable_energy_pct <- sim_data$Renewable_energy_pct + renewable_increase
  pred_after <- predict(predictive_model, newdata = sim_data)
  
  #Calcular cambio
  sim_data <- sim_data %>%
    mutate(
      CO2_predicted_before = pred_before,
      CO2_predicted_after = pred_after,
      CO2_reduction_absolute = CO2_predicted_before - CO2_predicted_after,
      CO2_reduction_pct = (CO2_reduction_absolute / CO2_predicted_before) * 100
    )
  
  return(sim_data)
}

#Simular escenarios
all_scenarios <- map_dfr(investment_scenarios, function(increase) {
  simulate_investment(scenario_countries, increase)
})

cat("\n=== SIMULACIÓN COMPLETADA ===\n")
cat("Total simulaciones:", nrow(all_scenarios), "\n")

#Resumen
summary_by_scenario <- all_scenarios %>%
  group_by(renewable_increase_pct) %>%
  summarise(
    avg_CO2_reduction_pct = mean(CO2_reduction_pct, na.rm = TRUE),
    median_CO2_reduction_pct = median(CO2_reduction_pct, na.rm = TRUE),
    total_CO2_reduction_Mt = sum(CO2_reduction_absolute, na.rm = TRUE),
    countries = n(),
    .groups = "drop"
  )

print(summary_by_scenario)


###############################
# 6. Calcular probabilidad de reducción con clasificador
###############################

#Preparar datos para clasificador
#Necesitamos simular cómo cambiarían las features después de la inversión
scenarios_for_classification <- all_scenarios %>%
  mutate(
    #Features para el clasificador (ajustadas por inversión)
    avg_GDP_per_capita = GDP / Population,
    avg_renewable_pct = Renewable_energy_pct,  #Ya aumentado
    avg_energy_per_capita = Energy_use_per_capita,
    avg_urban_pct = Urban_population_pct,
    avg_CO2_per_capita = CO2_predicted_after / Population,  #CO2 proyectado
    avg_CO2_intensity = CO2_predicted_after / GDP,
    avg_population = Population,
    avg_GDP_growth = 2  #Asumir crecimiento moderado
  )

#Predecir probabilidad de reducción con clasificador
probability_reducing <- predict(classifier_model, 
                                newdata = scenarios_for_classification, 
                                type = "response")

#Agregar probabilidad a resultados
scenarios_with_prob <- scenarios_for_classification %>%
  mutate(
    probability_reduction = probability_reducing,
    likely_to_reduce = ifelse(probability_reduction > 0.5, "Yes", "No")
  )

#Resumen por escenario
cat("\n=== PROBABILIDAD DE REDUCCIÓN POR ESCENARIO ===\n")
prob_summary <- scenarios_with_prob %>%
  group_by(renewable_increase_pct) %>%
  summarise(
    avg_probability = mean(probability_reduction, na.rm = TRUE),
    median_probability = median(probability_reduction, na.rm = TRUE),
    pct_likely_reduce = mean(probability_reduction > 0.5, na.rm = TRUE) * 100,
    avg_CO2_reduction = mean(CO2_reduction_pct, na.rm = TRUE),
    .groups = "drop"
  )

print(prob_summary)

#Top 10 países con mayor probabilidad (escenario 15%)
cat("\n=== TOP 10 PAÍSES: Mayor probabilidad con +15% renewable ===\n")
top_countries_15 <- scenarios_with_prob %>%
  filter(renewable_increase_pct == 15) %>%
  arrange(desc(probability_reduction)) %>%
  select(country, income, renewable_original, probability_reduction, 
         CO2_reduction_pct) %>%
  head(10)

print(top_countries_15)



###############################
# 7. Priorización de inversiones por tipo
###############################

#Calcular costo-efectividad por nivel de ingreso
priority_analysis <- scenarios_with_prob %>%
  filter(renewable_increase_pct == 15) %>%  #Escenario óptimo
  mutate(
    #Impacto score = CO2 reduction * probability
    impact_score = CO2_reduction_pct * probability_reduction,
    #Clasificar por nivel de inversión inicial
    investment_readiness = case_when(
      renewable_original < 10 ~ "Low baseline (solar/wind priority)",
      renewable_original >= 10 & renewable_original < 30 ~ "Medium baseline (diversify)",
      renewable_original >= 30 ~ "High baseline (efficiency focus)"
    )
  ) %>%
  arrange(desc(impact_score))

#Top 20 países por impacto
cat("\n=== TOP 20 PAÍSES: Priorización de inversión (15% scenario) ===\n")
top_20_priority <- priority_analysis %>%
  select(country, income, renewable_original, probability_reduction, 
         CO2_reduction_pct, impact_score, investment_readiness) %>%
  head(20)

print(top_20_priority)

#Resumen por nivel de ingreso
cat("\n=== PRIORIZACIÓN POR NIVEL DE INGRESO ===\n")
income_priority <- priority_analysis %>%
  group_by(income) %>%
  summarise(
    countries = n(),
    avg_probability = mean(probability_reduction, na.rm = TRUE),
    avg_CO2_reduction = mean(CO2_reduction_pct, na.rm = TRUE),
    avg_impact_score = mean(impact_score, na.rm = TRUE),
    total_CO2_potential = sum(CO2_reduction_absolute, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_impact_score))

print(income_priority)

#Guardar resultados
write_csv(summary_by_scenario, "results/tables/Q5_scenario_summary.csv")
write_csv(prob_summary, "results/tables/Q5_probability_by_scenario.csv")
write_csv(top_20_priority, "results/tables/Q5_top20_priority.csv")
write_csv(income_priority, "results/tables/Q5_priority_by_income.csv")



###############################
# 8. Visualizaciones
###############################

#Gráfica 1: Probabilidad vs CO2 reduction por escenario
Graf_Q5_1 <- ggplot(prob_summary, 
                    aes(x = renewable_increase_pct)) +
  geom_line(aes(y = avg_probability * 100, color = "Probability of Reduction"), 
            size = 1.2) +
  geom_line(aes(y = avg_CO2_reduction, color = "CO2 Reduction"), 
            size = 1.2) +
  geom_point(aes(y = avg_probability * 100), size = 3) +
  geom_point(aes(y = avg_CO2_reduction), size = 3) +
  scale_color_manual(values = c("Probability of Reduction" = "steelblue", 
                                "CO2 Reduction" = "coral")) +
  labs(
    title = "Impact of Renewable Energy Investment on CO2 Reduction",
    subtitle = "Average across all countries",
    x = "Renewable Energy Increase (%)",
    y = "Percentage (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(Graf_Q5_1)
ggsave("results/figures/Q5_scenario_impact.png", Graf_Q5_1, 
       width = 10, height = 6, dpi = 300)

#Gráfica 2: Top 15 países por impact score
top_15_impact <- priority_analysis %>%
  arrange(desc(impact_score)) %>%
  head(15)

Graf_Q5_2 <- ggplot(top_15_impact, 
                    aes(x = reorder(country, impact_score), 
                        y = impact_score, fill = income)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 Countries: Investment Priority",
    subtitle = "Impact Score = CO2 Reduction × Probability (15% renewable increase)",
    x = "Country",
    y = "Impact Score",
    fill = "Income Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(Graf_Q5_2)
ggsave("results/figures/Q5_top15_priority.png", Graf_Q5_2, 
       width = 10, height = 8, dpi = 300)

#Gráfica 3: Priorización por nivel de ingreso
Graf_Q5_3 <- ggplot(income_priority %>% filter(!is.na(avg_impact_score)), 
                    aes(x = reorder(income, avg_impact_score), 
                        y = avg_impact_score, fill = income)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Investment Priority by Income Level",
    subtitle = "Average impact score (15% renewable increase)",
    x = "Income Level",
    y = "Average Impact Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(Graf_Q5_3)
ggsave("results/figures/Q5_income_priority.png", Graf_Q5_3, 
       width = 10, height = 6, dpi = 300)
