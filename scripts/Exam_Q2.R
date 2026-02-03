#### PREGUNTA 2: Predictive Modeling and Scenario Analysis ####

###############################
# 1. SETUP
###############################
#install.packages("caret")

library(tidyverse)
library(caret)       #Para modelado y validación
library(broom)       #Para limpiar outputs de modelos

#Cargar datos limpios
data <- read_csv("data/processed/world_bank_clean_renamed.csv")

#Ver estructura
glimpse(data)

###############################
# 2. DATA PREPARATION
###############################

#Seleccionar variables para el modelo
model_data <- data %>%
  select(
    CO2_total, #Variable dependiente
    
    GDP, GDP_per_capita, GDP_growth,
    Population, Urban_population_pct, Population_growth,
    Energy_use_per_capita, Renewable_energy_pct,
    Industry_value_added, Forest_area_pct, #Variables independientes
    
    country, year, region, income #Variables de control
  ) %>%
  
  drop_na(CO2_total, GDP, Population, Energy_use_per_capita) #Remover filas con NAs en variables clave


###############################
# 3. TRAIN-TEST SPLIT
###############################

set.seed(222)  #Para reproducibilidad

#Crear índices para train/test (80/20 split)
train_index <- createDataPartition(model_data$CO2_total, p = 0.8, list = FALSE)

#Dividir datos
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("\nTrain set:", nrow(train_data), "observaciones\n")
cat("Test set:", nrow(test_data), "observaciones\n")




################################
# 4. Model training
###############################
#install.packages("car")
#install.packages("fixest")
#install.packages("stargazer")
library(fixest)      #Para efectos fijos
library(stargazer) #Para mi tablita comparativa
library(car)

#Modelo 1: OLS básico
Mod_1 <- lm(CO2_total ~ GDP + Population + Energy_use_per_capita + 
              Renewable_energy_pct + Urban_population_pct + Industry_value_added,
            data = train_data)

#Verificar multicolinealidad (solo para Mod_1)
cat("\n=== VIF (Variance Inflation Factor) ===\n")
vif_values <- vif(Mod_1)
print(vif_values)
#Nota: VIF > 10 indica multicolinealidad problemática

#Modelo 2: OLS con GDP per capita en lugar de GDP total
Mod_2 <- lm(CO2_total ~ GDP_per_capita + Population + Energy_use_per_capita + 
              Renewable_energy_pct + Urban_population_pct + Industry_value_added,
            data = train_data)

#Modelo 3: OLS con término cuadrático de GDP
train_data$GDP_squared <- train_data$GDP^2
Mod_3 <- lm(CO2_total ~ GDP + GDP_squared + Population + Energy_use_per_capita + 
              Renewable_energy_pct + Urban_population_pct + Industry_value_added,
            data = train_data)

#Modelo 4: Fixed effects de país (usando fixest)
Mod_4 <- feols(CO2_total ~ GDP + Population + Energy_use_per_capita + 
                 Renewable_energy_pct + Urban_population_pct + Industry_value_added | 
                 country,
               data = train_data)

#Modelo 5: Fixed effects de país y año
Mod_5 <- feols(CO2_total ~ GDP + Population + Energy_use_per_capita + 
                 Renewable_energy_pct + Urban_population_pct + Industry_value_added | 
                 country + year,
               data = train_data)

#Ver resumen rápido
cat("\n=== MODEL R² COMPARISON ===\n")
cat("Mod_1 (OLS Basic) R²:", round(summary(Mod_1)$r.squared, 4), "\n")
cat("Mod_2 (OLS GDP pc) R²:", round(summary(Mod_2)$r.squared, 4), "\n")
cat("Mod_3 (OLS Quadratic) R²:", round(summary(Mod_3)$r.squared, 4), "\n")
cat("Mod_4 (FE Country) R²:", round(r2(Mod_4, "r2"), 4), "\n")
cat("Mod_5 (FE Country+Year) R²:", round(r2(Mod_5, "r2"), 4), "\n")

#Tabla solo con OLS
stargazer(Mod_1, Mod_2, Mod_3,
          title = "OLS Regression Results: CO2 Emissions",
          digits = 2,
          covariate.labels = c("GDP", "GDP per capita", "GDP squared", "Population", 
                               "Energy use per capita", "Renewable energy %", 
                               "Urban population %", "Industry value added %"),
          dep.var.labels = "CO2 Total Emissions (Mt)",
          column.labels = c("Basic", "GDP pc", "Quadratic"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          no.space = TRUE,
          out = "results/tables/ols_models.txt")

#Tabla con FE
etable(Mod_4, Mod_5,
       title = "Fixed Effects Models: CO2 Emissions",
       dict = c("GDP" = "GDP",
                "Population" = "Population", 
                "Energy_use_per_capita" = "Energy use per capita",
                "Renewable_energy_pct" = "Renewable energy %",
                "Urban_population_pct" = "Urban population %",
                "Industry_value_added" = "Industry value added %"),
       digits = 2,
       se.below = TRUE,
       fitstat = c("r2", "n"),
       file = "results/tables/fe_models.txt")

#Para mostrar en consola
etable(Mod_4, Mod_5, 
       digits = 2,
       se.below = TRUE,
       fitstat = c("r2", "n"))



###############################
# 5. Model evaluation and selection
###############################

#Agregar GDP_squared al test set
test_data$GDP_squared <- test_data$GDP^2

#Hacer las predicciones
pred_1<-predict(Mod_1, newdata = test_data)
pred_2<-predict(Mod_2, newdata = test_data)
pred_3<-predict(Mod_3, newdata = test_data)
pred_4<-predict(Mod_4, newdata = test_data)
pred_5<-predict(Mod_5, newdata = test_data)

#Calcular RMSE y R2 

actual <- test_data$CO2_total

#Modelo 1
rmse_1 <- sqrt(mean((actual-pred_1)^2, na.rm = TRUE))
r2_1 <- cor(actual, pred_1, use = "complete.obs")^2

#Modelo 2
rmse_2 <- sqrt(mean((actual-pred_2)^2, na.rm = TRUE))
r2_2 <- cor(actual, pred_2, use = "complete.obs")^2

#Modelo 3
rmse_3 <- sqrt(mean((actual-pred_3)^2, na.rm = TRUE))
r2_3 <- cor(actual, pred_3, use = "complete.obs")^2

#Modelo 4
rmse_4 <- sqrt(mean((actual-pred_4)^2, na.rm = TRUE))
r2_4 <- cor(actual, pred_4, use = "complete.obs")^2

#Modelo 5
rmse_5 <- sqrt(mean((actual-pred_5)^2, na.rm = TRUE))
r2_5 <- cor(actual, pred_5, use = "complete.obs")^2


#Crear tabal comparativa

metrics_comparison <- data.frame(
  Model = c("OLS Basic", "OLS GDP pc", "OLS Quadratic", "FE Country", "FE Country + year"), 
  RMSE = c(rmse_1, rmse_2, rmse_3, rmse_4, rmse_5),
  R2 = c(r2_1, r2_2, r2_3, r2_4, r2_5)
)

#Guardarla
write_csv(metrics_comparison, "results/tables/model_metrics_comparison.csv")

#Identificar el mejor modelo
best_model_name <- metrics_comparison$Model[which.min(metrics_comparison$RMSE)]
best_rmse <- min(metrics_comparison$RMSE)
best_r2 <- metrics_comparison$R2[which.min(metrics_comparison$RMSE)]


cat("\n=== BEST MODEL ===\n")
cat("Model:", best_model_name, "\n")
cat("RMSE:", round(best_rmse, 2), "\n")
cat("R²:", round(best_r2, 4), "\n")


##############################
# 6. 10% GDP increase
###############################


#Usamos el modelo 5, que fue el mejor
best_model <- Mod_5

#Ver coeficientes del modelo
coef_summary <- summary(best_model)
print(coef_summary)


gdp_coef <- coef(best_model)["GDP"]
print(gdp_coef)


#Simulamos aumentar GDP en 10% para cada país. 
scenario_data<- test_data %>% 
  mutate(
    GDP_original = GDP,
    GDP_increased = GDP * 1.10, 
    GDP_change_pct = 10
  )


#Predicciones con GDP original

pred_original <- predict(best_model, newdata = scenario_data)

#Predicciones con GDP aumentado en 10%

scenario_data$GDP <- scenario_data$GDP_increased
pred_increased <- predict(best_model, newdata = scenario_data)

#Calcular cambio en CO2
scenario_results <- scenario_data %>%
  mutate(
    CO2_predicted_original = pred_original,
    CO2_predicted_increased = pred_increased,
    CO2_change_absolute = CO2_predicted_increased - CO2_predicted_original,
    CO2_change_pct = (CO2_change_absolute / CO2_predicted_original) * 100
  ) %>%
  select(country, year, region, income,
         GDP_original, GDP_increased, GDP_change_pct,
         CO2_predicted_original, CO2_predicted_increased,
         CO2_change_absolute, CO2_change_pct)

#Resumen por país (en promedio)

scenario_summary <- scenario_results %>% 
  group_by(country, region, income) %>% 
  summarise(
    avg_CO2_change_pct = mean(CO2_change_pct, na.rm = TRUE),
    avg_CO2_change_absolute = mean(CO2_change_absolute, na.rm= TRUE), 
    .groups = "drop"
  ) %>% 
  arrange(desc(avg_CO2_change_pct))


cat("\n=== SCENARIO ANALYSIS RESULTS ===\n")
cat("Average CO2 change when GDP increases 10%:\n")
cat("Mean:", round(mean(scenario_summary$avg_CO2_change_pct, na.rm = TRUE), 2), "%\n")
cat("Median:", round(median(scenario_summary$avg_CO2_change_pct, na.rm = TRUE), 2), "%\n")
cat("Range:", round(min(scenario_summary$avg_CO2_change_pct, na.rm = TRUE), 2), "% to",
    round(max(scenario_summary$avg_CO2_change_pct, na.rm = TRUE), 2), "%\n")

# Top 10 países con mayor incremento en CO2
cat("\n=== TOP 10 COUNTRIES WITH HIGHEST CO2 INCREASE ===\n")
print(head(scenario_summary, 10))

# Guardar resultados
write_csv(scenario_results, "results/tables/scenario_analysis_detailed.csv")
write_csv(scenario_summary, "results/tables/scenario_analysis_summary.csv")


###############################
# 7. Visaulization
###############################

# Gráfico: Distribución de cambios en CO2
Graf_2 <- ggplot(scenario_summary, aes(x = avg_CO2_change_pct)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = median(scenario_summary$avg_CO2_change_pct, na.rm = TRUE),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of CO2 Change (%) when GDP increases 10%",
       subtitle = "Across all countries in test set",
       x = "CO2 Change (%)",
       y = "Number of Countries") +
  theme_minimal()

print(Graf_2)

# Guardar
ggsave("results/figures/scenario_co2_distribution.png", Graf_2, width = 10, height = 6)

# Gráfico: Top 15 países
top_15 <- head(scenario_summary, 15)

Graf_3 <- ggplot(top_15, aes(x = reorder(country, avg_CO2_change_pct), 
                             y = avg_CO2_change_pct, fill = income)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 15 Countries: CO2 Increase when GDP grows 10%",
       x = "Country",
       y = "CO2 Change (%)",
       fill = "Income Level") +
  theme_minimal()

print(Graf_3)

ggsave("results/figures/scenario_top15_countries.png", Graf_3, width = 10, height = 8)

cat("\nVisualizations saved!\n")



















