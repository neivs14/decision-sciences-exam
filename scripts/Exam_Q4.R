#### PREGUNTA 4: Classification and Policy Implications ####

###############################
# 1. Setup
###############################

library(tidyverse)
library(caret)        #Para train/test split y métricas
#install.packages("randomForest")
library(randomForest) #Para Random Forest classifier
#install.packages("pROC")
library(pROC)         #Para ROC curve y AUC

# Cargar datos limpios
data <- read_csv("data/processed/world_bank_clean_renamed.csv")

cat(nrow(data), "observaciones\n")
cat(min(data$year), "-", max(data$year), "\n")


###############################
# 2. Crear variable TARGET
###############################

#Objetivo: Identificar países que están reduciendo emisiones (tendencia decreciente)
#Método: Calcular cambio en CO2 total entre periodo inicial (2015-2017) y final (2021-2023)

#Calcular promedios por país en dos periodos
co2_trends <- data %>%
  filter(year >= 2015) %>%  #Usar periodo reciente
  mutate(
    period = case_when(
      year >= 2015 & year <= 2017 ~ "initial",
      year >= 2021 & year <= 2023 ~ "final",
      TRUE ~ "middle"
    )
  ) %>%
  filter(period %in% c("initial", "final")) %>%  # Solo periodos inicial y final
  group_by(country, region, income, period) %>%
  summarise(
    avg_CO2 = mean(CO2_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = avg_CO2
  ) %>%
  mutate(
    #Calcular cambio porcentual
    CO2_change_pct = ((final - initial) / initial) * 100,
    
    #Definir TARGET:
    #Si redujo más de 5% → "Reducing" (1)
    #Si no redujo o aumentó → "Not Reducing" (0)
    is_reducing = ifelse(CO2_change_pct < -5, 1, 0),
    is_reducing_label = ifelse(is_reducing == 1, "Reducing", "Not Reducing")
  ) %>%
  drop_na(initial, final)  #Remover países sin datos completos

#Ver distribución del target
cat("\n=== DISTRIBUCIÓN DEL TARGET ===\n")
table(co2_trends$is_reducing_label)
cat("\nPorcentaje reduciendo:", 
    round(mean(co2_trends$is_reducing) * 100, 1), "%\n")

#Ver ejemplos de cada clase
cat("\n=== EJEMPLOS: Países REDUCIENDO (>5%) ===\n")
print(co2_trends %>% 
        filter(is_reducing == 1) %>% 
        arrange(CO2_change_pct) %>% 
        select(country, income, initial, final, CO2_change_pct) %>% 
        head(10))

cat("\n=== EJEMPLOS: Países NO REDUCIENDO ===\n")
print(co2_trends %>% 
        filter(is_reducing == 0) %>% 
        arrange(desc(CO2_change_pct)) %>% 
        select(country, income, initial, final, CO2_change_pct) %>% 
        head(10))



###############################
# 3. Preparar FEATURES (X) para clasificación
###############################

#Calcular promedios de features en el periodo 2015-2023
features <- data %>%
  filter(year >= 2015 & year <= 2023) %>%
  group_by(country, region, income) %>%
  summarise(
    #Features principales
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

#Unir features con target
classification_data <- co2_trends %>%
  select(country, region, income, CO2_change_pct, is_reducing, is_reducing_label) %>%
  left_join(features, by = c("country", "region", "income")) %>%
  drop_na()  # Remover países sin features completos

cat("\n=== DATASET PARA CLASIFICACIÓN ===\n")
cat("Total países:", nrow(classification_data), "\n")
cat("Reduciendo:", sum(classification_data$is_reducing), "\n")
cat("No reduciendo:", sum(classification_data$is_reducing == 0), "\n")

#Ver primeras filas
head(classification_data, 10)





###############################
# 4. Train/Test Split
###############################

#Convertir target a factor para clasif
classification_data$is_reducing <- factor(classification_data$is_reducing, 
                                          levels = c(0, 1),
                                          labels = c("Not_Reducing", "Reducing"))

#Hacer split 80/20 aleatorio por PAÍS
set.seed(123) 
train_indices <- createDataPartition(classification_data$is_reducing, 
                                     p = 0.80, 
                                     list = FALSE)

train_data <- classification_data[train_indices, ]
test_data <- classification_data[-train_indices, ]

cat("\n=== TRAIN/TEST SPLIT ===\n")
cat("Train set:", nrow(train_data), "países\n")
cat("  - Reducing:", sum(train_data$is_reducing == "Reducing"), "\n")
cat("  - Not Reducing:", sum(train_data$is_reducing == "Not_Reducing"), "\n")

cat("\nTest set:", nrow(test_data), "países\n")
cat("  - Reducing:", sum(test_data$is_reducing == "Reducing"), "\n")
cat("  - Not Reducing:", sum(test_data$is_reducing == "Not_Reducing"), "\n")

#Sólo columnas numéricas para el modelo
feature_cols <- c("avg_GDP_per_capita", "avg_renewable_pct", 
                  "avg_energy_per_capita", "avg_urban_pct",
                  "avg_CO2_per_capita", "avg_CO2_intensity",
                  "avg_population", "avg_GDP_growth")

cat("\nFeatures usados:", length(feature_cols), "\n")
print(feature_cols)


###############################
# 5. Entrenar modelo - Logistic Regression
###############################

#Preparar fórmula
formula_str <- paste("is_reducing ~", paste(feature_cols, collapse = " + "))
model_formula <- as.formula(formula_str)

cat("\n=== ENTRENANDO MODELO ===\n")

#Entrenar modelo
logistic_model <- glm(model_formula, 
                      data = train_data, 
                      family = binomial(link = "logit"))

#Ver resumen del modelo
summary(logistic_model)

#Predecir en test set
test_predictions_prob <- predict(logistic_model, 
                                 newdata = test_data, 
                                 type = "response")

#Convertir probabilidades a clases (umbral 0.5)
test_predictions <- ifelse(test_predictions_prob > 0.5, 
                           "Reducing", 
                           "Not_Reducing")
test_predictions <- factor(test_predictions, 
                           levels = c("Not_Reducing", "Reducing"))

#Crear confusion matrix
conf_matrix <- confusionMatrix(test_predictions, 
                               test_data$is_reducing,
                               positive = "Reducing")

cat("\n=== RESULTADOS EN TEST SET ===\n")
print(conf_matrix)




###############################
# 6. Feature Importance
###############################

#Extraer coeficientes del modelo
coef_table <- summary(logistic_model)$coefficients
coef_df <- as.data.frame(coef_table)
coef_df$feature <- rownames(coef_df)

#Remover intercept
coef_df <- coef_df %>%
  filter(feature != "(Intercept)") %>%
  mutate(
    #Calcular importancia como valor absoluto del z-value
    importance = abs(`z value`),
    #Clasificar significancia
    significance = case_when(
      `Pr(>|z|)` < 0.001 ~ "***",
      `Pr(>|z|)` < 0.01 ~ "**",
      `Pr(>|z|)` < 0.05 ~ "*",
      `Pr(>|z|)` < 0.1 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  arrange(desc(importance))

cat("\n=== FEATURE IMPORTANCE ===\n")
print(coef_df %>% 
        select(feature, Estimate, `Pr(>|z|)`, significance, importance))

#Visualización de Feature Importance
Graf_importance <- ggplot(coef_df, aes(x = reorder(feature, importance), 
                                       y = importance, 
                                       fill = significance != "")) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("gray70", "steelblue"),
                    labels = c("No significativo", "Significativo")) +
  labs(
    title = "Feature Importance - Logistic Regression Model",
    subtitle = "Basado en z-values absolutos",
    x = "Feature",
    y = "Importance (|z-value|)",
    fill = "Significancia"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(Graf_importance)

#Guardar
ggsave("results/figures/Q4_feature_importance.png", 
       Graf_importance, 
       width = 10, height = 6, dpi = 300)



###############################
# 7. ROC Curve y AUC
###############################

#Calcular ROC
roc_obj <- roc(test_data$is_reducing, test_predictions_prob)

#Ver AUC
cat("\n=== ROC CURVE METRICS ===\n")
cat("AUC:", round(auc(roc_obj), 4), "\n")

#Crear gráfica ROC
Graf_roc <- ggroc(roc_obj, color = "steelblue", size = 1.2) +
  geom_abline(slope = 1, intercept = 1, 
              linetype = "dashed", color = "gray50") +
  labs(
    title = "ROC Curve - Logistic Regression Model",
    subtitle = paste0("AUC = ", round(auc(roc_obj), 4)),
    x = "Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(Graf_roc)

#Guardar
ggsave("results/figures/Q4_roc_curve.png", 
       Graf_roc, 
       width = 8, height = 6, dpi = 300)



###############################
# 8. Análisis de países mal clasificados
###############################

#Crear tabla con predicciones
test_results <- test_data %>%
  mutate(
    prediction = test_predictions,
    probability = test_predictions_prob,
    correct = (prediction == is_reducing)
  ) %>%
  select(country, income, region, is_reducing, prediction, 
         probability, correct, CO2_change_pct, 
         avg_GDP_per_capita, avg_GDP_growth)

#Ver países CORRECTAMENTE clasificados
cat("\n=== PAÍSES CORRECTAMENTE CLASIFICADOS ===\n")
correct_predictions <- test_results %>% 
  filter(correct == TRUE) %>%
  arrange(is_reducing, desc(probability))

print(correct_predictions)

#Ver países MAL clasificados (errores)
cat("\n=== PAÍSES MAL CLASIFICADOS (ERRORES) ===\n")
incorrect_predictions <- test_results %>% 
  filter(correct == FALSE) %>%
  arrange(is_reducing, desc(probability))

print(incorrect_predictions)

cat("\n=== ANÁLISIS DE ERRORES ===\n")
cat("Total errores:", nrow(incorrect_predictions), "de", nrow(test_results), "\n")
cat("Falsos Negativos (dijo NO pero SÍ redujeron):", 
    sum(incorrect_predictions$is_reducing == "Reducing"), "\n")
cat("Falsos Positivos (dijo SÍ pero NO redujeron):", 
    sum(incorrect_predictions$is_reducing == "Not_Reducing"), "\n")


#Visualización de predicciones correctas vs incorrectas
Graf_errors <- ggplot(test_results, 
                      aes(x = avg_GDP_per_capita, 
                          y = avg_GDP_growth,
                          color = correct,
                          shape = is_reducing)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(data = test_results %>% filter(correct == FALSE),
            aes(label = country), 
            size = 3, hjust = -0.1, vjust = 0.5, color = "black") +
  scale_color_manual(values = c("FALSE" = "darkred",  "TRUE" = "darkgreen"),
                     labels = c("Mal clasificado", "Bien clasificado")) +
  scale_shape_manual(values = c("Not_Reducing" = 16, "Reducing" = 17),
                     labels = c("No redujo", "Redujo")) +
  labs(
    title = "Clasificación: Correcta vs Incorrecta",
    subtitle = "Países mal clasificados destacados con nombre",
    x = "PIB per cápita promedio (USD)",
    y = "Crecimiento GDP promedio (%)",
    color = "Clasificación",
    shape = "Realidad"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(Graf_errors)

#Guardar
ggsave("results/figures/Q4_classification_errors.png", 
       Graf_errors, 
       width = 12, height = 8, dpi = 300)



###############################
# 9. Características de países exitosos
###############################

#Analizar países que SÍ redujeron (en TODO el dataset, no solo test)
reducing_countries <- classification_data %>%
  filter(is_reducing == "Reducing") %>%
  arrange(CO2_change_pct)

#Estadísticas descriptivas de países que redujeron
cat("\n=== CARACTERÍSTICAS DE PAÍSES QUE REDUJERON ===\n")
cat("Total países que redujeron >5%:", nrow(reducing_countries), "\n\n")

#Calcular promedios por grupo
comparison <- classification_data %>%
  group_by(is_reducing) %>%
  summarise(
    n = n(),
    avg_GDP_pc = mean(avg_GDP_per_capita, na.rm = TRUE),
    avg_renewable = mean(avg_renewable_pct, na.rm = TRUE),
    avg_energy_pc = mean(avg_energy_per_capita, na.rm = TRUE),
    avg_urban = mean(avg_urban_pct, na.rm = TRUE),
    avg_CO2_pc = mean(avg_CO2_per_capita, na.rm = TRUE),
    avg_growth = mean(avg_GDP_growth, na.rm = TRUE),
    median_reduction = median(CO2_change_pct, na.rm = TRUE)
  )

cat("\n=== COMPARACIÓN: REDUCING vs NOT REDUCING ===\n")
print(comparison)

#Ver distribución por nivel de ingreso
cat("\n=== DISTRIBUCIÓN POR NIVEL DE INGRESO ===\n")
income_distribution <- classification_data %>%
  group_by(is_reducing, income) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(is_reducing, desc(n))

print(income_distribution)

#Top 10 países con mayor reducción
cat("\n=== TOP 10 PAÍSES CON MAYOR REDUCCIÓN ===\n")
top_reducers <- reducing_countries %>%
  select(country, income, region, CO2_change_pct, 
         avg_GDP_per_capita, avg_renewable_pct, avg_GDP_growth) %>%
  head(10)

print(top_reducers)



###############################
# 10. Visualizaciones finales
###############################

#Gráfica: Comparación de características
comparison_long <- comparison %>%
  select(-median_reduction) %>%
  pivot_longer(cols = c(avg_GDP_pc, avg_renewable, avg_energy_pc, 
                        avg_urban, avg_CO2_pc, avg_growth),
               names_to = "variable", values_to = "value")

Graf_comparison <- ggplot(comparison_long, 
                          aes(x = variable, y = value, fill = is_reducing)) +
  geom_col(position = "dodge") +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = c("Not_Reducing" = "coral", "Reducing" = "steelblue")) +
  labs(title = "Características: Países que Reducen vs No Reducen",
       x = "", y = "Valor promedio", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom")

print(Graf_comparison)
ggsave("results/figures/Q4_comparison_characteristics.png", 
       Graf_comparison, width = 12, height = 8, dpi = 300)


#Gráfica: Distribución por ingreso
Graf_income <- ggplot(income_distribution, 
                      aes(x = income, y = n, fill = is_reducing)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("Not_Reducing" = "coral", "Reducing" = "steelblue")) +
  labs(title = "Distribución por Nivel de Ingreso",
       x = "Nivel de ingreso", y = "Número de países", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(Graf_income)
ggsave("results/figures/Q4_income_distribution.png", 
       Graf_income, width = 10, height = 6, dpi = 300)


###############################
# 11. Guardar resultados
###############################

#Guardar métricas del modelo
model_metrics <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Kappa", "AUC"),
  Value = c(conf_matrix$overall["Accuracy"],
            conf_matrix$byClass["Sensitivity"],
            conf_matrix$byClass["Specificity"],
            conf_matrix$overall["Kappa"],
            auc(roc_obj))
)

write_csv(model_metrics, "results/tables/Q4_model_metrics.csv")

#Guardar tabla de coeficientes
write_csv(coef_df, "results/tables/Q4_feature_importance.csv")

#Guardar predicciones completas
write_csv(test_results, "results/tables/Q4_test_predictions.csv")

#Guardar comparación
write_csv(comparison, "results/tables/Q4_comparison_reducing_vs_not.csv")

#Guardar top reducers
write_csv(top_reducers, "results/tables/Q4_top_reducers.csv")
