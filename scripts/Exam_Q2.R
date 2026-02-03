#### PREGUNTA 2: Predictive Modeling and Scenario Analysis ####

###############################
# 1. SETUP
###############################
install.packages("caret")



library(tidyverse)
library(caret)       # Para modelado y validación
library(broom)       # Para limpiar outputs de modelos




# Cargar datos limpios
data <- read_csv("data/processed/world_bank_clean_renamed.csv")

# Ver estructura
glimpse(data)
cat("\nDatos cargados:", nrow(data), "observaciones\n")