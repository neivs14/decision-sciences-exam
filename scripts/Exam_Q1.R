#### PREGUNTA 1: Data Acquisition and Prepossessing ####

###############################
# 1. Setup and exploration
###############################


# install.packages("WDI")
# install.packages("tidyverse")
# install.packages("corrplot")

# Cargar librerías
library(WDI)
library(tidyverse)
library(corrplot)


###############################
# 2. Indicator selection
###############################

selected_indicators <- c(
  # CO2 y ambiente
  "EN.GHG.CO2.MT.CE.AR5",     # CO2 emissions total (Mt CO2e)
  "EN.GHG.CO2.PC.CE.AR5",     # CO2 emissions per capita (t CO2e/capita)
  "EN.GHG.CO2.TR.MT.CE.AR5",  # CO2 from transport (Mt CO2e)
  "EN.GHG.CO2.RT.GDP.KD",     # Carbon intensity of GDP (kg CO2e per constant 2015 US$ of GDP)
  
  # Economía
  "NY.GDP.MKTP.CD",           # GDP (current US$)
  "NY.GDP.PCAP.CD",           # GDP per capita (current US$)
  "NY.GDP.MKTP.KD.ZG",        # GDP growth (annual %)
  
  # Población
  "SP.POP.TOTL",              # Population, total
  "SP.URB.TOTL.IN.ZS",        # Urban population (% of total)
  "SP.POP.GROW",              # Population growth (annual %)
  
  # Energía
  "EG.USE.PCAP.KG.OE",        # Energy use (kg of oil equivalent per capita)
  "EG.ELC.ACCS.ZS",           # Access to electricity (% of population)
  "EG.FEC.RNEW.ZS",           # Renewable energy consumption (% of total)
  "EG.USE.ELEC.KH.PC",        # Electric power consumption (kWh per capita)
  
  # Educación e industria
  "SE.XPD.TOTL.GD.ZS",        # Government expenditure on education (% of GDP)
  "NV.IND.TOTL.ZS",           # Industry value added (% of GDP)
  
  # Otros
  "EN.URB.LCTY",              # Population in largest city
  "AG.LND.FRST.ZS"            # Forest area (% of land area)
)

###############################
# 3. Downloading
###############################

#Descargar todos los indicadores
raw_data <- WDI(
  indicator = selected_indicators,
  start = 2000,
  end = 2023,
  extra = TRUE  #Esto añade info adicional como región, nivel de ingreso
)

#Verificar los datos
dim(raw_data)
head(raw_data)
str(raw_data)

#Guardar datos crudos
write_csv(raw_data, "data/raw/world_bank_raw_data.csv")


###############################
# 4. Data Cleaning
###############################

#Ver estructura de los datos
summary(raw_data)

na_count <- raw_data %>%
  summarise(across(everything(), ~sum(is.na(.))))
print(na_count)

#Ver qué países y zonas agregadas tenemos
unique_countries <- raw_data %>%
  distinct(country, region, income) %>%
  arrange(country)

#Filtrar para quitar agregados
clean_data <- raw_data %>%
  filter(region!= "Aggregates",
         !is.na(region))

#Filas restantes
nrow(raw_data)
nrow(clean_data)

#Ver cuántos países únicos que quedaron
clean_data %>%
  distinct(country) %>%
  nrow()

write_csv(clean_data, "data/processed/world_bank_clean_data.csv")


###############################
#### 5. EDA - Missing values
###############################

#Contar NAs por indicador
na_summary <- clean_data %>%
  select(starts_with("EN."), starts_with("NY."), 
         starts_with("SP."), starts_with("EG."), 
         starts_with("SE."), starts_with("NV."),
         starts_with("AG.")) %>%
  summarise(across(everything(),
                   list(n_missing = ~sum(is.na(.)),
                        pct_missing = ~round(sum(is.na(.))/n()*100, 2)))) %>% 
  pivot_longer(everything(),
               names_to = c("indicator", ".value"),
               names_pattern = "(.+)_(.+)")

print(na_summary)

# Completitud general del dataset
total_cells <- nrow(clean_data) * length(grep("^(EN\\.|NY\\.|SP\\.|EG\\.|SE\\.|NV\\.|AG\\.)", names(clean_data)))
missing_cells <- sum(is.na(select(clean_data, starts_with("EN."), starts_with("NY."), 
                                  starts_with("SP."), starts_with("EG."), 
                                  starts_with("SE."), starts_with("NV."),
                                  starts_with("AG."))))

cat("\n=== RESUMEN DE COMPLETITUD ===\n")
cat("Completitud general:", round((1 - missing_cells/total_cells) * 100, 2), "%\n")
cat("Datos disponibles para CO2:", sum(!is.na(clean_data$EN.GHG.CO2.MT.CE.AR5)), 
    "de", nrow(clean_data), "observaciones\n")


###############################
# 6. Descriptive stats and correlations
###############################

# Estadísticas descriptivas de variables clave
descriptive_stats <- clean_data %>%
  select(EN.GHG.CO2.MT.CE.AR5, EN.GHG.CO2.PC.CE.AR5, 
         NY.GDP.MKTP.CD, NY.GDP.PCAP.CD, SP.POP.TOTL,
         EG.USE.PCAP.KG.OE, EG.FEC.RNEW.ZS) %>%
  summary()

print(descriptive_stats)

# Renombrar columnas para mejor legibilidad
clean_data_renamed <- clean_data %>%
  rename(
    CO2_total = EN.GHG.CO2.MT.CE.AR5,
    CO2_per_capita = EN.GHG.CO2.PC.CE.AR5,
    CO2_transport = EN.GHG.CO2.TR.MT.CE.AR5,
    CO2_intensity_GDP = EN.GHG.CO2.RT.GDP.KD,
    GDP = NY.GDP.MKTP.CD,
    GDP_per_capita = NY.GDP.PCAP.CD,
    GDP_growth = NY.GDP.MKTP.KD.ZG,
    Population = SP.POP.TOTL,
    Urban_population_pct = SP.URB.TOTL.IN.ZS,
    Population_growth = SP.POP.GROW,
    Energy_use_per_capita = EG.USE.PCAP.KG.OE,
    Electricity_access = EG.ELC.ACCS.ZS,
    Renewable_energy_pct = EG.FEC.RNEW.ZS,
    Electric_power_consumption = EG.USE.ELEC.KH.PC,
    Education_expenditure = SE.XPD.TOTL.GD.ZS,
    Industry_value_added = NV.IND.TOTL.ZS,
    Largest_city_pop = EN.URB.LCTY,
    Forest_area_pct = AG.LND.FRST.ZS
  )

# Matriz de correlación con nombres claros
correlation_data <- clean_data_renamed %>%
  select(CO2_total, CO2_per_capita, GDP, GDP_per_capita,
         Population, Urban_population_pct,
         Energy_use_per_capita, Renewable_energy_pct) %>%
  na.omit()

cor_matrix <- cor(correlation_data)
print(round(cor_matrix, 3))

# Crear y mostrar gráfico de correlaciones
Graf_1 <- corrplot(cor_matrix, 
                   method = "color",
                   type = "upper",
                   addCoef.col = "black",
                   tl.col = "black",
                   tl.cex = 0.8,
                   number.cex = 0.7,
                   title = "Correlation Matrix: CO2 and Key Indicators",
                   mar = c(0,0,2,0))

# Guardar el gráfico
png("results/figures/correlation_matrix.png", width = 1200, height = 1000, res = 150)
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7,
         title = "Correlation Matrix: CO2 and Key Indicators",
         mar = c(0,0,2,0))
dev.off()

cat("\nCorrelation plot saved to: results/figures/correlation_matrix.png\n")

# Guardar datos limpios con nombres claros
write_csv(clean_data_renamed, "data/processed/world_bank_clean_renamed.csv")



###############################
# 7. DETAILED DESCRIPTIVE STATISTICS
###############################

#Tabla de estadísticas por variable principal
detailed_stats <- clean_data_renamed %>%
  select(CO2_total, CO2_per_capita, GDP, GDP_per_capita, 
         Population, Energy_use_per_capita, Renewable_energy_pct) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    Mean = round(mean(value, na.rm = TRUE), 2),
    Median = round(median(value, na.rm = TRUE), 2),
    SD = round(sd(value, na.rm = TRUE), 2),
    Min = round(min(value, na.rm = TRUE), 2),
    Max = round(max(value, na.rm = TRUE), 2),
    Q1 = round(quantile(value, 0.25, na.rm = TRUE), 2),
    Q3 = round(quantile(value, 0.75, na.rm = TRUE), 2)
  )

print(detailed_stats)

#Guardar tabla
write_csv(detailed_stats, "results/tables/descriptive_statistics.csv")

#Top 10 de países con más emisiones (promedio) entre 2015-2023
top_emitters <- clean_data_renamed %>%
  filter(year >= 2015) %>%
  group_by(country) %>%
  summarise(
    avg_CO2 = round(mean(CO2_total, na.rm = TRUE), 2),
    avg_CO2_pc = round(mean(CO2_per_capita, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(avg_CO2)) %>%
  head(10)

print("\n=== TOP 10 EMITTERS (2015-2023 Average) ===")
print(top_emitters)

#Guardar
write_csv(top_emitters, "results/tables/top_10_emitters.csv")

cat("\nTables saved to results/tables/\n")