#### PREGUNTA 3: Fermi Problem and Sensitivity Analysis ####

###############################
# 1. Setup
###############################

library(tidyverse)


###############################
# 2. Data preparation and proxy method
###############################

#PROXY METHODOLOGY NOTE:
#Vehicle data like: IS.VEH.PCAR.P3 (Passenger cars (per 1,000 people)), IS.VEH.ROAD.K1 (Vehicles (per km of road)), IS.VEH.NVEH.P3 (Motor vehicles (per 1,000 people)) have been removed from World Bank WDI due to licensing issues.
#Sources:
#https://databank.worldbank.org/metadataglossary/world-development-indicators/series/IS.VEH.ROAD.K1#:~:text=%5BNote%3A%20Data%20have%20been%20removed%20from%20external%20publication%20pending%20a%20review%20of%20their%20licensing%20agreement.%5D
#
#https://databank.worldbank.org/metadataglossary/world-development-indicators/series/IS.VEH.PCAR.P3#:~:text=%5BNote%3A%20Data%20have%20been%20removed%20from%20external%20publication%20pending%20a%20review%20of%20their%20licensing%20agreement.%5D
#
#https://databank.worldbank.org/metadataglossary/world-development-indicators/series/IS.VEH.NVEH.P3#:~:text=%5BNote%3A%20Data%20have%20been%20removed%20from%20external%20publication%20pending%20a%20review%20of%20their%20licensing%20agreement.%5D
#
#Proposal: I'll be using CO2 from transport per capita as proxy for vehicle density. Rationale: Higher transport emissions could correlate strongly with vehicle ownership.
# Proxy used: CO2 from Transport sector (EN.GHG.CO2.TR.MT.CE.AR5)
#   - Includes: Road, Aviation, Railways, Water-borne, Other transportation
#   - Assumption: Road transport represents majority in most countries
#   - EV adoption would primarily impact ROAD transportation portion
#
#Limitation: This may overestimate EV impact in countries where 
# aviation/shipping dominate transport emissions (e.g., island nations).
#
#Alternative metric: Transport_intensity (% of total CO2) identifies
# countries where transport is a major emissions source.

#Cargar datos limpios
data <- read_csv("data/processed/world_bank_clean_renamed.csv")

#Preparar dataset para análisis EV
ev_data <- data %>%
  filter(year >= 2015) %>%  # Usar años recientes
  mutate(
    #Emisiones de transporte per cápita (toneladas CO2e por persona)
    CO2_transport_pc = (CO2_transport * 1000) / Population,  # Convertir Mt a t
    
    #Intensidad de transporte (% del total de emisiones)
    Transport_intensity = (CO2_transport / CO2_total) * 100,
    
    #Mix energético (% renovable en generación eléctrica)
    Clean_energy_score = Renewable_energy_pct
  ) %>%
  select(country, year, region, income,
         CO2_total, CO2_transport, CO2_transport_pc, Transport_intensity,
         Population, GDP_per_capita, Urban_population_pct,
         Energy_use_per_capita, Renewable_energy_pct, Clean_energy_score,
         Electric_power_consumption) %>%
  drop_na(CO2_transport, Population, CO2_total)

#Ver datos preparados
cat("Dataset para análisis EV:", nrow(ev_data), "observaciones\n")
cat("Países únicos:", n_distinct(ev_data$country), "\n")
cat("Periodo:", min(ev_data$year), "-", max(ev_data$year), "\n")

#Promediar por país (últimos años disponibles)
ev_summary <- ev_data %>%
  group_by(country, region, income) %>%
  summarise(
    avg_CO2_transport = mean(CO2_transport, na.rm = TRUE),
    avg_CO2_transport_pc = mean(CO2_transport_pc, na.rm = TRUE),
    avg_transport_intensity = mean(Transport_intensity, na.rm = TRUE),
    avg_renewable_pct = mean(Renewable_energy_pct, na.rm = TRUE),
    avg_population = mean(Population, na.rm = TRUE),
    .groups = "drop"
  )

head(ev_summary, 10)

###############################
# 3. Fermi estimation - ev adoption impact
###############################

#Supuestos del análisis Fermi:
#1. 50% de la población adopta EVs
#2. EVs reemplazan vehículos convencionales (no son adicionales)
#3. Emisiones de transporte se reducen proporcionalmente por:
#  - Eficiencia de EVs vs combustión interna (~70% más eficiente)
#  - Pero depende del mix energético del país (coal vs renewable)
#4. Reducción neta = 50% adopción * Factor de limpieza energética

#Calcular factor de reducción por país
ev_impact <- ev_summary %>%
  mutate(
    #Factor de limpieza: Si 100% renovable = 100% reducción
    #Si 0% renovable (todo fósil) = menor reducción (~30% por eficiencia)
    Clean_factor = (avg_renewable_pct / 100) * 0.7 + 0.3,  # Entre 0.3 y 1.0
    
    #Reducción potencial con 50% adopción EV
    EV_adoption_rate = 0.50,  # 50% de población
    
    #Emisiones actuales de transporte
    Current_transport_emissions = avg_CO2_transport,
    
    #Reducción absoluta (Mt CO2e)
    #50% población adopta * factor limpieza * emisiones transporte actuales
    Emissions_reduction_Mt = EV_adoption_rate * Clean_factor * Current_transport_emissions,
    
    #Reducción porcentual del total de emisiones del país
    Reduction_pct_total = (Emissions_reduction_Mt / (avg_CO2_transport / (avg_transport_intensity / 100))) * 100,
    
    #Reducción per cápita
    Reduction_per_capita = (Emissions_reduction_Mt * 1000) / avg_population  # toneladas por persona
  )

#Ordenar por mayor reducción absoluta
ev_impact_ranked <- ev_impact %>%
  arrange(desc(Emissions_reduction_Mt))

cat("\n=== GLOBAL EV ADOPTION IMPACT (50% Population) ===\n")
cat("Total global reduction:", round(sum(ev_impact$Emissions_reduction_Mt, na.rm = TRUE), 2), "Mt CO2e\n")
cat("Average reduction per country:", round(mean(ev_impact$Emissions_reduction_Mt, na.rm = TRUE), 2), "Mt CO2e\n")
cat("Median reduction:", round(median(ev_impact$Emissions_reduction_Mt, na.rm = TRUE), 2), "Mt CO2e\n\n")

cat("=== TOP 20 COUNTRIES - HIGHEST ABSOLUTE REDUCTION ===\n")
print(ev_impact_ranked %>% 
        select(country, income, Current_transport_emissions, 
               Emissions_reduction_Mt, avg_renewable_pct, Clean_factor) %>% 
        head(20))

#Guardar resultados
write_csv(ev_impact, "results/tables/ev_impact_analysis.csv")


###############################
# 4. SENSITIVITY ANALYSIS
###############################

#Crear diferentes escenarios de adopción EV
adoption_scenarios <- c(0.30, 0.40, 0.50, 0.60, 0.70)

#Función para calcular impacto con diferentes tasas de adopción
calculate_scenario <- function(adoption_rate, data) {
  data %>%
    mutate(
      EV_adoption_rate = adoption_rate,
      Clean_factor = (avg_renewable_pct / 100) * 0.7 + 0.3,
      Emissions_reduction_Mt = adoption_rate * Clean_factor * avg_CO2_transport,
      Scenario = paste0(adoption_rate * 100, "% EV Adoption")
    )
}

#Aplicar función a cada escenario
sensitivity_results <- map_dfr(
  adoption_scenarios,
  ~calculate_scenario(.x, ev_summary)
)

#Resumen por escenario
scenario_summary <- sensitivity_results %>%
  group_by(Scenario) %>%
  summarise(
    Total_reduction_Mt = sum(Emissions_reduction_Mt, na.rm = TRUE),
    Avg_reduction_Mt = mean(Emissions_reduction_Mt, na.rm = TRUE),
    Median_reduction_Mt = median(Emissions_reduction_Mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_reduction_Mt))

cat("\n=== SENSITIVITY ANALYSIS - DIFFERENT EV ADOPTION RATES ===\n")
print(scenario_summary)

#Guardar 
write_csv(sensitivity_results, "results/tables/sensitivity_analysis.csv")


###############################
# 5. VISUALIZATIONS
###############################

#Gráfica 1: Top 20 países con mayor reducción absoluta (50% scenario)
Graf_1 <- ev_impact_ranked %>%
  head(20) %>%
  ggplot(aes(x = reorder(country, Emissions_reduction_Mt), 
             y = Emissions_reduction_Mt,
             fill = income)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Countries: Highest CO2 Reduction Potential",
    subtitle = "50% EV Adoption Scenario",
    x = "Country",
    y = "Emissions Reduction (Mt CO2e)",
    fill = "Income Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(Graf_1)
ggsave("results/figures/top20_reduction_potential.png", 
       Graf_1, width = 10, height = 8, dpi = 300)


#Gráfica 2: Comparación de escenarios (adopción 30%-70%)
Graf_2 <- scenario_summary %>%
  ggplot(aes(x = Scenario, y = Total_reduction_Mt, fill = Scenario)) +
  geom_col() +
  geom_text(aes(label = round(Total_reduction_Mt, 0)), 
            vjust = -0.5, size = 4) +
  labs(
    title = "Global CO2 Reduction by EV Adoption Scenario",
    x = "Adoption Scenario",
    y = "Total Global Reduction (Mt CO2e)"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(Graf_2)
ggsave("results/figures/sensitivity_scenarios.png", 
       Graf_2, width = 10, height = 6, dpi = 300)


#Gráfica 3: Relación entre energía renovable y potencial de reducción
Graf_3 <- ev_impact %>%
  ggplot(aes(x = avg_renewable_pct, 
             y = Emissions_reduction_Mt,
             size = avg_population,
             color = region)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(labels = scales::comma) +
  labs(
    title = "EV Impact vs. Renewable Energy Mix",
    subtitle = "Larger bubbles = Larger population",
    x = "Renewable Energy (%)",
    y = "Potential CO2 Reduction (Mt)",
    color = "Region",
    size = "Population"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(Graf_3)
ggsave("results/figures/renewable_vs_reduction.png", 
       Graf_3, width = 12, height = 8, dpi = 300)


#Gráfica 4: Países con mayor reducción per cápita
Graf_4 <- ev_impact %>%
  arrange(desc(Reduction_per_capita)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(country, Reduction_per_capita), 
             y = Reduction_per_capita,
             fill = avg_renewable_pct)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "darkred", high = "chartreuse4") +
  labs(
    title = "Top 20 Countries: Per Capita Reduction Potential",
    subtitle = "50% EV Adoption Scenario",
    x = "Country",
    y = "Per Capita Reduction (tonnes CO2e/person)",
    fill = "Renewable %"
  ) +
  theme_minimal()

print(Graf_4)
ggsave("results/figures/top20_percapita_reduction.png", 
       Graf_4, width = 10, height = 8, dpi = 300)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Files saved:\n")
cat("- results/tables/ev_impact_analysis.csv\n")
cat("- results/tables/sensitivity_analysis.csv\n")
cat("- results/figures/top20_reduction_potential.png\n")
cat("- results/figures/sensitivity_scenarios.png\n")
cat("- results/figures/renewable_vs_reduction.png\n")
cat("- results/figures/top20_percapita_reduction.png\n")
