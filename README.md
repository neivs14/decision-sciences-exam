# Decision Sciences Practical Exam

**Candidate:** Neiva Calvillo  
**Position:** Research Programmer and Data Scientist  
**Institution:** Decision Sciences Research Center, Tecnológico de Monterrey

---

## Overview

This repository contains a comprehensive analysis of global CO2 emissions using World Bank climate data (2000-2023). The project demonstrates proficiency in data science, predictive modeling, classification, and strategic policy analysis through five interconnected research questions.

**Key Findings:**
- Fixed Effects model achieves 96.8% R² predicting CO2 emissions
- 50% EV adoption could reduce global transport emissions by 1,375 Mt CO2e annually
- Upper middle-income countries offer highest emission reduction potential (97.4 impact score)
- Renewable energy investment shows 40-44% likelihood of achieving >5% CO2 reduction within 5 years

---

## Repository Structure
```
decision-sciences-exam/
├── scripts/
│   ├── Exam_Q1.R                    # Data acquisition and preprocessing
│   ├── Exam_Q2.R                    # Predictive modeling and scenario analysis
│   ├── Exam_Q3.R                    # Fermi problem and sensitivity analysis
│   ├── Exam_Q4.R                    # Classification and policy implications
│   └── Exam_Q5.R                    # Strategic analysis and model application
├── data/
│   ├── raw/                         # Original World Bank data (not in repo)
│   └── processed/
│       └── world_bank_clean_renamed.csv
├── results/
│   ├── figures/                     # All visualizations (PNG files)
│   └── tables/                      # Analysis outputs (CSV files)
├── reports/
│   ├── question1_report.Rmd         # Q1 analysis report
│   ├── question2_report.Rmd         # Q2 analysis report
│   ├── question3_report.Rmd         # Q3 analysis report
│   ├── question4_report.Rmd         # Q4 analysis report
│   └── question5_report.Rmd         # Q5 analysis report
├── renv.lock                        # R package dependencies
├── .Rprofile                        # renv configuration
└── README.md
```

---

## Setup and Installation

### Prerequisites

- **R version:** 4.0 or higher
- **RStudio:** Latest version recommended
- **Operating System:** macOS, Windows, or Linux

### Installation Steps

1. **Clone the repository:**
```bash
git clone https://github.com/neivs14/decision-sciences-exam.git
cd decision-sciences-exam
```

2. **Open the project in RStudio:**
```bash
open decision-sciences-exam.Rproj
```

3. **Restore R package dependencies:**
```r
# In R console
install.packages("renv")
renv::restore()
```

This will install all required packages with exact versions used in the analysis:
- `tidyverse` - Data manipulation and visualization
- `WDI` - World Bank data access
- `fixest` - Fixed effects regression models
- `caret` - Machine learning and model evaluation
- `stargazer` - Regression tables
- `corrplot` - Correlation visualizations
- `car` - VIF calculation
- `broom` - Model tidying
- `pROC` - ROC curves and AUC
- `randomForest` - Random forest classifier
- `scales` - Axis formatting

---

## Running the Analysis

### Sequential Execution (Recommended)

Run scripts in order, as each question builds on previous results:
```r
# Question 1: Data Acquisition and Preprocessing
source("scripts/Exam_Q1.R")

# Question 2: Predictive Modeling and Scenario Analysis
source("scripts/Exam_Q2.R")

# Question 3: Fermi Problem and Sensitivity Analysis
source("scripts/Exam_Q3.R")

# Question 4: Classification and Policy Implications
source("scripts/Exam_Q4.R")

# Question 5: Strategic Analysis and Model Application
source("scripts/Exam_Q5.R")
```

**Estimated runtime:** 15-20 minutes total (depends on API speed and system performance)

### Individual Question Execution

Each script can be run independently, but note:
- **Q2, Q3, Q4, Q5** require Q1's output (`data/processed/world_bank_clean_renamed.csv`)
- **Q5** integrates models from Q2 and Q4

---

## Research Questions and Methodology

### Question 1: Data Acquisition and Preprocessing

**Objective:** Download and clean World Bank climate data with comprehensive socio-economic indicators.

**Key Steps:**
- Downloaded data for 215 countries (2000-2023) using `WDI` package
- Selected 30 variables: CO2 emissions, GDP, population, energy, renewable energy, urbanization, etc.
- Data completeness: 89.61% overall
- Output: `world_bank_clean_renamed.csv` (5,160 observations)

**Key Results:**
- Strong correlation between GDP and CO2 emissions (0.85)
- Energy use per capita highly correlated with CO2 per capita (0.92)
- Renewable energy percentage negatively correlated with CO2 intensity (-0.31)

---

### Question 2: Predictive Modeling and Scenario Analysis

**Objective:** Build predictive model to forecast CO2 emissions and simulate 10% GDP increase scenario.

**Models Trained:**
1. OLS Basic (RMSE: 363.36, R²: 0.8929)
2. OLS with GDP per capita (RMSE: 361.80, R²: 0.8938)
3. OLS Quadratic (RMSE: 359.20, R²: 0.8953)
4. Fixed Effects - Country (RMSE: 199.43, R²: 0.9582)
5. **Fixed Effects - Country + Year (RMSE: 184.96, R²: 0.9685)** ← Best Model

**Scenario Analysis Results:**
- **Research Question:** "If GDP increases by 10%, what is expected CO2 change?"
- **Answer:** Median 1.34% increase (range: -527% to +96%)
- **Key Finding:** GDP coefficient NOT significant in Fixed Effects model (p=0.9998)
- **True drivers:** Energy use per capita, urbanization, population, renewable energy %

---

### Question 3: Fermi Problem and Sensitivity Analysis

**Objective:** Estimate global CO2 impact if 50% of population adopts electric vehicles.

**Methodology:**
- **Challenge:** World Bank removed vehicle data due to licensing issues
- **Solution:** Used CO2 from transport sector as proxy
- **Clean Factor Formula:** (Renewable % / 100 × 0.7) + 0.3

**Key Results:**

**Baseline Scenario (50% EV adoption):**
- **Total global reduction:** 1,374.82 Mt CO2e
- Top reducers (absolute): USA (312 Mt), China (179 Mt), India (79 Mt)
- Top reducers (efficiency): Nigeria (0.87 clean factor), Brazil (0.63), India (0.54)

**Sensitivity Analysis:**
- 30% adoption → 825 Mt reduction
- 70% adoption → 1,925 Mt reduction
- Linear relationship confirmed (~275 Mt per 10% adoption increase)

**Policy Insight:** High-income countries (USA, China) have high volume but low efficiency due to fossil fuel grids. Developing countries with high renewable % achieve 2-3x more reduction per EV.

---

### Question 4: Classification and Policy Implications

**Objective:** Build classifier to identify countries likely to reduce CO2 emissions significantly.

**Target Variable:**
- "Reducing" = Countries that reduced CO2 >5% (2015-2017 vs 2021-2023)
- 49 reducing countries (34.5%) vs 93 not reducing (65.5%)

**Model Performance:**
- **Algorithm:** Logistic Regression
- **Accuracy:** 77.78%
- **Sensitivity:** 55.56% (detects countries that will reduce)
- **Specificity:** 88.89% (detects countries that won't reduce)
- **AUC:** 0.8765 (very good)

**Key Findings:**

**Significant Predictors (only 2 of 8):**
1. **GDP per capita** (+): Wealthier countries more likely to reduce
2. **GDP growth** (-): Rapidly growing economies less likely to reduce

**Common Characteristics of Successful Countries:**
- Average GDP per capita: $32,830 (vs $8,763 for non-reducers)
- Average GDP growth: 1.12% (vs 3.18% for non-reducers)
- Income distribution: 69% are high-income countries

**Policy Implication:** Emission reduction is primarily a high-income, low-growth phenomenon. Countries attempting aggressive reductions before ~$15,000 GDP per capita face economic/political challenges.

---

### Question 5: Strategic Analysis and Model Application

**Objective:** Assess likelihood of CO2 reduction from renewable energy investment and prioritize investments.

**Methodology:**
- Integrated Q2 predictive model + Q4 classifier
- Simulated 4 scenarios: +5%, +10%, +15%, +20% renewable energy
- Analyzed 161 countries

**Key Results:**

**Likelihood of Success:**
- **Overall:** 40-44% across all investment levels
- **High-income:** 82.5% probability (but minimal reduction: -1.3%)
- **Upper middle-income:** 30.5% probability (but massive reduction: 407%)
- **Lower-income:** 7-12% probability

**Investment Priority Framework:**

**Tier 1 (Highest Impact):** Upper middle-income fossil economies
- Countries: Brunei, Botswana, Bosnia & Herzegovina
- Impact score: 97.4 (highest)
- Why: Massive reduction potential × moderate probability

**Tier 2 (High Certainty):** High-income low-renewable countries
- Countries: Singapore, USA, Australia
- Impact score: 27.2
- Why: Very high probability (>95%) but lower absolute impact

**Tier 3 (Efficiency Gains):** Renewable leaders
- Countries: Iceland, Norway, Paraguay
- Why: Demonstrate full decarbonization pathway

**Technology Prioritization:**
- 0-10% baseline → Solar PV + Wind
- 10-30% baseline → Wind + Grid storage
- 30-50% baseline → Offshore wind + Electrification
- 50%+ baseline → Green hydrogen + Efficiency

**Critical Insight:** Investment size determines emission reduction magnitude, but country readiness (GDP per capita, institutional capacity) determines probability of success.

---

## Key Findings Summary

### Across All Questions

1. **Economic development is the primary driver of emission reduction capacity**
   - Countries with >$15,000 GDP per capita have 5-10x higher success probability
   - GDP growth negatively correlates with reduction likelihood

2. **Upper middle-income countries offer the highest expected impact**
   - Despite moderate success probability (30%), massive reduction potential (400-2,000%)
   - Examples: Brunei (2,081% potential), Botswana (2,159% potential)

3. **Technology effectiveness depends on energy mix**
   - EVs in fossil-grid countries (USA, China) achieve only 30-40% of theoretical maximum
   - EVs in renewable-grid countries (Nigeria, Brazil) achieve 60-90% effectiveness

4. **Investment prioritization requires balancing probability and magnitude**
   - High-income countries: High probability, low impact
   - Upper middle-income: Moderate probability, high impact
   - Low-income: Low probability, moderate impact

---

## Visualizations

All figures are saved in `results/figures/` as high-resolution PNG files (300 DPI).

**Question 1:**
- Correlation matrix of key variables
- Missing data patterns

**Question 2:**
- CO2 change distribution from GDP increase scenario
- Top 15 countries by CO2 increase

**Question 3:**
- Top 20 countries by reduction potential
- Sensitivity analysis (30-70% EV adoption)
- Renewable energy % vs reduction potential
- Per capita reduction leaders

**Question 4:**
- Feature importance (z-values)
- ROC curve (AUC = 0.8765)
- Classification errors analysis
- Characteristics comparison (reducing vs not reducing)
- Income level distribution

**Question 5:**
- Scenario impact (renewable increase vs probability)
- Top 15 countries by investment priority
- Income level priority comparison

---

## Technical Notes

### Data Sources

- **World Bank World Development Indicators** via `WDI` package
- **Period:** 2000-2023
- **Countries:** 215 (preprocessing), 161 (analysis after cleaning)
- **Variables:** 30 socio-economic and environmental indicators

### Modeling Decisions

**Question 2 (Predictive Model):**
- Fixed Effects model selected over OLS (50% RMSE reduction)
- Train/test split: 80/20 (2,531 train, 633 test observations)
- VIF check confirmed no multicollinearity (all VIF < 2)

**Question 4 (Classifier):**
- Logistic regression selected for interpretability
- Train/test split: 80/20 stratified by target (115 train, 27 test countries)
- 5% threshold for "significant reduction" based on policy relevance

**Question 5 (Integration):**
- Combined predictive model coefficient (-2.61) with classifier probabilities
- Impact score = CO2 reduction × Success probability
- Scenarios based on 15% renewable increase as optimal balance

### Limitations

1. **Temporal:** Model trained on 2000-2023 data; future trends may differ
2. **Causality:** Observational data cannot establish causal relationships
3. **Omitted variables:** Political will, policy quality, institutional capacity not captured
4. **Proxy data:** Q3 uses transport CO2 as proxy for vehicle emissions (includes aviation/shipping)
5. **Linear assumptions:** Renewable energy impact assumed linear (may have diminishing returns)

---

## Results Files

### Tables (CSV format in `results/tables/`)

**Question 1:**
- `descriptive_statistics.csv` - Summary statistics for all variables
- `correlation_matrix.csv` - Correlation coefficients

**Question 2:**
- `model_metrics_comparison.csv` - Performance metrics for 5 models
- `scenario_analysis_summary.csv` - GDP increase scenario results by country
- `ols_models.txt` - Stargazer output for OLS models
- `fe_models.txt` - Fixed effects model results

**Question 3:**
- `ev_impact_analysis.csv` - Country-level EV adoption impact (192 countries)
- `sensitivity_analysis.csv` - Results for 30-70% adoption scenarios

**Question 4:**
- `Q4_model_metrics.csv` - Classifier performance metrics
- `Q4_feature_importance.csv` - Coefficient significance
- `Q4_test_predictions.csv` - Country-level predictions
- `Q4_comparison_reducing_vs_not.csv` - Group comparisons
- `Q4_top_reducers.csv` - Top 10 successful countries

**Question 5:**
- `Q5_scenario_summary.csv` - Impact by renewable investment level
- `Q5_probability_by_scenario.csv` - Success probability estimates
- `Q5_top20_priority.csv` - Investment priority ranking
- `Q5_priority_by_income.csv` - Strategic framework by income level

---

## Reports

HTML reports with comprehensive analysis, methodology, and policy implications are generated from R Markdown files in `reports/`:
```r
# Render all reports
rmarkdown::render("reports/question1_report.Rmd")
rmarkdown::render("reports/question2_report.Rmd")
rmarkdown::render("reports/question3_report.Rmd")
rmarkdown::render("reports/question4_report.Rmd")
rmarkdown::render("reports/question5_report.Rmd")
```

Each report includes:
- Detailed methodology
- Complete results with visualizations
- Policy implications and recommendations
- Limitations and assumptions

---

## Contact

**Neiva Calvillo**  
MSc in Data Science and Public Policy  
Tecnológico de Monterrey, Mexico City

**Repository:** https://github.com/neivs14/decision-sciences-exam

---

## Acknowledgments

This analysis was completed as part of the practical examination for the Research Programmer and Data Scientist position at the Decision Sciences Research Center, Tecnológico de Monterrey.

Data source: World Bank World Development Indicators (https://data.worldbank.org/)

---

*Last updated: February 2026*
