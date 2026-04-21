# Violence_against_children
# Child Maltreatment & Violence Against Children: Trend Analysis (Australia)

This repository contains R code used for analysing trends in violence and maltreatment among children and adolescents in Australia, including child maltreatment substantiations, sexual assault, domestic violence, and hospitalisations.

## Repository Contents

| File | Description |
|------|-------------|
| `DV_github.R` | Family and Domestic violence trend analysis |
| `Hospitalisations_github.R` | Hospitalisation rates for assault |
| `Sexual_assault_github.R` | Sexual assault rates |
| `CM_github.R` | Child maltreatment substantiation rates and substantiations by maltreatment type |

> **Note:** Some input data files are not included in this repository due to data access restrictions. Please contact the authors for access.

## Methods

Each analysis script follows a common analytic approach:

- **Descriptive plots** — time-series visualisations by sex and/or age group using `ggplot2`
- **Trend estimation** — annual percent change (APC) estimated via:
  - Linear regression on log-transformed rates
  - Quasi-Poisson regression on counts with population offset
- **Joinpoint detection** — change-point testing using:
  - Davies test (`segmented` package)
  - Segmented regression with 1 joinpoint
  - ΔAIC grid search to compare linear vs. joinpoint models
- **Summary statistics** — absolute and percentage changes, average APC

## R Packages Required

```r
install.packages(c(
  "tidyverse",
  "readxl",
  "ggplot2",
  "dplyr",
  "broom",
  "summarytools",
  "segmented"
))
```

## Usage

1. Clone the repository and place your input data files in the working directory.
2. Open the relevant `.R` script in RStudio.
3. Set your working directory to the project folder.
4. Run the script from top to bottom.

## Data Sources

Data are derived from Australian administrative datasets including child protection substantiation records and police-reported crime statistics. Population denominators are sourced from the Australian Bureau of Statistics (ABS).

## Contact

For questions about the code or data access, please open an issue or contact the repository owner.
