# CBDC-Adoption-and-Bank-Stability

This project investigates how the adoption of Central Bank Digital Currencies (CBDCs) impacts banking sector stability across countries, with a focus on differences between developing and advanced economies. The analysis was conducted as part of a senior economics research paper at Queens College (CUNY).


### Research Question
Does the adoption of CBDCs improve banking stability, and does this effect vary with a country's level of economic development?

### Tools & Methods
- Language: R
- Libraries: dplyr, tidyr, plm, lmtest, sandwich

### Methods:
- Fixed-effects panel regression
- Robustness checks using interaction terms
- Alternative specifications using dummy variables and Bank Z-scores

### Data Sources
- World Bank (World Development Indicators, Global Financial Development Database)
- Atlantic Council CBDC Tracker

### Key Variables
- Dependent: Bank Liquidity (%), Bank Z-score
- Independent: CBDC adoption level (0–4 index and dummies), GDP per capita, control variables (e.g., internet use %, unemployment)

## Methodology
- Cleaned and merged macroeconomic panel data using dplyr and tidyr
- Constructed a time-varying CBDC adoption index for each country
- Performed fixed-effects regression models to analyze the relationship between CBDC adoption and bank liquidity
- Included interaction terms with GDP per capita to test heterogeneity in effects by income level
- Ran robustness checks using alternative specifications (e.g., Z-score as dependent variable)

** Key Findings
- CBDC adoption is positively associated with bank liquidity in developing economies.
- The positive impact diminishes as GDP per capita increases, suggesting a conditional effect based on economic development.
- Robustness checks with Z-score and alternative specifications support the main findings.
