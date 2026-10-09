# Salary Prediction with Multiple Linear Regression in R

**A statistical modeling project using U.S. Census data on programmers and engineers**

**Tools:** R · Multiple Linear Regression · Exploratory Data Analysis · Variance Inflation Factors (VIF) · Residual Diagnostics · Cook's Distance · DFBETAS · Nested F-Tests

## Overview

This project investigates factors associated with wage income among programmers and engineers using data derived from the 2000 U.S. Census. The objective was to develop an interpretable multiple linear regression model for predicting salary while evaluating model assumptions and identifying relevant predictors.

This project was completed as part of a Linear Regression course.

## Data

**Source:** [`freqparcoord::prgeng`](https://search.r-project.org/CRAN/refmans/freqparcoord/html/prgeng.html)

The original analysis considered these variables:

| Variable | Description | Role |
| --- | --- | --- |
| `wageinc` | Wage income in U.S. dollars | Response |
| `age` | Age in years | Predictor |
| `sex` | Recorded sex category | Predictor |
| `educ` | Education variable, as coded in the dataset | Predictor |
| `wkswrkd` | Weeks worked | Predictor |
| `engl` | English proficiency | Predictor |
| `cit` | Citizenship category | Considered in full models |
| `occ` | Occupation category | Considered in full models |
| `yrentry` | Year of entry into the U.S. | Considered in initial model |
| `birth`, `powspuma` | Place-of-birth and workplace-location variables | Explored but not modeled |


## Methodology

The analysis was implemented in R and involved:

-Exploratory Data Analysis: Examined salary distributions and relationships between demographic, educational, and employment characteristics.
-Model Development: Constructed multiple linear regression models, investigated multicollinearity using Variance Inflation Factors (VIF), and introduced a quadratic age term to capture nonlinear relationships.
-Regression Diagnostics: Evaluated residual behavior, influential observations, and model assumptions using residual plots, Cook's distance, DFBETAS, and Q-Q plots.
-Model Refinement: Applied a log transformation to the response variable and compared nested models using partial F-tests.
-Prediction: Generated salary estimates and 95% prediction intervals for individuals with different characteristics.

## Results and Key findings: 
The final model included age, age squared, sex, education, weeks worked, and English proficiency.

-The log-transformed regression model achieved an in-sample R² of approximately 0.51.
-Including a quadratic age term captured a nonlinear association between age and predicted income.
-Regression diagnostics motivated adjustments to the initial model.-
-Nested-model F-tests showed that occupation and citizenship contributed explanatory information, although the final model excluded them to prioritize simplicity and interpretability.
-The analysis also illustrated limitations of salary prediction using observational survey data, particularly the effects of influential observations and variability in income.

## Limitations

The analysis used observational survey data, and the results should not be interpreted as causal relationships. Data-filtering decisions may affect the estimates, and model performance was evaluated in-sample rather than on an independent test set.

## How to Run

Install the required R packages and run the analysis script. The dataset is available through the freqparcoord package.
