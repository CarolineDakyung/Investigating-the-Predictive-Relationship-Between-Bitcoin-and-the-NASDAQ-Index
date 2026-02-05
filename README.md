```md
# Investigating the Predictive Relationship Between Bitcoin and NASDAQ

## Overview

This project analyzes the dynamic relationship between Bitcoin (BTC) and the NASDAQ Index using time-series modeling techniques. 

The goal is to evaluate whether macroeconomic indicators and financial variables improve the forecasting performance of BTC and NASDAQ returns compared to standard univariate models.

Specifically, the project compares:

- Univariate ARIMA models
- Structural regression models with ARIMA-forecasted drivers
- Vector Autoregression (VAR) models

to assess predictive accuracy and interdependence between markets.

---

## Data Description

All data are programmatically collected from public financial data sources:

### Data Sources

- **Federal Reserve Economic Data (FRED)**
  - VIX (Volatility Index)
  - DGS10 (10-Year Treasury Yield)
  - T10YIE (Inflation Expectation)
  - T10Y2Y (Yield Curve Spread)
  - ICSA (Initial Jobless Claims)

- **Yahoo Finance**
  - Bitcoin (BTC)
  - NASDAQ Index

### Data Type

- Frequency: Daily
- Format: Time series
- Period: Based on availability from FRED and Yahoo Finance

The data are merged, aligned, and transformed into level and differenced series for modeling.

---

## Methodology

The analysis follows a structured time-series modeling pipeline.

### 1. Data Preparation

- Download and clean macroeconomic and financial series
- Align timestamps across sources
- Handle missing values
- Generate differenced series

### 2. Train-Test Split

- Split the time series into training and testing periods
- Reserve the final segment for out-of-sample evaluation

### 3. Baseline Modeling (ARIMA)

- Fit univariate ARIMA models for BTC and NASDAQ
- Use `auto.arima` with bias adjustment
- Generate out-of-sample forecasts

### 4. Structural Modeling with Exogenous Drivers

- Construct regression models using macroeconomic variables
- Forecast exogenous variables using ARIMA
- Predict BTC/NASDAQ using forecasted drivers

Model form:

```text
Target ~ Macroeconomic Drivers

### 5. Vector Autoregression (VAR)

- Estimate VAR(2) and VAR(7) systems
- Perform lag selection using AIC
- Conduct stability checks
- Generate multi-step forecasts

### 6. Granger Causality & IRF Analysis

- Conduct Granger causality tests
- Estimate impulse response functions
- Evaluate dynamic interactions

7. Model Evaluation

Performance metrics:

- RMSE
- MAE

Models are evaluated on out-of-sample test sets.

## Results & Key Findings

- Structural models incorporating macroeconomic drivers improve forecast accuracy over univariate ARIMA in several cases.
- VAR models capture bidirectional dynamics between BTC and NASDAQ.
- Granger causality tests suggest statistically significant lead-lag relationships.
- Impulse response functions reveal short-term spillover effects between crypto and equity markets.
- Overall, multivariate models provide more informative forecasts than standalone univariate approaches.

## Recommendations

- Investors may benefit from incorporating macroeconomic indicators when modeling crypto-equity interactions.
- Portfolio managers should consider dynamic dependencies in risk management.
- Future forecasting systems should integrate multivariate time-series frameworks.

## Limitations

- Model performance is sensitive to training window selection.
- Structural models rely on accurate forecasts of exogenous variables.
- VAR models may suffer from overparameterization with limited data.
- External shocks (policy changes, regulatory events) are not explicitly modeled.

## How to Run

1. Install Dependencies
```bash
install.packages(c(
  "forecast",
  "vars",
  "tseries",
  "ggplot2",
  "dplyr",
  "tidyr",
  "tibble"
))

2. Run the Script

Place model.R in your working directory and run:
```bash
Rscript model.R

or open in RStudio and execute.

