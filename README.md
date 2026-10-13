# Flat Glass Company Revenue Forecasting

This project focuses on forecasting the monthly revenue of a small flat glass company based in Malta trading across Europe. The goal is to provide a reliable predictive tool to support production planning, improve inventory management, and minimize waste.

## Project Context
The European flat glass market is closely tied to construction demand and energy prices, as glass manufacturing is highly energy-intensive. To understand the external drivers affecting the company, the analysis includes:
1. **Company Revenues:** Monthly data (January 2023 - November 2025).
2. **Flat Glass Manufacture (Spain):** Monthly Industrial Production Index (IPI) data from Eurostat.
3. **Energy Supplies Prices:** Electricity and gas price indices from Eurostat.

## Exploratory Data Analysis
Initial inspection via Autocorrelation Functions (ACF) revealed that the revenue time series acts almost entirely as **white noise**, displaying no significant autocorrelation or obvious seasonality. Therefore, standard ARIMA models on the revenue alone were excluded from the final selection.

## Modeling Approaches
Several predictive models were tested to evaluate the impact of time and external regressors:

*   **Linear Regression:** Four combinations of regressors (Energy, Spain IPI, and linear trend) were tested. None proved statistically significant due to the high variance and small scale of the data.
*   **Generalized Additive Models (GAM):** Non-linear approaches were applied using natural splines and local regression. The best model (Time + Spain IPI) yielded a low AIC but the parameters showed weak statistical significance.
*   **Splines:** Regression Splines selection led to a simple degree of freedom (`df=1`), flattening back to a linear model. Smoothing Splines required a massive penalty parameter (lambda), forcing a nearly linear trajectory.
*   **Exponential Smoothing:** 
    *   *Simple Exponential Smoothing (SES):* Returned an $\alpha \approx 0$, considering all variance as noise and forecasting a flat global average.
    *   *Holt's Exponential Smoothing:* Successfully captured a slight positive trend in the revenue, slightly outperforming SES in terms of Root Mean Squared Error (RMSE).
*   **Prophet (Chosen Model):** Modeled with a logistic trend capped at €90,000 capacity and yearly seasonality. Prophet handled the high volatility of the dataset more gracefully than the others while still projecting a realistic, slow-growth upward trend. 

## Conclusions
Due to the white-noise nature of the historical data and the relatively small scale of the enterprise, macro-economic external variables (Energy and Spain Glass Production) do not heavily dictate the immediate revenue fluctuations. Among the tested frameworks, **Prophet** is recommended as the most robust tool for mid-term forecasting, as it effectively balances trend visualization without overfitting the underlying noise.

## Repository Contents
*   `BEF.R`: The complete R script containing EDA, variable forecasting, and all time-series modeling (Linear, GAM, Prophet, Splines, Exponential Smoothing).
