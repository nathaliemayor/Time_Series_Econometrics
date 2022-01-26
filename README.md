# Introduction 

This repository contains R code for a Time Series Econometrics (M.A./ PhD) course at UNISG.

# Specifications on Files

## [A1_arima.R](https://github.com/nathaliemayor/Time_Series_Econometrics/blob/main/A1_arima.R) - *Time Series Modelling, Robust ACF Confidence Bands, Robust Portmanteau Estimators* [[1]](#1)

1. Computing sample ACF and PACF with classical confidence bands. Based, on the results, selection of an *ARMA(p,q)* process.
3. Implementation of the robust ACF confidence bands and the robust Portmanteau estimators.
4. Applying the robust portmanteau tests to a time series with time-varying variance. 
5. Building ACF plots with robust confidence bands.  

## [A2_forecasting.R](https://github.com/nathaliemayor/Time_Series_Econometrics/blob/main/A2_forecasting.R) - *Time Series Decomposition, Cointegration*

1. Creation of a seasonal plot for sales data, decomposition of the time series into seasonal, trend and residual component. Analysis of the ACF of the residual component. Fitting an ARIMA model and forecasting of the time series.
2. Given two time series (ETFs with ticker *EWA* and *EWC*), computation of the spread. 
3. Analysis and and modelling (ARIMA) of the spread. 
4. Execution of Augmented Dickey Fuller tests on the time series.

# References

<a id="1">[1]</a> 
Dalla, V., Giraitis, L. and Phillips, P.C., 2019. Robust tests for white noise and cross-correlation. Econometric Theory, pp.1-29.
