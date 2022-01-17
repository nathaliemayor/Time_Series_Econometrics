setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(stats)      # formacf(), pacf(), Box.test()
library(lmtest)     # to infer std. errors
library(ggthemes)
library(forecast)

data2021 <- read.csv("data2021.csv", header = TRUE)

##(a) Plot the data series “data2021”. Describe the time series and its properties.

plot <- ggplot(data = data2021, aes(x=X, y = x)) + 
  geom_line(colour="darkblue") +
  theme_economist() +
  ggtitle(" Index") +
  labs(y = "Value", x = "Time")
plot

#Data seems to be serially correlated with a cyclical component.

##(b) Compute the sample ACF and PACF up to 10 lags with classical confidence bands 
#(i.e. under iid assumptions and with a confidence level of 95%).
#You can use the acf(), pacf() functions of the R library “stats”. What can you infer from the plots?
data2021m <- data2021[,-1]
ggAcf(data2021m,lag.max=50,ci = 0.95) + ggtitle("ACF data2021")


ggPacf(data2021m,lag.max = 100, plot = TRUE) + ggtitle("PACF")
# PACF removes components already explained by earlier lags, so we only get the lags which have the correlation with the residual.
# i.e the component not explained by earlier lags. 
# So if there is any hidden information in the residual which can be modeled by the next lag, 
# we might get a good correlation and we will keep that next lag as a feature while modeling.

# # PACF. Order p is the lag value after which PACF plot crosses the upper confidence interval for the first time
# These p lags will act as our features while forecasting the AR time series. We cannot use the ACF plot here
# because it will show good correlations even for the lags which are far in the past. If we consider those 
# many features, we will have multicollinearity issues.This is not a problem with PACF plot as it removes 
# components already explained by earlier lags, so we only get the lags which have the correlation with the 
# residual i.e the component not explained by earlier lags.
#AR(2)


# (c) What kind of ARMA(p,q) process would you deem reasonable for modelling the time series 
#considering the ACF and PACF plots? Explain!
#ARMA(2,1): PACF with two significant after 2 lags points towards AR(2) component. Alternating sign in PACF hints toward positive MA(1) component

#ARMA (2,1)
#for an ARMA(1,1) the AR parameter is pos. and the MA parameter negative.


#(d) Implement the robust ACF confidence bands and the robust Portmanteau estimators (Qm and Q ̃m) 
# suggested in “Robust Tests for White Noise and Cross- Correlation” by Dalla

#enable testing with both univariate and bivariate time series that is robust to multiple forms of heteroskedastic and dependence departures from i.i.d noise.

  rob_portmanteau <- function(j,k,data,opt){
    N      = length(data)
    etk    = (data[1:(N-k)] - mean(data))*(data[(k+1):N] - mean(data)) 
    etj    = (data[1:(N-j)] - mean(data))*(data[(j+1):N] - mean(data))
    lmi    = min(length(etj),length(etk))
    etk    = etk[(length(etk)-lmi+1):length(etk)]
    etj    = etj[(length(etj)-lmi+1):length(etj)]
    num    = sum(etj*etk)
    den    = sqrt(sum(etj^2))*sqrt(sum(etk^2))
    Rjk    = num/den #We will estimate R by rjk which is a sample cross-correlations of the variables etj and etk 
    den2   = sqrt(sum(((etj^2)*(etk^2))))
    taujk  = num/den2 # self-normalized t-type statistic (used for rjkb)
    threshold = 1.96 # suggested threshold setting λ = 1.96.
    Rjkb = Rjk*(abs(taujk) > threshold) #To improve the finite sample performance of the cumulative test, we use the thresholded version of R
    if (opt == 1) {
      R = Rjkb
    } else {
      R = Rjk
    }
    return(R)} #R is the sample analogue of the variance-covariance matrix of t for which we threshold the off-diagonal elements by checking at the 5% significance level whether they are significant.


## (e) Load the data set “data20212” in R. This time series features no serial correlation – 
#but it is not iid either due to a time-varying variance. 

data20212 <- read.csv("data20212.csv", header = TRUE)

##Plot the data and comment. 
plot2 <- ggplot(data = data20212, aes(x=X, y = x)) + 
  geom_line(colour="darkblue") +
  theme_economist() +
  ggtitle(" Index") +
  labs(y = "Value", x = "Time")
plot2

data20212m <- data20212[2]
rhos <- acf(x = data20212m, lag.max = 10, plot = FALSE)[[1]]
## Then set the number of lags to 10, calculate 95%-confidence bands and use a significance level of 5%.
#When {xt} is uncorrelated but not i.i.d. the standard method for testing zero serial correlation based on 
#the asymptotic distribution in √n(ρ,...,ρ )→ N(0,I ) generally fails.
# Taylor (1984) suggested correcting the standard error of ρk, leading to a modified t-statistic.


K <- 10
alpha <- 0.05
N = length(data20212[[1]]) 
t_values = 1:K
numerator   = seq(0, 0, length.out = N)
denominator = seq(0, 0, length.out = N)
xmean = mean(data20212m[[1]])

for (k in 1:K) {
  for (t in (K + 1):N) {
    numerator[t] = (data20212m[[1]][t] - xmean)*(data20212m[[1]][t-k] - xmean)
    denominator[t] = numerator[t]^2
  }
  t_values[k] = sum(numerator)/sqrt(sum(denominator))
}
# Matrix for the Confidence Intervals:
CI = matrix(1:(2*K), ncol = 2)
for (k in 1:K) {
  # Sample Autocorrelation
  rho = rhos[k + 1]
  # Correct sample autocorrelation for its variance
  c = t_values[k]/rho #t = rho*c --> N(0,1)
  # Confidence Intervals around H0: autocorrelation = 0
  CI[k, 1] = 0 - qnorm(alpha/2)/c 
  CI[k, 2] = 0 + qnorm(alpha/2)/c
  
}
print(CI)
##Apply your robust Portmanteau tests to the data and create ACF plots with robust confidence bands. 

lag <- 10
opt <- 1
data = read.csv(file = "data20212.csv", header = TRUE)[,2]

portmanteau_stats = function(k, lag, data, opt) {
  vect = do.call(rbind, lapply(1:lag, rob_portmanteau, k, data, opt))
return(vect)
  }

R_mat = do.call(cbind, lapply(1:lag, portmanteau_stats, lag, data, opt))

Q_m_tilde = t(t_values)%*%solve(R_mat)%*%t_values
print(round(Q_m_tilde, 0))

Box.test(x = data20212m, lag = 10, type = "Box-Pierce")
Box.test(x = data20212m, lag = 10, type = "Ljung-Box")

band <- CI[10,]
ggAcf(data20212m,lag.max=10, ci = NULL) + ggtitle("ACF data20212") + 
  geom_hline(yintercept=CI[10,],linetype="dashed", color = "blue")

# A test for a group of autocorrelations is called a portmanteau test, 
#from a French word describing a suitcase or coat rack carrying several items of clothing.
#How large is too large? If the autocorrelations did come from a white noise series, then Q would have a χ2
#distribution with  (h−K) degrees of freedom, where  K is the number of parameters in the model. 
# If they are calculated from raw data (rather than the residuals from a model), then set  K= 0.
