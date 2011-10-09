# http://www.r-bloggers.com/time-series-analysis-and-mining-with-r/
f <- decompose(AirPassengers)
# seasonal figures
f$figure
plot(f$figure, type = 'b', xaxt = 'n', xlab = '')

# get names of 12 months in English words
monthNames <- months(ISOdate(2011, 1:12, 1))

# label x-axis with month names
# las is set to 2 for vertical label orientation

axis(1, at = 1:12, labels = monthNames, las = 2)
plot(f)

# Time series forecasting is to forecast future events based on known past data.
# Below is an example for time series forecasting with an autoregressive
# integrated moving average (ARIMA) model.

fit <- arima(AirPassengers, order = c(1, 0, 0), list(order = c(2, 1, 0), period
                                                     = 12))
fore <- predict(fit, n.ahead = 24)

# error bounds at 95% confidence interval
U <- fore$pred + 2 * fore$se
L <- fore$pred - 2 * fore$se
ts.plot(AirPassengers, fore$pred, U, L, col = c(1, 2, 4, 4), lty = c(1, 1, 2,
                                                                     2))
legend('topleft', c('Actual', 'Forecast', 'Error Bounds (95 Confidence)'), col =
       c(1, 2, 4), lty = c(1, 1, 2))

# ****************************************************************************
# ************************** Time Series Clustering **************************
# ****************************************************************************

# Time series clustering is to partition time series data into groups based on
# similarity or distance, so that time series in the same cluster are similar.
# For time series clustering with R, the first step is to work out an
# appropriate distance/similarity metric, and then, at the second step, use
# existing clustering techniques, such as k-means, hierarchical clustering,
# density-based clustering or subspace clustering, to find clustering
# structures.

# Dynamic Time Warping (DTW) finds optimal alignment between two time series,
# and DTW distance is used as a distance metric in the example below. A data set
# of synthetic control chart time series is used in this example, which contains
# 600 examples of control charts. Each control chart is a time series with 60
# values. There are six classes: 1) 1-100 Normal 2) 101-200 cyclic, 3) 201-300
# Increasing trend, 4) 301-400 descreasing trend, 5) 401-500 updward shift, and
# 6) 501-600 downward shift. 

setwd('c:/users/idris/sandbox/r/decomp_ts')
sc <- read.table('synthetic_control.data.txt', header = F, sep = '')

# randomly sampled n cases from each class, to make it easy for plotting
n <- 10
s <- sample(1:100, n)
idx <- c(s, 100 + s, 200 + s, 300 + s, 400 + s, 500 + s)
sample2 <- sc[idx,]
observedLabels <- c(rep(1,n), rep(2,n), rep(3,n), rep(4,n), rep(5,n), rep(6,n))

# compute the DTW distances
require(dtw)
distMatrix <- dist(sample2, method = 'DTW')

# hierarchical clustering
hc <- hclust(distMatrix, method = 'average')

# and wtf does this mean?
plot(hc, labels = observedLabels, main = '')


