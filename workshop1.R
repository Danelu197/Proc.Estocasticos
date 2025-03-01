# Load necessary libraries
library(tseries)   # For ADF test
library(ggplot2)   # For better plots
library(forecast)  # For ACF/PACF and additional time series tools
library(zoo)       # For rolling statistics
library(TSA)
library(tidyr)
library(dplyr)

#---- a ----
# Create a data frame with the given values
data <- data.frame(
  Years = c(1995, 1996, 1997, 1998),
  I = c(153, 133, 145, 111),
  II = c(189, 177, 200, 170),
  III = c(221, 241, 187, 243),
  IV = c(215, 228, 201, 178),
  V = c(302, 283, 292, 248),
  VI = c(223, 255, 220, 202),
  VII = c(201, 238, 233, 163),
  VIII = c(173, 164, 172, 139),
  IX = c(121, 128, 119, 120),
  X = c(106, 108, 81, 96),
  XI = c(86, 87, 65, 95),
  XII = c(87, 74, 76, 53),
  XIII = c(108, 95, 74, 94)
)

# Display the data frame
print(data)

#---- b ----
# Investigate the structure of the data frame
str(data)

#---- c ----
# Convert to long format to get time series structure
data_long <- pivot_longer(data, cols = -Years, names_to = "Category", values_to = "Sales") %>%
  arrange(Years) %>%
  mutate(Period = row_number())  # Creating sequential periods

# Convert to time series (assuming 13 periods per year)
ts_data <- ts(data_long$Sales, frequency = 13, start = c(1995, 1))

# Plot time series
autoplot(ts_data) +
  ggtitle("Time Series of Coded Sales (Company X)") +
  xlab("4-Week Periods") +
  ylab("Coded Sales") +
  theme_minimal()


#---- d ----
# Plot ACF to check for stationarity
acf(ts_data, main = "Autocorrelation Function (ACF) of Coded Sales", lag.max = 52)

# Apply difference
ts_seasonal_diff <- diff(ts_data, lag = 13)
autoplot(ts_seasonal_diff) + ggtitle("Seasonally Differenced Series (Lag=13)")
acf(ts_seasonal_diff, main="ACF After Seasonal Differencing")


#---- e ----
# Decompose the time series using STL (Seasonal-Trend decomposition)
ts_decomp <- stl(ts_seasonal_diff, s.window = "periodic")

# Plot the decomposition
autoplot(ts_decomp) + ggtitle("Decomposition of Coded Sales Time Series")

adf_test <- adf.test(ts_seasonal_diff)

# Extract trend, seasonal, and residual components
seasonal_component <- ts_decomp$time.series[, "seasonal"]
trend_component <- ts_decomp$time.series[, "trend"]
residual_component <- ts_decomp$time.series[, "remainder"]

# Compute variances
seasonal_var <- var(na.omit(seasonal_component))
trend_var <- var(na.omit(trend_component))
residual_var <- var(na.omit(residual_component))

# Compute strengths
seasonal_strength <- seasonal_var / (seasonal_var + residual_var)
trend_strength <- trend_var / (trend_var + residual_var)

# Print results
print(seasonal_strength)
print(trend_strength)
print(adf_test)

# ---- 3 ----

# Load the wages dataset
data(wages)

# Create a time variable
time <- 1:length(wages)

# Fit a linear trend model using least squares
linear_model <- lm(wages ~ time)

# Plot the time series of wages
plot(time, wages, type = "l", main = "Time Series of Wages (1981-1987)", 
     ylab = "Wages", xlab = "Time", col = "blue", lwd = 2)

# Overlay the linear trend line
lines(time, predict(linear_model), col = "red", lwd = 2)

# Add a legend with appropriate positioning
legend("topleft", legend = c("Data", "Linear Trend"), 
       col = c("blue", "red"), lty = 1, lwd = 2, cex = 0.8, bty = "n")

intercept <- coef(linear_model)[1]
slope <- coef(linear_model)[2]

# Print the equation
cat("Wages =", intercept, "+", slope, "* Time\n")

# Compute residuals
residuals <- residuals(linear_model)

# Plot residuals over time
plot(time, residuals, type = "o", col = "blue", pch = 16,
     main = "Residuals from Linear Regression Over Time",
     xlab = "Time", ylab = "Residuals")

# Add a horizontal line at zero for reference
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Plot Autocorrelation Function of Residuals
acf(residuals, main = "Autocorrelation Function of Residuals", col = "blue", lwd = 2)

# Q-Q plot
qqnorm(residuals, main = "Q-Q Plot of Residuals")
qqline(residuals, col = "red", lwd = 2)

# Shapiro-Wilk test for normality
shapiro.test(residuals)
