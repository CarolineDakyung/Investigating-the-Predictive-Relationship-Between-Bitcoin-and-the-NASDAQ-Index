# Modify the setwd() path to match your local project folder
setwd("YOUR_LOCAL_PROJECT_PATH")

library(forecast)
library(tseries)
library(xts)
library(fredr) 
library(quantmod)
library(zoo)
library(dplyr)
library(tidyverse)
library(lubridate)
library(vars)

# Use your own fredr API key
fredr_set_key("YOUR_FRED_API_KEY")

readRenviron("~/.Renviron")
Sys.getenv("FRED_API_KEY")

# --- 1. Get Market Data (Yahoo) ---
start_date <- as.Date("2017-01-01")
end_date <- as.Date("2025-11-01")
getSymbols(c("BTC-USD", "^IXIC"), src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

btc  <- `BTC-USD`[, "BTC-USD.Adjusted"]
ndaq <- `IXIC`[, "IXIC.Adjusted"]

# --- 2. Get Macroeconomic Data (FRED) ---
# Load ICSA data one week early to "prime" the na.locf() function
getSymbols("ICSA", src = "FRED", from = as.Date("2016-12-28"))
getSymbols(c("VIXCLS", "DGS10", "T10YIE", "T10Y2Y"), src = "FRED", from = start_date, to = end_date)

# --- 3. Align All Data to Weekly (Friday) ---

# Filter Daily data to Fridays
btc_fri  <- btc[weekdays(index(btc)) == "Friday"]
ndaq_fri <- ndaq[weekdays(index(ndaq)) == "Friday"]
vix_fri  <- VIXCLS[weekdays(index(VIXCLS)) == "Friday"]
d10_fri  <- DGS10[weekdays(index(DGS10)) == "Friday"]
t10_fri  <- T10YIE[weekdays(index(T10YIE)) == "Friday"]
t10y2_fri  <- T10Y2Y[weekdays(index(T10Y2Y)) == "Friday"]


# Rename columns
colnames(btc_fri) <- "BTC"
colnames(ndaq_fri) <- "NASDAQ"
colnames(vix_fri) <- "VIX"
colnames(d10_fri) <- "DGS10"
colnames(t10_fri) <- "T10YIE"
colnames(t10y2_fri) <- "T10Y2Y"


# Merge all Friday data first
weekly_data <- merge(ndaq_fri, btc_fri, vix_fri, d10_fri, t10_fri, t10y2_fri)

# Merge the weekly Saturday ICSA data
all_data_merged <- merge(weekly_data, ICSA, all = TRUE)

# Carry the last known observation forward (for Saturdays AND holidays)
all_data_filled <- na.locf(all_data_merged, fromLast = FALSE)

# Trim back to just our original Fridays
final_data_levels <- all_data_filled[index(weekly_data)]

# Clean up any NAs at the very beginning
final_data_levels <- na.omit(final_data_levels)

print("--- Final Merged Data (Levels) ---")
print(tail(final_data_levels))

# --- 4. Plot Levels (Before Transformation) ---
plot.xts(final_data_levels,
         main = "Weekly Macro & Market Levels (Friday Data)",
         cex.axis = 0.8,
         yaxis.same = FALSE,  # Use independent Y-axes
         multi.panel = TRUE)  # Plot in separate panels

# --- 5. Stationarity Test (Levels) ---
print("--- ADF Test for Stationarity (Levels) ---")
for (col in colnames(final_data_levels)) {
  test_result <- adf.test(final_data_levels[, col])
  print(paste("ADF p-value for", col, ":", round(test_result$p.value, 4)))
  # You will see large p-values (e.g., > 0.05), proving non-stationarity
}


for (col in colnames(final_data_levels)) {
  x_xts <- final_data_levels[, col]
  x_num <- as.numeric(x_xts)                 # numeric vector
  x_ts  <- ts(coredata(x_xts), frequency=52) # weekly-ish ts for ggAcf
  ttl1  <- paste0(col, ": Weekly Levels — Fri→Fri")
  ttl2  <- paste0(col, ": ACF — Weekly Levels")
  ttl3  <- paste0(col, ": ACF — Weekly Levels (base)")
  
  # Base plot of the series
  plot(x_xts, main = ttl1)
  
  # ggAcf (must print inside loop)
  p <- ggAcf(x_ts, lag.max = 52) + ggtitle(ttl2)
  print(p)
  
  # Base ACF
  acf(x_num, lag.max = 52, main = ttl3, na.action = na.omit)
}

# --- 6. Function to transform any dataset ---
transform_data <- function(data) {
  price_data <- data[, c("NASDAQ", "BTC", "VIX", "ICSA")]
  index_data <- data[, c("DGS10", "T10YIE", "T10Y2Y")]
  
  price_log_ret <- na.omit(diff(log(price_data)))
  index_diff    <- na.omit(diff(index_data))
  
  model_data <- merge(price_log_ret, index_diff)
  return(na.omit(model_data))
}

data_model_ready <- transform_data(final_data_levels)

print(head(data_model_ready))

# --- 7. Transformed Series Diagnostics (Returns & Differences) ---

plot.xts(data_model_ready,
         main = "Weekly Log Returns & Diffs (Stationary Data)",
         cex.axis = 0.8,
         yaxis.same = FALSE,
         multi.panel = TRUE)


for (col in colnames(data_model_ready)) {
  x_xts_ret <- data_model_ready[, col]
  x_num_ret <- as.numeric(x_xts_ret)                 # numeric vector
  x_ts_ret  <- ts(coredata(x_xts_ret), frequency=52) # weekly-ish ts for ggAcf
  ttl4  <- paste0(col, ": Weekly Returns — Fri→Fri")
  ttl5  <- paste0(col, ": ACF — Weekly Returns")
  ttl6  <- paste0(col, ": ACF — Weekly Returns (base)")
  
  # Base plot of the series
  plot(x_xts_ret, main = ttl4)
  
  # ggAcf (must print inside loop)
  p <- ggAcf(x_ts_ret, lag.max = 52) + ggtitle(ttl5)
  print(p)
  
  # Base ACF
  acf(x_num_ret, lag.max = 52, main = ttl6, na.action = na.omit)
}


# --- 8. Stationarity Test (Returns) ---
print("--- ADF Test for Stationarity (Log Returns) ---")
for (col in colnames(data_model_ready)) {
  test_result <- adf.test(data_model_ready[, col])
  print(paste("ADF p-value for", col, ":", round(test_result$p.value, 4)))
  # should see very small p-value
}

# --- 9. Correlation Matrix ---

library(corrplot)

cor_vars <- data_model_ready[, c("NASDAQ", "BTC", "VIX", "DGS10", "T10Y2Y", "T10YIE", "ICSA")]
cor_matrix <- cor(cor_vars, use = "complete.obs")
print(round(cor_matrix, 3))

corrplot(
  cor_matrix,
  method = "color",          # color-filled squares
  type = "upper",            # show upper triangle only
  tl.col = "black",          # text label color
  tl.srt = 45,               # label rotation
  addCoef.col = "black",     # add correlation coefficients
  number.cex = 0.7,          # coefficient text size
  col = colorRampPalette(c("blue", "white", "red"))(200), # color scale
  title = "Correlation Matrix of Stationary Variables",
  mar = c(0,0,2,0)
)


# ---------------------------------------------------------------
# ARIMAX MODEL (Using Actual Values) (Dependent Variable = BTC)
# ---------------------------------------------------------------

# --- 1. Data Preprocessing ---

total_obs <- nrow(data_model_ready)
split_point <- floor(total_obs * 0.80)


# --- 2. Split "Levels" Data CHRONOLOGICALLY ---
train_data_levels <- data_model_ready[1:split_point, ]
test_data_levels  <- data_model_ready[(split_point + 1):total_obs, ]

print(paste("Training observations:", nrow(train_data_levels)))
print(paste("Testing observations:", nrow(test_data_levels)))

# Cross Correlation
ccf(
  as.numeric(data_model_ready$NASDAQ),
  as.numeric(data_model_ready$BTC),
  lag.max = 12,
  main = "Cross-Correlation: NASDAQ → BTC"
)
# lag = 0

# VIX and BTC
ccf(as.numeric(data_model_ready$VIX), 
    as.numeric(data_model_ready$BTC),
    lag.max = 12,
    main = "CCF: VIX -> BTC")
# lag 0, -3 and -12

# DGS10 and BTC
ccf(as.numeric(data_model_ready$DGS10), 
    as.numeric(data_model_ready$BTC), 
    lag.max = 12,
    main = "CCF: DGS10 -> BTC")
# no significant leading indicator

# T10YIE and BTC
ccf(as.numeric(data_model_ready$T10YIE), 
    as.numeric(data_model_ready$BTC), 
    lag.max = 12,
    main = "CCF: T10YIE -> BTC")
# lag = 0, -10

# ICSA and BTC
ccf(as.numeric(data_model_ready$ICSA), 
    as.numeric(data_model_ready$BTC), 
    lag.max = 12,
    main = "CCF: ICSA -> BTC")
# lag = -5

# T10Y2Y and BTC
ccf(as.numeric(data_model_ready$T10Y2Y), 
    as.numeric(data_model_ready$BTC), 
    lag.max = 12,
    main = "CCF: T10Y2Y -> BTC")
# no significant leading indicator


# Create the xreg matrix from a given dataset
create_xreg_matrix <- function(data) {
  # Initialize an xts object with the right dates
  xreg <- xts(order.by = index(data))
  
  # Adding lags that are signficant (identified earlier)
  #NASDAQ
  xreg$ndaq_lag0 <- data$NASDAQ
  
  # VIX
  xreg$vix_lag0  <- data$VIX
  xreg$vix_lag3  <- stats::lag(data$VIX, k = 3)
  xreg$vix_lag12 <- stats::lag(data$VIX, k = 12)
  
  # T10YIE
  xreg$t10yie_lag0  <- data$T10YIE
  xreg$t10yie_lag10 <- stats::lag(data$T10YIE, k = 10)
  
  # ICSA
  xreg$icsa_lag5 <- stats::lag(data$ICSA, k = 5)
  
  # DGS10 had no significant lags, so we correctly leave it out.
  
  return(xreg)
}

# Create the specific xreg matrix for the TRAINING set
xreg_train_specific <- create_xreg_matrix(train_data_levels)


# --- 4. Combine and Clean for Final Model Training ---

# Merge the target (y) with all the new lagged predictors (xreg)
y_train <- train_data_levels$BTC
final_train_data <- merge(y_train, xreg_train_specific, all = FALSE)

# Clean NAs from the longest lag (k=12)
final_train_data_clean <- na.omit(final_train_data)
colnames(final_train_data_clean)[1] <- "BTC_target" # Rename y

# Separate into final train objects
y_train_final    <- final_train_data_clean$BTC_target
xreg_train_final <- as.matrix(final_train_data_clean[, -1])


# --- 5. Fit the ARIMAX Model on TRAINING Data ---
arimax_model <- auto.arima(
  y_train_final,
  xreg = xreg_train_final,
  stationary = TRUE,
  seasonal = FALSE
)

print("--- Final ARIMAX Model (Trained on Specific Lags) ---")
print(summary(arimax_model))


# --- 6. Validate on TEST Set (Out-of-Sample) ---

# Create the *exact same* lagged features for the test set
y_test <- test_data_levels$BTC
xreg_test_specific <- create_xreg_matrix(test_data_levels)

# Merge and clean test data
final_test_data <- merge(y_test, xreg_test_specific, all = FALSE)
final_test_data_clean <- na.omit(final_test_data)
colnames(final_test_data_clean)[1] <- "BTC_target"

# Separate into final test objects
y_test_final    <- final_test_data_clean$BTC_target
xreg_test_final <- as.matrix(final_test_data_clean[, -1])

# Generate forecasts using the *same* model
forecast_model <- Arima(y_test_final, model = arimax_model, xreg = xreg_test_final)

# Check the accuracy
print("--- Out-of-Sample Accuracy (Test Set) ---")
print(accuracy(forecast_model))


# --- 7. Plot Forecast vs. Actual (Out-of-Sample) ---

# Get the predictions from your model
# These are the "fitted" values on the test set
predictions <- forecast_model$fitted
predictions
# Get the actual values
actuals <- y_test_final

# 1. Merge them into a single xts object
comparison_data <- merge(actuals, predictions)

# 2. Rename the columns for a clean legend
colnames(comparison_data) <- c("Actual BTC Returns", "ARIMAX Forecast")

# 3. Plot the *merged* object.
# plot.xts is smart enough to handle the rest.
plot.xts(comparison_data, 
         main = "Out-of-Sample Forecast vs. Actual BTC Returns", 
         ylab = "Log Return",
         col = c("black", "red"),    # First column is black, second is red
         lwd = c(1, 1),              # Make "Actual" thicker
         lty = c(1, 1),              # 1=solid (Actual), 2=dashed (Forecast)
         legend.loc = "topleft")


# ------------------------------------------------------------------
# ARIMAX MODEL (Using Forecasted Values) (Dependent Variable = BTC)
# ------------------------------------------------------------------

y_tr <- as.numeric(train_data_levels$BTC)
y_te <- as.numeric(test_data_levels$BTC)
h    <- length(y_te)

# =====================================================
# 1) Define lag structure
# =====================================================
lags_spec <- list(
  NASDAQ = c(0),
  VIX    = c(0, 3, 12),
  T10YIE = c(0, 10),
  ICSA   = c(5)
)

# =====================================================
# 2) Helper functions
# =====================================================
fc_from_train <- function(x_tr, h) {
  fit <- auto.arima(ts(as.numeric(x_tr)), seasonal = FALSE)
  as.numeric(forecast(fit, h = h)$mean)
}

build_lag_cols <- function(vec, lags, prefix) {
  n <- length(vec)
  out <- lapply(lags, function(k) {
    v <- rep(NA_real_, n)
    if (k == 0) {
      v[] <- vec
    } else if (k < n) {
      v[(k+1):n] <- vec[1:(n-k)]
    }
    v
  })
  out <- as.data.frame(out, check.names = FALSE)
  names(out) <- paste0(tolower(prefix), "_lag", lags)
  out
}

# =====================================================
# 3) Build TRAIN xreg matrix and align y
# =====================================================
# --- TRAIN xreg (actuals) + align y ---
Xtr_list <- lapply(names(lags_spec), function(nm) {
  build_lag_cols(as.numeric(train_data_levels[, nm]), lags_spec[[nm]], nm)
})
X_train_full <- do.call(cbind, Xtr_list)
max_lag <- max(unlist(lags_spec))
keep_idx <- (max_lag + 1):nrow(X_train_full)
X_train <- as.matrix(X_train_full[keep_idx, , drop = FALSE])
y_tr_aligned <- y_tr[keep_idx]
stopifnot(nrow(X_train) == length(y_tr_aligned))

# --- TEST xreg from train + forecasts (no look-ahead) ---
Xte_list <- lapply(names(lags_spec), function(nm) {
  full <- c(as.numeric(train_data_levels[, nm]),
            fc_from_train(train_data_levels[, nm], h))
  full_lagged <- build_lag_cols(full, lags_spec[[nm]], nm)
  tail(full_lagged, h)
})
X_test <- as.matrix(do.call(cbind, Xte_list))

# --- Fit ARIMAX and forecast TEST ---
fit_arimax <- forecast::auto.arima(
  y = y_tr_aligned, xreg = X_train, seasonal = FALSE, stationary = TRUE
)
fc <- forecast::forecast(fit_arimax, xreg = X_test, h = h)
yhat <- as.numeric(fc$mean)

# --- Evaluate + plot ---
RMSE <- function(a,b) sqrt(mean((a-b)^2, na.rm = TRUE))
MAE  <- function(a,b) mean(abs(a-b),    na.rm = TRUE)
cat(sprintf("ARIMAX (lagged, forecasted xreg)  RMSE: %.5f   MAE: %.5f\n",
            RMSE(y_te, yhat), MAE(y_te, yhat)))

plot_df <- tibble(t = seq_len(h), Actual = y_te, ARIMAX = yhat) |>
  pivot_longer(-t, names_to = "Series", values_to = "Value")
ggplot(plot_df, aes(t, Value, color = Series)) +
  geom_line(linewidth = 0.9) +
  labs(title = "BTC — ARIMAX with lagged, forecasted exogenous drivers",
       x = "Test index", y = "Return / Δ", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")


# -------------------------------------------------------------------
# ARIMAX MODEL (Using Actual Values) (Dependent Variable = NASDAQ)
# -------------------------------------------------------------------

# Cross Correlation
ccf(
  as.numeric(data_model_ready$BTC),
  as.numeric(data_model_ready$NASDAQ),
  lag.max = 12,
  main = "Cross-Correlation: BTC → NASDAQ"
)
# lag = 0, -1, -4

# VIX and NASDAQ
ccf(as.numeric(data_model_ready$VIX), 
    as.numeric(data_model_ready$NASDAQ),
    lag.max = 12,
    main = "CCF: VIX -> NASDAQ")
# lag 0

# DGS10 and NASDAQ
ccf(as.numeric(data_model_ready$DGS10), 
    as.numeric(data_model_ready$NASDAQ), 
    lag.max = 12,
    main = "CCF: DGS10 -> NASDAQ")
# lag -5, -6

# T10YIE and NASDAQ
ccf(as.numeric(data_model_ready$T10YIE), 
    as.numeric(data_model_ready$NASDAQ), 
    lag.max = 12,
    main = "CCF: T10YIE -> NASDAQ")
# lag = 0

# ICSA and NASDAQ
ccf(as.numeric(data_model_ready$ICSA), 
    as.numeric(data_model_ready$NASDAQ), 
    lag.max = 12,
    main = "CCF: ICSA -> NASDAQ")
# lag = -3

# T10Y2Y and NASDAQ
ccf(as.numeric(data_model_ready$T10Y2Y), 
    as.numeric(data_model_ready$NASDAQ), 
    lag.max = 12,
    main = "CCF: T10Y2Y -> NSADAQ")
# lag = -7


# This function will create the xreg matrix from a given dataset
create_xreg_matrix_ndaq <- function(data) {
  # Initialize an xts object with the right dates
  xreg_ndaq <- xts(order.by = index(data))
  
  # Adding lags that are significant (identified earlier)
  # NASDAQ
  xreg_ndaq$btc_lag0  <- data$BTC
  xreg_ndaq$btc_lag1  <- stats::lag(data$BTC, k = 1)
  xreg_ndaq$btc_lag4 <- stats::lag(data$BTC, k = 4)
  
  # VIX
  xreg_ndaq$vix_lag0  <- data$VIX
  
  # T10YIE
  xreg_ndaq$t10yie_lag0  <- data$T10YIE
  
  # DGS10
  xreg_ndaq$dgs10_lag5 <- stats::lag(data$DGS10, k = 5)
  xreg_ndaq$dgs10_lag6 <- stats::lag(data$DGS10, k = 6)
  
  # ICSA
  xreg_ndaq$icsa_lag3 <- stats::lag(data$ICSA, k = 3)
  
  # T10Y2Y
  xreg_ndaq$t10y2y_lag7 <- stats::lag(data$T10Y2Y, k = 7)
  
  return(xreg_ndaq)
}

# Create the specific xreg matrix for the TRAINING set
xreg_train_specific_ndaq <- create_xreg_matrix_ndaq(train_data_levels)

# --- 4. Combine and Clean for Final Model Training ---

# Merge the target (y) with all the new lagged predictors (xreg)
y_train_ndaq <- train_data_levels$NASDAQ
final_train_data_ndaq <- merge(y_train_ndaq, xreg_train_specific_ndaq, all = FALSE)

# Clean NAs from the longest lag (k=7)
final_train_data_ndaq_clean <- na.omit(final_train_data_ndaq)
colnames(final_train_data_ndaq_clean)[1] <- "NASDAQ_target" # Rename y

# Separate into final train objects
y_train_final_ndaq    <- final_train_data_ndaq_clean$NASDAQ_target
xreg_train_final_ndaq <- as.matrix(final_train_data_ndaq_clean[, -1])

# --- 5. Fit the ARIMAX Model on TRAINING Data ---
arimax_ndaq_model <- auto.arima(
  y_train_final_ndaq,
  xreg = xreg_train_final_ndaq,
  stationary = TRUE,
  seasonal = FALSE
)

print("--- Final ARIMAX Model (Trained on Specific Lags) ---")
print(summary(arimax_ndaq_model))


# --- 6. Validate on TEST Set (Out-of-Sample) ---

# Create the *exact same* lagged features for the test set
y_test_ndaq <- test_data_levels$NASDAQ
xreg_test_ndaq_specific <- create_xreg_matrix_ndaq(test_data_levels)

# Merge and clean test data
final_test_data_ndaq <- merge(y_test_ndaq, xreg_test_ndaq_specific, all = FALSE)
final_test_data_ndaq_clean <- na.omit(final_test_data_ndaq)
colnames(final_test_data_ndaq_clean)[1] <- "NASDAQ_target"

# Separate into final test objects
y_test_ndaq_final    <- final_test_data_ndaq_clean$NASDAQ_target
xreg_test_ndaq_final <- as.matrix(final_test_data_ndaq_clean[, -1])

# Generate forecasts using the *same* model
forecast_model_ndaq <- Arima(y_test_ndaq_final, model = arimax_ndaq_model, xreg = xreg_test_ndaq_final)

# Check the accuracy
print("--- Out-of-Sample Accuracy (Test Set) ---")
print(accuracy(forecast_model_ndaq))


# --- 7. Plot Forecast vs. Actual (Out-of-Sample) ---

# Get the predictions from the model
# These are the "fitted" values on the test set
predictions_ndaq <- forecast_model_ndaq$fitted
predictions_ndaq
# Get the actual values
actuals_ndaq <- y_test_ndaq_final

# 1. Merge them into a single xts object
comparison_data_ndaq <- merge(actuals_ndaq, predictions_ndaq)

# 2. Rename the columns for a clean legend
colnames(comparison_data_ndaq) <- c("Actual NASDAQ Returns", "ARIMAX Forecast")

# 3. Plot the *merged* object.
plot.xts(comparison_data_ndaq, 
         main = "Out-of-Sample Forecast vs. Actual NASDAQ Returns", 
         ylab = "Log Return",
         col = c("black", "red"),    # First column is black, second is red
         lwd = c(1, 1),              # Make "Actual" thicker
         lty = c(1, 1),              # 1=solid (Actual), 2=dashed (Forecast)
         legend.loc = "topleft")


# --------------------------------------------------------------------
# ARIMAX MODEL (Using Forecasted Values) (Dependent Variable = NASDAQ) 
# --------------------------------------------------------------------

y_tr_ndaq <- as.numeric(train_data_levels$NASDAQ)
y_te_ndaq <- as.numeric(test_data_levels$NASDAQ)
h <- length(y_te_ndaq)

# ================================
# 1) Lag spec from your CCF notes
# ================================
lags_spec_ndaq <- list(
  BTC   = c(0, 1, 4),
  VIX   = c(0),
  DGS10 = c(5, 6),
  T10YIE= c(0),
  ICSA  = c(3),
  T10Y2Y= c(7)
)

# ================================
# 3) TRAIN xreg (actuals) + align y
# ================================
Xtr_list <- lapply(names(lags_spec_ndaq), function(nm) {
  build_lag_cols(as.numeric(train_data_levels[, nm]), lags_spec_ndaq[[nm]], nm)
})
X_train_full_ndaq <- do.call(cbind, Xtr_list)

max_lag_ndaq <- max(unlist(lags_spec_ndaq))
keep_idx <- (max_lag_ndaq + 1):nrow(X_train_full_ndaq)

X_train_ndaq <- as.matrix(X_train_full_ndaq[keep_idx, , drop = FALSE])
y_tr_ndaq_aligned <- y_tr_ndaq[keep_idx]
stopifnot(nrow(X_train_ndaq) == length(y_tr_ndaq_aligned))

# ================================
# 4) TEST xreg from train + forecasts (no look-ahead)
# ================================
Xte_list <- lapply(names(lags_spec_ndaq), function(nm) {
  full <- c(as.numeric(train_data_levels[, nm]),
            fc_from_train(train_data_levels[, nm], h))
  full_lagged <- build_lag_cols(full, lags_spec_ndaq[[nm]], nm)
  tail(full_lagged, h)
})
X_test_ndaq <- as.matrix(do.call(cbind, Xte_list))

# ================================
# 5) Fit ARIMAX on TRAIN and forecast TEST
# ================================
fit_arimax_ndaq <- auto.arima(
  y = y_tr_ndaq_aligned,
  xreg = X_train_ndaq,
  seasonal = FALSE,
  stationary = TRUE  # set FALSE if NASDAQ is in levels (not returns/Δ)
)

fc_ndaq <- forecast(fit_arimax_ndaq, xreg = X_test_ndaq, h = h)
yhat_ndaq <- as.numeric(fc_ndaq$mean)

# ================================
# 6) Evaluate & plot
# ================================
cat(sprintf("NASDAQ ARIMAX (lagged, forecasted xreg)  RMSE: %.5f   MAE: %.5f\n",
            RMSE(y_te_ndaq, yhat_ndaq), MAE(y_te_ndaq, yhat_ndaq)))

plot_df_ndaq <- tibble(t = seq_len(h), Actual = y_te_ndaq, ARIMAX = yhat_ndaq) |>
  pivot_longer(-t, names_to = "Series", values_to = "Value")

ggplot(plot_df_ndaq, aes(t, Value, color = Series)) +
  geom_line(linewidth = 0.9) +
  labs(title = "NASDAQ — ARIMAX with lagged, forecasted exogenous drivers",
       x = "Test index", y = "Return / Δ", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")



# -----------------------------------
# ARIMA FORECASTED STRUCTURAL MODEL
# -----------------------------------

library(tibble)

# ----------------------------
# 1) Train/Test split (assumes train_data_levels / test_data_levels already exist)
# ----------------------------
n <- NROW(data_model_ready)
split_idx <- floor(0.8 * n)

# Target: BTC
y_btc_tr <- ts(as.numeric(train_data_levels$BTC), frequency = 1)
y_btc_te <- ts(as.numeric(test_data_levels$BTC),
               start = length(y_btc_tr) + 1,
               frequency = frequency(y_btc_tr))

# Optional benchmark: univariate ARIMA on BTC (fit on TRAIN only)
fit_btc_tr <- auto.arima(y_btc_tr, stepwise = FALSE, approximation = FALSE, biasadj = TRUE)
fc_btc_tr  <- forecast(fit_btc_tr, h = length(y_btc_te))

# ----------------------------
# 2) Helper to forecast from TRAIN slice only
# ----------------------------
fc_from_train <- function(x, s_idx, h) {
  x_num <- as.numeric(x)
  fit <- auto.arima(ts(x_num[1:s_idx]),
                    stepwise = FALSE, approximation = FALSE, biasadj = TRUE)
  as.numeric(forecast(fit, h = h)$mean)
}

# Series (levels or transformed series consistent with your regression scale)
d_ndaq   <- data_model_ready$NASDAQ
d_vix    <- data_model_ready$VIX
d_tr10   <- data_model_ready$DGS10
d_t10yie <- data_model_ready$T10YIE
d_t10y2y <- data_model_ready$T10Y2Y
d_icsa   <- data_model_ready$ICSA

h <- length(y_btc_te)

# ----------------------------
# 3) Build X_train (ACTUALS on TRAIN) and Xhat_test (FORECASTS from TRAIN for TEST horizon)
# ----------------------------
X_train <- data.frame(
  NASDAQ = as.numeric(train_data_levels$NASDAQ),
  VIX    = as.numeric(train_data_levels$VIX),
  DGS10  = as.numeric(train_data_levels$DGS10),
  T10YIE = as.numeric(train_data_levels$T10YIE),
  T10Y2Y = as.numeric(train_data_levels$T10Y2Y),
  ICSA   = as.numeric(train_data_levels$ICSA)
)

Xhat_test <- data.frame(
  NASDAQ = fc_from_train(d_ndaq,   split_idx, h),
  VIX    = fc_from_train(d_vix,    split_idx, h),
  DGS10  = fc_from_train(d_tr10,   split_idx, h),
  T10YIE = fc_from_train(d_t10yie, split_idx, h),
  T10Y2Y = fc_from_train(d_t10y2y, split_idx, h),
  ICSA   = fc_from_train(d_icsa,   split_idx, h)
)

# ----------------------------
# 4) Fit structural model on TRAIN actuals, predict TEST with forecasted X
# ----------------------------
m_btc <- lm(as.numeric(y_btc_tr) ~ ., data = X_train)
yhat_btc_struct <- as.numeric(predict(m_btc, newdata = Xhat_test))

# ----------------------------
# 5) Evaluate vs actuals (and optional ARIMA benchmark)
# ----------------------------
btc_actual    <- as.numeric(y_btc_te)
btc_arima_oos <- as.numeric(fc_btc_tr$mean)

RMSE <- function(a,b) sqrt(mean((a-b)^2, na.rm = TRUE))
MAE  <- function(a,b) mean(abs(a-b), na.rm = TRUE)

results <- tibble(
  Target = "BTC",
  Model  = c("ARIMA (univariate)", "Structural (ARIMA-forecasted X)"),
  RMSE   = c(RMSE(btc_actual, btc_arima_oos),
             RMSE(btc_actual, yhat_btc_struct)),
  MAE    = c(MAE (btc_actual, btc_arima_oos),
             MAE (btc_actual, yhat_btc_struct))
)

print(results)


# ----------------------------
# 6) Visualization: Actual vs. ARIMA vs. Structural forecasts
# ----------------------------
library(ggplot2)

plot_df <- tibble(
  time  = seq_along(btc_actual),
  Actual = btc_actual,
  ARIMA  = btc_arima_oos,
  Struct = yhat_btc_struct
) |>
  tidyr::pivot_longer(-time, names_to = "Series", values_to = "Value")

ggplot(plot_df, aes(x = time, y = Value, color = Series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "BTC — Actual vs. ARIMA vs. Structural (ARIMA-forecasted drivers)",
    x = "Test period",
    y = "Value / Return",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# ============================
# NASDAQ as Target (mirror of BTC block)
# ============================

# 1) Target series (train/test)
y_ndaq_tr <- ts(as.numeric(train_data_levels$NASDAQ), frequency = 1)
y_ndaq_te <- ts(as.numeric(test_data_levels$NASDAQ),
                start = length(y_ndaq_tr) + 1,
                frequency = frequency(y_ndaq_tr))

# 2) Univariate ARIMA benchmark for NASDAQ (fit on TRAIN only)
fit_ndaq_tr <- auto.arima(y_ndaq_tr, stepwise = FALSE, approximation = FALSE, biasadj = TRUE)
fc_ndaq_tr  <- forecast(fit_ndaq_tr, h = length(y_ndaq_te))

# 3) Build design matrices
#    - Endogenous target: NASDAQ
#    - Exogenous drivers for NASDAQ: BTC + macros (contemporaneous)
X_train_ndaq <- data.frame(
  BTC    = as.numeric(train_data_levels$BTC),
  VIX    = as.numeric(train_data_levels$VIX),
  DGS10  = as.numeric(train_data_levels$DGS10),
  T10YIE = as.numeric(train_data_levels$T10YIE),
  T10Y2Y = as.numeric(train_data_levels$T10Y2Y),
  ICSA   = as.numeric(train_data_levels$ICSA)
)

h_ndaq <- length(y_ndaq_te)
Xhat_test_ndaq <- data.frame(
  BTC    = fc_from_train(data_model_ready$BTC,    split_idx, h_ndaq),
  VIX    = fc_from_train(data_model_ready$VIX,    split_idx, h_ndaq),
  DGS10  = fc_from_train(data_model_ready$DGS10,  split_idx, h_ndaq),
  T10YIE = fc_from_train(data_model_ready$T10YIE, split_idx, h_ndaq),
  T10Y2Y = fc_from_train(data_model_ready$T10Y2Y, split_idx, h_ndaq),
  ICSA   = fc_from_train(data_model_ready$ICSA,   split_idx, h_ndaq)
)

# 4) Fit structural model on TRAIN actuals, predict TEST with forecasted X
m_ndaq <- lm(as.numeric(y_ndaq_tr) ~ ., data = X_train_ndaq)
yhat_ndaq_struct <- as.numeric(predict(m_ndaq, newdata = Xhat_test_ndaq))

# 5) Evaluate vs actuals (and ARIMA benchmark)
ndaq_actual    <- as.numeric(y_ndaq_te)
ndaq_arima_oos <- as.numeric(fc_ndaq_tr$mean)

RMSE <- function(a,b) sqrt(mean((a-b)^2, na.rm = TRUE))
MAE  <- function(a,b) mean(abs(a-b), na.rm = TRUE)

results_ndaq <- tibble(
  Target = "NASDAQ",
  Model  = c("ARIMA (univariate)", "Structural (ARIMA-forecasted X)"),
  RMSE   = c(RMSE(ndaq_actual, ndaq_arima_oos),
             RMSE(ndaq_actual, yhat_ndaq_struct)),
  MAE    = c(MAE (ndaq_actual, ndaq_arima_oos),
             MAE (ndaq_actual, yhat_ndaq_struct))
)

print(results_ndaq)

# 6) Visualization
plot_df_ndaq <- tibble(
  time   = seq_along(ndaq_actual),
  Actual = ndaq_actual,
  ARIMA  = ndaq_arima_oos,
  Struct = yhat_ndaq_struct
) |>
  tidyr::pivot_longer(-time, names_to = "Series", values_to = "Value")

ggplot(plot_df_ndaq, aes(x = time, y = Value, color = Series)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "NASDAQ — Actual vs. ARIMA vs. Structural (ARIMA-forecasted drivers)",
    x = "Test period",
    y = "Value / Return",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# -------------------------------------------------------------------
# VAR MODELS (Using Forecasts)
# -------------------------------------------------------------------

# ======================================================================
# VAR(2-var): BTC & NASDAQ ONLY  (no exogenous variables)
# ======================================================================
Y2      <- data_model_ready[, c("BTC","NASDAQ")]
Y2_tr   <- Y2[1:split_point, , drop = FALSE]
Y2_te   <- Y2[(split_point+1):n, , drop = FALSE]

# Lag selection (AIC)
sel2 <- VARselect(Y2_tr, lag.max = 12, type = "const")$selection[["AIC(n)"]]
p2   <- ifelse(is.null(sel2), 3, sel2)

var2 <- VAR(Y2_tr, p = p2, type = "const")
cat("\n[VAR(2-var)] chosen lag p =", p2, "\n")
print(roots(var2))  # stability: all moduli < 1
print(summary(var2))

# Forecast
fc2 <- predict(var2, n.ahead = h, ci = 0.95)

btc_fc2  <- fc2$fcst$BTC[ , "fcst"]
ndaq_fc2 <- fc2$fcst$NASDAQ[ , "fcst"]

btc_act2  <- Y2_te$BTC[seq_along(btc_fc2)]
ndaq_act2 <- Y2_te$NASDAQ[seq_along(ndaq_fc2)]

perf_var2 <- tibble(
  Model = c("VAR(2) BTC","VAR(2) NASDAQ"),
  RMSE  = c(RMSE(btc_act2,  btc_fc2),  RMSE(ndaq_act2, ndaq_fc2)),
  MAE   = c(MAE (btc_act2,  btc_fc2),  MAE (ndaq_act2, ndaq_fc2))
)
print(perf_var2)

# Pairwise Granger within VAR(2)
cat("\n[VAR(2)] Granger BTC→NASDAQ p-value:",
    causality(var2, cause = "BTC")$Granger$p.value, "\n")
cat("[VAR(2)] Granger NASDAQ→BTC p-value:",
    causality(var2, cause = "NASDAQ")$Granger$p.value, "\n")

# IRFs (2-var)
irf2_btc_to_ndaq <- irf(var2, impulse="BTC",    response="NASDAQ", n.ahead=12, boot=TRUE)
irf2_ndaq_to_btc <- irf(var2, impulse="NASDAQ", response="BTC",    n.ahead=12, boot=TRUE)
plot(irf2_btc_to_ndaq); plot(irf2_ndaq_to_btc)

plot(var2)
plot(var2, names = "NASDAQ")

# ======================================================================
# VAR(7-var): BTC, NASDAQ, VIX, T10YIE, ICSA, DGS10, T10Y2Y  (all endogenous)
# ======================================================================
Y7_cols <- c("BTC","NASDAQ","VIX","T10YIE","ICSA","DGS10","T10Y2Y")
Y7_df   <- data.frame(coredata(data_model_ready[, Y7_cols]))     # from YX to keep rows synced
colnames(Y7_df) <- Y7_cols

Y7_tr <- Y7_df[1:split_point, , drop = FALSE]
Y7_te <- Y7_df[(split_point+1):n, , drop = FALSE]

# Lag selection (lower lag.max to avoid overfitting with many params)
sel7 <- VARselect(Y7_tr, lag.max = 8, type = "const")$selection[["AIC(n)"]]
p7   <- ifelse(is.null(sel7), 2, sel7)

var7 <- VAR(Y7_tr, p = p7, type = "const")
cat("\n[VAR(7-var)] chosen lag p =", p7, "\n")
print(roots(var7))  # stability check
print(summary(var7))

# Forecast h steps
fc7 <- predict(var7, n.ahead = h, ci = 0.95)

btc_fc7  <- fc7$fcst$BTC[ , "fcst"]
ndaq_fc7 <- fc7$fcst$NASDAQ[ , "fcst"]

btc_act7  <- Y7_te$BTC[seq_along(btc_fc7)]
ndaq_act7 <- Y7_te$NASDAQ[seq_along(ndaq_fc7)]

perf_var7 <- tibble(
  Model = c("VAR(7) BTC","VAR(7) NASDAQ"),
  RMSE  = c(RMSE(btc_act7,  btc_fc7),  RMSE(ndaq_act7, ndaq_fc7)),
  MAE   = c(MAE (btc_act7,  btc_fc7),  MAE (ndaq_act7, ndaq_fc7))
)
print(perf_var7)

# Granger within VAR(7)
cat("\n[VAR(7)] Granger BTC→NASDAQ p-value:",
    causality(var7, cause = "BTC")$Granger$p.value, "\n")
cat("[VAR(7)] Granger NASDAQ→BTC p-value:",
    causality(var7, cause = "NASDAQ")$Granger$p.value, "\n")

# Block Granger: do macros jointly Granger-cause {BTC,NASDAQ}?
macro_vars <- c("VIX","T10YIE","ICSA","DGS10","T10Y2Y")
cat("[VAR(7)] Block Granger (macros jointly) p-value:",
    causality(var7, cause = macro_vars)$Granger$p.value, "\n")

# IRFs between BTC and NASDAQ in the 7-var system (orthogonalized)
irf7_btc_to_ndaq <- irf(var7, impulse="BTC",    response="NASDAQ", n.ahead=12, boot=TRUE, ortho=TRUE)
irf7_ndaq_to_btc <- irf(var7, impulse="NASDAQ", response="BTC",    n.ahead=12, boot=TRUE, ortho=TRUE)
plot(irf7_btc_to_ndaq); plot(irf7_ndaq_to_btc)

plot(var7)
plot(var7, names = "NASDAQ")

# ======================================================================
# Tidy comparison table
# ======================================================================
out <- dplyr::bind_rows(
  dplyr::mutate(perf_var2, System = "VAR(2)"),
  dplyr::mutate(perf_var7, System = "VAR(7)")) %>%
  dplyr::select(System, dplyr::everything())

print(out)
