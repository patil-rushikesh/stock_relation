# Function to calculate EMAs
calculate_ema <- function(df, period) {
  df[[paste0("EMA_", period)]] <- EMA(df$Close, n = period)
  return(df)
}

# Function to calculate SMAs
calculate_sma <- function(df, period) {
  df[[paste0("SMA_", period)]] <- SMA(df$Close, n = period)
  return(df)
}

# Function to calculate MACD
calculate_macd <- function(df) {
  macd_values <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  df <- cbind(df, macd_values)
  return(df)
}

# Function to calculate RSI
calculate_rsi <- function(df, period = 14) {
  df$RSI <- RSI(df$Close, n = period)
  return(df)
}
# Function to clean the data by removing NA values
clean_data <- function(df) {
  original_shape <- dim(df)
  df_dropped <- na.omit(df)  # Drop rows with any NA values
  new_shape <- dim(df_dropped)
  
  message(paste("Original shape:", paste(original_shape, collapse = ", "),
                "New shape after dropping NAs:", paste(new_shape, collapse = ", ")))
  
  return(df_dropped)
}

# Function to drop outliers based on the Interquartile Range (IQR)
drop_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25)
  Q3 <- quantile(df[[column]], 0.75)
  IQR <- Q3 - Q1  # Interquartile Range
  
  # Define the outlier range
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Drop outliers
  df_no_outliers <- df[df[[column]] >= lower_bound & df[[column]] <= upper_bound, ]
  
  return(df_no_outliers)
}

