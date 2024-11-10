# Installing necessary packages
install.packages("quantmod")
install.packages("TTR")
install.packages("randomForest")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")
install.packages("gridExtra")
# install.packages("GGally")

# Loading libraries
library(quantmod)
library(TTR)
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)
library(GGally)

# Setting parameters for stock analysis
tickers <- c('AAPL', 'MSFT', 'GOOGL', 'AMZN')
start_date <- '2015-01-01'
end_date <- '2024-01-01'

# Fetching historical data for all tickers
data <- list()
for (ticker in tickers) {
  stock_data <- getSymbols(ticker, src = 'yahoo', from = start_date, to = end_date, auto.assign = FALSE)
  data[[ticker]] <- stock_data
}

# Data Analysis
for (ticker in tickers) {
  print(head(data[[ticker]]))
  print(tail(data[[ticker]]))
}

# Convert data to data frames and select necessary columns
dataFrames <- list()
for (ticker in tickers) {
  df <- data[[ticker]]
  colnames(df) <- gsub(paste0(ticker, "."), "", colnames(df))  # Remove ticker prefix
  # Ensure columns exist and are selected safely
  selected_cols <- intersect(colnames(df), c("Open", "High", "Low", "Close", "Adjusted"))
  dataFrames[[ticker]] <- df[, selected_cols]
  print(summary(dataFrames[[ticker]]))
}


# Calculating Technical Indicators: RSI, EMA, SMA, MACD, Signal Line
for (ticker in tickers) {
  df <- dataFrames[[ticker]]
  df$RSI <- RSI(df$Close, n = 14)
  df$EMA_20 <- EMA(df$Close, n = 20)
  df$EMA_50 <- EMA(df$Close, n = 50)
  df$SMA_20 <- SMA(df$Close, n = 20)
  df$SMA_50 <- SMA(df$Close, n = 50)
  macd_result <- MACD(df$Close, nFast = 20, nSlow = 50, nSig = 9)
  df$MACD <- macd_result[, 1]
  df$Signal_Line <- macd_result[, 2]
  dataFrames[[ticker]] <- na.omit(df)  # Remove NA values
}

# Dropping Outliers Function
drop_outliers <- function(df, column) {
  Q1 <- quantile(df[, column], 0.25)
  Q3 <- quantile(df[, column], 0.75)
  IQR <- Q3 - Q1
  df[df[, column] >= (Q1 - 1.5 * IQR) & df[, column] <= (Q3 + 1.5 * IQR), ]
}

# Removing outliers in each technical indicator
for (ticker in tickers) {
  dataFrames[[ticker]] <- drop_outliers(dataFrames[[ticker]], 'RSI')
  dataFrames[[ticker]] <- drop_outliers(dataFrames[[ticker]], 'EMA_20')
  dataFrames[[ticker]] <- drop_outliers(dataFrames[[ticker]], 'EMA_50')
  dataFrames[[ticker]] <- drop_outliers(dataFrames[[ticker]], 'SMA_20')
  dataFrames[[ticker]] <- drop_outliers(dataFrames[[ticker]], 'SMA_50')
  dataFrames[[ticker]] <- drop_outliers(dataFrames[[ticker]], 'MACD')
  dataFrames[[ticker]] <- drop_outliers(dataFrames[[ticker]], 'Signal_Line')
}

# Set theme
theme_set(theme_minimal())

# Visualization: Open, Close, High, Low Trends for each ticker
for (ticker in tickers) {
  df <- dataFrames[[ticker]]
  
  # Convert to data frame and add the Date column
  df <- data.frame(Date = index(df), coredata(df))
  
  p <- ggplot(df, aes(x = Date)) +
    geom_line(aes(y = Open, color = "Open")) +
    geom_line(aes(y = Close, color = "Close")) +
    geom_line(aes(y = High, color = "High")) +
    geom_line(aes(y = Low, color = "Low")) +
    labs(title = paste('Stock Price Trends of', ticker),
         x = 'Date', y = 'Price') +
    scale_color_manual(values = c("Open" = "orange", "Close" = "blue", "High" = "green", "Low" = "red")) +
    theme(legend.position = "top")
  
  print(p)
}


# Preparing plot_list for grid.arrange with Date column added
plot_list <- list()
for (ticker in tickers) {
  df <- dataFrames[[ticker]]
  
  # Convert to data frame and add Date column
  df <- data.frame(Date = index(df), coredata(df))
  
  # Create the plot for each ticker
  plot_list[[ticker]] <- ggplot(df, aes(x = Date, y = Close)) +
    geom_line() +
    labs(title = ticker, x = 'Date', y = 'Close Price')
}

# Display plots in a grid layout with 2 columns
do.call(grid.arrange, c(plot_list, ncol = 2))

# Technical Indicator Comparisons Across Companies: EMA_20, SMA_20, EMA_50, SMA_50, Signal Line
# List of indicators to plot
indicators <- c("EMA_20", "SMA_20", "EMA_50", "SMA_50", "Signal_Line")

for (indicator in indicators) {
  plot_list <- list()
  
  for (ticker in tickers) {
    df <- dataFrames[[ticker]]
    
    # Convert xts object to a data frame and add Date column
    df <- data.frame(Date = index(df), coredata(df))
    
    # Check if the indicator column exists before plotting
    if (indicator %in% colnames(df)) {
      plot_list[[ticker]] <- ggplot(df, aes(x = Date, y = .data[[indicator]])) +
        geom_line() +
        labs(title = paste(ticker, indicator), x = 'Date', y = indicator)
    } else {
      message(paste("Indicator", indicator, "not found in", ticker))
    }
  }
  
  # Arrange all plots in a 2-column layout
  if (length(plot_list) > 0) {
    do.call(grid.arrange, c(plot_list, ncol = 2))
  }
}

# Ensure each data frame has a Date column and a Company column, then combine
combined_df <- bind_rows(
  lapply(names(dataFrames), function(ticker) {
    df <- data.frame(Date = index(dataFrames[[ticker]]), coredata(dataFrames[[ticker]]))
    df$Company <- ticker
    df
  })
)

# Check the first few rows to confirm structure
head(combined_df)


# Distribution of Closing Prices Across Companies
ggplot(combined_df, aes(x = Company, y = Close)) +
  geom_boxplot() +
  labs(title = 'Distribution of Closing Prices Across Companies', y = 'Close Price', x = 'Company')

# KDE with Rug Plot of Closing Prices by Company
ggplot(combined_df, aes(x = Close, color = Company)) +
  geom_density() +
  geom_rug() +
  labs(title = 'KDE with Rug Plot of Closing Prices')

# StockPricePredictor Class Definition
StockPricePredictor <- setRefClass(
  "StockPricePredictor",
  fields = list(
    data = "data.frame",
    target_company = "character",
    features = "data.frame",
    target = "numeric",
    model = "ANY",
    X_train = "matrix",
    X_test = "matrix",
    y_train = "numeric",
    y_test = "numeric"
  ),
  methods = list(
    initialize = function(data, target_company) {
      .self$data <- data
      .self$target_company <- target_company
    },
    
    prepare_data = function() {
      # Ensure the 'Company' column is a factor
      .self$data$Company <- as.factor(.self$data$Company)
      
      # Filter data for the target company
      company_data <- .self$data[.self$data$Company == .self$target_company, ]
      
      # Extract relevant features for the model
      .self$features <- company_data[, c("Open", "High", "Low", "Adjusted", "RSI", "EMA_20", "EMA_50", "SMA_20", "SMA_50", "MACD", "Signal_Line")]
      
      # Prepare the target variable for the selected company
      .self$target <- company_data[["Close"]]
    },
    
    create_datasets = function() {
      # Convert features to a matrix and target to a numeric vector
      X <- as.matrix(.self$features)
      y <- as.numeric(.self$target)
      list(X, y)
    },
    
    train_model = function(X, y) {
      set.seed(42)
      trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
      
      .self$X_train <- X[trainIndex, ]
      .self$X_test <- X[-trainIndex, ]
      
      .self$y_train <- y[trainIndex]
      .self$y_test <- y[-trainIndex]
      
      # Print dimensions for debugging
      cat(sprintf("Dimensions of X_train: %s\n", paste(dim(.self$X_train), collapse = "x")))
      cat(sprintf("Dimensions of X_test: %s\n", paste(dim(.self$X_test), collapse = "x")))
      cat(sprintf("Length of y_train: %d\n", length(.self$y_train)))
      cat(sprintf("Length of y_test: %d\n", length(.self$y_test)))
      
      # Train the model
      .self$model <- randomForest(.self$X_train, .self$y_train, ntree = 100)
      
      # Make predictions
      y_pred <- predict(.self$model, newdata = .self$X_test)
      
      # Check lengths
      cat(sprintf("Length of y_pred: %d\n", length(y_pred)))
      
      # Calculate performance metrics
      mse <- mean((.self$y_test - y_pred) ^ 2)
      r2 <- cor(.self$y_test, y_pred, use = "complete.obs") ^ 2
      cat(sprintf("Mean Squared Error: %.2f\n", mse))
      cat(sprintf("R² Score: %.2f\n", r2))
      
      y_pred
    },
    
    plot_predictions = function(y_pred) {
      results <- data.frame(Actual = .self$y_test, Predicted = y_pred)
      ggplot(results, aes(x = 1:nrow(results))) +
        geom_line(aes(y = Actual, color = "Actual")) +
        geom_line(aes(y = Predicted, color = "Predicted")) +
        labs(title = sprintf("Actual vs Predicted Closing Price of %s", .self$target_company),
             x = "Index", y = "Closing Price") +
        scale_color_manual("", values = c("Actual" = "blue", "Predicted" = "orange"))
    },
    
    feature_importances = function() {
      # Get importance values from the model
      importance_values <- importance(.self$model)
      
      # Ensure importance values are valid and create the data frame
      if (!is.null(importance_values) && nrow(importance_values) > 0) {
        # Convert importance values to a data frame
        importance_df <- data.frame(
          Feature = rownames(importance_values),
          Importance = importance_values[, 1]  # Assuming the first column contains importance scores
        )
        
        # Remove NA values if any
        importance_df <- na.omit(importance_df)
        
        # Check if importance_df is not empty before plotting
        if (nrow(importance_df) > 0) {
          ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            coord_flip() +
            labs(title = "Feature Importances", x = "Feature", y = "Importance")
        } else {
          cat("No feature importance data available for plotting.\n")
        }
      } else {
        cat("No importance values returned from the model.\n")
      }
    }
    

  )
)

# Initialize the StockPricePredictor class with the dataset and target company
predictor <- StockPricePredictor$new(combined_df, "AAPL")

# Prepare data, create datasets, and train the model
predictor$prepare_data()
datasets <- predictor$create_datasets()
X <- datasets[[1]]
y <- datasets[[2]]

# Train the model and get predictions
y_pred <- predictor$train_model(X, y)

# Plot predictions
predictor$plot_predictions(y_pred)

# Display feature importances
predictor$feature_importances()

