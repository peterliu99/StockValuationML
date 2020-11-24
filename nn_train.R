# nn_train.R
# Training the neural network neural network to predict stock price


#-------------------------------------------------------------------------------------------------
# PART 1: GLOBAL PARAMETERS
startDate <- "2007-01-01" # the first date supported by getSymbols for "AAPL" is "2007-01-03"
NN_vars <- c("alpha", "DDM_price") # names of variables which are fed into training neural network

#-------------------------------------------------------------------------------------------------
# PART 2: FUNCTIONS

# Calculates net growth rates between elements of a vector
# Argument x: vector containing prices to calculate returns
# Value: vector of the same length as x containing each period's return (NA in first period)
returns <- function(x) {
  x_prev <- c(NA, x[-length(x)])
  (x - x_prev) / x_prev
}


# Gets stock prices
# Argument stocks: vector of strings containing stock symbols
# Argument commonDates: logical value to indicate if only data of dates common to all stocks should be kept
# Value: list where each component represents a stock. Each component is a data frame with the first variable being dates, second variable being prices.
getStockPrices <- function(stocks, commonDates = TRUE) {
  library(quantmod)
  
  # Gets all stocks' prices since 2007
  # Arg symbol: string containing one stock symbol
  # Value: data frame with date as the first variable, stock prices as second variable
  getSymbols_wrapper <- function(symbol) {
    stock_data <- getSymbols(symbol, from = startDate, auto.assign = FALSE)[, 4]
    data.frame(date = rownames(data.frame(stock_data)), price = as.numeric(stock_data), stringsAsFactors = FALSE)
  }
  
  prices <- lapply(stocks, getSymbols_wrapper)
  names(prices) <- stocks
  
  # Algorithm to only keep dates which are common to all stocks if commonDates == TRUE
  if (commonDates) {
    # Create list with dates from each stock
    dates_list <- list()
    for (i in seq_len(length(prices))) {
      dates_list[[i]] <- prices[[i]]$date
    }
    
    # Find common dates
    common_dates <- Reduce(intersect, dates_list)
    
    # Delete rows in each stock's data frame if that date is not in common_dates
    # Loop over each stock's data frame
    for (i in seq_len(length(prices))) {
      delete_rows <- numeric(0) # vector to store row numbers that don't have dates in common_dates
      
      # Loop over each row
      for (j in seq_len(nrow(prices[[i]]))) {
        # If that row's date isn't in common_dates, add that row number to the vector of row numbers to delete
        if (!(prices[[i]]$date[j] %in% common_dates)) {
          delete_rows <- c(delete_rows, j)
        }
      }
      
      # Delete rows with non-common dates, but only do so if delete_rows is non-empty
      if (length(delete_rows) > 0) {
        prices[[i]] <- prices[[i]][-delete_rows, ]
      }
    }
  }
  prices
}


# Gets stock info
# Argument stocks: vector of strings containing stock symbols
# Value: list where each component represents a stock. Each component is a data frame with the first variable being dates, second variable being prices, third variable being returns, fourth variable being beta over time
getStockInfo <- function(stocks) {
  info <- getStockPrices(stocks)
  
  # Calculate returns for each stock
  for (i in seq_len(length(info))) {
    info[[i]]$return <- returns((info[[i]])$price)
  }
  
  info
}


# Adds stocks' correct trade decision for each day
# Argument stocks_info: list of stocks, where each component has at least a price variable
# Value: stocks_info, but with each stock's data frame component of the list having an added decision variable 
attachDecision <- function(stocks_info) {
  for (i in seq_len(length(stocks_info))) {
    stocks_info[[i]]$decision <- NA
    
    for (j in seq_len(nrow(stocks_info[[i]]) - 1)) {
      # If tomorrow's price is lower than today's price, sell today
      if (stocks_info[[i]]$price[j] > stocks_info[[i]]$price[j + 1]) {
        stocks_info[[i]]$decision[j] <- "sell"
      } else { # If tomorrow's price if higher or equal to today's price, buy today (or neutral)
        if (stocks_info[[i]]$price[j] < stocks_info[[i]]$price[j + 1]) {
          stocks_info[[i]]$decision[j] <- "buy"
        } else { # If tomorrow's price is equal to today's price, trade decision is neutral
          stocks_info[[i]]$decision[j] <- "neutral"
        }
      }
    }
    
    stocks_info[[i]]$decision[nrow(stocks_info[[i]])] <- "neutral"
    stocks_info[[i]]$decision <- as.factor(stocks_info[[i]]$decision)
  }
  stocks_info
}


# Adds stocks' beta
# Argument stocks_info: list of stocks as outputted by getStockInfo, where each component is a data frame with date, price, and return
# Argument mkt_info: list as outputted by getStockInfo, with the first component representing market information (e.g. S&P 500 index). The data frame of this first component must have the same number of rows as those in stocks_info
# Value: `stock_info`, but with each stock's data frame component of the list having an added beta variable
attachBeta <- function(stocks_info, mkt_info) {
  # Loop over each stock in stocks_info
  for (i in seq_len(length(stocks_info))) {
    stocks_info[[i]]$beta <- NA
    
    N <- nrow(stocks_info[[i]])
    # Loop over each row in the stock's data frame
    for (j in seq_len(N)) {
      if (j <= 350) {
        calc_index <- seq_len(j)
      } else {
        calc_index <- (j - 349):j
      } # index of returns to use to calculate beta
      stocks_info[[i]]$beta[j] <- ifelse(j <= 350,
                                         cov(stocks_info[[i]]$return[calc_index][-1], mkt_info[[1]]$return[calc_index][-1]) /
                                           var(mkt_info[[1]]$return[calc_index], na.rm = TRUE),
                                         cov(stocks_info[[i]]$return[calc_index], mkt_info[[1]]$return[calc_index]) /
                                           var(mkt_info[[1]]$return[calc_index], na.rm = TRUE)
      )
    }
  }
  stocks_info
}


# Adds stocks' CAPM rate
# Argument stocks_info: list of stocks as outputted by attachBeta, where each component is a data frame with date, price, return, and beta
# Argument mkt_info: list with first component representing market information (e.g. S&P 500 index), second component representing risk-free asset info
# Value: stock_info, but with each stock's data frame component of the list having an added CAPM.rate variable
attachCAPMRate <- function(stocks_info, mkt_info) {
  for (i in seq_len(length(stocks_info))) {
    stocks_info[[i]]$CAPM.rate <- mkt_info[[2]]$price / 100 + stocks_info[[i]]$beta * (mkt_info[[1]]$return - mkt_info[[2]]$price / 100)
  }
  stocks_info
}


# Adds stocks' Jensen's alpha
# Argument stocks_info: list of stocks as outputted by attachCAPMRate, where each component is a data frame with date, price, return, decision, beta, and CAPM rate
# Value: stocks_info, but with each stock's data frame component of the list having an added (Jensen's) alpha variable
attachAlpha <- function(stocks_info) {
  for (i in seq_len(length(stocks_info))) {
    stocks_info[[i]]$alpha <- stocks_info[[i]]$return - stocks_info[[i]]$CAPM.rate
  }
  stocks_info
}


# Adds stocks' dividends
# Argument stocks_info: list of stocks as outputted by attachAlpha, where each component is a data frame
# Value: stocks_info, but with each stock's data frame component of the list having an added dividend variable
attachDividends <- function(stocks_info) {
  for (i in seq_len(length(stocks_info))) {
    dividends <- as.data.frame(getDividends(names(stocks_info)[i], from = startDate)) # ISSUE WITH THIS LINE FOR AMZN
    stocks_info[[i]]$dividend <- 0
    
    # Loops through each day of dividend payment
    for (j in seq_len(nrow(dividends))) {
      divDay <- rownames(dividends)[j]
      
      # If dividend payment date exists already in the stock's data frame, add the dividend info
      if (any(divDay == stocks_info[[i]]$date)) {
        stocks_info[[i]]$dividend[stocks_info[[i]]$date == divDay] <- dividends[j, ]
      } else { # If dividend payment date does not already exist in the stock's data frame, RETURN ERROR
        stop("Dividend date not found in stock's data frame, nn_train.R: attachDividends")
      }
    }
  }
  stocks_info
}


# Calculates expected dividend in 1 year
# Argument stocks_info: list of stocks as outputted by attachDividends, where each component is a data frame
# Value: stocks_info, but with each stock's data frame component of the list having an added exp_div_1yr variable
calculateExpDiv1Yr <- function(stocks_info) {
  for (i in seq_len(length(stocks_info))) {
    stocks_info[[i]]$exp_div_1yr <- 0
    
    # Temporarily change the date variable from character to Date class
    stocks_info[[i]]$date <- as.Date(stocks_info[[i]]$date)
    
    # Loops through each date and calculates the expected dividend payment within 1 year of that date
    for (j in seq_len(nrow(stocks_info[[i]]))) {
      current_date <- stocks_info[[i]]$date[j]
      first_date_to_add <- current_date - 365
      
      # Determines the index of the first date to be included in the sum
      first_idx <- which(first_date_to_add <= stocks_info[[i]]$date)[1]
      
      # Add dividend payments from first_idx to now
      stocks_info[[i]]$exp_div_1yr[j] <- sum(stocks_info[[i]]$dividend[first_idx:j])
    }
    
    # Change date variable back to character
    stocks_info[[i]]$date <- as.character(stocks_info[[i]]$date)
  }
  stocks_info
}


# Calculates annual dividend growth rate
# Argument stocks_info: list of stocks as outputted by calculateExpDiv1Yr, where each component is a data frame
# Value: stocks_info, but with each stock's data frame component of the list having an added div_growth variable
attachDividendGrowth <- function(stocks_info) {
  for (i in seq_len(length(stocks_info))) {
    stocks_info[[i]]$div_growth <- NA
    
    # Temporarily change the date variable from character to Date class
    stocks_info[[i]]$date <- as.Date(stocks_info[[i]]$date)
    
    # Loops through each date and calculates the dividend growth rate from one year ago to that date
    for (j in seq_len(nrow(stocks_info[[i]]))) {
      current_date <- stocks_info[[i]]$date[j]
      prev_year <- current_date - 365
      dates_before_prev_year <- which(prev_year >= stocks_info[[i]]$date)
      prev_year_idx <- dates_before_prev_year[length(dates_before_prev_year)]
      
      # It may be that prev_year_idx is 0, which happens if there was no date available in data before prev_year
      # In that case, leave growth rate as NA
      # But if prev_year_idx has an actual index, calculate growth rate as follows
      if (length(prev_year_idx) != 0) {
        exp_div_now <- stocks_info[[i]]$exp_div_1yr[j]
        exp_div_prev_year <- stocks_info[[i]]$exp_div_1yr[prev_year_idx]
        stocks_info[[i]]$div_growth[j] <- (exp_div_now - exp_div_prev_year) / exp_div_prev_year
      }
    }
    
    # Change date variable back to character
    stocks_info[[i]]$date <- as.character(stocks_info[[i]]$date)
  }
  stocks_info
}


# Calculates DDM price
# Argument stocks_info: list of stocks as outputted by attachDividendGrowth, where each component is a data frame
# Value: stocks_info, but with each stock's data frame component of the list having an added DDM_price variable
attachDDM <- function(stocks_info) {
  for (i in seq_len(length(stocks_info))) {
    # DDM formula
    stocks_info[[i]]$DDM_price <- stocks_info[[i]]$exp_div_1yr /
      (stocks_info[[i]]$CAPM.rate - stocks_info[[i]]$div_growth)
  }
  stocks_info
}


# Removes rows where any of the variables NN_vars have NA values
# To see what NN_vars are, scroll up to global variables definition
# Argument stocks_info: list of stocks as intended to train neural network, where each component is a data frame
# Value: stocks_info, but with each stock's data frame component of the list having certain rows removed.
# The rows removed are ones with NA in at least one of the NN_vars variables.
removeNA <- function(stocks_info) {
  for (i in seq_len(length(stocks_info))) {
    keep <- rep(TRUE, nrow(stocks_info[[i]]))
    
    # Loops through each variable in NN_vars to get rid of rows with NA
    for (var in NN_vars) {
      var_idx <- which(colnames(stocks_info[[i]]) == var)
      keep <- keep & (!is.na(stocks_info[[i]][, var_idx]))
    }
    
    stocks_info[[i]] <- stocks_info[[i]][keep, ]
  }
  stocks_info
}


#-------------------------------------------------------------------------------------------------
# PART 2: PREPARING STOCK INFORMATION
mkt <- c("^GSPC", "^TNX") # S&P 500 and 10-year T-bill
stocks <- c("AAPL", "MAIN") # Stocks
assets <- c(stocks, mkt)
assets_info <- getStockInfo(assets)

mkt_info <- assets_info[mkt] # List for S&P 500 and 10-year T-bill
info <- assets_info[stocks] # List for stocks

# Get the correct buy/sell decision for each stock on each day
info <- attachDecision(info)

# Valuation Method 1: Jensen's Alpha
# Calculate beta over time for each stock
info <- attachBeta(info, mkt_info)
# Calculate CAPM rate over time for each stock
info <- attachCAPMRate(info, mkt_info)
# Calculate Jensen's alpha by return minus CAPM rate
info <- attachAlpha(info)

# Valuation Method 2: Dividend Discount Model
# Retrieve dividends
info <- attachDividends(info)
# Calculate expected dividend in 1 year
info <- calculateExpDiv1Yr(info)
# Calculate annual dividend growth rate
info <- attachDividendGrowth(info)
# Calculate DDM price
info <- attachDDM(info)


#-------------------------------------------------------------------------------------------------
# PART 3: PRE-TRAINING ADJUSTMENTS

# Remove rows which have NA as the value for an input variable of the neural network
info <- removeNA(info)


#-------------------------------------------------------------------------------------------------
# PART 4: TRAINING NEURAL NETWORK
library(nnet)
library(NeuralNetTools)
networks <- list()
# Train a separate neural network for each stock
for (i in seq_len(length(info))) {
  networks[[i]] <- nnet(decision ~ alpha + DDM_price, size = 3, data = info[[i]])
}
names(networks) <- names(info)
