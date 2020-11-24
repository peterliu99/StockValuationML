# nn_predict.R
# Uses the neural network already trained to predict price for new data (new dates)
source("nn_train.R")

#-------------------------------------------------------------------------------------------------
# PART 1: FUNCTIONS

# Predicts a trade decision based on previously trained neural network and new information provided
# Argument networks: list of neural networks corresponding to each stock, which is used to predict
# Argument stocks_info_predict: list of stocks, where each component is a data frame with targeted
# dates and corresponding information to use to predict
# Value: 
makePredictions <- function(networks, stocks_info_predict) {
  predictions <- list()
  decisions <- c("buy", "neutral", "sell")
  
  for (i in seq_len(length(networks))) {
    predictions[[i]] <- predict(networks[[i]], stocks_info_predict[[i]])
    colnames(predictions[[i]]) <- c("prob_buy", "prob_neutral", "prob_sell")
    prob_max <- decisions[max.col(predictions[[i]])]
    predictions[[i]] <- data.frame(date = stocks_info_predict[[i]]$date, predictions[[i]],
                                   stringsAsFactors = FALSE)
    predictions[[i]]$pred_decision <- prob_max
  }
  
  names(predictions) <- names(networks)
  predictions
}

#-------------------------------------------------------------------------------------------------
# PART 2: PREDICTING USING NEURAL NETWORK

# Obtain trade decision predictions
predictions <- makePredictions(networks, info)







results <- list()
for (i in seq_len(length(predictions))) {
  results[[i]] <- data.frame(predicted = colnames(predictions[[i]])[max.col(predictions[[i]])],
                             actual = stocks_info_predict[[i]]$decision)
}
names(results) <- names(predictions)




# Turn each column in results from factor to character
for (i in seq_len(length(results))) {
  for (j in ncol(results[[i]])) {
    results[[i]][, j] <- as.character(results[[i]][, j])
  }
}
