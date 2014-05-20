# Function that returns Mean Squared Error
mse <- function(error)
{
        mean(error^2)
}

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
        sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
        mean(abs(error))
}

# Function that returns Mean Absolute Percent Error
mape <- function(error, y)
{
        mean(abs(error / y))
}

# Cross Validation function
# type - is the type of the regression model: 
# 	2 - MARS
# 	3 - CART
# 	4 - RF
cv <- function(data, nfolds, type, formula, predictors, parameters, response) {
        # Number of instances
        rows <- nrow(data)
        
        # Matrix to store predictions
        rsq.cv <- matrix(NA, nfolds)
        mse.cv <- matrix(NA, nfolds)
        rmse.cv <- matrix(NA, nfolds)
        mae.cv <- matrix(NA, nfolds)
        mape.cv <- matrix(NA, nfolds)
        
        # Prepare the folds
        folds <- rep(1:nfolds, ceiling(rows/nfolds))[1:rows]
        folds <- sample(folds)
        
        # Cross-validation
        for (i in seq(nfolds)) {
                # Fit model to the training set
                if (type == 1) { # LR 
                        fit <- lm(formula, data = data[folds!=i,])
                        rsq.cv[i,] <- summary(lm(formula, data = data[folds!=i,]))$r.squared
                }
                else if (type == 2) { # MARS
                        fit <- earth(formula, data = data[folds!=i,], nk = unlist(parameters)[1], thresh = unlist(parameters)[2], nprune = unlist(parameters)[3])
                        rsq.cv[i,] <- fit$rsq[length(fit$rsq)]
                }
                else if (type == 3) # CART
                {
                        y = data[folds!=i,response]
                        xx = data[folds!=i,] # Crop the rows
                        x = xx[c(unlist(predictors))] # Crop the columns
                        fit <- train(x, y, method = "rpart", control = rpart.control(cp = unlist(parameters)[2], minsplit = unlist(parameters)[1]), trControl = trainControl(method = "cv", number = 10))
                        rsq.cv[i,] <- fit$results$Rsquared[1]
                }
                else if (type == 4) # RF
                {
                        y = data[folds!=i,response]
                        xx = data[folds!=i,] # Crop the rows
                        x = xx[c(unlist(predictors))] # Crop the columns
                        fit <- randomForest(x, y, ntree = unlist(parameters)[1], mtry = unlist(parameters)[2], importance=TRUE)
                        rsq.cv[i,] <- fit$rsq[length(fit$rsq)]
                }
                
                # Predict the response on the validation set
                y_hat <- predict(fit, newdata = data[folds==i,])
                
                # Estimate the residuals
                residuals = data[folds==i, response] - y_hat
                mse.cv[i,] <- mse(residuals)
                rmse.cv[i,] <- rmse(residuals)
                mae.cv[i,] <- mae(residuals)
                mape.cv[i,] <- mape(residuals, data[folds==i, response])
        }
        
        # Calculate mean CV error
        rsq_mean_error <- mean(rsq.cv)
        rmse_mean_error <- mean(rmse.cv)
        mse_mean_error <- mean(mse.cv)
        mae_mean_error <- mean(mae.cv)
        mape_mean_error <- mean(mape.cv)
        
        return (c(rmse_mean_error, rsq_mean_error, mse_mean_error, mae_mean_error, mape_mean_error))
}

# MARS CV
# cv(data_total[1:100,], 10, 2, formula(formlist[[127]]), NULL, pairs[500,], responses[1])

# CART CV
# cv(data_total[1:100,], 10, 3, formula(formlist[[127]]), xcomb[k], pairs[10,], responses[1])

# RF CV
# cv(data_total[1:100,], 10, 4, formula(formlist[[127]]), xcomb[k], pairs[10,], responses[1])
