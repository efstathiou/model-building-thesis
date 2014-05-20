####################################################################################################################
##
## Random Forests
##

library(foreach)
library(e1071)
library(languageR)
library(party)
library(randomForest)
library(MASS)

#
# Calculate the Predictive Performance of RF given the optimal set of parameters found in the previous step.
#
# INPUT:
# data_train : The training dataset
# data_valid : The validation dataset
# responses : The QoS metrics/response variables of interest
# predictors: The predictor variables of interest
#
# RETURNS:
# best_parameters_valid : The optimal parameters used for the model
# best_formula_valid : The optimal formula (predictor variables) of the model
#
findRFParameters <- function(data_train, data_valid, responses, predictors) {
        
        #
        # Value space of the two parameters of the Random Forests model
        #
        
        # ntree = overall number of trees in the forest
        trees <- c(500, 750, 1000, 2000, 3000)
        
        # mtry = number of randomly preselected predictor variables for each split
        try <- seq(1, length(predictors))
        
        # Create all the possible combinations of pairs (ntree, mtry)
        pairs <- expand.grid(trees, try)
        
        # Store the parameters of the best model for each QoS
        best_parameters_valid <- c()
        best_formula_valid <- c()
        
        # Create all the possible combinations for the column number of the 7 independent variables
        xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors, i, simplify=FALSE) }
        
        # For each response variable (QoS metrics) of interest
        for(j in 1:length(responses)) {
                y = data_train[, responses[j]]
                
                # Store the minimum error of each possible model (combination of predictor variables)
                min_valid_error <- c()
                parameters_valid <- c()
                
                # Find the optimal complexity (# of predictor variables) of the model by trying all the possible combination of predictor variables 
                for(k in 1:length(xcomb)) { 
                        # Create a x frame with only the considered predictor variables
                        x = data_train[c(unlist(xcomb[k]))] # Crop the columns
                        
                        valid_error_vector <- c() # Temp vector for storing the validation error of the candidate model
                        
                        # Given a model complexity, find the best parameter combination (ntree, mtry) of the model
                        for(i in 1:dim(pairs)[1]) {
                                cat("\n############## Random Forest with id = ", i, " and parameters (", pairs[i,1], ", ", pairs[i,2], ")\n", sep = "") 
                                # Build the model by using the parameters: (pairs[i,1], pairs[i,2])
                                rf <- randomForest(x, y, ntree = pairs[i,1], mtry = pairs[i,2], importance=TRUE)
                                # Cross Validation error
                                rmse_cv <-  cv(data_valid, 10, 4, formula(formlist[[k]]), xcomb[k], pairs[i,], responses[j])[1]
                                valid_error_vector <- c(valid_error_vector, rmse_cv)
                        }
                        
                        # Find the model with the lowest validation error
                        best_index = which(valid_error_vector == min(valid_error_vector), arr.ind = TRUE)
                        # Retrieve and store the parameters of the best model
                        parameters_valid <- c(parameters_valid, pairs[best_index,])
                        min_valid_error <- c(min_valid_error, min(valid_error_vector))
                        cat("Best model based validation error = ", best_index, " with error ", min(valid_error_vector), " and parameters ", unlist(parameters_valid), "\n")
                }
                
                # Find the model with the lowest validation error
                best_index_valid = which(min_valid_error == min(min_valid_error), arr.ind = TRUE)
                best_parameters_valid <- c(best_parameters_valid, c(parameters_valid[best_index_valid], parameters_valid[best_index_valid + 1]))
                
                cat("RF Optimal # of Predictors: Best model based validation error = ", which(min_valid_error == min(min_valid_error), arr.ind = TRUE), " with error ", min(min_valid_error), " and parameters ", unlist(best_parameters_valid), "\n")
                
                best_formula_valid <- c(best_formula_valid, best_index_valid)
        }
        
        list(result1 = best_parameters_valid, result2 = best_formula_valid)
}

# Function that calculate the Rsq, RMSE, MAE, and MAPE errors for a given dataset and its residuals.
calculate_errors <- function(data_train, data_test, residuals, rf, response){
        c(rf$rsq[length(rf$rsq)], rmse(residuals), mae(residuals), mape(residuals, data_test[,response]))
}  

#
# Calculate the Predictive Performance of RFs given the optimal set of parameters found in the previous step.
#
# INPUT:
# data_train : The training dataset
# data_valid : The validation dataset
# data_test : The testing dataset
# parameters : The optimal parameters of the RFs found in the previous step
# best_formula : The optimal formulas used for each QoS
# responses : The QoS metrics/response variables of interest
# CV : (1) True when use the validation approach / (0) False when we use the three sets splitting approach
#
calculateRFperformance <- function(data_train, data_valid, data_test, parameters, best_formula, responses, CV){
        
        # Rows represent QoS properties and columns quadrotuples of (RSq, RMSE, MAE, MAPE)
        # for each of the training, validation, and testing datasets.
        errors <- matrix(data = NA, nrow = 4, ncol = 12) 
        
        build_time = vector(length=length(responses))
        predict_time = vector(length=length(responses))
        
        xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors, i, simplify=FALSE) }
        
        
        # Calculate the out-of-bag error for each QoS of interest
        for(j in 1:length(responses)) {
                optimal_ntree = as.numeric(parameters[j, 1])
                optimal_mtry = as.numeric(parameters[j, 2])
                
                if ( CV == 1 ) {
                        rf <- randomForest(data_train[, min(predictors):max(predictors)], data_train[,responses[j]], ntree = optimal_ntree, mtry = optimal_mtry, importance=TRUE)
                        
                        ## Cross Validation error
                        all_errors <- cv(rbind(data_train, data_valid), 10, 4, formula(formlist[[best_formula[j]]]), xcomb[best_formula[j]], parameters[j,], responses[j])
                        best_index_rmse_cv = which(all_errors[1] == min(all_errors[1]), arr.ind = TRUE)
                        #cat("CART Model with best CV RMSE = ", min(all_errors[1]), " and formula = \n", as.character(formlist[best_index_rmse_cv]), "\n\n")
                        errors[j, 1:4] = c(all_errors[2], all_errors[1], all_errors[4], all_errors[5])	
                }
                else {
                        ## Training Error 
                        rf <- randomForest(data_train[, min(predictors):max(predictors)], data_train[,responses[j]], ntree = optimal_ntree, mtry = optimal_mtry, importance=TRUE)
                        rf_pred <- predict(rf, newdata = data_train)            # Evaluate model on the training data
                        residuals = data_train[,responses[j]] - rf_pred         # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 1:4] = calculate_errors(data_train, data_train, residuals, rf, responses[j])
                        
                        ## Validation Error 
                        rf <- randomForest(data_train[, min(predictors):max(predictors)], data_train[,responses[j]], ntree = optimal_ntree, mtry = optimal_mtry, importance=TRUE)
                        rf_pred <- predict(rf, newdata = data_valid)            # Evaluate model on the validation data
                        residuals = data_valid[,responses[j]] - rf_pred         # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 5:8] = calculate_errors(data_valid, data_valid, residuals, rf, responses[j])
                        
                        ## Test Error 
                        build_time[j] <- system.time(rf <- randomForest(rbind(data_train, data_valid)[,min(predictors):max(predictors)], rbind(data_train, data_valid)[,responses[j]], ntree = optimal_ntree, mtry = optimal_mtry, importance=TRUE))[3]
                        predict_time[j] <- system.time(rf_pred <- predict(rf, newdata = data_test))[3]       # Evaluate model on the test data
                        residuals = data_test[,responses[j]] - rf_pred          # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 9:12] = calculate_errors(rbind(data_train, data_valid), data_test, residuals, rf, responses[j])
                }
        }
        
        errors <- signif(errors, digits=2)
        
        # Print the in latex format
        print("\n\nAccuracy Metrics (Latex Table Format): \n")
        
        for(i in 1:length(responses)) {
                string <- c()
                for(j in 1:12) {
                        string <- c(string, errors[i, j], " & ")
                }
                cat(string, "\n")
        }
        
        # Print the in latex format
        print("\n\nEfficiency Metrics (Latex Table Format): \n")
        for(j in 1:length(responses)) {
                cat(build_time[j], " & ", predict_time[j], "\n", sep="")
        }	
}

##
## End of Random Forests
##
####################################################################################################################
