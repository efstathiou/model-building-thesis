####################################################################################################################
##
## Multivariate Adaptive Regression Splines (MARS)
##

library(foreach)
library(leaps)
library(earth)
library(DAAG)
library(e1071)

#
# Calculate the Predictive Performance of LR given the optimal set of parameters found in the previous step.
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
findMARSParameters <- function(data_train, data_valid, responses, predictors) {
        
        #
        # Value space of the two parameters of the MARS model
        #
        
        # nk = Maximum number of model terms before pruning
        nk <- seq(15, 25)
        
        # thresh = Forward stepping threshold.  Default is 0.001.
        thresh <- seq(0.001, 0.005, by = 0.001)
        
        # nprune = Maximum number of terms (including intercept) in the pruned model
        nprune <- seq(15, 25)
        
        # Create all the possible combinations of pairs (nk, thresh, nprune)
        pairs <- expand.grid(nk, thresh, nprune)
        
        # Store the parameters of the best model for each QoS
        best_parameters_valid <- c()
        best_formula_valid <- c()
        
        # Create all the possible combinations for the column number of the 7 independent variables	
        xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors, i, simplify=FALSE) }
        xcomb2 <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors_names, i, simplify=FALSE) }
        
        # For each response variable (QoS metrics) of interest
        for(j in 1:length(responses)) {
                y = data_train[, responses[j]]
                
                # Store the minimum error of each possible model (combination of predictor variables)
                min_valid_error <- c()
                parameters_valid <- c()
                
                formlist <- lapply(xcomb2, function(l) formula(paste(responses_names[j], paste(l, collapse="+"), sep="~")))
                
                # Find the optimal complexity (# of predictor variables) of the model by trying all the possible combination of predictor variables 
                for(k in 1:length(xcomb)) { 
                        # Create a x frame with only the considered predictor variables
                        x = data_train[c(unlist(xcomb[k]))] # Crop the columns
                        
                        valid_error_vector <- c() # Temp vector for storing the validation error of the candidate model
                        
                        # Given a model complexity, find the best parameter combination (nk, thresh, nprune) of the model                        
                        for(i in 1:dim(pairs)[1]) {
                                cat("\n############## MARS with id = ", i, " and parameters (", pairs[i,1], ", ", pairs[i,2], ")", ", ", pairs[i,3], ")\n", sep = "") 
                                # Build the model by using the parameters: (pairs[i,1], pairs[i,2], pairs[i,3])
                                mars = earth(formula(formlist[[k]]), data = data_train, nk = pairs[i,1], thresh = pairs[i,2], nprune = pairs[i,3])
                                # Cross Validation error
                                rmse_cv <- cv(data_valid, 10, 2, formula(formlist[[k]]), NULL, pairs[i,], responses[j])[1]
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
                best_parameters_valid <- c(best_parameters_valid, c(parameters_valid[best_index_valid], parameters_valid[best_index_valid + 1], parameters_valid[best_index_valid + 2]))
                
                cat("MARS Optimal # of Predictors: Best model based validation error = ", best_index_valid , " with parameters ", unlist(best_parameters_valid), "\n")
                
                best_formula_valid <- c(best_formula_valid, best_index_valid)
                
                print("Best Formulas: \n", best_formula_valid, xcomb2[best_formula_valid])
        }
        
        list(result1 = best_parameters_valid, result2 = best_formula_valid)
}

# Function that calculate the Rsq, RMSE, MAE, and MAPE errors for a given dataset and its residuals.
calculate_errors <- function(data_train, data_test, residuals, formula, response){
        c(summary(lm(formula, data = data_train))$r.squared, rmse(residuals), mae(residuals), mape(residuals, data_test[,response]))
}  

#
# Calculate the Predictive Performance of MARS given the optimal set of parameters found in the previous step.
#
# INPUT:
# data_train : The training dataset
# data_valid : The validation dataset
# data_test : The testing dataset
# parameters : The optimal parameters of the RFs found in the previous step
# best_formula: The optimal formulas used for each QoS
# responses : The QoS metrics/response variables of interest
# predictors : The predictor variables of interest
# CV : (1) True when use the validation approach / (0) False when we use the three sets splitting approach
#
calculateMARSperformance <- function(data_train, data_valid, data_test, parameters, best_formula, responses, predictors, CV){
        
        # Rows represent QoS properties and columns quadrotuples of (RSq, RMSE, MAE, MAPE)
        # for each of the training, validation, and testing datasets.
        errors <- matrix(data = NA, nrow = 4, ncol = 12) 
        
        build_time = vector(length=length(responses))
        predict_time = vector(length=length(responses))
        
        xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors_names, i, simplify=FALSE) }
        
        # Calculate the out-of-bag error for each QoS of interest
        for(j in 1:length(responses)) {
                formlist <- lapply(xcomb, function(l) formula(paste(responses_names[j], paste(l, collapse="+"), sep="~")))	
                
                optimal_nk = as.numeric(parameters[j,1])
                optimal_thresh = as.numeric(parameters[j,2])
                optimal_nprune = as.numeric(parameters[j,3])
                
                if ( CV == 1 ) {
                        mars = earth(formula(formlist[[best_formula[j]]]), data = data_train, nk = optimal_nk, thresh = optimal_thresh, nprune = optimal_nprune)
                        
                        ## Cross Validation error
                        all_errors <- cv(rbind(data_train, data_valid), 10, 2, formula(formlist[[best_formula[j]]]), NULL, parameters[j,], responses[j])
                        best_index_rmse_cv = which(all_errors[1] == min(all_errors[1]), arr.ind = TRUE)
                        #cat("CART Model with best CV RMSE = ", min(all_errors[1]), " and formula = \n", as.character(formlist[best_index_rmse_cv]), "\n\n")
                        errors[j, 1:4] = c(all_errors[2], all_errors[1], all_errors[4], all_errors[5])	
                }
                else {
                        ## Training Error 
                        mars <- earth(formula(formlist[[best_formula[j]]]), data = data_train, nk = optimal_nk, thresh = optimal_thresh, nprune = optimal_nprune)
                        mars_pred <- predict(mars, newdata = data_train)        # Evaluate model on the training data
                        residuals = data_train[,responses[j]] - mars_pred       # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 1:4] = calculate_errors(data_train, data_train, residuals, formlist[[best_formula[j]]], responses[j])
                        
                        ## Validation Error 
                        mars <- earth(formula(formlist[[best_formula[j]]]), data = data_train, nk = optimal_nk, thresh = optimal_thresh, nprune = optimal_nprune)
                        mars_pred <- predict(mars, newdata = data_valid)        # Evaluate model on the validation data
                        residuals = data_valid[,responses[j]] - mars_pred       # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 5:8] = calculate_errors(data_valid, data_valid, residuals, formlist[[best_formula[j]]], responses[j])
                        
                        ## Test Error 
                        build_time[j] <- system.time(mars <- earth(formula(formlist[[best_formula[j]]]), data = rbind(data_train, data_valid), nk = optimal_nk, thresh = optimal_thresh, nprune = optimal_nprune))[3]
                        predict_time[j] <- system.time(mars_pred <- predict(mars, newdata = data_test))[3] # Evaluate model on the testing data
                        residuals = data_test[,responses[j]] - mars_pred                      # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 9:12] = calculate_errors(rbind(data_train, data_valid), data_test, residuals, formlist[[best_formula[j]]], responses[j])
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
## End of Multivariate Adaptive Regression Splines (MARS)
##
####################################################################################################################