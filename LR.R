####################################################################################################################
##
## Linear Regression (LR)
##

library(foreach)
library(leaps)

#
# Calculate the Predictive Performance of LR given the optimal set of parameters found in the previous step.
#
# INPUT:
# data_train : The dataset which contains the Training and Validation sets
# data_valid : The validation dataset
# responses : The QoS metrics/response variables of interest
# predictors: The predictor variables of interest
#
# RETURNS:
# best_formula_valid : The optimal formula (predictor variables) of the model
#
findLRParameters <- function(data_train, data_valid, responses, predictors) {
        
        xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors_names, i, simplify=FALSE) }
        
        # Store the id of the best models for each response variable of interest
        best_models <- c()
        
        # Create a vector where each position is dedicated to store the error of the model 
        temp <- rep(0, length(xcomb))
        
        # For each response variable (QoS metrics) of interest
        for(j in 1:length(responses)) {
                # Repeat the evaluation many times for statistical purposes
                for(i in 1:repetitions) {
                        cat("################ RV = ", responses_names[j], " - Rep = ", i, " ################## \n")
                        
                        formlist <- lapply(xcomb, function(l) formula(paste(responses_names[j], paste(l, collapse="+"), sep="~")))	
                        
                        # Training Metrics
                        # R squared
                        ## models.rsq <- sapply(formlist, function(j) summary(lm(j, data = data_train))$r.squared)
                        ## best_index_rsq = which(models.rsq == max(models.rsq), arr.ind = TRUE)
                        ## cat("Model with best R-squared = \n", as.character(formlist[best_index_rsq]), "\n\n")
                        
                        # MSE
                        ## models.mse <- sapply(formlist, function(j) mse(lm(j, data = data_train)$residuals))
                        ## best_index_mse = which(models.mse == min(models.mse), arr.ind = TRUE)
                        ## cat("Model with best MSE = \n", as.character(formlist[best_index_mse]), "\n\n")
                        
                        # RMSE
                        models.rmse <- sapply(formlist, function(j) rmse(lm(j, data = data_train)$residuals))
                        temp <- temp + models.rmse
                        # Select the model with the best RMSE. Choose randomly if there are many equivalent.
                        ## best_index_rmse = sample(which(models.rmse == min(models.rmse), arr.ind = TRUE), 1)
                        ## cat("Model with best RMSE = \n", as.character(formlist[best_index_rmse]), "\n\n")
                        
                        # MAE
                        ## models.mae <- sapply(formlist, function(j) mae(lm(j, data = data_train)$residuals))
                        ## best_index_mae = which(models.mae == min(models.mae), arr.ind = TRUE)
                        ## cat("Model with best MAE = \n", as.character(formlist[best_index_mae]), "\n\n")
                        
                        # Mallow's Cp
                        ## models.Cp <- sapply(formlist, function(j) {
                        ##         SSEp <- anova(lm(j, data = data_train))['Sum Sq']['Residuals',]
                        ##         mod.mat <- model.matrix(lm(j, data = data_train))
                        ##         n <- dim(mod.mat)[1]
                        ##         p <- dim(mod.mat)[2]
                        ##         c(p,SSEp / models.mse - (n - 2*p))
                        ## })
                        
                        # Cross Validation error
                        #models.all_errors <- sapply(formlist, function(i) cv(rbind(data_train, data_valid), 10, 1, i, NULL, NULL, responses[j]))
                        #temp <- temp + models.all_errors[1,]
                }
                
                # Select the model with the best metric. Choose randomly if there are many equivalent.
                best_model =  sample(which(temp == min(temp), arr.ind = TRUE), 1)
                cat("Model with best CV RMSE = \n", as.character(formlist[best_model]), "\n\n")
                best_models <- c(best_models, best_model)
        }
        
        list(result1 = best_models)
}

# Function that calculate the Rsq, RMSE, MAE, and MAPE errors for a given dataset and its residuals.
calculate_errors <- function(data_train, data_test, residuals, formula, response){
        c(summary(lm(formula, data = data_train))$r.squared, rmse(residuals), mae(residuals), mape(residuals, data_test[,response]))
}  

#
# Calculate the Predictive Performance of LR given the optimal set of parameters found in the previous step.
#
# INPUT:
# data_train : The training dataset
# data_valid : The validation dataset
# data_test : The testing dataset
# parameters : The optimal parameters of the RFs found in the previous step
# best_formula : The optimal formulas used for each QoS
# responses : The QoS metrics/response variables of interest
# predictors : The predictor variables of interest
# CV : (1) True when use the validation approach / (0) False when we use the three sets splitting approach
#
calculateLRperformance <- function(data_train, data_valid, data_test, parameters, best_formula, responses, predictors, CV){
        
        # Rows represent QoS properties and columns quadrotuples of (RSq, RMSE, MAE, MAPE)
        # for each of the training, validation, and testing datasets.
        errors <- matrix(data = NA, nrow = 4, ncol = 12) 
        
        build_time = vector(length=length(responses))
        predict_time = vector(length=length(responses))
        
        xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors_names, i, simplify=FALSE) }
        
        # Calculate the out-of-bag error for each QoS of interest
        for(j in 1:length(responses)) {
                formlist <- lapply(xcomb, function(l) formula(paste(responses_names[j], paste(l, collapse="+"), sep="~")))	
                
                if ( CV == 1 ) {
                        # Build the model by using the optimal parameters
                        models.all_errors <- sapply(formlist, function(i) cv(rbind(data_train, data_valid), 10, 1, i, NULL, NULL, responses[j]))
                        
                        errors[j, 1:4] = c( sample(models.all_errors[2, best_formula], 1), sample(models.all_errors[1, best_formula], 1),
                                            sample(models.all_errors[4, best_formula], 1), sample(models.all_errors[5, best_formula], 1))
                }
                else {
                        ## Training Error 
                        lr <- lm(formlist[[best_formula[j]]], data = data_train)        # Build model
                        lr_pred <- predict(lr, newdata = data_train)                    # Evaluate model on the training data
                        residuals = data_train[,responses[j]] - lr_pred                 # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 1:4] = calculate_errors(data_train, data_train, residuals, formlist[[best_formula[j]]], responses[j])
                        
                        ## Validation Error 
                        lr <- lm(formlist[[best_formula[j]]], data = data_train)        # Build model
                        lr_pred <- predict(lr, newdata = data_valid)                    # Evaluate model on the validation data
                        residuals = data_valid[,responses[j]] - lr_pred                 # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 5:8] = calculate_errors(data_valid, data_valid, residuals, formlist[[best_formula[j]]], responses[j])
                        
                        ## Test Error 
                        build_time[j] <- system.time(lr <- lm(formlist[[best_formula[j]]], data = rbind(data_train, data_valid)))[3] # Build model
                        predict_time[j] <- system.time(lr_pred <- predict(lr, newdata = data_test))[3] # Evaluate model on the testing data                              
                        residuals = data_test[,responses[j]] - lr_pred                  # Calculate residuals
                        # Calculate Rsq, RMSE, MAE, and MAPE
                        errors[j, 9:12] = c(summary(lm(formlist[[best_formula[j]]], data = rbind(data_train, data_valid)))$r.squared, rmse(residuals), mae(residuals), mape(residuals, data_test[,responses[j]]))
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
## End of Linear Regression (LR)
##
####################################################################################################################