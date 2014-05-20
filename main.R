install.packages("devtools")
install.packages("codetools")
install.packages("foreach")
install.packages("leaps")
install.packages("earth")
install.packages("DAAG")
install.packages("class")
install.packages("e1071")
install.packages("cluster")
install.packages("lattice")
install.packages("caret")
install.packages("languageR")
install.packages("party")
install.packages("rpart")

##
## Read,transform, and split data
##

set.seed(1)

source(file="load_split_data.R")

file <- "datasets/trainingSet.csv" 
sample_size <- 970
display_graphs <- 0
graphs_path <- "images/Regression/"
repetitions <- 30                       # Repetitions of model testing 

# Response Variables
responses <- c(1, 2, 3, 4)
responses_names <- c("Delay2", "Latency2", "Success_Ratio", "Energy")
responses_names2 <- c("Response_Time", "Network_Latency", "Success_Ratio", "Energy")

# Explanatory Variables
predictors <- c(5, 6, 7, 8, 9, 10, 11, 12)
predictors_names <- c("Hops",  "Orchestrators", "DevFast", "DevMedium", "DevSlow", "LoadSmall", "LoadMedium", "LoadBig")

data <- load_data(file, sample_size)

data_train <- data$data_train
data_valid <- data$data_valid
data_test <- data$data_test

##
## Functions for calculating the performance prediction metrics
##

source(file="basic_functions.R")

####################################################################################################################
##
## Linear Regression (LR)
##

source(file="LR.R")

##
## Model Selection
##

results <- findLRParameters(data_train, data_valid, responses, predictors)

##
## Model Assessment
##

## 1st Choice
# Brute Force:
# Optimal Parameters found in a previous call:
best_formula <-  c(100,  34,  67, 105)

## 2nd Choice
# By observing the graphs RMSE vs. model complexity
best_formula <-  c(9,  9, 37, 30)

best_formula <-  c(255, 237, 234, 255)

calculateLRperformance(data_train, data_valid, data_test, NULL, best_formula, responses, predictors, 1)
calculateLRperformance(data_train, data_valid, data_test, NULL, best_formula, responses, predictors, 0)

# Print the id of predictor variables included in the developed models
xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors, i, simplify=FALSE) }
formlist <- lapply(xcomb, function(l) formula(paste(responses_names[1], paste(l, collapse="+"), sep="~")))
print(formlist[best_formula]) 

##
## End of Linear Regression (LR)
##
####################################################################################################################


####################################################################################################################
##
## Multivariate Adaptive Regression Splines (MARS)
##

source(file="MARS.R")

##
## Model Selection
##

results <- findMARSParameters(data_train, data_valid, responses, predictors)	

##
## Model Assessment
##

## 1st Choice
# Brute Force:
# Optimal Parameters found in a previous call:
best_parameters <- matrix( c(21, 20, 22, 25, 0.001, 0.002, 0.003, 0.001, 23, 25, 23, 25), nrow=4, ncol =3)
#best_parameters_valid <- matrix( c(21, 21, 16, 20, 0.002, 0.004,0.002, 0.001, 21, 25, 23, 16), nrow=4, ncol =3)
best_formula <- c(66, 66, 101, 69)

calculateMARSperformance(data_train, data_valid, data_test, best_parameters, best_formula, responses, predictors, 1)
calculateMARSperformance(data_train, data_valid, data_test, best_parameters, best_formula, responses, predictors, 0)

# Print the id of predictor variables included in the developed models
xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors, i, simplify=FALSE) }
formlist <- lapply(xcomb, function(l) formula(paste(responses_names[1], paste(l, collapse="+"), sep="~")))
print(formlist[best_formula]) 

##
## End of Multivariate Adaptive Regression Splines (MARS)
##
####################################################################################################################


####################################################################################################################
##
## Classification and Regression Trees (CART)
##

source(file="CART.R")

##
## Model Selection
##

results <- findCARTParameters(data_train, data_valid, responses, predictors)

# Print the id of predictor variables included in the developed models
xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors, i, simplify=FALSE) }
formlist <- lapply(xcomb, function(l) formula(paste(responses_names[1], paste(l, collapse="+"), sep="~")))
formulas <- results$result2
print(formlist[best_formula_valid]) 

##
## Model Assessment
##

## 1st Choice
# Brute Force:
# Optimal Parameters found in a previous call:
# best_parameters_valid <- matrix( c(50, 50, 100, 50, 0.005, 0.005, 0.015, 0.001), nrow=4, ncol =2)
best_parameters_valid <- matrix( c(50, 50, 50, 50, 0.005, 0.01, 0.01, 0.005), nrow=4, ncol =2)
best_formula_valid <- c(109, 125, 74, 54)

calculateCARTperformance(data_train, data_valid, data_test, best_parameters_valid, best_formula_valid, responses, predictors, 1)
calculateCARTperformance(data_train, data_valid, data_test, best_parameters_valid, best_formula_valid, responses, predictors, 0)

# Visualise CART
xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors_names, i, simplify=FALSE) }
formlist <- lapply(xcomb, function(l) formula(paste(responses_names[4], paste(l, collapse="+"), sep="~")))
library(tree)
tr = tree(formula(formlist[[54]]), data=data_train)
summary(tr)
plot(tr, main="Classification Tree for the Energy QoS Metric"); text(tr)
dev.copy2eps(file=paste(graphs_path,"/CART_example.eps", sep = ""))

##
## End of Classification and Regression Trees (CART)
##
####################################################################################################################


####################################################################################################################
##
## Random Forests
##

source(file="RF.R")

##
## Model Selection
##

results <- findRFParameters(data_train, data_test, responses, predictors)

# Print the id of predictor variables included in the developed models
xcomb <- foreach(i=1:length(predictors), .combine=c) %do% {combn(predictors, i, simplify=FALSE) }
formlist <- lapply(xcomb, function(l) formula(paste(responses_names[1], paste(l, collapse="+"), sep="~")))
formulas <- results$result2
print(formlist[formulas]) 

##
## Model Assessment
##

## 1st Choice
# Brute Force:
# Optimal Parameters found in a previous call:
# 2000    1  500    2  750    1 3000    1
best_parameters_valid <- matrix( c(750, 3000, 500, 2000, 7, 2, 2, 7), nrow = 4, ncol = 2)
best_formula_valid <- c(65, 65, 101, 120)

# Example call:
calculateRFperformance(data_train, data_valid, data_test, best_parameters_valid, best_formula, responses, 1)
calculateRFperformance(data_train, data_valid, data_test, best_parameters_valid, best_formula, responses, 0)

##
## End of Random Forests
##
####################################################################################################################





####################################################################################################################
##
## Comparing the performance of the various studied approximation models
## by considering different sample size of the available dataset
##

source(file="load_split_data.R")
source(file="basic_functions.R")
source(file="LR.R")
source(file="MARS.R")
source(file="CART.R")
source(file="RF.R")

sample_size <- c(500, 2000, 3000, 4000, 5000)

for(i in 1:length(samples)){	
        cat("################ Sample size = ", sample_size[i], " ################## \n")
        
        data <- load_data(sample_size[i], display_graphs, graphs_path)
        
        data_train <- data$data_train
        data_valid <- data$data_valid
        data_test <- data$data_test
        responses <- data$responses
        predictors <- data$predictors
        responses_names <- data$responses_names
        responses_names2 <- data$responses_names2
        predictors_names <- data$predictors_names
        
        cat("            ################ LR ################## \n")
        best_formula_valid <-  c(9,  9, 37, 30)
        results <- calculateLRperformance(data_train, data_valid, data_test, best_formula_valid, NULL, responses, predictors, 0)
        
        cat("            ################ MARS ################## \n")
        best_parameters_valid <- matrix( c(21, 20, 22, 25, 0.001, 0.002, 0.003, 0.001, 23, 25, 23, 25), nrow=4, ncol =3)
        best_formula_valid <- c(66, 66, 101, 69)
        calculateMARSperformance(data_train, data_valid, data_test, best_parameters_valid, best_formula_valid, responses, predictors, 0)
        
        cat("            ################ CART ################## \n")
        best_parameters_valid <- matrix( c(50, 50, 50, 50, 0.005, 0.01, 0.01, 0.005), nrow=4, ncol =2)
        best_formula_valid <- c(109, 125, 74, 54)
        calculateCARTperformance(data_train, data_valid, data_test, best_parameters_valid, best_formula_valid, responses, predictors, 0)
        
        cat("            ################ RF ################## \n")
        best_parameters_valid <- matrix( c(750, 3000, 500, 2000, 7, 2, 2, 7), nrow = 4, ncol = 2)
        best_formula_valid <- c(65, 65, 101, 120)
        calculateRFperformance(data_train, data_valid, data_test, best_parameters_valid, best_formula, responses, 0)
}
##
## 
##
####################################################################################################################
