##########################
#
# Bash commands
#

## Convert all generated .eps files to pdf
ls *.eps | xargs -n1 epstopdf

## Delete the generated .eps files
ls *.eps | xargs -n1 rm
##########################


####################################
#
# Plots for document
#

##
## Step 1: Read data and plot pairplots
##
	graphs_path <- "../images/Regression/Generated_Graphs"
	display_graphs <- 1

	data_train <- read.csv("scenario4-TrainingSet/allConfigurations.csv", sep="\t", header=T)
	cat("Initial size of training data = ", dim(data_train), "\n")	
	data_train <- data_train[sample(nrow(data_train), 300), ] # Randomly choose 1000 data points
	cat("Sampled size of training data = ", dim(data_train), "\n")

	data_test <- read.csv("scenario4/allConfigurations.csv", sep="\t", header=T)
	cat("Initial size of testing data = ", dim(data_test), "\n")	
	data_test <- data_test[sample(nrow(data_test), 200), ] # Randomly choose 1000 data points
	cat("Sampled size of testing data = ", dim(data_test), "\n")

	data_train$ID <- NULL			# Delete ID column
	data_train$ShortestPath <- NULL	# Delete ShortestPath column
	data_train$rxPackets <- NULL		# Delete rxPackets column

	data_test$ID <- NULL			# Delete ID column
	data_test$ShortestPath <- NULL	# Delete ShortestPath column
	data_test$rxPackets <- NULL		# Delete rxPackets column

	if ( display_graphs == 1) {
		plot(data_train)
		dev.new()
		plot(data_test)
	}

	# Response Variables: Delay	Delay2	Latency	Latency2	Success_Ratio	Energy
	responses <- c(2, 4, 5, 6)
	responses_names <- c("Delay2", "Latency2", "Success_Ratio", "Energy")
	responses_names2 <- c("Response_Time", "Network_Latency", "Success_Ratio", "Energy")
	
	# Explanatory Variables: Hops	txPackets		LongestPath	Orchestrators	Neighbors	Distance
	predictors <- c(7, 8, 9, 10, 11, 12, 13)
	predictors_names <- c("Hops", "txPackets", "LongestPath", "Orchestrators", "Neighbors", "Paths", "Distance")

##
## Simple Linear Regression
## Scatterplot X - Y (Latex label: fig_XvsY)
##

	data <- data_train
	
	if ( display_graphs == 1) {
		plot(data$Hops, data$Latency2, xlab="# of Hops", ylab="Network Latency (Seconds)", pch = 21, cex = 1, col = 1, bg = "black", cex.axis=1.5, cex.lab = 1.5)
		dev.copy2eps(file=paste(graphs_path,"/SimpleLinear-XversusY.eps", sep = ""))
		dev.new()

		plot(data$Hops, data$Latency2, xlab="# of Hops", ylab="Network Latency (Seconds)", pch = 21, cex = 1, col = 1, bg = "black", cex.axis=1.5, cex.lab = 1.5)
		lm <- lm(data$Latency2 ~ data$Hops, data = data)
		abline(lm)
		dev.copy2eps(file=paste(graphs_path,"/SimpleLinear-XversusY-fitted.eps", sep = ""))
	}

##
## Complexity Vs RMSE
## 
##
	# Taken from regression_comparison file
	
##
## Feature Scaling
## (Latex label: )
##

scale = function(x){
 	newX = (x - min(x))/(max(x) - min(x))
	return (newX)
}

# Example
plot(data$Hops, data$Latency2)
dev.new()
plot(scale(data$Hops), scale(data$Latency2))

cor(data$Hops, data$Latency2)
cor(scale(data$Hops), scale(data$Latency2))
