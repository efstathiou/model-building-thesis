install.packages("ellipse")
install.packages("corrplot")
install.packages("lattice")
install.packages("PerformanceAnalytics")

library(ellipse)
library(corrplot)
library(lattice)
library(PerformanceAnalytics)

file <- "datasets/trainingSet.csv" 
sample_size <- 979
graphs_path <- "images"

# Response Variables
responses <- c(1, 2, 3, 4)
responses_names <- c("Delay2", "Latency2", "Success_Ratio", "Energy")
responses_names2 <- c("Response_Time", "Network_Latency", "Success_Ratio", "Energy")

# Explanatory Variables
predictors <- c(7, 8, 9, 10, 11, 12, 13, 14)
predictors_names <- c("Hops",  "Orchestrators", "DevFast", "DevMedium", "DevSlow", "LoadSmall", "LoadMedium", "LoadBig")

# Function that cleans the dataset and prepares it for building the surrogate approximation models.
clean_dataset <- function(file, sample_size) {
	data <- read.csv(file, sep="\t", header=T)
	cat("Initial size of data = ", dim(data), "\n")	
	data <- data[sample(nrow(data), sample_size), ] # Randomly choose 1000 data points
	cat("Sampled size of data = ", dim(data), "\n")	

	data$ID <- NULL			# Delete ID column
	data$ShortestPath <- NULL	# Delete ShortestPath column
	data$rxPackets <- NULL		# Delete rxPackets column

	# Variable transformation
	# Leave untransformed: Orchestrators (10) and Distance (13)
	# Moderately positive skewness
	data[,7] = log(data[,7])
		
	# LongesthPath = log(LongesthPath)
	data[,9] = log(data[,9])	

	# Neighbors = log(Neighbors)	
	data[,11] = log(data[,11])
		
	# Substantially negatively skewed txPackets
	data[,8] = sqrt(max(data[,8]) + 1 - data[,8])

	# Paths = hist(sqrt(max(Paths) + 1 - Paths))
	data[,12] = sqrt(max(data[,12]) + 1 - data[,12])

	data$Delay <- NULL		# Delete Delay column
	data$Latency <- NULL		# Delete Latency column
	data$txPackets <- NULL		# Delete txPackets column
	data$LongestPath <- NULL	# Delete LongestPath column
	data$Neighbors <- NULL		# Delete Neighbors column
	data$Distance <- NULL		# Delete Distance column
	data$Paths <- NULL		# Delete Paths column

	return(data)
}	

## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
	R <- cor(X, use="pairwise.complete.obs")
	above <- row(R) < col(R)
	r2 <- R[above]^2
	Fstat <- r2 * dfr/(1 - r2)
	R[above] <- 1 - pf(Fstat, 1, dfr)
	R[row(R) == col(R)] <- NA
	R
}
 
## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
	if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
	if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
	ut <- upper.tri(m)
	data.frame(i = rownames(m)[row(m)[ut]],
	j = rownames(m)[col(m)[ut]],
	cor=t(m)[ut],
	p=m[ut])
}

# Function for printing various correlation plots for the input dataset.
plot_correlation <- function(data) {

	plot(data)
	dev.copy2eps(file=paste(graphs_path,"/scatterplot.eps", sep = ""))
	dev.new()
	
	library(ellipse)
	plotcorr(cor(data))
	dev.copy2eps(file=paste(graphs_path,"/correlation_ellipses.eps", sep = ""))
	dev.new()

	library(corrplot)
	corrplot(cor(data), method = "number" , tl.cex = 0.5)
	dev.copy2eps(file=paste(graphs_path,"/correlation_table.eps", sep = ""))
	dev.new()
	 
	chart.Correlation(data)
	dev.copy2eps(file=paste(graphs_path,"/correlation.eps", sep = ""))
	dev.new()

	# For each response variable plot pair scatterplots, regression lines and histograms (VERY NICE!)
	for(i in 1:length(responses)){	
    data_response <- cbind(data[,responses[i]], data[,5:12])
    colnames(data_response)[[1]] <- responses_names2[i]
	  chart.Correlation(data_response)	  
		dev.copy2eps(file=paste(graphs_path,"/scatterplot-",responses_names[i],".eps", sep = ""))
		dev.new()
	}
}

data <- clean_dataset(file)
plot_correlation(data)