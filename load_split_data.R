# Function that cleans, transforms the predictor variables and splits the input dataset.
# The training, validation and test sets are returned for building the surrogate approximation models.
load_data <- function(file, sample_size) {
        
        ##
        ## Step 1: Read dataset
        ##
        
        data <- read.csv(file, sep="\t", header=T)
        cat("Initial size of total data = ", dim(data), "\n")	
        data <- data[sample(nrow(data), sample_size), ] # Randomly choose 1000 data points
        cat("Sampled size of total data = ", dim(data), "\n")
        
        data$ID <- NULL			# Delete ID column
        data$ShortestPath <- NULL	# Delete ShortestPath column
        data$rxPackets <- NULL		# Delete rxPackets column
        
        ##
        ## Step 2: Variable transformation
        ##
        
        if ( display_graphs == 1) {
                # Checking for normality
                for(i in 1:length(predictors)){
                        qqnorm(data[,predictors[i]], main = paste("Predictor Variable = ", predictors_names[i]))
                        dev.new()
                        hist(data[,predictors[i]], main = paste("Histogram Predictor Variable = ", predictors_names[i]))
                        dev.new()
                        hist(log(data[,predictors[i]]), main = paste("Log Predictor Variable = ", predictors_names[i]))
                        dev.new()
                        Sys.sleep(10)
                }
        }
        
        # Leave untransformed: Orchestrators (10) and Distance (13)
        # Moderately positive skewness
        data[,7] = log(data[,7])
        
        # LongesthPath = log(LongesthPath)
        data[,9] = log(data[,9])
        
        # Neighbors = log(Neighbors)	
        data[,11] = log(data[,11])
        
        # Substantially negatively skewed
        data[,8] = sqrt(max(data[,8]) + 1 - data[,8])
        
        # Paths = hist(sqrt(max(Paths) + 1 - Paths))
        data[,12] = sqrt(max(data[,12]) + 1 - data[,12])
        
        data$Delay <- NULL		    # Delete Delay column
        data$Latency <- NULL		  # Delete Latency column
        data$txPackets <- NULL		# Delete txPackets column
        data$LongestPath <- NULL	# Delete LongestPath column
        data$Neighbors <- NULL		# Delete Neighbors column
        data$Distance <- NULL		  # Delete Distance column
        data$Paths <- NULL		    # Delete Paths column
        
        ##
        ## Step 3: Data Splitting
        ##
        
        # 60%
        data_train = data[1:(dim(data)[1] * 0.6), ]
        
        # 20%
        from = dim(data)[1] * 0.6 + 1
        to = dim(data)[1] * 0.6 + dim(data)[1] * 0.2
        data_valid = data[from:to, ]
        
        # 20%
        from = dim(data)[1] * 0.6 + dim(data)[1] * 0.2 + 1
        to = dim(data)[1] 
        data_test = data[from:to, ]
        
        return(list(data_train = data_train, data_valid = data_valid, data_test = data_test))
}