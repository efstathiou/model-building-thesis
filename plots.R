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
graphs_path <- "images/Regression/"
display_graphs <- 0
file <- "datasets/trainingSet.csv" 
sample_size <- 300

source(file="load_split_data.R")
data <- load_data(file, sample_size)


data <- c(data$data_train, data$data_valid, data$data_test)
##
## Simple Linear Regression
## Scatterplot X - Y (Latex label: fig_XvsY)
##


plot(data$Hops, data$Latency2, xlab="# of Hops", ylab="Network Latency (Seconds)", pch = 21, cex = 1, col = 1, bg = "black", cex.axis=1.5, cex.lab = 1.5)
dev.copy2eps(file=paste(graphs_path,"SimpleLinear-XversusY.eps", sep = ""))
dev.new()

plot(data$Hops, data$Latency2, xlab="# of Hops", ylab="Network Latency (Seconds)", pch = 21, cex = 1, col = 1, bg = "black", cex.axis=1.5, cex.lab = 1.5)
lm <- lm(data$Latency2 ~ data$Hops, data = data)
abline(lm)
dev.copy2eps(file=paste(graphs_path,"SimpleLinear-XversusY-fitted.eps", sep = ""))



##
## Complete Scatterplot of the dataset
##

foo.upper <- function(x,y,...){
        points(x,y,...)
        #abline(lm(y~x), lty=1) 
}

diagonal.panel=function(x){ 
        par(new=T) 
        hist(x, main="", axes=F, nclass=12)
}

foo.lower <- function(x,y,...){
        # Set user coordinates of plotting region
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        test = summary(lm(y~x))
        #r <- test$r.squared[1]
        r = (cor(x, y))
        txt <- format(c(r, 0.123456789), digits=2)[1]
        txt <- paste(txt, sep="")
        cat("txt = ", txt, "\n")
        text(0.5, 0.5, txt, cex = 1 + 1.1 * r)
}

for(i in 1:length(responses)){	
        pairs(cbind(data[,responses[i]], data[,min(predictors):max(predictors)]), upper.panel = foo.upper, lower.panel = foo.lower, diag.panel = diagonal.panel, labels = c(responses_names2[i], "Hops", "txPackets", "LongestPath", "Orchestrators", "Neighbors", "Paths", "Distance"), cex.labels = 0.9)
        dev.copy2eps(file=paste(graphs_path,"/",responses_names3[i],"scatterplot.eps", sep = ""))
        dev.new()
}

##
## Complexity Vs RMSE
## 
##
# Taken from LR.R file

##
## CART example tree
## 
##
# Taken from main.R file



####################################
#
# Comparison Plots for document
#

compared_techniques <- 4

## 1 QoS : Response time

LR <- c(0.67, 0.34, 0.27, 0.12)
MARS <- c(0.68, 0.28, 0.23, 0.095)
CART <- c(0.5, 0.43, 0.34, 0.14)
RF <- c(0.62, 0.36, 0.3, 0.12)

metrics <- c("R-Squared", "RMSE", "MAE", "MAPE")
techniques <- c("LR", "MARS", "CART", "RF")

for (i in 1:compared_techniques) {
        y <- c(LR[i], MARS[i], CART[i], RF[i]) 
        
        # Get the range for the x and y axis
        xrange <- range(1:compared_techniques)
        yrange <- range(y)
        
        barplot(y, main=cat(metrics[i], " of compared techniques"), xlab="Regression Technique", ylab= metrics[i], width=.1, space = 1.5, names.arg=techniques, pch = 21, cex = 1.5, cex.axis=1.5, cex.lab = 1.5)
        dev.copy2eps(file=paste(graphs_path,"/", metrics[i], "-QoS1.eps", sep = ""))
        dev.new()
}

## 2 QoS : Network Latency

LR <- c(0.67, 0.33, 0.26, 0.12)
MARS <- c(0.68, 0.28, 0.23, 0.099)
CART <- c(0.5, 0.43, 0.34, 0.15)
RF <- c(0.62, 0.37, 0.3, 0.13)

for (i in 1:compared_techniques) {
        y <- c(LR[i], MARS[i], CART[i], RF[i]) 
        
        # Get the range for the x and y axis
        xrange <- range(1:compared_techniques)
        yrange <- range(y)
        
        barplot(y, main=cat(metrics[i], " of compared techniques"), xlab="Regression Technique", ylab= metrics[i], width=.1, space = 1.5, names.arg=techniques, pch = 21, cex = 1.5, cex.axis=1.5, cex.lab = 1.5)
        dev.copy2eps(file=paste(graphs_path,"/", metrics[i], "-QoS2.eps", sep = ""))
        dev.new()
}

## 3 QoS : Success Ratio

LR <- c(0.4, 7.8, 6.5, 0.081)
MARS <- c(0.39, 7.4, 6.1, 0.074)
CART <- c(0.096, 8.5, 7, 0.086)
RF <- c(0.3, 7.5, 6.3, 0.078)

for (i in 1:compared_techniques) {
        y <- c(LR[i], MARS[i], CART[i], RF[i]) 
        
        # Get the range for the x and y axis
        xrange <- range(1:compared_techniques)
        yrange <- range(y)
        
        barplot(y, main=cat(metrics[i], " of compared techniques"), xlab="Regression Technique", ylab= metrics[i], width=.1, space = 1.5,  names.arg=techniques, pch = 21, cex = 1.5, cex.axis=1.5, cex.lab = 1.5)
        dev.copy2eps(file=paste(graphs_path,"/", metrics[i], "-QoS3.eps", sep = ""))
        dev.new()
}

## 4 QoS : Energy

LR <- c(0.57, 0.088, 0.07, 0.075)
MARS <- c(0.57, 0.074, 0.057, 0.06)
CART <- c(0.35, 0.11, 0.083, 0.085)
RF <- c(0.48, 0.094, 0.071, 0.072)

for (i in 1:compared_techniques) {
        y <- c(LR[i], MARS[i], CART[i], RF[i]) 
        
        # Get the range for the x and y axis
        xrange <- range(1:compared_techniques)
        yrange <- range(y)
        
        barplot(y, main=cat(metrics[i], " of compared techniques"), xlab="Regression Technique", ylab= metrics[i], width=.1, space = 1.5,  names.arg=techniques, pch = 21, cex = 1.5, cex.axis=1.5, cex.lab = 1.5)
        dev.copy2eps(file=paste(graphs_path,"/", metrics[i], "-QoS4.eps", sep = ""))
        dev.new()
}
####################################