Surrogate Model Creation	 		          May 20, 2014
===========================================================================

This file contains information about how to build and evaluate four popular approximation models used as surrogates in the developed surrogate-assisted optimisation approach.

1. File list
------------------------------------------------
main.R		R code orchestrating the creation and evaluation of the four surrogate models.
LR.R		R code for creating and evaluating the LR model.
MARS.R		R code for creating and evaluating the MARS model.
CART.R		R code for creating and evaluating the CART model.
RF.R		R code for creating and evaluating the RF model.
datasets/	Folder containing the necessary datasets.
images/		Folder containing all the generated graphs.
README.txt	This file


2. Data generation and graph formatting
------------------------------------------------

1. Execute the R scripts in chapter_plotting.R to generate all the figures for the chapter.

2. Convert all the generated .eps files to .pdf with the following command:
	find . -name "*.eps" -exec epstopdf {} \;

3. Execute the R scripts in main.R to create and evaluate the developed surrogate models.
