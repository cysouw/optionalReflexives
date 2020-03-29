#' ---
#' title: "Predicting Reflexive Usage"
#' author: "Michael Cysouw"
#' date: "`r Sys.Date()`"
#' ---

# make html-version of this manual with:
# rmarkdown::render("analysis.R")

#' Loading libraries

library(readxl) # for reading excel files
library(rpart) # making decision trees
library(rpart.plot) # plotting rpart trees
library(C50) # another implementatino of decision trees

#' Loading data

files <- list.files("data", full.names = TRUE)
data <- sapply(files, read_excel)

columns <- sapply(data, names)
startPred <- sapply(columns, function(x){which(x == "Reflexiv")})
endPred <- sapply(columns, length)

#' Analysis

make.analyses <- function(verb) {
	
	cat("\n======\n")
	cat(verb)
	cat("\n======\n")

	verbdata <- data.frame(lapply(data[[verb]], factor))
	verbdata <- verbdata[,startPred[verb]:endPred[verb]]

	for (factor in 2:ncol(verbdata)) {
		
		freq <- table(verbdata$Reflexiv, verbdata[,factor])
		chi2 <- suppressWarnings(chisq.test(freq))
		res <- round(chi2$stdres, digits = 1)
		
		cat("\n")
		cat(colnames(verbdata)[factor])
		cat(", Chi-squared p-value = ")
		cat(signif(chi2$p.value, digits = 3))
		cat("\n")
		
		print(freq)
		print(res)
		
	}

	model <- glm(Reflexiv ~ . 
			, data = verbdata
			, family = binomial
			)
	treeRpart <- rpart(Reflexiv ~ . 
			, data = verbdata
			, model = TRUE
			)
	treeC50 <- C5.0(Reflexiv ~ . 
			, data = verbdata
			)
	
	print(summary(model))
	best <- step(model)
	print(summary(best))
				
	rpart.plot(treeRpart)
	title(main = verb)
	plot(treeC50)
		
}

sapply(files, make.analyses)

# show Session Info
sessionInfo()