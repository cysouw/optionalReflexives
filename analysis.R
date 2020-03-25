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

#' Decision trees

make.trees <- function(verb) {
	
	verbdata <- data.frame(lapply(data[[verb]], factor))
	treeRpart <- rpart(Reflexiv ~ . 
			, data = verbdata[,startPred[verb]:endPred[verb]]
			, model = TRUE
			)
	treeC50 <- C5.0(Reflexiv ~ . 
			, data = verbdata[,startPred[verb]:endPred[verb]]
			)
				
	rpart.plot(treeRpart)
	title(main = verb)
	plot(treeC50)
		
}

sapply(files, make.trees)

#' Linear models

for (verb in files) {
	
	verbdata <- data.frame(lapply(data[[verb]], factor))
	model <- glm(Reflexiv ~ . 
			, data = verbdata[,startPred[verb]:endPred[verb]]
			, family = binomial
			)
	cat("\n======\n")
	cat(verb)
	cat("\n======\n")
	print(summary(model))
	best <- step(model)
	print(summary(best))

}

# show Session Info
sessionInfo()