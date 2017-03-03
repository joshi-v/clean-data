rm(list=ls())
library (data.table)
library(dplyr)
library(DMwR)

subject.train <- read.delim("subject_train.txt", sep = "", header = FALSE)
y.train <- read.delim("y_train.txt", sep = "", header = FALSE)
X.train <- read.delim("X_train.txt", sep = "", header = FALSE)

subject.test <- read.delim("subject_test.txt", sep = "", header = FALSE)
y.test <- read.delim("y_test.txt", sep = "", header = FALSE)
X.test <- read.delim("X_test.txt", sep = "", header = FALSE)

#fix the activity type df
y.total <- rbind(y.test, y.train)
colnames(y.total) <- c("Activity")

# Fix the user df
subject.tot <- rbind(subject.test, subject.train)
colnames(subject.tot) <- c("SubjectID")

#get the correct column headings and extract mean and sdev from the main df

maindir <- "C:\\Users\\vivek\\Documents\\Data Science\\cleaning data\\project\\dataset"
setwd(maindir)
features <- read.delim("features.txt", sep = "", header = FALSE)
validfeat <- t(features$V2)
validfeat <- as.character(validfeat)
# clean feature names using gsub(replacethis, replaceby, var)
validfeat <- gsub("()", ".", validfeat, fixed = TRUE)
validfeat <- gsub(",", ".", validfeat, fixed = TRUE)
validfeat <- gsub(")", "", validfeat, fixed = TRUE)
validfeat <- gsub("(", ".", validfeat, fixed = TRUE)
validfeat <- gsub("-", ".", validfeat, fixed = TRUE)
X.total <- rbind(X.test, X.train)

# to avoid error due to naming issue, error is around 400: 420, make valid variable name
valid.column.names <- make.names(c(validfeat), unique = TRUE, allow_ = TRUE)
# replace .. by . in column names
valid.column.names <- gsub("..", ".", valid.column.names, fixed = TRUE)
# replace . at end of variable name
valid.column.names <- gsub("(\\.)+$", "", valid.column.names) 
names(X.total) <- valid.column.names

# extract columns with mean and sdev in their name

mean.std <- X.total %>%
  dplyr::select(contains("std", ignore.case = TRUE), contains("mean", ignore.case = TRUE))

finalarray <- subject.tot
finalarray <- cbind(finalarray, y.total)
finalarray <- cbind(finalarray, mean.std)

sapply(finalarray, class)  # class type of variables in the finalarray

Num_NA<-sapply(finalarray,function(y)length(which(is.na(y)==T))) # courtesy of data mining from library DmWR
cat ("Number of NA in the final array is: ", sum(Num_NA))

finalarray$Activity <- as.factor(finalarray$Activity)
finalarray$Activity <- factor(finalarray$Activity,
                    levels = c(1, 2, 3, 4, 5, 6),
                    labels = c("Walking", "Walking Upstairs", "Walking Downstairs",
                               "Sitting", "Standing", "Laying"))

# tidy data according the Hadley Wickham's paper http://vita.had.co.nz/papers/tidy-data.pdf
# is data with each variable in a column and each observation in a row.

resdf <- finalarray %>%
  group_by(Activity, SubjectID) %>%
  summarise_all(mean)

write.table(resdf, file = "cleandata.txt", row.names=FALSE)
