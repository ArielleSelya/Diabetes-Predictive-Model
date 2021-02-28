########################################################################
# Logistic Regression Classification
# Author: Arielle S. Selya
# Date posted: 2/28/2021
########################################################################
load("Managed Sanford Data.RData")

# Create empty tables/arrays to accumulate results across CV iterations 
train.table.all <- array(0, dim=c(2,2))
test.table.all <- array(0, dim=c(2,2))
train.cell00 <- NULL
train.cell01 <- NULL
train.cell10 <- NULL
train.cell11 <- NULL
test.cell00 <- NULL
test.cell01 <- NULL
test.cell10 <- NULL
test.cell11 <- NULL

# Split into 5 outer folds
ind <- sample(1:dim(Sanford.data.LDA.diabetes)[1])
n.folds <- length(ind)/5
ind <- split(ind, ceiling(seq_along(ind)/n.folds))

# Loop over CV iterations

for(i in 1:5){
  # Train logistic regression model
  Logreg.out <- glm(Unplanned.Visits.bin ~ ., data=Sanford.data.LDA.diabetes[-ind[[i]],], family="binomial")
  # Get predicted values
  train.result <- round(predict(Logreg.out, type="response"))
  test.result <- round(predict(Logreg.out, newdata=Sanford.data.LDA.diabetes[ind[[i]],], type="response"))
  # Get tables of real vs. predicted classes
  train.table <- prop.table(table(train.result, Sanford.data.LDA.diabetes$Unplanned.Visits.bin[-ind[[i]]]),1)
  test.table <- prop.table(table(test.result, Sanford.data.LDA.diabetes$Unplanned.Visits.bin[ind[[i]]]),1)
  # Print out row percentages of prediction tables
  print(train.table, 1)
  print(test.table, 1)
  # Increment tables (for average confusion matrix)
  train.table.all <- train.table.all + train.table
  test.table.all <- test.table.all + test.table
  # Increment arrays (for variance calculations within each cell)
  train.cell00 <- c(train.cell00, train.table[1,1])
  train.cell01 <- c(train.cell01, train.table[1,2])
  train.cell10 <- c(train.cell10, train.table[2,1])
  train.cell11 <- c(train.cell11, train.table[2,2])
  test.cell00 <- c(test.cell00, test.table[1,1])
  test.cell01 <- c(test.cell01, test.table[1,2])
  test.cell10 <- c(test.cell10, test.table[2,1])
  test.cell11 <- c(test.cell11, test.table[2,2])
}

# Create averaged confusion matrix across all CV iterations
train.table.all <- train.table.all/i
test.table.all <- test.table.all/i

Lreg.train <- train.table.all
Lreg.test <- test.table.all

Lreg.train.CR <- train.cell00  # CR = correct rejections
Lreg.train.FA <- train.cell01  # FA = false alarms
Lreg.train.M <- train.cell10   # M = misses
Lreg.train.H <- train.cell11   # H = hits

Lreg.test.CR <- test.cell00
Lreg.test.FA <- test.cell01
Lreg.test.M <- test.cell10
Lreg.test.H <- test.cell11

# Get means and sd's of each cell
#  Training set (Table 4)
summary(Lreg.train.CR)
sd(Lreg.train.CR)
summary(Lreg.train.FA)
sd(Lreg.train.FA)
summary(Lreg.train.M)
sd(Lreg.train.M)
summary(Lreg.train.H)
sd(Lreg.train.H)

#  Test set (Table 2)
summary(Lreg.test.CR)
sd(Lreg.test.CR)
summary(Lreg.test.FA)
sd(Lreg.test.FA)
summary(Lreg.test.M)
sd(Lreg.test.M)
summary(Lreg.test.H)
sd(Lreg.test.H)
