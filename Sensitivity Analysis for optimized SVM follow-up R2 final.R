########################################################################
# Sensitivity Analysis for Best-Case Classifier (Here, Linear SVM)
# Author: Arielle S. Selya
# Date posted: 2/28/2021
########################################################################
load("Managed Sanford Data.RData")
require(e1071)

# Create different subsets that introduces noise to each variable,
#  one at a time:
#   A1C
Sanford.data.diabetes.sub.A1C <- Sanford.data.diabetes
noise <- rnorm(nrow(Sanford.data.diabetes.sub.A1C), 
               mean=mean(Sanford.data.diabetes.sub.A1C$A1C), 
               sd=sd(Sanford.data.diabetes.sub.A1C$A1C))
Sanford.data.diabetes.sub.A1C$A1C <- Sanford.data.diabetes.sub.A1C$A1C + 0.3*noise

#   BMI
Sanford.data.diabetes.sub.BMI <- Sanford.data.diabetes
noise <- rnorm(nrow(Sanford.data.diabetes.sub.BMI), 
               mean=mean(Sanford.data.diabetes.sub.BMI$BMI), 
               sd=sd(Sanford.data.diabetes.sub.BMI$BMI))
Sanford.data.diabetes.sub.BMI$BMI <- Sanford.data.diabetes.sub.BMI$BMI + 0.3*noise

#   BP
Sanford.data.diabetes.sub.BP <- Sanford.data.diabetes
noise <- rnorm(nrow(Sanford.data.diabetes.sub.BP), 
               mean=mean(Sanford.data.diabetes.sub.BP$LastSystolic), 
               sd=sd(Sanford.data.diabetes.sub.BP$LastSystolic))
Sanford.data.diabetes.sub.BP$LastSystolic <- Sanford.data.diabetes.sub.BP$LastSystolic + 0.3*noise
noise <- rnorm(nrow(Sanford.data.diabetes.sub.BP), 
               mean=mean(Sanford.data.diabetes.sub.BP$LastDiastolic), 
               sd=sd(Sanford.data.diabetes.sub.BP$LastDiastolic))
Sanford.data.diabetes.sub.BP$LastDiastolic <- Sanford.data.diabetes.sub.BP$LastDiastolic + 0.3*noise

#   Tobacco use
Sanford.data.diabetes.sub.Tob <- Sanford.data.diabetes
noise <- rnorm(nrow(Sanford.data.diabetes.sub.Tob), 
               mean=mean(Sanford.data.diabetes.sub.Tob$Tobacco.Use.Ranked), 
               sd=sd(Sanford.data.diabetes.sub.Tob$Tobacco.Use.Ranked))
Sanford.data.diabetes.sub.Tob$Tobacco.Use.Ranked <- Sanford.data.diabetes.sub.Tob$Tobacco.Use.Ranked + 0.3*noise

#   LDL
Sanford.data.diabetes.sub.LDL <- Sanford.data.diabetes
noise <- rnorm(nrow(Sanford.data.diabetes.sub.LDL), 
               mean=mean(Sanford.data.diabetes.sub.LDL$LDL), 
               sd=sd(Sanford.data.diabetes.sub.LDL$LDL))
Sanford.data.diabetes.sub.LDL$LDL <- Sanford.data.diabetes.sub.LDL$LDL + 0.3*noise

#   HDL
Sanford.data.diabetes.sub.HDL <- Sanford.data.diabetes
noise <- rnorm(nrow(Sanford.data.diabetes.sub.HDL), 
               mean=mean(Sanford.data.diabetes.sub.HDL$HDL), 
               sd=sd(Sanford.data.diabetes.sub.HDL$HDL))
Sanford.data.diabetes.sub.HDL$HDL <- Sanford.data.diabetes.sub.HDL$HDL + 0.3*noise


# Create empty tables for accumulation
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

# Select current dataset (original, or noise-added ones above) --
#  Need to re-run from here below for each different dataset, and record results
Sanford.current.subset <- Sanford.data.diabetes

# Loop over 25 cross-validation iterations
for(i in 1:25){
  
  # Randomly select current 20% to hold out, for repeated subsampling
  ind <- sample(1:nrow(Sanford.current.subset), round(0.2*nrow(Sanford.current.subset)), replace = T)
  
  # Train model, using optimal hyperparameters
  svm.result <- svm(Unplanned.Visits.bin ~ ., data=Sanford.current.subset[-ind,], type = "C-classification",
            kernel="linear", cost=25)
  
  # Get training and testing results
  train.result <- predict(svm.result)
  test.result <- predict(svm.result, newdata=Sanford.current.subset[ind,])
  
  train.table <- prop.table(table(Sanford.current.subset$Unplanned.Visits.bin[-ind], train.result),1)
  test.table <- prop.table(table(Sanford.current.subset$Unplanned.Visits.bin[ind], test.result),1)
  
  # Print out row percentages of prediction tables
  print(train.table, 1)
  print(test.table, 1)
  
  # Increment tables
  for(j in 1:dim(train.table)[1])
    for(k in 1:dim(train.table)[2])
      train.table.all[j,k] <- train.table.all[j,k] + train.table[j,k]
  for(j in 1:dim(test.table)[1])
    for(k in 1:dim(test.table)[2])
      test.table.all[j,k] <- test.table.all[j,k] + test.table[j,k]
  print(prop.table(train.table, 1))
  print(prop.table(test.table, 1))
  print(i)
  
  # Increment arrays for variance
  train.cell00 <- c(train.cell00, train.table[1,1])
  train.cell01 <- c(train.cell01, train.table[1,2])
  train.cell10 <- c(train.cell10, train.table[2,1])
  train.cell11 <- c(train.cell11, train.table[2,2])
  test.cell00 <- c(test.cell00, test.table[1,1])
  test.cell01 <- c(test.cell01, test.table[1,2])
  test.cell10 <- c(test.cell10, test.table[2,1])
  test.cell11 <- c(test.cell11, test.table[2,2])
}

# Create averaged table
train.table.all <- train.table.all/i
test.table.all <- test.table.all/i

train.table.all
test.table.all

train.CR <- train.cell00  # CR = correct rejections
train.FA <- train.cell01  # FA = false alarms
train.M <- train.cell10   # M = misses
train.H <- train.cell11   # H = hits

test.CR <- test.cell00
test.FA <- test.cell01
test.M <- test.cell10
test.H <- test.cell11

# Define and calculate mean, SD of balanced accuracy for train and test sets
mean(train.avg <- (train.H + train.CR)/2)
sd(train.avg)
mean(test.avg <- (test.H + test.CR)/2)
sd(test.avg)

