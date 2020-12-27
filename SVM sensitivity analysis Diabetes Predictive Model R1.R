########################################################################
# Follow-up sensitivity analysis for optimized radial SVM, 
#  to examine effect of removing "risky" ranges of each modifiable
#  predictor variable
# Author: Arielle S. Selya
# Date posted: 12/27/2020
########################################################################

# Load data
load("Managed Sanford Data.RData")
require(e1071)
require(descr)

# Create data subsets, each one restricting one variable at a time to 
#  "normal" or "low-risk" ranges
Sanford.data.diabetes.sub.BMI <- 
  Sanford.data.diabetes[Sanford.data.diabetes$BMI < 30,]
freq(Sanford.data.diabetes.sub.BMI$Unplanned.Visits.bin)
# Restricting on BMI leaves 15,885 obs, with 58.22% with unplanned visits

Sanford.data.diabetes.sub.BP <- 
  Sanford.data.diabetes[Sanford.data.diabetes$LastSystolic < 120 &
                              Sanford.data.diabetes$LastDiastolic < 80,]
freq(Sanford.data.diabetes.sub.BP$Unplanned.Visits.bin)
# Restricting on BP leaves 11,966 obs, with 60.1% with unplanned visits

Sanford.data.diabetes.sub.Tob <- 
  Sanford.data.diabetes[Sanford.data.diabetes$Tobacco.Use.Ranked < 3,]
freq(Sanford.data.diabetes.sub.Tob$Unplanned.Visits.bin)
# Restricting on tobacco use leaves 38,370 obs with 56.8% unplanned visits

Sanford.data.diabetes.sub.LDL <-
  Sanford.data.diabetes[Sanford.data.diabetes$LDL < 130,] 
freq(Sanford.data.diabetes.sub.LDL$Unplanned.Visits.bin)
# Restricting on LDL gives 38,384 obs with 57.1% unplanned visits

Sanford.data.diabetes.sub.HDL <-
  Sanford.data.diabetes[Sanford.data.diabetes$HDL < 50,]
freq(Sanford.data.diabetes.sub.HDL$Unplanned.Visits.bin)
# Restricting on HDL gives 30,058 observations with 58.4% unplanned visits

Sanford.data.diabetes.sub.A1C <-
  Sanford.data.diabetes[Sanford.data.diabetes$A1C < 6.5,] 
freq(Sanford.data.diabetes.sub.A1C$Unplanned.Visits.bin)
# Restricting on A1C gives 13,857 obs with 58.4% unplanned visits 

# For one variable at a time, re-train optimized radial SVM model on each
#  data subset and see what happens to prediction 
#  (REPEAT FOR EACH DATASET ABOVE)

# Create empty tables for accumulation
train.table.all <- array(0, dim=c(2,2))
test.table.all <- array(0, dim=c(2,2))

# Loop over CV iterations
for(i in 1:1000){
  samp <- sample(1:dim(Sanford.data.diabetes.sub.A1C)[1], round(0.1*dim(Sanford.data.diabetes.sub.A1C)[1]))
  
  svm.result <- svm(Unplanned.Visits.bin ~ ., data=Sanford.data.diabetes.sub.A1C[-samp,], type = "C-classification",
                    kernel="sigmoid", gamma=.007)
  train.result <- predict(svm.result)
  test.result <- predict(svm.result, newdata=Sanford.data.diabetes.sub.A1C[samp,])
  
  train.table <- prop.table(table(Sanford.data.diabetes.sub.A1C$Unplanned.Visits.bin[-samp], train.result),1)
  test.table <- prop.table(table(Sanford.data.diabetes.sub.A1C$Unplanned.Visits.bin[samp], test.result),1)
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
}

# Create averaged table
train.table.all <- train.table.all/i
test.table.all <- test.table.all/i

train.table.all
test.table.all
