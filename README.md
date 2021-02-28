# Diabetes-Predictive-Model
########################################################################
# R Code for Diabetes Predictive Model
# Author: Arielle S. Selya
# Date last edited: 2/28/2021
########################################################################

This project utilizes electronic medical record (EMR) data from Sanford Health to identify a predictive model that predicts unplanned medical visits (any vs. none) over a 3-year period, among patients with diabetes. This code accompanies a manuscript under review at BMC Medical Informatics and Decision Making, which is also available as a pre-print at ResearchSquare (https://doi.org/10.21203/rs.3.rs-72164/v1).

This code assumes that all data management of individual variables has been performed appropriately (e.g, outcome variable is binary for 0 = no unplanned medical visits; 1 = at least one unplanned medical visit). 

The file "Diabetes Predictive Model Analysis" provides some data management (removing missing data on key variables) and performs descriptive statistics for Table 1 of the accompanying manuscrupt.

Initial Revision Files (do not use):

The files "LDA Diabetes Predictive Model R1", "QDA Diabetes Predictive Model R1", "Linear SVM  Diabetes Predictive Model R1", "Radial SVM Diabetes Predictive Model R1", "NN Diabetes Predictive Model R1", "Deepnet Diabetes Predictive Model R1", and "Logistic Regression Diabetes Predictive Model R1" train and cross-validate each respective classifier, and evaluate training and testing metrics (tables 3 and 2, respectively, in the accompanying manuscript).

The file "SVM sensitivity analysis Diabetes Predictive Model R1" performs a post-hoc sensitivity analysis to assess the contribution of each modifiable predictor variable to the preditive performance of the optimized radial SVM model (table 4 in the accompanying manuscript).

Second Revision Files (current verions):

The files "LDA Diabetes Predictive Model R2", "QDA Diabetes Predictive Model R2", "Linear SVM  Diabetes Predictive Model R2", "Radial SVM Diabetes Predictive Model R2", "NN Diabetes Predictive Model R2", "Deepnet Diabetes Predictive Model R2", "XG Boost Diabetes Predictive Model R2", and "Logistic Regression Diabetes Predictive Model R2" train and cross-validate each respective classifier. Classifiers with hyperparameters employ nested cross-validation for selection of optimal hyperparameters (this is the case for linear and radial SVM, neural net, deep net, and XG boost classifiers). These scripts evaluate training and testing metrics (tables 4 and 2, respectively, in the accompanying manuscript), and in the case of the instances using nested cross-validation, records the best hyper-parameters within each outer loop (table 3 in the accompanying manuscript.
