# Diabetes-Predictive-Model
########################################################################
# R Code for Diabetes Predictive Model
# Author: Arielle S. Selya
# Date posted: 12/27/2020
########################################################################

This project utilizes electronic medical record (EMR) data from Sanford Health to identify a predictive model that predicts unplanned medical visits (any vs. none) over a 3-year period, among patients with diabetes. This code accompanies a manuscript under review at BMC Medical Informatics and Decision Making, which is also available as a pre-print at ResearchSquare (https://doi.org/10.21203/rs.3.rs-72164/v1).

This code assumes that all data management of individual variables has been performed appropriately (e.g, outcome variable is binary for 0 = no unplanned medical visits; 1 = at least one unplanned medical visit). 

The file "Diabetes Predictive Model Analysis" provides some data management (removing missing data on key variables) and performs descriptive statistics for Table 1 of the accompanying manuscrupt.

The files "LDA Diabetes Predictive Model R1", "QDA Diabetes Predictive Model R1", "Linear SVM  Diabetes Predictive Model R1", "Radial SVM Diabetes Predictive Model R1", "NN Diabetes Predictive Model R1", "Deepnet Diabetes Predictive Model R1", and "Logistic Regression Diabetes Predictive Model R1" train and cross-validate each respective classifier, and evaluate training and testing metrics (tables 3 and 2, respectively, in the accompanying manuscript).

The file "SVM sensitivity analysis Diabetes Predictive Model R1" performs a post-hoc sensitivity analysis to assess the contribution of each modifiable predictor variable to the preditive performance of the optimized radial SVM model (table 4 in the accompanying manuscript).
