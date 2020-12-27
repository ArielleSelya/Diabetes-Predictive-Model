########################################################################
# Data management and descriptive statistics of predictive variables 
#   for unplanned visits among patients with diabetes, 
#   by any vs. no unplanned visits
# Author: Arielle S. Selya
# Date posted: 12/27/2020
########################################################################

load("Managed Sanford Data.RData")
library(descr)
options(scipen = 999, digits=4)

# Create subset of patients with diabetes
Sanford.data.diabetes <- Sanford.data.adults[Sanford.data.adults$Diabetes == 1,
                   c("Unplanned.Visits.bin", "BMI", "LastSystolic",
                     "LastDiastolic", "ProbListN", "Pat.Age", "HSD_Rx",
                     "Tobacco.Use.Ranked", "LDL", "HDL", "A1C")]

# Remove observations that are missing on any of the above variables
Sanford.data.diabetes <- na.omit(Sanford.data.diabetes)

# Remove BMI values below 10 or above 100 due to biological implausibility 
Sanford.data.diabetes$BMI[Sanford.data.diabetes$BMI < 15] <- NA
Sanford.data.diabetes$BMI[Sanford.data.diabetes$BMI > 60] <- NA
Sanford.data.diabetes <- na.omit(Sanford.data.diabetes)

# Remove A1C values below 4 or above 15 due to biological implausibility
Sanford.data.diabetes$A1C[Sanford.data.diabetes$A1C < 4] <- NA
Sanford.data.diabetes$A1C[Sanford.data.diabetes$A1C > 15] <- NA
Sanford.data.diabetes <- na.omit(Sanford.data.diabetes)

# Remove HDL values below 10 or above 100 due to biological implausibility
Sanford.data.diabetes$HDL[Sanford.data.diabetes$HDL < 10] <- NA
Sanford.data.diabetes$HDL[Sanford.data.diabetes$HDL > 100] <- NA
Sanford.data.diabetes <- na.omit(Sanford.data.diabetes)

# Remove LDL values below 20 or above 200 due to biological implausibility
Sanford.data.diabetes$LDL[Sanford.data.diabetes$LDL < 20] <- NA
Sanford.data.diabetes$LDL[Sanford.data.diabetes$LDL > 200] <- NA
Sanford.data.diabetes <- na.omit(Sanford.data.diabetes)

# Final dataset is Sanford.data.diabetes, N=43,831.

# Descriptive statistics (Table 1), broken down by 0 vs. 1+ Unplanned Visit
freq(Sanford.data.diabetes$Unplanned.Visits.bin) # for overall N's

# Age
by(Sanford.data.diabetes$Pat.Age, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(Pat.Age ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# Body mass index (BMI)
by(Sanford.data.diabetes$BMI, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(BMI ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# Systolic blood pressure (BP)
by(Sanford.data.diabetes$LastSystolic, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(LastSystolic ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# Diastolic BP
by(Sanford.data.diabetes$LastDiastolic, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(LastDiastolic ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# LDL cholesterol
by(Sanford.data.diabetes$LDL, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(LDL ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# HDL cholesterol
by(Sanford.data.diabetes$HDL, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(HDL ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# A1C
by(Sanford.data.diabetes$A1C, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(A1C ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# Ranked smoking status
by(Sanford.data.diabetes$Tobacco.Use.Ranked, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(Tobacco.Use.Ranked ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# Number of diagnoses on problem list
by(Sanford.data.diabetes$ProbListN, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(ProbListN ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)

# Number of prescriptions
by(Sanford.data.diabetes$HSD_Rx, Sanford.data.diabetes$Unplanned.Visits.bin, summary)
t.test(HSD_Rx ~ Unplanned.Visits.bin, data=Sanford.data.diabetes)
