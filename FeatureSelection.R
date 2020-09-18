#R file that selects features in the training dataset


#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R
setwd("Data/Built_Tables/Combined")

train_data <- readRDS("train_data.RDS")


#I want to look specifically at DBP, SBP and MAP
#because these are linear combinations of eachother
names(train_data)
# [1] "patientid"             "Age"                   "Gender"
# [4] "Unit1"                 "Unit2"                 "HospAdmTime"
# [7] "HR_mean"               "O2Sat_mean"            "Temp_mean"
# [10] "SBP_mean"              "MAP_mean"              "DBP_mean"
# [13] "Resp_mean"             "EtCO2_mean"            "HR_min"
# [16] "O2Sat_min"             "Temp_min"              "SBP_min"
# [19] "MAP_min"               "DBP_min"               "Resp_min"
# [22] "EtCO2_min"             "HR_max"                "O2Sat_max"
# [25] "Temp_max"              "SBP_max"               "MAP_max"
# [28] "DBP_max"               "Resp_max"              "EtCO2_max"
# [31] "HR_sd"                 "O2Sat_sd"              "Temp_sd"
# [34] "SBP_sd"                "MAP_sd"                "DBP_sd"
# [37] "Resp_sd"               "EtCO2_sd"              "BaseExcess_mean"
# [40] "HCO3_mean"             "FiO2_mean"             "pH_mean"
#...

demographics = names(train_data)[1:6]
# > demographics
# [1] "patientid"   "Age"         "Gender"      "Unit1"       "Unit2"
# [6] "HospAdmTime"
demInc = demographics[2:3] #only including variables inherent to the patient


vitalSigns = names(train_data)[7:(6+8*4)]
Lab = names(train_data)[39:length(names(train_data))]

train_df = train_data[,colnames(train_data)%in% c(demInc,vitalSigns,Lab)]

library(caret)

train_corr = cor(train_df)

#Error in findCorrelation_fast(x = x, cutoff = cutoff, verbose = verbose) :
#  The correlation matrix has some missing values.


