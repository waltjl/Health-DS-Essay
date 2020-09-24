#opening up sepsis tables, answering questions and building the label table
#pick threshold for Sepsis prediction beyond 12 hours
#build Sepsis table

#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R


setwd("Data/Built_Tables/Combined")

##LOS
LOS_df = readRDS("LOS.RDS")
Sepsis_label_df = readRDS("Sepsis_Label.RDS")
Sepsis_Onset_6HB = readRDS("Sepsis_Onset6HB.RDS")

#> dim(Sepsis_label_df)
#[1] 40336     2

sum(Sepsis_label_df$SepsisLabel)
#[1] 2932 get sepsis in the ICU
#> 2932/40336
#1] 0.07268941

min(Sepsis_Onset_6HB$SepsisOnset)
#[1] 1

table(Sepsis_Onset_6HB$SepsisOnset)
#1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
#370  89  71  56  57  63  47  58  50  35  35  41  48  51  28  30  30  36  23  37

summary(Sepsis_Onset_6HB$SepsisOnset)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#1.0     7.0    29.0    50.9    73.0   331.0

head(LOS_df)
summary(LOS_df$ICULOS)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#8.00   24.00   39.00   39.01   47.00  336.00
#all have longer

sum(Sepsis_Onset_6HB$SepsisOnset>=6)
#2289

sum(Sepsis_Onset_6HB$SepsisOnset<6)

#6 hours as a chosen threshold point
head(Sepsis_Onset_6HB)
#643/2932

#need to build a new Sepsis Label table
Sepsis_A12HoursT = Sepsis_Onset_6HB$patientid[Sepsis_Onset_6HB$SepsisOnset >=6]
Sepsis_A12HoursT = data.frame(patientid = Sepsis_A12HoursT,SepsisLabel = TRUE)

Sepsis_A12HoursF = Sepsis_label_df$patientid[!Sepsis_label_df$SepsisLabel]
Sepsis_A12HoursF = data.frame(patientid = Sepsis_A12HoursF,SepsisLabel = FALSE)

SepsisLabel_A12H = rbind(Sepsis_A12HoursT,Sepsis_A12HoursF)
sum(SepsisLabel_A12H$SepsisLabel)
saveRDS(SepsisLabel_A12H, "SepsisLabel_A12H.RDS")
#SepsisLabel_A12H <- readRDS("SepsisLabel_A12H.RDS")
#see now containsFirst6Hours.R, basically insert here/run here

#subset for patients which contain those first 6 hours of data
containFirst6Hours <- readRDS("patient_containFirst6Hours.RDS")
ids_containFirst6Hours <- names(containFirst6Hours)[containFirst6Hours]
SepsisLabel6H_A12H <- SepsisLabel_A12H[SepsisLabel_A12H$patientid %in% ids_containFirst6Hours,]
saveRDS(SepsisLabel6H_A12H, "SepsisLabel6H_A12H.RDS")

#need to create a class balanced test and train set
library(caret)
set.seed(56)

trainIndex <- createDataPartition(SepsisLabel6H_A12H$SepsisLabel, p = .8, 
                                  list = FALSE, 
                                  times = 1)


SepsisLabel6H_A12H_Train <-SepsisLabel6H_A12H[trainIndex,]
dim(SepsisLabel6H_A12H_Train)
# > dim(SepsisLabel6H_A12H_Train)
# [1] 24781     2
#> sum(SepsisLabel6H_A12H_Train$SepsisLabel)
#[1] 1366
saveRDS(SepsisLabel6H_A12H_Train, "train_SepsisLabel_A12H.RDS")

SepsisLabel6H_A12H_Test <- SepsisLabel6H_A12H[-trainIndex,]
dim(SepsisLabel6H_A12H_Test)
saveRDS(SepsisLabel6H_A12H_Test, "test_SepsisLabel_A12H.RDS")

# > dim(SepsisLabel6H_A12H_Test)
# [1] 6194    2