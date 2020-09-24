#R file for isolating first 6 hours of ICU data for each patient in test/train, save in separate lists.

#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R
#get patient keys in train set and test set
setwd("Data/Built_Tables/Combined")
SepsisLabel_A12H_train <- readRDS("train_SepsisLabel_A12H.RDS")
SepsisLabel_A12H_test <- readRDS("test_SepsisLabel_A12H.RDS")

setwd("../../training_setA")
patientA = list.files(pattern = "p*")
head(patientA)
#[1] "p000001.psv" "p000002.psv" "p000003.psv" "p000004.psv" "p000005.psv" "p000006.psv"
patientAkey = gsub(".psv","",patientA)
head(patientAkey)

#select patient ids in train set for hospital A
train_patientAkey_files = patientA[patientAkey %in% SepsisLabel_A12H_train$patientid]
test_patientAkey_files = patientA[patientAkey %in% SepsisLabel_A12H_test$patientid]

#read first 6 hours of stay of patient psv
first6Hours <- function(file_name){
  psv = read.table(file_name,header = T,sep = "|")
  psv = psv[,!colnames(psv) %in% "SepsisLabel"]
  psv$patientid = gsub(".psv","",file_name)
  psv = psv[psv$ICULOS<=6,]
  psv
}

tmp <- first6Hours(train_patientAkey_files[55])


#import patient matrices in train set for hospital A
train_patientA_mats = lapply(train_patientAkey_files,first6Hours)
test_patientA_mats = lapply(test_patientAkey_files,first6Hours)

setwd("../training_setB")
patientsB = list.files(pattern = "p*")
head(patientsB)
#[1] "p100001.psv" "p100002.psv" "p100003.psv" "p100004.psv" "p100005.psv" "p100006.psv"

patientBkey = gsub(".psv","",patientsB)
head(patientBkey)

#select patient ids in train set for hospital B
train_patientBkey_files = patientsB[patientBkey %in% SepsisLabel_A12H_train$patientid]
test_patientBkey_files = patientsB[patientBkey %in% SepsisLabel_A12H_test$patientid]

#import patient matrices in train set for hospital B
train_patientB_mats = lapply(train_patientBkey_files,first6Hours)
test_patientB_mats = lapply(test_patientBkey_files,first6Hours)

setwd("../../Data/Built_Tables/Combined")
#combine both hospitals together
train_mats = append(train_patientA_mats,train_patientB_mats)
test_mats = append(test_patientA_mats,test_patientB_mats)

#randomize order
train_mats_list = train_mats[sample(1:length(train_mats))]
test_mats_list = test_mats[sample(1:length(test_mats))]

#here
length(train_mats_list)
length(test_mats_list)
# > length(train_mats_list)
# [1] 24781
# > length(test_mats_list)
# [1] 6194

saveRDS(train_mats_list,"train_patient_mat_list.RDS")
saveRDS(test_mats_list,"test_patient_mat_list.RDS")

#little test file
saveRDS(train_mats_list[1:100],"smol_train_patient_mat_list.RDS")
