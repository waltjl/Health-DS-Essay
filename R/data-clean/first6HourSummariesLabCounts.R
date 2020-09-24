#R file that summarizes the first 6 hours in the ICU of each patient


#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R
library(parallel)
detectCores()
#[1] 48
num.cores = detectCores() - 10

setwd("Data/Built_Tables/Combined")
train_mats_list <- readRDS("train_patient_mat_list.RDS")

train_mats_list[[1]]


library(dplyr)

summarize_patient_data <- function(patient_mat){
  lmin <- function(vec,na.rm){
    if (all(is.na(vec))){return(NaN)} else{min(vec,na.rm = na.rm)}}
  lmax <- function(vec,na.rm){
    if (all(is.na(vec))){return(NaN)}else{max(vec,na.rm = na.rm)
    }}
  countMeasurements <- function(measurements){
    sum(!is.na(measurements))
  }
  
  
  
  Demographics <- patient_mat %>% group_by(patientid) %>%
    summarise(Age = mean(Age), Gender=mean(Gender),Unit1 = sum(Unit1), Unit2=sum(Unit2)
              ,HospAdmTime = mean(HospAdmTime))
  
  vitalSigns <- patient_mat %>% group_by(patientid) %>%
    summarise_at(vars(HR:EtCO2),list(mean=mean,min=lmin,max=lmax,sd=sd), na.rm = TRUE)
  
  Lab <- patient_mat %>% group_by(patientid) %>%
    summarise_at(vars(BaseExcess:Platelets),list(count = countMeasurements))
  
  merge(merge(Demographics,vitalSigns,by ="patientid"), Lab, by = "patientid")
  
  
}


train_pat_summaries <-mclapply(train_mats_list, summarize_patient_data,mc.cores = num.cores)
train_LabCount_data <- bind_rows(train_pat_summaries)
dim(train_LabCount_data)
length(unique(train_LabCount_data$patientid))

# > dim(train_LabCount_data)
# [1] 24781    64
# > length(unique(train_LabCount_data$patientid))
# [1] 24781

saveRDS(train_LabCount_data,"train_LabCount_data.RDS")

#same for test data
test_mats_list <- readRDS("test_patient_mat_list.RDS")
test_pat_summaries <-mclapply(test_mats_list, summarize_patient_data,mc.cores = num.cores)
test_LabCount_data <- bind_rows(test_pat_summaries)

dim(test_LabCount_data)
# [1] 6194  64
length(unique(test_LabCount_data$patientid))
# [1] 6194

saveRDS(test_LabCount_data,"test_LabCount_data.RDS")


