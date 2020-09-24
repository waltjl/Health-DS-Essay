#R file that summarizes the first 6 hours in the ICU of each patient


#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R
library(parallel)
detectCores()
#[1] 16
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
  
  Demographics <- patient_mat %>% group_by(patientid) %>%
    summarise(Age = mean(Age), Gender=mean(Gender),Unit1 = sum(Unit1), Unit2=sum(Unit2)
              ,HospAdmTime = mean(HospAdmTime))
  
  vitalSigns <- patient_mat %>% group_by(patientid) %>%
    summarise_at(vars(HR:EtCO2),list(mean=mean,min=lmin,max=lmax,sd=sd), na.rm = TRUE)
  
  
  merge(Demographics,vitalSigns,by ="patientid")
  
  
}


train_pat_summaries <-mclapply(train_mats_list, summarize_patient_data,mc.cores = num.cores)
train_vital_data <- bind_rows(train_pat_summaries)
dim(train_vital_data)
length(unique(train_vital_data$patientid))

# > dim(train_vital_data)
# [1] 24781   142
# > length(unique(train_vital_data$patientid))
# [1] 24781

saveRDS(train_vital_data,"train_vital_data.RDS")

#same for test data
test_mats_list <- readRDS("test_patient_mat_list.RDS")
test_pat_summaries <-mclapply(test_mats_list, summarize_patient_data,mc.cores = num.cores)
tes_vital_data <- bind_rows(test_pat_summaries)

# > dim(test_vital_data)
# [1] 6194  142
# length(unique(test_vital_data$patientid))
# [1] 6194

saveRDS(test_vital_data,"test_vital_data.RDS")


