#R file that summarizes the first 6 hours in the ICU of each patient


#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R
#get patient keys in train set and test set
setwd("/Users/jordyn/Documents/Health-DS-Essay/Built_Tables/")
train_mats_list <- readRDS("smol_train_patient_mat_list.RDS")

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
  
  
  Lab <- patient_mat %>% group_by(patientid) %>%
    summarise_at(vars(BaseExcess:Platelets),list(mean=mean,min=lmin,max=lmax,sd=sd), na.rm = TRUE)
  
  merge(merge(Demographics,vitalSigns,by ="patientid"),Lab,by="patientid")
  
  
}

trainpat1 <- summarize_patient_data(train_mats_list[[1]])

train_pat_summaries <-lapply(train_mats_list, summarize_patient_data)


system.time({ df <- do.call("rbind", train_pat_summaries) })
#user  system elapsed 
#0.005   0.009   0.031 
#system.time({ df2 <- ldply(train_pat_summaries, data.frame) })

system.time({ df3 <- bind_rows(train_pat_summaries) })
#user  system elapsed 
#0.006   0.014   0.023

#identical(df, df2)
identical(df,df3)
#[1] TRUE

df3

head(df3[,grep("Temp",colnames(df3))])
