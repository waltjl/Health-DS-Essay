#getting started

#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

R
setwd("/u5/jlrwalto/User/Documents/Health-DS-Essay/Data/physionet.org/files/eicu-crd/2.0/")

eicu_table_names = list.files(pattern = "*csv.gz")

eicu_patient = read.csv(list.files(pattern = "patient*"))

eicu_samp_patient =subset(eicu_patient,uniquepid == "002-37665")

#how to subset for a patients first stay

eicu_patient_small = eicu_patient[,colnames(eicu_patient) %in% c('uniquepid','patientunitstayid'
                                                                 ,'hospitaldischargelocation','unitdischargelocation'
                                                                 ,'hospitaldischargeyear')]

#> dim(eicu_patient_small)
#[1] 200859      5

#get stay id of patients who had a single visit in a table
get_saved_ids <- function(patient_table){
  patient_count = table(patient_table$uniquepid)
  patient_singles = names(patient_count)[patient_count == 1]
  patient_table_single = subset(patient_table, uniquepid %in% patient_singles)
  #save stay ids
  saved_patientunitstayid = patient_table_single$patientunitstayid
  return(saved_patientunitstayid)
}

saved_patientunitstayid = get_saved_ids(eicu_patient_small)

#get rid of patients with single visits
reduce_table_morethan1 <- function(patient_table){
  patient_count = table(patient_table$uniquepid)
  patient_morethan1 = names(patient_count)[patient_count > 1]
  patient_table_morethan1 = subset(patient_table, uniquepid %in% patient_morethan1)
  return(patient_table_morethan1)
}


patient_table = eicu_patient

get_first_stay<- function(patient_table){
 require(dplyr)
  
  saved_stay_ids = get_saved_ids(patient_table)
  morethan1_table = reduce_table_morethan1(patient_table)
  
  #remove discharges with death, because they can't be the first visit
  #removeDeath_hosp = subset(morethan1_table, hospitaldischargelocation != 'Death')
  #saved_stay_ids_death_hosp = get_saved_ids(removeDeath_hosp)
  #morethan1_removeDeath_hosp = reduce_table_morethan1(removeDeath_hosp)
  
  removeDeath_unit = subset(morethan1_table, unitdischargelocation != 'Death')
  
  saved_stay_ids_death_unit = get_saved_ids(removeDeath_unit)
  saved_stay_ids = c(saved_stay_ids, saved_stay_ids_death_unit)
  
  morethan1_removeDeath_unit = reduce_table_morethan1(removeDeath_unit)
  
  #remove dischargers that were in later years if they exist
  removeLaterYears <- morethan1_removeDeath_unit %>%
    group_by(uniquepid) %>%
    filter(hospitaldischargeyear == min(hospitaldischargeyear,na.rm=T))
  
  saved_stay_ids_removeLaterYears = get_saved_ids(removeLaterYears)
  saved_stay_ids = c(saved_stay_ids, saved_stay_ids_removeLaterYears)
  
  morethan1_removeLaterYears = reduce_table_morethan1(removeLaterYears)
  
  #remove later icu stays within the same hospital stay
  removeLaterICU <- morethan1_removeLaterYears %>%
    group_by(patientHealthSystemStayID) %>%
    filter(hospitalAdmitOffset == min(hospitalAdmitOffset,na.rm=T))
  
  saved_stay_ids_removeLaterICU = get_saved_ids(removeLaterICU)
  saved_stay_ids = c(saved_stay_ids, saved_stay_ids_removeLaterICU)
  
  morethan1_removeLaterICU = reduce_table_morethan1(removeLaterICU)
  
}

#check
saved_stay_ids = unique(saved_stay_ids)
length(saved_stay_ids)



samp = subset(morethan1_removeDeath_unit, unitdischargelocation != 'Death')

max(table(morethan1_removeDeath_unit$uniquepid))
