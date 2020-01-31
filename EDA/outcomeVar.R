#getting started

#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R
#setwd("/u5/jlrwalto/User/Documents/Health-DS-Essay/Data/physionet.org/files/eicu-crd/2.0/")

setwd("/Users/jordyn/Documents/Health-DS-Essay/Data/eicu-collaborative-research-database-2.0/")

eicu_table_names = list.files(pattern = "*csv.gz")

eicu_diagnosis = read.csv(list.files(pattern = "diagnosis*"))

#> dim(eicu_diagnosis)
#[1] 2710672       7

head((eicu_diagnosis$diagnosisstring))
#3933 Levels, 3933 different diagnosis

all_diag = eicu_diagnosis$diagnosisstring
#cardiovascular|shock / hypotension|sepsis    cardiovascular|shock / hypotension|sepsis

diag_w_sepsis = all_diag[grepl("sepsis",all_diag)]
length(diag_w_sepsis)
#> length(diag_w_sepsis)
#[1] 115375

#length(unique(diag_w_sepsis))
#> length(unique(diag_w_sepsis))
#[1] 53

unique(diag_w_sepsis)


#suggested logic
#select by patient stay/patient
#select for diagnosis that contain sepsis
#take minimum offset time to get the time closest to icu admit, ie the first sepsis diagnosis