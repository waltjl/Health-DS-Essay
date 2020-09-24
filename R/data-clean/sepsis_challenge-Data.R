#opening up sepsis challenge data and building basic tables
# table 1: Sepsis Label, which patients get Sepsis
# table 2: LOS in ICU of each patient
# table 3: Sepsis Onset table, of patients which get Sepsis when.

#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R

setwd("Data/training_setA")
patientsA = list.files(pattern = "p*")
head(patientsA)
#[1] "p000001.psv" "p000002.psv" "p000003.psv" "p000004.psv" "p000005.psv" "p000006.psv"
patientAkey = gsub(".psv","",patientsA)
head(patientAkey)

#remove later
#patientAkey = patientAkey[1:100]
#patientsA = patientsA[1:100]

patientA_mats = lapply(patientsA,read.table,header = T,sep = "|")

setwd("../Built_Tables/SetA")

SepsisInc = sapply(patientA_mats, function(x){any(x$SepsisLabel)})
head(SepsisInc)
#[1] FALSE FALSE FALSE FALSE FALSE FALSE
sum(SepsisInc)

Sepsis_LabelA = data.frame(patientid = patientAkey,SepsisLabel = SepsisInc)
head(Sepsis_LabelA)
#patientid SepsisLabel
# 1   p000001       FALSE
# 2   p000002       FALSE
# 3   p000003       FALSE
# 4   p000004       FALSE
# 5   p000005       FALSE
# 6   p000006       FALSE

saveRDS(Sepsis_LabelA,"Sepsis_LabelA.RDS")
tmp = readRDS("Sepsis_LabelA.RDS")

#what is the length of stay we are working with?
A_LOS = sapply(patientA_mats, function(pat){max(pat$ICULOS)})
A_LOS = data.frame(patientid= patientAkey,ICULOS = A_LOS)
head(A_LOS)
#patientid ICULOS
# 1   p000001     54
# 2   p000002     23
# 3   p000003     48
# 4   p000004     29
# 5   p000005     49
# 6   p000006     19

# patientid ICULOS
# 1   p000001     54
# 2   p000002     23
# 3   p000003     48
# 4   p000004     29
# 5   p000005     49
# 6   p000006     19
saveRDS(A_LOS,"LOSA.RDS")



#LOS when SepsisLabel ==1, what hour is 6 hours before Sepsis Onset
SepsisA_mats = patientA_mats[SepsisInc]
SepsisA_keys = patientAkey[SepsisInc]

SepsisA_Onset6HB = sapply(SepsisA_mats,function(pat){min(pat$ICULOS[pat$SepsisLabel==1])})
SepsisA_Onset6HB= data.frame(patientid = SepsisA_keys, SepsisOnset = SepsisA_Onset6HB)

head(SepsisA_Onset6HB)
# patientid SepsisOnset
# 1   p000009         249
# 2   p000011          26
# 3   p000015           6
# 4   p000018         126
# 5   p000022          14
# 6   p000028          22

saveRDS(SepsisA_Onset6HB,"Sepsis_Onset6HBA.RDS")


######################################################################################
#For set B

setwd("../../training_setB/")
patientsB = list.files(pattern = "p*")
head(patientsB)
#[1] "p100001.psv" "p100002.psv" "p100003.psv" "p100004.psv" "p100005.psv" "p100006.psv"



patientBkey = gsub(".psv","",patientsB)

#remove later
#patientsB = patientsB[1:100]
#patientBkey = patientBkey[1:100]


head(patientBkey)


patientB_mats = lapply(patientsB,read.table,header = T,sep = "|")

setwd("../Built_Tables/SetB")

SepsisInc = sapply(patientB_mats, function(x){any(x$SepsisLabel)})
head(SepsisInc)
#[1] FALSE FALSE FALSE FALSE FALSE FALSE

sum(SepsisInc)
#[1] 10
Sepsis_LabelB = data.frame(patientid = patientBkey,SepsisLabel = SepsisInc)
head(Sepsis_LabelB)
# patientid SepsisLabel
# 1   p100001       FALSE
# 2   p100002       FALSE
# 3   p100003       FALSE
# 4   p100004       FALSE
# 5   p100005       FALSE
# 6   p100006       FALSE

saveRDS(Sepsis_LabelB,"Sepsis_LabelB.RDS")
tmp = readRDS("Sepsis_LabelB.RDS")

#what is the length of stay we are working with?
B_LOS = sapply(patientB_mats, function(pat){max(pat$ICULOS)})
B_LOS = data.frame(patientid= patientBkey,ICULOS = B_LOS)
head(B_LOS)
# patientid ICULOS
# 1   p100001     24
# 2   p100002     25
# 3   p100003     43
# 4   p100004     59
# 5   p100005     52
# 6   p100006     52

saveRDS(B_LOS,"LOSB.RDS")



#LOS when SepsisLabel ==1, what hour is 6 hours before Sepsis Onset
SepsisB_mats = patientB_mats[SepsisInc]
SepsisB_keys = patientBkey[SepsisInc]

SepsisB_Onset6HB = sapply(SepsisB_mats,function(pat){min(pat$ICULOS[pat$SepsisLabel==1])})
SepsisB_Onset6HB= data.frame(patientid = SepsisB_keys, SepsisOnset = SepsisB_Onset6HB)

head(SepsisB_Onset6HB)
# patientid SepsisOnset
# 1   p100013          81
# 2   p100016         266
# 3   p100047          27
# 4   p100049          79
# 5   p100055           1
# 6   p100068          95

saveRDS(SepsisB_Onset6HB,"Sepsis_Onset6HBB.RDS")

#combine each of the three tables
setwd("../Combined")

Sepsis_Label = rbind(Sepsis_LabelA,Sepsis_LabelB)
saveRDS(Sepsis_Label,"Sepsis_Label.RDS")

LOS = rbind(A_LOS,B_LOS)
saveRDS(LOS, "LOS.RDS")

Sepsis_Onset6HB = rbind(SepsisA_Onset6HB,SepsisB_Onset6HB)
saveRDS(Sepsis_Onset6HB,"Sepsis_Onset6HB.RDS")







#and we see in general

patient1= patientB_mats[[1]]
names(patient1)
#[1] "HR"               "O2Sat"            "Temp"             "SBP"              "MAP"              "DBP"              "Resp"            
#[8] "EtCO2"            "BaseExcess"       "HCO3"             "FiO2"             "pH"               "PaCO2"            "SaO2"            
#[15] "AST"              "BUN"              "Alkalinephos"     "Calcium"          "Chloride"         "Creatinine"       "Bilirubin_direct"
#[22] "Glucose"          "Lactate"          "Magnesium"        "Phosphate"        "Potassium"        "Bilirubin_total"  "TroponinI"       
#[29] "Hct"              "Hgb"              "PTT"              "WBC"              "Fibrinogen"       "Platelets"        "Age"             
#[36] "Gender"           "Unit1"            "Unit2"            "HospAdmTime"      "ICULOS"           "SepsisLabel"     



patient2= read.table("p100002.psv",header=T,sep="|")

patient3= read.table("p100003.psv",header=T,sep="|")
#each have a varying number of rows, different amount of times being recorded




