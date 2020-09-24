#R file for isolating first 6 hours of ICU data

#ssh jlrwalto@linux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R

#looking closer at LOS stats, do we contain the first 6 hours in the ICU


#our cases in the set
setwd("/u5/jlrwalto/User/Documents/Health-DS-Essay/Data/Built_Tables/Combined")
SepsisLabel = readRDS("SepsisLabel_A12H.RDS")
head(SepsisLabel)

positiveCases = SepsisLabel$patientid[SepsisLabel$SepsisLabel]
ids = as.character(SepsisLabel$patientid)

setwd("../../../Data/training_setA")
patientsA = list.files(pattern = "p*")
head(patientsA)
#[1] "p000001.psv" "p000002.psv" "p000003.psv" "p000004.psv" "p000005.psv" "p000006.psv"
patientAkey = gsub(".psv","",patientsA)
head(patientAkey)

patientATrain = patientAkey[patientAkey %in% ids]
patientATrain_files = patientsA[patientAkey %in% ids]

#do we have first 6 hours in ICU
containFirst6Hours <- function(file_name){
  tmp <- read.table(file_name,header = T,sep = "|")
  all(1:6 %in% tmp$ICULOS)
}

test <- containFirst6Hours(patientATrain_files[1])
test


patientA_contain6hours = sapply(patientATrain_files,containFirst6Hours)
names(patientA_contain6hours) <- gsub(".psv","",names(patientA_contain6hours))

# sum(patientA_contain6hours)
# [1] 12565

# > length(patientA_contain6hours)
# [1] 19995

# > 12565/19995
# [1] 0.6313434

SepsisLabelA = SepsisLabel[SepsisLabel$patientid %in% patientAkey,]
positiveCasesA = SepsisLabelA$patientid[SepsisLabelA$SepsisLabel]
length(positiveCasesA)
#1449

patientA_contain6hoursNames = names(patientA_contain6hours)[patientA_contain6hours] 
SepsisLabelA_contain6hours= SepsisLabelA[SepsisLabelA$patientid %in% patientA_contain6hoursNames,]
dim(SepsisLabelA_contain6hours)
#12565
sum(SepsisLabelA_contain6hours$SepsisLabel)
#942

#lose 500 cases in hospital A



setwd("../training_setB/")
patientsB = list.files(pattern = "p*")
head(patientsB)
#[1] "p100001.psv" "p100002.psv" "p100003.psv" "p100004.psv" "p100005.psv" "p100006.psv"

patientBkey = gsub(".psv","",patientsB)

head(patientBkey)

patientBTrain = patientBkey[patientBkey %in% ids]
patientBTrain_files = patientsB[patientBkey %in% ids]

patientB_contain6hours = sapply(patientBTrain_files,containFirst6Hours)
names(patientB_contain6hours) <- gsub(".psv","",names(patientB_contain6hours))



sum(patientB_contain6hours)
# [1] 18410

length(patientB_contain6hours)
# [1] 19698

# > 18410/19698
# [1] 0.9346127

SepsisLabelB = SepsisLabel[SepsisLabel$patientid %in% patientBkey,]
positiveCasesB = SepsisLabelB$patientid[SepsisLabelB$SepsisLabel]
length(positiveCasesB)
#840

dim(SepsisLabel)
#39693
sum(SepsisLabel$SepsisLabel)
#2289

2289/39693


patientB_contain6hoursNames = names(patientB_contain6hours)[patientB_contain6hours] 
SepsisLabelB_contain6hours= SepsisLabelB[SepsisLabelB$patientid %in% patientB_contain6hoursNames,]
dim(SepsisLabelB_contain6hours)
#18410     2
#18410+12565 =30975

sum(SepsisLabelB_contain6hours$SepsisLabel)
#765

#765+942=1707 positives

1707/30975
#0.05510896


setwd("../../Data/Built_Tables/Combined")
containFirst6Hours = c(patientA_contain6hours,patientB_contain6hours)
saveRDS(containFirst6Hours,"patient_containFirst6Hours.RDS")

