setwd("/Users/jordyn/Documents/Health-DS-Essay/R/scale_sim/")
train_data <- readRDS("train_labCount_input_data.RDS")
train_indices <- readRDS("train_indices.RDS")

TrainFold1<- train_data[train_indices[[1]],]
TestFold1 <- train_data[-train_indices[[1]],]

TrainFoldtest<- TrainFold1[1:5,1:5]

TestFoldtest<- TestFold1[1:5,1:5]


rescalePredictors <- function(train_data,test_data){
  Mins = colwise(min)(train_data)
  Maxs = colwise(max)(train_data)

  train_new = sapply(1:ncol(train_data), function(j) {
    ((train_data[,j] - Mins[1,j])/(Maxs[1,j] - Mins[1,j])*2)-1
  })
  dimnames(train_new)<- dimnames(train_data)
  train_new =as.data.frame(train_new)
  
  
  test_new = sapply(1:ncol(test_data), function(j) {
    ((test_data[,j] - Mins[1,j])/(Maxs[1,j] - Mins[1,j])*2)-1
  })
  test_new = as.data.frame(test_new)
  dimnames(test_new)<- dimnames(test_data)

  
  list(train = train_new, test = test_new)
}  
 
tmp = rescalePredictors(TrainFold1[,!colnames(TrainFold1) %in% "SepsisLabel"]
                        ,TestFold1[,!colnames(TestFold1) %in% "SepsisLabel"])
dim(tmp$train)
dim(tmp$test)

head(tmp$train)

#compares each row to each row
cos.sim=function(ma, mb){
  mat=tcrossprod(ma, mb)
  t1=sqrt(apply(ma, 1, crossprod))
  t2=sqrt(apply(mb, 1, crossprod))
  mat / outer(t1,t2)
}

#wrapper around the function        
cosineSimilarity <- function(index_patients,train_patients,K, rescale = FALSE){
  #handle NAs?
  #distance with any NAs = NAs
  index_patients = index_patients[,!colnames(index_patients) %in% "SepsisLabel"]
  train_patients = train_patients[,!colnames(train_patients) %in% "SepsisLabel"]
  
  if(rescale){
    tmp = rescalePredictors(train_patients,index_patients)
    index_patients =tmp$test
    train_patients = tmp$train
  }
  
  index_patients = as.matrix(index_patients)
  train_patients = as.matrix(train_patients)
  cosine = cos.sim(index_patients,train_patients)

  #order according to similarity
  cosineOrder = apply(cosine,1, order,decreasing = TRUE)
  topKindices = t(cosineOrder[1:K,])
  
  lapply(1:nrow(topKindices), function(i) topKindices[i,])
  
}




euclideanDist<- function(index_patients,train_patients,K, rescale = FALSE){
  require(pdist)
  index_patients = index_patients[,!colnames(index_patients) %in% "SepsisLabel"]
  train_patients = train_patients[,!colnames(train_patients) %in% "SepsisLabel"]
  
  if(rescale){
    tmp = rescalePredictors(train_patients,index_patients)
    index_patients =tmp$test
    train_patients = tmp$train
  }
  index_patients = as.matrix(index_patients)
  train_patients = as.matrix(train_patients)
  euclidean = pdist(index_patients,train_patients)
  euclidean = as.matrix(euclidean)

  #order according to similarity
  EuclideanOrder = apply(euclidean,1, order, decreasing = FALSE)

  topKindices = t(EuclideanOrder[1:K,])
  
  lapply(1:nrow(topKindices), function(i) topKindices[i,])
  
  
}


#test here

euc_rescale = euclideanDist(TestFold1,train_patients = TrainFold1,100, rescale = T)
euc_ = euclideanDist(TestFold1,train_patients = TrainFold1,100, rescale = F)



common = sapply(1:length(euc_rescale), function(x){sum(euc_rescale[[x]] %in% euc_[[x]]) })
mean(common) #how many neighbours do they have in common, 0.5%
8.287968/100
0.08287968
max(common)

cos_rescale = cosineSimilarity(TestFold1,train_patients = TrainFold1,100, rescale = T)
cos_ = cosineSimilarity(TestFold1,train_patients = TrainFold1,100, rescale = F)
sum(cos_[[2]] %in% cos_rescale[[2]])

common = sapply(1:length(cos_rescale), function(x){sum(cos_rescale[[x]] %in% cos_[[x]]) })
mean(common) #how many neighbours do they have in common, 0.5%
max(common)

common_euc = function(trainfold,testfold,K){
  euc_rescale = euclideanDist(testfold,train_patients = trainfold,K, rescale = T)
  euc_ = euclideanDist(testfold,train_patients = trainfold,K, rescale = F)
  common_patients = sapply(1:length(euc_rescale), function(x){sum(euc_rescale[[x]] %in% euc_[[x]]) })
  c(mean = mean(common_patients), sd= sd(common_patients),max = max(common_patients), n = length(common_patients))
}

common_cos  = function(trainfold,testfold,K){
  cos_rescale = cosineSimilarity(testfold,train_patients = trainfold,K, rescale = T)
  cos_ = cosineSimilarity(testfold,train_patients = trainfold,K, rescale = F)
  common_patients = sapply(1:length(cos_rescale), function(x){sum(cos_rescale[[x]] %in% cos_[[x]]) })
  c(mean = mean(common_patients), sd= sd(common_patients),max = max(common_patients), n = length(common_patients))
}

common_cos(TrainFold1,TestFold1,14000)


nSimilarsFull1 = c(50,100,200,500,1000,1500,3000,6000,9000,12000,15000)
nSimilarsFull2 = c(2000,4000,5000,7000,8000,10000,11000,13000,14000,16000,17000,18000)
nSimilars = c(nSimilarsFull1,nSimilarsFull2)
nSimilars = nSimilars[order(nSimilars)]
nSimilars

common_euc_nsimilar = sapply(nSimilars, function(K){
  common_euc(TrainFold1,TestFold1,K)})
colnames(common_euc_nsimilar) = nSimilars

common_cos_nsimilar =sapply(nSimilars, function(K){
  common_cos(TrainFold1,TestFold1,K)})
colnames(common_cos_nsimilar) = nSimilars

upper_euc = common_euc_nsimilar[1,] + 1.96*common_euc_nsimilar[2,]/sqrt(2028)
lower_euc = common_euc_nsimilar[1,] - 1.96*common_euc_nsimilar[2,]/sqrt(2028)

upper_cos = common_cos_nsimilar[1,] + 1.96*common_cos_nsimilar[2,]/sqrt(2028)
lower_cos = common_cos_nsimilar[1,] - 1.96*common_cos_nsimilar[2,]/sqrt(2028)

plot(nSimilars,common_euc_nsimilar[1,], xlab = "Number of Similar Patients", ylab = "Number of Shared Similar Patients",cex=0.7)
lines(nSimilars,common_euc_nsimilar[1,],lty = 1)
lines(nSimilars,upper_euc,lty = 2)
lines(nSimilars,lower_euc,lty = 2)

plot(nSimilars,common_euc_nsimilar[1,]/nSimilars*100, xlab = "Number of Similar Patients"
     , ylab = "Percentage of Shared Similar Patients",cex=0.7)
lines(nSimilars,common_euc_nsimilar[1,]/nSimilars*100)
lines(nSimilars,upper_euc/nSimilars*100,lty = 2)
lines(nSimilars,lower_euc/nSimilars*100,lty = 2)

plot(nSimilars,common_cos_nsimilar[1,], xlab = "Number of Shared Similar Patients", ylab = "Number of Shared Similar Patients")

setwd("/Users/jordyn/Documents/Health-DS-Essay/Paper-Latex/fig/")
png("Shared_patients_euc.png")
plot(nSimilars,common_euc_nsimilar[1,]/nSimilars*100, xlab = "Number of Similar Patients"
     , ylab = "Shared Similar Patients (%)",cex=0.7)
polygon(c(0,nSimilars,max(nSimilars)),c(0,common_euc_nsimilar[1,]/nSimilars*100,0),col="light blue" , border = NA)
points(nSimilars,common_euc_nsimilar[1,]/nSimilars*100,cex =0.7)
lines(nSimilars,common_euc_nsimilar[1,]/nSimilars*100)
lines(nSimilars,upper_euc/nSimilars*100,lty = 2)
lines(nSimilars,lower_euc/nSimilars*100,lty = 2)
dev.off()

png("Shared_patients_cos.png")
plot(nSimilars,common_cos_nsimilar[1,]/nSimilars*100, xlab = "Number of Similar Patients"
     , ylab = "Shared Similar Patients (%)",cex =0.7, xlim = c(-5, max(nSimilars)-5))

polygon(c(0,nSimilars,max(nSimilars)),c(0,common_cos_nsimilar[1,]/nSimilars*100,0),col="light blue" , border = NA)
points(nSimilars,common_cos_nsimilar[1,]/nSimilars*100,cex =0.7)
lines(nSimilars,common_cos_nsimilar[1,]/nSimilars*100)
lines(nSimilars,upper_cos/nSimilars*100,lty = 2)
lines(nSimilars,lower_cos/nSimilars*100,lty = 2)

dev.off()





#also table describing covariates

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/eda_notebook/")
list.files(pattern = "p*")

train_labCount_data <- read.table(file = "p000001.psv",header= T,sep = "|")

columns = colnames(train_labCount_data)


VitalDescriptors = c("Heart rate (beats per minute)","Pulse oximetry (%)", "Temperature (Deg C)","Systolic BP (mm Hg)"
                     ,"	Mean arterial pressure (mm Hg)","Diastolic BP (mm Hg)","Respiration rate (breaths per minute)"
                     ,"	End tidal carbon dioxide (mm Hg)")

LabDescriptors = c("Measure of excess bicarbonate (mmol/L)","Bicarbonate (mmol/L)","Fraction of inspired oxygen (%)",
                   "N/A","Partial pressure of carbon dioxide from arterial blood (mm Hg)","Oxygen saturation from arterial blood (%)"
                   ,"Aspartate transaminase (IU/L)","Blood urea nitrogen (mg/dL)","Alkaline phosphatase (IU/L)"
                   ,"(mg/dL)","	(mmol/L)","	(mg/dL)","Bilirubin direct (mg/dL)","Serum glucose (mg/dL)","Lactic acid (mg/dL)"
                   ,"(mmol/dL)", "(mg/dL)","(mmol/L)","Total bilirubin (mg/dL)","Troponin I (ng/mL)","Hematocrit (%)"
                   ,"Hemoglobin (g/dL)","partial thromboplastin time (seconds)","Leukocyte count (count*10^3/µL)"
                   ,"(mg/dL)","(count*$10^3$/µL)")

DemDescriptors = c("Years (100 for patients 90 or above)","Female (0) or Male (1)", "Administrative identifier for ICU unit (MICU)"
                , "Administrative identifier for ICU unit (SICU)", "Hours between hospital admit and ICU admit",
                "ICU length-of-stay (hours since ICU admit)")
SepsisDescriptor = "For sepsis patients, SepsisLabel is 1 if $t +6> t_sepsis$, 0 otherwise."


mat = cbind(columns,c(VitalDescriptors,LabDescriptors,DemDescriptors,SepsisDescriptor))

library(xtable)
xtable(mat)
