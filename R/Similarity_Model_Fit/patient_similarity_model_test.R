#ssh jlrwalto@linux.math.uwaterloo.ca

#cd User/Documents/Health-DS-Essay/
  
  #R
set.seed(3)
runif(1)

library(caret)
library(parallel)

num.cores = detectCores()-2

setwd("Data/Built_Tables/Model_fit")
#setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/")
train_data <- readRDS("down_train_labCount_data.RDS")

train_indices <- readRDS("down_train_indices.RDS")



#test this function
train_sample = train_data[train_indices[[1]],]
test_sample = train_data[-c(train_indices[[1]]),]

train_sample1 = train_sample[,c(1,2,3,4,5,22,40)]
test_sample1 = test_sample[,c(1,2,3,4,5,22,40)]

cosine_test = cosineSimilarity(test_sample1[1,],train_sample1,K=100)
cosine_test

euc_test = euclideanDist(test_sample1[1,],train_sample1,K=100)
sum(euc_test[[1]] %in%cosine_test[[1]])

train_sample2 = train_sample[,c(1,2,3,4,5,22)]
test_sample2 = test_sample[,c(1,2,3,4,5,22)]
tmp =pdist(test_sample2[1,],train_sample2)

dist = as.matrix(tmp)
dim(dist)

dist[1:6,1:6]

dist1 =cos.sim(as.matrix(test_sample2[1,]),as.matrix(train_sample2))
dim(dist1)
dist1[1,1:10]

#test here, how to get auroc, sens,spec,precision,recall
pred = lr_ps_predict(train_sample1,test_sample1,K=100,"euclidean")
pred

system.time(rf_ps_predict(train_sample1,test_sample1,K=100,mtry = 6))

rf_ps_predict(train_sample1,test_sample1[c(1,2,4),],K=100,mtry = 6,"euclidean")

#2          4          8 
#Non.Sepsis Non.Sepsis Non.Sepsis 
#Levels: Non.Sepsis Sepsis

system.time(rf_ps_predict_parallel(train_sample1,test_sample1,K=100,mtry = 6,ncore =2))

rf_ps_predict_parallel(train_sample1,test_sample1[c(1,2,4),],K=100,mtry = 6,ncore =2,"euclidean")

lr_ps_predict_parallel(train_sample1,test_sample1[c(1,2,4),],K=100,ncore =2,"euclidean")

test_sample1$SepsisLabel
data = data.frame(pred = pred,obs = test_sample1$SepsisLabel)
data1 = data.frame(pred = pred,obs = ifelse(test_sample1$SepsisLabel=="Sepsis",1,0))


fastAUC(pred,ifelse(test_sample1$SepsisLabel=="Sepsis",1,0))
fastROC(pred,ifelse(test_sample1$SepsisLabel=="Sepsis",1,0))

cf_mat =confusionMatrix(factor(ifelse(pred>0.5,"Sepsis","Non.Sepsis")), test_sample1$SepsisLabel, positive="Sepsis") 
cf_mat$byClass

c(AUROC = fastAUC(pred,ifelse(test_sample1$SepsisLabel=="Sepsis",1,0)),cf_mat$byClass[1:2])
# AUROC Sensitivity Specificity 
# 0.5781036   0.5833333   0.5277778



data1 = data.frame(pred = factor(ifelse(pred>0.5,"Sepsis","Non.Sepsis")),obs = test_sample1$SepsisLabel
                   ,Sepsis = pred, Non.Sepsis = 1-pred) 



prSummary(data1,lev = c("Sepsis","Non.Sepsis"))

auroc.fun(data1)

system.time(twoClassSummary(data1,lev = c("Sepsis","Non.Sepsis")))

#implement it for K fold CV

cv_results = crossval_rf_ps(train_data,mtry=5,ntree = 100,nSimilar = 100,similarity_metric = "cosine"
                           ,indices=train_indices[1:3],folds=10
                           ,repeats=2, doparallel = TRUE,ncore =2)

cv_lr_results = crossval_lr_ps(train_data,nSimilar = 100,similarity_metric = "euclidean"
                            ,indices=train_indices[1:3],folds=10
                            ,repeats=2, doparallel = TRUE,ncore =2)

cv_results$predictions
cv_lr_results$predictions

perf = cv_lr_results$performance
perf_vec = c(perf)
names(perf_vec) = c(paste0(rownames(perf),".MEAN"),paste0(rownames(perf),".SD"))
#              MEAN          SD
# AUROC     0.07221685 0.006885386
# Sens      0.85945017 0.019596670
# Spec      0.85120275 0.002594445
# AUPRC     0.81836361 0.002853694
# Precision 0.85236327 0.005051065
# Recall    0.85945017 0.019596670
# F         0.85585136 0.012254990


tunegrid <- expand.grid(mtry=round(sqrt(ncol(train_data)-1)),ntree = 100,nSimilar=100)
tunegrid
# mtry ntree nSimilar
# 1    6   100      100
# 2    6   100      200

tunegrid <- expand.grid(mtry=round(sqrt(ncol(train_data)-1)),ntree = 100,nSimilar=c(100,200))
tunegrid

#test rf tuning
test_tune = tune_rf_ps(train_data,similarity_metric = "cosine",indices=train_indices[1:2],K=10,repeats=2
                       , tuneGrid =tunegrid,metric = "AUROC", doparallel = TRUE,ncore = 2)


tunegrid_lr <- expand.grid(nSimilar=c(100,1000))

test_tune_lr = tune_lr_ps(train_data,similarity_metric = "cosine",indices=train_indices[1:2],K=10,repeats=20
                       , tuneGrid =tunegrid_lr,metric = "AUROC", doparallel = TRUE,ncore = 2)

test_tune_lr$performance_grid



###add in downsampling within resamples
train_indices[[1]]
tmp = downSample(train_indices[[1]], train_data[train_indices[[1]],"SepsisLabel"], list = TRUE)
length(tmp$x[,1])

train_down_indices = lapply(train_indices, function(ind){tmp = downSample(ind, train_data[ind,"SepsisLabel"]
                                                                          , list = TRUE)
                                                          tmp$x[,1]})
max(sapply(train_down_indices,length))


cv_results = crossval_rf_ps(train_data,mtry=5,ntree = 100,nSimilar = 100,similarity_metric = "cosine"
                            ,indices=train_indices[1:3],folds=10
                            ,repeats=2, doparallel = TRUE,ncore =2, downSamples = train_down_indices[1:3])

cv_lr_results = crossval_lr_ps(train_data,nSimilar = 100,similarity_metric = "cosine"
                               ,indices=train_indices[1:3],folds=10
                               ,repeats=2, doparallel = TRUE,ncore =2, downSamples = train_down_indices[1:3])


cv_lr_results$performance
cv_results$performance

test_tune_lr = tune_lr_ps(train_data,similarity_metric = "cosine",indices=train_indices[1:2],K=10,repeats=20
                          , tuneGrid =tunegrid_lr,metric = "AUROC", doparallel = TRUE,ncore = 2,sampling = "down")

test_tune = tune_rf_ps(train_data,similarity_metric = "cosine",indices=train_indices[1:2],K=10,repeats=2
                       , tuneGrid =tunegrid,metric = "AUROC", doparallel = TRUE,ncore = 2,sampling = "down")


pred = c(0.2,0.3)
obs = factor(c("Sepsis","Non.Sepsis"))

factor(c("Non.Sepsis","Sepsis"), levels = c("Sepsis","Non.Sepsis"))

pred_ref = data.frame(pred = factor(ifelse(pred>0.5,"Sepsis","Non.Sepsis")),obs = obs
                      ,Sepsis = pred, Non.Sepsis = 1-pred) 

#calculate AUROC,AUPRC, perf metric
AUPRC = prSummary(pred_ref,lev = c("Sepsis","Non.Sepsis"))
names(AUPRC)[1] <- c("AUPRC")
AUROC = twoClassSummary(pred_ref,lev = c("Sepsis","Non.Sepsis"))
names(AUROC)[1] <- c("AUROC")


