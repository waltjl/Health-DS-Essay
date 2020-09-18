#ssh jlrwalto@biglinux.math.uwaterloo.ca
#ssh jlrwalto@fastlinux.math.uwaterloo.ca
#ssh jlrwalto@linux.math.uwaterloo.ca

#cd User/Documents/Health-DS-Essay/

#R
set.seed(908083)




library(caret)
library(parallel)

num.cores = detectCores()-3
#setwd("../../Data/Built_Tables/Model_fit")
setwd("Data/Built_Tables/Model_fit")

#all data has labcounts
train_data <- readRDS("train_labCount_input_data.RDS")



train_indices = readRDS("train_indices.RDS")
min(sapply(train_indices,length))
#18252

train_down_indices = lapply(train_indices, function(ind){tmp = downSample(ind, train_data[ind,"SepsisLabel"]
                                                                          , list = TRUE)
                            tmp$x[,1]})
min(sapply(train_down_indices,length))
#highest value for nsimilar in the downsampling models
#1940


nSimilars = c(50,100,200,500,1000,1500)

nSimilars1=c(50, 100,150, 200,250, 300,350, 400,450,500,600,700,1000)


tunegrid_rf1 <-  expand.grid(mtry=3,ntree = 200,nSimilar=nSimilars1)
tunegrid_lr1 <- expand.grid(nSimilar=nSimilars1)

tunegrid_rf <-  expand.grid(mtry=3,ntree = 200,nSimilar=nSimilars)
tunegrid_lr <- expand.grid(nSimilar=nSimilars)

nSimilarsFull = c(50, 100,150, 200,250, 300,350, 400,450,500,600,700,1000,seq(1500,18000,500))
nSimilarsFull1 = c(50,100,200,500,1000,1500,3000,6000,9000,12000,15000)
nSimilarsFull2 = c(2000,4000,5000,7000,8000,10000,11000,13000,14000,16000,17000,18000)


tunegrid_rfFull <-  expand.grid(mtry=3,ntree = 200,nSimilar=nSimilarsFull)
tunegrid_rfFull1 <-  expand.grid(mtry=3,ntree = 200,nSimilar=nSimilarsFull1)
tunegrid_rfFull2 <-  expand.grid(mtry=3,ntree = 200,nSimilar=nSimilarsFull2)

tunegrid_lrFull <- expand.grid(nSimilar=nSimilarsFull)
tunegrid_lrFull1 <- expand.grid(nSimilar=nSimilarsFull1)
tunegrid_lrFull2 <- expand.grid(nSimilar=nSimilarsFull2)

#for each Logistic Regression and Random forest, we need cosine and 2nd distance, one with no sampling and downsampling

setwd("../../../R/Model_fit_Sim")
lr_ps_fit_down0 = tune_lr_ps(train_data,similarity_metric = "cosine",indices=train_indices,K=10,repeats=20
                      , tuneGrid =tunegrid_lr,metric = "AUROC", doparallel = TRUE
                      , ncore = num.cores , sampling = "down", saveGrid = "lr_ps_fit_down0_progress.RDS")
  
saveRDS(lr_ps_fit_down0,"lr_ps_fit_down0.RDS") 
  
  
rf_ps_fit_down0test = tune_rf_ps(train_data,similarity_metric = "cosine",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rf
                       ,metric = "AUROC"
                      , doparallel = TRUE, ncore = num.cores , sampling = "down", saveGrid = "rf_ps_fit_down0test_progress.RDS")

saveRDS(rf_ps_fit_down0test,"rf_ps_fit_down0test.RDS")


##without downsampling

lr_ps_fit1 = tune_lr_ps(train_data,similarity_metric = "cosine",indices=train_indices,K=10,repeats=20
                       , tuneGrid =tunegrid_lrFull1,metric = "AUROC", doparallel = TRUE
                       , ncore = num.cores, saveGrid = "lr_ps_fit1_progress2.RDS" )


saveRDS(lr_ps_fit1,"lr_ps_fit1.RDS")


rf_ps_fit1 = tune_rf_ps(train_data,similarity_metric = "cosine",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rfFull1
                       ,metric = "AUROC"
                       , doparallel = TRUE, ncore = num.cores, saveGrid = "rf_ps_fit1_progress.RDS" )

saveRDS(rf_ps_fit1,"rf_ps_fit1.RDS")





tunegrid_test <-  expand.grid(mtry=3,ntree = 200,nSimilar=500)

system.time({rf_ps_fit_test =crossval_rf_ps(train_data,mtry=3,ntree = 200,nSimilar = 500,similarity_metric = "euclidean"
                                         ,indices=train_indices,folds=10
                                         ,repeats=2, doparallel = TRUE, ncore = num.cores, downSamples = NULL, saveGrid = "rf_ps_fit_test_prog.RDS")}
  
)
#69 minutes
rf_ps_fit_test




setwd("../../../R/Model_fit_Sim")
lr_ps_fit_euc_down0 = tune_lr_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20
                             , tuneGrid =tunegrid_lr,metric = "AUROC", doparallel = TRUE
                             , ncore = num.cores , sampling = "down", saveGrid = "lr_ps_fit_euc_down0_progress.RDS")

saveRDS(lr_ps_fit_euc_down0,"lr_ps_fit_euc_down0.RDS") 

# 
# lr_ps_fit_euc_downtest = tune_lr_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20
#                                  , tuneGrid =expand.grid(nSimilar=50),metric = "AUROC", doparallel = TRUE
#                                  , ncore = num.cores , sampling = "down", saveGrid = "lr_ps_fit_euc_down0_progress.RDS")
# 
# saveRDS(lr_ps_fit_euc_down0,"lr_ps_fit_euc_down0.RDS") 
# 
rf_ps_fit_euc_down0 = tune_rf_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rf
                             ,metric = "AUROC"
                             , doparallel = TRUE, ncore = num.cores , sampling = "down", saveGrid = "rf_ps_fit_euc_down0_progress.RDS")

saveRDS(rf_ps_fit_euc_down0,"rf_ps_fit_euc_down0.RDS")


##without downsampling

lr_ps_fit_euc1 = tune_lr_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20
                        , tuneGrid =tunegrid_lrFull1,metric = "AUROC", doparallel = TRUE
                        , ncore = num.cores, saveGrid = "lr_ps_fit_euc1_progress.RDS" )


saveRDS(lr_ps_fit_euc1,"lr_ps_fit_euc1.RDS")


rf_ps_fit_euc1 = tune_rf_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rfFull1
                        ,metric = "AUROC"
                        , doparallel = TRUE, ncore = num.cores, saveGrid = "rf_ps_fit_euc1_progress.RDS" )

saveRDS(rf_ps_fit_euc1,"rf_ps_fit_euc1.RDS")



#filling out more nSimilar values



lr_ps_fit2 = tune_lr_ps(train_data,similarity_metric = "cosine",indices=train_indices,K=10,repeats=20
                        , tuneGrid =tunegrid_lrFull2,metric = "AUROC", doparallel = TRUE
                        , ncore = num.cores, saveGrid = "lr_ps_fit2_progress.RDS" )


saveRDS(lr_ps_fit2,"lr_ps_fit2.RDS")


rf_ps_fit2 = tune_rf_ps(train_data,similarity_metric = "cosine",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rfFull2
                        ,metric = "AUROC"
                        , doparallel = TRUE, ncore = num.cores, saveGrid = "rf_ps_fit2_progress.RDS" )

saveRDS(rf_ps_fit2,"rf_ps_fit2.RDS")



lr_ps_fit_euc2 = tune_lr_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20
                            , tuneGrid =tunegrid_lrFull2,metric = "AUROC", doparallel = TRUE
                            , ncore = num.cores, saveGrid = "lr_ps_fit_euc2_progress.RDS" )


saveRDS(lr_ps_fit_euc2,"lr_ps_fit_euc2.RDS")


rf_ps_fit_euc2 = tune_rf_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rfFull2
                            ,metric = "AUROC"
                            , doparallel = TRUE, ncore = num.cores, saveGrid = "rf_ps_fit_euc2_progress.RDS" )

saveRDS(rf_ps_fit_euc2,"rf_ps_fit_euc2.RDS")


####issues with rf progress saves, to run if needed

#rf_ps_fit1
#rf_ps_fit_euc1
#rf_ps_fit_euc2
#rf_ps_fit2

setwd("rf")

for( i in 1:nrow(tunegrid_rfFull2)){
  rf_ps_fit_euc2 = tune_rf_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rfFull2[i,]
                              ,metric = "AUROC"
                              , doparallel = TRUE, ncore = num.cores, saveGrid = "rf_ps_fit_euc2_tmp.RDS" )
  saveRDS(rf_ps_fit_euc2,paste0("rf_ps_fit_euc2",i,".RDS"))
}


for( i in 1:nrow(tunegrid_rfFull2)){
  rf_ps_fit2 = tune_rf_ps(train_data,similarity_metric = "cosine",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rfFull2[i,]
                          ,metric = "AUROC"
                          , doparallel = TRUE, ncore = num.cores, saveGrid = "rf_ps_fit2_progress.RDS" )
  
  saveRDS(rf_ps_fit2,pastw0("rf_ps_fit2",i,".RDS"))
  
  
}

for( i in 1:nrow(tunegrid_rfFull1)){
  rf_ps_fit1 = tune_rf_ps(train_data,similarity_metric = "cosine",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rfFull1[i,]
                          ,metric = "AUROC"
                          , doparallel = TRUE, ncore = num.cores, saveGrid = "rf_ps_fit1_progress.RDS" )
  
  saveRDS(rf_ps_fit1,paste0("rf_ps_fit1",i,".RDS"))
  
}

# for( i in 1:nrow(tunegrid_rfFull1)){
#   rf_ps_fit_euc1 = tune_rf_ps(train_data,similarity_metric = "euclidean",indices=train_indices,K=10,repeats=20, tuneGrid =tunegrid_rfFull1[i,]
#                               ,metric = "AUROC"
#                               , doparallel = TRUE, ncore = num.cores, saveGrid = "rf_ps_fit_euc1_progress.RDS" )
#   
#   saveRDS(rf_ps_fit_euc1,paste0("rf_ps_fit_euc1",i,".RDS"))
#   
# }


