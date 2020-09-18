##model fitting

#ssh jlrwalto@fastlinux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R

library(caret)
#setwd("../../../Data/Built_Tables/Model_fit")
setwd("Data/Built_Tables/Model_fit")
train_vital_data <- readRDS("train_vital_input_data.RDS")
train_labCount_data <- readRDS("train_labCount_input_data.RDS")

set.seed(87)
train_indices = readRDS("train_indices.RDS")

#Initial Model Fitting with random Forest on both data sets
setwd("../../../R/Model_fit/rf")
rffitControl <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 20,
                              index = train_indices,
                              ## Estimate class probabilities
                              classProbs = TRUE)

tunegrid <- expand.grid(.mtry=round(sqrt(ncol(train_vital_data))))

rfFit_vital<- train(SepsisLabel~., data = train_vital_data,
               method = "rf",
               trControl = rffitControl
               , tuneGrid=tunegrid
               , metric = "Kappa"
               , ntree = 500)
saveRDS(rfFit_vital,"rf_fit_vital.RDS")

rfFit_vital
saveRDS(rfFit_vital,"rf_fit_vital.RDS")
#rfFit_vital = readRDS("rf_fit_vital.RDS")

jpeg("rf_fit_vital_errorplot.jpeg")
plot(rfFit_vital$finalModel)
dev.off()



tunegrid_labCount <- expand.grid(.mtry=round(sqrt(ncol(train_labCount_data))))

rfFit_labCount<- train(SepsisLabel~., data = train_labCount_data,
                    method = "rf",
                    trControl = rffitControl
                    , tuneGrid=tunegrid_labCount
                    , metric = "Kappa"
                    , ntree = 500)


rfFit_labCount
saveRDS(rfFit_labCount,"rf_fit_labcount.RDS")

jpeg("rf_fit_labCount_errorplot.jpeg")
plot(rfFit_labCount$finalModel)
dev.off()


#try again with down sampling

setwd("../../../Data/Built_Tables/Model_fit")
down_train_vital_data <- readRDS("down_train_vital_data.RDS")
down_train_labCount_data <- readRDS("down_train_labCount_data.RDS")

down_train_indices = readRDS("down_train_indices.RDS")

#Initial Model Fitting with random Forest on both data sets
setwd("../../../R/Model_fit/rf")
down_rffitControl <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 20,
                                  index = down_train_indices,
                                  ## Estimate class probabilities
                                  classProbs = TRUE)


tunegrid_vital <- expand.grid(.mtry=round(sqrt(ncol(down_train_vital_data))))


down_rfFit_vital<- train(SepsisLabel~., data = down_train_vital_data
                         , method = "rf"
                         , trControl = down_rffitControl
                         , tuneGrid = tunegrid_vital
                         , metric = "Kappa"
                         , ntree = 500)


down_rfFit_vital
saveRDS(down_rfFit_vital,"down_rf_fit_vital.RDS")


#model including labcounts as well
tunegrid_labCount <- expand.grid(.mtry=round(sqrt(ncol(down_train_vital_data))))

down_rfFit_labCounts<- train(SepsisLabel~., data = down_train_labCount_data
                             , method = "rf"
                             , trControl = down_rffitControl
                             , tuneGrid =tunegrid_labCount
                             , metric = "Kappa"
                             , ntree = 500)

down_rfFit_labCounts
saveRDS(down_rfFit_labCounts,"down_rf_fit_labCounts.RDS")

