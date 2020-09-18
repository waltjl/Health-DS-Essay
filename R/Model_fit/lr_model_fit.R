##model fitting

#ssh jlrwalto@fastlinux.math.uwaterloo.ca
#cd User/Documents/Health-DS-Essay/

#R

library(caret)
setwd("Data/Built_Tables/Model_fit")
train_vital_data <- readRDS("train_vital_input_data.RDS")
train_labCount_data <- readRDS("train_labCount_input_data.RDS")

set.seed(87)
train_indices = readRDS("train_indices.RDS")

#Initial Model Fitting with random Forest on both data sets
setwd("../../../R/Model_fit/lr")
lrfitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 20,
                             index = train_indices,
                             ## Estimate class probabilities
                             classProbs = TRUE)


lrFit_vital<- train(SepsisLabel~., data = train_vital_data
                    , method = "glm"
                    , family = "binomial"
                    , trControl = lrfitControl
                    , metric = "Kappa")


lrFit_vital
saveRDS(lrFit_vital,"lr_fit_vital.RDS")
lrFit_vital = readRDS("lr_fit_vital.RDS")

#model including labcounts as well
lrFit_labCounts<- train(SepsisLabel~., data = train_labCount_data
                    , method = "glm"
                    , family = "binomial"
                    , trControl = lrfitControl
                    , metric = "Kappa")

lrFit_labCounts
saveRDS(lrFit_labCounts,"lr_fit_labCounts.RDS")
lrFit_labCounts = readRDS("lr_fit_labCounts.RDS")

#try again with down sampling

setwd("../../../Data/Built_Tables/Model_fit")
down_train_vital_data <- readRDS("down_train_vital_data.RDS")
down_train_labCount_data <- readRDS("down_train_labCount_data.RDS")

down_train_indices = readRDS("down_train_indices.RDS")

#Initial Model Fitting with random Forest on both data sets
setwd("../../../R/Model_fit/lr")
down_lrfitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 20,
                             index = down_train_indices,
                             ## Estimate class probabilities
                             classProbs = TRUE)


down_lrFit_vital<- train(SepsisLabel~., data = down_train_vital_data
                    , method = "glm"
                    , family = "binomial"
                    , trControl = down_lrfitControl
                    , metric = "Kappa")


down_lrFit_vital
saveRDS(down_lrFit_vital,"down_lr_fit_vital.RDS")
#lrFit_vital = readRDS("lr_fit_vital.RDS")

#model including labcounts as well
down_lrFit_labCounts<- train(SepsisLabel~., data = down_train_labCount_data
                        , method = "glm"
                        , family = "binomial"
                        , trControl = down_lrfitControl
                        , metric = "Kappa")

down_lrFit_labCounts
saveRDS(down_lrFit_labCounts,"down_lr_fit_labCounts.RDS")
#lrFit_labCounts = readRDS("lr_fit_labCounts.RDS")
