setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit/Models/")
load("model_analysis.RData")
library(MLeval)

list.files(pattern = "*.RDS")
# [1] "lr_fit_labCounts_down.RDS"   "lr_fit_labCounts.RDS"        "lr_fit_vital_down.RDS"      
# [4] "lr_fit_vital.RDS"            "rf_fit_labcount_down100.RDS" "rf_fit_labcount_down200.RDS"
# [7] "rf_fit_labcount_down500.RDS" "rf_fit_labcount100.RDS"      "rf_fit_labcount200.RDS"     
# [10] "rf_fit_labcount500.RDS"      "rf_fit_vital_down100.RDS"    "rf_fit_vital_down200.RDS"   
# [13] "rf_fit_vital_down500.RDS"    "rf_fit_vital100.RDS"         "rf_fit_vital200.RDS"        
# [16] "rf_fit_vital500.RDS"

Model.list = lapply(list.files(pattern = "*.RDS"), readRDS)
names(Model.list) = gsub(".RDS","",list.files(pattern = "*.RDS"))

#first look at LR
LR_model.list = Model.list[grepl("lr",names(Model.list))]
LR_model.list[[1]]$results
performance_LR_best = sapply(LR_model.list, function(x){x$results})
performance_LR_best = t(performance_LR_best)

#best AUROC

performance_LR_best
#                       parameter ROC       Sens      Spec        ROCSD      SensSD       SpecSD     
# lr_fit_labCounts_down factor,1  0.6680996 0.6769233 0.5768536   0.0264677  0.01319258   0.04839727 
# lr_fit_labCounts      factor,1  0.6728766 0.9995782 0.004224645 0.02727858 0.000507221  0.005872962
# lr_fit_vital_down     factor,1  0.6250584 0.6144455 0.5666096   0.02872506 0.01356557   0.04831517 
# lr_fit_vital          factor,1  0.6288369 0.9999531 0           0.02890497 0.0001494011 0      

confusionMatrix(LR_model.list[[1]])
#it can be seen here that Sepsis negative is treated as the positive class
# Reference
# Prediction   Non.Sepsis Sepsis
# Non.Sepsis       64.1    2.2
# Sepsis           30.6    3.1
# 
# Accuracy (average) : 0.6716


# lr_fit_labCounts does best
roc.CI = sapply(1:4, function(x){ m = performance_LR_best[x,"ROC"][[1]]
s = performance_LR_best[x,"ROCSD"][[1]]
CI = round(c(lower = m - 1.96*s, upper = m+ 1.96*s),3)
paste0(round(m,3) ," [",CI[1],", ", CI[2],"]")})

roc.CI


RF_model.list = Model.list[grepl("rf",names(Model.list))]
RF_model.list[[1]]$bestTune[1,1]
performance_RF = sapply(RF_model.list, function(x){x$results[x$results$mtry== x$bestTune[1,1],]})
performance_RF = t(performance_RF)
#after tuning for ntree variable

performance_RF

performance_RF_best = performance_RF[c(3,6,9,12),]
performance_RF_best
#                         mtry ROC       Sens      Spec      ROCSD      SensSD       SpecSD    
# rf_fit_labcount_down500 3    0.6871446 0.6397802 0.6390113 0.02478687 0.01485812   0.0463519 
# rf_fit_labcount500      3    0.6775176 0.9999948 0         0.02408435 5.193879e-05 0         
# rf_fit_vital_down500    4    0.6387347 0.5990627 0.6043164 0.02418311 0.01619039   0.05171882
# rf_fit_vital500         3    0.6295795 0.999987  0         0.02610305 8.151065e-05 0         

 #rf_fit_labcount_down500 does the best

CI = function(perfgrid,metric,n){
  sapply(1:nrow(perfgrid), function(x){ m = perfgrid[x,metric][[1]]
  s = perfgrid[x,paste0(metric,"SD")][[1]]/sqrt(n)
  CI = round(c(lower = m - 1.96*s, upper = m+ 1.96*s),3)
  paste0(round(m,3) ," [",CI[1],", ", CI[2],"]")})
}

roc.CI  =CI(performance_RF_best,"ROC",200)
Sens.CI= CI(performance_RF_best,"Spec",200)  #switch labels here to accomadate for switchd
Spec.CI= CI(performance_RF_best,"Sens",200)


CI(performance_RF[1:2,],"ROC",200)
##############Make Tables for Latexxx
#Calculate AUPRC

aucpr = lapply(Model.list, function(x){eval_model = evalm(x,showplots = F)
eval_model$stdres$`Group 1`[rownames(eval_model$stdres$`Group 1`) %in% "AUC-PR",1]                                 
})

names(aucpr) <-names(Model.list)
aucpr = simplify2array(aucpr)
aucpr.RF = aucpr[c(7,10,13,16)]


##############Make Tables for Latexxx
CI.RF  = cbind(AUROC = roc.CI, Sens = Sens.CI,Spec = Spec.CI, AUPRC = aucpr.RF)
rownames(CI.RF)<- c("Lab Counts, Down", "Lab Counts","Only Vitals, Down","Only Vitals")
xtable(CI.RF)


performance_LR_best
roc.CI  =CI(performance_LR_best,"ROC",200)
Sens.CI= CI(performance_LR_best,"Spec",200) #switch labels here to accomadate for switchd
Spec.CI= CI(performance_LR_best,"Sens",200)
aucpr.LR = aucpr[1:4]

CI.LR = cbind(AUROC = roc.CI, Sens = Sens.CI,Spec = Spec.CI, AUPRC = aucpr.LR)
rownames(CI.LR)<- c("Lab Counts, Down", "Lab Counts","Only Vitals, Down","Only Vitals")
xtable(CI.LR)


############## rfTree error plots


plotRF<- function(rfmodel,maint="",ylim = c(0,1), legendx = 200, legendy = 0.8){
  ntree = rfmodel$ntree
  plot(1:ntree,rfmodel$err.rate[,1],type = "l",ylim = ylim,xlab = "number of trees",ylab = "Error",main = maint)
  lines(1:ntree,rfmodel$err.rate[,2], col = "blue")
  lines(1:ntree,rfmodel$err.rate[,3], col = "red")
  legend(legendx, legendy, legend=c("OOB", "Sepsis", "Non-Sepsis"),
         col=c("black","red", "blue"), lty=1)
  
}
names(RF_model.list)

setwd("/Users/jordyn/Documents/Health-DS-Essay/Paper-Latex/fig/")
png("rf_labcount_down500_error.png", width = 6.4, height = 6.4, units = 'in', res = 800)
plotRF(RF_model.list[[3]]$finalModel,legendx = 360, legendy = 0.9)
dev.off()
png("rf_labcount500_error.png", width = 6.4, height = 6.4, units = 'in', res = 800)
plotRF(RF_model.list[[6]]$finalModel,legendx = 360, legendy = 0.9)
dev.off()

png("rf_vital_down_error.png", width = 6.4, height = 6.4, units = 'in', res = 800)
plotRF(RF_model.list[[9]]$finalModel,legendx = 360, legendy = 0.9)
dev.off()
png("rf_vital_error.png", width = 6.4, height = 6.4, units = 'in', res = 800)
plotRF(RF_model.list[[12]]$finalModel,legendx = 360, legendy = 0.9)
dev.off()


############################### Variable Importance

#variable importance from the best LR model and best RF model
# rf_fit_labcount_down500
# lr_fit_labCounts

best_models.list = Model.list[names(Model.list) %in% c("rf_fit_labcount_down500","lr_fit_labCounts")]
names(best_models.list)
#"lr_fit_labCounts"        "rf_fit_labcount_down500"
varImp.list = lapply(best_models.list, varImp)
varImp.mat = cbind(varImp.list[[1]]$importance,varImp.list[[2]]$importance)
colnames(varImp.mat) <- names(best_models.list)
varImp.mat
rownames(varImp.mat)[varImp.mat[,1] >50 & varImp.mat[,2]>50]

rownames(varImp.mat)[varImp.mat[,1] >20 & varImp.mat[,2]>20]



png("lr_fit_labCounts_VI.png", width = 6.4, height = 6.4, units = 'in', res = 800)
plot(varImp.list[[1]],20)
dev.off()

png("rf_fit_labcount_down500_VI.png", width = 6.4, height = 6.4, units = 'in', res = 800)
plot(varImp.list[[2]],20)
dev.off()

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit/Models/")
save.image("model_analysis.RData")
load("model_analysis.RData")
