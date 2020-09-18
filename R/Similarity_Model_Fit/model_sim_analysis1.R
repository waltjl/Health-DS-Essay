setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/")
library(dplyr)


setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/lr_ps/")
list.files(pattern = "*.RDS")
LR_cosine_norm.list = lapply(list.files(pattern = "*.RDS"), readRDS)

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/lr_ps_down/")
list.files(pattern = "*.RDS")
LR_cosine_down.list = lapply("lr_ps_fit_down0.RDS", readRDS)

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/lr_ps_euc/")
list.files(pattern = "*.RDS")
LR_euc_norm.list = lapply(list.files(pattern = "*.RDS"), readRDS)

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/lr_ps_euc_down/")
list.files(pattern = "*.RDS")
LR_euc_down.list = lapply(list.files(pattern = "*.RDS"), readRDS)


setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/rf_ps/")
list.files(pattern = "*.RDS")
RF_cosine_norm.list = lapply(list.files(pattern = "*.RDS"), readRDS)

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/rf_ps_down/")
list.files(pattern = "*.RDS")
RF_cosine_down.list = lapply(c("rf_ps_fit_down0test.RDS"), readRDS)

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/rf_ps_euc/")
list.files(pattern = "*.RDS")
RF_euc_norm.list = lapply(list.files(pattern = "*.RDS"), readRDS)

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/rf_ps_euc_down/")
list.files(pattern = "*.RDS")
RF_euc_down.list = lapply(list.files(pattern = "*.RDS"), readRDS)





###combine all the grids
buildPerfGrid = function(list){
  grid.list = lapply(list, function (x){
    if(is.list(x)){
      x$performance_grid
    } else{
      x
    }
  })
  
  grid = do.call(rbind, grid.list)
  grid = grid[order(grid[,"nSimilar"]),]
  as.data.frame(grid)
}

LR_euc_norm = buildPerfGrid(LR_euc_norm.list)
LR_euc_norm = distinct(LR_euc_norm)

LR_euc_down = buildPerfGrid(LR_euc_down.list)
LR_cosine_down = buildPerfGrid(LR_cosine_down.list)
LR_cosine_norm = buildPerfGrid(LR_cosine_norm.list)
LR_cosine_norm = distinct(LR_cosine_norm)

#readRDS("lr_ps_fit1_progress2.RDS")

RF_euc_norm = buildPerfGrid(RF_euc_norm.list)
RF_euc_down = buildPerfGrid(RF_euc_down.list)
RF_cosine_down = buildPerfGrid(RF_cosine_down.list)
RF_cosine_norm = buildPerfGrid(RF_cosine_norm.list)





###make the plots

plotAUROC <- function(pgrid,ylim= c(0,1),xlim = c(min(pgrid[,"nSimilar"]),max(pgrid[,"nSimilar"])), add= FALSE, colour = "black"){
  AUROC.mean = pgrid[,"AUROC.MEAN"]
  AUROC.SD = pgrid[,"AUROC.SD"]
  upper = AUROC.mean + 1.96*AUROC.SD/sqrt(200)
  lower = AUROC.mean - 1.96*AUROC.SD/sqrt(200)
  nSimilars =pgrid[,"nSimilar"]
  if(!add){
    plot(nSimilars, AUROC.mean, xlim = xlim, ylim = ylim, type = "l", xlab = "Number of Similar Patients", ylab="AUROC", col = colour)
    points(nSimilars, AUROC.mean,pch = 19,cex=0.7, col = colour)
  } else{
    lines(nSimilars, AUROC.mean, col = colour)
    points(nSimilars, AUROC.mean,pch = 19,cex=0.7, col = colour)
  }
  
  lines(nSimilars, upper,lty = 2, col = colour)
  lines(nSimilars,lower,lty =2, col = colour)
}


AUROC.CI <- function(pgrid,ylim= c(0,1),xlim = c(min(pgrid[,"nSimilar"]),max(pgrid[,"nSimilar"])), add= FALSE, colour = "black"){
  nSimilars =pgrid[,"nSimilar"]
  AUROC.mean = pgrid[,"AUROC.MEAN"]
  AUROC.SD = pgrid[,"AUROC.SD"]
  upper = AUROC.mean + 1.96*AUROC.SD/sqrt(200)
  lower = AUROC.mean - 1.96*AUROC.SD/sqrt(200)
  
  rbind(nSimilars = nSimilars,Mean = AUROC.mean, upper95 = upper,lower95 = lower)
}


#change this tomorrow
AUPRC.CI <- function(pgrid,ylim= c(0,1),xlim = c(min(pgrid[,"nSimilar"]),max(pgrid[,"nSimilar"])), add= FALSE, colour = "black"){
  nSimilars =pgrid[,"nSimilar"]
  AUROC.mean = pgrid[,"AUROC.MEAN"]
  AUROC.SD = pgrid[,"AUROC.SD"]
  upper = AUROC.mean + 1.96*AUROC.SD/sqrt(200)
  lower = AUROC.mean - 1.96*AUROC.SD/sqrt(200)
  
  rbind(nSimilars = nSimilars,Mean = AUROC.mean, upper95 = upper,lower95 = lower)
}


setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit/Models/tuning/")
list.files(pattern ="*.RDS")
#for comparison to best models
LRBest = readRDS("lr_fit_labCounts.RDS")
LRBest$results["ROC"]

LR_downBest = readRDS("lr_fit_labCounts_down.RDS")
LR_downBest$results

RF200 = readRDS("rf_fit_labcount200.RDS")
RFBEST = readRDS("rf_fit_labcount500.RDS")

RF_down200 = readRDS("rf_fit_labcount_down200.RDS")
RF_downBEST = readRDS("rf_fit_labcount_down500.RDS")

aurpc.RF200 = lapply(list(RF200,RF_down200), function(x){eval_model = evalm(x,showplots = F)
                                            eval_model$stdres                              
})

auprc.RF200 = sapply(aurpc.RF200, function(x) x$`Group 1`["AUC-PR",1])
names(auprc.RF200 ) = c("RF200","RF_down200")



setwd("/Users/jordyn/Documents/Health-DS-Essay/Paper-Latex/fig/")
#downsample models first


#LR
plotAUROC(LR_cosine_down,c(0.5,0.7),colour = "red")
addPoplevel_lines(LR_downBest,"black")
plotAUROC(LR_euc_down,add=TRUE,col = "deeppink4")
legend(900,0.6,legend = c("Population LR","Similarity Cosine LR","Similarity Euc LR","95% CI")
       ,col = c("black","red","deeppink4","black"),lty =c(1,1,1,2))

#RF
plotAUROC(RF_cosine_down,c(0.5,0.7),colour = "red")
addPoplevel_lines(RF_downBEST,"black")
plotAUROC(LR_euc_down,add=TRUE,col = "deeppink4")
legend(900,0.6,legend = c("Population LR","Similarity Cosine LR","Similarity Euc LR","95% CI")
       ,col = c("black","red","deeppink4","black"),lty =c(1,1,1,2))



png("LR_cosine_sim_down.png", width = 6.4, height = 6.4, units = 'in', res = 1000)
plotAUROC(LR_cosine_down,c(0.53,0.68))
abline(h= LR_downBest$results["ROC"],col ="red")
abline(h = LR_downBest$results["ROC"] + 1.96*LR_downBest$results["ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h = LR_downBest$results["ROC"] - 1.96*LR_downBest$results["ROCSD"]/sqrt(200) ,col = "red",lty= 2)
legend(1000,0.6,legend = c("Population LR","Similarity LR","95% CI"),col = c("red","black","black"),lty =c(1,1,2))
dev.off()
png("LR_euc_sim_down.png", width = 6.4, height = 6.4, units = 'in', res = 1000)
plotAUROC(LR_euc_down,c(0.5,0.7))
abline(h= LR_downBest$results["ROC"],col ="red")
abline(h = LR_downBest$results["ROC"] + 1.96*LR_downBest$results["ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h = LR_downBest$results["ROC"] - 1.96*LR_downBest$results["ROCSD"]/sqrt(200) ,col = "red",lty= 2)
legend(1000,0.6,legend = c("Population LR","Similarity LR","95% CI"),col = c("red","black","black"),lty =c(1,1,2))
dev.off()
png("RF_cos_sim_down.png", width = 6.4, height = 6.4, units = 'in', res = 1000)
plotAUROC(RF_cosine_down,c(0.6,0.7))
abline(h= RF_down200$results[1,"ROC"],col ="red")
abline(h = RF_down200$results[1,"ROC"] + 1.96*RF_down200$results[1,"ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h = RF_down200$results[1,"ROC"] - 1.96*RF_down200$results[1,"ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h= RF_downBEST$results[1,"ROC"],col ="blue")
abline(h = RF_downBEST$results[1,"ROC"] + 1.96*RF_downBEST$results[1,"ROCSD"]/sqrt(200) ,col = "blue",lty= 2)
abline(h = RF_downBEST$results[1,"ROC"] - 1.96*RF_downBEST$results[1,"ROCSD"]/sqrt(200) ,col = "blue",lty= 2)
legend(600,0.64,legend = c("Pop RF, 200 trees","Sim RF, 200 trees","Pop RF, 500 trees","95% CI"),col = c("red","black","blue","black"),lty =c(1,1,1,2))
dev.off()


png("RF_euc_sim_down.png", width = 6.4, height = 6.4, units = 'in', res = 1000)
plotAUROC(RF_euc_down,c(0.6,0.7) )
abline(h= RF_down200$results[1,"ROC"],col ="red")
abline(h = RF_down200$results[1,"ROC"] + 1.96*RF_down200$results[1,"ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h = RF_down200$results[1,"ROC"] - 1.96*RF_down200$results[1,"ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h= RF_downBEST$results[1,"ROC"],col ="blue")
abline(h = RF_downBEST$results[1,"ROC"] + 1.96*RF_downBEST$results[1,"ROCSD"]/sqrt(200) ,col = "blue",lty= 2)
abline(h = RF_downBEST$results[1,"ROC"] - 1.96*RF_downBEST$results[1,"ROCSD"]/sqrt(200) ,col = "blue",lty= 2)
dev.off()





#then normal models which are complete

png("LR_euc_sim.png", width = 6.4, height = 6.4, units = 'in', res = 1000)
plotAUROC(LR_euc_norm,c(0.5,0.7))
abline(h = LRBest$results["ROC"],col = "red")
abline(h = LRBest$results["ROC"] + 1.96*LRBest$results["ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h = LRBest$results["ROC"] - 1.96*LRBest$results["ROCSD"]/sqrt(200) ,col = "red",lty= 2)
plotAUROC(LR_euc_down, add = TRUE,col ="deeppink4")
legend(7000,0.6,legend = c("Population LR","Similarity LR","Similarity, down LR","95% CI"),col = c("red","black", "deeppink4","black"),lty =c(1,1,1,2))
dev.off()

#this one is incomplete
png("LR_cos_sim.png", width = 6.4, height = 6.4, units = 'in', res = 1000)
plotAUROC(LR_cosine_norm,c(0.53,0.7),xlim = c(50,15000))
abline(h = LRBest$results["ROC"],col = "red")
abline(h = LRBest$results["ROC"] + 1.96*LRBest$results["ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h = LRBest$results["ROC"] - 1.96*LRBest$results["ROCSD"]/sqrt(200) ,col = "red",lty= 2)
plotAUROC(LR_cosine_down, add = TRUE,col ="deeppink4")
dev.off()

AUROC.CI(LR_euc_down)
AUROC.CI(LR_euc_norm)


png("RF_euc_sim_norm.png", width = 6.4, height = 6.4, units = 'in', res = 1000)
plotAUROC(RF_euc_norm, c(0.6,0.7))
abline(h= RF200$results[1,"ROC"],col ="red")
abline(h = RF200$results[1,"ROC"] + 1.96*RF200$results[1,"ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h = RF200$results[1,"ROC"] - 1.96*RF200$results[1,"ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h= RFBEST$results[1,"ROC"],col ="blue")
abline(h = RFBEST$results[1,"ROC"] + 1.96*RFBEST$results[1,"ROCSD"]/sqrt(200) ,col = "blue",lty= 2)
abline(h = RFBEST$results[1,"ROC"] - 1.96*RFBEST$results[1,"ROCSD"]/sqrt(200) ,col = "blue",lty= 2)
plotAUROC(RF_euc_down, add = TRUE,col ="deeppink4")
legend(8000,0.65,legend = c("Pop RF, 500 trees","Pop RF, 200 trees","Sim RF, 200 trees","Sim down RF, 200 trees","95% CI")
       ,col = c("blue","red","black","deeppink4","black"),lty =c(1,1,1,1,2))
dev.off()

png("RF_cos_sim_norm.png", width = 6.4, height = 6.4, units = 'in', res = 1000)
plotAUROC(RF_cosine_norm, c(0.6,0.7))
abline(h= RF200$results[1,"ROC"],col ="red")
abline(h = RF200$results[1,"ROC"] + 1.96*RF200$results[1,"ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h = RF200$results[1,"ROC"] - 1.96*RF200$results[1,"ROCSD"]/sqrt(200) ,col = "red",lty= 2)
abline(h= RFBEST$results[1,"ROC"],col ="blue")
abline(h = RFBEST$results[1,"ROC"] + 1.96*RFBEST$results[1,"ROCSD"]/sqrt(200) ,col = "blue",lty= 2)
abline(h = RFBEST$results[1,"ROC"] - 1.96*RFBEST$results[1,"ROCSD"]/sqrt(200) ,col = "blue",lty= 2)
plotAUROC(RF_cosine_down, add = TRUE,col ="deeppink4")
legend(8000,0.65,legend = c("Pop RF, 500 trees","Pop RF, 200 trees","Sim RF, 200 trees","Sim down RF, 200 trees","95% CI")
       ,col = c("blue","red","black","deeppink4","black"),lty =c(1,1,1,1,2))
dev.off()



#add something for auprc

plotAUPRC <- function(pgrid, ylim = c(0,1),add = FALSE,color = "black"){
  AUPRC.mean = pgrid[,"AUPRC.MEAN"]
  AUPRC.SD = pgrid[,"AUPRC.SD"]
  upper = AUPRC.mean + 1.96* AUPRC.SD/sqrt(200)
  lower = AUPRC.mean - 1.96* AUPRC.SD/sqrt(200)
  nSimilars =pgrid[,"nSimilar"]
  
  if(add){
    lines(nSimilars,AUPRC.mean,lty=1,col = color)
  }else{
    plot(nSimilars,AUPRC.mean, ylim = ylim, type = "l", xlab = "Number of Similar Patients", ylab="AUPRC",col = color)
  }
  
  points(nSimilars, AUPRC.mean,pch = 19,col = color)
  lines(nSimilars, upper,lty = 2,col = color)
  lines(nSimilars,lower,lty =2,col = color)
  
}



plotAUPRC(LR_cosine_norm, c(0.05,0.14),color = "red")
abline(h=0.11)
plotAUPRC(LR_euc_norm,add = TRUE,color = "blue")


plotAUPRC(RF_euc_norm, c(0.05,0.14),color = "blue")
abline(h=0.1)
plotAUPRC(RF_cosine_norm,add=TRUE,color ="red")



#tables here instead of plots
plotAUPRC(RF_euc_down, c(0.05,0.14,color = "blue")
abline(h=0.11)
plotAUPRC(RF_cosine_down,add= TRUE,color ="red")


plotAUPRC(LR_euc_down, c(0.05,0.14),color = "blue")
abline(h=0.11)
plotAUPRC(LR_cosine_down,add= TRUE,color ="red")







plotAUROC(RF_euc_down)
plotAUPRC(RF_euc_down)

plotAUROC(RF_cosine_down)
plotAUPRC(RF_cosine_down)

setwd("/Users/jordyn/Documents/Health-DS-Essay/R/Model_fit_sim/Models/lr_ps/")
readRDS("lr_ps_fit1_progresstest.RDS")
readRDS("lr_ps_fit1_progress.RDS")
readRDS("lr_ps_fit2_progress.RDS")

save.image()
