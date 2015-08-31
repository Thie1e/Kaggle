library(data.table)
library(caret)
library(beepr)
library(doSNOW)
library(bit64)
library(Metrics)
library(randomForest)

load("Mydata/dat4transTrain.RData")

cl <- makeCluster(2)
registerDoSNOW(cl)

M1 -----------------------------------------------------------------
myTrainControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
                               savePredictions = T,
                               allowParallel = T,
                               classProbs = TRUE,
                               summaryFunction = twoClassSummary)
exclude <- c("bidder_id", "outcome", "typicalContinent", "typicalRegion", "typicalMerch", "V6", "V7")
set.seed(4321)
start <- Sys.time()
M1 <- train(y = dat4transTrain$outcome,
    tuneLength = 5,
    x = as.matrix(dat4transTrain[, -c(which(names(dat4transTrain) %in% exclude)), with = F]),
    tuneGrid = expand.grid(.mtry = 8),
    method = "rf", ntree = 500,
    metric = "ROC",
    trControl = myTrainControl)
beep(); M1; Sys.time(); Sys.time() - start

# Random Forest
#
# 1984 samples
# 71 predictor
# 2 classes: 'human', 'bot'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 5 times)
#
# Summary of sample sizes: 1786, 1786, 1786, 1785, 1786, 1786, ...
#
# Resampling results
#
# ROC        Sens       Spec       ROC SD      Sens SD      Spec SD
# 0.9224588  0.9960655  0.2329091  0.03885524  0.004405449  0.09346843
#
# Tuning parameter 'mtry' was held constant at a value of 8
#
# [1] "2015-06-05 00:53:44 CEST"
# Time difference of 6.970832 mins


### public leaderboard: 90.8
### private leaderboard: 92.1

