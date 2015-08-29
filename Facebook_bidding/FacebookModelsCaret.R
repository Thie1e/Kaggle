library(data.table)
library(caret)
library(beepr)
library(doSNOW)
library(bit64)
library(ggplot2)
library(convenience)
library(Metrics)
library(randomForest)
library(plyr)


if (Sys.info()['sysname'] == "Windows"){
    setwd("W:/asc90698583/Wuala Sync/Diverses/Kaggle/Facebook/")
} else setwd("/home/khl4v/Wuala Sync/Diverses/Kaggle/Facebook")

load("Mydata/dat4Train.RData")
load("Mydata/dat4Test.RData")

load("Mydata/outlier.RData")
load("Mydata/topVars_dat4trans_999.RData")
load("Mydata/topVars_dat4trans_999Train.RData")

load("Mydata/trainIndex.RData")
load("Mydata/testIndex.RData")
load("Mydata/highCorrVars.RData")

load("Mydata/dat4transTrain.RData")
load("Mydata/dat4transTest.RData")

load("Mydata/tsne.RData")

test <- fread("Data//test.csv")

cl <- makeCluster(2)
registerDoSNOW(cl)

# M1 -----------------------------------------------------------------
# myTrainControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5,
#                                savePredictions = T,
#                                allowParallel = T,
#                                classProbs = TRUE,
#                                summaryFunction = twoClassSummary)
# exclude <- c("bidder_id", "outcome", "typicalContinent", "typicalRegion", "typicalMerch", "V6", "V7")
# set.seed(4321)
# start <- Sys.time()
# M1 <- train(y = dat4transTrain$outcome,
#     #             preProcess = "pca",
#     tuneLength = 5,
#     x = as.matrix(dat4transTrain[, -c(which(names(dat4transTrain) %in% exclude)), with = F]),
#     tuneGrid = expand.grid(.mtry = 8),
#     method = "rf", ntree = 500,
#     #             strata = outcome, sampsize = c(100, 20),
#     metric = "ROC",
#     trControl = myTrainControl)
# beep(); M1; Sys.time(); Sys.time() - start
# Gibt jetzt immer einen Fehler wegen fehlender ROC-Werte
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

# LEADERBOARD 90.8


# alle Variablen + Ts-features + tsne, ohne outlier
load("Mydata/tsne.RData")
dummyDat <- data.frame(dat4Train[trainIndex])
# dummyDat <- dummyDat[, which(colnames(dummyDat) %in% c(topVars_dat4trans_999Train,
#                                                        "bidder_id",
#                                                        "outcome"))]
# Means of tsFeatures variables
mean2 <- function(x) mean(x, na.rm = T)
load("Mydata/tsFeatures.RData")
tsFeatures[, auction := NULL]
tsFeatures <- tsFeatures[, lapply(.SD, mean2), by = bidder_id]
dummyDat <- join_all(list(dummyDat, tsFeatures, tsne), by = "bidder_id")
outcome <- dummyDat$outcome
factors <- which(unlist(lapply(dummyDat, class)) %in% c("factor"))
dummies <- sapply(factors, function(x){
    model.matrix(~dummyDat[, x] - 1)
})
dummies <- do.call(cbind, dummies)
colnames(dummies) <- gsub(pattern = "dummyDat\\[, x\\]", replacement = "",
                          colnames(dummies))
dummies <- as.data.frame(dummies)
dummyDat <- dummyDat[, -factors]
dummyDat <- cbind(dummyDat, dummies)
predictors <- dummyDat[, -which(colnames(dummyDat) %in%
                                    c("bidder_id", "outcome", "human", "bot"))]
predictors <- as.matrix(predictors)
# predictors[is.na(predictors)] <- 0
pp <- preProcess(predictors, method = "medianImpute")
predictors <- predict(pp, predictors)

# repeatedcv
# set.seed(456)
repeatedcv <- createMultiFolds(outcome, k = 5, times = 5) # List
aucFoldsTrain <- NA
aucFoldsTest <- NA
for (f in 1:length(repeatedcv)){
    tempTrainPredictors <- predictors[repeatedcv[[f]], ]
    tempTrainOutcome <- outcome[repeatedcv[[f]]]
    tempTestPredictors <- predictors[-(repeatedcv[[f]]), ]
    tempTestOutcome <- outcome[-(repeatedcv[[f]])]

    rf <- randomForest(tempTrainPredictors, tempTrainOutcome, classwt = c(0.7, 0.3))

    predsFoldTest <- predict(rf, tempTestPredictors, type = "prob")[, "bot"]
    predsFoldTrain <- predict(rf, tempTrainPredictors, type = "prob")[, "bot"]
    aucFoldsTrain[f] <- auc(actual = num(tempTrainOutcome) - 1, predicted = predsFoldTrain)
    aucFoldsTest[f] <- auc(actual = num(tempTestOutcome) - 1, predicted = predsFoldTest)
}
summary(aucFoldsTrain)
summary(aucFoldsTest)

# if trained on trainIndex, predict on Testindex
rf <- randomForest(predictors, outcome, classwt = c(0.7, 0.3), ntree = 1500)
dummyDat <- data.frame(dat4Train[, outcome := NULL])
dummyDat <- dummyDat[testIndex, ]
# Means of tsFeatures variables
mean2 <- function(x) mean(x, na.rm = T)
load("Mydata/tsFeatures.RData")
tsFeatures[, auction := NULL]
tsFeatures <- tsFeatures[, lapply(.SD, mean2), by = bidder_id]
dummyDat <- join_all(list(dummyDat, tsFeatures, tsne), by = "bidder_id")
factors <- which(unlist(lapply(dummyDat, class)) %in% c("factor"))
dummies <- sapply(factors, function(x){
    model.matrix(~dummyDat[, x] - 1)
})
dummies <- do.call(cbind, dummies)
colnames(dummies) <- gsub(pattern = "dummyDat\\[, x\\]", replacement = "",
                          colnames(dummies))
dummies <- as.data.frame(dummies)
dummyDat <- dummyDat[, -factors]
dummyDat <- cbind(dummyDat, dummies)
predictors <- dummyDat[, -which(colnames(dummyDat) %in%
                                    c("bidder_id", "outcome", "human", "bot"))]
predictors <- as.matrix(predictors)
# predictors[is.na(predictors)] <- -9999
pp <- preProcess(predictors, method = "medianImpute")
predictors <- predict(pp, predictors)
predictions <- predict(rf, predictors, type = "prob")[, "bot"]
cvPredRf <- data.frame(bidder_id = dat4Train[testIndex]$bidder_id,
                          prediction = predictions,
                          stringsAsFactors = F)


# Submission
load("Mydata/tsne.RData")
dummyDat <- data.frame(dat4Train)
# dummyDat <- dummyDat[, which(colnames(dummyDat) %in% c(topVars_dat4trans_999Train,
#                                                        "bidder_id",
#                                                        "outcome"))]
# Means of tsFeatures variables
mean2 <- function(x) mean(x, na.rm = T)
load("Mydata/tsFeatures.RData")
tsFeatures[, auction := NULL]
tsFeatures <- tsFeatures[, lapply(.SD, mean2), by = bidder_id]
dummyDat <- join_all(list(dummyDat, tsFeatures, tsne), by = "bidder_id")
outcome <- dummyDat$outcome
factors <- which(unlist(lapply(dummyDat, class)) %in% c("factor"))
dummies <- sapply(factors, function(x){
    model.matrix(~dummyDat[, x] - 1)
})
dummies <- do.call(cbind, dummies)
colnames(dummies) <- gsub(pattern = "dummyDat\\[, x\\]", replacement = "",
                          colnames(dummies))
dummies <- as.data.frame(dummies)
dummyDat <- dummyDat[, -factors]
dummyDat <- cbind(dummyDat, dummies)
predictors <- dummyDat[, -which(colnames(dummyDat) %in%
                                    c("bidder_id", "outcome", "human", "bot"))]
predictors <- as.matrix(predictors)
# predictors[is.na(predictors)] <- 0
pp <- preProcess(predictors, method = "medianImpute")
predictors <- predict(pp, predictors)
rf <- randomForest(predictors, outcome, classwt = c(0.7, 0.3), ntree = 1500)
load("Mydata/tsne.RData")
load("Mydata/dat4Test.RData")
dummyDat <- data.frame(dat4Test[, outcome := NULL])
# Means of tsFeatures variables
mean2 <- function(x) mean(x, na.rm = T)
load("Mydata/tsFeatures.RData")
tsFeatures[, auction := NULL]
tsFeatures <- tsFeatures[, lapply(.SD, mean2), by = bidder_id]
dummyDat <- join_all(list(dummyDat, tsFeatures, tsne), by = "bidder_id")
factors <- which(unlist(lapply(dummyDat, class)) %in% c("factor"))
dummies <- sapply(factors, function(x){
    model.matrix(~dummyDat[, x] - 1)
})
dummies <- do.call(cbind, dummies)
colnames(dummies) <- gsub(pattern = "dummyDat\\[, x\\]", replacement = "",
                          colnames(dummies))
dummies <- as.data.frame(dummies)
dummyDat <- dummyDat[, -factors]
dummyDat <- cbind(dummyDat, dummies)
predictors <- dummyDat[, -which(colnames(dummyDat) %in%
                                    c("bidder_id", "outcome", "human", "bot"))]
predictors <- as.matrix(predictors)
# predictors[is.na(predictors)] <- -9999
pp <- preProcess(predictors, method = "medianImpute")
predictors <- predict(pp, predictors)
predictions <- predict(rf, predictors, type = "prob")[, "bot"]
predictions <- data.frame(bidder_id = dat4Test$bidder_id,
                          prediction = predictions,
                          stringsAsFactors = F)
# Add bidders without data
predictions <- rbind(predictions,
                     data.frame(bidder_id = test$bidder_id[which(!test$bidder_id %in% predictions$bidder_id)],
                                prediction = 0.05, stringsAsFactors = F))
write.csv(predictions, file = "predictions.csv", quote = F, row.names = F)

# M2 -----------------------------------------------------------
myTrainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5,
                               savePredictions = T,
                               allowParallel = T,
                               classProbs = TRUE,
                               summaryFunction = twoClassSummary)
exclude <- c("bidder_id", "outcome", "auction")
# set.seed(4321)
start <- Sys.time()
M2 <- train(y = outcome,
            preProcess = c("center", "scale"),
            tuneLength = 3,
            x = predictors,
            tuneGrid = expand.grid(.n.trees = c(500),
                                   .interaction.depth = c(9),
                                   .shrinkage = c(0.01),
                                   .n.minobsinnode = c(10)),
            method = "gbm",
            metric = "ROC",
            trControl = myTrainControl)
# stopCluster(cl)
beep(); M2; Sys.time(); Sys.time() - start

# Hohe Spec beachten

# Stochastic Gradient Boosting
#
# 1172 samples
# 34 predictors
# 2 classes: 'human', 'bot'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 5 times)
#
# Summary of sample sizes: 1055, 1055, 1055, 1054, 1056, 1054, ...
#
# Resampling results across tuning parameters:
#
#     n.trees  ROC        Sens       Spec       ROC SD      Sens SD     Spec SD
# 500     0.9301666  0.9917487  0.3026667  0.03928318  0.01184378  0.1861217
# 1000     0.9283467  0.9913884  0.3380000  0.04138740  0.01038590  0.1892832
# 2000     0.9280668  0.9899501  0.3700000  0.04142934  0.01194634  0.2106302
#
# Tuning parameter 'interaction.depth' was held constant at a value of 9
# Tuning parameter 'shrinkage' was
# held constant at a value of 0.01
# Tuning parameter 'n.minobsinnode' was held constant at a value of 10
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 500, interaction.depth = 9, shrinkage = 0.01 and
# n.minobsinnode = 10.
# [1] "2015-05-13 02:45:08 CEST"
# Time difference of 4.793317 mins


# load("Mydata/tsFeatures.RData")
# dummyDat <- data.frame(dat4trans_999Train)
# # Means of tsFeatures variables
# mean2 <- function(x) mean(x, na.rm = T)
# load("Mydata/tsFeatures.RData")
# tsFeatures[, auction := NULL]
# tsFeatures <- tsFeatures[, lapply(.SD, mean2), by = bidder_id]
# dummyDat <- merge(dummyDat, tsFeatures, by = "bidder_id", all.x=T)
# dummyDat[is.na(dummyDat)] <- -99999
# dummyDat <- dummyDat[-(which(dummyDat$bidder_id %in% outlier)), ]
# outcome <- dummyDat$outcome
# factors <- which(unlist(lapply(dummyDat, class)) %in% c("factor"))
# dummies <- sapply(factors, function(x){
#     model.matrix(~dummyDat[, x] - 1)
# })
# dummies <- do.call(cbind, dummies)
# colnames(dummies) <- gsub(pattern = "dummyDat\\[, x\\]", replacement = "",
#                           colnames(dummies))
# dummies <- as.data.frame(dummies)
# dummyDat <- dummyDat[, -factors]
# dummyDat <- cbind(dummyDat, dummies)
# predictors <- dummyDat[, -which(colnames(dummyDat) %in%
#                                     c("bidder_id", "outcome", "human", "bot"))]
# predictors <- as.matrix(predictors)


### alle Variablen + Ts-features + tsne, ohne outlier
load("Mydata/dat4transTrain.RData")
load("Mydata/tsne.RData")
dummyDat <- data.frame(dat4transTrain)
# Nur topVars
# dummyDat <- dummyDat[, which(colnames(dummyDat) %in% c(topVars_dat4trans_999Train,
#                                                        "bidder_id",
#                                                        "outcome"))]
# Means of tsFeatures variables
mean2 <- function(x) mean(x, na.rm = T)
load("Mydata/tsFeatures.RData")
tsFeatures[, auction := NULL]
tsFeatures <- tsFeatures[, lapply(.SD, mean2), by = bidder_id]
# dummyDat <- join_all(list(dummyDat, tsFeatures, tsne), by = "bidder_id")
dummyDat <- join_all(list(dummyDat), by = "bidder_id")
outcome <- dummyDat$outcome
# factors <- which(unlist(lapply(dummyDat, class)) %in% c("factor"))
# dummies <- sapply(factors, function(x){
#     model.matrix(~dummyDat[, x] - 1)
# })
# dummies <- do.call(cbind, dummies)
# colnames(dummies) <- gsub(pattern = "dummyDat\\[, x\\]", replacement = "",
#                           colnames(dummies))
# dummies <- as.data.frame(dummies)
# dummyDat <- dummyDat[, -factors]
# dummyDat <- cbind(dummyDat, dummies)
predictors <- dummyDat[, -which(colnames(dummyDat) %in%
                                    c("bidder_id", "outcome", "human", "bot"))]
# predictors <- as.matrix(predictors)
# predictors[is.na(predictors)] <- 0
factors <- which(unlist(lapply(predictors, class)) %in% c("factor"))
pp <- preProcess(predictors[, -factors], method = "medianImpute")
predictors <- data.frame(predict(pp, predictors[, -factors]), predictors[, factors])
# pp <- preProcess(predictors, method = "medianImpute")
# predictors <- predict(pp, predictors)

# SMOTE
# newdat <- SMOTE(outcome ~ ., cbind(outcome, predictors),
#               perc.over = 400, k = 5, perc.under = 600)
# newdat <- draw(newdat, n = nrow(newdat), replace = F)
# outcome <- newdat$outcome
# predictors <- newdat[, -1]

# repeatedcv
set.seed(456)
repeatedcv <- createMultiFolds(outcome, k = 5, times = 3) # List
aucFoldsTrain <- NA
aucFoldsTest <- NA
trees = 500
# weights <- scale(mean_rec_error)+2
# weights <- weights[!is.na(dat4trans$outcome)]

# varCorrs <- c(1,1,1,1,0,
#               0,0,0,1,1,
#               -1,-1,0,0,1,
#               0,1,1,0,0,
#               0,1,1,0,0,
#               0,0,1,1,1,
#               1,0,0,0,0,
#               1,1,0,0,0,
#               1,1,0,0,0,
#               0,0,1,1,1,
#               1,0,0,0,0,
#               0,0,0,0,0,
#               0,-1,-1,1,1,
#               1,0,0,0,0,
#               0)
for (f in 1:length(repeatedcv)){
    print(f)
    tempTrainPredictors <- predictors[repeatedcv[[f]], ]
    tempTrainOutcome <- outcome[repeatedcv[[f]]]
#     newdat <- SMOTE(outcome ~ .,
#                     data.frame(outcome = tempTrainOutcome, tempTrainPredictors),
#                     perc.over = 1000, k = 5, perc.under = 200)
#     newdat <- draw(newdat, n = nrow(newdat), replace = F)
#     tempTrainOutcome <- newdat$outcome
#     tempTrainPredictors <- newdat[, -1]
    tempTestPredictors <- predictors[-(repeatedcv[[f]]), ]
    tempTestOutcome <- outcome[-(repeatedcv[[f]])]
    # tempWeights <- weights[repeatedcv[[f]]]

    verb <- ifelse (f %in% 1:3, T, F)
    gbm <- gbm.fit(tempTrainPredictors, num(tempTrainOutcome) - 1,
                   # var.monotone = varCorrs,
                   # w = tempWeights,
                   n.trees = trees, interaction.depth = 7, shrinkage = 0.03,
                   n.minobsinnode = 100, verbose = verb)

    predsFoldTest <- predict(gbm, tempTestPredictors, type = "response", n.trees = trees)
    predsFoldTrain <- predict(gbm, tempTrainPredictors, type = "response", n.trees = trees)
    aucFoldsTrain[f] <- auc(actual = num(tempTrainOutcome) - 1, predicted = predsFoldTrain)
    aucFoldsTest[f] <- auc(actual = num(tempTestOutcome) - 1, predicted = predsFoldTest)
}
summary(aucFoldsTrain)
summary(aucFoldsTest)

# if trained on trainIndex, predict on Testindex
gbm <- gbm.fit(predictors, num(outcome) - 1,
               n.trees = trees, interaction.depth = 7, shrinkage = 0.03,
               n.minobsinnode = 100, verbose = 1)
load("Mydata/dat4transTrain.RData")
dummyDat <- data.frame(dat4transTrain)
dummyDat <- dummyDat[testIndex, ]
predictors <- dummyDat[, -which(colnames(dummyDat) %in%
                                    c("bidder_id", "outcome", "human", "bot"))]
factors <- which(unlist(lapply(predictors, class)) %in% c("factor"))
pp <- preProcess(predictors[, -factors], method = "medianImpute")
predictors <- data.frame(predict(pp, predictors[, -factors]), predictors[, factors])
predictions <- predict(gbm, predictors, type = "response", n.trees = trees)
cvPredGbm <- data.frame(bidder_id = dat4transTrain[testIndex]$bidder_id,
                       prediction = predictions,
                       stringsAsFactors = F)


# Submission
gbm <- gbm.fit(predictors, num(outcome) - 1,
               # var.monotone = varCorrs,
               # w = weights,
               n.trees = trees, interaction.depth = 9, shrinkage = 0.01,
               n.minobsinnode = 10, verbose = T)
load("Mydata/dat4transTest.RData")
dummyDat <- data.frame(dat4transTest[, outcome := NULL])
predictors <- dummyDat[, -which(colnames(dummyDat) %in%
                                    c("bidder_id", "outcome", "human", "bot"))]
factors <- which(unlist(lapply(predictors, class)) %in% c("factor"))
pp <- preProcess(predictors[, -factors], method = "medianImpute")
predictors <- data.frame(predict(pp, predictors[, -factors]), predictors[, factors])
predictions <- predict(gbm, predictors, type = "response", n.trees = trees)
predictions <- data.frame(bidder_id = dat4Test$bidder_id,
                          prediction = predictions,
                          stringsAsFactors = F)
predsGbm <- predictions
# Add bidders without data
predictions <- rbind(predictions,
                     data.frame(bidder_id = test$bidder_id[which(!test$bidder_id %in% predictions$bidder_id)],
                                prediction = 0.05, stringsAsFactors = F))
write.csv(predictions, file = "predictions.csv", quote = F, row.names = F)



# M7 -----------------------------------------------------------
load("Mydata/dat4Train.RData")
mytrain <- data.frame(dat4Train[trainIndex])
mean2 <- function(x) mean(x, na.rm = T)
load("Mydata/tsFeatures.RData")
tsFeatures[, auction := NULL]
tsFeatures <- tsFeatures[, lapply(.SD, mean2), by = bidder_id]
mytrain <- merge(mytrain, tsFeatures, by = "bidder_id", all.x = T)
mytrain <- data.frame(mytrain[, -1]) # ohne bidder_id
factors <- which(unlist(lapply(mytrain, class)) %in% c("factor"))
pp <- preProcess(mytrain[, -factors], method = "medianImpute")
mytrain <- data.frame(predict(pp, mytrain[, -factors]), mytrain[, factors])

myTrainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                               savePredictions = T,
                               allowParallel = T,
                               classProbs = TRUE,
                               summaryFunction = twoClassSummary)
# set.seed(4321)
start <- Sys.time()
M7 <- train(y = mytrain$outcome,
            #                         preProcess = c("center", "scale", "pca"),
            tuneLength = 4,
            x = mytrain[, -c(which(names(mytrain) %in% c("outcome",
                                                         "bidder_id",
                                                         "auction",
                                                         "typicalMerch",
                                                         "typicalRegion",
                                                         "imputed")))],
                        tuneGrid = expand.grid(.trials = c(100),
                                               .model = "tree",
                                               .winnow = FALSE),
            method = "C5.0",
            metric = "ROC",
            trControl = myTrainControl)
# stopCluster(cl)
beep(); M7; Sys.time(); Sys.time() - start

# C5.0
#
# 1191 samples
# 82 predictors
# 2 classes: 'human', 'bot'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times)
#
# Summary of sample sizes: 1071, 1072, 1072, 1072, 1072, 1072, ...
#
# Resampling results
#
# ROC        Sens       Spec       ROC SD      Sens SD     Spec SD
# 0.9157351  0.9917325  0.2555556  0.04209439  0.00898678  0.1708865
#
# Tuning parameter 'trials' was held constant at a value of 100
# Tuning parameter 'model' was held constant at
# a value of tree
# Tuning parameter 'winnow' was held constant at a value of FALSE
#
# [1] "2015-06-08 20:24:39 CEST"
# Time difference of 4.867825 mins


# Predict on cv-set, trained on trainset
load("Mydata/dat4Train.RData")
mytrain <- data.frame(dat4Train[testIndex])
mean2 <- function(x) mean(x, na.rm = T)
load("Mydata/tsFeatures.RData")
tsFeatures[, auction := NULL]
tsFeatures <- tsFeatures[, lapply(.SD, mean2), by = bidder_id]
mytrain <- merge(mytrain, tsFeatures, by = "bidder_id", all.x = T)
mytrain <- data.frame(mytrain[, -1]) # ohne bidder_id
factors <- which(unlist(lapply(mytrain, class)) %in% c("factor"))
pp <- preProcess(mytrain[, -factors], method = "medianImpute")
mytrain <- data.frame(predict(pp, mytrain[, -factors]), mytrain[, factors])
cvPredC50 <- predict(M7, mytrain, type = "prob")[, "bot"]
cvPredC50 <- data.frame(bidder_id = dat4Train$bidder_id[testIndex], pred = cvPredC50)



# M8 -----------------------------------------------------------
myTrainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                               savePredictions = T,
                               allowParallel = T,
                               classProbs = TRUE,
                               summaryFunction = twoClassSummary)
mytrain <- dat4transTrain
typicalMerchDum <- model.matrix( ~ mytrain$typicalMerch - 1)
typicalRegionDum <- model.matrix( ~ mytrain$typicalRegion - 1)
V6Dum <- model.matrix( ~ mytrain$V6 - 1)
V7Dum <- model.matrix( ~ mytrain$V7 - 1)

mytrain2 <- cbind(mytrain, typicalMerchDum, typicalRegionDum, V6Dum, V7Dum)

set.seed(4321)
(start <- Sys.time())
M8 <- train(y = outcome,
            #             preProcess = c("center", "scale", "pca"),
            tuneLength = 5,
            x = mytrain2[, -c(which(names(mytrain2) %in% c(exclude,
                                                           "typicalMerch",
                                                           "typicalRegion",
                                                           "typicalContinent",
                                                           "V6",
                                                           "V7",
                                                           "imputed"))),
                         with = F],
            tuneGrid = expand.grid(.nprune = 27,
                                   .degree = 2),
            method = "bagEarth",# control = list(maxit = 100),
            metric = "ROC",
            trControl = myTrainControl)
# stopCluster(cl)
Sys.sleep(1); beep(); Sys.sleep(1); M8; Sys.time(); Sys.time() - start

# Bagged MARS
#
# 1172 samples
# 62 predictors
# 2 classes: 'human', 'bot'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 5 times)
#
# Summary of sample sizes: 1055, 1055, 1055, 1055, 1055, 1055, ...
#
# Resampling results
#
# ROC        Sens       Spec   ROC SD     Sens SD      Spec SD
# 0.9053399  0.9928089  0.264  0.0526418  0.007907635  0.1724625
#
# Tuning parameter 'nprune' was held constant at a value of 27
# Tuning parameter 'degree' was held constant at
# a value of 2
#
# [1] "2015-05-13 00:40:27 CEST"
# Time difference of 2.738605 hours



