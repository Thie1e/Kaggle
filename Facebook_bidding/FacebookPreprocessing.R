library(data.table)
library(caret)
library(doSNOW)
library(beepr)
library(bit64)
library(plyr)
library(dplyr)
library(reshape2)
library(countrycode)
library(ggplot2)
library(tau)
library(convenience)
library(h2o)

if (Sys.info()['sysname'] == "Windows"){
    setwd("W:/asc90698583/Wuala Sync/Diverses/Kaggle/Facebook/")
} else setwd("/home/khl4v/Wuala Sync/Diverses/Kaggle/Facebook")

# find indices where a vector changes
dup <- function(v) data.table:::uniqlist(list(v))

load("Mydata/dat4.RData")
load("Mydata/dat4na.RData")
load("Mydata/dat4trans.RData")
# Die Daten ohne Imputation transformiert
load("Mydata/dat4transNa.RData")
# NAs in den rohen Daten durch -999 ersetzt
load("Mydata/dat4_999.RData")
# NAs in den transformierten Daten durch -999 ersetzt
load("Mydata/dat4trans_999.RData")

load("Mydata/dat4transTrain.RData")



# Train- und Testset
set.seed(1235)
trainIndex <- createDataPartition(y = factor(dat4transTrain$outcome), p = 0.6, list = F)
trainIndex <- draw(trainIndex, n = length(trainIndex))
testIndex <- (1:nrow(dat4transTrain))[-trainIndex]
testIndex <- draw(testIndex, n = length(testIndex))

exclude <- c("auction", "bidder_id", "outcome", "country",
             "east", "north", "imputed")

tempdata <- dat4trans[, -c(which(names(dat4trans) %in% exclude)), with = F]
tempdata <- dat4trans[, which(unlist(lapply(dat4trans, class)) %in% c("integer", "numeric")),
                    with = F]
correlations <- cor(tempdata)
highCorr <- findCorrelation(correlations, cutoff = 0.80)
(highCorrVars <- colnames(tempdata[,highCorr, with = F]))
# [1] "ipChangesRange"      "nBidsRange"          "nSeqRange"           "maxNSeq"             "nIPsRange"
# [6] "ipChangesMax"        "deviceChangesMax"    "nDevicesRange"       "sumBids"             "sumIPs"
# [11] "deviceChangesMean"   "nUrlRange"           "nDevicesOverall"     "ipChangesMean"       "nDevicesSD"
# [16] "countryChangesMax"   "countryChangesRange" "nIPsMean"            "nDevicesMean"        "nBidsMean"
# [21] "nParticipations"     "distanceRange"       "nCountriesRange"     "distanceMax"         "nSeqMean"
# [26] "sumDistanceMean"     "nSeqSD"              "ipChangesSD"         "nIPsSD"              "maxSeqRange"
# [31] "bidderIpProbRange"   "nBidsSD"             "maxSeqMax"           "devicesPerBid"       "countryChangesMean"
# [36] "nMerch"              "countryChangesSD"    "meanRankRange"       "maxSeqSD"
# Nur eine Variable unterschied zwischen dat4trans und dat4transTrain

save(trainIndex, file = "Mydata/trainIndex.RData")
save(testIndex, file = "Mydata/testIndex.RData")
save(highCorrVars, file = "Mydata/highCorrVars.RData")


# rfRFE -----------------------------------------------------------------------

cl <- makeCluster(3)
registerDoSNOW(cl)

# Mit Loop:
factors <- which(unlist(lapply(dat4trans_999Train, class)) %in% c("factor"))
excludeIndex <- which(names(dat4trans_999Train) %in% exclude)
outcome <- dat4trans_999Train$outcome[trainIndex]
predictors <- dat4trans_999Train[trainIndex, -c(factors, excludeIndex), with = F]
topVarsLoop <- list()
for (i in 4:5){
    start <- Sys.time()
    subsets <- seq(5, 65, 5)
    myrfFuncs <- rfFuncs
    myrfFuncs$summary <- twoClassSummary
    myrfFuncs$fit <- function(x, y, first, last, ...){
        library(randomForest)
        randomForest(x, y, nodesize = 1,
                     ntree = 500,
                     importance = first,
                     #                      mtry = ceiling(sqrt(ncol(x))),
                     ...)
    }
    ctrl <- rfeControl(functions = myrfFuncs,
                       method = "repeatedcv", repeats = 5)
    rfprofile <- rfe(y = outcome,
                     metric = "ROC",
                     x = predictors,
                     sizes = subsets, rfeControl = ctrl)
    topVarsLoop[[i]] <- rfprofile$optVariables
    print(i)
    print(Sys.time() - start)
    print(rfprofile)
    gc()
}
beep()
hist(unlist(lapply(topVarsLoop, FUN = function(x) length(x))),100)
summary(unlist(lapply(topVarsLoop, FUN = function(x) length(x))))
# Mean 29, Median 30
top <- lapply(topVarsLoop, FUN = function(x) na.omit(x[1:30]))
# Welche davon sind wie oft in Profilen?
vars <- unique(unlist(top))
counts <- unlist(lapply(vars, function(x) sum(x == unlist(top))))
topVars_dat4trans_999Train <- vars[which(counts >= 2)]
save(topVars_dat4trans_999Train, file = "Mydata/topVars_dat4trans_999Train.RData")



# SA --------------------------------------------------------------------

factors <- which(unlist(lapply(dat4trans_999, class)) %in% c("factor"))
# factors <- NULL
excludeIndex <- which(names(dat4trans_999) %in% exclude)
outcome <- dat4trans_999$outcome[trainIndex]
predictors <- dat4trans_999[trainIndex, -c(factors, excludeIndex), with = F]
sa_ctrl <- safsControl(functions = rfSA,
                       allowParallel = T, verbose = T,
                       method = "repeatedcv",
                       repeats = 4,
                       improve = 30)
set.seed(10)
start <- Sys.time()
rf_sa <- safs(x = predictors,
              y = outcome,
              iters = 500,
              safsControl = sa_ctrl)
rf_sa; beep(); Sys.time() - start
# Acc 95.47, Kappa 26.72
(topVars_SA_dat4trans_999 <- rf_sa$optVariables)
# [1] "nBidsSD"                 "nIPsRange"               "lastBidSD"               "nDevicesSD"              "nDevicesRange"
# [6] "maxSeqMean"              "meanRankSD"              "maxSeqRange"             "nDevicesOverall"         "nUrlSD"
# [11] "ipChangesRange"          "bidderIpBigramProbsMean" "bidderIpBigramProbsMax"  "bidderIpProbSD"          "bidderIpProbRange"
# [16] "deviceChangesMax"        "distanceMax"             "distanceRange"           "commonIPmean"            "merchDiscrepancyMean"
# [21] "timeDiffSDSD"            "timeDiffSDRange"         "timeDiffRangeMean"       "timeDiffRangeSD"         "timeDiffRangeRange"
# [26] "devicesPerBid"           "IPsPerBid"               "countriesPerBid"         "sumDistance"
save(topVars_SA_dat4trans_999, file = "Mydata/topVars_SA_dat4trans_999.RData")


# Genetic Algorithm  ----------------------------------------------

# factors <- which(unlist(lapply(dat4trans_999, class)) %in% c("factor"))
# # factors <- NULL
# excludeIndex <- which(names(dat4trans_999) %in% exclude)
# outcome <- dat4trans_999$outcome[trainIndex]
# predictors <- dat4trans_999[trainIndex, -c(factors, excludeIndex), with = F]
# ga_ctrl <- gafsControl(functions = rfGA,
#                        method = "repeatedcv",
#                        repeats = 2)
# set.seed(11)
# start <- Sys.time()
# rf_ga <- gafs(x = predictors, y = outcome,
#               iters = 3,
#               gafsControl = ga_ctrl)
# rf_ga; beep(); Sys.time() - start


# Auto Encoder Outlier detection ------------------------------------------
# Auf alle Daten, dazu werden die labels ja nicht benötigt

h2oServer <- h2o.init(nthreads=1) #, max_mem_size = "3072M")
dat_hex <- as.h2o(h2oServer, dat4trans)
# Unsupervised dating, ohne bidder_id und ohne outcome
predictors <- 2:(ncol(dat_hex) - 1)

# How many principal components are needed?
pca_model <- h2o.prcomp(dat_hex[, predictors])
plot(pca_model@model$sdev)
# Elbow bei 4 oder 8 mit den Trainingsdaten
# 10 mit allen Daten, also kein großer Unterschied
# Mit caret:
factors <- which(unlist(lapply(dat4trans, class)) %in% c("factor"))
preProcess(x = data.frame(dat4trans)[, -c(which(colnames(dat4trans) %in% exclude), factors)],
           method = "pca")
# Call:
#     preProcess.default(x = data.frame(dat4trans)[, -c(which(colnames(dat4trans) %in% exclude), factors)], method = "pca")
#
# Created from 6614 samples and 53 variables
# Pre-processing: principal component signal extraction, scaled, centered
#
# PCA needed 16 components to capture 95 percent of the variance

higherr <- c()
lowerr <- c()
mean_rec_error <- matrix(NA, ncol = 10, nrow = 6614)
for (i in 1:10){ # dann nochmal 1:10 mit Rectifier
    print(i)
      ae_model <- h2o.deeplearning(x=predictors,
                                   y=42, #response (ignored - pick any non-constant column)
                                   data=dat_hex,
                                   classification = F,
                                   activation="Tanh",
                                   autoencoder=T,
                                   hidden=c(10),
                                   ignore_const_cols=F,
                                   epochs=20)

      # Which are the outliers?
      dat_rec_error <- as.data.frame(h2o.anomaly(dat_hex, ae_model))
      # summary(dat_rec_error)
      # High reconstruction error
      # cutoff <- 0.065
      cutoff <- quantile(unlist(dat_rec_error), probs = 0.97)
      hist(unlist(dat_rec_error), 100); abline(v = cutoff)
      higherr <- c(higherr, which(dat_rec_error > cutoff))
      cutofflow <- quantile(unlist(dat_rec_error), probs = 0.15)
      abline(v = cutofflow)
      lowerr <- c(lowerr, which(dat_rec_error < cutofflow))
      mean_rec_error[, i] <- unlist(dat_rec_error)
}
# Zusammenfassen
higherr <- num(names(table(higherr)[table(higherr) > 10]))
dat4trans[higherr, ]
# human   bot  <NA>
#     28    13    77
lowerr <- num(names(table(lowerr)[table(lowerr) > 4]))
dat4trans[lowerr, ]
# human   bot  <NA>
#     510     5  1338
# Menschen, die outlier sind, als outlier kennzeichnen. Dass bots outlier sind,
# ist ja zu erwarten. 1,8% fallen damit heraus.
outlier <- higherr[!is.na(dat4$outcome[higherr]) & dat4$outcome[higherr] == "human"]
# Umgekehrt für bots
outlier <- c(outlier,
             lowerr[!is.na(dat4$outcome[lowerr]) & dat4$outcome[lowerr] == "bot"])
# Welche IDs haben die outlier?
outlier <- dat4trans[outlier]$bidder_id
save(outlier, file = "Mydata/outlier.RData")
# Save mean_rec_error
mean_rec_error <- rowMeans(mean_rec_error)


# ### Mit outcome, welche humans und welche bots sind ungewöhnlich?
# # Hier muss ich wieder das Trainings- und Testset nehmen, um später auf das
# # Testset testen zu können
# # outcome nicht als Faktor, weil mir das Ergebnis dann nicht ganz klar ist...
# train_hex <- as.h2o(h2oServer, dat4trans[trainIndex])
# # test_hex <- as.h2o(h2oServer, draw(dat4trans[testIndex], n = length(testIndex)))
# predictors <- c(2:70)
# # How many principal components are needed?
# pca_model <- h2o.prcomp(train_hex[, predictors])
# plot(pca_model@model$sdev)
# # Elbow bei 4 oder 9
# # Mit caret:
# factors <- which(unlist(lapply(dat4trans, class)) %in% c("factor"))
# preProcess(x = data.frame(dat4trans)[, -c(which(colnames(dat4trans) %in% exclude), factors)],
#            method = "pca")
# # Created from 1984 samples and 63 variables
# # Pre-processing: principal component signal extraction, scaled, centered
# # PCA needed 22 components to capture 95 percent of the variance
#
# ae_model <- h2o.deeplearning(x=predictors,
#                              y=42, #response (ignored - pick any non-constant column)
#                              data=train_hex,
#                              classification = F,
#                              activation="Tanh",
#                              autoencoder=T,
#                              hidden=c(9),
#                              ignore_const_cols=F,
#                              epochs=2000)
#
# # Interessant ist vor allem, welche humans aussehen wie bots, also welche humans
# # als bots rekonstruiert werden
# train_recon <- h2o.predict(ae_model, train_hex)
# temp <- cbind(train_recon[, c("reconstr_outcome.human", "reconstr_outcome.bot")],
#               train_hex$outcome)
# temp <- as.data.frame(temp)
# hist(temp$reconstr_outcome.human, 100); abline(v = 0.85)
# hist(temp$reconstr_outcome.bot, 100); abline(v = 0.25)
# # Cutoffs anhand der Histogramme
# (badRecon <- which(temp$outcome == "human" &
#                         temp$reconstr_outcome.human < 0.85 &
#                          temp$reconstr_outcome.bot > 0.25)
#  )
# # 134 226 341 451 509 639 858 925 1075 1177
# # [1]   16  154  304  840 1030
# dat4[trainIndex][badRecon]

