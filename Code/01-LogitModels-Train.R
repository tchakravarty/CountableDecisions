#==========================================================
# purpose: logit models for the "Countable Decisions" DS challenge
# author: tirthankar chakravarty
# created: 5th march 2015
# revised:
# TODO:
# - DONE function that extracts the relevant dataset for missingness threshold
# - DONE create the dataset, and fit a logit model for each of the labels
# - DONE compute the accuracy of the models (in-sample)
# - DONE create n-fold validation samples & compute the score
# Parameterizations:
# 1. missingness threshold
# 2. the accuracy metric
# 3. variable selection
# comments: 
#==========================================================

#==========================================================
# function: modeling data for missingness threshold
#==========================================================
makeModelData = function(dfOriginal, threshMiss = 0.2) {
  vCovariatesN = grep("^n_", names(dfOriginal), value = TRUE)
  vCovariatesC = grep("^c_", names(dfOriginal), value = TRUE)
  vCovariatesO = grep("^o_", names(dfOriginal), value = TRUE)
  vCovariates = c(vCovariatesC, vCovariatesN, vCovariatesN)
  vLabels = grep("^service_", names(dfOriginal), value = TRUE)
  
  vMissing = sapply(dfOriginal[, vCovariates], function(x) mean(is.na(x)))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
  vCovFinal = vCovariates[vMissing < 0.2]
  
  # the categorical covariates to retain in the model --> as.factor
  vCovariatesCFinal = intersect(vCovariatesC, vCovFinal)
  dfFinal = dfOriginal[, c(vLabels, vCovFinal)]
  dfFinal = dfFinal[complete.cases(dfFinal), ]
  dfFinal[, vCovariatesCFinal] = lapply(dfFinal[, vCovariatesCFinal], as.factor)
  vCovFinal = setdiff(vCovFinal, names(which(sapply(dfFinal[, vCovariatesCFinal], nlevels) == 1)))
  dfFinal = dfFinal[ , c(vLabels, vCovFinal)]
  return(list(data = dfFinal, covariates = vCovFinal))
}

#==========================================================
# model: only use the variables for which the missingness is < 0.2
#==========================================================
modelDataLogit = makeModelData(dfTrain, 0.2)
dfTrainLogit = modelDataLogit$data
vCovLogit = modelDataLogit$covariates
liLogit = lapply(vLabels, function(label) {
  glm(as.formula(paste(label, "~", paste0(vCovLogit, collapse = "+"))), 
          data = dfTrainLogit, family = binomial(link = logit))
})

#=========================================================
# score the models and compute accuracy
# NOTES:
# - very high classification accuracy of the model
#=========================================================
computeAccMetric = function(modelObject, accuracyMetric = "auc") {
  predResponse = predict.glm(object = modelObject, type = "response")  
  predROCR = prediction(predResponse, modelObject$y)
  performance(prediction.obj = predROCR, measure = accuracyMetric)
}

vAccLogit = sapply(liLogit, function(x) computeAccMetric(x)@y.values[[1]])
names(vAccLogit) = sapply(liLogit, function(x) lhs(x$formula))

#=========================================================
# cross-validated model accuracy
# NOTES:
# - Need to explicitly make the label variable a factor
# - What does the output of each of the models look like?
#   Where is the ROC lurking
#=========================================================
logitControl = trainControl(method = "cv", number = 5, 
                            verboseIter = TRUE, returnData = FALSE, 
                            allowParallel = TRUE, summaryFunction = twoClassSummary, 
                            classProb = TRUE)

# dfTrainLogit[, vLabels] = lapply(dfTrainLogit[, vLabels], factor)
liLogitCV = lapply(vLabels, function(label) {
  train(form = as.formula(paste(label, "~", paste0(vCovLogit, collapse = "+"))), 
      data = dfTrainLogit, 
      trControl = logitControl, 
      metric = "ROC", 
      method = "glm", 
      family = binomial(link = "logit"))
})

#==========================================================
# compare the CV errors to the in-sample errors
# NOTES: 
# - names being picked up from one of the vectors
#==========================================================
dfAccLogit = data.frame(auc.cv = sapply(liLogitCV, function(x) x$results["ROC"]$ROC), 
           auc = vAccLogit)

# end #