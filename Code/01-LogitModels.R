#==========================================================
# logit models: 
# 1. create n-fold validation samples
# 2. assess accuracy over fitting these models
# 3. vary the parameters
#   - the degree of missingness allowed in the model
#   - 
# 4. 
#==========================================================
library(ROCR)
library(pROC)

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
#=========================================================
computeAccMetric = function(modelObject, modelData, accuracyMetric) {
  predResponse = predict.glm(object = x, newdata = dfTrainLogit, type = "response")  
  predROCR = prediction(predResponse, )

}
lapply(liLogit, function(x) {
  predict.glm(object = x, newdata = dfTrainLogit, type = "response")  
})