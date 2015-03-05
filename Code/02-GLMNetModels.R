#==========================================================
# Elastic net models: 
# 1. create n-fold validation samples
# 2. assess accuracy over fitting these models
# 3. vary the parameters
#   - the degree of missingness allowed in the model
#   - 
# 4. 
#==========================================================
# model 1.1: only use the variables for which the missingness is < 0.25
vCov1.1 = vCovariates[vMissing < 0.2]

# the categorical covariates to retain in the model --> as.factor
vCovariatesC1.1 = intersect(vCovariatesC, vCov1.1)
dfTrain1.1 = dfTrain[, c("service_a", vCov1.1)]
dfTrain1.1 = dfTrain1.1[complete.cases(dfTrain1.1), ]
dfTrain1.1[, vCovariatesC1.1] = lapply(dfTrain1.1[, vCovariatesC1.1], as.factor)
vCov1.1 = setdiff(vCov1.1, names(which(sapply(dfTrain1.1[, vCovariatesC1.1], nlevels) == 1)))
dfTrain1.1 = dfTrain1.1[ , c("service_a", vCov1.1)]

liGLMNet1.1 = lapply(vLabels, function(label) {
  fLogit1.1 = as.formula(paste(label, "~", paste0(vCov1.1, collapse = "+")))
  
  cv.glmnet(x = model.matrix(fLogit1.1, data = dfTrain1.1), 
            y = model.response(fLogit1.1, data = dfTrain1.1),
            family = "binomial", type.measure = "auc", nfolds = 5)
})