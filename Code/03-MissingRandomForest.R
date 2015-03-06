#====================================================================
# purpose: imputation of missing values using "missForest"
# author: tirthankar chakravarty
# created: 6th March 2015
# revised:
# comments: need to do this since 
# TODO: 
# 1. Experiment with the parameters of the missing random forest implementation
#====================================================================

# combine the values from the train and test values
dfValuesMiss = rbind.data.frame(dfTrainValues, dfTestValues)
dfValuesMiss[, vCovariatesC] = lapply(dfValuesMiss[, vCovariatesC], as.factor)

# missing value imputation with the missForest 
dfValues = missForest(xmis = dfValuesMiss[, c(vCovariatesC, vCovariatesO, vCovariatesN)], 
                      maxiter = 100, 
                      ntree = 1000,
                      verbose = TRUE)


