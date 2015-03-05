#====================================================================
# purpose: data preparation and exploration for the drivedata.org "Countable Decisions"
#   problem
# author: tirthankar chakravarty
# created: 4th march 2015
# revised: 
# comments: 
# ISSUES:
# 1. logit regression failing because the number of levels of the factor
#   are less than 2 after omitting NAs.
# 2. 
#====================================================================

rm(list = ls())

# load the data
dfTrainValues = read.csv("Data/cef07265-8623-4680-b501-a5688f5babb0/train_values.csv", stringsAsFactors = FALSE,
                         na.strings = "")
dfTrainLabels = read.csv("Data/cef07265-8623-4680-b501-a5688f5babb0/train_labels.csv", stringsAsFactors = FALSE,
                         na.strings = "")
dfTrain = merge(dfTrainLabels, dfTrainValues, by = "id")
dfTestValues = read.csv("Data/cef07265-8623-4680-b501-a5688f5babb0/test_values.csv", stringsAsFactors = FALSE,
                        na.strings = "")

# create the regressor groups by name
vCovariatesN = grep("^n_", names(dfTrain), value = TRUE)
vCovariatesC = grep("^c_", names(dfTrain), value = TRUE)
vCovariatesO = grep("^o_", names(dfTrain), value = TRUE)
vCovariates = c(vCovariatesC, vCovariatesN, vCovariatesN)
vLabels = grep("^service_", names(dfTrain), value = TRUE)

# end # 

