require(xgboost)
require(Matrix)
require(data.table)
require(ggplot2)
#set the random seed
set.seed(686)

#load datasets for modeling
load("Datasets/TreeTraining.RData")
load("Datasets/TreeTesting.RData")
load("Datasets/TreeEval.RData")
load("Datasets/StoreSalesGroups.RData")
#load("Datasets/FinalTest.RData")
