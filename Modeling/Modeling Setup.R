#load required libraries 
library(caret)
library(randomForest)
library(gbm)
library(ggplot2)

#set the random seed
set.seed(686)

#load datasets for modeling
load("Datasets/Training.RData")
load("Datasets/Testing.RData")
load("Datasets/Eval.RData")
load("Datasets/FinalTest.RData")



