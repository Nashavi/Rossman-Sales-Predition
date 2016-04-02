
library(xgboost)

?list()

# Load Default Setup 
source("Modeling/Modeling Setup.R")

require(caret)
library(Matrix)
require(data.table)

d <- training
rm(training)

d <- d[,-1]
d <- d[d$Open==1 & d$Sales!=0,]
d <- d[,-2]
d <- d[,-2]
d <- d[,-2]
d$Sales <- log(d$Sales)

Sales <- d$Sales

dtrain <- xgb.DMatrix(as.matrix(sapply(d, as.numeric)), label=Sales) 

sparse_matrix <- sparse.model.matrix(Sales ~ .-1, data = d)
output_vector = d$Sales
labels = d['labels']

#d <- data.table(d)

# Create the output vector (not sparse)
# 1. Set, for all rows, field in Y column to 0; 
# 2. set Y to 1 when Improved == Marked; 
# 3. Return Y column

sales <- d$Sales
d <- d[,-1]
d <- as.matrix(d)

d <- as(d, "dgCMatrix")

labels(d)

train <- xgb.DMatrix(sparse_matrix)

dtest <- xgb.DMatrix(d,label=sales)

class(dtrain)


bst <- xgboost(data = train, label = sales, max.depth = 9,
               eta = 1, nround = 10,objective = "reg:linear")

?xgb.train()

param <- list(max.depth = 5
              , eta = .1
              , silent = 0
              , objective=RMSE
              #,eval_metric=
                )
xgb.train(param, dtrain, nrounds= 10,nthread=2)
#, obj = reg:linear,
 #         feval = NULL, verbose = 1, print.every.n = 1L,
  #        early.stop.round = NULL, maximize = FALSE)



pred <- predict(bst, test$data)






