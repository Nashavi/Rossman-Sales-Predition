
source("Modeling/Modeling Setup.R")
st <- "BW"

training <- training[training$Open==1 & training$Sales!=0 & training$State==st, ]
training$Sales <- log(training$Sales)
training <- training[,-c(1,3:5)]
#x = data.frame(nearZeroVar(training, saveMetrics = TRUE))
#zerovarcols <- which(x$zeroVar==TRUE)
#training <- training[,-zerovarcols]
#training <- as.matrix(training)

testing <- testing[testing$Open==1 & testing$Sales!=0 & testing$State==st, ]
testing$sales <- log(testing$Sales)
testing <- testing[,-c(1,3:5)]
#testing <- as.matrix(testing)

eval <- eval[eval$Open==1 & eval$Sales!=0 & eval$State==st, ]
eval$Sales <- log(eval$Sales)
eval <- eval[,-c(1,3:5)]
#eval <- as.matrix(eval)


attach(training)
null.model = lm(Sales~1,data=training)
full.model = lm(Sales~.,data=training)
step.mod = step(null.model,scope=list(upper=full.model),data=training,direction="both")
step.mod$anova
step.aic = as.data.frame(step.mod$anova,row.names = c(rep(1:nrow(step.mod$anova)))) #dump results to a data frame
step.aic
step.aic$variables_added = rep(1:nrow(step.mod$anova))
ggplot(step.aic,aes(variables_added,AIC))+geom_point()+geom_line(color="red")+ggtitle("Stepwise AIC by Total Variables Added")

step.aic[which(step.aic$Step<=50),"Step"]

sqrt(mean((predict(step.mod,testing,type="response")-testing$sales)^2))

lm.mod <- lm(Sales~Promo.1+Promo.20+month.12+dayofWeek.0+Assortment.a+Open_L1.2+dayofWeek.1+dayofMonth.30+
   weekNum.51+weekNum.53+StoreType.a+dayofMonth.30+dayofWeek.5+promovalid._L11+CompetitionDistance,data=training)
summary(lm.mod)

sqrt(mean((predict(lm.mod,testing,type="response")-testing$sales)^2))
