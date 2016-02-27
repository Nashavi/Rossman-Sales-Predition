require(ggplot2)
#d<- read.csv("masterdataset.csv")

j <- sample(d$Store,1,replace=FALSE)

s <- d[d$Store==j & d$Open=="1",]
s$Date <- as.Date(s$Date)
s<-s[order(s$Date),]

ggplot(s,aes(s$Date,s$Sales,color=factor(s$Promo)))+geom_point()+stat_smooth(method="lm",se=FALSE)



