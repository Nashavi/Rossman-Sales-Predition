require(DataCombine)
d <- read.csv("masterdataset.csv")

d<-d[order(d$Store,d$Date),] #order by store and then state
d$Date <- as.Date(d$Date)
s <- unique(d$Store)

sub <-d[d$Store %in% c(1,2,3),]

sub$plag <- slide(sub, Var = "Promo", GroupVar = "Store", slideBy = 1)

d$promovalid <- ifelse(d$Promo=="1" & d$Open=="1",1,0)


############### well,no need for this crap anymore :) ###########3
d$ol1 <- 0

for(j in 1:length(s)){
  r <- d[d$Store==j,"X"]
  for(i in 1:length(r)){
    x1 <- r[i]
    x2 <- r[i+1]
    #o2 <- d[d$X==x2,"Open"]
    o1 <- d[d$X==x1,"Open"]
    if(is.na(x2)==FALSE){
    d[d$X==x2,"ol1"] <- o1
    }
  }
}

d$pl1 <- 0

for(j in 1){   #length(s)){
  r <- d[d$Store==j,"X"]
  
  for(i in 1:length(r)){
    x1 <- r[i]
    x2 <- r[i+1]
    #p2 <- d[d$X==x2,"Promo"]
    p1 <- d[d$X==x1,"Promo"]
    if (is.na(x2)==FALSE) {
      d[d$X==x2,"pl1"] <- p1
    }
  }
}


