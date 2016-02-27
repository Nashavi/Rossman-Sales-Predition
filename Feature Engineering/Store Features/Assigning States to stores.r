require(dplyr)
d <- read.csv("masterdataset.csv")
s <- read.csv("stores.csv")

############# STATE HOLIDAYS ##################
#subset based on state holiday flag
h <- d[d$StateHoliday=="a",]
rm(d)
open_on_hol <- unique(h[h$Open==1,"Store"])

### set states to unknown
s$State <- "Unk"

# saxony celebrates repentance day on 11/20/2013
sn <- h[h$Date=="2013-11-20","Store"] 
sn2 <- h[h$Date=="2014-11-19","Store"]
setdiff(sn,sn2)
rm(sn2)
s$State <- ifelse(s$Store %in% sn,"SN",s$State)

# epiphany is celebrated on 1/6/2014 in BW,BY,ST
bw_by_st <- h[h$Date=="2013-01-06","Store"]
bw_by_st2 <- h[h$Date=="2014-01-06","Store"]
bw_by_st3 <- h[h$Date=="2015-01-06","Store"]
setdiff(bw_by_st,bw_by_st2)
setdiff(bw_by_st,bw_by_st3)
intersect(bw_by_st,sn)
rm(bw_by_st3)
rm(bw_by_st2)

# corpus christi is celebrated on 5/30/2013 in BW, BY, HE, NW, RP, SL
bw_by_he_nw_rp_sl <- h[h$Date=="2013-05-30","Store"]
bw_by_he_nw_rp_sl2 <- h[h$Date=="2014-06-19","Store"]
bw_by_he_nw_rp_sl3 <- h[h$Date=="2015-06-04","Store"]
setdiff(bw_by_he_nw_rp_sl,bw_by_he_nw_rp_sl2)
setdiff(bw_by_he_nw_rp_sl,bw_by_he_nw_rp_sl3)
rm(bw_by_he_nw_rp_sl3)
rm(bw_by_he_nw_rp_sl2)
intersect(bw_by_he_nw_rp_sl,sn2) # saxony stores ended up in this set - 
intersect(bw_by_he_nw_rp_sl2,sn2)
length(intersect(bw_by_he_nw_rp_sl3,sn))
#if saxony stores are in this set, is it reliable?????
bw_by_he_nw_rp_sl <- setdiff(bw_by_he_nw_rp_sl,sn) #remove saxony 

# 2013-08-15 is the Assumption of Mary in BY, SL
by_sl <- h[h$Date=="2013-08-15","Store"]
by_sl2 <- h[h$Date=="2014-08-15","Store"] #no stores in this 
by_sl3 <-   h[h$Date=="2015-08-15","Store"] #no stores in this 
setdiff(by_sl,by_sl3)
rm(by_sl2)
rm(by_sl3)
intersect(sn,by_sl)

#BY is the common element
by <- intersect(bw_by_st,by_sl)
s$State <- ifelse(s$Store %in% by,"BY",s$State)

#2013-10-31 is Reformation Day in BB, MV, SN, ST, TH
bb_mv_sn_st_th <- h[h$Date=="2013-10-31","Store"]
bb_mv_sn_st_th2 <- h[h$Date=="2014-10-31","Store"]
bb_mv_sn_st_th3 <- h[h$Date=="2015-10-31","Store"] #no stores here
rm(bb_mv_sn_st_th2)
setdiff(bb_mv_sn_st_th,bb_mv_sn_st_th2)

#2013-11-01 is All Saints' Day in BW, BY, NW, RP, SL
bw_by_nw_rp_sl <- h[h$Date=="2013-11-01","Store"]#Friday
#bw_by_nw_rp_sl2 <- h[h$Date=="2014-11-01","Store"]#Saturday 
#bw_by_nw_rp_sl3 <- h[h$Date=="2015-11-01","Store"]#Sunday
#rm(bw_by_nw_rp_sl3)
#setdiff(bw_by_nw_rp_sl,bw_by_nw_rp_sl2)


# use set based opperations to filter out unique values 
#b_by is the common elements between these two sets:
bw_by <- intersect(bw_by_st,bw_by_nw_rp_sl)
#now, st is the set of elements not common between these two sets:
st <- setdiff(bw_by_st,bw_by)
intersect(st,sn)
s$State <- ifelse(s$Store %in% st, "ST",s$State)

#nw_rp_sl is the set of elements not common between: 
nw_rp_sl <- setdiff(bw_by_nw_rp_sl,bw_by)

#by is the set of elements common: 
by <- intersect(bw_by,by_sl)
intersect(by,sn)
intersect(by,st)
s$State <- ifelse(s$Store %in% by,"BY",s$State)


#sl is not common: 
sl <- setdiff(by_sl,by)
intersect(sl,sn)
intersect(sl,st)
s$State <- ifelse(s$Store %in% sl,"SL",s$State)


#bw is not common: 
bw <- setdiff(bw_by,by)
intersect(bw,sn)
intersect(bw,st)
s$State <- ifelse(s$Store %in% bw,"BW",s$State)

#he is not common: 
he <- setdiff(bw_by_he_nw_rp_sl,bw_by_nw_rp_sl)
intersect(he,sn)
intersect(he,st)
s$State <- ifelse(s$Store %in% he,"HE",s$State)


#bb_mv_sn_th is not common:
bb_mv_sn_th <- setdiff(bb_mv_sn_st_th,st)
#bb_mv_th is not common: 
bb_mv_th <- setdiff(bb_mv_sn_th,sn)

#nw_rp_sl is not common
nw_rp_sl <- setdiff(bw_by_nw_rp_sl,bw_by)
#nw_rop is not common
nw_rp <- setdiff(nw_rp_sl,sl)

s$State <- ifelse(s$Store %in% nw_rp,"NWRP",s$State)
s$State <- ifelse(s$Store %in% bb_mv_th,"BBMVTH",s$State)
s$State <- ifelse(s$State=="Unk","BEHBHHNISH",s$State)

rm(bb_mv_sn_st_th)
rm(bb_mv_sn_th)
rm(bb_mv_th)
rm(be_hb_hh_ni_sh)
rm(bw)
rm(bw_by)
rm(bw_by_he_nw_rp_sl)
rm(bw_by_nw_rp_sl)
rm(bw_by_st)
rm(by)
rm(by_sl)
rm(he)
rm(i)
rm(nw_rp_sl)
rm(nw_rp)
rm(sl)
rm(sn)
rm(st)
rm(StateName)


s[s$State]

write.csv(s,file="StoresWithStates.csv")

################### SCHOOL HOLIDAYS #######################

#### I DON"T THINK YOU CAN DISTINGUISH STORES BEYOND THIS LEVEL ###############

rm(h)
d<- read.csv("masterdataset.csv")

#### NWRP 
nw_rp<-s[s$State=="NWRP","Store"]

sch <- d[d$Store %in% nw_rp &d$Open=="0" &d$dayofWeek %in% c(1,2,3,4,5),]
sch <-sch[sch$SchoolHoliday=="b",]
length(nw_rp)
length(unique(sch$Store))
dates <- as.Date(unique(sch$Date))
#### nw Easter = 3/25/13 - 4/6/13, RP easter = 3/20/13 - 4/5/2013

rp <- sch[sch$Store==23,]

t <- sch[sch$Date=="2013-03-20","Store"]

# 
# for(i in 1:length(dates)){
#   print(dates[i])
# }

d$Date <- as.Date(d$Date)
sch <- d[d$SchoolHoliday=="b",]
## Baden Wuerttemberg EAster : 3/25/2013 - 4/5/2013, 
dates <- unique(sch$Date)
dates
bw <- sch[sch$Date=="2013-04",]

sub<- d[d$Date=="2015-03-26",]













