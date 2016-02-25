require(dplyr)
d <- read.csv("masterdataset.csv")
s <- read.csv("stores.csv")

############# STATE HOLIDAYS ##################
#subset based on state holiday flag
h <- d[d$StateHoliday=="a",]
rm(d)
open_on_hol <- unique(h[h$Open==1,"Store"])

# saxony celebrates repentance day on 11/20/2013
sn <- h[h$Date=="2013-11-20","Store"] 
sn2 <- h[h$Date=="2014-11-19","Store"]
setdiff(sn,sn2)
rm(sn2)

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
intersect(sn,bw_by_he_nw_rp_sl) # saxony stores ended up in this set - 
#if saxony stores are in this set, is it reliable?????
bw_by_he_nw_rp_sl <- setdiff(bw_by_he_nw_rp_sl,sn) #remove saxony 

# 2013-08-15 is the Assumption of Mary in BY, SL
by_sl <- h[h$Date=="2013-08-15","Store"]
by_sl2 <- h[h$Date=="2014-08-15","Store"] #no stores in this 
by_sl3 <-   h[h$Date=="2015-08-15","Store"] #no stores in this 
#setdiff(by_sl,by_sl3)
rm(by_sl2)
rm(by_sl3)
intersect(sn,by_sl)

#2013-10-31 is Reformation Day in BB, MV, SN, ST, TH
bb_mv_sn_st_th <- h[h$Date=="2013-10-31","Store"]
bb_mv_sn_st_th2 <- h[h$Date=="2014-10-31","Store"]
bb_mv_sn_st_th3 <- h[h$Date=="2015-10-31","Store"] #no stores here
rm(bb_mv_sn_st_th3)
setdiff(bb_mv_sn_st_th,bb_mv_sn_st_th2)


#2013-11-01 is All Saints' Day in BW, BY, NW, RP, SL
bw_by_nw_rp_sl <- h[h$Date=="2013-11-01","Store"]#Friday
bw_by_nw_rp_sl2 <- h[h$Date=="2014-11-01","Store"]#Saturday 
bw_by_nw_rp_sl3 <- h[h$Date=="2015-11-01","Store"]#Sunday
rm(bw_by_nw_rp_sl3)
setdiff(bw_by_nw_rp_sl,bw_by_nw_rp_sl2)


# use set based opperations to filter out unique values 
#b_by is the common elements between these two sets:
bw_by <- intersect(bw_by_st,bw_by_nw_rp_sl)
#now, st is the set of elements not common between these two sets:
st <- setdiff(bw_by_st,bw_by)
intersect(st,sn)
#nw_rp_sl is the set of elements not common between: 
nw_rp_sl <- setdiff(bw_by_nw_rp_sl,bw_by)
#by is the set of elements common: 
by <- intersect(bw_by,by_sl)
intersect(by,sn)
intersect(by,st)
#sl is not common: 
sl <- setdiff(by_sl,by)
intersect(sl,sn)
intersect(sl,st)
#bw is not common: 
bw <- setdiff(bw_by,by)
intersect(bw,sn)
intersect(bw,st)
#he is not common: 
he <- setdiff(bw_by_he_nw_rp_sl,bw_by_nw_rp_sl)
intersect(he,sn)
intersect(he,st)

#bb_mv_sn_th is not common:
bb_mv_sn_th <- setdiff(bb_mv_sn_st_th,st)
#bb_mv_th is not common: 
bb_mv_th <- setdiff(bb_mv_sn_th,sn)

#nw_rp_sl is not common
nw_rp_sl <- setdiff(bw_by_nw_rp_sl,bw_by)
#nw_rop is not common
nw_rp <- setdiff(nw_rp_sl,sl)




#set all states to unknown
s$State <- "Unk"

# bw by he sl sn st    are good
StateName <- "BW"
for(i in bw){
  store <- s[s$Store==i,"Store"]
  s$State[i] <- StateName
}

# bb mv th    , nw rp      not fully assinged
StateName <- "NW_RP"
for(i in nw_rp){
  store <- s[s$Store==i,"Store"]
  s$State[i] <- StateName
}

# be hb hh ni sh       don't have distinct holidays
be_hb_hh_ni_sh <- s[s$State=="Unk","Store"]
StateName <- "BE_HB_HH_NI_SH"
for(i in be_hb_hh_ni_sh){
  store <- s[s$Store==i,"Store"]
  s$State[i] <- StateName
}

################### SCHOOL HOLIDAYS #######################

rm(h)
d <- read.csv("masterdataset.csv")

length(nw_rp)
sch <- d[match(nw_rp,d$Store),]
sch<- sch[sch$dayofWeek %in% c(1,2,3,4,5),]
sch<- sch[sch$Open=="0",]

sch <- d[d$SchoolHoliday=="a",]
#sch <- d[d$SchoolHoliday=="a",] 


sch$Date  <- as.Date(sch$Date)

sch[sch$Date=="2015-03-30",]


rm(d)
nw_rp
#NW_RP - North Rhine-Westphalia and Rhineland-Palatinate
nwrp<- sch[sch$Store %in% nw_rp,]
#NW easter: 3/25 - 4/5, RP 3/20 - 4/6
rp<- 
  nwrp[nwrp$Date=="2015-03-26","Store"]
nw <- setdiff(nw_rp,rp)
nwrp[nwrp$Store==3,]

sub <-d[d$SchoolHoliday=="a",]
rwrp <- sub[match(nw_rp,sub$Store,nomatch = 0),]


# bb mv th - Brandenburg, Mecklenburg-West Pomerania, Thuringia
bbmvth <- sch[sch$Store %in% bb_mv_th,]
bbmvth$Date <- as.Date(bbmvth$Date)
bbmvth[bbmvth$Date=="2015-01-31","Store"]

# ##  bb summer = 5/26 - 6/6, mv = 5/22 - 5/26, th = 5/15
# mv <- bbmvth[bbmvth$Date=="2015-05-25","Store"]
#  <- bbmvth[bbmvth$Date=="2013-10-31","Store"]

#### be hb hh ni sh
#Berlin, Bremen, Hamburg, Lower Saxony, Schleswig-Holstein
bbhls <- sch[match(be_hb_hh_ni_sh,sch$Store),]
bbhls[bbhls$Date=="2013-01-31",]

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
rm(store)
