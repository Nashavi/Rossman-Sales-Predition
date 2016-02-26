sdata<-read.csv("~/Documents/UCD/BA Prac/Practicum/MasterDataSet.csv")
teststore<-subset(sdata,Store==151)
teststore$count0s=0
for (i in sort(teststore$Date)){
  teststore$lagv=teststore$count0s
  ifelse(teststore$Open==0,teststore$count0s=teststore$count0s+1,teststore$count0s=0)
  if((teststore$lagv>=7) & (teststore$count0s==0)){teststore$GO=1}
}