setwd("~/TBillman/Stapleton-Lab/vQTL Random and Family/data/tidied")
data=read.csv("familyandrandom.csv",header=T)
practicedata=data[c(1:18,1243:1256),]
write.csv(practicedata,file="familyrandomsample.csv",na="")
