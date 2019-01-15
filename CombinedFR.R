setwd("~/TBillman/Stapleton-Lab/vQTL Random and Family/data/tidied")
familydata=read.csv("Family.csv",header=T)
randomdata=read.csv("Random2.csv",header=T)
attach(familydata)
attach(randomdata)
plotstructure=rep(1,1212)
dat=cbind(familydata[,c(1:3)],plotstructure,familydata[,c(4:3238)])
plotstructure=rep(2,870) 
datt=cbind(randomdata[,c(1:3)],plotstructure,randomdata[,c(4:3238)])
write.csv(dat,file="newfamily.csv")
write.csv(datt,file="newrandom.csv")
newfamily=read.csv("newfamily.csv")
newrandom=read.csv("newrandom.csv")
newfamily=newfamily[,-1]
newrandom=newrandom[,-1]

combined=rbind(newfamily,newrandom)
editcombined=combined[-c(1213:1214),]            

write.csv(editcombined,file="familyandrandom.csv",na="")
