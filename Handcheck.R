dat = read.csv(file='familyandrandom.csv')
# Top 10 Genes 
dat = dat[,c(1,117,2353,2309,2348,2352,1430,2349,2347,2354,2358)]
dat = dat[-c(1:2),]
dat= dat[-ditch]
write.csv(dat,file = 'useinpython.csv')
hA = 0
tA = 0
sdA = c()
for (i in 1:nrow(dat)) {
  if (dat[i,2] == 'A'){
    hA = dat[i,1] + hA
    tA = tA + 1
    sdA<-c(sdA,dat[i,1])}
}

meanA = hA/tA
meanA
sq=c()
for(o in sdA){
  sqi = (o-meanA)^2
  sq = c(sq,sqi)
}

fsd = sqrt(sum(sq)/(tA-1))
fsd
#12.33972839
hB = 0
tB = 0
sdB = c()
for (i in 1:nrow(dat)) {
  if (dat[i,4] == 'B'){
    hB = dat[i,1] + hB
    tB = tB + 1
    sdB<-c(sdB,dat[i,1])}
}

meanB = hB/tB
meanB
sd(sdB)

ph = c()
for (i in 1:nrow(dat)) {
  ph = c(ph,dat[i,1])}
