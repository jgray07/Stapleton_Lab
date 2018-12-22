setwd("/Users/jvanillawafers/Stapleton_Lab")
dat=read.csv(("ManchingPracticeData.csv"),header=TRUE)
attach(dat)
Low_W_P=ifelse((Low.Water == 1) & (Pathogen == 1), 1, 0)
Low_N_P=ifelse((Low.Nitrogen == 1) & (Pathogen == 1), 1, 0)
All=ifelse((Low.Water == 1) & (Low.Nitrogen == 1) & (Pathogen == 1), 1, 0)
None=ifelse((Low.Water == 0) & (Low.Nitrogen == 0) & (Pathogen == 0), 1, 0)
LowWater=ifelse(Low.Water==1&Low_W_N!=1&Low_W_P!=1&All!=1,1,0)
LowNitrogen=ifelse(Low.Nitrogen==1&Low_N_P!=1&Low_W_N!=1&All!=1,1,0)
pathogen=ifelse(Pathogen==1&Low_W_P!=1&Low_N_P!=1&All!=1,1,0)
LowWN=ifelse(Low_W_N==1&All!=1,1,0)
LowWP=ifelse(Low_W_P==1&All!=1,1,0)
LowNP=ifelse(Low_N_P==1&All!=1,1,0)
Env=ifelse(None==T,1,
           ifelse(LowWater==T&Low_W_N!=T&Low_W_P!=T&All!=T,2,
                  ifelse(LowNitrogen==T&Low_W_N!=T&Low_N_P!=T&All!=T,3,
                         ifelse(pathogen==T&Low_W_P!=T&Low_N_P!=T&All!=T,4,
                                ifelse(Low_W_N==T&All!=T,5,
                                       ifelse(Low_W_P==T&All!=T,6,
                                              ifelse(Low_N_P==T&All!=T,7,8)))))))


newdat=cbind(Height,LowWater,LowNitrogen,pathogen,LowWN,LowWP,LowNP,All,None,Env,dat[,c(6:3246)])
names(newdat)           
write.csv(newdat,file="FixedManchingPracticeData.csv",na="")
