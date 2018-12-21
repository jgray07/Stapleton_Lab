setwd("/Users/jvanillawafers/Stapleton_Lab")
dat=read.csv(("ManchingPracticeData.csv"),header=TRUE)
attach(dat)
Low_W_P=ifelse((Low.Water == 1) & (Pathogen == 1), 1, 0)
Low_N_P=ifelse((Low.Nitrogen == 1) & (Pathogen == 1), 1, 0)
All=ifelse((Low.Water == 1) & (Low.Nitrogen == 1) & (Pathogen == 1), 1, 0)
None=ifelse((Low.Water == 0) & (Low.Nitrogen == 0) & (Pathogen == 0), 1, 0)

Env=ifelse(None==T,1,
             ifelse(Low.Water==T&Low_W_N!=T&Low_W_P!=T&All!=T,2,
                    ifelse(Low.Nitrogen==T&Low_W_N!=T&Low_N_P!=T&All!=T,3,
                           ifelse(Pathogen==T&Low_W_P!=T&Low_N_P!=T&All!=T,4,
                                  ifelse(Low_W_N==T&All!=T,5,
                                         ifelse(Low_W_P==T&All!=T,6,
                                                ifelse(Low_N_P==T&All!=T,7,8)))))))


newdat=cbind(dat[,c(1:5)],Low_W_P,Low_N_P,All,None,Env,dat[,c(6:3240)])
names(newdat)           
write.csv(newdat,file="NewManchingPracticeData.csv")
