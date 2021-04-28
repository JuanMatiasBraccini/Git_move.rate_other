#Movement model conditioned on recaptures only (McGarvey line of thought)

#note: individual-based model that compares the probability of occurrying in a particular zone
#      after an given time at liberty with the observed recapture zone.


library(expm)
if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

setwd(handl_OneDrive("Analyses/Movement rate estimation/Gummy and whiskery/Gummy/Individual based model"))
Dummy.dat=read.csv("Conv.tg.rec.exp.csv")

#Check prop stay prop leave
Dummy.dat$N=1
Agg=aggregate(N~Rec.zn+Rel.yr+Rel.zn,Dummy.dat,sum)
ZNS=unique(Agg$Rel.zn)
for(i in 1:length(ZNS))
{
  a=subset(Agg,Rel.zn==ZNS[i])
  yr=unique(a$Rel.yr)
  Str=vector('list',length(yr))
  for (y in 1:length(yr))
  {
    w=subset(a,Rel.yr==yr[y])
    w$Prop=w$N/sum(w$N)
    Str[[y]]=w
  }
  print(as.character(ZNS[i]))
  print(Str)
  print("#######################")
}


#movement function
fn.move=function(pars)
{
  #Put movement pars in matrix
  p.stay=pars[1]
  p.move1=pars[2]
  p.move2=1-(p.stay+p.move1)  
  Mov.mat=matrix(c(p.stay,p.move1,p.move2,
                   p.move1,p.stay,p.move2,
                   p.move1,p.move2,p.stay),nrow=3,byrow=T)
  colnames(Mov.mat)=rownames(Mov.mat)=c("West","Zone1","Zone2")
  
  #Calculate likelihood of observations
  N.rel=nrow(Dummy.dat)
  neg.LL=0  
  for(i in 1:N.rel)
  {
    #Calculate predicted probability of each observation
    d=Dummy.dat[i,]
    N.yrs=d$Rec.yr-d$Rel.yr
    if(N.yrs==0)N.yrs=1
    Move=Mov.mat %^% N.yrs 
    id.rel=which(rownames(Move)==d$Rel.zn)
    id.rec=which(colnames(Move)==d$Rec.zn)
    Pred.Prob=Move[id.rel,id.rec]
    
    #Calculate likelihoods
      #Least squares
    #neg.LL=neg.LL+(1-Pred.Prob)^2
    
      #Poisson likelihood (Haddon page 112)
    neg.LL=neg.LL+(Pred.Prob-1*log(Pred.Prob+0.00001))
    
  }
  
  return(neg.LL)
}


#Fit model
Stay=0.8
Move1=0.1
pars=c(p.stay=Stay,p.move1=Move1)


fit=optim(pars,fn.move,method="Nelder-Mead", control = list(trace=T))
fit$par


