#Movement model conditioned on recaptures only (McGarvey line of thought)

#note: individual-based model that compares the probability of occurrying in a particular zone
#      after an given time at liberty with the observed recapture zone.


library(expm)

time.liberty=rep(c(0,0,0,0,1,1,1,1,2,2),3)
Dummy.dat=data.frame(
  Rel.zn=rep(c("West","Zn1","Zn2"),each=10),
  Rel.yr=rep(1:10,3),
  Rec.zn=c(rep("West",7),"Zn1","Zn1","Zn2",
           rep("Zn1",6),"West","West","Zn2","Zn2",
           rep("Zn2",8),"Zn1","Zn1")
  )
Dummy.dat$Rec.yr=Dummy.dat$Rel.yr+time.liberty


Stay=0.8
Move1=0.1
pars=c(p.stay=Stay,p.move1=Move1)
original.pars=pars

fn.move=function(pars)
{
  #Put movement pars in matrix
  p.stay=pars[1]
  p.move1=pars[2]
  p.move2=1-(p.stay+p.move1)  
  Mov.mat=matrix(c(p.stay,p.move1,p.move2,
                   p.move1,p.stay,p.move2,
                   p.move1,p.move2,p.stay),nrow=3,byrow=T)
  colnames(Mov.mat)=rownames(Mov.mat)=c("West","Zn1","Zn2")
  
  #Calculate likelihood of observations
  N.rel=nrow(Dummy.dat)
  neg.LL=0
  
  for(i in 1:N.rel)
  {
    d=Dummy.dat[i,]
    N.yrs=d$Rec.yr-d$Rel.yr
    if(N.yrs==0)N.yrs=1
    Move=Mov.mat %^% N.yrs 
    id.rel=which(rownames(Move)==d$Rel.zn)
    id.rec=which(colnames(Move)==d$Rec.zn)
    Pred.Prob=Move[id.rel,id.rec]
    #Least squares
    #neg.LL=neg.LL+(1-Pred.Prob)^2
    
    #Poisson likelihood (Haddon page 112)
    neg.LL=neg.LL+(Pred.Prob-1*log(Pred.Prob+0.00001))
    
  }
  
  
  return(neg.LL)
}

fit=optim(pars,fn.move,method="Nelder-Mead", control = list(trace=T))



#Simulated release

RELS=data.frame(Rel.yr=1:5,West=rep(10,5),Zn1=rep(10,5),Zn2=rep(10,5))
time.liberty=5
fn.sim=function(pars,RELS,Error)
{
  #Put movement pars in matrix
  p.stay=pars[1]
  p.move1=pars[2]
  p.move2=1-(p.stay+p.move1)  
  Mov.mat=matrix(c(p.stay,p.move1,p.move2,
                   p.move1,p.stay,p.move2,
                   p.move1,p.move2,p.stay),nrow=3,byrow=T)
  colnames(Mov.mat)=rownames(Mov.mat)=c("West","Zn1","Zn2")
  
    N.rel=nrow(RELS)
    STORE=vector("list",length=N.rel)
    for(i in 1:N.rel)
    {
      a=RELS[i,]
      store=vector("list",length=3)
      for(j in 2:4)
      {
        b=a[,j]
        rel.zn=colnames(a)[j]
        N.yrs=time.liberty-a$Rel.yr
        if(N.yrs==0) N.yrs=1
        Move=Mov.mat %^% N.yrs 
        id.rel=which(rownames(Move)==rel.zn)
        pred=jitter(b*Move[id.rel,],Error)
        Pred=as.data.frame(matrix(pred,ncol=3))
        colnames(Pred)=names(pred)
        Pred.exp=matrix(c(rep("West",Pred[1]),rep("Zn1",Pred[2]),rep("Zn2",Pred[3])))
        
        store[[j]]=data.frame(Rel.zn=rel.zn,Rel.yr=a$Rel.yr,Rec.zn=Pred.exp,Rec.yr=time.liberty)
      }
      STORE[[i]]=do.call(rbind,store)
    }
    Pred.recapture=do.call(rbind,STORE)   
  return(Pred.recapture)
}



#test simulated data
n.sim=10
jitter.error=seq(0,1.5,length.out=n.sim)
store.fit.sim=vector('list',n.sim)
for (s in 1:n.sim)
{
  Dummy.dat=fn.sim(original.pars,RELS,jitter.error[s])
  fit=optim(c(0.7,0.2),fn.move,method="Nelder-Mead")
  store.fit.sim[[s]]=fit$par
}

#compare original parameters and those obtained from simulation
Sims=matrix(unlist(store.fit.sim),ncol=2,byrow=T)

par(mfcol=c(2,1))
plot(jitter.error,Sims[,1],ylim=c(0,1))
abline(h=fit$par[1])

plot(jitter.error,Sims[,2],ylim=c(0,1))
abline(h=fit$par[2])
mtext("Added error",1,line=2,cex=2)
