#--------------------------MULTI TAGGING EXAMPLE----------------------------

#note: implementation of Hilborn 1990.
#       Three regions
#       Two years of observations
#       Two sampling events per region


rm(list=ls(all=TRUE))

##############################
#1. SIMULATED DATA
##############################

#Time
Months=12
years=1990:1991
n.step=Months*length(years) #time steps (continuous months)


#Space
regions=1:3
n.rg=length(regions)


#True population movement rates between regions
#note: no movement if regions are not adjacent
p11=.8;p13=0;p12=1-(p11+p13)
p21=.1;p22=.8;p23=1-(p22+p21)
p31=0;p33=.8;p32=1-(p33+p31)
prop_mov=matrix(c(p11,p12,p13,p21,p22,p23,p31,p32,p33),n.rg,n.rg,byrow=T)


#True catchabilities per region
q_obs=c(0.005,0.005,0.005) 
#q_mat=matrix(rep(q_obs,n.rg),n.rg,n.rg,byrow=T)   #observed q in matrix form


#True release data
#note: Six tagging groups, 2 years in each of 3 regions,tagging only during 1st month of each yr
releases=data.frame(Time=rep(c(1,13),each=3),Space=rep(regions,2),Number=rep(c(1000,1000),each=3))
releases$Tg.gp=with(releases,paste(Time,Space))
Tg.Grp=sort(unique(releases$Tg.gp))
n.tgGr=length(Tg.Grp)

STEPS=match(releases$Time,1:n.step)


#effort data
Ef=rnorm(n.step,10,2)
Effort=matrix(c(Ef,Ef,Ef),n.step,n.rg)
#Effort=matrix(c(rnorm(n.step,10,2),rnorm(n.step,10,2),rnorm(n.step,10,2)),n.step,n.rg)      

#array of numbers and recaptures for each tag group
N.tg=Rec.tg=array(NA,dim=c(n.step,n.rg,n.tgGr))

for(tg in 1:n.tgGr)
{
  #Store predicted number of tagged sharks in population and those being recaptured for tag group= tg
  N=R=matrix(NA,n.step,n.rg)
  
  #Select tag group
  dummy=subset(releases,Tg.gp==Tg.Grp[tg])
  dummy1=subset(releases,Time==dummy$Time)     #WATCH OUT IF DIFFERENT SIZE CLASSESS!!!
  tag.reg=ifelse(dummy1$Tg.gp==dummy$Tg.gp,1,0)
  Tiat=rep(dummy$Number,n.rg)*tag.reg  #initial number of tags releases from tag group= tg
  
  Start=STEPS[tg]
  for(i in Start:n.step)
  {
    h=matrix(rep(q_obs*Effort[i,],n.rg),n.rg,n.rg,byrow=T)  #exploitaiton rate per region and time step
    
    NN=NULL
    if(i==Start) 
    {
      NN=matrix(Tiat,ncol=n.rg,nrow=n.rg)      #Initial condition
    }else     
    {      
      NN=matrix(N[i-1,],ncol=n.rg,nrow=n.rg)
    }
    
    #Numbers dying from fishing and moving (second, after sampling)
    N[i,]=colSums(round(NN*prop_mov*(1-h)))
    
    #Numbers recaptured (first sampling, i.e. recapturing)
    R[i,]=round(N[i,]*q_obs*Effort[i,])    
  }
  
  #Store each tag group
  N.tg[,,tg]=N
  Rec.tg[,,tg]=R
  
}
Obs.N=N.tg
Obs.recaptures=Rec.tg



##############################
#2)     ESTIMATION
##############################


#---PARAMETERS SECTION----
#initial values
p11.init=.7;p12.init=.1
p22.init=.8;p21.init=.1
p33.init=.7;p32.init=.1

P_int=c(p11.init,p12.init,p21.init,p22.init,p32.init,p33.init)
names(P_int)=c("p11","p12","p21","p22","p32","p33")

q_init=c(0.005,0.005,0.005) 
names(q_init)=c("qR1","qR2","qR3")
pars=c(P_int,q_init)



#PROCEDURE SECTION
Pop.dyn=function(pars)
{
  Qs=pars[match(names(q_init),names(pars))]
  
  #set up for matrix multiplication
  prop_mov.pred=matrix(c(pars[1],pars[2],1-(pars[1]+pars[2]),
                         pars[3],pars[4],1-(pars[3]+pars[4]),
                         1-(pars[5]+pars[6]),pars[5],pars[6]),n.rg,n.rg,byrow=T)
  
  #array of numbers and recaptures for each tag group
  N.tg=Rec.tg=array(NA,dim=c(n.step,n.rg,n.tgGr))
  
  for(tg in 1:n.tgGr)
  {
    #Store predicted number of tagged sharks in population and those being recaptured for tag group= tg
    N=R=matrix(NA,n.step,n.rg)
    
    #Select tag group
    dummy=subset(releases,Tg.gp==Tg.Grp[tg])
    dummy1=subset(releases,Time==dummy$Time)     #WHATCH OUT IF DIFFERENT SIZE CLASSESS!!!
    tag.reg=ifelse(dummy1$Tg.gp==dummy$Tg.gp,1,0)
    Tiat=rep(dummy$Number,n.rg)*tag.reg  #initial number of tags releases from tag group= tg
    
    Start=STEPS[tg]
    for(i in Start:n.step)
    {
      h=matrix(rep(Qs*Effort[i,],n.rg),n.rg,n.rg,byrow=T)  #exploitaiton rate per region and time step
      
      
      #penalty to avoid H>1    
      eps=0.999 #maximimum accepted exploitation rate
      pen=0
      pen=sum((h>eps)*0.01*(h-eps)^2)
      h=ifelse(h>eps,eps,h)
      
      #penalty to avoid q<0    
      eps1=0.001 #minimum accepted exploitation rate
      pen=pen+sum((h<eps1)*0.01*(h-eps)^2)
      h=ifelse(h<eps1,eps1,h)
      
      
      NN=NULL
      if(i==Start) 
      {
        NN=matrix(Tiat,ncol=n.rg,nrow=n.rg)      #Initial condition
      }else     
      {      
        NN=matrix(N[i-1,],ncol=n.rg,nrow=n.rg)
      }
      
      #Numbers dying from fishing and moving (second, after sampling)
      N[i,]=colSums(round(NN*prop_mov.pred*(1-h)))
      
      #Numbers recaptured (first sampling, i.e. recapturing)
      R[i,]=round(N[i,]*Qs*Effort[i,])    
    }
    
    #Store each tag group
    N.tg[,,tg]=N
    Rec.tg[,,tg]=R
    
  }
  
  Rec_pred=c(Rec.tg)
  Rec_pred=Rec_pred[!is.na(Rec_pred)]
  recaptures=c(Obs.recaptures)
  recaptures=recaptures[!is.na(recaptures)]
  
  #negative log likelihood (kernel)
  neg.LL=sum(Rec_pred-recaptures*log(Rec_pred+0.00001))+1000*pen
  
  #dpois(observed,predicted,log=T)  #Poisson
  
  return(list(N_pred=N.tg,Rec_pred=Rec.tg,neg.LL=neg.LL))
}

#MAIN SECTION
fn_obj=function(pars)Pop.dyn(pars)$neg.LL  #objfun to minimize 
fit=optim(pars,fn_obj)
fit=optim(pars,fn_obj,method="Nelder-Mead")



#REPORT SECTION
#parameters
par(mfcol=c(2,1),mai=c(.4,1.1,.2,.2),omi=c(.3,.2,.1,.1))

Pr=fit$par
prop_mov_pred=matrix(c(Pr[1],Pr[2],p13,Pr[3],Pr[4],1-(Pr[3]+Pr[4]),
                       p31,Pr[5],Pr[6]),n.rg,n.rg,byrow=T)

barplot(rbind(c(prop_mov),c(prop_mov_pred)),beside=T,
        names.arg=c("p11","p21","p31","p12","p22","p32","p13","p23","p33"),
        legend.text = c("obs", "prd"),args.legend = list(x = "topleft",bty='n'),ylab="Mov coefficient")
box()


q_pred=fit$par[7:9]
barplot(rbind(c(q_obs),c(q_pred)),beside=T,names.arg=c("q_rg1","q_rg2","q_rg3"),ylim=c(0,max(c(q_obs,q_pred))*1.1),
        legend.text = c("obs", "prd"),args.legend = list(x = "top",bty='n'),ylab="catchability")
box()


#Numbers
N_pred=Pop.dyn(Pr)$N_pred
COL=c(2:4)
par(mfcol=c(3,2),mai=c(.4,1.1,.2,.2),omi=c(.3,.2,.1,.1))
for( i in 1:n.tgGr)
{
  N.pred=N_pred[,,i]
  Ns=Obs.N[,,i]
  plot(1:n.step,N.pred[,1],col="transparent",type='l',ylim=c(0,max(Ns,na.rm=T)*1.1),ylab="Population number",
       main=paste("tag group",Tg.Grp[i]))
  for(x in 1:ncol(Ns))
  {
    lines(1:n.step,N.pred[,x],col=COL[x],type='l')
    points(1:n.step,Ns[,x],col=COL[x],pch=19)
  }
  if(i==1)legend('topright',c("pred_rg1","pred_rg2","pred_rg3","obs_rg1","obs_rg2","obs_rg3"),
                 bty='n',col=c(COL,COL),lty=c(1,1,1,NA,NA,NA),pch=c(NA,NA,NA,19,19,19))
  
}



#Recaptures
Rec_pred=Pop.dyn(Pr)$Rec_pred
for( i in 1:n.tgGr)
{
  N.pred=Rec_pred[,,i]
  Ns=Obs.recaptures[,,i]
  plot(1:n.step,N.pred[,1],col="transparent",type='l',ylim=c(0,max(Ns,na.rm=T)*1.1),ylab="Recaptures",
       main=paste("tag group",Tg.Grp[i]))
  for(x in 1:ncol(Ns))
  {
    lines(1:n.step,N.pred[,x],col=COL[x],type='l')
    points(1:n.step,Ns[,x],col=COL[x],pch=19)
  }
  if(i==1)legend('topright',c("pred_rg1","pred_rg2","pred_rg3","obs_rg1","obs_rg2","obs_rg3"),
                 bty='n',col=c(COL,COL),lty=c(1,1,1,NA,NA,NA),pch=c(NA,NA,NA,19,19,19))
  
}



