#Assumptions
#1. The waters along the coast are considered to berepresented by a number of discrete linear segments 
#   of equal measure
#2. The probability of movement of a tagged shark from one segment is p, thus prob of no movement is 1-p
#3. There is equal probability of moving to either of the adjacent segments, but the proability of moving beyond 
#   those segments is 0 in the time step
#4. We extend the linear extent of the model sufficiently to cover all possible movements and return to 
#   the area of interest
#5. A shark tagged in one segement will therefore have a probabability of being in any of the 
#   other segments at a specific time in the future that is dependent on the above probability.
#6. Movement of an individual shark is random bewtwen time steps, i.e. no persistence of direction
#   moved, thus ignore repeat measures (Aldo: cover your ears!!)
#7. The probability of detection within the specific segments, if the shark is present in that 
#   segment at that time step, that contain the receiver lines is q
#8. Probability of death or tag failure or loss is 1 - exp(-M/365)

library(expm)


#DATA SECTION

#note: 1 week time step


#Coastal segments
#Segments=c("Lost",1:5)
Segments=paste("Segment",1:5,sep="")
n.seg=length(Segments)
Receiver.segments=Segments[c(2,5)]


#visualise segments and lines
plot(1:4,ylim=c(1,4),xlim=c(1,4),col="transparent",xaxt="n",yaxt="n",ylab="",xlab="")
fn.see.seg=function(X1,X2,Y1,Y2,TXT,CL)
{
    polygon(x=c(X1,X2,X2,X1),y=c(Y1,Y1,Y2,Y2),col=CL)
    text(mean(c(X1,X2)),mean(c(Y1,Y2)),TXT)
  }
fn.see.seg(4,3,1,2,'Segment 1',"white")
fn.see.seg(3,2,1,2,'Segment 2',"red")
fn.see.seg(2,1,1,2,'Segment 3',"white")
fn.see.seg(2,1,2,3,'Segment 4',"white")
fn.see.seg(2,1,3,4,'Segment 5',"red")
segments(2.5,1,2.5,2,col=4,lwd=2,lty=2)
segments(1,3.5,2,3.5,col=4,lwd=2,lty=2)
box(col="white")



#PARAMETER SECTION

#Probability of moving 
P=0.1 

#Probability of detecting
Q=0.9

#Daily loss through natural mortality, tag loss or tag failure
M=0.1
loss=exp(-M/365)



#POCEDURE SECTION

#A --Simulate movement given movement parameters and simulated tagged sharks

  #A.1 Movement simulation function 
fn.mov.sim=function(theta)
{
  #1. extract parameters
  p=theta[1]
  qq=theta[2]
  
  
  #2. Build matrices
  
    #2.1 Movement transition (rows are orign, columns are destination)
  #note: this is the underlaying movement propabilities of the population
  Mov_trans=matrix(0,nrow=n.seg,ncol=n.seg)
  rownames(Mov_trans)=colnames(Mov_trans)=Segments  
  for(i in 1:n.seg)   
  {
    #if(i==1)            Mov_trans[i,i]=1   #if lost, then stays lost
    if(i==1)            Mov_trans[i,i:(i+1)]=c(1-p,p)
    if(i>1 & i<n.seg)   Mov_trans[i,(i-1):(i+1)]=c(p/2,1-p,p/2)
    if(i==n.seg)        Mov_trans[i,(i-1):i]=c(p,1-p)
  }
  
    #2.2. add mortality/tagg failure
  LOSS=matrix(loss,nrow=n.seg,ncol=n.seg)
  Mov_trans_LOSS=Mov_trans*LOSS
    
    #2.3 Detection probability
  Detect=matrix(qq,nrow=n.seg,ncol=n.seg)
  
  
  #3. Release tag groups, move sharks for N time and detect sharks
  Tag.gp=unique(Rel_shk$Tag.Group)
  n.t.g=length(Tag.gp)
  Store.pred=vector('list',length=n.t.g)
  names(Store.pred)=Tag.gp
  Store.Rel=Store.pred
  for(x in 1:n.t.g)
  {
      #select tag group information
    dd=subset(Rel_shk,Tag.Group==Tag.gp[x])
    N.time=Sim.time-dd$Week
    Rel=rep(0,length(rownames(Mov_trans)))
    Rel[match(as.character(dd$Segments),rownames(Mov_trans))]=dd$N.rel
    
    Mov_trans_N.time=Mov_trans_LOSS %^% N.time          #move over N.time
    Mov_trans_N.time_detect= Mov_trans_N.time* Detect   #detect sharks    
    N_shrk=Mov_trans_N.time_detect * Rel                #release sharks 
    Pred_numbers=N_shrk[match(as.character(dd$Segments),rownames(Mov_trans)),]
    Store.pred[[x]]=Pred_numbers
    Store.Rel[[x]]=dd
  }
  
  Store.pred=do.call(rbind,Store.pred)
  Store.Rel=do.call(rbind,Store.Rel)
  
  #4. Objective function  
  if(Simulation=="YES") nloglike=0
  
  if(Simulation=="NO")
  {
    obs=nrow(DATA)
    nloglike=rep(NA,obs)
    for(n in 1:obs)
    {
      observed=DATA[n,]
      predicted=Store.pred[n,]                #issue1: should only compare segment with receiver line as there are no data for segments
      predicted.prop=predicted/sum(predicted)         # without receiver lines?
                                      #issue2: should add multiple comparisons each time a shark was detected (random effects?)
      
      #multinomial log like
      nloglike[n]=sum(-observed*log(predicted.prop))
    }
    nloglike=sum(nloglike)
  }
  
  
  return(list(nloglike=nloglike,Sim_det=Store.pred,Sim.rel=Store.Rel))
  
}


  #A.2. Simulated release data set
#note: release 10 sharks in different Segments in week 1
N.segs=5
N.shks=10
Rel_shk.sim=data.frame(Week=rep(1,N.segs),
            Segments=as.character(paste("Segment",1:N.segs,sep="")),
            N.rel=rep(N.shks,N.segs))
Rel_shk.sim$Tag.Group=as.character(with(Rel_shk.sim,paste(Week,Segments)))  #Tag groups

Sim.weeks=10  #number of monitoring weeks


  #A.3 Execute movement simulation function
Simulation="YES"
Rel_shk=Rel_shk.sim
Sim.time=Sim.weeks
Sim.mov=fn.mov.sim(theta=c(P=P,Q=Q))


  #A.4 Extract simulated data
Sim.mov.observations=Sim.mov$Sim_det
Sim.rel.data=Sim.mov$Sim.rel


  #A.5 Fit simulated data
Simulation="NO"
DATA=round(Sim.mov.observations)
Rel_shk=Sim.rel.data

theta1=c(P=0.2,Q=0.8)
objfun=function(theta1){fn.mov.sim(theta1)$nloglike}
fit=optim(theta1,objfun, hessian=T, control=c(trace=1, maxit=10000))   
v_ob=solve(fit$hessian)  

#apply parameters to function
Best.fit=fn.mov.sim(fit$par)

#compare simulated and estimated 
fn.compare=function(D1,D2)
{
  a=nrow(D1)
  for(i in 1:a)
  {
    X=1:length(D1[i,])
    plot(X,D1[i,],type='h',xlab="Segments",ylab="Numbers",main=paste("week=", rownames(D1)[i],sep=""))
    points(X+.1,D2[i,],type='h',col=2)
    if(i==1)legend("topright",c("Observed","Predicted"),lty=c(1,1),col=c(1,2),bty='n',cex=1)
  }
}

par(mfcol=c(3,2))
fn.compare(D1=DATA,D2=Best.fit$Sim_det)
plot(1:10,col="transparent",ann=F,xaxt='n',yaxt='n',main="Parameter values")
box(col="white")
legend("topleft",paste(names(theta),"=",theta),bty='n',text.col=1,cex=1.5)
legend("bottomright",paste(names(fit$par),"=",round(fit$par,3)),bty='n',text.col=2,cex=1.5)




#Random effect: multiply the detection prob by a value (for each shark) drawn from distribution



 

#B --Observed movement
Obs.det=data.frame(
  week.rel=rep("one",15),
  segment.rel=rep("1",15),
  TagCode=rep(c("GS.1","GS.2","GS.3"),each=5),
  week=rep(1:5,3),
  segment=c(1,1,1,2,2,1,2,3,4,5,1,2,2,3,3))
Obs.det$TagGroup=paste(Obs.det$segment.rel,Obs.det$week.rel)



#NOT USED
#   #4. Loop over each shark
#   shks=unique(Obs.det$TagCode)
#   n.shk=length(shks)
#   for(s in 1:n.shk)
#   {
#     dat=subset(Obs.det,TagCode==shks[s])
#     
#     #4.1 find release segment of each sharks
#     id=match(as.character(dat$segment[1]),rownames(Mov_trans))
#     Rel_shk[id]=1
#     
#     #4.2 Loop over each detected day
#     days=unique(dat$day[-1])
#     n.days=length(days)
#     for(d in 1:n.days)
#     {
#       dd=subset(dat,day==days[d])
#       N.days=dd$day-dat$day[1]
#       Mov_trans_N.days=Mov_trans %^% N.days  #move over N.days
#       Mov_trans_N.days_detect= Mov_trans_N.days* Detect  #detect
#       Mov_trans_shrk=Mov_trans_N.days_detect * Rel_shk   #seed shark
#       Mov_trans_shrk=Mov_trans_shrk[id,]
#       
#       #Find segments where receiver lines are located
#      # PROBS=Mov_trans_shrk[]
#       
#      }
#      
#   }
