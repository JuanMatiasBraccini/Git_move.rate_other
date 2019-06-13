# Application of Hilborn et al 1990 to shark tagging data

#note: calculate 2 sets of movement parameters, one for juveniles, one for adults


rm(list=ls(all=TRUE))

# SHEDDING DATA


# EFFORT DATA
Effort.zn=read.csv("C:/Matias/Data/Population dynamics/Data inputs for models/Gummy/2016/effort.annual.by.zone.TDGDLF.csv")

Effort.zn=rbind(Effort.zn,Effort.zn[nrow(Effort.zn),])   #add dummy 2014 effort  
Effort.zn$FINYEAR=as.numeric(substr(Effort.zn$FINYEAR,1,4))
Effort.zn$FINYEAR[nrow(Effort.zn)]=2014

# REPORTING RATE DATA


# TAGGING DATA
hndl="C:/Matias/Data/Tagging/Pop dyn model/Conventional/"

#at Size
  #dusky
Rel.dus_adu=read.csv(paste(hndl,"BW_Zn.rel.adul_Conv.Tag_size.csv",sep=""))
Rel.dus_juv=read.csv(paste(hndl,"BW_Zn.rel.juv_Conv.Tag_size.csv",sep=""))
Rec.dus_adu=read.csv(paste(hndl,"BW_Zn.rec.adul_Conv.Tag_size.csv",sep=""))
Rec.dus_juv=read.csv(paste(hndl,"BW_Zn.rec.juv_Conv.Tag_size.csv",sep=""))

  #sandbar
Rel.san_adu=read.csv(paste(hndl,"TK_Zn.rel.adul_Conv.Tag_size.csv",sep=""))
Rel.san_juv=read.csv(paste(hndl,"TK_Zn.rel.juv_Conv.Tag_size.csv",sep=""))
Rec.san_adu=read.csv(paste(hndl,"TK_Zn.rec.adul_Conv.Tag_size.csv",sep=""))
Rec.san_juv=read.csv(paste(hndl,"TK_Zn.rec.juv_Conv.Tag_size.csv",sep=""))

  #gummy
Rel.gum_adu=read.csv(paste(hndl,"GM_Zn.rel.adul_Conv.Tag_size.csv",sep=""))
Rel.gum_juv=read.csv(paste(hndl,"GM_Zn.rel.juv_Conv.Tag_size.csv",sep=""))
Rec.gum_adu=read.csv(paste(hndl,"GM_Zn.rec.adul_Conv.Tag_size.csv",sep=""))
Rec.gum_juv=read.csv(paste(hndl,"GM_Zn.rec.juv_Conv.Tag_size.csv",sep=""))

  #whiskery
Rel.whi_adu=read.csv(paste(hndl,"WH_Zn.rel.adul_Conv.Tag_size.csv",sep=""))
Rel.whi_juv=read.csv(paste(hndl,"WH_Zn.rel.juv_Conv.Tag_size.csv",sep=""))
Rec.whi_adu=read.csv(paste(hndl,"WH_Zn.rec.adul_Conv.Tag_size.csv",sep=""))
Rec.whi_juv=read.csv(paste(hndl,"WH_Zn.rec.juv_Conv.Tag_size.csv",sep=""))

Stay=0.85
Move=0.1

PARS.ad=list(WH=c(w_w=Stay,w_z1=Move,
                  z1_w=Move,z1_z1=Stay,
                  z2_z1=Move,z2_z2=Stay,
                  qw=1e-3,qz1=1e-3,qz2=1e-3),
             GM=c(w_w=Stay,w_z1=Move,
                  z1_w=Move,z1_z1=Stay,
                  z2_z1=Move,z2_z2=Stay,
                  qw=1e-3,qz1=1e-3,qz2=1e-3),
             BW=c(n_n=0.8,n_c=0.1,n_w=0.1,n_z1=0,n_z2=0,
                  c_n=0.1,c_c=0.8,c_w=0.1,c_z1=0,c_z2=0,
                  w_n=0,w_c=0.1,w_w=0.8,w_z1=0.1,w_z2=0,
                  z1_n=0,z1_c=0,z1_w=0.1,z1_z1=0.8,z1_z2=0.1,
                  z2_n=0,z2_c=0,z2_w=0.1,z2_z1=0.1,z2_z2=0.8,
                  qn=1e-6,qc=1e-6,qw=1e-6,qz1=1e-6,qz2=1e-6),
             TK=c(n_n=0.8,n_c=0.1,n_w=0.1,n_z1=0,
                  c_n=0.1,c_c=0.8,c_w=0.1,c_z1=0,
                  w_n=0,w_c=0.1,w_w=0.8,w_z1=0.1,
                  z1_n=0,z1_c=0.1,z1_w=0.1,z1_z1=0.8,
                  qn=1e-6,qc=1e-6,qw=1e-6,qz1=1e-6))

PARS.ju=list(WH=c(w_w=Stay,w_z1=Move,
                  z1_w=Move,z1_z1=Stay,
                  z2_z1=Move,z2_z2=Stay,
                  qw=1e-3,qz1=1e-3,qz2=1e-3),
             GM=c(w_w=Stay,w_z1=Move,
                  z1_w=Move,z1_z1=Stay,
                  z2_z1=Move,z2_z2=Stay,
                  qw=1e-3,qz1=1e-3,qz2=1e-3),
             BW=c(n_n=0.8,n_c=0.1,n_w=0.1,n_z1=0,n_z2=0,
                  c_n=0.1,c_c=0.8,c_w=0.1,c_z1=0,c_z2=0,
                  w_n=0,w_c=0.1,w_w=0.8,w_z1=0.1,w_z2=0,
                  z1_n=0,z1_c=0,z1_w=0.1,z1_z1=0.8,z1_z2=0.1,
                  z2_n=0,z2_c=0,z2_w=0.1,z2_z1=0.1,z2_z2=0.8,
                  qn=1e-6,qc=1e-6,qw=1e-6,qz1=1e-6,qz2=1e-6),
             TK=c(n_n=0.8,n_c=0.1,n_w=0.1,n_z1=0,n_z2=0,
                  c_n=0.1,c_c=0.8,c_w=0.1,c_z1=0,c_z2=0,
                  w_n=0,w_c=0.1,w_w=0.8,w_z1=0.1,w_z2=0,
                  z1_n=0,z1_c=0,z1_w=0.1,z1_z1=0.8,z1_z2=0.1,
                  z2_n=0,z2_c=0,z2_w=0.1,z2_z1=0.1,z2_z2=0.8,
                  qn=1e-6,qc=1e-6,qw=1e-6,qz1=1e-6,qz2=1e-6))


#Which species to analyse
SPecies=c("WH","GM")
#SPecies=c("WH","GM","BW","TK")
n.sp=length(SPecies)

if(n.sp==4)
{
  RELEASE=list(Adults=list(Rel.whi_adu,Rel.gum_adu,Rel.dus_adu,Rel.san_adu),
               Juveniles=list(Rel.whi_juv,Rel.gum_juv,Rel.dus_juv,Rel.san_juv))
  
  RECAPTURE=list(Adults=list(Rec.whi_adu,Rec.gum_adu,Rec.dus_adu,Rec.san_adu),
                 Juveniles=list(Rec.whi_juv,Rec.gum_juv,Rec.dus_juv,Rec.san_juv))  
}
  
if(n.sp==2)
{
  RELEASE=list(Adults=list(Rel.whi_adu,Rel.gum_adu),Juveniles=list(Rel.whi_juv,Rel.gum_juv))  
  RECAPTURE=list(Adults=list(Rec.whi_adu,Rec.gum_adu),Juveniles=list(Rec.whi_juv,Rec.gum_juv))
}

names(RELEASE$Adults)=names(RELEASE$Juveniles)=
names(RECAPTURE$Adults)=names(RECAPTURE$Juveniles)=SPecies


fn.estim.mov.rate=function(releases,Obs.recaptures,pars)
{
  Last.yr=max(Obs.recaptures$Yr.rec)
  First.yr=min(releases$Yr.rel)
  YRS=First.yr:Last.yr
  n.step=length(YRS)
  
  Tg.Grp=sort(as.character(unique(releases$TG.zn)))
  n.tgGr=length(Tg.Grp)
  names(releases)[match("Yr.rel",names(releases))]="Time" 
  if(n.sp==4)releases$Rel.zone=factor(as.character(releases$Rel.zone),levels=c("North","Closed","West","Zone1","Zone2"))
  if(n.sp==2)releases$Rel.zone=factor(as.character(releases$Rel.zone),levels=c("West","Zone1","Zone2"))
  Obs.recaptures=subset(Obs.recaptures,Rec.zone%in%Rel.zns)

   #Estimate parameters
  Pop.dyn=function(pars)
  {
    #extract parameters
    Qs=pars[grepl("q",substr(names(pars),1,1))]
    Ps=pars[!grepl("q",substr(names(pars),1,1))]  
    n.rg=length(Rel.zns)
    
    #set up for matrix multiplication
    Ps=c(Ps[1:2],w_z2=1-(Ps[1]+Ps[2]),
         Ps[3:4],z1_z2=1-(Ps[3]+Ps[4]),
         z2_w=1-(Ps[5]+Ps[6]),Ps[5:6])
    prop_mov.pred=matrix(Ps,n.rg,n.rg,byrow=T)
    
    #array of numbers and recaptures for each tag group
    N.tg=array(NA,dim=c(n.step,n.rg,n.tgGr))
    colnames(N.tg)=Rel.zns
    rownames(N.tg)=YRS
    Rec.tg=Rec.tg.obs=Obs.tg=N.tg
    
    #loop over each tag group to calculate numbers and recaptures at time and space
    for(tg in 1:n.tgGr)
    {
      #Store predicted number of tagged sharks in population and those being recaptured for tag group= tg
      N=matrix(NA,n.step,n.rg)
      colnames(N)=Rel.zns
      rownames(N)=YRS
      R=Obs.R=R.compare=N
      
      #Select tag group
      dummy=subset(releases,TG.zn==Tg.Grp[tg])
      tag.reg=rep(0,n.rg)
      tag.reg[match(dummy$Rel.zone,Rel.zns)]=1
      Tiat=rep(dummy$Number,n.rg)*tag.reg  #initial number of tags releases from tag group= tg
      
      
      Start=dummy$Time
      End=Last.yr
      for(i in Start:End)
      {
        Effort=unlist(Effort.zn[match(i,Effort.zn$FINYEAR),2:4])
        h=matrix(rep(Qs*Effort,n.rg),n.rg,n.rg,byrow=T)  #exploitaiton rate per region and time step
        
        #penalty to avoid H>1    
        eps=0.999 #maximimum accepted exploitation rate
        pen=0
        pen=sum((h>eps)*0.01*(h-eps)^2)
        h=ifelse(h>eps,eps,h)
        
        #penalty to avoid q<0    
        eps1=0.001 #minimum accepted exploitation rate
        pen=pen+sum((h<eps1)*0.01*(h-eps)^2)
        h=ifelse(h<eps1,eps1,h)
        
        indx=match(i,YRS)
        NN=NULL
        if(i==Start) 
        {
          NN=matrix(Tiat,ncol=n.rg,nrow=n.rg)      #Initial condition
        }else     
        {      
          NN=matrix(N[indx-1,],ncol=n.rg,nrow=n.rg)
        }
        
        #Numbers dying from fishing and moving (second, after sampling)
        N[indx,]=colSums(NN*prop_mov.pred*(1-h))
        
        #Numbers recaptured 
        R[indx,]=N[indx,]*Qs*Effort    
      }
      
      #Store each tag group
      N.tg[,,tg]=N
      Rec.tg[,,tg]=R
      
      
      #Fill in observed recaptures
      obs.recaptures=subset(Obs.recaptures,TG.zn==Tg.Grp[tg])
      area.index=match(obs.recaptures$Rec.zone,Rel.zns)
      time.index=match(obs.recaptures$Yr.rec,YRS)
      replc=cbind(time.index,area.index)
      
      if(nrow(replc)>0)
      {
        for(l in 1:nrow(obs.recaptures)) Obs.R[replc[l,1],replc[l,2]]=obs.recaptures$Number[l]
        
        #Extract observations and predictions for those observations
        for(l in 1:nrow(obs.recaptures)) R.compare[replc[l,1],replc[l,2]]=R[replc[l,1],replc[l,2]]
        Rec.tg.obs[,,tg]=R.compare
        Obs.tg[,,tg]=Obs.R
      }
      
    }
    
    #Observed vs Predicted
    predicted=c(Rec.tg.obs)
    predicted=predicted[!is.na(predicted)]
    
    observed=c(Obs.tg)
    observed=observed[!is.na(observed)]
    
    Residuals=observed-predicted
    
    #Poission negative log like
    neg.LL=sum(dpois(observed,predicted,log=T))  
    
    
    return(list(Rec_obs=Obs.tg,Rec_pred=Rec.tg.obs,neg.LL=neg.LL,Residuals=Residuals))
  }
  fn_obj=function(pars)Pop.dyn(pars)$neg.LL  #objfun to minimize 
  fit=optim(pars,fn_obj,method="Nelder-Mead")
  
  #Execute function with optimised pars
  fn.run=Pop.dyn(fit$par) 
  
  return(list(fn.run=fn.run,fit=fit))
}


Rel.zns=as.character(c("West","Zone1","Zone2"))

Store.Ad=vector('list',n.sp)
names(Store.Ad)=SPecies
Store.ju=Store.Ad
for(s in 1:n.sp) 
{
    
  #Adults
  Store.Ad[[s]]=fn.estim.mov.rate(releases=RELEASE$Adults[[s]],
                    Obs.recaptures=RECAPTURE$Adults[[s]],
                    pars=PARS.ad[[s]])
  #Juveniles
  Store.ju[[s]]=fn.estim.mov.rate(releases=RELEASE$Juveniles[[s]],
                    Obs.recaptures=RECAPTURE$Juveniles[[s]],
                    pars=PARS.ju[[s]])
}














#############################################################NOT USED #####################

# 
# all.yrs=sort(unique(as.character(levels(Rel.san$yr))))
# all.yrs=as.factor(all.yrs)
# N.sp=length(RELEASE)
# 
# 
# 
# 
# 
# 
# #View releases and recoveries
# fn.view.rel.rec=function(dat,dat1)
# {
#   dat=subset(dat,!yr=="2013-14" & !area=="North")
#   dat1=subset(dat1,!year=="2013-14")
#   dat$yr=factor(as.character(dat$yr),levels=all.yrs)
#   dat1$year=factor(as.character(dat1$year),levels=all.yrs)
#   Rel=aggregate(Nrelease~yr+area,dat,sum)
#   Rec=aggregate(Number~year+area,dat1,sum)
#   
#   wide <- reshape(Rel, v.names = "Nrelease", idvar = "yr",timevar = "area", direction = "wide")
#   id=all.yrs[which(!all.yrs%in%as.character(wide$yr))]
#   if(length(id>0))
#   {
#     plus=wide[1:length(id),]
#     plus$yr=id
#     plus[,2:ncol(plus)]=0
#     wide=rbind(wide,plus)
#     wide=wide[order(wide$yr),]
#   }
#   wide[is.na(wide)]=0
#   
#   wide.rec <- reshape(Rec, v.names = "Number", idvar = "year",timevar = "area", direction = "wide")
#   id=all.yrs[which(!all.yrs%in%as.character(wide.rec$year))]
#   if(length(id>0))
#   {
#     plus=wide.rec[1:length(id),]
#     plus$year=id
#     plus[,2:ncol(plus)]=0
#     wide.rec=rbind(wide.rec,plus)
#     wide.rec=wide.rec[order(wide.rec$year),]
#   }
#   wide.rec[is.na(wide.rec)]=0
#   
#   Zns=sort(unique(dat$area))
#   Zns.rec=sort(unique(dat1$area))
#   for(r in 1:length(Zns))
#   {
#     aa=wide[,r+1]
#     bb=wide.rec[,r+1]
#     plot(1:length(all.yrs),1:length(all.yrs),ylim=c(0,max(aa)),col="transparent",main=Zns[r],
#          xaxt='n',ylab="",xlab="")
#     lines(1:length(all.yrs),aa,col=1,lwd=2)
#     
#     par(new=TRUE)
#     plot(1:length(all.yrs),bb,type="l",col=2,lwd=2,xaxt="n",yaxt='n',xlab="",ylab="")
#     axis(4)
#     
#     
#     lines(1:length(all.yrs),bb,col=2,lwd=2)
#     axis(1,seq(1,length(all.yrs),5),all.yrs[seq(1,length(all.yrs),5)],tck=-0.04)
#     axis(1,seq(1,length(all.yrs),1),F,tck=-0.02)
#     if(r==1)legend("topright",c("release","recapture"),bty='n',lty=1,lwd=2,col=1:2,cex=1.25)
#   }
#   mtext("Financial year",1,outer=T,line=-2,cex=2)
#   mtext("Number of tag releases",2,outer=T,las=3,line=-2,cex=2)
#   mtext("Number of tag recaptures",side=4,outer=T,las=3,line=-2,cex=2)
#   
#   
# }
# 
# setwd("C:/Matias/Analyses/Population dynamics/Conventional_outputs")
# 
# for(i in 1:N.sp)
# {
#   tiff(file=paste(SPecies[i],"number.rel.rec.tiff"),width = 2400, height = 2400,units = "px", res = 300,compression = "lzw")
#   par(mfcol=c(2,2),las=1,cex.axis=1.25,mai=c(.9,.9,.3,.8),cex.main=1.5)
#   fn.view.rel.rec(RELEASE[[i]],RECAPTURE[[i]]) 
#   dev.off()
# }
# 
# 
# #Aggregate by size class: juveniles and adults for sandbar, all together for other species
# #note: annual time step  (too few tags for monthly)
# #       Also, some tag groups have very small sample sizes (e.g. 5 individuals) and were not recaptured
# #       hence, I removed any with no recaptures
# 
# Ag.sandbar=12
# #Min.Rel=5  #minimum number of shark released per tag group to keep in analysis
# 
# fn.group=function(dat,dat1,SPEC)
# {
#   #all.yrs=levels(dat$yr)
#   if(SPEC=="TK")
#   {
#     #releases
#     dat$size=with(dat,ifelse(Age<Ag.sandbar,"Juv","Adul"))
#     dat$Tg.gp=paste(dat$yr,dat$area,dat$size)
#     ag=aggregate(Nrelease~yr+area+Tg.gp,dat,sum) 
#     colnames(ag)[match("Nrelease",colnames(ag))]="Number"
#     #     wide <- reshape(ag, v.names = "Nrelease", idvar = c("yr","size"),
#     #                     timevar = "area", direction = "wide")
#     #     wide[is.na(wide)]=0
#     #     
#     #     wide.Ad=subset(wide,size=="Adul")
#     #     id=all.yrs[which(!all.yrs%in%as.character(wide.Ad$yr))]
#     #     if(length(id>0))
#     #     {
#     #       plus=wide.Ad[1:length(id),]
#     #       plus$yr=id
#     #       plus[,3:ncol(plus)]=0
#     #       wide.Ad=rbind(wide.Ad,plus)
#     #       wide.Ad=wide.Ad[order(wide.Ad$yr),]
#     #     }
#     #     
#     #     wide.Ju=subset(wide,size=="Juv")
#     #     id=all.yrs[which(!all.yrs%in%as.character(wide.Ju$yr))]
#     #     if(length(id>0))
#     #     {
#     #       plus=wide.Ju[1:length(id),]
#     #       plus$yr=id
#     #       plus[,3:ncol(plus)]=0
#     #       wide.Ju=rbind(wide.Ju,plus)
#     #       wide.Ju=wide.Ju[order(wide.Ju$yr),]
#     #     }
#     #     wide=rbind(wide.Ju,wide.Ad)
#     
#     #recaptures
#     this.gps=match(dat1$TG,dat$TG)
#     dat1$Tg.gp=dat$Tg.gp[this.gps]
#     ag.rec=aggregate(Number~year+area+Tg.gp,dat1,sum)    
#     
#     
#   }else
#   {
#     #releases
#     dat$Tg.gp=paste(dat$yr,dat$area)
#     ag=aggregate(Nrelease~yr+area+Tg.gp,dat,sum)    
#     colnames(ag)[match("Nrelease",colnames(ag))]="Number"
#     #wide <- reshape(ag, v.names = "Nrelease", idvar ="yr",timevar = "area", direction = "wide")
#     #wide[is.na(wide)]=0
#     
#     #recaptures
#     this.gps=match(dat1$TG,dat$TG)
#     dat1$Tg.gp=dat$Tg.gp[this.gps]
#     ag.rec=aggregate(Number~year+area+Tg.gp,dat1,sum)    
#     # wide.rec <- reshape(ag, v.names = "Number", idvar ="year",timevar = "area", direction = "wide")
#     #  wide.rec[is.na(wide.rec)]=0
#     
#   }
#   
#   
#   #remove tag groups with no recaptures
#   #ag=subset(ag,Number<=Min.Rel)
#   ag=subset(ag,Tg.gp%in%unique(ag.rec$Tg.gp))
#   
#   
#   return(list(rels=ag,recs=ag.rec))
# }
# 
# RELS=RECS=RELEASE
# for(i in 1:N.sp)
# {
#   dummy=fn.group(RELEASE[[i]],RECAPTURE[[i]],SPecies[[i]])
#   RELS[[i]]=dummy$rels
#   RECS[[i]]=dummy$recs  
# }
# 
# 
# 
# #Put data as array
# 
# x=substr(1994:2013,start=3,stop=4)
# FinYrs=paste(1993:2012,"-",x,sep="")
# n.step=length(FinYrs) #time steps
# 
# Effort=as.matrix(Effort.zn[match(FinYrs,Effort.zn$FINYEAR),2:ncol(Effort)])
# 
# fn.array=function(dat,dat1)
# {
#   dat=subset(dat,yr%in%FinYrs)
#   Tg.Grp=sort(unique(dat$Tg.gp))
#   n.tgGr=length(Tg.Grp)
#   Rgs=unique(c(as.character(dat$area),as.character(dat1$area)))
#   n.rg=length(Rgs)
#   
#   STEPS=match(dat$yr,FinYrs)
#   
#   #array of recaptures for each tag group
#   Rec.tg=array(NA,dim=c(n.step,n.rg,n.tgGr))
#   for(tg in 1:n.tgGr)
#   {
#     dummy=subset(dat1,Tg.gp==Tg.Grp[tg])
#     R=matrix(NA,n.step,n.rg)
#     colnames(R)=Rgs
#     rownames(R)=FinYrs
#     id.row=match(dummy$year,rownames(R))
#     id.col=match(dummy$area,colnames(R))
#     for(s in 1:length(id.row)) R[id.row[s],id.col[s]]=dummy$Number[s]
#     Rec.tg[,,tg]=R
#   }
#   
#   return(Rec.tg=Rec.tg)
# }
# 
# for(i in 1:length(RECS)) RECS[[i]]=fn.array(RELS[[i]],RECS[[i]])
# 
# 
# 
# #Left here, catchabilities are not constant (different fisheries, etc, affecting recaptures
# # (e.g.) tg=1 for sandbars, only 4 released, and 1 recapture, which supposes a huge catchability which
# # will kill the number of released sharks straightaway....)
# #another issue: more than 3 zones for sandbar and dusky.....
# 
# 
# #---PARAMETERS SECTION----
# #initial values
# p11.init=.7;p12.init=.1
# p22.init=.8;p21.init=.1
# p33.init=.7;p32.init=.1
# 
# P_int=c(p11.init,p12.init,p21.init,p22.init,p32.init,p33.init)
# names(P_int)=c("p11","p12","p21","p22","p32","p33")
# 
# q_init=c(0.001,0.001,0.001) 
# names(q_init)=c("qR1","qR2","qR3")
# pars=c(P_int,q_init)
# 
# Rgs=c("West","Zone1","Zone2")
# n.rg=length(Rgs)
# 
# #PROCEDURE SECTION
# Pop.dyn=function(pars)
# {
#   Qs=pars[match(names(q_init),names(pars))]
#   
#   #set up for matrix multiplication
#   prop_mov.pred=matrix(c(pars[1],pars[2],1-(pars[1]+pars[2]),
#                          pars[3],pars[4],1-(pars[3]+pars[4]),
#                          1-(pars[5]+pars[6]),pars[5],pars[6]),n.rg,n.rg,byrow=T)
#   
#   #array of numbers and recaptures for each tag group
#   Tg.Grp=sort(unique(releases$Tg.gp))
#   n.tgGr=length(Tg.Grp)
#   STEPS=match(releases$yr,FinYrs)
#   
#   N.tg=Rec.tg=array(NA,dim=c(n.step,n.rg,n.tgGr))
#   
#   for(tg in 1:n.tgGr)
#   {
#     #Store predicted number of tagged sharks in population and those being recaptured for tag group= tg
#     N=R=matrix(NA,n.step,n.rg)
#     
#     #Select tag group
#     dummy=subset(releases,Tg.gp==Tg.Grp[tg])
#     tag.reg=rep(0,n.rg)
#     names(tag.reg)=Rgs
#     tag.reg[match(as.character(dummy$area),names(tag.reg))]=1 
#     Tiat=rep(dummy$Number,n.rg)*tag.reg  #initial number of tags releases from tag group= tg
#     
#     Start=STEPS[tg]
#     for(i in Start:n.step)
#     {
#       h=matrix(rep(Qs*Effort[i,],n.rg),n.rg,n.rg,byrow=T)  #exploitaiton rate per region and time step
#       
#       
#       #penalty to avoid H>1    
#       eps=0.999 #maximimum accepted exploitation rate
#       pen=0
#       pen=sum((h>eps)*0.01*(h-eps)^2)
#       h=ifelse(h>eps,eps,h)
#       
#       #penalty to avoid q<0    
#       eps1=0.001 #minimum accepted exploitation rate
#       pen=pen+sum((h<eps1)*0.01*(h-eps)^2)
#       h=ifelse(h<eps1,eps1,h)
#       
#       
#       NN=NULL
#       if(i==Start) 
#       {
#         NN=matrix(Tiat,ncol=n.rg,nrow=n.rg)      #Initial condition
#       }else     
#       {      
#         NN=matrix(N[i-1,],ncol=n.rg,nrow=n.rg)
#       }
#       
#       #Numbers dying from fishing and moving (second, after sampling)
#       N[i,]=colSums(round(NN*prop_mov.pred*(1-h)))
#       
#       #Numbers recaptured (first sampling, i.e. recapturing)
#       R[i,]=round(N[i,]*Qs*Effort[i,])    
#     }
#     
#     #Store each tag group
#     N.tg[,,tg]=N
#     Rec.tg[,,tg]=R
#     
#   }
#   
#   Rec_pred=c(Rec.tg)
#   Rec_pred=Rec_pred[!is.na(Rec_pred)]
#   recaptures=c(Obs.recaptures)
#   recaptures=recaptures[!is.na(recaptures)]
#   
#   #negative log likelihood (kernel)
#   neg.LL=sum(Rec_pred-recaptures*log(Rec_pred+0.00001))+1000*pen
#   
#   return(list(N_pred=N.tg,Rec_pred=Rec.tg,neg.LL=neg.LL))
# }
# 
# 
# for (i in 1:length(RELS))
# {
#   releases=RELS[[i]]
#   Obs.recaptures=RECS[[i]]
#   
#   #MAIN SECTION
#   fn_obj=function(pars)Pop.dyn(pars)$neg.LL  #objfun to minimize 
#   fit=optim(pars,fn_obj)
#   fit=optim(pars,fn_obj,method="Nelder-Mead")
#   
# }