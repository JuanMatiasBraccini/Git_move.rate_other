
#DATA SECTION
Tag_StartYear=1990
Tag_EndYear=1993
Yrs=Tag_StartYear:Tag_EndYear
start.yr=c(1,2)
Tag_nyrs=length(Yrs)
Tag_Areas=3
Tag_numTagGp=2
Tag_Releases_areas=array(data=c(5000,0,0,0,5000,0),dim=c(1,Tag_Areas,Tag_numTagGp))
Tag_Releases_yrs=array(data=c(1990,1991),dim=c(1,1,Tag_numTagGp))
Tag_Recaptures=array(,dim=c(Tag_nyrs,Tag_Areas,Tag_numTagGp))
Tag_Recaptures[,,1]=matrix(c(180,20,15, 100,35,20, 85,30,30, 60,35,30),ncol=Tag_Areas,byrow=T)
Tag_Recaptures[,,2]=matrix(c(0,0,0, 25,160,18, 34,120,20, 34,80,29),ncol=Tag_Areas,byrow=T)
Tag_Recapture_index=array(,dim=c(Tag_nyrs,Tag_Areas,Tag_numTagGp))
Tag_Recapture_index[,,1]=matrix(rep(1,12),ncol=Tag_Areas,byrow=T)
Tag_Recapture_index[,,2]=matrix(c(0,0,0, rep(1,9)),ncol=Tag_Areas,byrow=T)
Tag_Reporting=matrix(rep(0.9,12),ncol=Tag_Areas,byrow=T)
Tag_Shedding=0.01
Tag_M=0.1
Tag_Effort=matrix(rep(c(10,9,8),4),ncol=Tag_Areas,byrow=T)
Tag_nMovParams=6
liketype=1
likePoissonK=.0001



#PARAMETERS SECTION
movPar_1_1=0.6
movPar_1_2=0.14
movPar_2_1=0.14
movPar_2_2=0.6
movPar_3_2=0.15
movPar_3_3=0.75
Tag_log_Q1=-5
Tag_log_Q2=-5
Tag_log_Q3=-5
Tag_log_Tau=1


#Declare some objects
MovMat=matrix(nrow=Tag_Areas,ncol=Tag_Areas)
Tag_N=array(,dim=c(Tag_nyrs,Tag_Areas,Tag_numTagGp)) 
Tag_Pred_Rec=Tag_N_no_mov=Tag_N
Tag_F=matrix(nrow=Tag_nyrs,ncol=Tag_Areas) 
Tag_Q=Tag_F
Tag_Tau=0
Tag_NLL=0

#These are the values used for simulating the observed data, should recover them.
Original.pars=c(
  movPar_1_1=0.8,
  movPar_1_2=0.1,
  movPar_2_1=0.1,
  movPar_2_2=0.8,
  movPar_3_2=0.1,
  movPar_3_3=0.8,
  Tag_log_Q1=-5,
  Tag_log_Q2=-5,
  Tag_log_Q3=-5
)

pars=c(movPar_1_1,movPar_1_2,movPar_2_1,movPar_2_2,movPar_3_2,movPar_3_3,Tag_log_Q1,Tag_log_Q2,Tag_log_Q3)
names(pars)=names(Original.pars)
est.mov.rate=function(pars)
{
  #fill in Movement matrix
  MovMat[1,1]=pars[1];
  MovMat[1,2]=pars[2];
  MovMat[1,3]=1-(pars[1]+pars[2]);
  
  MovMat[2,1]=pars[3];
  MovMat[2,2]=pars[4];
  MovMat[2,3]=1-(pars[3]+pars[4]);
  
  MovMat[3,1]=1-(movPar_3_3+movPar_3_2);
  MovMat[3,2]=pars[5];
  MovMat[3,3]=pars[6];
  
  
  
  #put catchability in normal space
  for(t in 1:Tag_nyrs)
  {
    Tag_Q[t,1]=exp(pars[7]);
    Tag_Q[t,2]=exp(pars[8]);
    Tag_Q[t,3]=exp(pars[9]);
  }
  Original.pars=c

  #calculate fishing mortality at time and area
  for(t in 1:Tag_nyrs) Tag_F[t,]=Tag_Q[t,]*Tag_Effort[t,]
  
  #fill in numbers
  for(t in 1:Tag_numTagGp)                                            #loop over tag groups
  {
    Tag_yearTag=match(Tag_Releases_yrs[1,1,t],Yrs);
    n=start.yr[t]
    for(y in n:Tag_nyrs)                               #loop over years
    {         
      if(y==Tag_yearTag)
      {                  
        #Recruitment tags
        Tag_N[y,,t]=Tag_Releases_areas[,,t];
        
        #Apply mortality
        for(a in 1:Tag_Areas)                                    #loop over areas
        {
          Tag_N[y,a,t] =  Tag_N[y,a,t] * exp(-(Tag_F[y,a] + Tag_M + Tag_Shedding));
        }
        
      }else
      {                 
        #Apply mortality
        for(a in 1:Tag_Areas)                                    #loop over areas
        {
          Tag_N[y,a,t] =  Tag_N[y-1,a,t] * exp(-(Tag_F[y,a] + Tag_M + Tag_Shedding));
        }   
      }
      
      #Apply movement
      Tag_N[y,,t]=Tag_N[y,,t]%*%MovMat;
    }
  }
  
  
  #Observation model
  for(t in 1:Tag_numTagGp)                               
  {
    Tag_yearTag=match(Tag_Releases_yrs[1,1,t],Yrs)
    for(y in 1:Tag_nyrs)                   
    {
      for(a in 1:Tag_Areas)                            
      {
        Tag_Pred_Rec[y,a,t]=Tag_Reporting[y,a] * Tag_N[y,a,t] * ((Tag_F[y,a])/(Tag_F[y,a] + Tag_M + Tag_Shedding)) *(1-exp(-(Tag_F[y,a] + Tag_M + Tag_Shedding)));
      }
    }
  }
  
  #Objective function
  Tag_Tau=exp(Tag_log_Tau);
  
  #Calculate observations' likelihood
  for(t in 1:Tag_numTagGp)                               
  {
    for(y in 1:Tag_nyrs)                   
    {
      for(a in 1:Tag_Areas) 
      {
        #normal
        if(Tag_Recapture_index[y,a,t]>0) Tag_NLL= Tag_NLL+((Tag_Recaptures[y,a,t]+likePoissonK)-((Tag_Pred_Rec[y,a,t] + likePoissonK)))^2;
       }
    }
   }

  



  return(list(neg.LL=Tag_NLL,Tag_Pred_Rec=Tag_Pred_Rec))
}
fn_obj=function(pars)est.mov.rate(pars)$neg.LL  #objfun to minimize 
fit=optim(pars,fn_obj,method="Nelder-Mead")

Eval=est.mov.rate(fit$par)
Pred_recap=Eval$Tag_Pred_Rec



#Compare original parameter with predicted
Original.pars
round(fit$par,2)

#compare original data with predicted
plot(Tag_Recaptures)
points(round(Pred_recap),pch=19,col=2)
