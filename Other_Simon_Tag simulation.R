## 2 regions, 6 timestpes
handl_OneDrive=function(x)paste('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias',x,sep='/')
obs_dat <- read.csv(handl_OneDrive('Analyses/Movement rate estimation/Simon/deeptags.csv'))
windows.options(record=T, restoreConsole=T)
Lambda <- 0.58                       # Tag reporting rate
## Phi these values match the rate of decline shown in Kims tank trials
phi <- c(exp(-0.0481404), rep(exp(-0.15), 6))
#phi <- exp(-0.0513)               # 5% tag loss per month
last=1
ft <- 1; lt <- last                 # time steps first and last
fr <- 1; lr <- 2                  # regions 1-2
fy <- 1; ly <- 6                  # First year, last year
# F = qE
#Fs are from brownie model for months August to January
brownieFs <- c(0.2425415, 0.4287591, 0.3186824, 0.2966673, 0.3767184, 0.2930311)
brownieFs <- rep(mean(brownieFs), length(brownieFs))
Fs <- matrix(c(rep(0, ly),brownieFs), ncol=lr, nrow=ly, byrow = F)
M <- 0.24/last                            # Natural mort
propM <- c(0,0,0.1,0,0,0)         # Prop migrating. Starting par values
mig_to <- c(2,0)                     # regions migration moves to from regions 1:2 (region 2 migrates to nowhere)
tagnums <- matrix(c(56,323,0,0,0,0,rep(0, ly)), ncol=lr, nrow=ly, byrow = F)
dimnames(tagnums) <- dimnames(Fs) <- list(month=month.abb[c(8:12,1)], area=c('Deep','Shallow'))
## Make arrays to store daat in
catch <- array(0, c(lt,lr,ly,ly,lr))          # Timestep, region, year, tag release year, tag release location
nl   <-  array(0, c(lt,lr,ly,ly,lr))          # Timestep, region, year, tag release year, tag release location
f_matrix <- Fs

modelfunc <- function(x, plot=F){
  propM <- (x)^2
for(iy in fy:ly){                        # year loop
  for(it in ft:lt){                      # timestep loop
    for(ir in fr:lr){                    
#Update abundance and tag every on 1st tstep               iy <- 3; ir <- 1; it <- 1; ry <- 1
    if(it==ft & iy==fy){ nl[it,ir,iy,fy,ir]  <-  tagnums[iy,ir] } #if first timestep and first year
    if(!(it==ft & iy==fy)) {
      if(it==ft) {for(ry in fy:ly) { nl[it,ir,iy,ry,]  <- nl[lt,ir,(iy-1),ry,] }  #release yr loop
                                     nl[it,ir,iy,iy,ir]  <-  tagnums[iy,ir]}
      if(it>ft) { for(ry in fy:ly) { nl[it,ir,iy,ry,]  <- nl[(it-1),ir,iy,ry,]}}
                    }}
    #Migration
    for(ir in fr:lr){ for(ry in fy:ly) {                   
      if(it==1) {
        if(ir<lr) {  nl[it,mig_to[ir],iy,ry,] <- nl[it,mig_to[ir],iy,ry,] + nl[it,ir,iy,ry,]  *  propM[iy]
                     nl[it,ir,iy,ry,] <-  nl[it,ir,iy,ry,] * (1-propM[iy]) }
        if(ir==lr) { nl[it,ir,iy,ry,] <-  nl[it,ir,iy,ry,] * (1-propM[iy]) }
                }}   }
    #Tag loss
    for(ir in fr:lr){ for(ry in fy:ly) {                    
        nl[it,ir,iy,ry,]  <- nl[it,ir,iy,ry,]  * phi[ry]}}
    #Fish              
    #ifelse((it<2 | it==12), M <- Mw, M <- Mr)  ## Allows for whites/reds specific M
    for(ir in fr:lr){ for(ry in fy:ly) {                    
         catch[it,ir,iy,ry,] <- nl[it,ir,iy,ry,] * f_matrix[iy,ir]/(f_matrix[iy,ir]+M) * (1-exp(-(f_matrix[iy,ir]+M))) * Lambda
         nl[it,ir,iy,ry,]    <- nl[it,ir,iy,ry,] * exp(-(f_matrix[iy,ir]+M))
                  }}}}
         ## year region Timestep tag release year, tag release location
catchout <- data.frame(year=rep(fy:ly,each=lr*lt*ly*lr), region=rep(rep(fr:lr,each=ly*lt*lr),ly), tstep=rep(rep(ft:lt, each=ly*lr),ly*lr), rel_year=rep(rep(fy:ly, each=lr),lt*ly*lr), rel_loc=rep(fr:lr,ly*lt*ly*lr), tags=NA)
#head(catchout, 40)                                                                                                                                            3       *   4  *2 *3 *4                4  *  3*2*3*4

for(it in ft:lt){          # timestep          it <- 1; iy <- 1; ir <- 1; ry <- 1
  for(iy in fy:ly){        # year
    for(ir in fr:lr){      # region
      for(ry in fy:ly){    # release year
              catchout$tags[catchout$tstep==it & catchout$year==iy & catchout$region==ir & catchout$rel_year==ry] <- catch[it,ir,iy,ry,]}}}}


sum_catch <- aggregate(list(tags=catchout$tags), list(tstep=catchout$year, region=catchout$region, rel_tstep=catchout$rel_year), sum)
sum_catch <- sum_catch[sum_catch$region==2 & sum_catch$rel_tstep<=2,]
sum_catch$obs_tags <- obs_dat$tags[match(paste(sum_catch$tstep,sum_catch$region,sum_catch$rel_tstep), paste(obs_dat$tstep,obs_dat$region,obs_dat$rel_tstep))]
if(plot==T){ par(las=1)
             ymx <- max(ceiling(c(sum_catch$tags, sum_catch$obs_tags)))
             plot(sum_catch$tstep, sum_catch$tags, pch=15+sum_catch$rel_tstep, ylab='Recaptured tags', xlab='Fishing Month', ylim=c(0,ymx), cex=1.2, axes=F)
             points(sum_catch$tstep, sum_catch$obs_tags, pch=15+sum_catch$rel_tstep, col=grey(0.7), cex=1.2)
             legend('topleft', col=rep(c(1,grey(0.7)), each=2), pch=rep(16:17,2), legend=c('Predicted Aug','Predicted Sep','Observed Aug','Observed Sep'),bty='n')
             axis(1, 1:6, month.abb[c(8:12,1)]); axis(2, 0:ymx)
             }
  
##Liklihood
#Sum Squares
#sum_catch$ss <-  ifelse(sum_catch$obs_tags>0.01, (sqrt(sum_catch$obs_tags)-sqrt(sum_catch$tags))^2, 0) 
sum_catch$ss <-  (sqrt(sum_catch$obs_tags)-sqrt(sum_catch$tags))^2
#Number of observations
#ncnt <- length(sum_catch$obs_tags[sum_catch$obs_tags>0])
ncnt <- length(sum_catch$obs_tags)
Like <- ncnt*0.5*(1.0+log(sqrt(sum(sum_catch$ss)/ncnt))+log(2.0*pi))
return(1*Like)
#cor(sum_catch$obs_tags,sum_catch$tags)^2
}

x <- propM <- sqrt(c(0.0,0,0.15,0.0,0,0))         # Prop migrating
mod_out <- optim(propM, modelfunc, hessian = T)
se <- sqrt(diag(solve(mod_out$hessian)))
cvs <- abs(se/mod_out$par)
x <-propM <- (mod_out$par)
modelfunc(propM, T)
ymx <- (ceiling((max(mod_out$par)^2)*10))/10
plot(unique(obs_dat$tstep), (mod_out$par)^2, pch=16, ylim=c(0, ymx), ylab='Proportion migrating', xlab='Month', axes=F)
arrows(1:6, (mod_out$par)^2*(1-cvs), y1=(mod_out$par)^2*(1+cvs), code=3, angle=90, length=0.05)
axis(1, 1:6, month.abb[c(8:12,1)]); axis(2)
