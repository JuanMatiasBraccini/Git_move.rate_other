
#source data from "Analysis conv tagging data.R"
setwd("H:/Matias WA Fisheries/Analyses/Conventional tagging/Movement rate estimation")


Tagging=read.csv(Tagging.csv)

  #6. MOVEMENT TRANSITION MATRIX


#################################################
#Simulate movement
n.area=4 #numbers of areas
n.pop=1000000 #initial population size in each are

prop.tagging=.1 #proportion of population tagged
prop.recap=.01 #proportion recaptured
report.rate=.5 #reporting rate
reported.recaptures=prop.recap*report.rate

#movement probabilities
stay=.75
left.one=.15
left.two=.075
left.three=.025

N=matrix(rep(n.pop,n.area),nrow=1) #numbers per area
n.tagged=N*prop.tagging #number tagged
n.recap=N*reported.recaptures #number recaptured that where reported

mov.rates=c(stay,left.one,left.two,left.three,left.three,stay,left.one,left.two,
            left.three,left.two,stay,left.one,left.one,left.two,left.three,stay)
mov=matrix(mov.rates,ncol=n.area,nrow=n.area,byrow=T)
rowSums(mov) #should be all ones

N.expanded=matrix(rep(N,nrow(mov)),nrow=nrow(mov)) #numbers per cell at time t
N.expanded.tagged=matrix(rep(n.tagged,nrow(mov)),nrow=nrow(mov)) #numbers tagged per cell at time t
N.expanded.recap=matrix(rep(n.recap,nrow(mov)),nrow=nrow(mov)) #numbers recaptured per cell at time t

N.expanded_1=mov*N.expanded #matrix at time t+1
N.expanded.tagged_1=mov*N.expanded.tagged #matrix tagged  at time t+1
N.expanded.recap_1=mov*N.expanded.recap #matrix tagged at time t+1

Nt_1=rowSums(N.expanded_1) #population numbers
n.tagged_1=rowSums(N.expanded.tagged_1)
n.recap_1=rowSums(N.expanded.recap_1)


#################################################
