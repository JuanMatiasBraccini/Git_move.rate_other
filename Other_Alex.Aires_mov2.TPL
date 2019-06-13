//GLOBALS_SECTION

  //#include <admodel.h>
  //int mcmc_report_lines;  // how many lines have been written to MCreport
  //ofstream mcmc_report("mcmc.csv");  // MCMC report


DATA_SECTION

 //========= Read data
 !!ad_comm::change_datafile_name("mov2.dat");  
 init_int firstYear;		// First year of tagging data
 init_int lastYear;			// Last year of tagging data
 init_int numAreas;			// Number of geographical areas
 init_int numFish;		// Number of fisheries
 init_int numTagGroups;		// Numbers of tag groups
 init_matrix FishSummary(1,numFish,1,2);		// Fishery summary
 init_matrix TagSummary(1,numTagGroups,1,4);		// Tag release summary
 init_matrix RecapSummary(1,1612,1,5);		// Tag recapture summary
 init_matrix HooksSummary(1,988,1,3);		// Fishing effort summary
 init_matrix MovH1(1,20,1,3);		// Movement hypothesis 1 - movement parameter indexing
 init_matrix MovH2(1,10,1,3);		// Movement hypothesis 2 - movement parameter indexing
 init_matrix MovH3(1,6,1,3);		// Movement hypothesis 3 - movement parameter indexing
 init_matrix qH1(1,numFish,1,2);		// Movement hypothesis 1 - movement parameter indexing
 init_matrix qH2(1,numFish,1,2);		// Movement hypothesis 2 - movement parameter indexing
 init_matrix TagRepH3(1,numFish,1,2);	// Tag reporting hypothesis 3 - tag reporting parameter indexing
 init_matrix TauH3(1,numFish,1,2);	// Tau hypothesis 3 - tau (overdispersion) parameter indexing
 init_matrix TauH4(1,numFish,1,2);	// Tau hypothesis 4 - tau (overdispersion) parameter indexing

 // Define matrices and arrays (only non-differentiable stuff in here)
 3darray TagsDat3d(1,numTagGroups,1,numAreas,firstYear,lastYear);	// 3d array for tag release data (group,area,year)
 3darray RecapsDat3d(1,numTagGroups,1,numFish,firstYear,lastYear);	// 3d array for tag recapture data (group,fishery,year)
 
 //matrix HooksData(1,numFish,firstYear,lastYear);						// matrix for fishing effort (hooks) data
 3darray HooksDat3d(1,numAreas,1,numFish,firstYear,lastYear);			// 3d array for fishing effort data, hook numbers (group,fishery,year)

 
 //======================= Read controls of model parameters
 !!ad_comm::change_datafile_name("mov2switches.dat");
 init_int movH;
 init_int qH;
 init_int tagRepH;
 init_number likeType;
 init_number likePoissonConst;
 init_int tauH;
 init_matrix ParamsInfo(1,4,1,4);
 init_matrix MovParamInfoH1(1,20,1,4);
 init_matrix MovParamInfoH2(1,10,1,4);
 init_matrix MovParamInfoH3(1,6,1,4);
 init_matrix QsInfoH1(1,25,1,4);
 init_matrix QsInfoH2(1,14,1,4);
 init_matrix TagRepInfoH1(1,1,1,4);
 init_matrix TagRepInfoH2(1,numFish,1,4);
 init_matrix TagRepInfoH3(1,16,1,4);
 init_matrix EdevsInfo(1,numFish,1,7);
 init_matrix TauInfoH1(1,1,1,4);
 init_matrix TauInfoH2(1,numFish,1,4);
 init_matrix TauInfoH3(1,16,1,4);
 init_matrix TauInfoH4(1,3,1,4);

 //!!exit(1);		// exit from the switch file

 //======================= Initialize controls for Edevs
 ivector EdevsFirstYear(1,numFish);
 ivector EdevsLastYear(1,numFish);
 ivector EdevsPhases(1,numFish);
 vector EdevsSD(1,numFish);

 //======================= Define global dimensional quantities for different model hypotheses
 int nMovParams;		// Number of movement parameters
 int numQs;				// Number of catchability pararametes
 int numTagRepParams;	// Number of tag reporting parameters
 int numTauParams;		// Number of overdispersion (tau) parameters
 // Define the number of movement parameters
 LOCAL_CALCS
  if(movH==1) {nMovParams=20;}		// Movement hypothesis 1 - Full Markovian model, movement to all areas		
  if(movH==2) {nMovParams=10;}		// Movement hypothesis 2 - Reduced model, movement to adjacent areas
  if(movH==3) {nMovParams=6;}		// Movement hypothesis 3 - Reduced model, movement to adjacent areas, clowise gyre
 END_CALCS
 matrix MovParamInfoSel(1,nMovParams,1,4);
 // Select the movement parameter controls among different model hypothesis
 LOCAL_CALCS
  if(movH==1) {MovParamInfoSel=MovParamInfoH1;}		
  if(movH==2) {MovParamInfoSel=MovParamInfoH2;}
  if(movH==3) {MovParamInfoSel=MovParamInfoH3;}
 END_CALCS
 
 // Define the number of catchability parameters
 LOCAL_CALCS
  if(qH==1) {numQs=25;}			// H1
  if(qH==2) {numQs=14;}			// H2
 END_CALCS
 matrix QsInfoSel(1,numQs,1,4);
 // Select the catchability controls among different model hypothesis
 LOCAL_CALCS
  if(qH==1) {QsInfoSel=QsInfoH1;}
  if(qH==2) {QsInfoSel=QsInfoH2;}
 END_CALCS

 // Define the number of tag reporting parameters
 LOCAL_CALCS
  if(tagRepH==1) {numTagRepParams=1;}	// H1
  if(tagRepH==2) {numTagRepParams=numFish;}	// H2
  if(tagRepH==3) {numTagRepParams=16;}	// H3
 END_CALCS
 matrix TagRepInfoSel(1,numTagRepParams,1,4);
 // Select the tag reporting controls among different model hypothesis
 LOCAL_CALCS
  if(tagRepH==1) {TagRepInfoSel=TagRepInfoH1;}	// H1
  if(tagRepH==2) {TagRepInfoSel=TagRepInfoH2;}	// H2
  if(tagRepH==3) {TagRepInfoSel=TagRepInfoH3;}	// H3		
 END_CALCS

 // Define the number of tau (overdispersion) parameters
 LOCAL_CALCS
  if(tauH==1) {numTauParams=1;}	// H1
  if(tauH==2) {numTauParams=numFish;}	// H2
  if(tauH==3) {numTauParams=16;}	// H3
  if(tauH==4) {numTauParams=3;}	// H4
 END_CALCS
 matrix TauInfoSel(1,numTauParams,1,4);
 // Select the tau controls among different model hypothesis
 LOCAL_CALCS
  if(tauH==1) {TauInfoSel=TauInfoH1;}	// H1
  if(tauH==2) {TauInfoSel=TauInfoH2;}	// H2
  if(tauH==3) {TauInfoSel=TauInfoH3;}	// H3
  if(tauH==4) {TauInfoSel=TauInfoH4;}	// H4		
 END_CALCS

 // ======== Initialize bounds, init values  and phases of model parameters ==========
 // Natural mortality
 number M_LO;		// Lower bound
 number M_HI;		// Upper bound
 number M_INIT;     // Initial value
 int M_PH;      	// Phase

 // Tag induced mortality
 number tagMort_LO;   	
 number tagMort_HI;   
 number tagMort_INIT;   
 int tagMort_PH;      

 // Tag loss
 number tagLoss_LO;   
 number tagLoss_HI;   
 number tagLoss_INIT;  
 int tagLoss_PH;
 
 // Fscale
 //vector Fscale_LO(1,numTagGroups);     
 //vector Fscale_HI(1,numTagGroups);     
 //vector Fscale_INIT(1,numTagGroups);     
 //ivector Fscale_PH(1,numTagGroups);     
 vector Fscale_LO(1,numAreas);     
 vector Fscale_HI(1,numAreas);     
 vector Fscale_INIT(1,numAreas);     
 ivector Fscale_PH(1,numAreas);     

 // Sigma effort
 number  sigmaEffort_LO;     
 number  sigmaEffort_HI;     
 number sigmaEffort_INIT;     
 int sigmaEffort_PH;     

 // Catchabilities
 vector logqEst_LO(1,numQs);    
 vector logqEst_HI(1,numQs);    
 vector logqEst_INIT(1,numQs);    
 ivector logqEst_PH(1,numQs);
 
 // Movement parameters
 vector MovParams_LO(1,nMovParams);    
 vector MovParams_HI(1,nMovParams);    
 vector MovParams_INIT(1,nMovParams);   
 ivector MovParams_PH(1,nMovParams);      	

 // Tag reporting rates
 vector TagRep_LO(1,numTagRepParams);    
 vector TagRep_HI(1,numTagRepParams);    
 vector TagRep_INIT(1,numTagRepParams);   
 ivector TagRep_PH(1,numTagRepParams);
 
 // Tau (overdispersion) parameters	
 vector Tau_LO(1,numTauParams);    
 vector Tau_HI(1,numTauParams);    
 vector Tau_INIT(1,numTauParams);   
 ivector Tau_PH(1,numTauParams);

 // ======== Define bounds, init values  and phases of model parameters ==========
 LOCAL_CALCS
  // Natural mortality
  M_LO = ParamsInfo(1,1);		
  M_HI = ParamsInfo(1,2);    
  M_INIT = ParamsInfo(1,3);
  M_PH = ParamsInfo(1,4);
  
  // Tag induced mortality
  tagMort_LO = ParamsInfo(2,1);	
  tagMort_HI = ParamsInfo(2,2);     
  tagMort_INIT = ParamsInfo(2,3);     
  tagMort_PH = ParamsInfo(2,4);     

  // Tag loss
  tagLoss_LO = ParamsInfo(3,1);     
  tagLoss_HI = ParamsInfo(3,2);      
  tagLoss_INIT = ParamsInfo(3,3);      
  tagLoss_PH = ParamsInfo(3,4);

  // Fscale
  //for (int i=1; i<=numTagGroups; i++)
  for (int i=1; i<=numAreas; i++)
  {
	  Fscale_LO(i) = ParamsInfo(4,1);
	  Fscale_HI(i) = ParamsInfo(4,2);
	  Fscale_INIT(i) = ParamsInfo(4,3);
	  Fscale_PH(i) = ParamsInfo(4,4);
  }

  // Sigma effort
  sigmaEffort_LO = ParamsInfo(4,1);     
  sigmaEffort_HI = ParamsInfo(4,2);      
  sigmaEffort_INIT = ParamsInfo(4,3);      
  sigmaEffort_PH = ParamsInfo(4,4);
  
  // Catchability parameters
  for (int iQ=1; iQ<=numQs; iQ++)
  {
	  logqEst_LO(iQ) = QsInfoSel(iQ,1);
	  logqEst_HI(iQ)= QsInfoSel(iQ,2);
	  logqEst_INIT(iQ)= QsInfoSel(iQ,3);
	  logqEst_PH(iQ)= QsInfoSel(iQ,4);
  }
 
  // Movement parameters
  for (int imovParam=1;imovParam<=nMovParams;imovParam++)
  {
	  MovParams_LO(imovParam)= MovParamInfoSel(imovParam,1);
	  MovParams_HI(imovParam)= MovParamInfoSel(imovParam,2);
	  MovParams_INIT(imovParam)= MovParamInfoSel(imovParam,3);
	  MovParams_PH(imovParam)= MovParamInfoSel(imovParam,4);
  }
  
  // Tag reporting parameters
  for (int itagRepParam=1;itagRepParam<=numTagRepParams;itagRepParam++)
  {
	  TagRep_LO(itagRepParam)= TagRepInfoSel(itagRepParam,1);
	  TagRep_HI(itagRepParam)= TagRepInfoSel(itagRepParam,2);
	  TagRep_INIT(itagRepParam)= TagRepInfoSel(itagRepParam,3);
	  TagRep_PH(itagRepParam)= TagRepInfoSel(itagRepParam,4);
  }
  
  // Tau (overdispersion) parameters
  for (int itauParam=1;itauParam<=numTauParams;itauParam++)
  {
	  Tau_LO(itauParam)= TauInfoSel(itauParam,1);
	  Tau_HI(itauParam)= TauInfoSel(itauParam,2);
	  Tau_INIT(itauParam)= TauInfoSel(itauParam,3);
	  Tau_PH(itauParam)= TauInfoSel(itauParam,4);
  }
 END_CALCS
    
 !!cout<<"       Movement hypothesis = "<<movH<<endl;
 !!cout<<"       Number of movement params = "<<nMovParams<<endl;
 //!!cout<<" Movement param controls = "<<endl<<MovParamInfoSel<<endl;
 !!cout<<""<<endl;

 !!cout<<"       Catchability hypothesis = "<<qH<<endl;
 !!cout<<"       Number of Qs = "<<numQs<<endl;
 //!!cout<<" Q controls = "<<endl<<QsInfoSel<<endl;
 !!cout<<""<<endl;

 !!cout<<"       Tag reporting hypothesis = "<<tagRepH<<endl;
 !!cout<<"       Number of tag reporting params = "<<numTagRepParams<<endl;
 //!!cout<<" Tag reporting param controls = "<<endl<<TagRepInfoSel<<endl;
 !!cout<<""<<endl;

 !!cout<<"       Likelihood type = "<<likeType<<endl;
 !!cout<<"       Tau (overdispersion) hypothesis = "<<tauH<<endl;
 !!cout<<""<<endl;

 
  // ======== Define MCMC variables ==========
  int mccounter;
  !! mccounter = 1;
  int done_run;
  !! done_run=0;
 
PARAMETER_SECTION

 init_number dummy(-1);   // dummy variable for code testing

 // ======== Initialize the estimated parameters ==========
 // Natural mortality
 init_bounded_number Mval(M_LO,M_HI,M_PH);	// Need to delete Mval above
 // Tag induced mortality
 //init_bounded_number tagMort(tagMort_LO,tagMort_HI,tagMort_PH);
 // Tag loss
 init_bounded_number tagLoss(tagLoss_LO,tagLoss_HI,tagLoss_PH);
 // Tag reporting rate
 init_bounded_number_vector TagRepEst(1,numTagRepParams,TagRep_LO,TagRep_HI,TagRep_PH);	// Vector of estimated tag reporting parameter
 vector TagRep(1,numFish);		// Vector of fishery-specific tag reporting rates

 // Movement parameters, movement matrix, and derived quantitites needed
 //init_bounded_number_vector MovParamVector(1,nMovParams,MovLbound,MovUbound,MovParPhases);		// Creates a vector of "nMovParams" estimable movement parameters 
 init_bounded_number_vector MovParamVector(1,nMovParams,MovParams_LO,MovParams_HI,MovParams_PH);		// Creates a vector of "nMovParams" estimable movement parameters
 matrix MovMat(1,numAreas,1,numAreas);		// Matrix of movement parameters 
 vector MovMatDiag(1,numAreas);		// Diagonal vector of MovMatrix
 sdreport_vector MovMatDiag2(1,numAreas);
 vector NonDiagSum(1,numAreas);			// Vector with col sums of non-diagonal elements for computing the "black box" (1 - NonDiagSum)
 
 // Exploitation
 // Fishery-specific catchabilities
 init_bounded_number_vector logqEst(1,numQs,logqEst_LO,logqEst_HI,logqEst_PH);
 //init_vector logqEst(1,numQs);			// Vector of fishery-specific catchabilities (in log space)						// DO NOT BOUND THE CATCHABILITIES
 vector logq(1,numFish);									// Vector of fishery-specific catchabilities
 // Fishery specific fishing mortalities by area (instantaneous Fs for each fishery by area)
 3darray Ffish3d(1,numAreas,1,numFish,firstYear,lastYear);		// 3d array for Fs (area,fishery,year)
 matrix Ffish(1,numFish,firstYear,lastYear);					// Fishery-specific fishing mortalities
 matrix Ffish80s(1,numFish,1980,1989);						// Vector for the fishery-specific fishing mortalities during the 1980s  	
 ////sdreport_vector Ffish80s2(1,numFish);
 vector FfishMean80s(1,numFish);						// Vector for the mean fishery-specific fishing mortalities during the 1980s  	
 sdreport_vector FfishMean80s2(1,numFish);
 matrix Ffish90s(1,numFish,1990,1999);						// Vector for the fishery-specific fishing mortalities during the 1990s  	
 ////sdreport_vector Ffish90s2(1,numFish);
 vector FfishMean90s(1,numFish);						// Vector for the mean fishery-specific fishing mortalities during the 1990s  	
 sdreport_vector FfishMean90s2(1,numFish);
 vector Ffish2000(1,numFish);							// Vector for the fishery-specific fishing mortality in 2000 			
 sdreport_vector Ffish2000b(1,numFish);
 // Area-specific fishing mortalities (instantaneous Fsum for each area)
 matrix Farea(1,numAreas,firstYear,lastYear); 
 matrix Farea80s(1,numAreas,1980,1989);						// Vector for the area-specific fishing mortalities during the 1980s  	
 vector FareaMean80s(1,numAreas);						// Vector for the mean area-specific fishing mortalities during the 1980s  	
 sdreport_vector FareaMean80s2(1,numAreas);
 matrix Farea90s(1,numAreas,1990,1999);						// Vector for the area-specific fishing mortalities during the 1990s  	
 vector FareaMean90s(1,numAreas);						// Vector for the mean area-specific fishing mortalities during the 1990s  	
 sdreport_vector FareaMean90s2(1,numAreas);
 vector Farea2000(1,numAreas);							// Vector for area-specific fishing mortalities in 2000
 sdreport_vector Farea2000b(1,numAreas);
 //init_bounded_number_vector Fscale(1,numTagGroups,Fscale_LO,Fscale_HI,Fscale_PH);				// Vector of Fscales for the tagging year of each tag release group
 init_bounded_number_vector Fscale(1,numAreas,Fscale_LO,Fscale_HI,Fscale_PH);				// Vector of Fscales for the tagging year of each tag release group

 // Effort deviates
 LOCAL_CALCS
  for (int ifish=1; ifish<=numFish; ifish++)
  {
	  EdevsFirstYear(ifish)= trans(EdevsInfo)(1)(ifish);
	  EdevsLastYear(ifish)= trans(EdevsInfo)(2)(ifish);
	  EdevsPhases(ifish)= trans(EdevsInfo)(7)(ifish);
  }
 END_CALCS
 init_vector_vector EdevsEst(1,numFish,EdevsFirstYear,EdevsLastYear,EdevsPhases);
 matrix Edevs(1,numFish,firstYear,lastYear);
 vector PenEdevs(1,numFish);
 
 // Declare different components of population dynamcis model (recruitment, exploitation and movement)
 3darray PopDyn3d(1,numTagGroups,1,numAreas,firstYear,lastYear+1);		// 3d array for population dynamics (group,area,year)
 3darray PopDynRec3d(1,numTagGroups,1,numAreas,firstYear,lastYear);		// 3d array for population dynamics - recruitment of tags
 3darray PopDynExp3d(1,numTagGroups,1,numAreas,firstYear,lastYear);		// 3d array for population dynamics - exploitation of tags
 3darray PopDynMov3d(1,numTagGroups,1,numAreas,firstYear,lastYear);		// 3d array for population dynamics - movement of tags
 
 // Estimated tag recoveries
 3darray RecapHat3d(1,numTagGroups,1,numFish,firstYear,lastYear);	// Estimated tag recaptures (group,fishery,year)
 vector RecapHatVec(1,1612);		// Vector of estimated tag recaptures (size of RecapSummary)
 matrix RecapObsSum(firstYear,lastYear,1,numFish);		// Matrix of estimated tag recaptures by fishery (for plotting)
 matrix RecapHatSum(firstYear,lastYear,1,numFish);		// Matrix of estimated tag recaptures by fishery (for plotting)

 // Likelihoods
 vector LogLikeVec(1,1612);					// Vector of negative log likelihood components for each area (column sums of LogLike)
 number likePen;							// Likelihood penaly
 
 // Overdispersion parameter tau (variance/mean) of the negative binomial distribution
 init_bounded_number_vector TauEst(1,numTauParams,Tau_LO,Tau_HI,Tau_PH);		// Vector of estimated tau (overdispersion) parameters
 vector Tau(1,numFish);		// Vector of fishery-specific tau (overdispersion) parameters
 
 number logLikeSum;

 // MCMC
 sdreport_number mcmcDummy;			// adding sdreport_number to make mcmc work  
 //sdreport_vector MovMatDiag;

 // Likelihood profiles
 //likeprof_number tagLossProf;
 
 objective_function_value objn;

 PRELIMINARY_CALCS_SECTION
 setupParams();
 

PROCEDURE_SECTION

 // Counters and indices		
 int irow;
 int icol;
 int igroupTag;
 int iareaTag;
 int iareaRec;
 int iyearRec;
 int ifish;
 int ilogqEst;
 int ilogq;
 int itagRepEst;
 int itagRep;
 int itauEst;
 int itau;

 //int iareaFrom;
 //int iareaTo;

 // Data
 int groupTag;
 int areaTag;
 int areaRec;
 int areaFish;
 int yearTag;
 int yearRec;
 int tagNum;
 int fishery;
 double hooks;  		// Should be a number		/dvariable 
 int recFish;		//? Should be a number
 
 // Initialize and populate the tag release 3d-array
 TagsDat3d.initialize();
 for (igroupTag=1; igroupTag<=numTagGroups; igroupTag++)		// Loop over tag group
 {
	 areaTag = TagSummary(igroupTag,2);		// Get tagging area
	 yearTag = TagSummary(igroupTag,3);	 	// Get tagging year
	 tagNum = TagSummary(igroupTag,4);		// Get numbers of tags releases	 
	 // Populate the tag release 3d array with the tag releases (recruitment of tags)
	 TagsDat3d(igroupTag,areaTag,yearTag) = tagNum;
 }
	
 // Initialize and populate the recapture 3d-array
 RecapsDat3d.initialize();
 for (irow=1; irow<=1612; irow++)
 {
	 groupTag = RecapSummary(irow,3);
	 fishery = RecapSummary(irow,1);	 
	 yearRec = RecapSummary(irow,4);	 
	 RecapsDat3d(groupTag,fishery,yearRec) = RecapSummary(irow,5);
 }	 
 
 // Initialize and populate the fishing effort 3d-array
 HooksDat3d.initialize();
 for (irow=1; irow<=988; irow++)
 {
	 fishery = HooksSummary(irow,1);
	 areaRec = FishSummary(fishery,2);
	 yearRec = HooksSummary(irow,2);	 
	 hooks = HooksSummary(irow,3);
	 if(yearRec>=firstYear & yearRec<=lastYear) {HooksDat3d(areaRec,fishery,yearRec)=hooks;}
 }	 

 // Initialize and populate the movement matrix for different movement hypothesis
 MovMat.initialize();
 //for (irow=1; irow<=nMovParams; irow++) {MovParamVector(irow) = .1;} 		// FOR DEBUGGING: Fix the elements of the movement vector	
 // Movement Hypothesis 1
 if(movH==1)
 {
	 for (irow=1; irow<=nMovParams; irow++)
	 {
		 areaTag = MovH1(irow,2);
		 areaRec = MovH1(irow,3);
		 MovMat(areaRec,areaTag) = MovParamVector(irow);
	 }
 }
 // Movement Hypothesis 2
 if(movH==2)
 {
	 for (irow=1; irow<=nMovParams; irow++)
	 {
		 areaTag = MovH2(irow,2);
		 areaRec = MovH2(irow,3);
		 MovMat(areaRec,areaTag) = MovParamVector(irow);
	 }
 }	 
 // Movement Hypothesis 3
 if(movH==3)
 {
	 for (irow=1; irow<=nMovParams; irow++)
	 {
		 areaTag = MovH3(irow,2);
		 areaRec = MovH3(irow,3);
		 MovMat(areaRec,areaTag) = MovParamVector(irow);
	 }
 }	 
 // Finish populating the movement matrix (diagonal computations)
 NonDiagSum = colsum(MovMat);		// Compute vector with col sums of the movement matrix non-diagonal elements
 likePen = 0;						// Initialize the likelihood penalty
 // Populate the movement matrix diagonal elements
 for (icol=1; icol<=numAreas; icol++)
 {
	 //dvariable fpen = 0.0;		// Create a penalty variable for posfun
     //MovMatDiag(icol) = 1 - NonDiagSum(icol);
     MovMatDiag(icol) = posfun(1 - NonDiagSum(icol), 0.01, likePen);		// The diagonal of MovMatrix is 1 - sum(column) restricted to be positive 
     MovMat(icol,icol) = MovMatDiag(icol);
     //likePen += 10000000*fpen;
 }
 
 // FOR DEBUGGING
 //for (irow=1; irow<=numQs; irow++) {logqEst(irow) = irow;} 		// FOR DEBUGGING:	

 // Initialize the catchability vector
 logq.initialize();
 ilogqEst=1;
 for (ilogq=1; ilogq<=numFish; ilogq++)			// Loop over the logq vector and fill in with the elements of qEst
 {
	 // Catchability hypothesis 1
	 if(qH==1)
	 {
		 if(qH1(ilogq,2)==-1)
		 {
			 logq(ilogq) = logqEst(ilogqEst);
			 ilogqEst++;
		 }
		 else
		 {
			 logq(ilogq)= logq(qH1(ilogq,2));
		 } 
      }
      
     // Catchability hypothesis 2
	 if(qH==2)
	 {
		 if(qH2(ilogq,2)==-1)
		 {
			 logq(ilogq) = logqEst(ilogqEst);
			 ilogqEst++;
		 }
		 else
		 {
			 logq(ilogq)= logq(qH2(ilogq,2));
		 } 
      }   
 }
 
 // ======== Populate the TagRep vector with the estimated tag-reporting parameters ==========
 // Initialize the tag-reporting vector
 TagRep.initialize();
 itagRepEst=1;
 for (itagRep=1; itagRep<=numFish; itagRep++)			// Loop over the tagRep vector and fill in with the elements of TagRepEst
 {
	 // Tag reporting hypothesis 1
	 if(tagRepH==1){TagRep(itagRep)=TagRepEst(1);}
     
	 // Tag reporting hypothesis 2
	 if(tagRepH==2){TagRep(itagRep)=TagRepEst(itagRep);}
	 
 	 // Tag reporting hypothesis 3
     if(tagRepH==3)
	 {
		 if(TagRepH3(itagRep,2)==-1)
		 {
			 TagRep(itagRep) =  TagRepEst(itagRepEst);
			 itagRepEst++;
		 }
		 else
		 {
			TagRep(itagRep)= TagRep(TagRepH3(itagRep,2));
		 } 
     }   
 }
 
 // ======== Populate the Tau vector with the estimated tau parameters ==========
 // Initialize the tao vector
 Tau.initialize();
 itauEst=1;
 for (itau=1; itau<=numFish; itau++)			// Loop over the Tau vector and fill in with the elements of TauEst
 {
	 // Tau hypothesis 1
	 if(tauH==1){Tau(itau)=TauEst(1);}
	 
     // Tau hypothesis 2 - fishery-specific overdispersion
	 if(tauH==2){Tau(itau)=TauEst(itau);}
 	 
	 // Tag reporting hypothesis 3
     if(tauH==3)
     {
	     if(TauH3(itau,2)==-1)
	     {
		     Tau(itau) = TauEst(itauEst);
		     itauEst++;
	     }
	     else
	     {
		     Tau(itau)= Tau(TauH3(itau,2));
	     }
     }
     // Tag reporting hypothesis 4
     if(tauH==4)
     {
	     if(TauH4(itau,2)==-1)
	     {
		     Tau(itau) = TauEst(itauEst);
		     itauEst++;
	     }
	     else
	     {
		     Tau(itau)= Tau(TauH4(itau,2));
	     }
     }   
 }
 
 // EFFORT DEVIATES
 Edevs.initialize();
 for(ifish=1; ifish<=numFish; ifish++)
 {
	 for(iyearRec=EdevsInfo(ifish,1); iyearRec<=EdevsInfo(ifish,2); iyearRec++)
	 {
		 Edevs(ifish,iyearRec)= EdevsEst(ifish,iyearRec);
 	 }
 	 PenEdevs(ifish)=0.5*norm2(EdevsEst(ifish)/EdevsInfo(ifish,6));
 }
 
 // Compute the fishing mortality rates for each fishery by area (Ffish)
 // Fishery-specific Fs by area
 Ffish3d.initialize();
 for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
 {
	 for (ifish=1; ifish<=numFish; ifish++)		// Loop over fishery
	 {
		 for (iyearRec=firstYear; iyearRec<=lastYear; iyearRec++)		// Loop over recpture year
		 {
			 Ffish3d(iareaRec,ifish,iyearRec)= mfexp(logq(ifish))*HooksDat3d(iareaRec,ifish,iyearRec)*mfexp(Edevs(ifish,iyearRec));		// Compute F=qE.exp(dev) for each fishery
		 }
	 }
 }
  
 // Compute the fishing mortality rates for each fishery - summary matrix
 // Fishery-specific Fs
 Ffish.initialize();
 for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
 {
	 for (iyearRec=firstYear; iyearRec<=lastYear; iyearRec++)		// Loop over recpture year
	 {
		 for (ifish=1; ifish<=numFish; ifish++)		// Loop over fishery
		 {
			 Ffish(ifish,iyearRec) += Ffish3d(iareaRec,ifish,iyearRec);		
		 }
	 }
 }

 // Compute the mean fishery-specific fishing mortalities during the 1980s
 // Get Ffish for 80s
 Ffish80s.initialize();
 for (ifish=1; ifish<=numFish; ifish++)		// Loop over fishery
 {
	 for (iyearRec=1980; iyearRec<=1989; iyearRec++)		// Loop over recpture year
	 {
		 Ffish80s(ifish,iyearRec) = Ffish(ifish,iyearRec);
	 }
 }
 FfishMean80s.initialize();
 for (ifish=1; ifish<=numFish; ifish++)		// Loop over fishery
 {
	 FfishMean80s[ifish] = mean(Ffish80s[ifish]);
 }

 // Compute the mean fishery-specific fishing mortalities during the 1990s
 // Get Ffish for 90s
 Ffish90s.initialize();
 for (ifish=1; ifish<=numFish; ifish++)		// Loop over fishery
 {
	 for (iyearRec=1990; iyearRec<=1999; iyearRec++)		// Loop over recpture year
	 {
		 Ffish90s(ifish,iyearRec) = Ffish(ifish,iyearRec);
	 }
 }
 FfishMean90s.initialize();
 for (ifish=1; ifish<=numFish; ifish++)		// Loop over fishery
 {
	 FfishMean90s[ifish] = mean(Ffish90s[ifish]);
 }

 // Get the fishery-specific fishing mortalities for 2000
 // Get Ffish for 2000
 Ffish2000.initialize();
 for (ifish=1; ifish<=numFish; ifish++)		// Loop over fishery
 {
	 Ffish2000(ifish) = Ffish(ifish,2000);
 }

 // Compute the fishing mortality rates for each area (Farea)
 // Area-specific Fs
 Farea.initialize();
 for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
 {
	 for (iyearRec=firstYear; iyearRec<=lastYear; iyearRec++)		// Loop over recpture year
	 {
		 for (ifish=1; ifish<=numFish; ifish++)		// Loop over fishery
		 {
			 Farea(iareaRec,iyearRec) += Ffish3d(iareaRec,ifish,iyearRec);		
		 }
	 }
 }
 
 // Compute the mean area-specific fishing mortalities during the 1980s
 // Get the Farea for the 80s
 Farea80s.initialize();
 {
	 for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
	 {
		 for (iyearRec=1980; iyearRec<=1989; iyearRec++)		// Loop over recpture year
		 {
			 Farea80s(iareaRec,iyearRec) = Farea(iareaRec,iyearRec);
		 }
	 }
 }
 FareaMean80s.initialize();
 for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
 {
	FareaMean80s[iareaRec] = mean(Farea80s[iareaRec]);
 }

 
 // Compute the mean area-specific fishing mortalities during the 1990s
 // Get the Farea for the 90s
 Farea90s.initialize();
 {
	 for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
	 {
		 for (iyearRec=1990; iyearRec<=1999; iyearRec++)		// Loop over recpture year
		 {
			 Farea90s(iareaRec,iyearRec) = Farea(iareaRec,iyearRec);
		 }
	 }
 }
 FareaMean90s.initialize();
 for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
 {
	FareaMean90s[iareaRec] = mean(Farea90s[iareaRec]);
 }

 // Compute the mean area-specific fishing mortalities in 2000
 // Get the Farea for 2000
 Farea2000.initialize();
 {
	 for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
	 {
		 Farea2000(iareaRec) = Farea(iareaRec,2000);
	 }
 }

 // Compute the dynamics of the tagged population
 PopDyn3d.initialize();
 PopDynRec3d.initialize();
 PopDynExp3d.initialize();
 PopDynMov3d.initialize();
 //cout<<PopDyn3d(1,1,firstYear)<<endl;
 for (igroupTag=1; igroupTag<=numTagGroups; igroupTag++)		// Loop over tag group
 {
	 // Get the tag release year
	 yearTag = TagSummary(igroupTag,3);
	  for (iyearRec=firstYear; iyearRec<=lastYear; iyearRec++)		// Loop over year
	  {
		  for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
		  {
			  // STEP 1 - Compute recruitment of tags
			  PopDynRec3d(igroupTag,iareaRec,iyearRec) = PopDyn3d(igroupTag,iareaRec,iyearRec) + TagsDat3d(igroupTag,iareaRec,iyearRec);
			  
			  // STEP 2 - Compute exploitation of tags
			  // Exploitation in the first year of recapture (with Fscale)
			  if(iyearRec==yearTag)
			  {
				  //PopDynExp3d(igroupTag,iareaRec,iyearRec) = PopDynRec3d(igroupTag,iareaRec,iyearRec) * mfexp(-((Farea(iareaRec,iyearRec) * Fscale(igroupTag)) + Mval + tagLoss));
				  PopDynExp3d(igroupTag,iareaRec,iyearRec) = PopDynRec3d(igroupTag,iareaRec,iyearRec) * mfexp(-((Farea(iareaRec,iyearRec) * Fscale(iareaRec)) + Mval + tagLoss));
			  }
			  // Exploitation is subsequent years of recapture
			  else // if(iyearRec!=yearTag)
			  {
				  PopDynExp3d(igroupTag,iareaRec,iyearRec) = PopDynRec3d(igroupTag,iareaRec,iyearRec) * mfexp(-(Farea(iareaRec,iyearRec)+ Mval + tagLoss));
			  }
		  }
		  // STEP 3 - Compute movement of tags
		  for (iareaRec=1; iareaRec<=numAreas; iareaRec++)		// Loop over recapture area
		  {
			  for (iareaTag=1; iareaTag<=numAreas; iareaTag++)		// Loop tagging area 
			  {
				  // Do movement
				  PopDynMov3d(igroupTag,iareaRec,iyearRec) += PopDynExp3d(igroupTag,iareaTag,iyearRec) * MovMat(iareaRec,iareaTag);
			  }
			  // Age fish to follwoing year
			  PopDyn3d(igroupTag,iareaRec,iyearRec+1) = PopDynMov3d(igroupTag,iareaRec,iyearRec);
		  }
      }
 }			  
 
 // Compute the estimated tag recaptures (observation model)
 RecapHat3d.initialize();
 RecapHatVec.initialize();	
 for (igroupTag=1; igroupTag<=numTagGroups; igroupTag++)		// Loop over tag group
 {
	// Get the tag release year
	yearTag = TagSummary(igroupTag,3);
	// Loop over fisheries
	for (ifish=1; ifish<=numFish; ifish++)
	{
		// Get the fishing area
		areaFish = FishSummary(ifish,2);

		// Loop over recapture year
		for (iyearRec=firstYear; iyearRec<=lastYear; iyearRec++)		// Loop over recapture year
		{
			// Compute recaptures in first year of recapture (catch equation with Fscale)
			if(iyearRec==yearTag)
			{
				//RecapHat3d(igroupTag,ifish,iyearRec) = TagRep(ifish) * PopDynRec3d(igroupTag,areaFish,iyearRec) * ((Ffish3d(areaFish,ifish,iyearRec)*Fscale(igroupTag))/((Farea(areaFish,iyearRec)*Fscale(igroupTag)) + Mval + tagLoss)) * (1-mfexp(-((Farea(areaFish,iyearRec)*Fscale(igroupTag)) + Mval + tagLoss)));
				RecapHat3d(igroupTag,ifish,iyearRec) = TagRep(ifish) * PopDynRec3d(igroupTag,areaFish,iyearRec) * ((Ffish3d(areaFish,ifish,iyearRec)*Fscale(areaFish))/((Farea(areaFish,iyearRec)*Fscale(areaFish)) + Mval + tagLoss)) * (1-mfexp(-((Farea(areaFish,iyearRec)*Fscale(areaFish)) + Mval + tagLoss)));
			}
			// Compute recpatures in subsequent years
			else //if(iyearRec!=yearTag)
			{
				RecapHat3d(igroupTag,ifish,iyearRec) = TagRep(ifish) * PopDynRec3d(igroupTag,areaFish,iyearRec) * (Ffish3d(areaFish,ifish,iyearRec)/(Farea(areaFish,iyearRec) + Mval + tagLoss)) * (1-mfexp(-(Farea(areaFish,iyearRec) + Mval + tagLoss)));
			}
		}
 	}
 }
 
 // Compute the likelihoods
 for (irow=1; irow<=1612; irow++)		// Loop over row in RecapSummary
 {
	 fishery = RecapSummary(irow,1);
	 groupTag = RecapSummary(irow,3);
	 yearRec = RecapSummary(irow,4);
	 
	 // Fill in vector of predicted recaptures for likelihood computations
	 RecapHatVec(irow) = RecapHat3d(groupTag,fishery,yearRec);

	 // Compute the likelihood
	 if(likeType==1)		// Least squares likelihood
	 {
		 LogLikeVec(irow) = square((RecapSummary(irow,5)+likePoissonConst)-((RecapHatVec(irow) + likePoissonConst)));
	 }
	 if(likeType==2)		// Poisson likelihood
	 {
		 LogLikeVec(irow) = RecapHatVec(irow) + likePoissonConst - RecapSummary(irow,5)*log(RecapHatVec(irow) + likePoissonConst);
	 }
	 if(likeType==3)		// Negative binomial likelihood
	 {
		 LogLikeVec(irow) = -log_negbinomial_density((RecapSummary(irow,5)+ likePoissonConst),(RecapHatVec(irow)+ likePoissonConst),Tau(fishery));
		 //LogLikeVec(irow) = log_negbinomial_density(RecapSummary(irow,5),RecapHatVec(irow),tau);
     }
 }

 logLikeSum = sum(LogLikeVec);	// Loglike

 mcmcDummy = exp(logqEst(1));		// Declared for sdreport to make MCMC run
 MovMatDiag2=MovMatDiag;
 FfishMean80s2=FfishMean80s;
 FareaMean80s2=FareaMean80s;
 FfishMean90s2=FfishMean90s;
 FareaMean90s2=FareaMean90s;
 Ffish2000b=Ffish2000;
 Farea2000b=Farea2000;

 //tagLossProf = tagLoss;
 
 // Objective function	 
 objn = sum(LogLikeVec) + likePen*1000 + (dummy - 1)*(dummy - 1) + sum(PenEdevs);
 //objn = dummy*dummy;			// for debugging
 
 if(mceval_phase()) get_posteriors();

FUNCTION recapObs
 // Loop over Recap3d and populate table with the annual series of total numbers of observed recaptures by fishery
 RecapObsSum.initialize();
 for (int ifish=1; ifish<=numFish; ifish++)
 {
	 for (int iyearRec=firstYear; iyearRec<=lastYear; iyearRec++)		// Loop over recapture year
	 {
		 for (int igroupTag=1; igroupTag<=numTagGroups; igroupTag++)		// Loop over tag group
		 {
			 RecapObsSum(iyearRec,ifish) += RecapsDat3d(igroupTag,ifish,iyearRec);
		 }
	 }
 }
 
FUNCTION recapHat
// TO BE FIXED
 // Loop over RecapHat3d and populate table with the annual series of total numbers of predicted recaptures by fishery
 //RecapHatSum.initialize();
 //for (int ifish=1; ifish<=numFish; ifish++)
 //{
//	 for (int iyearRec=firstYear; iyearRec<=lastYear; iyearRec++)		// Loop over recapture year
//	 {
//		 for (int igroupTag=1; igroupTag<=numTagGroups; igroupTag++)		// Loop over tag group
//		 {
//			 RecapHatSum(iyearRec,ifish) += RecapHat3d(igroupTag,ifish,iyearRec);
//		 }
//	 }
 //}
 dvariable recapHat;
 for(int ifish=1; ifish<=numFish; ifish++)
 {
	 for (int iyearRec=firstYear; iyearRec<=lastYear; iyearRec++)		// Loop over recapture year
	 {
		 recapHat.initialize();
		 for (int irow=1; irow<=1612; irow++)		// Loop over row in RecapSummary
		 {
			 int fishery = RecapSummary(irow,1);
			 //int groupTag = RecapSummary(irow,3);
			 int yearRec = RecapSummary(irow,4);
			 if(fishery==ifish & yearRec==iyearRec)
			 {
				 recapHat = RecapHatVec(irow);
			 }
			 else(recapHat=0);
//			 recapHat+=recapHat;
			 RecapHatSum(iyearRec,ifish)+=recapHat;
		 }
	 }
 }
				 
FUNCTION setupParams
 for (int i=1; i<=numQs; i++){logqEst(i) = logqEst_INIT(i);}	// Catchabilities	
 for (i=1; i<=nMovParams; i++) {MovParamVector(i) = MovParams_INIT(i);}		// Movement parameters
 Mval=M_INIT;			// Natural mortality
 //for (i=1; i<=numTagGroups; i++) {Fscale(i)=Fscale_INIT(i);}		// Fscale
 for (i=1; i<=numAreas; i++) {Fscale(i)=Fscale_INIT(i);}		// Fscale
 //Fscale = Fscale_INIT;  // Fscales
 //tagMort=tagMort_INIT;  // Tag-induced mortality
 tagLoss=tagLoss_INIT;	// Tag loss
 for (i=1; i<=numTagRepParams; i++) {TagRepEst(i) = TagRep_INIT(i);}		// Tag reporting parameters
 for (i=1; i<=numTauParams; i++) {TauEst(i) = Tau_INIT(i);}		// Tau (overdispersion) parameters
 //cout<<" logqEst = "<<endl<<logqEst<<endl;
 //cout<<" TagRepEst = "<<endl<<TagRepEst<<endl;

//FUNCTION write_mcmc

 //if(mcmc_report_lines == 0)
 //{
//	 mcmc_report<<"label1,label2"<<endl;
 //}
 //else
 //{ 
  //mcmc_report<<MovMat(1,1)<<endl;
  //mcmc_report<<MovVector(1)<<","<<MovVector(2)<<","<<MovVector(3)<<","<<MovVector(4)<<","<<MovDiag<<","<<logqEst<<endl;
 //}
 //mcmc_report_lines++;

FUNCTION get_posteriors
  cout<<" mceval counter: "<<mccounter<<endl;

  if (mccounter==1)    // First pass through the mceval phase
  {
    // Delete any old mcmc output files, will generate a warning if no files exist but will play through just fine
    system("del posteriors.rep");
    system("del derived_posteriors.rep");
    cout<<" did system commands "<<endl;
  };

  // Define the mcmc output files
  ofstream posts("posteriors.rep", ios::out | ios::app);
  ofstream der_posts("derived_posteriors.rep", ios::out | ios::app);

  if (mccounter==1)    // First pass through the mceval phase
  {
    // Produce headers for the posteriors.rep and derived_posteriors.rep files
    // Maintain order of par file and include all estimated parameters
    cout<<" write mcmc headers "<<endl;

    // Posteriors for model parameters
    //posts << "negLogLike" << "\t";	// Objective function
    posts << "Mval"   << "\t"; 		// Natural mortality
    //posts << "tagMort"  << "\t"; 	// Tag-induced motality
    posts << "tagLoss"  << "\t"; // Tag loss
    //for (int i=1; i<=nMovParams; i++) posts << "MovParVector" << i << "\t";		// Movement parameters
    for (int i=1; i<=numAreas; i++)
    {
	    for (int j=1; j<=numAreas; j++)
	    {
		    posts << "MovMat" << i << j << "\t";		// Movement parameters
	    }
    }
    //for (i=1; i<=numAreas; i++) posts << "MovMatDiag" << i << "\t";		// Movement parameters - diagonal elements
    for (i=1; i<=numTagRepParams; i++) posts << "TagRepVector" << i << "\t";		// Tag reporting rates
    //for (int irow=1; irow<=numQs; irow++)     posts << "logqEst" << irow << "\t"; 			// Catchability parameters
    posts << endl; 
    
    // Posteriros for derived quantities - fishing mortalities
    //der_posts << endl;
  };

   cout<<" do mcmc output"<<endl;
   // produce standard output of all estimated parameters
   posts << Mval << "\t"; // objective function
   posts << tagLoss << "\t"; // objective function
   //for (int i=1; i<=nMovParams; i++) posts << MovParamVector(i) << "\t";		// Movement parameters
   for (int i=1; i<=numAreas; i++)
   {
	   for (int j=1; j<=numAreas; j++)
	   {
		   posts << MovMat(i,j) << "\t";		// Movement parameters
	   }
   }
   //for (i=1; i<=numAreas; i++) posts << MovMatDiag(i) << "\t";		// Movement parameters - diagonal elements
   for (i=1; i<=numTagRepParams; i++) posts << TagRep(i) << "\t";		// Tag reporting rates
   posts << endl;

  // don't know why the next two lines are commented out.
  //  posts.close();
  //  der_posts.close();
  mccounter += 1;   // increment the counter
 
 
 
REPORT_SECTION

 report << "#==================== DIMENSIONS ====================" << endl;
 report << " " << endl;
 report << " " << endl;

 report << "# First year" << endl;
 report << firstYear << endl;
 report << " " << endl;
 
 report << "# Last year" << endl;
 report << lastYear << endl;
 report << " " << endl;
 
 report << "# Number of areas" << endl;
 report << numAreas << endl;
 report << " " << endl;
 
 report << "# Number of fisheries" << endl;
 report << numFish << endl;
 report << " " << endl;

 report << "# Number of tag groups" << endl;
 report << numTagGroups << endl;
 report << " " << endl;
 
 report << " " << endl;
 report << "#==================== INDICES FOR DIFFERENT HYPOTHESIS ====================" << endl;
 report << " " << endl;
 report << " " << endl;

 report << "# Movement hypothesis 1 - indices for movement matrix" << endl;
 report << MovH1 << endl;
 report << " " << endl;

 report << "# Movement hypothesis 2 - indices for movement matrix" << endl;
 report << MovH2 << endl;
 report << " " << endl;

 report << "# Movement hypothesis 3 - indices for movement matrix" << endl;
 report << MovH3 << endl;
 report << " " << endl;
 
 report << "# Catchability hypothesis 1 - indices for sharing qs" << endl;
 report << qH1 << endl;
 report << " " << endl;

 report << "# Catchability hypothesis 2 - indices for sharing qs" << endl;
 report << qH2 << endl;
 report << " " << endl;
 
 report << "# Tag reporting hypothesis 3 - indices for sharing tag reporting rates" << endl;
 report << TagRepH3 << endl;
 report << " " << endl;

 report << "# Tau (overdispersion) hypothesis 3 - indices for sharing tau parameters" << endl;
 report << TauH3 << endl;
 report << " " << endl;

 report << "# Tau (overdispersion) hypothesis 4 - indices for sharing tau parameters" << endl;
 report << TauH4 << endl;
 report << " " << endl;

 
 report << " " << endl;
 report << "#==================== SWITCHES ====================" << endl;
 report << " " << endl;
 report << " " << endl;

 report << "# Movement hypothesis" << endl;
 report << movH << endl;
 report << " " << endl;
 
 report << "# Catchability hypothesis" << endl;
 report <<qH << endl;
 report << " " << endl;

 report << "# Tag-reporting hypothesis" << endl;
 report << tagRepH << endl;
 report << " " << endl;
  
 report << "# Likelihood type switch" << endl;
 report << likeType << endl;
 report << " " << endl;
 
 report << "# Tau (overdispersion) hypothesis" << endl;
 report << tauH << endl;
 report << " " << endl;

 report << " " << endl;
 report << "#==================== PARAMETER COUNTS AND OTHER QUANTITIES ====================" << endl;
 report << " " << endl;
 report << " " << endl;
 
 report << "# Number of estimated movement parameters" << endl;
 report << nMovParams << endl;
 report << " " << endl;
 
 report << "# Number of estimated catchabilities" << endl;
 report << numQs << endl;
 report << " " << endl;
 
 report << "# Number of estimated tag-reporting parameters" << endl;
 report << numTagRepParams << endl;
 report << " " << endl;

 report << "# Poisson likelihood additive constant" << endl;
 report << likePoissonConst << endl;
 report << " " << endl;

 report << " " << endl;
 report << "#==================== PARAMETER CONTROLS ====================" << endl;
 report << " " << endl;
 report << " " << endl;

 report << "# Parameter controls" << endl;
 report << ParamsInfo << endl;
 report << " " << endl;
 
 report << "# Catchability controls - H1" << endl;
 report << QsInfoH1 << endl;
 report << " " << endl;

 report << "# Catchability controls - H2" << endl;
 report << QsInfoH2 << endl;
 report << " " << endl;
 
 report << "# Catchabilty controls - selected" << endl;
 report << QsInfoSel << endl;
 report << " " << endl;

 report << "# Movement parameter controls - H1" << endl;
 report << MovParamInfoH1 << endl;
 report << " " << endl;

 report << "# Movement parameter controls - H2" << endl;
 report << MovParamInfoH2 << endl;
 report << " " << endl;

 report << "# Movement parameter controls - H3" << endl;
 report << MovParamInfoH3 << endl;
 report << " " << endl;
  
 report << "# Movement parameter controls - selected" << endl;
 report << MovParamInfoSel << endl;
 report << " " << endl;

 report << "# Tag reporting rate controls - H1" << endl;
 report << TagRepInfoH1 << endl;
 report << " " << endl;
 
 report << "# Tag reporting rate controls - H2" << endl;
 report << TagRepInfoH2 << endl;
 report << " " << endl;

 report << "# Tag reporting rate controls - H3" << endl;
 report << TagRepInfoH3 << endl;
 report << " " << endl;

 report << "# Tag reporting rate controls - selected" << endl;
 report << TagRepInfoSel << endl;
 report << " " << endl;
 
 report << "# Effort deviate controls" << endl;
 report << EdevsInfo << endl;
 report << " " << endl;
 
 report << "# Tau (overdispersion) controls - H1" << endl;
 report << TauInfoH1 << endl;
 report << " " << endl;

 report << "# Tau (overdispersion) controls - H2" << endl;
 report << TauInfoH2 << endl;
 report << " " << endl;

 report << "# Tau (overdispersion) controls - H3" << endl;
 report << TauInfoH3 << endl;
 report << " " << endl;

 report << "# Tau (overdispersion) controls - H4" << endl;
 report << TauInfoH4 << endl;
 report << " " << endl;

 report << "# Tau (overdispersion) controls - selected" << endl;
 report << TauInfoSel << endl;
 report << " " << endl;

 report << "# logqEst_LO" << endl;
 report << logqEst_LO << endl;
 report << " " << endl;

 report << "# logqEst_HI" << endl;
 report << logqEst_HI << endl;
 report << " " << endl;

 report << "# logqEst_INIT" << endl;
 report << logqEst_INIT << endl;
 report << " " << endl;

 report << "# logqEst_PH" << endl;
 report << logqEst_PH << endl;
 report << " " << endl;
 
 report << "# movParams_LO" << endl;
 report << MovParams_LO << endl;
 report << " " << endl;

 report << "# movParams_HI" << endl;
 report << MovParams_HI << endl;
 report << " " << endl;

 report << "# movParams_INIT" << endl;
 report << MovParams_INIT << endl;
 report << " " << endl;

 report << "# movParams_PH" << endl;
 report << MovParams_PH << endl;
 report << " " << endl;

 report << "# tagRep_LO" << endl;
 report << TagRep_LO << endl;
 report << " " << endl;

 report << "# tagRep_HI" << endl;
 report << TagRep_HI << endl;
 report << " " << endl;

 report << "# tagRep_INIT" << endl;
 report << TagRep_INIT << endl;
 report << " " << endl;
 
 report << "# tagRep_PH" << endl;
 report << TagRep_PH << endl;
 report << " " << endl;
 
 report << "# F scale INIT" << endl;
 report << Fscale_INIT << endl;
 report << " " << endl;

 
 report << " " << endl;
 report << "#==================== ESTIMATED PARAMETERS  ====================" << endl;
 report << " " << endl;
 report << " " << endl;
 
 report << "# Vector of movement parameters" << endl;
 report << MovParamVector << endl;
 report << " " << endl;

 report << "# Movement matrix" << endl;
 report << MovMat << endl;
 report << " " << endl;

 report << "# Diagonal elements of movement matrix" << endl;
 report << MovMatDiag << endl;
 report << " " << endl;

 report << "# Movement matrix non-diagonal row sum vector" << endl;
 report << NonDiagSum << endl;
 report << " " << endl;
 
 report << "# Estimated catchability parameters (in log space) - logqEst" << endl;
 report << logqEst << endl;
 report << " " << endl;
 
 report << "# Fishery-specific catchabilities (in log space) - logq" << endl;
 report << logq << endl;
 report << " " << endl;

 report << "# Natural mortality" << endl;
 report << Mval << endl;
 report << " " << endl;
 
 report << "# Tag-induced mortality" << endl;
 //report << tagMort << endl;
 report << " " << endl;

 report << "# Tag loss" << endl;
 report << tagLoss << endl;
 report << " " << endl;

 report << "# Estimated tag reporting rates" << endl;
 report << TagRepEst << endl;
 report << " " << endl;

 report << "# Fishery-specfic tag reporting rates" << endl;
 report << TagRep << endl;
 report << " " << endl;
 
 report << "# Fscale" << endl;
 report << Fscale << endl;
 report << " " << endl;

 report << "# Effort deviates - estimated" << endl;
 //report << EdevsEst << endl;
 report << " " << endl;

 report << "# Effort deviates" << endl;
 report << Edevs << endl;
 report << " " << endl;

 report << " " << endl;
 report << "#==================== DATA  ====================" << endl;
 report << " " << endl;
 report << " " << endl;

 report << "# Fishery summary" << endl;
 report << FishSummary << endl;
 report << " " << endl;

 report << "# Tags summary" << endl;
 report << TagSummary << endl;
 report << " " << endl;
 
 report << "# Recaps summary" << endl;
 report << RecapSummary << endl;
 report << " " << endl;
 
 report << "# Hooks summary" << endl;
 report << HooksSummary << endl;
 report << " " << endl;
 
 report << "# Tag release data - 3d array" << endl;
 report << TagsDat3d << endl;
 report << " " << endl;

 report << "# Recap data - 3d array" << endl;
 report << RecapsDat3d << endl;
 report << " " << endl;

 report << "# Hooks - 3d array" << endl;
 report << HooksDat3d << endl;
 report << " " << endl;

 report << " " << endl;
 report << "#==================== DERIVED QUANTITIES  ====================" << endl;
 report << " " << endl;
 report << " " << endl;

 report << "# Fishery-specifc fishing mortalities by area (Ffish3d)" << endl;
 report << Ffish3d << endl;
 report << " " << endl;

 report << "# Fishery-specifc fishing mortalities (Ffish)" << endl;
 report << Ffish << endl;
 report << " " << endl;
 
 report << "# Fishery-specifc fishing mortalities during the 1980s (Ffish80s)" << endl;
 report << Ffish80s << endl;
 report << " " << endl;

 report << "# Mean fishery-specifc fishing mortalities during the 1980s (FfishMean80s)" << endl;
 report << FfishMean80s << endl;
 report << " " << endl;

 report << "# Fishery-specifc fishing mortalities during the 1990s (Ffish90s)" << endl;
 report << Ffish90s << endl;
 report << " " << endl;

 report << "# Mean fishery-specifc fishing mortalities during the 1990s (FfishMean90s)" << endl;
 report << FfishMean90s << endl;
 report << " " << endl;

 report << "# Fishery-specifc fishing mortalities in 2000 (Ffish2000)" << endl;
 report << Ffish2000 << endl;
 report << " " << endl;

 report << "# Area-specific fishing mortalities (Farea)" << endl;
 report << Farea << endl;
 report << " " << endl;
 
 report << "# Area-specific fishing mortalities during the 1980s (Farea80s)" << endl;
 report << Farea80s << endl;
 report << " " << endl;

 report << "# Mean area-specific fishing mortalities during the 1980s (FareaMean80s)" << endl;
 report << FareaMean80s << endl;
 report << " " << endl;

 report << "# Area-specific fishing mortalities during the 1990s (Farea90s)" << endl;
 report << Farea90s << endl;
 report << " " << endl;
 
 report << "# Mean area-specific fishing mortalities during the 1990s (FareaMean90s)" << endl;
 report << FareaMean90s << endl;
 report << " " << endl;

 report << "# Area-specific fishing mortalities in 2000 (Farea2000)" << endl;
 report << Farea2000 << endl;
 report << " " << endl;

 report << "# Population dynamcis - recruitment of tags " << endl;
 report << PopDynRec3d << endl;
 report << " " << endl;

 report << "# Population dynamcis - exploitation of tags " << endl;
 report << PopDynExp3d << endl;
 report << " " << endl;
 
 report << "# Population dynamcis - movement of tags " << endl;
 report << PopDynMov3d << endl;
 report << " " << endl;

 report << "# Population dynamcis" << endl;
 report << PopDyn3d << endl;
 report << " " << endl;
 
 report << "# Estimated numbers of tag recaptures - 3d array" << endl;
 report << RecapHat3d << endl;
 report << " " << endl;
 
 recapObs();
 report << "# Observed annual numbers of tag recaptures by fishery - for plotting" << endl;
 report << RecapObsSum << endl;
 report << " " << endl;

 recapHat();
 report << "# Estimated annual numbers of tag recaptures by fishery - for plotting" << endl;
 report << RecapHatSum << endl;
 report << " " << endl;

 report << "# Estimated numbers of tag recaptures - vector" << endl;
 report << RecapHatVec << endl;
 report << " " << endl;
  
 report << "# Log-likelihood" << endl;
 report << logLikeSum << endl;
 report << " " << endl;

 report << "# Vector of log-likelihoods" << endl;
 report << LogLikeVec << endl;
 report << " " << endl;

 report << "# Likelihood penalty" << endl;
 report <<  likePen << endl;
 report << " " << endl;

 report << "# Tau (overdispersion) of the negative binomial density" << endl;
 report << TauEst << endl;
 report << Tau << endl;
 report << " " << endl;

 //report << Tau_LO << endl;
 //report << Tau_HI << endl;
 //report << Tau_INIT << endl;
 //report << Tau_PH << endl;
 
TOP_OF_MAIN_SECTION
  
  gradient_structure::set_MAX_NVAR_OFFSET(1000);



