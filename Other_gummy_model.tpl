GLOBALS_SECTION
  #include <iostream>
  #include <fstream>
  #undef REPORT
  #define REPORT(object) report<<#object "\n" << object << endl;
  #include <time.h>
  time_t start,finish;
  long hour,minute,second;
  double elapsed_time;

DATA_SECTION
  init_number nzones;
  init_number nrec;
  init_matrix Rec_dat(1,nrec,1,4);
   
  
   int t;
   int y;
  
  

PARAMETER_SECTION
  //init_bounded_number prob_stay(0.5,1,1);          //West coast
  //init_bounded_number prob_move(0,0.6,2);

  init_bounded_number prob_stay1(0.5,1,1);          //West Coast and Zone 1
  init_bounded_number prob_move1(0,0.6,2);

  init_bounded_number prob_stay2(0.5,1,1);          //Zone 2
  init_bounded_number prob_move2(0,0.6,2);


  objective_function_value f

  //Declare objects used in Move_rate()
  matrix Mov_mat(1,nzones,1,nzones);
  vector Rels(1,nrec);
  vector Recs(1,nrec);
  vector Rels_yr(1,nrec);
  vector Recs_yr(1,nrec);
  number Tag_NLL;
  matrix Move(1,nzones,1,nzones);
  number N_yrs;
  vector Pred_Prob(1,nrec);

  

PRELIMINARY_CALCS_SECTION
  Rels=column(Rec_dat,1);
  Rels_yr=column(Rec_dat,2);
  Recs=column(Rec_dat,3);
  Recs_yr=column(Rec_dat,4);

  //cout<<Recs_yr<<endl;exit(1);

PROCEDURE_SECTION
  Move_rate();

  

FUNCTION Move_rate
  Tag_NLL.initialize();
   
   //fill in Movement matrix
  Mov_mat(1,1)=prob_stay1;
  Mov_mat(1,2)=1-(prob_stay1+prob_move1);
  Mov_mat(1,3)=prob_move1;

  Mov_mat(2,1)=prob_move1;
  Mov_mat(2,2)=prob_stay1;
  Mov_mat(2,3)=1-(prob_move1+prob_stay1);

  Mov_mat(3,1)=1-(prob_stay2+prob_move2);
  Mov_mat(3,2)=prob_move2;
  Mov_mat(3,3)=prob_stay2;


  //Predict location of recaptured shark
  for(t=1; t<=nrec;t++)
  {
    N_yrs=Recs_yr(t)-Rels_yr(t);
    Move=Mov_mat;
    if(N_yrs>1)
    {    
     for(y=2; y<=N_yrs;y++) Move=Move*Mov_mat;
    }
    int from=value(Rels(t));          //C++ syntax: need value() to convet to int
    int to=value(Recs(t));
    Pred_Prob(t)=Move(from,to);

   // cout<<"Release "<<Rels_yr(t)<<"  Recapture"<<Recs_yr(t)<<"   Move\n "<<Move<<endl;
  //cout<<t<<"  Rels(t) "<<Rels(t)<< "   Recs(t) "<<Recs(t)<<"  Pred_Prob(t) "<<Pred_Prob(t)<<endl;

  //Calculate likelihood of observations
  
     // Least squares likelihood
  //Tag_NLL += square((Tag_Recaptures(t,y,a)+likePoissonK)-((Tag_Pred_Rec(t,y,a) + likePoissonK)));
	 
    // Poisson likelihood
  Tag_NLL += Pred_Prob(t) - 1*log(Pred_Prob(t) + 0.00001);	 
 
  }
  

 
 // Objective function	 
 f = Tag_NLL ;
 
 //cout<<"f "<<f<<endl;
 //exit(1);




REPORT_SECTION
  //REPORT(Pred_Prob);               //predicted probabilities


