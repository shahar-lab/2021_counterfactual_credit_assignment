data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Nraffle;                                           //number of offers per trial


  //Behavioral data:
  //each variable being a subject x trial matrix
  //the data is padded in make_standata function so that all subjects will have the same number of trials
  int<lower = 0> choice[Nsubjects,Ntrials];        //index of which arm was pulled coded 1 to 4
  int<lower = 0> unchosen[Nsubjects,Ntrials];     //index of which arm was not selected coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> offer1[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> offer2[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> selected_offer[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials];           //coding whether a trial is the first in a block to allow for Qval rest

}

transformed data{
  int<lower = 1> Nparameters=3; //number of parameters
  vector[Narms] Qvalue_initial;     // initial values for Qvalues (defined here to aviod doing this many times across iterations)
  Qvalue_initial = rep_vector(0.5, Narms);
}

parameters {
// Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  //population level parameters 
  vector[Nparameters] population_locations;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] population_scales;          //vector of random effects variance for each model parameter
  
//individuals level
  vector[Nsubjects] alpha_ch_random_effect;
  vector[Nsubjects] alpha_unch_random_effect;
  vector[Nsubjects] beta_random_effect;

}


transformed parameters {
//declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects]  alpha_ch;
  vector<lower=0, upper=1>[Nsubjects]  alpha_unch;
  vector                  [Nsubjects]  beta;
  

    
  for (subject in 1:Nsubjects) {
    alpha_ch[subject]   = inv_logit(population_locations[1]  + population_scales[1]  * alpha_ch_random_effect[subject]);
   alpha_unch[subject]       = inv_logit(population_locations[2]  + population_scales[2]  * alpha_unch_random_effect[subject]);
    beta[subject]    =          (population_locations[3]  + population_scales[3]  * beta_random_effect[subject]);
    
  }

}



model {
  
  // population level  
  population_locations  ~ normal(0, 2);            
  population_scales     ~ cauchy(0,2);        

  // indvidual level  
  alpha_ch_random_effect ~ std_normal();
  alpha_unch_random_effect  ~ std_normal();
  beta_random_effect  ~ std_normal();



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector[Narms] Qcard; 
    vector[Nraffle] Qoffer;
    real Qdiff;
    real PE;
    

      for (trial in 1:Ntrials_per_subject[subject]){
        
          if (first_trial_in_block[subject,trial] == 1) {
                        Qcard=Qvalue_initial;
        }

          Qoffer[1]=Qcard[offer1[subject,trial]];
          Qoffer[2]=Qcard[offer2[subject,trial]];

        //liklihood function

        Qdiff = Qoffer[2]-Qoffer[1];
        // if (subject==3) {
        //   print(Qdiff);
        //   print("beta");
        //   print(beta[subject]);
        //   print("alpha_unch");
        //   print(alpha_unch[subject]);
        //   print("alpha_ch");
        //   print(alpha_ch[subject]);
        // }
        selected_offer[subject, trial] ~ categorical_logit(beta[subject] * Qoffer);
            
        //Qvalues update
        PE = (reward[subject,trial] - Qcard[choice[subject,trial]]);
        Qcard[choice[subject,trial]] += alpha_ch[subject]  * PE;
        Qcard[unchosen[subject,trial]] += alpha_unch[subject]  * -PE;

        //put the Q values in the range of 0 to 1
          if (Qcard[choice[subject,trial]]>1){
            Qcard[choice[subject,trial]]=1;
          }
          else if(Qcard[choice[subject,trial]]<0){
            Qcard[choice[subject,trial]]=0;
          }
          if (Qcard[unchosen[subject,trial]]>1){
            Qcard[unchosen[subject,trial]]=1;
          }
          else if(Qcard[unchosen[subject,trial]]<0){
            Qcard[unchosen[subject,trial]]=0;
          }
      } 
  }
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
