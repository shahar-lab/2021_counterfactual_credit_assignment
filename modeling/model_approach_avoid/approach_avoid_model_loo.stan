data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    
  int<lower = 2> Narms;                                             
  int<lower = 2> Nraffle;                                           


  //Behavioral data:
  int<lower = 0> choice[Nsubjects,Ntrials];        
  int<lower = 0> unchosen[Nsubjects,Ntrials];     
  int<lower = 0> reward[Nsubjects,Ntrials];       
  int<lower = 0> offer1[Nsubjects,Ntrials];       
  int<lower = 0> offer2[Nsubjects,Ntrials];       
  int<lower = 0> selected_offer[Nsubjects,Ntrials];
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials];           
  int<lower = 0> fold[Nsubjects,Ntrials];           
  real           testfold;                          

}

transformed data{
  int<lower = 1> Nparameters=3; 
  vector[Narms] Qvalue_initial; 
  Qvalue_initial = rep_vector(0.5, Narms);
}

parameters {

  //population level parameters 
  vector[Nparameters] population_locations;         
  vector<lower=0>[Nparameters] population_scales;   
  
//individuals level
  vector[Nsubjects] alpha_ch_random_effect;
  vector[Nsubjects] beta_random_effect;
  vector[Nsubjects] omega_random_effect;

}


transformed parameters {
//declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects]  alpha_ch;
  vector                  [Nsubjects]  beta;
  vector<lower=0, upper=1>[Nsubjects]  omega;

    
  for (subject in 1:Nsubjects) {
    alpha_ch[subject]   = inv_logit(population_locations[1]  + population_scales[1]  * alpha_ch_random_effect[subject]);
    beta[subject]       =          (population_locations[2]  + population_scales[2]  * beta_random_effect[subject]);
    omega[subject]      = inv_logit(population_locations[3]  + population_scales[3]  * omega_random_effect[subject]);
  }

}



model {
  
  // population level  
  population_locations  ~ normal(0, 2);            
  population_scales     ~ cauchy(0,2);        

  // indvidual level  
  alpha_ch_random_effect    ~ std_normal();
  beta_random_effect        ~ std_normal();
  omega_random_effect  ~ std_normal();
 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector[Narms] Qcard; 
    vector[Narms] Qavoid; 
    vector[Nraffle] Qoffer; 
    

      for (trial in 1:Ntrials_per_subject[subject]){
          if(fold[subject,trial] != testfold) {
            
          if (first_trial_in_block[subject,trial] == 1) {
                        Qcard=Qvalue_initial;
                        Qavoid=Qvalue_initial;
        }
        
          Qoffer[1]=omega[subject]*Qcard[offer1[subject,trial]]+(1-omega[subject])*Qavoid[offer2[subject,trial]];
          Qoffer[2]=omega[subject]*Qcard[offer2[subject,trial]]+(1-omega[subject])*Qavoid[offer1[subject,trial]];

        //liklihood function 
         selected_offer[subject, trial] ~ categorical_logit(beta[subject] * Qoffer);
            
        //Qvalues update
        Qcard[choice[subject,trial]]    += alpha_ch[subject] * (reward[subject,trial] - Qcard[choice[subject,trial]]);
        Qavoid[unchosen[subject,trial]] += alpha_ch[subject] * (reward[subject,trial] - Qavoid[unchosen[subject,trial]]);

      }
      }
  }
  
  
  
  
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

generated quantities {

  matrix[Nsubjects,Ntrials]     log_lik;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial (placed in generetaed quantities block to save time and memory)

  { //
    log_lik=rep_matrix(0,Nsubjects,Ntrials);
        
    for (subject in 1:Nsubjects) {
        vector[Narms]   Qcard;
        vector[Narms]   Qavoid; 
        vector[Nraffle] Qoffer;

        Qcard=Qvalue_initial;
        Qavoid=Qvalue_initial;



        for (trial in 1:Ntrials_per_subject[subject]){
         
         if(fold[subject,trial] == testfold) {

        //offer values
          Qoffer[1]=omega[subject]*Qcard[offer1[subject,trial]]+(1-omega[subject])*Qavoid[offer2[subject,trial]];;
          Qoffer[2]=omega[subject]*Qcard[offer2[subject,trial]]+(1-omega[subject])*Qavoid[offer1[subject,trial]];;

        // compute log likelihood of current trial
        log_lik[subject,trial] = categorical_logit_lpmf(selected_offer[subject, trial] | beta[subject] * Qoffer);

        //Qvalues update
        Qcard[choice[subject,trial]]    += alpha_ch [subject] * (reward[subject,trial] - Qcard[choice[subject,trial]]);
        Qavoid[unchosen[subject,trial]] += alpha_ch [subject] * (reward[subject,trial] - Qavoid[unchosen[subject,trial]]);
        }
        }
    }
  }
}
