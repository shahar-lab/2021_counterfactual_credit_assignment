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
  vector[Nsubjects] alpha_unch_random_effect;

}


transformed parameters {
//declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects]  alpha_ch;
  vector                  [Nsubjects]  beta;
  vector<lower=0, upper=1>[Nsubjects]  alpha_unch;

    
  for (subject in 1:Nsubjects) {
    alpha_ch[subject]   = inv_logit(population_locations[1]  + population_scales[1]  * alpha_ch_random_effect[subject]);
    beta[subject]       =          (population_locations[2]  + population_scales[2]  * beta_random_effect[subject]);
    alpha_unch[subject] = inv_logit(population_locations[3]  + population_scales[3]  * alpha_unch_random_effect[subject]);
  }

}



model {

  // population level  
  population_locations  ~ normal(0, 2);            
  population_scales     ~ cauchy(0,2);        

  // indvidual level  
  alpha_ch_random_effect    ~ std_normal();
  beta_random_effect        ~ std_normal();
  alpha_unch_random_effect  ~ std_normal();


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector[Narms] Qcard; 
    vector[Nraffle] Qoffer; 
    real PE_chosen;
    real PE_unchosen;


    
      for (trial in 1:Ntrials_per_subject[subject]){
          if(fold[subject,trial] == testfold) {
          if (first_trial_in_block[subject,trial] == 1) {
                        Qcard=Qvalue_initial;
        }
        
          Qoffer[1]=Qcard[offer1[subject,trial]];
          Qoffer[2]=Qcard[offer2[subject,trial]];

        //liklihood function 
         selected_offer[subject, trial] ~ categorical_logit(beta[subject] * Qoffer);
            
        //Qvalues update
        PE_chosen   = (reward[subject,trial] - Qcard[choice[subject,trial]]);
        PE_unchosen = ((1-reward[subject,trial]) - Qcard[choice[subject,trial]]);

        Qcard[choice[subject,trial]] += alpha_ch[subject] * PE_chosen;
        Qcard[unchosen[subject,trial]] += alpha_unch[subject] * PE_unchosen;

      }
      }
  }
}


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



generated quantities {
  //define parameters that will be saved in the final output
  real<lower=0, upper=1>  ind_alpha_ch[Nsubjects];
  real<lower=0, upper=1>  ind_alpha_unch[Nsubjects];
  real                    ind_beta[Nsubjects];
  real log_lik[Nsubjects];
  
    for (subject in 1:Nsubjects) {
    ind_alpha_ch[subject]   = inv_logit(population_locations[1]  + population_scales[1]  * alpha_ch_random_effect[subject]);
    ind_beta[subject]       =          (population_locations[2]  + population_scales[2]  * beta_random_effect[subject]);
    ind_alpha_unch[subject] = inv_logit(population_locations[3]  + population_scales[3]  * alpha_unch_random_effect[subject]);

  }

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial (placed in generetaed quantities block to save time and memory)

  { //
    for (subject in 1:Nsubjects) {
        vector[Narms] Qcard;
        vector[Nraffle] Qoffer;
        real PE_chosen;
        real PE_unchosen;


        Qcard=Qvalue_initial;

        log_lik[subject] = 0;

        for (trial in 1:Ntrials_per_subject[subject]){
         
         if(fold[subject,trial] == testfold) {

        //offer values
          Qoffer[1]=Qcard[offer1[subject,trial]];
          Qoffer[2]=Qcard[offer2[subject,trial]];

        // compute log likelihood of current trial
        log_lik[subject] += categorical_logit_lpmf(selected_offer[subject, trial] | ind_beta[subject] * Qoffer);

        //Qvalues update
        PE_chosen   = (reward[subject,trial] - Qcard[choice[subject,trial]]);
        PE_unchosen = ((1-reward[subject,trial]) - Qcard[choice[subject,trial]]);

        Qcard[choice[subject,trial]] += alpha_ch[subject] * PE_chosen;
        Qcard[unchosen[subject,trial]] += alpha_unch[subject] * PE_unchosen;
        }
        }
    }
  }
}
