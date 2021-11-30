data {

  //General fixed parameters for the experiment/models
  int<lower = 1> Nsubjects;                                         //number of subjects
  int<lower = 1> Nblocks;           
  int<lower = 1> Ntrials;                                           //maximum number of trials per subject (without missing data). Used to form the subject x trials matricies. 
  int<lower = 1> Ntrials_per_subject[Nsubjects];                    //number of trials left for each subject after data omission
  int<lower = 2> Narms;                                             //number of overall alternatives
  int<lower = 2> Nraffle;                                           //number of offers per trial



  //Behavioral data:
  int<lower = 0> choice[Nsubjects,Ntrials];              //index of which arm was pulled coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];              //outcome of bandit arm pull
  int<lower = 0> offer1[Nsubjects,Ntrials];              //outcome of bandit arm pull
  int<lower = 0> offer2[Nsubjects,Ntrials];              //outcome of bandit arm pull
  int<lower = 0> selected_offer[Nsubjects,Ntrials];      //outcome of bandit arm pull
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials];           //coding whether a trial is the first in a block to allow for Qval rest
}

transformed data{
  int<lower = 1> Nparameters=2; //number of parameters
  vector[Narms] Qvalue_initial;     // initial values for Qvalues (defined here to aviod doing this many times across iterations)
  Qvalue_initial = rep_vector(0.5, Narms);
}

parameters {
  //population level parameters 
  vector         [Nparameters] population_locations;       //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] population_scales;          //vector of random effects variance for each model parameter
  
  //individuals level
  vector[Nsubjects] alpha_random_effect;
  vector[Nsubjects] beta_random_effect;
}


transformed parameters {
  vector<lower=0, upper=1>[Nsubjects] alpha;
  vector                  [Nsubjects] beta;
  
  for (subject in 1:Nsubjects) {
    alpha[subject]   = inv_logit(population_locations[1]  + population_scales[1] * alpha_random_effect[subject]);
    beta[subject]    =          (population_locations[2]  + population_scales[2] * beta_random_effect [subject]) ; #isn't it a new outlook on exploration? shouldn't we stick to the standards for now and exponent it?
  }

}


model {
  
  // population level  
  population_locations  ~ normal(0, 2);            
  population_scales     ~ cauchy(0,2);        

  // indvidual level  
  alpha_random_effect ~ std_normal();
  beta_random_effect  ~ std_normal();
 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector[Narms] Qcard; 
    vector[Nraffle] Qoffer; 
    
 
      for (trial in 1:Ntrials_per_subject[subject]){
        if (first_trial_in_block[subject,trial] == 1) {
                        Qcard=Qvalue_initial;
        }

          Qoffer[1]=Qcard[offer1[subject,trial]];
          Qoffer[2]=Qcard[offer2[subject,trial]];

        //like function
         selected_offer[subject, trial] ~ categorical_logit(beta[subject] * Qoffer);

        //Qvalues update
        Qcard[choice[subject,trial]] += alpha[subject] * (reward[subject,trial] - Qcard[choice[subject,trial]]);

      } 
  }
}
