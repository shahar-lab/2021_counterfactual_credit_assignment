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
  int<lower = 0> action[Nsubjects,Ntrials];        //index of which arm was pulled coded 1 to 4
  int<lower = 0> unchosen[Nsubjects,Ntrials];     //index of which arm was not selected coded 1 to 4
  int<lower = 0> reward[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> offer1[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> offer2[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> selected_offer[Nsubjects,Ntrials];            //outcome of bandit arm pull
  int<lower = 0> first_trial_in_block[Nsubjects,Ntrials];           //coding whether a trial is the first in a block to allow for Qval rest
  int<lower = 0> fold[Nsubjects,Ntrials];           //using my block since block seems to be reserved in stan
  real           testfold;                           //which block should be used as test

}

transformed data{
  int<lower = 1> Nparameters=3; //number of parameters
  vector[Narms] Qvalue_initial;     // initial values for Qvalues (defined here to aviod doing this many times across iterations)
  Qvalue_initial = rep_vector(0.5, Narms);
}

parameters {
// Declare parameters vectors. the notation "aux" indicate that the values are before transformation
  //population level parameters 
  vector[Nparameters] mu_aux;                    //vector with the population level mean for each model parameter
  vector<lower=0>[Nparameters] sigma_aux;          //vector of random effects variance for each model parameter
  
//individuals level
  vector[Nsubjects] alpha_individal_aux;
  vector[Nsubjects] w_individal_aux;
  vector[Nsubjects] beta_indvidial_aux;
}


transformed parameters {
//declare variables and parameters
  vector<lower=0, upper=1>[Nsubjects]  alpha;
  vector<lower=0, upper=1>[Nsubjects]  w;
  vector<lower=0, upper=10>[Nsubjects] beta;
    
  for (subject in 1:Nsubjects) {
    alpha[subject]   = Phi_approx(mu_aux[1]  + sigma_aux[1]  * alpha_individal_aux[subject]);
    w[subject]       = Phi_approx(mu_aux[2]  + sigma_aux[2]  * w_individal_aux[subject])*0.5+0.5;
    beta[subject]    = Phi_approx(mu_aux[3]  + sigma_aux[3]  * beta_indvidial_aux[subject]) * 10;
  }

}



model {
  
  // population level priors (hyper-parameters)
  mu_aux  ~ normal(0, 1);            
  sigma_aux ~ normal(0, 0.5);        

  // indvidual level priors (subjects' parameters)
  alpha_individal_aux~ normal(0, 1);
  w_individal_aux    ~ normal(0, 1);
  beta_indvidial_aux ~ normal(0, 1);
 

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial

  for (subject in 1:Nsubjects){
    vector[Narms] Qcard; 
    vector[Nraffle] Qoffer;
    real PE;
    

      for (trial in 1:Ntrials_per_subject[subject]){
          if(fold[subject,trial] != testfold) {

          if (first_trial_in_block[subject,trial] == 1) {
                        Qcard=Qvalue_initial;
        }
        
          Qoffer[1]=Qcard[offer1[subject,trial]];
          Qoffer[2]=Qcard[offer2[subject,trial]];

        //liklihood function 
        
         target +=log_softmax(beta[subject] * Qoffer)[selected_offer[subject, trial]];
            
        //Qvalues update
        PE = (reward[subject,trial] - Qcard[action[subject,trial]]);
        Qcard[action[subject,trial]] += alpha[subject] * w[subject] * PE;
        Qcard[unchosen[subject,trial]] += alpha[subject] * (1-w[subject]) * -PE;

      } 
      }
  }
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
}

generated quantities {
  //define parameters that will be saved in the final output
  real<lower=0, upper=1>  mu_alpha;
  real<lower=0, upper=1>  mu_w;
  real<lower=0, upper=10> mu_beta;
  
  vector<lower=0, upper=1>[Nsubjects]  ind_alpha;
  vector<lower=0, upper=1>[Nsubjects]  ind_w;
  vector<lower=0, upper=10>[Nsubjects] ind_beta;
  
  real log_lik[Nsubjects];
  
  mu_alpha  =Phi_approx(mu_aux[1]);
  mu_w      =Phi_approx(mu_aux[2])*0.5+0.5;
  mu_beta   =Phi_approx(mu_aux[3])*10;
  
  for (subject in 1:Nsubjects) {
    ind_alpha[subject]   = Phi_approx(mu_aux[1]  + sigma_aux[1]  * alpha_individal_aux[subject]);
    ind_w[subject]       = Phi_approx(mu_aux[2]  + sigma_aux[2]  * w_individal_aux[subject])*0.5+0.5;
    ind_beta[subject]    = Phi_approx(mu_aux[3]  + sigma_aux[3]  * beta_indvidial_aux[subject]) * 10;
  }

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial (placed in generetaed quantities block to save time and memory)

  { //
    for (subject in 1:Nsubjects) {
        vector[Narms] Qcard;
        vector[Nraffle] Qoffer;
        real PE;



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
        PE = (reward[subject,trial] - Qcard[action[subject,trial]]);
        Qcard[action[subject,trial]] += ind_alpha[subject] * w[subject] * PE;
        Qcard[unchosen[subject,trial]] += ind_alpha[subject] * (1-w[subject]) * -PE;
        }
        }
    }
  }
}

