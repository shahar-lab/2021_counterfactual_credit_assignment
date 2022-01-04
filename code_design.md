project
├─┬ empirical data 
│ └── preprocessing
│ └── regression
│ └── data
│      └── df.csv
│      └── df.rdata
|
├─┬ empirical data replication
│ └── preprocessing
│ └── regression
│ └── data
│      └── df.csv
│      └── df.rdata
|
├─┬ model_model_name (e.g., model_single learning rate)
│ └── simulate_parameters.r
│ └── simulate_data_based_on_empirical_parameters.r
│ └── simulate_data_based_on_artificial_parameters.r
│ └── modelfit_compile.r
│ └── modelfit_mcmc.r
│ └── examine_regression.r
│ └── examine_mcmc.r
│ └── examine_parameter_recovery.r
│ └── files
│      └── model.stan
│      └── model_loo.stan
│      └── model.r
|
│ └── data
│      └── simulated_parameters.rdata
│      └── simulated_data_based_on_empirical_parameters.rdata
│      └── simulated_data_based_on_artificial_parameters.rdata
│      └── simulated_data_based_on_artificial_parameters_standata.rdata
│      └── modelfit_compiled_model.rdata
│      └── modelfit.rdata
│      └── modelfit_loo-block.rdata





