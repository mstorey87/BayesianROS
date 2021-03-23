# BayesianROS

This repository contains R code to run Bayesian Rate of Spread predictions as described in the journal article _Derivation of a Bayesian fire spread model using large-scale wildfire observations_
The main code is "Bayesian_ROS_prediction.R", which is an R code that samples posterior distribution produced by JAGS. JAGS is not required to run predictions, as the posterior distibution was already produced by the authors. The posterior distribution samples are contained in the file "post.4.csv". The script also requires the file "dat.sum.4.csv", as this contains data required for standardization of predictor variables.
