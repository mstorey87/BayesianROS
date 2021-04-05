# BayesianROS

This repository contains R code to run predictions from the Bayesian rate of spread model resported in the journal article draft journal article _Derivation of a Bayesian fire spread model using large-scale wildfire observations_. The article is currently under review with a link to be provided here if it is accepted for publication.
In the repository is "Bayesian_ROS_prediction.R", which is an R code that can be be used to run predictions from the model. A new run JAGS (which was used for model fitting) is not required to make a new predictions, as the table of posterior distribution samples ("post.4.csv") was already produced and uploaded here by the authors. The script also requires the file "dat.sum.4.csv", as this contains data required for standardization of predictor variables. Further details on running predictions are included in Appendix B of the manuscript and in comments in "Bayesian_ROS_prediction.R".
Also included in the repository is an example code showing the model structure and code string used to fit the Bayesian ROS model using the R runjags package and JAGS ("runjags_ROS_code_example.R"). This is an example code only and does not included the data required, which is restricted due to licence conditions.
