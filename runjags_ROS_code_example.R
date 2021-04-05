#Input 'runjags' code string for Bayesian model of rate of spread (ROS), with 3 predictor weather variables (X1, X2 and X3)
#Predictor variables were standardized for modeling
#Each ROS observation has a unique 'lineid'
#Sequential ROS observations from the same fire have the same 'groupseqid'

library(runjags)



######## Code string for JAGS, giving structure of the model

model.sigmoid.code <- "model {

  # Sampling of mean predictor weather values from hourly sampled and standardized weather for each ROS observation
  for (i in 1:length(X1)) {
    X1[i] ~ dnorm(mu.X1[lineid[i]], phi.X1[lineid[i]])
    X2[i] ~ dnorm(mu.X2[lineid[i]], phi.X2[lineid[i]])
    X3[i] ~ dnorm(mu.X3[lineid[i]], phi.X3[lineid[i]])
  }


  for (i in 1:max(lineid)) {  
    mu.X1[i] ~ dnorm(0, 1)
    phi.X1[i] ~ dexp(1)
    
    mu.X2[i] ~ dnorm(0, 1)
    phi.X2[i] ~ dexp(1)
    
    mu.X3[i] ~ dnorm(0, 1)
    phi.X3[i] ~ dexp(1)
  }
  
  
  for (i in 1:max(lineid)) {
  
    # Gamma likelihood function for ROS 
    ROS[i] ~ dgamma(phi.ROS * mu.ROS[i], phi.ROS)
    
    # ROS sigmoidal function
    mu.ROS[i] <- ROS.MAX / (1 + exp(lp[i]))
    
    # linear predictor
    lp[i] <- b0 + b1*mu.X1[i]  + b2*mu.X2[i] + b3*mu.X3[i] + z[groupseqid[i]]
    
    # monitor of loglikelihood for each observation to allow WAIC calculation
    LogLik[i]<-logdensity.gamma(ROS[i],phi.ROS * mu.ROS[i], phi.ROS)
  }

  # random effect
  for (i in 1:max(groupseqid)) {
    z[i] ~ dnorm(0, z.prec)
  }
  
  
  # Priors for rate of spread regression parameters
  b0 ~ dnorm(0, 0.1)
  b1 ~ dnorm(0, 1)
  b2 ~ dnorm(0, 1)
  b3 ~ dnorm(0, 1)


  # Prior for Gamma dispersion parameter
  phi.ROS ~ dexp(1)
  
  
  # Prior for random effect standard deviation (converted to precision for JAGS)
  z.sd ~ dexp(1)
  z.prec <- pow(z.sd, -2)
}"



# A-priori maximum possible rate of spread (km/h) used for Storey et al. paper
ROS.MAX <- 15

######### Data for model

#note actual data not included, below is just the example code

# Extract the required weather variables from sampled weather data, including 'lineid'
dat.weather <- dat.weather %>% 
  select(lineid, X1 = wind_speed_stnd, X2 = rel_hum_stnd, X3=soil_mois_stnd) 

# Extract the required variables from rate of spread data
# Note - ROS values must be in are in lineid order
dat.ros <- dat.lines %>% 
  arrange(lineid) %>%
  select(lineid, ROS, groupseqid)



# Combine weather, ROS and ROS.MAX to list for input to JAGS 
model.sigmoid.data <- c(as.list(dat.weather), 
                          list(ROS = dat.ros$ROS, 
                               groupseqid = dat.ros$groupseqid, 
                               ROS.MAX = ROS.MAX))




########## Run the model via 'runjags' package

model.sigmoid <- run.jags(model.sigmoid.code,
                            monitor = c("b0", "b1","b2","b3", "phi.ROS", "z.sd", "LogLik"),
                            data = model.sigmoid.data, 
                            n.chains = 6, 
                            burnin = 1000, 
                            sample = 2000, 
                            thin = 10,
                            method = "parallel",
                            inits = function() {
                              list(
                                b0 = 0,
                                b1 = runif(1,-1,1),
                                b2 = runif(1,-1,1),
                                b3 = runif(1,-1,1),
                                phi.ROS = 1
                              )
                            })


# summary plots and statistics
plot(model.sigmoid,vars = c("b0", "b1","b2","b3", "phi.ROS"))
summary(model.sigmoid,vars = c("b0", "b1","b2","b3", "phi.ROS"))

# calculate waic
LogLik.model.sigmoid <- do.call(rbind, model.sigmoid$mcmc)
LogLik.model.sigmoid <- LogLik.model.sigmoid[,stringr::str_detect(colnames(LogLik.model.sigmoid),"LogLik")]
waic.model.sigmoid <- loo::waic(LogLik.model.sigmoid)
