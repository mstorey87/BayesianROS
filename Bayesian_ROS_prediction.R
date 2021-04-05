### This script can be used to produce Bayesian model rate of spread (ROS) predictions based on the main model in Storey et al.
### The script requires two data files. 
### These are provided in the same repository on GitHub (user needs to define their location on their local machine)
### The user can change the input values of windspeed, relative humidity and soilmoisture to produce ROS predictive distributions,
### which allow a user to explore model behaviour.
### Further information is included in Appendix B of Storey et al.
### User must define folder location of data downloaded 
library(ggplot2)



### The output folder where table from running JAGS (provided as file on GitHub)
dat.fold <- "D:\\model_data\\"

# load in the JAGS output (table of posterior distribution samples).
# The table contains sampled of regression slopes (b0,b1,b2,b3) for each predictor
# phi.ROS, which is the phi parameter from the gamma likelihood distribution, and z.sd and z for the random effect term
dat.post <- read.csv(paste0(dat.fold,"post.4.csv"))

## load in mean, sd of predictor variables from original data.
## this is required to transform variables- i.e. as predictor values were standardized for model fitting
model.data.summary.4<- read.csv(paste0(dat.fold,"dat.sum.4.csv"))




###User defined values of predictor values for ROS prediction
#wind speed in km h-1
windspeed <- 35

#relative humidity in %
relativehumidity <- 10

#soil moisture in kg m-2
soilmoisture <- 3
  
  
#standardize the user defined values for sampling of posterior distribution

ws_stnd <- (windspeed - model.data.summary.4$X1_mean_orgnl) /  model.data.summary.4$X1_sd_orgnl

rh_stnd <- (relativehumidity - model.data.summary.4$X2_mean_orgnl) /  model.data.summary.4$X2_sd_orgnl

soilmois_stnd <- (soilmoisture - model.data.summary.4$X3_mean_orgnl) /  model.data.summary.4$X3_sd_orgnl




### define the function to predict mean ROS. Function as used in Storey et al.

#ROS MAX was defined in sigmoid equation in the analysis was 15 km/h. Do not change
ROS.MAX <- 15

#sigmoid prediction function
fn_predict <- function(x1,x2,x3, b0, b1,b2,b3,z) { 
  lp <- b0 + b1 * x1 + b2 * x2+ b3 * x3+z
  ROS.MAX / (1 + exp(lp))
}



#Run prediction function. Creates a mean  predicted ROS ("trend") for each row in dat.post
dat.post$trend <- fn_predict(ws_stnd,rh_stnd,soilmois_stnd,dat.post$b0,dat.post$b1,dat.post$b2,dat.post$b3,dat.post$z)

#Create predictive distribution, i.e. sample from rgamma for each row in dat.post
#rgamma rate = phi from dat.post, shape = phi*mean predicted ROS from dat.post and prediction function respectively
dat.post$ROS_prediction <- rgamma(nrow(dat.post),dat.post$phi.ROS*dat.post$trend,dat.post$phi.ROS) 


### Plot predictive distribution
bold.text <- element_text(face = "bold", color = "black",size = 15)

#base plot
plot.1 <- ggplot(data=dat.post) +
  geom_density(aes(x = ROS_prediction),fill="grey")+
  theme_bw()+ 
  theme(axis.title = bold.text,axis.text = bold.text)

plot.1

#save plot data for later plots
plot.1.data <- ggplot_build(plot.1)$data[[1]]

### Calculate % of predictive distribution above, below of between selected ROS values

#ROS is above x value. Note may be error is none of predictive distribution is > selected ROS_value
ROS_value <- 3
selection1 <- sum(dat.post$ROS_prediction > ROS_value)/length(dat.post$ROS_prediction)*100
lab.1 <- paste0(round(selection1,1)," % of the predictive distribution is > ", ROS_value, " km/h")
print(lab.1)

plot.1+  
  geom_area(data = subset(plot.1.data, x > ROS_value), aes(x=x, y=y), fill="red") +
  geom_vline(xintercept = ROS_value,size=1.2)+
  xlab(expression(bold(paste("Rate of Spread (km"~h^-1,")"))))+
  scale_x_continuous(breaks = seq(0,20,2),limits = c(0,10))+
  geom_text(x=8, y=max(plot.1.data$y)/2, label=paste0(round(selection1,1)," %"),size=8)+ 
  ggtitle(lab.1)+
  theme(axis.title = bold.text,axis.text = bold.text)


#ROS is below x. Note may be error is none of predictive distribution is < selected ROS_value
ROS_value <- 1
selection2 <- sum(dat.post$ROS_prediction < ROS_value)/length(dat.post$ROS_prediction)*100
lab.2 <- paste0(round(selection2,1)," % of the predictive distribution is < ", ROS_value, " km/h")
print(lab.2)

plot.1+  
  geom_area(data = subset(plot.1.data, x < ROS_value), aes(x=x, y=y), fill="red") +
  geom_vline(xintercept = ROS_value,size=1.2)+
  xlab(expression(bold(paste("Rate of Spread (km"~h^-1,")"))))+
  scale_x_continuous(breaks = seq(0,20,2),limits = c(0,10))+
  geom_text(x=8, y=max(plot.1.data$y)/2, label=paste0(round(selection2,1)," %"),size=8)+ 
  ggtitle(lab.2)+
  theme(axis.title = bold.text,axis.text = bold.text)





#ROS is between x and x. Note may be error is none of predictive distribution is between selected ROS_values
ROS_value_lower <- 1
ROS_value_upper <- 3
selection3 <- sum(dat.post$ROS_prediction > ROS_value_lower & dat.post$ROS_prediction < ROS_value_upper)/length(dat.post$ROS_prediction)*100
lab.3 <- paste0(round(selection3,1)," % of the predictive distribution is > ", ROS_value_lower, " km/h & < ", ROS_value_upper, " km/h")

plot.1+  
  geom_area(data = subset(plot.1.data, x > ROS_value_lower & x < ROS_value_upper), aes(x=x, y=y), fill="red") +
  geom_vline(xintercept = ROS_value_lower,size=1.2)+
  geom_vline(xintercept = ROS_value_upper,size=1.2)+
  xlab(expression(bold(paste("Rate of Spread (km"~h^-1,")"))))+
  scale_x_continuous(breaks = seq(0,20,2),limits = c(0,10))+
  geom_text(x=8, y=max(plot.1.data$y)/2, label=paste0(round(selection3,1)," %"),size=8)+ 
  ggtitle(lab.3)+
  theme(axis.title = bold.text,axis.text = bold.text)




