require(reshape2)
source("load.r")
source("load_results.r")

formula<-rep(c("recruits ~n + z + z2 + z_bar + (1|year) + (1|ID)", 
               "recruits ~n + z + z2 + relz + (1|year) + (1|ID) ", 
               "recruits ~n + z + z2 + relz2 + (1|year) + (1|ID)", 
               "recruits ~n + z + z2 + z:z_bar + (1|year) + (1|ID)"), each=2) 


formula<-rep(c("recruits ~n + z + z2 + z_bar", 
               "recruits ~n + z + z2 + relz ", 
               "recruits ~n + z + z2 + relz2", 
               "recruits ~n + z + z2 + z:z_bar"), each=2) 


samp=c(10, 20, 40, 80)
scen_data <- list(s1c, s1d, s2c, s2d, s3c, s3d, s4c, s4d)
scen_params <- list(s1c.x, s1d.x, s2c.x, s2d.x, s3c.x, s3d.x, s4c.x, s4d.x)
scen_sim_eqs<-list(s1c_sim_eqs, s1d_sim_eqs, s2c_sim_eqs, s2d_sim_eqs, s3c_sim_eqs, s3d_sim_eqs, s4c_sim_eqs, s4d_sim_eqs)

bias_res<-ests_res<-list()

counter <- 0
for( i in 1:length(formula)){
for(j in 1:length(samp)){
ests<-est_coefs(scen_data[[i]], formula=formula[i], years=c((n.years-samp[j]):n.years))

if (substring(scen_params[[i]]$scenID, 1, 1) == "1") {
 reg_eqs <- additiveFD_eq(ests)
} else if (substring(scen_params[[i]]$scenID, 1, 1) == "2") {
  reg_eqs <- relativeFD_eq(ests)
} else if((substring(scen_params[[i]]$scenID, 1, 1) == "3")) {
 reg_eqs <- relativeFD2_eq(ests)
} else if((substring(scen_params[[i]]$scenID, 1, 1) == "4")) {
 reg_eqs <- MultiplicativeFD_eq(ests)
}

ests$eq_z<- reg_eqs[,1]
ests$eq_n<- reg_eqs[,2]

bias_ests<-est_bias(ests, scen_params[[i]], samp[j], scen_sim_eqs[[i]])
counter=counter+1
ests_res[[counter]]<-ests
bias_res[[counter]]<-bias_ests
}
}

save.image("Results/Bias2.RData")


