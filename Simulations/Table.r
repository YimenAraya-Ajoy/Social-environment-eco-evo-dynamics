source("load.r")

function.extract<-function(sx, est_all_res){
  pars<-sx[,c("Bnr", "Bzr", "Bzr2", "Bznr", "BZr", "BZzr", "BZnr", "BZznr")]
  dev_N<- est_all_res$est_Eq_res$E_n-est_all_res$est_Eq_res$Eq_n   
  dev_z<- est_all_res$est_Eq_res$E_z-est_all_res$est_Eq_res$Eq_z   
  Bias_N<-paste(format(round(mean(dev_N),1),nsmall = 1), " (", format(round(sd(dev_N),1),nsmall = 1), ")", sep="")
  Bias_z<-paste(format(round(mean(dev_z),1),nsmall = 1), " (",format(round(sd(dev_N),1),nsmall = 1), ")", sep="")
  pars$Bias_N<-Bias_N
  pars$Bias_z<-Bias_z
  pars
  }

load("Results/BaseScenario.RData")
sa.x <-s1a.x 
sb.x <-s2b.x
sc.x <- s3c.x

est_all_res_sa<-est_all_res_s1a
est_all_res_sb<-est_all_res_s2b
est_all_res_sc<-est_all_res_s3c

load("Results/DensityRegulation.RData")
load("Results/DensityDependentSelection.RData")
load("Results/FrequencyDependence.RData")
load("Results/FrequencyDependentSelection.RData")
load("Results/FrequencyDensityDependence.RData")
load("Results/FrequencyDensityDependentSelection.RData")



est_all_res_list<-list(
                       est_all_res_s1b,
                       est_all_res_sa, est_all_res_sc,
                       est_all_res_s1c,est_all_res_s1d, 
                       est_all_res_s2c,est_all_res_s2d,
                       est_all_res_s3c,est_all_res_s3d,
                       est_all_res_s4c,est_all_res_s4d,
                       est_all_res_s5c,est_all_res_s5d,
                       est_all_res_s6c,est_all_res_s6d
                       )

sx_list<-list(s1b.x, 
              sa.x, sc.x,
              s1c.x, s1d.x,
              s2c.x, s2d.x,
              s3c.x, s3d.x,
              s4c.x, s4d.x,
              s5c.x, s5d.x,
              s6c.x, s6d.x
              )

res<-list()
for(i in 1:15){
res[[i]]<-function.extract(sx_list[[i]], est_all_res_list[[i]])
}

res

table_2<-do.call(rbind.data.frame, res)
Scenarios<-c("Base scenario", "Selection", "Selection", "Density regulation", "Density regulation", 
             "Density-dep. selection", "Density-dep. selection", 
             "Frequency dependence", "Frequency dependence", 
             "Frequency-dep. selection","Frequency-dep. selection",
             "Frequency-density-dep.",  "Frequency-density-dep.",
             "Freq.-dens.-dep. selec.", "Freq.-dens.-dep. selec.")

table<-cbind(Scenarios, table_2)


res

table_2a<-do.call(rbind.data.frame, res)

table_2<-table_2a[, 1:8]

table2<-cbind(Scenarios, table_2)
names(table)


parnames<-c('\\textbf{Scenario}', '$\\bm{b_{n}}$', '$\\bm{b_{z}}$', '$\\bm{b_{q}}$', '$\\bm{b_{zn}}$', '$\\bm{b_{\\bar{z}}}$', '$\\bm{b_{z \\bar{z}}}$',  '\\bm{$b_{n \\bar{z}}}$', '$\\bm{b_{z \\bar{z}n}}$') 

colnames(table2)<-c(parnames)

print(xtable(table, type = "latex", digits=c(0,0,2,3,2,3,3,4,1,1), caption = "Values in the individual based simulations. $b$ values are the coefficients used in the IBS, determining
             the effects on recurit production of the social environment and an individual's phenotype. 
             This are analogous to the $\\beta$ parameters that describe the effects on individual fitness wich 
             are presented in the main text. Individual recruit production when population sizes were very small was set to 1.1 
             and the average survival propbability was set to 0.475 for all the simulations.", 
             floating=FALSE), file="/home/yi/Dropbox/SocialFitnessEffects/Table2.tex",include.rownames=FALSE,  
           sanitize.text.function = identity, sanitize.colnames.function=function(x){paste0("\\multicolumn{1}{l}{",x,"}")})

print(xtable(table2, type = "latex", digits=c(0,0,2,2,2,3,2,3,3,4), caption = "Values in the individual based simulations. $b$ values are the coefficients used in the IBS, determining
             the effects on recurit production of the social environment and an individual's phenotype. 
             This are analogous to the $\\beta$ parameters that describe the effects on individual fitness wich 
             are presented in the main text. Individual recruit production when population sizes were very small was set to 1.1 
             and the average survival propbability was set to 0.475 for all the simulations.", 
             floating=FALSE), file="/home/yi/Dropbox/SocialFitnessEffects/Table2.2.tex",include.rownames=FALSE,  
      sanitize.text.function = identity, sanitize.colnames.function=function(x){paste0("\\multicolumn{1}{l}{",x,"}")})



