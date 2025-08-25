# this function combines functions Nbar() and zbar()
# accepts as arguments the output of sim.pops for three scenarios (a, b,c) and calculates the avg yearly population size and phenotype accross all simulations in each scenario
calc.simAvg <- function(a){
  rbind(a$Pop) %>%                # combine data from all scenarios
    group_by(year) %>%                    # for each year in each scenario, calculate... 
    summarize(Nbar = mean(N, na.rm=TRUE), 
              zbar = mean(z_bar, na.rm=TRUE))     # mean phenotype
}


# FunctionExtinction
# a function to calculate extinction (replaces Ext())
calc.ext <- function(a, b, c){
  rbind(a$Pop, b$Pop, c$Pop) %>%
    group_by(scenID, sim) %>% 
    summarize(max.year=max(year), .group='keep') %>%
    mutate(prop.ext=sum(max.year<100)/max(sim)) %>%
    group_by(scenID) %>% summarize(ext=mean(prop.ext), )
}


##Analyze the data
coefs<-function(y, formula, years){
  x<-subset(y, year %in% years) 
  m_z<-mean(x$z)
  m_n<-mean(x$n)
#  x$z=x$z-m_z
 # x$z_bar=x$z_bar-m_z
  x$z2<-x$z^2
  x$relz<-x$z-x$z_bar
  x$relz2<-(x$z-x$z_bar)^2
  #x$n<-x$n-m_n
  m<-glm(formula, data=x, family="poisson")
  coefs<-data.frame(B0=coef(m)[1], "n"=0, "z"=0, "z2"=0, "z_bar"=0, "n:z_bar"=0, "n:z"=0, "z:z_bar"=0, "n:z:z_bar"=0, "relz"=0, "relz2"=0)
  nam<-gsub(":", ".", names(coef(m)[2:length(coef(m))]))
  coefs[match(nam, colnames(coefs))]<-coef(m)[2:length(coef(m))]
  colnames(coefs)<-c("B0", "Bn", "Bz", "Bq", "Bz_bar", "Bnz_bar", "Bnz", "Bz_barz", "Bnz_barz", "BzminZ", "BzminZ2")
  coefs$m_z<-m_z
  coefs$m_n<-m_n
  coefs
}


coefs2<-function(y, formula, years){
  x<-subset(y, year %in% years) 
  m_z<-mean(x$z)
  m_n<-mean(x$n)
  x$z=x$z-m_z
  x$z_bar=x$z_bar-m_z
  x$z2<-x$z^2
  x$relz<-x$z-x$z_bar
  x$relz2<-(x$z-x$z_bar)^2
  x$n<-x$n-m_n
  m<-glmer(formula, data=x, family="poisson")
  coefs<-data.frame(B0=fixef(m)[1], "n"=0, "z"=0, "z2"=0, "z_bar"=0, "n:z_bar"=0, "n:z"=0, "z:z_bar"=0, "n:z:z_bar"=0, "relz"=0, "relz2"=0)
  nam<-gsub(":", ".", names(fixef(m)[2:length(fixef(m))]))
  coefs[match(nam, colnames(coefs))]<-fixef(m)[2:length(fixef(m))]
  colnames(coefs)<-c("B0", "Bn", "Bz", "Bq", "Bz_bar", "Bnz_bar", "Bnz", "Bz_barz", "Bnz_barz", "BzminZ", "BzminZ2")
  coefs$m_z<-m_z
  coefs$m_n<-m_n
  coefs
}


est_coefs<-function(x, formula, years){
  est_res<-do.call(rbind.data.frame,mclapply(x, coefs, formula, years, mc.cores=detectCores()-3))
  est_res
  }

est_coefs2<-function(x, formula, years){
  est_res<-do.call(rbind.data.frame,mclapply(x, coefs2, formula, years, mc.cores=detectCores()-3))
  est_res
  }


additiveFD_eq<-function(x){
eq_z  <-   x$Bz/(-2*x$Bq) 
eq_n  <-  exp((x$B0 -log(0.5) + (x$Bz + x$Bz_bar)*eq_z + x$Bq * (eq_z^2 + 1))/-x$Bn)
eq<-data.frame(eq_z=eq_z, eq_n=eq_n)
}

relativeFD_eq<-function(x){
eq_z  <-   (x$Bz + x$BzminZ)/(-2*x$Bq)
eq_n  <-  exp((x$B0 -log(0.5) + (x$Bz + x$Bz_bar)*eq_z + x$Bq * (eq_z^2 + 1) + x$BzminZ2)/-x$Bn)
eq<-data.frame(eq_z=eq_z, eq_n)
}

relativeFD2_eq<-function(x){
eq_z  <-   x$Bz/(-2*x$Bq)
eq_n  <-  exp((x$B0 -log(0.5) + (x$Bz + x$Bz_bar)*eq_z + x$Bq * (eq_z^2 + 1) + x$BzminZ2)/-x$Bn)
eq<-data.frame(eq_z=eq_z, eq_n)
}


MultiplicativeFD_eq<-function(x){
eq_z  <-   x$Bz/-(2*x$Bq + x$Bz_barz)
eq_n  <-  exp((x$B0 -log(0.5) + (x$Bz + x$Bz_bar)*eq_z + x$Bq * (eq_z^2 + 1) + x$Bz_barz*eq_z^2)/-x$Bn)
eq<-data.frame(eq_z=eq_z, eq_n)
}

FDDensityR_eq<-function(x){
eq_z  <-   x$Bz/-(2*x$Bq)
eq_n  <-  exp((x$B0 -log(0.5) + (x$Bz + x$Bz_bar)*eq_z + x$Bq * (eq_z^2 + 1) + x$Bz_barz*eq_z^2)/-x$Bn + x$Bnz_bar*eq_z)
eq<-data.frame(eq_z=eq_z, eq_n)
}


DDS_eq<-function(x){
eq_n  <-  ((-2 * sqrt((x$B0-log(0.5)) * x$Bq * x$Bnz^2 - x$Bz * x$Bq * x$Bn * x$Bnz + x$Bq^2 * x$Bn^2 + x$Bq^2 * x$Bnz^2 * 1) - x$Bz * x$Bnz + 2 * x$Bq * x$Bn) / x$Bnz^2)
eq_z  <-   (x$Bz+x$Bnz*eq_n)/-(2*x$Bq) 
eq<-data.frame(eq_z=eq_z, eq_n=exp(eq_n))
}


sim_eqs<-function(x){
  eq_z=mean(x$z[x$year>(max(x$year)-40)]) 
  eq_n=mean(x$n[x$year>(max(x$year)-40)])
  eq_sz=var(x$z[x$year>(max(x$year)-40)])
#  eq_sa=var(x$a[x$year>(max(x$year)-20)])
  
  eq<-data.frame(eq_z, eq_n, eq_sz)
                }

est_sim_eqs<-function(x){
  est_eqs<-do.call(rbind.data.frame,mclapply(x, sim_eqs, mc.cores=detectCores()-3))
  est_eqs
  }


trajectories<-function(x){
traj<-x[match(1:n.years, x$year),c("year", "z_bar", "N", "scenID", "simID")]
rownames(traj)<-1:nrow(traj)
traj
}

est_trajectories<-function(x){
  est_trajectories<-do.call(rbind.data.frame,mclapply(x, trajectories, mc.cores=detectCores()-3))
  est_trajectories
}


est_bias<-function(ests,  params, samp, sim_eqs, theta=2){
bias<-data.frame(ScenID=rep(params$scenID, nrow(ests)), Years=rep(samp, nrow(ests)), Scenario=rep(substring(params$scenID, 1, 1), nrow(ests)))
bias$B0 = ests$B0-(params$r_max-(params$gamma/2)*(sim_eqs$eq_z-theta)^2)
bias$Bn = ests$Bn-params$Bn
bias$Bz = ests$Bz-(params$gamma*theta)
bias$Bq<-ests$Bq--params$gamma/2
bias$BZ<-ests$Bz_bar-params$BZ
bias$Bnz_bar<-ests$Bnz_bar-s2c.x$BZn
bias$Bnz<-ests$Bnz-params$Bzn
bias$Bz_barz<-ests$Bz_barz-params$BZz
bias$Bnz_barz<-ests$Bnz_barz-params$BZzn
bias$BzminZ<-ests$BzminZ-params$BzminZ
bias$BzminZ2<-ests$BzminZ2-params$BzminZ2
bias$n<-log(ests$eq_n)-sim_eqs$eq_n
bias$z<-log(ests$eq_z)-log(sim_eqs$eq_z)
bias
}

