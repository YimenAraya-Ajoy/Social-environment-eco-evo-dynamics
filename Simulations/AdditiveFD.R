
source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

##Simulations
s1b.x <- create.x(scenID="1b")
s1c.x <- create.x(scenID="1c", BZ=-0.1)
s1d.x <- create.x(scenID="1d", BZ= 0.1)


s1b <- sim.pops(n.sims=n.sims, x=s1b.x, theta=thetas)
s1c <- sim.pops(n.sims=n.sims, x=s1c.x, theta=thetas)
s1d <- sim.pops(n.sims=n.sims, x=s1d.x, theta=thetas)

s1b_sim_eqs<-est_sim_eqs(s1b)
s1c_sim_eqs<-est_sim_eqs(s1c)
s1d_sim_eqs<-est_sim_eqs(s1d)


s1_sim_eqs<-rbind(s1c_sim_eqs, s1b_sim_eqs, s1d_sim_eqs)
s1_sim_eqs$scen<-rep(c(1:3), each=n.sims) 

s1b_coefs<-est_coefs(s1b, formula="recruits~n + z + z2", years=c(100:n.years))
s1c_coefs<-est_coefs(s1c, formula="recruits~n + z + z2 + z_bar", years=c(100:n.years))
s1d_coefs<-est_coefs(s1d, formula="recruits~n + z + z2 + z_bar", years=c(100:n.years))

s1b_reg_eqs<-additiveFD_eq(s1b_coefs)
s1c_reg_eqs<-additiveFD_eq(s1c_coefs)
s1d_reg_eqs<-additiveFD_eq(s1d_coefs)

s1_reg_eqs<-rbind(s1c_reg_eqs, s1b_reg_eqs, s1d_reg_eqs)
s1_reg_eqs$scen<-rep(c(1:3), each=n.sims) 

s1_eq_sim_n<-tapply(exp(s1_sim_eqs$eq_n), s1_sim_eqs$scen, mean)

s1_eq_sim_z<-tapply(s1_sim_eqs$eq_z, s1_sim_eqs$scen, mean)

s1_eq_reg_n<-tapply(s1_reg_eqs$eq_n, s1_reg_eqs$scen, mean)
s1_eq_reg_z<-tapply(s1_reg_eqs$eq_z, s1_reg_eqs$scen, mean)

