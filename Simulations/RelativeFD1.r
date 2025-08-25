source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

##Simulations
s2b.x <- create.x(scenID="2b")
s2c.x <- create.x(scenID="2c", BzminZ=-0.1)
s2d.x <- create.x(scenID="2d", BzminZ= 0.1)

s2b <- sim.pops(n.sims=n.sims, x=s2b.x, theta=thetas)
s2c <- sim.pops(n.sims=n.sims, x=s2c.x, theta=thetas)
s2d <- sim.pops(n.sims=n.sims, x=s2d.x, theta=thetas)

s2b_sim_eqs<-est_sim_eqs(s2b)
s2c_sim_eqs<-est_sim_eqs(s2c)
s2d_sim_eqs<-est_sim_eqs(s2d)

s2_sim_eqs<-rbind(s2c_sim_eqs, s2b_sim_eqs, s2d_sim_eqs)
s2_sim_eqs$scen<-rep(c(1:3), each=n.sims) 

s2b_coefs<-est_coefs(s2b, formula="recruits~n + z + z2", years=c((n.years-50):n.years))
s2c_coefs<-est_coefs(s2c, formula="recruits~n + z + z2 + relz", years=c((n.years-50):n.years))
s2d_coefs<-est_coefs(s2d, formula="recruits~n + z + z2 + relz", years=c((n.years-50):n.years))

s2b_reg_eqs<-relativeFD_eq(s2b_coefs)
s2c_reg_eqs<-relativeFD_eq(s2c_coefs)
s2d_reg_eqs<-relativeFD_eq(s2d_coefs)

s2_reg_eqs<-rbind(s2c_reg_eqs, s2b_reg_eqs, s2d_reg_eqs)
s2_reg_eqs$scen<-rep(c(1:3), each=n.sims) 

s2_eq_sim_n<-tapply(exp(s2_sim_eqs$eq_n), s2_sim_eqs$scen, mean)
s2_eq_sim_z<-tapply(s2_sim_eqs$eq_z, s2_sim_eqs$scen, mean)

s2_eq_reg_n<-tapply(s2_reg_eqs$eq_n, s2_reg_eqs$scen, mean)
s2_eq_reg_z<-tapply(s2_reg_eqs$eq_z, s2_reg_eqs$scen, mean)
