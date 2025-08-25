source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

s4b.x <- create.x(scenID="4b")
s4c.x <- create.x(scenID="4c", BZz=-0.04)
s4d.x <- create.x(scenID="4d", BZz= 0.04)

s4b <- sim.pops(n.sims=n.sims, x=s4b.x, theta=thetas)
s4c <- sim.pops(n.sims=n.sims, x=s4c.x, theta=thetas)
s4d <- sim.pops(n.sims=n.sims, x=s4d.x, theta=thetas)


s4b_sim_eqs<-est_sim_eqs(s4b)
s4c_sim_eqs<-est_sim_eqs(s4c)
s4d_sim_eqs<-est_sim_eqs(s4d)

s4_sim_eqs<-rbind(s4c_sim_eqs, s4b_sim_eqs, s4d_sim_eqs)
s4_sim_eqs$scen<-rep(c(1:3), each=n.sims) 

s4_eq_sim_n<-tapply(exp(s4_sim_eqs$eq_n), s4_sim_eqs$scen, mean)
s4_eq_sim_z<-tapply(s4_sim_eqs$eq_z, s4_sim_eqs$scen, mean)


s4b_coefs<-est_coefs(s4b, formula="recruits~n + z + z2", years=c((n.years-50):n.years))
s4c_coefs<-est_coefs(s4c, formula="recruits~n + z + z2 + z:z_bar", years=c((n.years-50):n.years))
s4d_coefs<-est_coefs(s4d, formula="recruits~n + z + z2 + z:z_bar", years=c((n.years-50):n.years))

s4b_reg_eqs<-MultiplicativeFD_eq(s4b_coefs)
s4c_reg_eqs<-MultiplicativeFD_eq(s4c_coefs)
s4d_reg_eqs<-MultiplicativeFD_eq(s4d_coefs)

s4_reg_eqs<-rbind(s4c_reg_eqs, s4b_reg_eqs, s4d_reg_eqs)
s4_reg_eqs$scen<-rep(c(1:3), each=n.sims) 

s4_eq_reg_n<-tapply(s4_reg_eqs$eq_n, s4_reg_eqs$scen, median)
s4_eq_reg_z<-tapply(s4_reg_eqs$eq_z, s4_reg_eqs$scen, median)

