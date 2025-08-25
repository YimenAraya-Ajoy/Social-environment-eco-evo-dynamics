source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

s7b.x <- create.x(scenID="7b")
s7c.x <- create.x(scenID="7c",  BZn=-0.022)
s7d.x <- create.x(scenID="7d",  BZn= 0.022)

s7b <- sim.pops(n.sims=n.sims, x=s5b.x, theta=thetas)
s7c <- sim.pops(n.sims=n.sims, x=s5c.x, theta=thetas)
s7d <- sim.pops(n.sims=n.sims, x=s5d.x, theta=thetas)

s7b_sim_eqs<-est_sim_eqs(s7b)
s7c_sim_eqs<-est_sim_eqs(s7c)
s7d_sim_eqs<-est_sim_eqs(s7d)

s7_sim_eqs<-rbind(s7c_sim_eqs, s7b_sim_eqs, s7d_sim_eqs)
s7_sim_eqs$scen<-rep(c(1:3), each=n.sims) 

s7_eq_sim_n<-tapply(exp(s7_sim_eqs$eq_n), s7_sim_eqs$scen, mean)
s7_eq_sim_z<-tapply(s7_sim_eqs$eq_z, s7_sim_eqs$scen, mean)


s7b_coefs<-est_coefs(s7b, formula="recruits~n + z + z2", years=c((n.years-50):n.years))
s7c_coefs<-est_coefs(sc, formula="recruits~n + z + z2 + n:z", years=c((n.years-50):n.years))
s5d_coefs<-est_coefs(s5d, formula="recruits~n + z + z2 + n:z", years=c((n.years-50):n.years))

s7b_reg_eqs<-FDDensityR_eq(s7b_coefs)
s7c_reg_eqs<-FDDensityR_eq(s7c_coefs)
s7d_reg_eqs<-FDDensityR_eq(s7d_coefs)

s7_reg_eqs<-rbind(s7c_reg_eqs, s7b_reg_eqs, s7d_reg_eqs)
s7_reg_eqs$scen<-rep(c(1:3), each=n.sims) 

s7_eq_reg_n<-tapply(s7_reg_eqs$eq_n, s7_reg_eqs$scen, median)
s_eq_reg_z<-tapply(s7_reg_eqs$eq_z, s7_reg_eqs$scen, median)



