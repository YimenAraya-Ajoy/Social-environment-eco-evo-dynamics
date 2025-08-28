source("load.r")

s6b.x <- create.x(scenID="4b",  n.years=n.years)
s6c.x <- create.x(scenID="4c",  n.years=n.years, BZzn=-0.0075)
s6d.x <- create.x(scenID="4d",  n.years=n.years, BZzn= 0.0075)

s6b <- sim.pops(n.sims=n.sims, x=s6b.x, theta=thetas)
s6c <- sim.pops(n.sims=n.sims, x=s6c.x, theta=thetas)
s6d <- sim.pops(n.sims=n.sims, x=s6d.x, theta=thetas)

s6b_sim_eqs<-est_sim_eqs(s6b)
s6c_sim_eqs<-est_sim_eqs(s6c)
s6d_sim_eqs<-est_sim_eqs(s6d)

s6_sim_eqs<-rbind(s6c_sim_eqs, s6b_sim_eqs, s6d_sim_eqs)
s6_sim_eqs$scen<-rep(c(1:3), each=n.sims) 

s6_eq_sim_n<-tapply(exp(s6_sim_eqs$eq_n), s6_sim_eqs$scen, mean)
s6_eq_sim_z<-tapply(s6_sim_eqs$eq_z, s6_sim_eqs$scen, mean)


#s6b_coefs<-est_coefs(s6b, formula="recruits~n + z + z2", years=c((n.years-50):n.years))
#s6c_coefs<-est_coefs(s6c, formula="recruits~n + z + z2 + n:z:z_bar", years=c((n.years-50):n.years))
#s6d_coefs<-est_coefs(s6d, formula="recruits~n + z + z2 + n:z:z_bar", years=c((n.years-50):n.years))

#s6b_reg_eqs<-MultiplicativeFD_eq(s6b_coefs)
#s6c_reg_eqs<-MultiplicativeFD_eq(s6c_coefs)
#s6d_reg_eqs<-MultiplicativeFD_eq(s6d_coefs)

#s6_reg_eqs<-rbind(s6c_reg_eqs, s6b_reg_eqs, s6d_reg_eqs)
#s6_reg_eqs$scen<-rep(c(1:3), each=n.sims) 

#s6_eq_reg_n<-tapply(s6_reg_eqs$eq_n, s6_reg_eqs$scen, median)
#s6_eq_reg_z<-tapply(s6_reg_eqs$eq_z, s6_reg_eqs$scen, median)
