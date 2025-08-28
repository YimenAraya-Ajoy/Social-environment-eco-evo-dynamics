source("load.r")

s3b.x <- create.x(scenID="3b")
s3c.x <- create.x(scenID="3c",  BzminZ2=-0.04)
s3d.x <- create.x(scenID="3d",  BzminZ2= 0.04)

s3b <- sim.pops(n.sims=n.sims, x=s3b.x, theta=thetas)
s3c <- sim.pops(n.sims=n.sims, x=s3c.x, theta=thetas)
s3d <- sim.pops(n.sims=n.sims, x=s3d.x, theta=thetas)

s3b_sim_eqs<-est_sim_eqs(s3b)
s3c_sim_eqs<-est_sim_eqs(s3c)
s3d_sim_eqs<-est_sim_eqs(s3d)

s3_sim_eqs<-rbind(s3c_sim_eqs, s3b_sim_eqs, s3d_sim_eqs)
s3_sim_eqs$scen<-rep(c(1:3), each=n.sims) 

s3_eq_sim_n<-tapply(exp(s3_sim_eqs$eq_n), s3_sim_eqs$scen, mean)
s3_eq_sim_z<-tapply(s3_sim_eqs$eq_z, s3_sim_eqs$scen, mean)



s3b_coefs<-est_coefs(s3b, formula="recruits~n + z + z2", years=c((n.years-40):n.years))

s3c_coefs<-est_coefs(s3c, formula="recruits~n + z + z2 + relz2", years=c((n.years-40):n.years))
s3d_coefs<-est_coefs(s3d, formula="recruits~n + z + z2 + relz2", years=c((n.years-40):n.years))

s3b_reg_eqs<-relativeFD2_eq(s3b_coefs)
s3c_reg_eqs<-relativeFD2_eq(s3c_coefs)
s3d_reg_eqs<-relativeFD2_eq(s3d_coefs)

s3_reg_eqs<-rbind(s3c_reg_eqs, s3b_reg_eqs, s3d_reg_eqs)
s3_reg_eqs$scen<-rep(c(1:3), each=n.sims) 

s3_eq_reg_n<-tapply(s3_reg_eqs$eq_n, s3_reg_eqs$scen, mean)
s3_eq_reg_z<-tapply(s3_reg_eqs$eq_z, s3_reg_eqs$scen, mean)
