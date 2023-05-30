
source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

##Simulations
n.sims = 100
n.years = 200

#s1a.x <- create.x(scenID="1a", n.years=n.years, r=0.5, Bnr=-0.3, Bzr= 0.1, Bzr2=-0.01)
s6b.x <- create.x(scenID="6b",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZnr =  0.00)
s6c.x <- create.x(scenID="6c",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZnr =  0.00, BZznr = -0.0005)
s6d.x <- create.x(scenID="6d",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZnr =  0.00, BZznr = 0.0003)

#s1a <- sim.pops(n.sims=n.sims, x=s1a.x)  
s6b <- sim.pops(n.sims=n.sims, x=s6b.x)
s6c <- sim.pops(n.sims=n.sims, x=s6c.x)
s6d <- sim.pops(n.sims=n.sims, x=s6d.x)

#est_all_res_s1a<-est_all(s1a, "w~n + z + z2")
est_all_res_s6b<-est_all(s6b, "w~n + z + z2")
est_all_res_s6c<-est_all(s6c, "w~n + z + z2 + n:z_bar:z")
est_all_res_s6d<-est_all(s6d, "w~n + z + z2 + n:z_bar:z")

save.image("Results/FrequencyDensityDependentSelection.RData")



