
source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

##Simulations
n.sims = 100
n.years = 300

#s1a.x <- create.x(scenID="1a", n.years=n.years, r=0.5, Bnr=-0.3, Bzr= 0.1, Bzr2=-0.01)
s2b.x <- create.x(scenID="2b",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01)
s2c.x <- create.x(scenID="2c",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, Bznr = -0.005)
s2d.x <- create.x(scenID="2d",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, Bznr =  0.003)

#s1a <- sim.pops(n.sims=n.sims, x=s1a.x)  
s2b <- sim.pops(n.sims=n.sims, x=s2b.x)
s2c <- sim.pops(n.sims=n.sims, x=s2c.x)
s2d <- sim.pops(n.sims=n.sims, x=s2d.x)

#est_all_res_s1a<-est_all(s1a, "w~n + z + z2")
est_all_res_s2b<-est_all(s2b, "w~n + z + z2")
est_all_res_s2c<-est_all(s2c, "w~n + z + z2 + n:z")
est_all_res_s2d<-est_all(s2d, "w~n + z + z2 + n:z")

save.image("Results/DensityDependentSelection.RData")



