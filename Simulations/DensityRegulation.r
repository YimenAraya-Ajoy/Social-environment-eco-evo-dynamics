source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

##Simulations
n.sims = 100
n.years = 200

#s1a.x <- create.x(scenID="1a", n.years=n.years, r=0.5, Bnr=-0.3, Bzr= 0.1, Bzr2=-0.01)
s1b.x <- create.x(scenID="1b",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01)
s1c.x <- create.x(scenID="1c",  n.years=n.years, r=0.3, Bnr=-0.32, Bzr= 0.2, Bzr2=-0.01)
s1d.x <- create.x(scenID="1d",  n.years=n.years, r=0.3, Bnr=-0.40, Bzr= 0.2, Bzr2=-0.01)

#s1a <- sim.pops(n.sims=n.sims, x=s1a.x)  
s1b <- sim.pops(n.sims=n.sims, x=s1b.x)
s1c <- sim.pops(n.sims=n.sims, x=s1c.x)
s1d <- sim.pops(n.sims=n.sims, x=s1d.x)

#est_all_res_s1a<-est_all(s1a, "w~n + z + z2")
est_all_res_s1b<-est_all(s1b, "w~n + z + z2")
est_all_res_s1c<-est_all(s1c, "w~n + z + z2")
est_all_res_s1d<-est_all(s1d, "w~n + z + z2")


save.image("Results/DensityRegulation.RData")

