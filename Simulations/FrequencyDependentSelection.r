source("/home/yi/Dropbox/SocialFitnessEffects/load.r")

##Simulations
n.sims = 100
n.years = 200

#s1a.x <- create.x(scenID="1a", n.years=n.years, r=0.5, Bnr=-0.3, Bzr= 0.1, Bzr2=-0.01)
s4b.x <- create.x(scenID="4b",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01)
s4c.x <- create.x(scenID="4c",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZzr = -0.005)
s4d.x <- create.x(scenID="4d",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZzr =  0.002)

#s1a <- sim.pops(n.sims=n.sims, x=s1a.x)  
s4b <- sim.pops(n.sims=n.sims, x=s4b.x)
s4c <- sim.pops(n.sims=n.sims, x=s4c.x)
s4d <- sim.pops(n.sims=n.sims, x=s4d.x)

#est_all_res_s1a<-est_all(s1a, "w~n + z + z2")
est_all_res_s4b<-est_all(s4b, "w~n + z + z2")
est_all_res_s4c<-est_all(s4c, "w~n + z + z2 + z:z_bar")
est_all_res_s4d<-est_all(s4d, "w~n + z + z2 + z:z_bar")

save.image("FrequencyDependentSelection.RData")


