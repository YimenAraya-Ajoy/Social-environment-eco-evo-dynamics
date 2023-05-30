
source("/home/yi/Dropbox/SocialFitnessEffects/load.r")

##Simulations
n.sims = 100
n.years = 200

#s1a.x <- create.x(scenID="1a", n.years=n.years, r=0.5, Bnr=-0.3, Bzr= 0.1, Bzr2=-0.01)
s5b.x <- create.x(scenID="5b",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01)
s5c.x <- create.x(scenID="5c",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZnr = -0.005)
s5d.x <- create.x(scenID="5d",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZnr =  0.0025)

#s1a <- sim.pops(n.sims=n.sims, x=s1a.x)  
s5b <- sim.pops(n.sims=n.sims, x=s5b.x)
s5c <- sim.pops(n.sims=n.sims, x=s5c.x)
s5d <- sim.pops(n.sims=n.sims, x=s5d.x)

#est_all_res_s1a<-est_all(s1a, "w~n + z + z2")
est_all_res_s5b<-est_all(s5b, "w~n + z + z2")
est_all_res_s5c<-est_all(s5c, "w~n + z + z2 + n:z_bar")
est_all_res_s5d<-est_all(s5d, "w~n + z + z2 + n:z_bar")

save.image("FrequencyDensityDependence.RData")




