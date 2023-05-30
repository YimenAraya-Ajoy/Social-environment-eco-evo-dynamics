source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

##Simulations
n.sims =50
n.years = 200

s1a.x <- create.x(scenID="1a", n.years=n.years,  Bnr=-0.35, Bzr= 0.17, Bzr2=-0.01)
s1b.x <- create.x(scenID="1b",  n.years=n.years, Bnr=-0.35, Bzr= 0.20, Bzr2=-0.01)
s1c.x <- create.x(scenID="1c",  n.years=n.years, Bnr=-0.35, Bzr= 0.22, Bzr2=-0.01)

s1a <- sim.pops(n.sims=n.sims, x=s1a.x)  
s1b <- sim.pops(n.sims=n.sims, x=s1b.x)
s1c <- sim.pops(n.sims=n.sims, x=s1c.x)

est_all_res_s1a<-est_all(s1a, "w~n + z + z2")
est_all_res_s1b<-est_all(s1b, "w~n + z + z2")
est_all_res_s1c<-est_all(s1c, "w~n + z + z2")
est_all_res_s1d<-est_all(s1d, "w~n + z + z2")


save.image("Results/BaseScenario.RData")
