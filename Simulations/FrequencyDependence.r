source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

##Simulations
n.sims = 200
n.years = 200

#s1a.x <- create.x(scenID="1a", n.years=n.years, r=0.5, Bnr=-0.3, Bzr= 0.1, Bzr2=-0.01)
s3b.x <- create.x(scenID="3b",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01)
s3c.x <- create.x(scenID="3c",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZr = -0.02)
s3d.x <- create.x(scenID="3d",  n.years=n.years, r=0.3, Bnr=-0.35, Bzr= 0.2, Bzr2=-0.01, BZr =  0.02)

#s1a <- sim.pops(n.sims=n.sims, x=s1a.x)  
s3b <- sim.pops(n.sims=n.sims, x=s3b.x)
s3c <- sim.pops(n.sims=n.sims, x=s3c.x)
s3d <- sim.pops(n.sims=n.sims, x=s3d.x)

#est_all_res_s1a<-est_all(s1a, "w~n + z + z2")
est_all_res_s3b<-est_all(s3b, "w~n + z + z2")
est_all_res_s3c<-est_all(s3c, "w~n + z + z2 + z_bar")
est_all_res_s3d<-est_all(s3d, "w~n + z + z2 + z_bar")

save.image("FrequencyDependence.RData")
