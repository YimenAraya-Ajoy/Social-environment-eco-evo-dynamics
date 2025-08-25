
## Additive FD
s1b_traj<-est_trajectories(s1b)
s1c_traj<-est_trajectories(s1c)
s1d_traj<-est_trajectories(s1d)

s2b_traj<-est_trajectories(s2b)
s2c_traj<-est_trajectories(s2c)
s2d_traj<-est_trajectories(s2d)

s3b_traj<-est_trajectories(s3b)
s3c_traj<-est_trajectories(s3c)
s3d_traj<-est_trajectories(s3d)


s4b_traj<-est_trajectories(s4b)
s4c_traj<-est_trajectories(s4c)
s4d_traj<-est_trajectories(s4d)



pdf("Figures/Trajectories.pdf", width= 7, height=4)


par(mfcol=c(2,4))
par(mar=c(1.5, 3.5, 2.5, 0))
plot(NA, ylim=c(50, 500), xlim=c(0,n.years),ylab="", xlab="", main="")
#mtext("Time step", side = 1, line = 2.1, cex=0.8)
mtext("Pop size", side = 2, line = 2.1, cex=0.8)
mtext("A) Additive FD", side = 3, cex=0.8, line = 0, adj = 0)

plot_n(s1b_traj, 0, "black")  
plot_n(s1c_traj, 0, "orange")  
plot_n(s1d_traj, 0, "blue")  

points(rep(n.years,3), s1_eq_reg_n, col=cols, pch=8, cex=2)
abline(v=50, lty=2, col="red") 

par(mar=c(3.5, 3.5, 1, 0))
plot(NA, ylim=c(-1, 4), xlim=c(0,n.years), ylab="", xlab="")
mtext("Time step", side = 1, line = 2.1, cex=0.8)
mtext("Mean phenotype", side = 2, line = 2.1, cex=0.8)
plot_z(s1b_traj, -39, "black")  
plot_z(s1c_traj, -39, "orange")  
plot_z(s1d_traj, -39, "blue")  
abline(v=50, lty=2, col="red") 

points(rep(n.years,3), s1_eq_reg_z, col=cols, pch=8, cex=2)

## Relative FD 1

par(mar=c(1.5, 3.5, 2.5, 0))
plot(NA, ylim=c(50, 350), xlim=c(0,n.years),ylab="", xlab="", main="")
#mtext("Time step", side = 1, line = 2.1, cex=0.8)
#mtext("Pop size", side = 2, line = 2.1, cex=0.8)
mtext("B) Relative FD 1", side = 3, cex=0.8, line = 0, adj = 0)

plot_n(s2b_traj, 0, "black")  
plot_n(s2c_traj, 0, "orange")  
plot_n(s2d_traj, 0, "blue")  
abline(v=50, lty=2, col="red") 

points(rep(n.years,3), s2_eq_reg_n, col=cols, pch=8, cex=2)

par(mar=c(3.5, 3.5, 1, 0))
plot(NA, ylim=c(-1, 4), xlim=c(0,n.years), ylab="", xlab="")
mtext("Time step", side = 1, line = 2.1, cex=0.8)
#mtext("Mean phenotype", side = 2, line = 2.1, cex=0.8)

plot_z(s2b_traj, -39, "black")  
plot_z(s2c_traj, -39, "orange")  
plot_z(s2d_traj, -39, "blue")  
abline(v=50, lty=2, col="red") 

points(rep(n.years,3), s2_eq_reg_z, col=cols, pch=8, cex=2)


## Relative FD 2
par(mar=c(1.5, 3.5, 2.5, 0))
plot(NA, ylim=c(50, 350), xlim=c(0,n.years),ylab="", xlab="", main="")
#mtext("Time step", side = 1, line = 2.1, cex=0.8)
#mtext("Pop size", side = 2, line = 2.1, cex=0.8)
mtext("C) Relative FD 2", side = 3, cex=0.8, line = 0, adj = 0)

plot_n(s3b_traj, 0, "black")  
plot_n(s3c_traj, 0, "orange")  
plot_n(s3d_traj, 0, "blue")  
abline(v=50, lty=2, col="red") 

points(rep(n.years,3), s3_eq_reg_n, col=cols, pch=8, cex=2)

par(mar=c(3.5, 3.5, 1, 0))
plot(NA, ylim=c(-1,4), xlim=c(0,n.years), ylab="", xlab="")
mtext("Time step", side = 1, line = 2.1, cex=0.8)
#mtext("Mean phenotype", side = 2, line = 2.1, cex=0.8)

plot_z(s3b_traj, -39, "black")  
plot_z(s3c_traj, -39, "orange")  
plot_z(s3d_traj, -39, "blue")  
abline(v=50, lty=2, col="red") 

points(rep(n.years,3), s3_eq_reg_z, col=cols, pch=8, cex=2)

## Multiplicative
par(mar=c(1.5, 3.5, 2.5, 0))
plot(NA, ylim=c(50, 500), xlim=c(0,n.years),ylab="", xlab="", main="")
#mtext("Time stepy", side = 1, line = 2.1, cex=0.8)
#mtext("Pop size", side = 2, line = 2.1, cex=0.8)
mtext("D) Multiplicative FD", side = 3, cex=0.8, line = 0, adj = 0)

plot_n(s4b_traj, 0, "black")  
plot_n(s4c_traj, 0, "orange")  
plot_n(s4d_traj, 0, "blue")  
abline(v=50, lty=2, col="red") 

points(rep(n.years,3), s4_eq_reg_n, col=cols, pch=8, cex=2)

par(mar=c(3.5, 3.5, 1, 0))
plot(NA, ylim=c(-1,4), xlim=c(0,n.years), ylab="", xlab="")
mtext("Time step", side = 1, line = 2.1, cex=0.8)
#mtext("Mean phenotype", side = 2, line = 2.1, cex=0.8)

plot_z(s4b_traj, -39, "black")  
plot_z(s4c_traj, -39, "orange")  
plot_z(s4d_traj, -39, "blue")  
abline(v=50, lty=2, col="red") 

points(rep(n.years,3), s4_eq_reg_z, col=cols, pch=8, cex=2)

dev.off()

file.copy(from="Figures/Trajectories.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures/Trajectories.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
