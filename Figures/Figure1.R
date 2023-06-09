load("Results/Selection.RData")
source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")
pdf("Figures/Figure_1.pdf", height= 5, width=5)

par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(NA, ylab="Mean log fitness", xlab="Log population size", ylim=c(-0.5,1.2), xlim=c(0,7))
#plot_rn(est_all_res_s1a,  col="gray", lty=2)
plot_rn(est_all_res_s1b,  col="darkgreen", lty=1)
plot_rn(est_all_res_s1d,  col="orange", lty=1)
plot_rn(est_all_res_s1c,  col="blue", lty=1)
abline(h=0, lty=2)
mtext("A", 3, adj=0)

plot(NA, ylab="Fitness", xlab="Phenotype", ylim=c(0,4), xlim=c(0,25))
plot_fit_func(est_all_res_s1b, col="darkgreen")
plot_fit_func(est_all_res_s1d, col="orange")
plot_fit_func(est_all_res_s1c, col="blue")
mtext("B", 3, adj=0)


plot(NA, ylim=c(0,700), xlim=c(0,200), ylab="Population size", xlab="Time steps")
#plot_n(s1a, "black")  
plot_n(s1b, "darkgreen")  
plot_n(s1d, "orange")  
plot_n(s1c, "blue")  
mtext("C", 3, adj=0)

plot(NA, ylim=c(0,13), xlim=c(0,200), ylab="Mean phenotype", xlab="Time steps")
#plot_z(s1a, "black")  
plot_z(s1b, "darkgreen")  
plot_z(s1d, "orange")  
plot_z(s1c, "blue")  
mtext("D", 3, adj=0)

dev.off()
