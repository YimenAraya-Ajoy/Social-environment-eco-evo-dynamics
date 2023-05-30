
source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")
load("Results/DensityDependentSelection.RData")

E_z<-seq(0,20,0.01)
Bnz<-seq(est_all_res_s2c$est_m_res$Bnz-0.001, est_all_res_s2d$est_m_res$Bnz+0.0005, by=0.00005)
n<-seq(0,7,by=0.01)
est_Eq_n_res<-matrix(NA,length(E_z), length(Bnz))
est_v_res<-matrix(NA,length(n), length(Bnz))
E_z2<-rep(NA, length(Bnz))
E_n2<-rep(NA, length(Bnz))

colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz))


for(i in 1:length(Bnz)){
  E_n=-(est_all_res_s2b$est_m_res$B0 + (est_all_res_s2b$est_m_res$Bz + est_all_res_s2b$est_m_res$Bz_bar)*E_z  
        + est_all_res_s2b$est_m_res$Bq*(E_z^2 + 3))/(est_all_res_s2b$est_m_res$Bn + Bnz[i]*E_z
                                                       + est_all_res_s2b$est_m_res$Bnz_bar*E_z) 
  est_Eq_n_res[,i]<-E_n
  E_n2[i]<-max(E_n)
  E_z2[i]<-E_z[max(E_n)==E_n]
  }
 

for(i in 1:length(Bnz)){
  v<-est_all_res_s2b$est_m_res$B0 + (est_all_res_s2b$est_m_res$Bz + est_all_res_s2b$est_m_res$Bz_bar)*E_z2[i]  + est_all_res_s2b$est_m_res$Bq*(E_z2[i]^2 + 3) + (est_all_res_s2b$est_m_res$Bn + Bnz[i]*E_z2[i] + est_all_res_s2b$est_m_res$Bnz_bar*E_z2[i])*n
  est_v_res[,i]<-v 
}
par(mar=c(4,4,4,2))

plot(NA, col="gray", ylab="Equilibrium n", xlab="Mean phenotype", cex=0.5, ylim=c(0,600), xlim=c(4,16), main="Density-dependent selection")
for(i in 1:length(Bnz)){
  points(exp(est_Eq_n_res[,i])~E_z, col=colors[i], type="l")
}

points(exp(E_n2)~E_z2, pch=8, col="darkgray", cex=0.4)

#plot_pred1(est_all_res_s1a, col="gray", lty=2)
plot_pred1(est_all_res_s2b, "white")
plot_pred1(est_all_res_s2c, colors[which.min(abs(Bnz - est_all_res_s2c$est_m_res$Bnz))])
plot_pred1(est_all_res_s2d, colors[which.min(abs(Bnz - est_all_res_s2d$est_m_res$Bnz))])

plot(NA, ylab="Mean log fitness", xlab="Log population size", ylim=c(0,0.8), xlim=c(2,6))
for(i in 1:length(Bnz)){
  points(est_v_res[,i]~n, col=colors[i], type="l")
}

#plot_rn(est_all_res_s1a,  col="gray", lty=2)
plot_rn(est_all_res_s2b,  "white", lty=1)
plot_rn(est_all_res_s2c,  col="black", lty=1)
plot_rn(est_all_res_s2d,  col="black", lty=1)

par(mar=c(4,3,2,1))
Bnz<-seq(est_all_res_s2c$est_m_res$Bnz-0.001, est_all_res_s2d$est_m_res$Bnz+0.0005, by=0.00001)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz))
plot(NA, ylim=c(min(Bnz),max(Bnz)), xlim=c(0,1), xaxt="n", xlab="", yaxt="n", axes="F", ylab="", main=expression(beta[zn]))
segments(0, Bnz, 1, Bnz, col=colors)
axis(2, round(seq(min(Bnz), max(Bnz),0.001),3), round(seq(min(Bnz), max(Bnz),0.001),3), las=2)
axis(2, 0, 0, las=2)
abline(h=0,col="white", lwd=2)
abline(h=Bnz[which.min(abs(Bnz - est_all_res_s2c$est_m_res$Bnz))],lwd=2)
abline(h=Bnz[which.min(abs(Bnz - est_all_res_s2d$est_m_res$Bnz))],lwd=2)

