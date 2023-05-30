source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")
load("Results/FrequencyDependence.RData")
E_z<-seq(0,20,0.01)
Bz_bar<-seq(est_all_res_s3c$est_m_res$Bz_bar-0.005, est_all_res_s3d$est_m_res$Bz_bar+0.001, by=0.0003)
n<-seq(0,6,by=0.01)
est_Eq_n_res<-matrix(NA,length(E_z), length(Bz_bar))
est_v_res<-matrix(NA,length(n), length(Bz_bar))
E_z2<-rep(NA, length(Bz_bar))
E_n2<-rep(NA, length(Bz_bar))

Bz_bar<-seq(est_all_res_s3c$est_m_res$Bz_bar-0.005, est_all_res_s3d$est_m_res$Bz_bar+0.001, by=0.0003)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bz_bar))


for(i in 1:length(Bz_bar)){
  E_n=-(est_all_res_s3b$est_m_res$B0 + (est_all_res_s3b$est_m_res$Bz + Bz_bar[i])*E_z  
        + est_all_res_s3b$est_m_res$Bq*(E_z^2 + 3))/(est_all_res_s3b$est_m_res$Bn + est_all_res_s3b$est_m_res$Bnz*E_z
                                                       + est_all_res_s3b$est_m_res$Bnz_bar*E_z) 
  est_Eq_n_res[,i]<-E_n
  E_n2[i]<-max(E_n)
  E_z2[i]<-E_z[max(E_n)==E_n]
  }
 

for(i in 1:length(Bz_bar)){
  v<-est_all_res_s3b$est_m_res$B0 + (est_all_res_s3b$est_m_res$Bz + Bz_bar[i])*E_z2[i]  + est_all_res_s3b$est_m_res$Bq*(E_z2[i]^2 + 1.5) + (est_all_res_s3b$est_m_res$Bn + est_all_res_s3b$est_m_res$Bnz*E_z2[i] + est_all_res_s3b$est_m_res$Bnz_bar*E_z2[i])*n
  est_v_res[,i]<-v 
}
par(mar=c(4,4,4,2))

plot(NA, col="gray", ylab="Equilibrium n", xlab="Mean phenotype", cex=0.5, ylim=c(0,600), xlim=c(4,16), main="Frequency dependence")
for(i in 1:length(Bz_bar)){
  points(exp(est_Eq_n_res[,i])~E_z, col=colors[i], type="l")
}

points(exp(E_n2)~E_z2, pch=8, col="darkgray", cex=0.4)

#plot_pred1(est_all_res_s1a, col="gray", lty=2)
plot_pred1(est_all_res_s3b, "white")
plot_pred1(est_all_res_s3c,  col=colors[which.min(abs(Bz_bar - est_all_res_s3c$est_m_res$Bz_bar))])
plot_pred1(est_all_res_s3d, col=colors[which.min(abs(Bz_bar - est_all_res_s3d$est_m_res$Bz_bar))])


plot(NA, ylab="Mean log fitness", xlab="Log population size", ylim=c(0,0.8), xlim=c(2,6))
for(i in 1:length(Bz_bar)){
  points(est_v_res[,i]~n, col=colors[i], type="l")
}

#plot_rn(est_all_res_s1a,  col="gray", lty=2)
plot_rn(est_all_res_s3b,  col="white", lty=1)
plot_rn(est_all_res_s3c,  col="black", lty=1)
plot_rn(est_all_res_s3d,  col="black", lty=1)

par(mar=c(4,3,2,1))
Bz_bar<-seq(est_all_res_s3c$est_m_res$Bz_bar-0.005, est_all_res_s3d$est_m_res$Bz_bar+0.001, by=0.0001)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bz_bar))

plot(NA, ylim=c(min(Bz_bar),max(Bz_bar)), xlim=c(0,1), xaxt="n", xlab="", yaxt="n", axes="F", ylab="", main=expression(beta[bar(z)]))
segments(0, Bz_bar, 1, Bz_bar, col=colors)
axis(2, round(seq(min(Bz_bar), max(Bz_bar),0.004),3), round(seq(min(Bz_bar), max(Bz_bar),0.004),3), las=2)
axis(2, 0, 0, las=2)
abline(h=0,col="white", lwd=2)
abline(h=Bz_bar[which.min(abs(Bz_bar - est_all_res_s3c$est_m_res$Bz_bar))],lwd=2)
abline(h=Bz_bar[which.min(abs(Bz_bar - est_all_res_s3d$est_m_res$Bz_bar))],lwd=2)
