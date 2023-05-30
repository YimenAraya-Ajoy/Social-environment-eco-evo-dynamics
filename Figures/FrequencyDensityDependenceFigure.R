
source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")
load("Results/FrequencyDensityDependence.RData")

E_z<-seq(0,20,0.01)
Bnz_bar<-seq(est_all_res_s5c$est_m_res$Bnz_bar-0.001, est_all_res_s5d$est_m_res$Bnz_bar+0.0001, by=0.0001)
n<-seq(0,6,by=0.01)
est_Eq_n_res<-matrix(NA,length(E_z), length(Bnz_bar))
est_Eq_nz_res<-matrix(NA,length(E_z), length(Bnz_bar))
est_v_res<-matrix(NA,length(n), length(Bnz_bar))
E_z2<-rep(NA, length(Bnz_bar))
E_n2<-rep(NA, length(Bnz_bar))

colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz_bar))


for(i in 1:length(Bnz_bar)){
  E_n=-(est_all_res_s5b$est_m_res$B0 + (est_all_res_s5b$est_m_res$Bz + est_all_res_s5b$est_m_res$Bz_bar)*E_z  
        + est_all_res_s5b$est_m_res$Bq*(E_z^2 + 3))/(est_all_res_s5b$est_m_res$Bn + est_all_res_s5b$est_m_res$Bnz*E_z
                                                       + Bnz_bar[i]*E_z) 
  est_Eq_n_res[,i]<-E_n
  est_Eq_nz_res[,i]<-E_n*E_z
  
  E_n2[i]<-max(E_n)
  E_z2[i]<-E_z[max(E_n)==E_n]
  }
 

for(i in 1:length(Bnz_bar)){
  v<-est_all_res_s5b$est_m_res$B0 + (est_all_res_s5b$est_m_res$Bz + est_all_res_s5b$est_m_res$Bz_bar)* est_all_res_s5b$est_m_res$Eq_z  + est_all_res_s5b$est_m_res$Bq*(est_all_res_s5b$est_m_res$Eq_z^2 + 3) + (est_all_res_s5b$est_m_res$Bn + est_all_res_s5b$est_m_res$Bnz*est_all_res_s5b$est_m_res$Eq_z + Bnz_bar[i]*est_all_res_s5b$est_m_res$Eq_z)*n
  est_v_res[,i]<-v 
}
par(mar=c(4,4,4,2))

plot(NA, col="gray", ylab="Equilibrium n", xlab="Mean phenotype", cex=0.5, ylim=c(0,600), xlim=c(5,15), main="Frequency-dependent denstiy regulation")
for(i in 1:length(Bnz_bar)){
  points(exp(est_Eq_n_res[,i])~E_z, col=colors[i], type="l")
}

points(exp(E_n2)~E_z2, pch=8, col="darkgray", cex=0.4)

#plot_pred1(est_all_res_s1a, col="gray", lty=2)
plot_pred1(est_all_res_s5b, "white")
plot_pred1(est_all_res_s5c, colors[which.min(abs(Bnz_bar - est_all_res_s5c$est_m_res$Bnz_bar))])
plot_pred1(est_all_res_s5d, colors[which.min(abs(Bnz_bar - est_all_res_s5d$est_m_res$Bnz_bar))])


plot(NA, ylab="Mean log fitness", xlab="Log population size", ylim=c(0,0.8), xlim=c(2,6))
for(i in 1:length(Bnz_bar)){
  points(est_v_res[,i]~n, col=colors[i], type="l")
}

#plot_rn(est_all_res_s1a,  col="gray", lty=2)
plot_rn(est_all_res_s5b,  col="white", lty=1)
plot_rn(est_all_res_s5c,  col="black", lty=1)
plot_rn(est_all_res_s5d,  col="black", lty=1)

par(mar=c(4,3,2,1))

Bnz_bar<-seq(est_all_res_s5c$est_m_res$Bnz_bar-0.001, est_all_res_s5d$est_m_res$Bnz_bar+0.0001, by=0.00001)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz_bar))

plot(NA, ylim=c(min(Bnz_bar),max(Bnz_bar)), xlim=c(0,1), xaxt="n", xlab="", yaxt="n", axes="F", ylab="", main=expression(beta[paste(n,bar(z))]))
segments(0, Bnz_bar, 1, Bnz_bar, col=colors)
axis(2, round(seq(min(Bnz_bar), max(Bnz_bar),0.001),3),  round(seq(min(Bnz_bar), max(Bnz_bar),0.001),3), las=2)
axis(2, 0, 0, las=2)
abline(h=0, lwd=1, col="white")
abline(h=Bnz_bar[which.min(abs(Bnz_bar - est_all_res_s5c$est_m_res$Bnz_bar))],lwd=1)
abline(h=Bnz_bar[which.min(abs(Bnz_bar - est_all_res_s5d$est_m_res$Bnz_bar))],lwd=1)

