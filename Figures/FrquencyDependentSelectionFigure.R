source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")
load("Results/FrequencyDependentSelection.RData")

E_z<-seq(0,20,0.01)
Bz_barz<-seq(est_all_res_s4c$est_m_res$Bz_barz-0.0001, est_all_res_s4d$est_m_res$Bz_barz+0.0001, by=0.00005)
n<-seq(0,6,by=0.01)
est_Eq_n_res<-matrix(NA,length(E_z), length(Bz_barz))
est_v_res<-matrix(NA,length(n), length(Bz_barz))
E_z2<-rep(NA, length(Bz_barz))
E_n2<-rep(NA, length(Bz_barz))

colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bz_barz))


for(i in 1:length(Bz_barz)){
  E_n=-(est_all_res_s4b$est_m_res$B0 + (est_all_res_s4b$est_m_res$Bz + est_all_res_s4b$est_m_res$Bz_bar)*E_z  + Bz_barz[i]*E_z^2
        + est_all_res_s4b$est_m_res$Bq*(E_z^2 + 3))/(est_all_res_s4b$est_m_res$Bn + est_all_res_s4b$est_m_res$Bnz*E_z
                                                       + est_all_res_s4b$est_m_res$Bnz_bar*E_z) 
  est_Eq_n_res[,i]<-E_n
  E_n2[i]<-max(E_n)
  E_z2[i]<-E_z[max(E_n)==E_n]
  }
 

for(i in 1:length(Bz_barz)){
  v<-est_all_res_s4b$est_m_res$B0 + (est_all_res_s4b$est_m_res$Bz + est_all_res_s4b$est_m_res$Bz_bar)*E_z2[i]  + est_all_res_s4b$est_m_res$Bq*(E_z2[i]^2 + 3) +  Bz_barz[i]*E_z2[i]^2  +(est_all_res_s4b$est_m_res$Bn + est_all_res_s4b$est_m_res$Bnz*E_z2[i] + est_all_res_s4b$est_m_res$Bnz_bar*E_z2[i])*n
  est_v_res[,i]<-v 
}
par(mar=c(4,4,4,2))

plot(NA, col="gray", ylab="Equilibrium n", xlab="Mean phenotype", cex=0.5, ylim=c(0,600), xlim=c(4,16), main="Frequency-dependent selection")
for(i in 1:length(Bz_barz)){
  points(exp(est_Eq_n_res[,i])~E_z, col=colors[i], type="l")
}

points(exp(E_n2)~E_z2, pch=8, col="darkgray", cex=0.4)

#plot_pred1(est_all_res_s1a, col="gray", lty=2)

plot_pred1(est_all_res_s4b, "white")
plot_pred1(est_all_res_s4c,  col=colors[which.min(abs(Bz_barz - est_all_res_s4c$est_m_res$Bz_barz))])
plot_pred1(est_all_res_s4d, col=colors[which.min(abs(Bz_barz - est_all_res_s4d$est_m_res$Bz_barz))])

plot(NA, ylab="Mean log fitness", xlab="Log population size", ylim=c(0,0.8), xlim=c(2,6))
for(i in 1:length(Bz_barz)){
  points(est_v_res[,i]~n, col=colors[i], type="l")
}

#plot_rn(est_all_res_s1a,  col="gray", lty=2)
plot_rn(est_all_res_s4b,  col="white", lty=1)
plot_rn(est_all_res_s4c,  col="black", lty=1)
plot_rn(est_all_res_s4d,  col="black", lty=1)

par(mar=c(4,3,2,1))
Bz_barz<-seq(est_all_res_s4c$est_m_res$Bz_barz-0.0001, est_all_res_s4d$est_m_res$Bz_barz+0.0001, by=0.00001)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bz_barz))

plot(NA, ylim=c(min(Bz_barz),max(Bz_barz)), xlim=c(0,1), xaxt="n", xlab="", yaxt="n", axes="F", ylab="", main=expression(beta[paste("z", bar(z))]))
segments(0, Bz_barz, 1, Bz_barz, col=colors)
axis(2, round(seq(min(Bz_barz), max(Bz_barz),0.001),3), round(seq(min(Bz_barz), max(Bz_barz),0.001),3), las=2)
abline(h=0, col="white", lwd=2)
abline(h=Bz_barz[which.min(abs(Bz_barz - est_all_res_s4c$est_m_res$Bz_barz))],lwd=2)
abline(h=Bz_barz[which.min(abs(Bz_barz - est_all_res_s4d$est_m_res$Bz_barz))],lwd=2)

