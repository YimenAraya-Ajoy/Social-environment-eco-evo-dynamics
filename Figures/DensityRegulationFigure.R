source("Code/Functions/FiguresFunctions.R")
load("Results/DensityRegulation.RData")
E_z<-seq(0,20,0.01)
Bn<-round(seq(est_all_res_s1d$est_m_res$Bn-0.005, est_all_res_s1c$est_m_res$Bn+0.005, by=0.0005),4)

n<-seq(0,6,by=0.01)
est_Eq_n_res<-matrix(NA,length(E_z), length(Bn))
est_v_res<-matrix(NA,length(n), length(Bn))
E_z2<-rep(NA, length(Bn))
E_n2<-rep(NA, length(Bn))

for(i in 1:length(Bn)){
  E_n=-(est_all_res_s1b$est_m_res$B0 + ( est_all_res_s1b$est_m_res$Bz + est_all_res_s1b$est_m_res$Bz_bar)*E_z  + est_all_res_s1b$est_m_res$Bq*(E_z^2 +3))/(Bn[i] +  est_all_res_s1b$est_m_res$Bnz*E_z + est_all_res_s1b$est_m_res$Bnz_bar*E_z) 
  est_Eq_n_res[,i]<-E_n
  E_n2[i]<-max(E_n)
  E_z2[i]<-E_z[max(E_n)==E_n]
  }
 

for(i in 1:length(Bn)){
  v<-est_all_res_s1b$est_m_res$B0 + ( est_all_res_s1b$est_m_res$Bz + est_all_res_s1b$est_m_res$Bz_bar)*E_z2[i]  + est_all_res_s1b$est_m_res$Bq*(E_z2[i]^2 + 3) + (Bn[i] + est_all_res_s1b$est_m_res$Bnz*E_z2[i] + est_all_res_s1b$est_m_res$Bnz_bar*E_z2[i])*n
  est_v_res[,i]<-v 
}

colors <- colorRampPalette(c("blue", "darkgreen","orange"))(length(Bn))

par(mar=c(4,4,4,2))
plot(NA, ylab="Equilibrium n", xlab="Mean phenotype", cex=0.5, ylim=c(0,600), xlim=c(4,16), main="Density regulation")
for(i in 1:length(Bn)){
  points(exp(est_Eq_n_res[,i])~E_z, col=colors[i], type="l")
}

points(exp(E_n2)~E_z2, pch=8, col="darkgrey", cex=0.4)

#plot_pred1(est_all_res_s1a, col="gray", lty=2)
plot_pred1(est_all_res_s1b, "white")
plot_pred1(est_all_res_s1c, colors[which.min(abs(Bn - est_all_res_s1c$est_m_res$Bn))])
plot_pred1(est_all_res_s1d, colors[which.min(abs(Bn - est_all_res_s1d$est_m_res$Bn))])

plot(NA, ylab="Mean log fitness", xlab="Log population size", ylim=c(0,0.8), xlim=c(2,6))
for(i in 1:length(Bn)){
 points(est_v_res[,i]~n, col=colors[i], type="l")
}

#plot_rn(est_all_res_s1a,  col="gray", lty=2)
plot_rn(est_all_res_s1b,  col="white", lty=1)
plot_rn(est_all_res_s1c,  col="black", lty=1)
plot_rn(est_all_res_s1d,  col="black", lty=1)

par(mar=c(4,3,3,1))
Bn<-round(seq(est_all_res_s1d$est_m_res$Bn-0.005, est_all_res_s1c$est_m_res$Bn+0.005, by=0.0001),4)
colors <- colorRampPalette(c("blue", "darkgreen","orange"))(length(Bn))

plot(NA, ylim=c(min(Bn),max(Bn)), xlim=c(0,1), xaxt="n", xlab="", yaxt="n", axes="F", ylab="", main=expression(beta[n]))
segments(0, Bn, 1, Bn, col=colors)
axis(2, round(seq(min(Bn), max(Bn),0.01),3), round(seq(min(Bn), max(Bn),0.01),3), las=2)
abline(h=est_all_res_s1b$est_m_res$Bn, col="white", lwd=2.5)
abline(h=Bn[which.min(abs(Bn - est_all_res_s1c$est_m_res$Bn))],lwd=2)
abline(h=Bn[which.min(abs(Bn - est_all_res_s1d$est_m_res$Bn))],lwd=2)

