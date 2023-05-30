
source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")
load("Results/FrequencyDensityDependentSelection.RData")
     
est_m_res<-rbind(est_all_res_s6b$est_m_res, est_all_res_s6c$est_m_res, est_all_res_s6d$est_m_res)
Bnz_barz<-seq(min(est_m_res$Bnz_barz)-0.0001, max(est_m_res$Bnz_barz)+0.00003, by=0.00001)
eqsS6_res<-eqsS6(est_m_res, Bnz_barz)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz_barz))


par(mar=c(4,4,4,2))
plot(NA, col="gray", ylab="Equilibrium n", xlab="Mean phenotype", cex=0.5, ylim=c(0,600), xlim=c(4,16), main="Frequency-denstiy-dependent selection")
for(i in 1:length(Bnz_barz)){
  points(exp(eqsS6_res$est_Eq_n_res[,i])~eqsS6_res$E_z$E_z, col=colors[i], type="l")
}

points(exp(eqsS6_res$max_n)~eqsS6_res$max_z, pch=8, col="darkgray", cex=0.4)
#plot_pred1(est_all_res_s1a, col="gray", lty=2)
plot_pred1(est_all_res_s6b, "white")
plot_pred1(est_all_res_s6c, colors[which.min(abs(Bnz_barz - est_all_res_s6c$est_m_res$Bnz_barz))])
plot_pred1(est_all_res_s6d, colors[which.min(abs(Bnz_barz - est_all_res_s6d$est_m_res$Bnz_barz))])


plot(NA, ylab="Mean log fitness", xlab="Log population size", ylim=c(0,0.8), xlim=c(2,6))
for(i in 1:length(Bnz_barz)){
  points(eqsS6_res$est_v_res[,i]~eqsS6_res$n$n, col=colors[i], type="l")
}

#plot_rn(est_all_res_s1a,  col="gray", lty=2)
plot_rn(est_all_res_s6b,  col="white", lty=1)
plot_rn(est_all_res_s6c,  col="black", lty=1)
plot_rn(est_all_res_s6d,  col="black", lty=1)

par(mar=c(4,3,2,1))
Bnz_barz<-seq(min(est_m_res$Bnz_barz)-0.0001, max(est_m_res$Bnz_barz)+0.00003, by=0.000001)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz_barz))
plot(NA, ylim=c(min(Bnz_barz),max(Bnz_barz)), xlim=c(0,1), xaxt="n", xlab="", yaxt="n", axes="F", ylab="", main=expression(beta[paste("z",bar(z),n)]))
segments(0, Bnz_barz, 1, Bnz_barz, col=colors)
axis(2, round(seq(min(Bnz_barz), max(Bnz_barz),0.0001),4), round(seq(min(Bnz_barz), max(Bnz_barz),0.0001),4), las=2)
axis(2, 0, 0, las=2)
abline(h=0, lwd=1, col="white")
abline(h=Bnz_barz[which.min(abs(Bnz_barz - est_all_res_s6c$est_m_res$Bnz_barz))],lwd=1)
abline(h=Bnz_barz[which.min(abs(Bnz_barz - est_all_res_s6d$est_m_res$Bnz_barz))],lwd=1)

