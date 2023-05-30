source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")
load("Results/DensityDependentSelection.RData")
load("Results/FrequencyDensityDependentSelection.RData")


pdf("Figures/Fig_5.pdf", height= 7, width=6)
m<-matrix(1:4,2,2, byrow = TRUE)
layout(m, widths = c(1,0.3))
est_m_res<-rbind(est_all_res_s2b$est_m_res, est_all_res_s2c$est_m_res, est_all_res_s2d$est_m_res)
Bnz<-seq(min(est_m_res$Bnz)-0.001, max(est_m_res$Bnz)+0.0005, by=0.00025)
eqsS2_res<-eqsS2(est_m_res, Bnz)

colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz))
col1<-c(colors[which.min(abs(Bnz - est_all_res_s2b$est_m_res$Bnz))],colors[which.min(abs(Bnz - est_all_res_s2c$est_m_res$Bnz))],colors[which.min(abs(Bnz - est_all_res_s2d$est_m_res$Bnz))])


par(mar=c(4,4,3,1))

plot(NA, ylim=c(3.5,7), xlim=c(6,14), xlab="Mean phenotype", ylab="Log population size", main="Density-dependent selection")
for(i in 1:length(Bnz)){
  points(eqsS2_res$n$n~ eqsS2_res$Eq_z2[,i], col=colors[i], type="l")
}
for(i in 1:length(Bnz)){
  points(eqsS2_res$est_Eq_n_res[,i]~eqsS2_res$E_z$E_z, col=colors[i], type="l")
}
#points(est_all_res_s6b$est_m_res$Eq_n~est_all_res_s6b$est_m_res$Eq_z, bg=colors[which.min(abs(Bnz_barz - est_all_res_s6b$est_m_res$Bnz_barz))], pch=21, cex=1.4)
points(est_all_res_s2b$est_Eq_res$Eq_n~est_all_res_s2b$est_Eq_res$Eq_z, bg="white", pch=21)

#points(est_all_res_s6c$est_m_res$Eq_n~est_all_res_s6c$est_m_res$Eq_z, bg=colors[which.min(abs(Bnz_barz - est_all_res_s6c$est_m_res$Bnz_barz))], pch=21, cex=1.4)
points(est_all_res_s2c$est_Eq_res$Eq_n~ est_all_res_s2c$est_Eq_res$Eq_z, col=col1[2])

#points(est_all_res_s6d$est_m_res$Eq_n~est_all_res_s6d$est_m_res$Eq_z, bg=colors[which.min(abs(Bnz_barz - est_all_res_s6d$est_m_res$Bnz_barz))], pch=21, cex=1.4)
points(est_all_res_s2d$est_Eq_res$Eq_n~ est_all_res_s2d$est_Eq_res$Eq_z, col=col1[3])



points(eqsS2_res$eqs$n~eqsS2_res$eqs$E_z, pch=21, cex=1.5, bg=colors)


par(mar=c(4,4,2,1))
Bnz<-seq(min(est_m_res$Bnz)-0.001, max(est_m_res$Bnz)+0.0005, by=0.00001)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz))

plot(NA, ylim=c(min(Bnz),max(Bnz)), xlim=c(0,1), xaxt="n", xlab="", yaxt="n", axes="F", ylab="", main=expression(beta[zn]))
segments(0, Bnz, 1, Bnz, col=colors, lwd=1)
axis(2, round(seq(min(Bnz), max(Bnz),0.001),3), round(seq(min(Bnz), max(Bnz),0.001),3), las=2)
axis(2, 0, 0, las=2)
abline(h=0, col="white", lwd=2)
abline(h=Bnz[which.min(abs(Bnz - est_all_res_s2c$est_m_res$Bnz))],lwd=2)
abline(h=Bnz[which.min(abs(Bnz - est_all_res_s2d$est_m_res$Bnz))],lwd=2)

#####

est_m_res<-rbind(est_all_res_s6b$est_m_res, est_all_res_s6c$est_m_res, est_all_res_s6d$est_m_res)
Bnz_barz<-seq(min(est_m_res$Bnz_barz)-0.0001, max(est_m_res$Bnz_barz)+0.00005, by=0.000025)
eqsS6_res<-eqsS6(est_m_res, Bnz_barz)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz_barz))
col1<-c(colors[which.min(abs(Bnz_barz - est_all_res_s6b$est_m_res$Bnz_barz))],colors[which.min(abs(Bnz_barz - est_all_res_s6c$est_m_res$Bnz_barz))],colors[which.min(abs(Bnz_barz - est_all_res_s6d$est_m_res$Bnz_barz))])


par(mar=c(4,4,3,1))
plot(NA, ylim=c(3.5,7), xlim=c(6,14), xlab="Mean phenotype", ylab="Log population size", main="Frequency-density-dependent selection")
for(i in 1:length(Bnz_barz)){
  points(eqsS6_res$n$n~ eqsS6_res$Eq_z2[,i], col=colors[i], type="l")
}
for(i in 1:length(Bnz_barz)){
  points(eqsS6_res$est_Eq_n_res[,i]~eqsS6_res$E_z$E_z, col=colors[i], type="l")
}
#points(est_all_res_s6b$est_m_res$Eq_n~est_all_res_s6b$est_m_res$Eq_z, bg=colors[which.min(abs(Bnz_barz - est_all_res_s6b$est_m_res$Bnz_barz))], pch=21, cex=1.4)
points(est_all_res_s6b$est_Eq_res$Eq_n~est_all_res_s6b$est_Eq_res$E_z, bg="white", pch=21)

#points(est_all_res_s6c$est_m_res$Eq_n~est_all_res_s6c$est_m_res$Eq_z, bg=colors[which.min(abs(Bnz_barz - est_all_res_s6c$est_m_res$Bnz_barz))], pch=21, cex=1.4)
points(est_all_res_s6c$est_Eq_res$Eq_n~ est_all_res_s6c$est_Eq_res$E_z, col=col1[2])

#points(est_all_res_s6d$est_m_res$Eq_n~est_all_res_s6d$est_m_res$Eq_z, bg=colors[which.min(abs(Bnz_barz - est_all_res_s6d$est_m_res$Bnz_barz))], pch=21, cex=1.4)
points(est_all_res_s6d$est_Eq_res$Eq_n~ est_all_res_s6d$est_Eq_res$E_z, col=col1[3])


points(eqsS6_res$eqs$n~eqsS6_res$eqs$E_z, pch=21, cex=1.5, bg=colors)

par(mar=c(4,4,2,1))
Bnz_barz<-seq(min(est_m_res$Bnz_barz)-0.0001, max(est_m_res$Bnz_barz)+0.00005, by=0.000001)
colors <- colorRampPalette(c("blue","darkgreen","orange"))(length(Bnz_barz))

plot(NA, ylim=c(min(Bnz_barz),max(Bnz_barz)), xlim=c(0,1), xaxt="n", xlab="", yaxt="n", axes="F", ylab="", main=expression(beta[paste("z",bar(z),n)]))
segments(0, Bnz_barz, 1, Bnz_barz, col=colors, lwd=1)
axis(2, round(seq(min(Bnz_barz), max(Bnz_barz),0.0001),4), round(seq(min(Bnz_barz), max(Bnz_barz),0.0001),4), las=2)
axis(2, 0, 0, las=2)
abline(h=0, col="white",lwd=2)
abline(h=Bnz_barz[which.min(abs(Bnz_barz - est_all_res_s6c$est_m_res$Bnz_barz))],lwd=2)
abline(h=Bnz_barz[which.min(abs(Bnz_barz - est_all_res_s6d$est_m_res$Bnz_barz))],lwd=2)

dev.off()
