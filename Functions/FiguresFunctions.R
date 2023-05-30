plot_pred1<-function(est_all, col="black", lty=1){
  points(exp(est_all$est_res$Eq_n)~est_all$est_res$Eq_z , bg=col, pch=21, cex=0.5)
  points(exp(est_all$est_Eq_z_res$E_n)~est_all$est_Eq_z_res$E_z, type="l", col="black", lty=lty, lwd=1)
  #points(m_est$E_n~m_est$E_z, col=col, cex=2)
  points(exp(est_all$est_m_res$Eq_n)~est_all$est_m_res$Eq_z, bg=col, pch=21,cex=2)
}

plot_pred2<-function(est_all, col="black", lty=1){
  points(est_all$est_res$Eq_n*est_all$est_res$Eq_z~est_all$est_res$Eq_z , bg=col, pch=21, cex=0.5)
  #points(exp(est_all$est_Eq_z_res$E_n)~est_all$est_Eq_z_res$E_z, type="l", col="black", lty=lty, lwd=1)
  #points(m_est$E_n~m_est$E_z, col=col, cex=2)
  points(est_all$est_m_res$Eq_n*est_all$est_m_res$Eq_z~est_all$est_m_res$Eq_z, bg=col, pch=21,cex=2)
}


####
plot_rn<-function(est_all, col="black", lty=1){
  points(est_all$est_v_n_res$E_w~est_all$est_v_n_res$n, type="l", col=col, lwd=1,lty=lty)
  #for(i in 1:n.sims){
  # xt<-x[[i]]
  #points(xt$lambda~xt$N, col= adjustcolor( col, alpha.f = 0.1))
  # }
}

plot_n<-function(s,col, lty=1){
  df_s<-make.dataframe(s)
  dfm_s <- calc.simAvg(df_s)
  for(i in 1:n.sims){
    xt<-lapply(s,"[[",2)[[i]]
    points(xt$N~xt$year, type="l", col=adjustcolor( col, alpha.f = 0.1))
  }
  points(dfm_s$Nbar~dfm_s$year, type="l", col="black", lty=lty)
  
}

plot_z<-function(s,col, lty=1){
  df_s<-make.dataframe(s)
  dfm_s <- calc.simAvg(df_s)
  for(i in 1:n.sims){
    xt<-lapply(s,"[[",2)[[i]]
    points(xt$z_bar~xt$year, type="l", col=adjustcolor( col, alpha.f = 0.1))
  }
  points(dfm_s$zbar~dfm_s$year, type="l", col="black", lty=lty)
  
}

plot_fit_func<-function(x, col="black"){
  points(exp(x$fitness_func_res$E_v)~x$fitness_func_res$E_z, type="l",col=col)
}


fitness_func_n<-function(x, col="black"){
  est_m_res<-x$est_m_res
  n<-log(c(10, 50, 400))
  E_z<-seq(0,20,0.01)
  E_v<-matrix(NA, length(E_z), length(n))
  for(i in 1:length(n)){
  E_v[,i]=(est_m_res$B0 + (est_m_res$Bz + est_m_res$Bz_bar)*E_z  + est_m_res$Bq*(E_z^2)) +
    (est_m_res$Bn + est_m_res$Bnz*E_z +est_m_res$Bnz_bar*E_z) * n[i]
}

for(i in 1:length(n)){
points(E_v[,i]~E_z, type="l", col=col)
}
}


