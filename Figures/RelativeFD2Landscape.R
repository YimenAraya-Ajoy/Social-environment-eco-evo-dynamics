source("Code/Figures/Base.r")

sim_values=c(s3c.x$BzminZ2, s3b.x$BzminZ2, s3d.x$BzminZ2)
BzminZ2 <- generate_points(sim_values[1], sim_values[2], sim_values[3], 20) 
E_n_sim_values = match(sim_values, BzminZ2)

# Create matrices
BZmat <- matrix(BzminZ2, nrow = length_z, ncol = length_B, byrow = TRUE)  
E_zmat <- matrix(E_z, nrow = length_z, ncol = length_B, byrow = FALSE) 

# Scalars
B0  <-  r_max - gamma / 2 * (theta[2] - 0)^2
Bz  <- -theta[2] * 2 * Bq
theta_pred<-Bz/(-2*Bq)

# Compute result
E_n   <- exp((B0 - log(1-p) + Bz * E_zmat + Bq * (E_zmat^2 + 1) + BZmat)/-Bn)
max_w<-apply(E_n, 2, max)

##Plot
plot_land()
mtext("C) Relative FD2", side = 3, cex=0.8)


for(i in 1:length_B){
  points(E_n[,i]~E_z, col=alpha("black",0.3), type="l")
  points(max_w[i]~E_z[E_n[,i]==max_w[i]], cex=0.3, pch=24)
}


for(i in 1:3){
    points(E_n[,E_n_sim_values[i]]~E_z, col="black", type="l", lty=2)
    points(E_n[theta_pred==E_z,E_n_sim_values[i]]~theta_pred, cex=2, col=cols[i], pch=8)
}


points(s3_eq_sim_n~s3_eq_sim_z,  col="black", cex=2, bg=alpha(cols,0.2), pch=21)
points(s3_eq_reg_n~s3_eq_reg_z, col=cols, cex=1.5, pch=18)

text(x = rep(3.9,3), y = E_n[E_z==3.9,E_n_sim_values]+c(-30,0,30), labels = sim_values, cex=1)

