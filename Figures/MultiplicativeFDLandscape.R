source("Base.r")

sim_values=c(s4c.x$BZz, s4b.x$BZz, s4d.x$BZz)
BzZ <- generate_points(sim_values[1], sim_values[2], sim_values[3], 20) 
E_n_sim_values = match(sim_values, BzZ)

# Create matrices
BZmat <- matrix(BzZ, nrow = length_z, ncol = length_B, byrow = TRUE)  
E_zmat <- matrix(E_z, nrow = length_z, ncol = length_B, byrow = FALSE) 

# Scalars
B0  <-  r_max - gamma / 2 * (theta[2] - 0)^2
Bz  <- -theta[2] * 2 * Bq
theta_pred<-round(Bz/-(2*Bq + BzZ),2)

# Compute result
E_n   <- exp((B0 - log(1-p) + Bz * E_zmat + Bq * (E_zmat^2 + 1) + BZmat*E_zmat^2)/-Bn)
max_w<-apply(E_n, 2, max)

##Plot
plot_land()
mtext("D) MultiplicativeFD", side = 3, cex=0.8)


for(i in 1:length_B){
  points(E_n[,i]~E_z, col=alpha("black",0.3), type="l")
points(max_w[i]~E_z[E_n[,i]==max_w[i]], cex=0.3, pch=24)
}


for(i in 1:3){
    points(E_n[,E_n_sim_values[i]]~E_z, col="black", type="l", lty=2)
    points(E_n[theta_pred[E_n_sim_values[i]]==E_z,E_n_sim_values[i]]~theta_pred[E_n_sim_values[i]], cex=2, col=cols[i], pch=8)
}


points(s4_eq_sim_n~s4_eq_sim_z,  col="black", cex=2, bg=alpha(cols,0.2), pch=21)
points(s4_eq_reg_n~s4_eq_reg_z, col=cols, cex=1.5, pch=18)

text(x = rep(3.9,3), y = E_n[E_z==3.9,E_n_sim_values], labels = sim_values, cex=1)








