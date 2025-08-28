source("Base.r")

sim_values=c(s7c.x$Bzn, s7b.x$Bzn, s7d.x$Bzn)
Bzn <- generate_points(sim_values[1], sim_values[2], sim_values[3], 20) 
E_n_sim_values = match(sim_values, Bzn)

# Create matrices
BZmat <- matrix(Bzn, nrow = length_z, ncol = length_B, byrow = TRUE)  
E_zmat <- matrix(E_z, nrow = length_z, ncol = length_B, byrow = FALSE) 

# Scalars
B0  <-  r_max - gamma / 2 * (theta[2] - 0)^2
Bz  <- -theta[2] * 2 * Bq

# Compute result
E_n   <- exp((B0 - log(1 - p) + Bz * E_zmat + Bq * (E_zmat^2 + 1))/-(Bn+BZmat*E_zmat))
max_w<-apply(E_n, 2, max)

Bnz<-Bzn[E_n_sim_values]
dds_eq_n  <-  exp((-2 * sqrt((B0-log(0.5)) * Bq * Bnz^2 - Bz * Bq * Bn * Bnz + Bq^2 * Bn^2 + Bq^2 * Bnz^2 * 1) - Bz * Bnz + 2 * Bq * Bn) / Bnz^2)
dds_eq_z  <-   (Bz+Bnz*log(eq_n))/-(2*Bq) 

dds_eq_n[2]<-202
dds_eq_z[2]<-2

##Plot
plot_land()


for(i in 1:length_B){
points(E_n[,i]~E_z, col=alpha("black",0.3), type="l")
points(max_w[i]~E_z[E_n[,i]==max_w[i]], cex=0.3, pch=24)
}



for(i in 1:3){
  points(E_n[,E_n_sim_values[i]]~E_z, col="black", type="l", lty=2)
  points(dds_eq_n[i]~ dds_eq_z[i], cex=2.2, col=cols[i], pch=8)
}


points(s7_eq_sim_n~s7_eq_sim_z,  col="black", cex=2, bg=alpha(cols,0.2), pch=21)
points(s7_eq_reg_n~s7_eq_reg_z, col=cols, cex=1.5, pch=18)

text(x = rep(3.9,3), y = E_n[E_z==3.9,E_n_sim_values], labels = sim_values, cex=1)








