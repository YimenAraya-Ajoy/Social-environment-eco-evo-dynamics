

generate_points <- function(smallest, middle, largest, num_points) {
  # Generate points from the smallest value to the middle value
  points_smallest_to_middle <- seq(smallest, middle, length.out = num_points)
  
  # Generate points from the middle value to the largest value
  points_middle_to_largest <- seq(middle, largest, length.out = num_points)
  
  # Combine the two sequences, removing the duplicate middle value
  points <- c(points_smallest_to_middle[-length(points_smallest_to_middle)], points_middle_to_largest)
  
  return(points)
}

plot_traj<-function(x){
if(x=="n"){  
plot(NA, ylim=c(50, 500), xlim=c(50,300),ylab="", xlab="", main="", cex.axis=1.5)
mtext("Pop size", side = 2, line = 3, cex=1.5)
} else {
 plot(NA, ylim=c(4,14), xlim=c(50,300), ylab="", xlab="", cex.axis=1.5)
mtext("Mean Phenotype", side = 2, line = 3, cex=1.5)
}
mtext("Time", side = 1, line = 4, cex=1.5)
abline(v=100, col="red", lty=2)
}


plot_land<-function(){plot(NA, ylab="", xlab="", cex=0.5, ylim=c(0, 550), xlim=c(0,4), main="")
mtext("Mean phenotype", side = 1, line = 3, cex=1)
mtext("Pop size", side = 2, line = 3, cex=1)
}



plot_n<-function(s,start,col, lty=1){
  for(i in 1:n.sims){
    xt<-s[s$simID==i,]
    points(xt$N~xt$year, type="l", col=adjustcolor( col, alpha.f = 0.01))
  }  
points(tapply(s$N, s$year, mean), type="l", col=col, lty=lty)
}

plot_z<-function(s,start,col, lty=1){
  for(i in 1:n.sims){
    xt<-s[s$simID==i,]
    points(xt$z_bar~xt$year, type="l", col=adjustcolor( col, alpha.f = 0.01))
  }  
points(tapply(s$z_bar, s$year, mean), type="l", col=col, lty=lty)
}




plot_pred1<-function(est_all, col="black", lty=1){
 # points(exp(est_all$est_res$Eq_n)~est_all$est_res$Eq_z , bg=col, pch=21, cex=0.5)
 # points(exp(est_all$est_Eq_z_res$E_n)~est_all$est_Eq_z_res$E_z, type="l", col="black", lty=lty, lwd=1)
  points(m_est$E_n~m_est$E_z, col=col, cex=2)
  points(exp(est_all$est_m_res$Eq_n)~est_all$est_m_res$Eq_z, bg=col, pch=21,cex=1.2)
}


cols<-c("orange", "black", "blue")

E_z <- seq(0, 4, 0.001)                                  
length_z<-length(E_z)
length_B<-39
