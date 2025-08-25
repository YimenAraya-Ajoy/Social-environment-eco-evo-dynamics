

cols<-c("orange", "black", "blue")

E_z <- seq(0, 4, 0.001)                                  
length_z<-length(E_z)
length_B<-39


plot_traj<-function(x){
if(x=="n"){  
plot(NA, ylim=c(50, 500), xlim=c(50,300),ylab="", xlab="", main="", cex.axis=1.5)
mtext("Pop size", side = 2, line = 3, cex=1.5)
} else {
 plot(NA, ylim=c(4,14), xlim=c(50,300), ylab="", xlab="", cex.axis=1.5)
mtext("Mean Phenotype", side = 2, line = 3, cex=1.5)
}
mtext("Time", side = 1, line = 4, cex=1.5)
}




theme.novpadding1 <-list(axis.line = list(col = "transparent"),
                         clip =list(panel="off"))


col<-matlab.like(100)
