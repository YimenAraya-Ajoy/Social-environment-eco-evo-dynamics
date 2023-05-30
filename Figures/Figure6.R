source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")

theme.novpadding2 <-list(axis.line = list(col = "transparent"), layout.heights = list(top.padding = 0,
                                                                                      main.key.padding = 0,
                                                                                      key.axis.padding = 0,
                                                                                      axis.xlab.padding = 0,
                                                                                      xlab.key.padding = 0,
                                                                                      key.sub.padding = 0,
                                                                                      bottom.padding = -20),
                         clip =list(panel="off"),
                         layout.widths =
                           list(left.padding = 0,
                                key.ylab.padding = 0,
                                ylab.axis.padding = 0,
                                axis.key.padding = 0,
                                right.padding = 0))


theme.novpadding1 <-list(axis.line = list(col = "transparent"), layout.heights = list(top.padding = -20,
                                                                                      main.key.padding = 0,
                                                                                      key.axis.padding = 0,
                                                                                      axis.xlab.padding = 0,
                                                                                      xlab.key.padding = 0,
                                                                                      key.sub.padding = 0,
                                                                                      bottom.padding = 0),
                         clip =list(panel="off"),
                         layout.widths =
                           list(left.padding = 0,
                                key.ylab.padding = 0,
                                ylab.axis.padding = 0,
                                axis.key.padding = 0,
                                right.padding = 0))

col<-matlab.like(100)

z<-seq(0, 12, by=0.5)
zb<-seq(0, 12, by=0.35)
zz<-expand.grid(z,zb)
colnames(zz)<-c("z", "z'")

w1=10 + 1*zz[,"z"] -1.1*zz[,"z'"] 
rw<-w1/mean(w1)

pb1a<-wireframe(rw~zz[,"z'"]*zz[,"z"], xlab=bquote(bar(z)), ylab=expression(z), zlab=expression(w),
                colorkey = FALSE, zlim=c(-1,3),
                screen = list(z = 60, x = -60), 
                scales = list( arrows = FALSE, col="black", cex.axis=0.8), 
                aspect = c(1.1, 1),
                drape = TRUE, 
                col.regions = col,
                par.settings = theme.novpadding1, main="B")

w1=10 + 1*zz[,"z"] + 1.1*zz[,"z'"] 
rw<-w1/mean(w1)

pb1b<-wireframe(rw~zz[,"z'"]*zz[,"z"], xlab=bquote(bar(z)), ylab=expression(z), zlab=expression(w),
                colorkey = FALSE, zlim=c(-1,3),
                screen = list(z = 60, x = -60), 
                scales = list( arrows = FALSE, col="black", cex.axis=0.8), 
                aspect = c(1.1, 1),
                drape = TRUE, 
                col.regions = col,
                par.settings = theme.novpadding2, main="A")


w1=w1=10 + 1*zz[,"z"] + 1.1*zz[,"z'"]  + (-0.3*zz[,"z"])*zz[,"z'"] 
rw<-w1/mean(w1)

pb1c<-wireframe(rw~zz[,"z'"]*zz[,"z"], xlab=bquote(bar(z)), ylab=expression(z), zlab=expression(w),
                colorkey = FALSE, zlim=c(-1,3),
                screen = list(z = 60, x = -60), 
                scales = list( arrows = FALSE, col="black", cex.axis=0.8), 
                aspect = c(1.1, 1),
                drape = TRUE, 
                col.regions = col,
                par.settings = theme.novpadding1, main="D")

w1=w1=10 + 1*zz[,"z"] + 1*zz[,"z'"]  + (0.3*zz[,"z"])*zz[,"z'"] 
rw<-w1/mean(w1)


pb1d<-wireframe(rw~zz[,"z'"]*zz[,"z"], xlab=bquote(bar(z)), ylab=expression(z), zlab=expression(w),
                colorkey = FALSE, zlim=c(-1,3),
                screen = list(z = 60, x = -60), 
                scales = list( arrows = FALSE, col="black", cex.axis=0.8), 
                aspect = c(1.1, 1),
                drape = TRUE, 
                col.regions = col,
                par.settings = theme.novpadding2, main="C")


w1=10  + 0.3*(zz[,"z"]-zz[,"z'"])^2
rw<-w1/mean(w1)

pb1e<-wireframe(rw~zz[,"z'"]*zz[,"z"], xlab=bquote(bar(z)), ylab=expression(z), zlab=expression(w),
                colorkey = FALSE, zlim=c(-1,3),
                screen = list(z = 60, x = -60), 
                scales = list( arrows = FALSE, col="black", cex.axis=0.8), 
                aspect = c(1.1, 1),
                drape = TRUE, 
                col.regions = col,
                par.settings =theme.novpadding1, main="F")

w1=10 + 0.1*(zz[,"z"]-zz[,"z'"])
rw<-w1/mean(w1)

pb1f<-wireframe(rw~zz[,"z'"]*zz[,"z"], xlab=bquote(bar(z)), ylab=expression(z), zlab=expression(w),
                colorkey = FALSE, zlim=c(-1,3),
                screen = list(z = 60, x = -60), 
                scales = list( arrows = FALSE, col="black", cex.axis=0.8), 
                aspect = c(1.1, 1),
                drape = TRUE, 
                col.regions = col,
                par.settings = theme.novpadding2, main="E")


margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))

pdf("Figures/Fig_6.pdf", height= 12, width=12)

grid.arrange(pb1b,  pb1d, pb1f, pb1a, pb1c, pb1e, ncol=3, nrow=2)

dev.off()
