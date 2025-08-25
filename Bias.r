require(reshape2)
source("/home/yi/Dropbox/SocialFitnessEffects/Code/load.r")
load("/home/yi/Dropbox/SocialFitnessEffects/Bias.RData")

bias_df$Year<-as.factor(bias_df$Years)
ests_long<- melt(bias_df, id.vars = c("Year", "Scenario"), measure.vars = c("n", "z"),
                  variable.name = "Response", value.name = "Value")


scenario_labels <- c(
  "1" = "Additive FD",
  "2" = "Relative FD 1",
  "3" = "Relative FD 2",
  "4" = "Multiplicative FD"
)


# Function to calculate the interquartile range (IQR)
iqr <- function(x) {
  return(quantile(x, 0.75) - quantile(x, 0.25))
}


facet_labels <- c(
  "n" = "A) Log population size",
  "z" = "B) Log Mean phenotype"
)

custom_colors <- c(
  "1" = "#377EB8",  # Blue
  "2" = "#FF7F00",  # Orange
  "3" = "#4DAF4A",  # Green
  "4" = "#984EA3"  # Purple
)

# Define the custom shapes
custom_shapes <- c(
  "1" = 21,  # Circle
  "2" = 22,  # Square
  "3" = 23,  # Diamond
  "4" = 24  # Triangle up
)


pdf("Figures/Bias.pdf", height= 4, width=8)
ggplot(ests_long, aes(x = Year, y = Value, fill = Scenario, color = Scenario, shape = Scenario)) +
  stat_summary(fun = median, geom = "point", size = 3, 
               position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun.data = "median_hilow", geom = "errorbar", width = 0.2, 
               position = position_dodge(width = 0.75)) +  # Add IQR bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add dashed line at y = 0
  labs(y = "Bias",
       x = "Length of study period (e.g years)",
       fill = "Type of Selection",  # Change legend title for fill
       color = "Type of Selection",  # Change legend title for color
       shape = "Type of Selection") +  # Change legend title for shape
  scale_x_discrete(labels = c("1" = "10", "2" = "20", "3" = "40")) +  # Custom x-axis labels
   scale_y_continuous(limits = c(-1, 1)) +   # Fix y-axis range
  scale_fill_manual(values = custom_colors, labels = scenario_labels) +  # Apply custom colors and labels to fill
  scale_color_manual(values = custom_colors, labels = scenario_labels) +  # Apply custom colors and labels to color
  scale_shape_manual(values = custom_shapes, labels = scenario_labels) +  # Apply custom shapes and labels to shape
  facet_wrap(~ Response, scales = "fixed", labeller = labeller(Response = c(n = "A) Log population size", z = "B) Log mean phenotype"))) +  # Custom facet titles
  theme_minimal(base_size = 14) +  # Base font size for publication
  theme(axis.title.x = element_text(size = 14),  # X-axis title
    axis.title.y = element_text(size = 14),  # Y-axis title
    axis.text.y = element_text(size = 12),  # Y-axis labels
    legend.title = element_text(size = 14),  # Legend title
    legend.text = element_text(size = 12),  # Legend text
    legend.position = "right",  # Position the legend on the right
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),  # Remove background
    axis.line = element_line(color = "black"),  # Add axis lines
    strip.background = element_blank(),  # Remove background from facet strip
    strip.text = element_text(face = "bold", size = 12),    panel.spacing = unit(2, "lines")    # Add axis lines
    )
                                        # + ylim(-1,1)

dev.off()


file.copy(from="Figures/Bias.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures/Bias.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

#####




ests_df <- do.call(rbind.data.frame, ests_res)
bias_df <- do.call(rbind.data.frame, bias_res)

df<-cbind(bias_df)

df$B0[df$Years==10]<-df$B0[df$Years==10]-1.98
df$B0[df$Years==20]<-df$B0[df$Years==20]-1.15
df$B0[df$Years==40]<-df$B0[df$Years==40]-0.55
df$B0[df$Years==80]<-df$B0[df$Years==80]-0.36

df$Bn[df$Years==10]<-df$Bn[df$Years==10]+0.23
df$Bn[df$Years==20]<-df$Bn[df$Years==20]+0.19
df$Bn[df$Years==40]<-df$Bn[df$Years==40]+0.09
df$Bn[df$Years==80]<-df$Bn[df$Years==80]+0.07

tapply(df$B0, df$Years, mean)
tapply(df$Bn, df$Years, mean)
  
df$Year<-as.factor(df$Years)
df$s<-as.factor(paste(df$Year, df$ScenID))

BZ=droplevels(df[df$Scenario==1,])
BzminZ=droplevels(df[df$Scenario==2,])
BzminZ2=droplevels(df[df$Scenario==3,])
BzZ=droplevels(df[df$Scenario==4,])



pdf("Figures/BiasFDEsts.pdf", height= 8, width=8)
par(mfrow=c(2,2), mar=c(5,5,1,1))
plot(BZ$BZ~BZ$s, ylab=bquote(beta[bar(z)]), xlab="", xaxt="n")
axis(1, 1:8, rep(c("a", "b"), 4))
axis(1, c(1.5,3.5, 5.5, 7.5), c("10 t", "20 t", "40 t", "80 t"), line=1.1, tick = FALSE)
abline(h=0,lty=2)

plot(BzminZ$BzminZ~BzminZ$s,ylab=bquote( beta[(z -bar(z))]), xlab="", xaxt="n")
abline(h=0,lty=2)
axis(1, 1:8, rep(c("a", "b"), 4))
axis(1, c(1.5,3.5, 5.5, 7.5), c("10 t", "20 t", "40 t", "80 t"), line=1.1, tick = FALSE)


plot(BzminZ2$BzminZ2~BzminZ2$s, ylab=bquote( beta[(z -bar(z))^2]), xlab="", xaxt="n")
abline(h=0,lty=2)
axis(1, 1:8, rep(c("a", "b"), 4))
axis(1, c(1.5,3.5, 5.5, 7.5), c("10 t", "20 t", "40 t", "80 t"), line=1.1, tick = FALSE)

plot(BzZ$Bz_barz~BzZ$s, ylab=bquote(beta[paste("z",bar(z))]), xlab="", xaxt="n")
abline(h=0,lty=2)
axis(1, 1:8, rep(c("a", "b"), 4))
axis(1, c(1.5,3.5, 5.5, 7.5), c("10 t", "20 t", "40 t", "80 t"), line=1.1, tick = FALSE)

dev.off()


file.copy(from="Figures/BiasFDEsts.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//BiasFDEsts.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)


pdf("Figures/BiasB0Bn.pdf", height= 4, width=8)
par(mfrow=c(2,4), mar=c(5,5,1,1))
plot(BZ$B0~BZ$Year, ylab=bquote(beta[0]), xlab="Study period (years)", main="Additive")
abline(h=0,lty=2)


plot(BzminZ$B0~BzminZ$Year, ylab=bquote(beta[0]), xlab="Study period (years)", main="Relative 1")
abline(h=0,lty=2)


plot(BzminZ2$B0~BzminZ2$Year, ylab=bquote(beta[0]), xlab="Study period (years)", main="Relative 2")
abline(h=0,lty=2)


plot(BzZ$B0~BzZ$Year, ylab=bquote(beta[0]), xlab="Study period (years)", main="Multiplicative")
abline(h=0,lty=2)

plot(BZ$Bn~BZ$Year, ylab=bquote(beta[n]), xlab="Study period (years)", main="")
abline(h=0,lty=2)

plot(BzminZ$Bn~BzminZ$Year, ylab=bquote(beta[n]), xlab="Study period (years)")
abline(h=0,lty=2)

plot(BzminZ2$Bn~BzminZ2$Year, ylab=bquote(beta[n]), xlab="Study period (years)")
abline(h=0,lty=2)


plot(BzZ$Bn~BzZ$Year, ylab=bquote(beta[n]), xlab="Study period (years)")
abline(h=0,lty=2)

dev.off()

file.copy(from="Figures/BiasB0Bn.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//BiasB0bn.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)


######
pdf("Figures/BiasBzBq.pdf", height= 4, width=8)
par(mfrow=c(2,4), mar=c(5,5,1,1))
plot(BZ$Bz~BZ$Year, ylab=bquote(beta[z]), xlab="Study period (years)", main="Additive")
abline(h=0,lty=2)


plot(BzminZ$Bz~BzminZ$Year, ylab=bquote(beta[z]), xlab="Study period (years)", main="Relative 1")
abline(h=0,lty=2)


plot(BzminZ2$Bz~BzminZ2$Year, ylab=bquote(beta[z]), xlab="Study period (years)", main="Relative 2")
abline(h=0,lty=2)


plot(BzZ$Bz~BzZ$Year, ylab=bquote(beta[z]), xlab="Study period (years)", main="Multiplicative")
abline(h=0,lty=2)

plot(BZ$Bq~BZ$Year, ylab=bquote(beta[q]), xlab="Study period (years)", main="")
abline(h=0,lty=2)

plot(BzminZ$Bq~BzminZ$Year, ylab=bquote(beta[q]), xlab="Study period (years)")
abline(h=0,lty=2)

plot(BzminZ2$Bq~BzminZ2$Year, ylab=bquote(beta[q]), xlab="Study period (years)")
abline(h=0,lty=2)


plot(BzZ$Bq~BzZ$Year, ylab=bquote(beta[q]), xlab="Study period (years)")
abline(h=0,lty=2)

dev.off()

file.copy(from="Figures/BiasBzBq.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//BiasBzBq.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)


pdf("Figures/BiasRelative1Ests.pdf", height= 8, width=8)
par(mfrow=c(2,2), mar=c(5,5,1,1))

plot(BzminZ$B0~BzminZ$Year, ylab=bquote(beta[0]), xlab="Study period (years)")
abline(h=1,lty=2)

plot(BzminZ$Bn~BzminZ$Year, ylab=bquote(beta[n]), xlab="Study period (years)")
abline(h=Bn,lty=2)

plot(BzminZ$Bq~BzminZ$Year, ylab=bquote(beta[q]), xlab="Study period (years)")
abline(h=-gamma/2,lty=2)

plot(BzminZ$BzminZ~BzminZ$s,ylab=bquote( beta[(z -bar(z))]), xlab="Study period (years)")
abline(h=-0.1,lty=2)
abline(h=0.1,lty=2)

dev.off()

file.copy(from="Figures/BiasRelative1Ests.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//BiasRelative1Ests.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)



####

pdf("Figures/BiasRelative2Ests.pdf", height= 8, width=8)

par(mfrow=c(2,2), mar=c(5,5,1,1))

plot(BzminZ2$B0~BzminZ2$Year, ylab=bquote(beta[0]), xlab="Study period (years)")
abline(h=B0,lty=2)

plot(BzminZ2$Bn~BzminZ2$Year, ylab=bquote(beta[n]), xlab="Study period (years)")
abline(h=Bn,lty=2)

plot(BzminZ2$Bq~BzminZ2$Year, ylab=bquote(beta[q]), xlab="Study period (years)")
abline(h=Bq,lty=2)

plot(BzminZ2$BzminZ2~BzminZ2$s, ylab=bquote( beta[(z -bar(z))^2]), xlab="Study period (years)")
abline(h=-0.04,lty=2)
abline(h=0.04,lty=2)

dev.off()

file.copy(from="Figures/BiasRelative2Ests.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//BiasRelative2Ests.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

##


pdf("Figures/BiasMultiplicativeEsts.pdf", height= 8, width=8)
par(mfrow=c(2,2), mar=c(5,5,1,1))

plot(BzZ$B0~BzZ$Year, ylab=bquote(beta[0]), xlab="Study period (years)")
abline(h=B0,lty=2)

plot(BzZ$Bn~BzZ$Year, ylab=bquote(beta[n]), xlab="Study period (years)")
abline(h=Bn,lty=2)

plot(BzZ$Bq~BzZ$Year, ylab=bquote(beta[q]), xlab="Study period (years)")
abline(h=Bq,lty=2)

plot(BzZ$Bz_barz~BzZ$s, ylab=bquote(beta[paste("z",bar(z))]), xlab="Study period (years)")
abline(h=0.04,lty=2)
abline(h=-0.04,lty=2)

file.copy(from="Figures/BiasMultiplicativeEsts.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//BiasMultiplicativeEsts.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)


dev.off()




pdf("Figures/BiasZ.pdf", height= 8, width=8)
par(mfrow=c(2,2), mar=c(5,5,1,1))

plot(BZ$z~BZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)

plot(BzminZ$z~BzminZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)

plot(BzminZ2$z~BzminZ2$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)


plot(BzZ$z~BzZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)


file.copy(from="Figures/BiasN.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//BiasN.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)



pdf("Figures/Z.pdf", height= 8, width=8)
par(mfrow=c(2,2), mar=c(5,5,1,1))

plot(BZ$eq_z~BZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)

plot(BzminZ$eq_z~BzminZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)

plot(BzminZ2$eq_z~BzminZ2$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)


plot(BzZ$eq_z~BzZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)
dev.off()

file.copy(from="Figures/Z.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//Z.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)


pdf("Figures/N.pdf", height= 8, width=8)
par(mfrow=c(2,2), mar=c(5,5,1,1))

plot(BZ$eq_n~BZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)

plot(BzminZ$eq_n~BzminZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)

plot(BzminZ2$eq_n~BzminZ2$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)


plot(BzZ$eq_n~BzZ$s, ylab="z", xlab="Study period (years)")
abline(h=0,lty=2)

dev.off()

file.copy(from="Figures/BiasN.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures//BiasN.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

