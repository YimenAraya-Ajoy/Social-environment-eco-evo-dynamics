setwd("/home/yi/Dropbox/SocialFitnessEffects")
m<-matrix(1:6,2,3, byrow = "TRUE")
pdf("Figures/Fig_4.pdf", height= 7, width=7)
layout(m, heights = c(1,1), widths = c(1,1,0.3))
source("Code/Figures/FrequencyDensityDependenceFigure.R")
source("Code/Figures/FrequencyDensityDependentSelectionFigure.R")
dev.off()

