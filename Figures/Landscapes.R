source("~/Dropbox/SocialFitnessEffects/Code/load_results.r")


pdf("Figures/EcoEvoLand_A.pdf", width=8, height=5)
par(mfrow=c(2,3), mar=c(4,4.2,3,1))
source("Code/Figures/AdditiveFDLandscape.R")
source("Code/Figures/RelativeFD1Landscape.R")
source("Code/Figures/RelativeFD2Landscape.R")
source("Code/Figures/MultiplicativeFDLandscape.R")
source("Code/Figures/FDDRLandscape.R")
source("Code/Figures/DDFDLandscape.R")
dev.off()


file.copy(from="Figures/EcoEvoLand_A.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures/EcoEvoLand_A.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)


pdf("Figures/DDS.pdf", width=4, height=4)
source("Code/Figures/DDSLandscape.R")
dev.off()

file.copy(from="Figures/DDS.pdf", to="/home/yi/Dropbox/Apps/Overleaf/SocialFitnessEffects/Figures/DDS.pdf", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

