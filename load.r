require(colorRamps)
require(dplyr)
require(MASS)
require(ggplot2)
library(parallel)
require(lattice)
library(latticeExtra)
require(gridExtra)
require(pander)
require(xtable)
library(RColorBrewer)
library(boot)
require(scales)
require(lme4)

#setwd("/home/yi/Dropbox/SocialFitnessEffects")
source("Code/Functions/Analyze.R")
source("Code/Functions/Simulate.R")
source("Code/Functions/FiguresFunctions.R")

b<-create.x()
r_max<-b$r_max
gamma<-b$gamma
Bq<--gamma/2
Bn<-b$Bn
p<-inv.logit(b$s)
n.years<-b$n.years
n.sims=100

theta=c(0,2)
thetas= c(rep(theta[1], each=50), rep(theta[2], each=200)) 
B0  <-  r_max - gamma / 2 * (theta[2] - 0)^2

