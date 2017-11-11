#################################################################################
##### Plots the best fit model for each group of species and explanatory variable
#####
#####           Aitor Ameztegui (UQAM)
#####                   January 2015
#################################################################################



# Create plots ------------------------------------------------------------


rm(list=ls()) 
## Abundance ####

respuesta <- "abundance"

source("./Code/PlotScript3.R")



pdf("./Figures/Final/Fig2_Habitat_Breadth3.pdf",width=7,height=7)
par(mfrow=c(2,2), mar=c(4,4,1,1))
figura_rel(habitat, seqHO,"none")
figura_rel(habitat, seqMG, "none")
figura_rel(habitat, seqDW, "none")
figura_rel(habitat, seqSH, "bottomleft")
dev.off()

pdf("./Figures/Final/Fig3_Nesting_Habitat3.pdf",width=7,height=7)
par(mfrow=c(2,2), mar=c(4,4,1,1))
figura_rel(nesting, seqHO,  "none")
figura_rel(nesting, seqMG)
figura_rel(nesting, seqDW)
figura_rel(nesting, seqSH, "bottomleft")
dev.off()

pdf("./Figures/Final/FigSM_A1_Foraging_Habitat3.pdf",width=7,height=7)
par(mfrow=c(2,2), mar=c(4,4,1,1))
figura_rel(foraging, seqHO, "none")
figura_rel(foraging, seqMG)
figura_rel(foraging, seqDW)
figura_rel(foraging, seqSH,"bottomleft")
dev.off()


pdf("./Figures/Final/FigSM_A2_Biv_Habitat_Breadth3.pdf",width=24,height=8)
figura_3D(depvar=c("GENA","UBIA", "SPEA"), seqHO, seqMG, -35, -80, c(3,1))     
dev.off()

pdf("./Figures/Final/Fig4_Bivariate_Nesting_Habitat3.pdf",width=16,height=16)
figura_3D(depvar=c("GRNA","UNNA","OVNA","CVNA"), seqHO, seqMG, -35, -80, c(2,2))     
dev.off()


pdf("./Figures/Final/FigSM_A3_Biv_Foraging_Habitat3.pdf",width=16,height=16)
figura_3D(depvar=c("GRFA","UNFA","OVFA","TRFA"), seqHO, seqMG, -35, -80, c(2,2))     
dev.off()



