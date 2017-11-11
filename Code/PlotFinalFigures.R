#################################################################################
##### Plots the best fit model for each group of species and explanatory variable
#####
#####           Aitor Ameztegui (UQAM)
#####                   January 2015
#################################################################################

# Create plots ------------------------------------------------------------

rm(list=ls()) 
library(gridExtra)
library(PMCMR)

## Densi and Quadr family ####

model_group <- "Densi"
source("./Code/PlotScript.R")


blankPlot <- ggplot()+geom_blank(aes(1,1))+
    theme(
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
    )

box_Basal <- figura_box("Basal", seq_Basal)
box_Densi <- figura_box("Densi", seq_Densi)
box_Quadr <- figura_box("Quadr", seq_Quadr)
box_Basal <- figura_box("Basal", seq_Basal)
box_DHeig <- figura_box("DHeig", seq_DHeig)
box_Cavit <- figura_box("Cavit", seq_Cavit)
box_Shrub <- figura_box("Shrub", seq_Shrub)

# Habitat breadth
habit_Densi <- figura_rel(habitat, seq_Densi,"none")
habit_Quadr <- figura_rel(habitat, seq_Quadr,"none")
habit_DHeig <- figura_rel(habitat, seq_DHeig,"none")
habit_Cavit <- figura_rel(habitat, seq_Cavit,"none")
habit_Shrub <- figura_rel(habitat, seq_Shrub,  c(0.2,0.21))


# Nesting habitat
nesting_Densi <- figura_rel(nesting, seq_Densi,"none")
nesting_Quadr <- figura_rel(nesting, seq_Quadr,"none")
nesting_DHeig <- figura_rel(nesting, seq_DHeig,"none")
nesting_Cavit <- figura_rel(nesting, seq_Cavit,"none")
nesting_Shrub <- figura_rel(nesting, seq_Shrub,c(0.23,0.21))

# Foraging habitat
foraging_Densi <- figura_rel(foraging, seq_Densi,"none")
foraging_Quadr <- figura_rel(foraging, seq_Quadr,"none")
foraging_DHeig <- figura_rel(foraging, seq_DHeig,"none")
foraging_Cavit <- figura_rel(foraging, seq_Cavit,"none")
foraging_Shrub <- figura_rel(foraging, seq_Shrub,c(0.24,0.21))

# boxplots
box_hab <- figura_box2(habitat)
box_nest <- figura_box2(nesting)
box_forag <- figura_box2(foraging)



pdf("./Figures/Final/Fig3b_Habitat_Breadth.pdf",width=10,height=15.5)
grid.arrange(box_Densi,box_Quadr,
             habit_Densi, habit_Quadr,
             blankPlot, blankPlot,
             box_DHeig,box_Cavit,
             habit_DHeig, habit_Cavit, 
             blankPlot, blankPlot,
             box_Shrub, blankPlot,
             habit_Shrub, blankPlot,
             ncol = 2,nrow=8,
             widths= c(4,4), heights = c(1.6, 5, 0.1, 1.6, 5, 0.1, 1.6, 5))
dev.off()


pdf("./Figures/Final/Fig4b_Nesting_Habitat.pdf",width=10,height=15.5)
grid.arrange(box_Densi,box_Quadr,
             nesting_Densi, nesting_Quadr,
             blankPlot, blankPlot,
             box_DHeig,box_Cavit,
             nesting_DHeig, nesting_Cavit, 
             blankPlot, blankPlot,
             box_Shrub, blankPlot,
             nesting_Shrub, blankPlot,
             ncol = 2,nrow=8,
             widths= c(4,4), heights = c(1.6, 5, 0.1, 1.6, 5, 0.1, 1.6, 5))
dev.off()

pdf("./Figures/Final/Fig5b_Foraging_Habitat.pdf",width=10,height=15.5)
grid.arrange(box_Densi,box_Quadr,
             foraging_Densi, foraging_Quadr,
             blankPlot, blankPlot,
             box_DHeig,box_Cavit,
             foraging_DHeig, foraging_Cavit, 
             blankPlot, blankPlot,
             box_Shrub, blankPlot,
             foraging_Shrub, blankPlot,
             ncol = 2,nrow=8,
             widths= c(4,4), heights = c(1.6, 5, 0.1, 1.6, 5, 0.1, 1.6, 5))
dev.off()



#### Other figures #####


pdf("./Figures/Final/Fig2b_Habitat_boxplot.pdf",width=6.5,height=6.5)
box_hab
dev.off()

pdf("./Figures/Final/FigSM_A1b_Nesting_boxplot.pdf",width=6.5,height=6.5)
box_nest
dev.off()

pdf("./Figures/Final/FigSM_A2b_Foraging_boxplot.pdf",width=6.5,height=6.5)
box_forag
dev.off()


pdf("./Figures/Final/bFigSM_A3_Biv_Habitat_Breadth.pdf",width=19,height=19)
figura_3D(depvar=c("GENA","UBIA","TOTA", "SPEA"), seq_DHeig, seq_Densi, -35, -80, c(2,2))     
dev.off()

pdf("./Figures/Final/bFigSM_A4__Bivariate_Nesting_Habitat.pdf",width=19,height=19)
figura_3D(depvar=c("GRNA","UNNA","OVNA","CVNA"), seq_DHeig, seq_Densi, -35, -80, c(2,2))     
dev.off()


pdf("./Figures/Final/bFigSM_A5_Biv_Foraging_Habitat.pdf",width=19,height=19)
figura_3D(depvar=c("GRFA","UNFA","OVFA","TRFA"), seq_DHeig, seq_Densi, -35, -80, c(2,2))     
dev.off()


fit <- kruskal.test(TOTA ~Forest_Type, data = ocells)
print(fit)
posthoc.kruskal.nemenyi.test(x=ocells$GRFA, g=ocells$Forest_Type,method="Tukey")

