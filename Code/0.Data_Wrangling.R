
# Load packages
rm(list=ls()) 
library(psych)
library(tidyverse)

# Load the dataset
ocells <- read.csv("./Data/data_analyze_unc_rich_abun_may_2016.csv", sep=";")

# Rename the variables so that it is easier to automathize processes (five letters each)
ocells <- ocells %>%
    rename(Densi = DT, #stem density
           DHeig = ALCDOM, # dominant height
           Basal = G_m2_ha, # basal area
           Snags = PEUSMHA,  # snags
           Thick = p_G_gruesa, # thickwood
           Cavit = CAVTOTAL,  # cavities
           Shrub = RECARBUSTIU, # shrub cover
           Herba = RECHERB, # herbaceous cover
           DVari = SD_maderaFMG) %>% # variability in diameter size
    mutate(Quadr = sqrt(Basal/(0.0000785*Densi))) # quadratic mean diameter

ocells <- ocells %>%
    mutate(Forest_Type = fct_recode(STRUCTURE,
                                    "Unman"    = "BV", 
                                    "Prep" = "FM",
                                    "Regen" = "TD",
                                    "Remov" = "TF"))

ocells$Forest_Type <- ordered(ocells$Forest_Type, 
                              levels = c("Remov","Regen","Prep","Unman"))

# Reclassify forest species in guilds -------------------------------------

ocells <- ocells %>%
    mutate(
        ### HABITAT BREADTH
        GENA = COPA + ANTR + PRMO + TROG + ERRU + TUTO + TUME + TUVI + GAGL + FRCO + SECI,
        SPEA = DRMA + DEMA + TUPH + SYBO + SYAT + PHBO + PHCO + REGU + REIG + AECA +
               PACR + PAAT + PAMA + SIEU + CEFA + CEBR + LOCU + PYRA,
        UBIA = PIVI + LUAR + PHOC + EMCA,

        ### NEST LOCATION
        OVNA = COPA + TUPH + TUVI + REGU + REIG + GAGL + FRCO + SECI + LOCU + PYRA,
        CVNA = PIVI + DRMA + DEMA + PACR + PAAT + PAMA + SIEU + CEFA + CEBR,
        GRNA = LUAR + ANTR + PHOC + TUTO + PHBO + EMCA,
        UNNA = PRMO + TROG + ERRU + TUME + SYBO + SYAT + PHCO + AECA,

        ### FORAGING BEHAVIOUR
        OVFA = PHBO + PHCO + REGU + REIG + AECA + PACR + PAAT + PAMA + GAGL + FRCO + SECI + LOCU + PYRA,
        TRFA = PIVI + DRMA + DEMA + SIEU + CEFA + CEBR,
        GRFA = COPA + LUAR + ANTR + PHOC + TUTO + TUME + TUPH + TUVI + EMCA,
        UNFA = PRMO + TROG + ERRU + SYBO + SYAT)

# Check the number of observations per guild
guilds <- c ("TOTA", "GENA", "SPEA", "UBIA", 
             "OVNA", "CVNA", "UNNA", "GRNA",
             "OVFA", "TRFA", "UNFA", "GRFA")
summary <- ocells %>%
    summarise_at(.vars = guilds,
                 .funs= c(mean = "mean", sd = "sd")) %>%
    gather()
                      
        
sink("./Data/guild_abun.txt")
print(summary)
sink()

# Save tidy dataset
#save(ocells, file="./Data/tidy_ocells.Rdata")

      
# Descriptive statistics ----------------------------------------------------------------------

# Describe the predictors
predictors <-c("Densi", "Basal", "Quadr", "DHeig", 
               "Snags", "Cavit",  "Herba","Shrub","Thick", "DVari")

summary(ocells[,predictors])

# Description by type of forest
(ocells %>%
         group_by(STRUCTURE) %>%
         summarise(Densi = mean (Densi), Basal=mean(Basal), 
                   Quadr=mean(Quadr), DHeig=mean(DHeig), 
                   Snags=mean(Snags), Cavit=mean(Cavit),
                   Herba=mean(Herba), Shrub= mean(Shrub), 
                   Thick=mean(Thick), SD= mean(DVari)))


# Check correlation between variables
corr.test(ocells[predictors], method = "spearman")

# Check the effect of forest structure on basal area and mean quadratic diameter
boxplot(Basal ~ STRUCTURE, data=ocells)
TukeyHSD(aov(Basal ~ STRUCTURE, data=ocells))

boxplot(Quadr ~ STRUCTURE, data=ocells)
TukeyHSD(aov(Quadr ~ STRUCTURE, data=ocells))

# Know the frequency (#plots with presence) and
# the abundance (total # of presences) per species
sapply(ocells[,26:58],function(x) length(x[x>0]))        # frequency
sapply(ocells[,26:58],function(x) sum(x))                #abundance

names(ocells)

