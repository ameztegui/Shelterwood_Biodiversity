mf_labeller <- function( value){
        value <- as.character(value)
        value[value=="seq_Basal"] <- "Basal Area (m2·ha-1) "
        value[value=="seq_Densi"] <- "Stem density (stems ha-1)"
        value[value=="seq_Quadr"] <- "Quadratic mean diameter (cm)"
        value[value=="seq_DHeig"] <- "Dominant height (m)"
        value[value=="seq_Cavit"] <- "Cavities per hectare"
        value[value=="seq_Shrub"] <- "Shrub cover (%)"
        value[value=="seq_Snags"] <- "Snags per hectare"
        value[value=="seq_Thick"] <- "Coarse wood"
        value[value=="seq_DVari"] <- "Irregularity (SD)"
        
        value[value=="TOTA"] <- "All species"
        value[value=="SPEA"] <- "Specialists"
        value[value=="UBIA"] <- "Ubiquitous"
        value[value=="GENA"] <- "Generalists"
        value[value=="OVNA"] <- "Canopy nesters"
        value[value=="CVNA"] <- "Cavity nesters"
        value[value=="GRNA"] <- "Ground nesters"
        value[value=="UNNA"] <- "Understory nesters"
        value[value=="OVFA"] <- "Canopy foragers"
        value[value=="TRFA"] <- "Trunk foragers"
        value[value=="GRFA"] <- "Ground foragers"
        value[value=="UNFA"] <- "Understory foragers"
        value[value=="NOCA"] <- "Non-dependents"
        value[value=="EXCA"] <- "Excavators"
        value[value=="SECA"] <- "Secondary users"

        return(value)}