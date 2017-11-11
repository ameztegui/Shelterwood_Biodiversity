rm(list=ls()) 

# Import fit and models depending on variables to plot

# Abundance
modelcomp <- read.delim ("./Results/ModelComparison.txt", dec=".", header=T)
parameters <- read.delim ("./Results/ModelParameters.txt", dec=".", header=T)
combine<-merge(modelcomp, parameters, by=c("Variable","Model")) 


# Select manually best model (takes into account parsimony)

    # Group 1: includes Densi and Quadr, but not Basal
    best_model1<- data.frame(Variable = c("TOTA","SPEA","GENA", "UBIA",
                                     "OVNA", "CVNA", "GRNA", "UNNA",
                                     "OVFA","TRFA", "GRFA", "UNFA"),
                        Model = c("penta_complete", "tetra_no_Cavit", "penta_complete", "bi_Densi_DHeig",
                                  "no_Shrub_Quadr", "penta_complete", "no_Shrub_DHeig", "tetra_no_Cavit",
                                  "no_Cavit_Quadr", "univaria_DHeig", "no_Shrub_DHeig", "no_Cavit_Densi"))
    best_model1<-left_join(best_model1,combine)
    
    # Group 2: includes Basal, but not Densi and Quadr
    best_model2<- data.frame(Variable = c("TOTA","SPEA","GENA", "UBIA",
                                          "OVNA", "CVNA", "GRNA", "UNNA",
                                          "OVFA","TRFA", "GRFA", "UNFA"),
                             Model = c("compl_no_Cavit", "tetra_complete", "tetra_complete", "bi_Basal_Cavit",
                                       "bi_Basal_DHeig", "tetra_complete", "bi_Basal_Cavit", "compl_no_Cavit", 
                                       "compl_no_Cavit", "univaria_DHeig", "tetra_complete", "compl_no_Cavit"))
    best_model2<-left_join(best_model2,combine)


write.table(x=best_model1, file="./Results/BestModel_Densi.txt",sep="\t",dec=".", row.names = F)
write.table(x=best_model2, file="./Results/BestModel_Basal.txt",sep="\t",dec=".", row.names = F)

