rm(list=ls()) 

# Import fit and models depending on variables to plot

# Abundance
modelcomp_ab <- read.delim ("./Results/ModelComparison_Abu3.txt", dec=".", header=T)
parameters_ab <- read.delim ("./Results/ModelParameters_Abu3.txt", dec=".", header=T)
combine_ab<-merge(modelcomp_ab, parameters_ab, by=c("Variable","Model")) 
# Just the minimum AIC
ag_ab <- aggregate(AIC~Variable, data=combine_ab, min)  # Select best fit

# Select manually best model (takes into account parsimony)
ag_ab2<- data.frame(Variable = c("TOTA","SPEA","GENA", "UBIA",
                                 "OVNA", "CVNA", "GRNA", "UNNA",
                                 "OVFA","TRFA", "GRFA", "UNFA",
                                 "NOCA","EXCA", "SECA"),
                    Model = c("nomg.model","nomg.model", "full.model", "nosh.model",
                              "nomg.model", "nomg.model", "nosh.model", "nomg.model",
                              "full.model", "nosh.model", "nomg.model", "nomg.model",
                              "full.model","nosh.model", "nomg.model"))
#ag_ab <- combine_ab[combine_ab$Model=="full.model",]  # Select full.model
df.min_ab<-merge(ag_ab2,combine_ab)

# Richness
# modelcomp_ri <- read.delim ("./Results/ModelComparison_Rich.txt", dec=".", header=T)
# parameters_ri <- read.delim ("./Results/ModelParameters_Rich.txt", dec=".", header=T)
# combine_ri<-merge(modelcomp_ri, parameters_ri, by=c("Variable","Model")) 
# ag_ri <- aggregate(AIC~Variable, data=combine_ri, min)  # Select best fit
# #ag_ri <- combine_ri[combine_ri$Model=="full.model",]  # Select full.model
# 
# df.min_ri<-merge(ag_ri,combine_ri)



write.table(x=df.min_ab, file="./Results/BestModel_Abu3.txt",sep="\t",dec=".", row.names = F)
# write.table(x=df.min_ri, file="./Results/BestModel_Rich.txt",sep="\t",dec=".")

