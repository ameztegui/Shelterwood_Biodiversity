#################################################################
#
#      Adjustment of several models for effect of stand-level 
#      dasometric variables on birds abundance
#
# ################################################################

# Load packages
rm(list=ls()) 

library(likelihood)
library(dplyr)

# Load the dataset
load("./Data/tidy_ocells.Rdata")

# Fit the models ------------------------------------------------------------------------------

# Load the dependent variables (create a vector that will be called later)

#### Habitat Breadth: specialists (SPE), generalists (GEN), ubiquituous (UBI)
#### Nesting habitat: overstory (OVN), tree cavity (CVN), ground (GRN), understory (UNN)
#### Foraging habitat: overstiry (OVF), trunk (TRF), ground (GRF), understory (UNF)
#### Cavity dependence: non-dependent (NOC), excavators (EXC), secondary users (SEC)

mle_function <- function(i,j) {
    depend.gr<-  c ("TOTA","SPEA", "GENA", "UBIA",
                    "OVNA", "CVNA", "GRNA", "UNNA",
                    "OVFA", "TRFA", "GRFA", "UNFA")
    
    source("./Code/ScientificModels.R")
    source("./Code/Parameter_Vectors.R")
    
    # Load the models to run (create a vector that will be called later)
    
    modelname <- c("null_null_null_model",
                   ## Penta and tetravariate models
                   "penta_complete_model", 
                   "tetra_no_Shrub_model", "tetra_no_Cavit_model", "tetra_no_DHeig_model", 
                   "tetra_no_Quadr_model", "tetra_no_Densi_model", "tetra_complete_model", 
                   
                   ## Trivariates
                   "no_Shrub_Cavit_model", "no_Shrub_DHeig_model", "no_Shrub_Quadr_model", 
                   "no_Shrub_Densi_model","no_Cavit_DHeig_model", "no_Cavit_Quadr_model", 
                   "no_Cavit_Densi_model","no_DHeig_Quadr_model", "no_DHeig_Densi_model", 
                   "no_Quadr_Densi_model","compl_no_Shrub_model", "compl_no_Cavit_model", 
                   "compl_no_DHeig_model",
                  
                  ## Bivariate models
                  "bi_Densi_Quadr_model", "bi_Densi_DHeig_model", "bi_Densi_Cavit_model", "bi_Densi_Shrub_model",
                  "bi_Quadr_DHeig_model", "bi_Quadr_Cavit_model", "bi_Quadr_Shrub_model", 
                  "bi_DHeig_Cavit_model", "bi_DHeig_Shrub_model", "bi_Cavit_Shrub_model",
                  "bi_Basal_DHeig_model", "bi_Basal_Cavit_model", "bi_Basal_Shrub_model",
                  
                  ## Univariate_models
                  "univaria_Basal_model", "univaria_Densi_model", "univaria_Quadr_model",
                  "univaria_DHeig_model", "univaria_Cavit_model", "univaria_Shrub_model", 
                  "univaria_Herba_model", "univaria_DVari_model", "univaria_Snags_model",
                  "univaria_Thick_model")
    
        
    depend.var<-depend.gr[[i]]
    model = model_functions[[j]]
    
    print(depend.var)
    print(modelname[j])
    
    # Parameter list (for effect of stage)
    par = par_model[[j]]
    par_hi = par_high[[j]]
    par_lo  = par_low[[j]]
    
    
    # NOVELTY: substitute PotRichness by actual max value                
    par_lo$PotRichness = 0.8*max(ocells[depend.var])
    par$PotRichness = max(ocells[depend.var]) 
    par_hi$PotRichness = max(ocells[depend.var])*3
    
                    
    var <- list(mean = "predicted", x=depend.var,Densi = ocells$Densi,
                DHeig=ocells$DHeig, Basal = ocells$Basal, Snags = ocells$Snags,
                Thick = ocells$Thick, Cavit = ocells$Cavit, Shrub = ocells$Shrub,
                Herba = ocells$Herba, DVari = ocells$DVari, Quadr = ocells$Quadr, log=T)
            
                    
    #now call the annealing algorithm, specifying the proper model
    results<-anneal(source_data=ocells,model = model, var = var,
                    par = par, par_lo=par_lo, par_hi=par_hi,
                    pdf=dnorm, depend.var,hessian = TRUE, max_iter=60000)                        
    
    write_results(results,paste("./Results/Model Fits/",depend.var,"_",modelname[j],".txt", sep=""))
        
}


# Penta and tetravariates
for (i in 1:12){
    for (j in 1:7){
        mle_function(i,j)
    }
}
# Trivariate
for (i in 1:12){
    for (j in 8:20){
        mle_function(i,j)
    }
}
# Bivariate
for (i in 1:12){
    for (j in 21:33){
        mle_function(i,j)
    }
}

# Univariate
for (i in 1:12){
    for (j in 41:44){
        mle_function(i,j)
    }
}

## display some of the results in the console

#results$best_pars
#results$max_likeli;
#results$aic_corr ;
#results$slope;
#results$R2

