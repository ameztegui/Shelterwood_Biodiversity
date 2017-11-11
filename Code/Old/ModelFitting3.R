#################################################################
#
#      Adjustment of several models for effect of stand-level 
#      dasometric variables on birds richness and abundance
# 
#################################################################

# NOTE: this script includes a double loop to authomatize the process and determine
# the effect of PPFD on (h) several variables using (j) several models


rm(list=ls()) 
library(likelihood)
library(psych)

# Subset the dataset to the desired sample
#ocells <- read.table("./Data/data_analyze_unc_rich_abun.txt", header=T, quote="\"")

ocells <- read.csv("./Data/data_analyze_unc_rich_abun_may_2016.csv")
# ocells <- ocells[ocells$Especie=="Uncinata",]


# Rename the variables so that it is easier to automathize processes (two letters each)
ocells$HO <- ocells$ALCDOM
ocells$BA <- ocells$G_m2_ha
ocells$DW <- ocells$PEUSMHA
ocells$MG <- ocells$p_G_gruesa
ocells$CV<- ocells$CAVTOTAL  #Not included (lacking on P.sylvestris database)
ocells$SH <- ocells$RECARBUSTIU
ocells$SD <- ocells$SD_maderaFMG


# Know the frequency (#plots with presence) and
# the abundance (total # of presences) per species

sapply(ocells[,5:37],function(x) length(x[x>0]))        # frequency
sapply(ocells[,5:37],function(x) sum(x))                #abundance

# Know the maximum abundance and richness per group in a plot
sapply(ocells[,57:82],max)        # maximum

# Describe the predictors
describe(ocells[,c(4,38:55)])

# Check correlation between variables
corr.test(ocells[c("DT","HO","BA", "DW", "MG", "CV", "SH", "SD")], method = "spearman")
fit<-princomp(ocells[c("DT","HO","BA", "DW", "MG", "CV", "SH", "SD")], rotation="varimax")
summary(fit)
loadings(fit)
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit,choices = c(1,2))

# Load the dependent variables (create a vector that will be called later)


#### Habitat Breadth: specialists (SPE), generalists (GEN), ubiquituous (UBI)
#### Nesting habitat: overstory (OVN), tree cavity (CVN), ground (GRN), understory (UNN)
#### Foraging habitat: overstiry (OVF), trunk (TRF), ground (GRF), understiry (UNF)
#### Cavity dependence: non-dependent (NOC), excavators (EXC), secondary users (SEC)

depend.gr<-  c ("TOTA","SPEA", "GENA", "UBIA",
                "OVNA", "CVNA", "GRNA", "UNNA",
                "OVFA", "TRFA", "GRFA", "UNFA",
                "NOCA", "EXCA", "SECA")




depend.var<-depend.gr



# Load the models to run (create a vector that will be called later)

modelname<- c("pool.full.model", "pool.noho.model",  "pool.nodw.model",
              "pool.nomg.model", "pool.nosh.model","pool.null.model")


# CREATE THE DOUBLE LOOP THAT WILL AUTHOMATIZE THE PROCESS

for (h in 1: length(depend.var))   {
        
        for (j in 1: length(modelname))  {       
                
                # drop any observations containing missing values for the independent or dependent variables
                #                 attach (ocells)
                #                 ocells<-na.omit (data.frame(Plot, HO, CV, DW, FG, Shrub, SD,
                #                                             ESPA, ESPR, GENA, GENR,GRIA, GRIR, SOTA, SOTR,
                #                                             OBEA, OBER,TOTA, TOTR ))
                #                 detach (ocells)
                data <- ocells
                
                ######################################################
                #  LOAD THE SCIENTIFIC MODELS
                ######################################################
                
                source("./Code/ScientificModels3.R")
                source("./Code/Parameter_Vectors3.R")
                
                ###########################################
                #  ANNEALING ALGORITHM
                ###########################################
                
                # Define the model and dependent variable to use (anneal procedure doesn't 
                # accept subindices, so we must define the value of the variables before)
                
                model <- get(modelname[j])
                dep.var <- depend.var[h]
                
                print(dep.var)
                print(modelname[j])
                
                
                # Define the parameters to use, as a function of the model running
                
                if (isTRUE(grepl("pool", modelname[j])))  {
                        
                        if (isTRUE(grepl("full", modelname[j]))) 
                        {par<-pool.full.par
                        par_lo<-pool.full.par.lo
                        par_hi<-pool.full.par.hi}
                        
                        if (isTRUE(grepl("noho", modelname[j]))) 
                        {par<-pool.noho.par
                        par_lo<-pool.noho.par.lo
                        par_hi<-pool.noho.par.hi}
                        
                        if (isTRUE(grepl("nodw", modelname[j]))) 
                        {par<-pool.nodw.par
                        par_lo<-pool.nodw.par.lo
                        par_hi<-pool.nodw.par.hi}
                        
                        if (isTRUE(grepl("nomg", modelname[j]))) 
                        {par<-pool.nomg.par
                        par_lo<-pool.nomg.par.lo
                        par_hi<-pool.nomg.par.hi}
                        
                        if (isTRUE(grepl("nosh", modelname[j]))) 
                        {par<-pool.nosh.par
                        par_lo<-pool.nosh.par.lo
                        par_hi<-pool.nosh.par.hi}
                        
                        if (isTRUE(grepl("null", modelname[j]))) 
                        {par<-pool.null.par
                        par_lo<-pool.null.par.lo
                        par_hi<-pool.null.par.hi}
                }
                
                
                
                
                #                 if (isTRUE(grepl("site", modelname[j])))  {
                #                         
                #                         if (isTRUE(grepl("full", modelname[j]))) 
                #                         { par<-site.full.par
                #                           par_lo<-site.full.par.lo
                #                           par_hi<-site.full.par.hi}
                #                         
                #                         if (isTRUE(grepl("noho", modelname[j]))) 
                #                         { par<-site.noho.par
                #                           par_lo<-site.noho.par.lo
                #                           par_hi<-site.noho.par.hi}
                #                         
                #                         if (isTRUE(grepl("nodw", modelname[j]))) 
                #                         { par<-site.nodw.par
                #                           par_lo<-site.nodw.par.lo
                #                           par_hi<-site.nodw.par.hi}
                #                         
                #                         if (isTRUE(grepl("nodw", modelname[j]))) 
                #                         { par<-site.nodw.par
                #                           par_lo<-site.nodw.par.lo
                #                           par_hi<-site.nodw.par.hi}
                #                         
                #                         if (isTRUE(grepl("nomg", modelname[j]))) 
                #                         { par<-site.nomg.par
                #                           par_lo<-site.nomg.par.lo
                #                           par_hi<-site.nomg.par.hi}
                #                         
                #                         if (isTRUE(grepl("nodw", modelname[j]))) 
                #                         { par<-site.nodw.par
                #                           par_lo<-site.nodw.par.lo
                #                           par_hi<-site.nodw.par.hi}
                #                         
                #                         if (isTRUE(grepl("nosh", modelname[j]))) 
                #                         { par<-site.nosh.par
                #                           par_lo<-site.nosh.par.lo
                #                           par_hi<-site.nosh.par.hi}
                #                         
                #                         if (isTRUE(grepl("nosd", modelname[j]))) 
                #                         { par<-site.nosd.par
                #                           par_lo<-site.nosd.par.lo
                #                           par_hi<-site.nosd.par.hi}
                #                         
                #                         if (isTRUE(grepl("null", modelname[j]))) 
                #                         { par<-site.null.par
                #                           par_lo<-site.null.par.lo
                #                           par_hi<-site.null.par.hi}
                #                 }
                
                
                # NOVELTY: substitute PotRichness by actual max value                
                par$PotRichness = 0.8*max(data[dep.var])
                par_lo$PotRichness = max(data[dep.var]) 
                par_hi$PotRichness = max(data[dep.var])*3
                
                
                var <- list(mean = "predicted", x=depend.var[h],HO=data$HO, DW=data$DW, 
                            MG=data$MG, SH=data$SH, log=T)
                
                
                
                #now call the annealing algorithm, specifying the proper model
                results<-anneal(model,par,var,data,par_lo,par_hi,pdf=dnorm,
                                dep.var,hessian = TRUE, max_iter=20000)                        
                write_results(results,paste("./Results/Model Fits/Abu3/",dep.var,"_","PIUNs","_",modelname[j],".txt", sep=""))
        }
}

## display some of the results in the console

#results$best_pars
#results$max_likeli;
#results$aic_corr ;
#results$slope;
#results$R2
