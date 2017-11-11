#############################################################################
###
###     A set of alternative models to relate forest 
###     attributes (structure) to bird species richness and abundance
###   
###   
###     Aitor Ameztegui, UQAM
###     November 2015
#############################################################################


# Models -------------------------------------------------------

model_functions <- list(null_null_null_model <- function(PotRichness ) {
    PotRichness
},

#### Penta and tetra models ####

penta_complete_model <- function(PotRichness, Densi, Quadr, DHeig, Cavit, Shrub, 
                                 Densi_a, Densi_b , Quadr_a , Quadr_b,DHeig_a, DHeig_b, 
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Densi.effect * Quadr.effect * DHeig.effect *  Cavit.effect  * Shrub.effect
},

tetra_no_Shrub_model <- function(PotRichness, Densi, Quadr, DHeig, Cavit, 
                                 Densi_a, Densi_b , Quadr_a , Quadr_b,DHeig_a, DHeig_b, 
                                 Cavit_a, Cavit_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Densi.effect * Quadr.effect * DHeig.effect *  Cavit.effect 
},

tetra_no_Cavit_model <- function(PotRichness, Densi, Quadr, DHeig, Shrub, 
                                 Densi_a, Densi_b , Quadr_a , Quadr_b,DHeig_a, DHeig_b, 
                                  Shrub_a,Shrub_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Densi.effect * Quadr.effect * DHeig.effect * Shrub.effect
},

tetra_no_DHeig_model <- function(PotRichness, Densi, Quadr, Cavit, Shrub, 
                                 Densi_a, Densi_b , Quadr_a , Quadr_b,
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Densi.effect * Quadr.effect * Cavit.effect  * Shrub.effect
},

tetra_no_Quadr_model <- function(PotRichness, Densi, DHeig, Cavit, Shrub, 
                                 Densi_a, Densi_b, DHeig_a, DHeig_b, 
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Densi.effect * DHeig.effect *  Cavit.effect  * Shrub.effect
},

tetra_no_Densi_model <- function(PotRichness, Quadr, DHeig, Cavit, Shrub, 
                                 Quadr_a , Quadr_b,DHeig_a, DHeig_b, 
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Quadr.effect * DHeig.effect *  Cavit.effect  * Shrub.effect
},

tetra_complete_model <- function(PotRichness, Basal, DHeig, Cavit, Shrub, 
                                 Basal_a , Basal_b,DHeig_a, DHeig_b, 
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    Basal.effect <- exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Basal.effect * DHeig.effect *  Cavit.effect  * Shrub.effect
},


#### Trivariate models ####

no_Shrub_Cavit_model <- function(PotRichness, Densi, Quadr, DHeig, 
                                 Densi_a, Densi_b , Quadr_a , Quadr_b,DHeig_a, DHeig_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    PotRichness * Densi.effect * Quadr.effect * DHeig.effect
},
no_Shrub_DHeig_model <- function(PotRichness, Densi, Quadr, Cavit, 
                                Densi_a, Densi_b , Quadr_a , Quadr_b,
                                Cavit_a, Cavit_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Densi.effect * Quadr.effect * Cavit.effect
},

no_Shrub_Quadr_model <- function(PotRichness, Densi, DHeig, Cavit,
                                 Densi_a, Densi_b, DHeig_a, DHeig_b, 
                                 Cavit_a, Cavit_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Densi.effect * DHeig.effect *  Cavit.effect 
},

no_Shrub_Densi_model <- function(PotRichness, Quadr, DHeig, Cavit, 
                                 Quadr_a , Quadr_b,DHeig_a, DHeig_b, 
                                 Cavit_a, Cavit_b){
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Quadr.effect * DHeig.effect *  Cavit.effect
},

no_Cavit_DHeig_model <- function(PotRichness, Densi, Quadr, Shrub, 
                                 Densi_a, Densi_b , Quadr_a , Quadr_b,Shrub_a,Shrub_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Densi.effect * Quadr.effect * Shrub.effect
},
no_Cavit_Quadr_model <- function(PotRichness, Densi, DHeig, Shrub, 
                                 Densi_a, Densi_b, DHeig_a, DHeig_b, 
                                 Shrub_a,Shrub_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Densi.effect * DHeig.effect * Shrub.effect
},

no_Cavit_Densi_model <- function(PotRichness, Quadr, DHeig, Shrub, 
                                 Quadr_a , Quadr_b,DHeig_a, DHeig_b, 
                                 Shrub_a,Shrub_b){
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Quadr.effect * DHeig.effect * Shrub.effect
},

no_DHeig_Quadr_model <- function(PotRichness, Densi, Cavit, Shrub, 
                                 Densi_a, Densi_b , 
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Densi.effect * Cavit.effect * Shrub.effect
},

no_DHeig_Densi_model <- function(PotRichness, Quadr, Cavit, Shrub, 
                                 Quadr_a , Quadr_b,
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Quadr.effect * Cavit.effect * Shrub.effect
},

no_Quadr_Densi_model <- function(PotRichness, DHeig, Cavit, Shrub, 
                                 DHeig_a, DHeig_b, 
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * DHeig.effect * Cavit.effect * Shrub.effect
},

compl_no_Shrub_model <- function(PotRichness, Basal, DHeig, Cavit, 
                                Basal_a, Basal_b, DHeig_a, DHeig_b, 
                                Cavit_a, Cavit_b){
    Basal.effect <- exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Basal.effect * DHeig.effect * Cavit.effect 
},
compl_no_Cavit_model <- function(PotRichness, Basal, DHeig, Shrub, 
                                 Basal_a, Basal_b, DHeig_a, DHeig_b, 
                                  Shrub_a,Shrub_b){
    Basal.effect <- exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Basal.effect * DHeig.effect * Shrub.effect
},
compl_no_DHeig_model <- function(PotRichness, Basal,  Cavit, Shrub, 
                                 Basal_a, Basal_b, 
                                 Cavit_a, Cavit_b, Shrub_a,Shrub_b){
    Basal.effect <- exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Basal.effect * Cavit.effect  * Shrub.effect
},


#### Bivariate models ####

bi_Densi_Quadr_model <- function(PotRichness, Densi, Quadr, 
                                 Densi_a, Densi_b , Quadr_a , Quadr_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    
    PotRichness * Densi.effect * Quadr.effect
},
bi_Densi_DHeig_model <- function(PotRichness, Densi, DHeig,
                                 Densi_a, Densi_b ,DHeig_a, DHeig_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    PotRichness * Densi.effect * DHeig.effect
},
bi_Densi_Cavit_model <- function(PotRichness, Densi, Cavit, 
                                 Densi_a, Densi_b,Cavit_a, Cavit_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Densi.effect *  Cavit.effect
},
bi_Densi_Shrub_model <- function(PotRichness, Densi, Shrub, 
                                 Densi_a, Densi_b, Shrub_a,Shrub_b){
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Densi.effect * Shrub.effect
},
bi_Quadr_DHeig_model <- function(PotRichness,  Quadr, DHeig,
                                 Quadr_a , Quadr_b,DHeig_a, DHeig_b){
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    PotRichness * Quadr.effect * DHeig.effect
},
bi_Quadr_Cavit_model <- function(PotRichness, Quadr, Cavit,
                                 Quadr_a , Quadr_b, Cavit_a, Cavit_b){
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Quadr.effect * Cavit.effect
},
bi_Quadr_Shrub_model <- function(PotRichness, Quadr, Shrub, 
                                 Quadr_a , Quadr_b, Shrub_a,Shrub_b){
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Quadr.effect * Shrub.effect
},
bi_DHeig_Cavit_model <- function(PotRichness,DHeig, Cavit, 
                                 DHeig_a, DHeig_b, Cavit_a, Cavit_b){
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * DHeig.effect * Cavit.effect
},
bi_DHeig_Shrub_model <- function(PotRichness, DHeig, Shrub, 
                                 DHeig_a, DHeig_b, Shrub_a, Shrub_b){
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * DHeig.effect * Shrub.effect
},

bi_Cavit_Shrub_model <- function(PotRichness, Cavit, Shrub, 
                                 Cavit_a, Cavit_b, Shrub_a, Shrub_b){
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Cavit.effect * Shrub.effect
},
bi_Basal_DHeig_model <- function(PotRichness, Basal, DHeig,
                                 Basal_a , Basal_b,DHeig_a, DHeig_b){
    Basal.effect <- exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    PotRichness * Basal.effect * DHeig.effect
},

bi_Basal_Cavit_model <- function(PotRichness, Basal, Cavit,
                                 Basal_a , Basal_b, Cavit_a, Cavit_b){
    Basal.effect <- exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Basal.effect * Cavit.effect
},
bi_Basal_Shrub_model <- function(PotRichness, Basal, Shrub, 
                                 Basal_a , Basal_b, Shrub_a, Shrub_b){
    Basal.effect <- exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Basal.effect * Shrub.effect
},


#### Univariate models ####
    
univaria_Basal_model <- function(PotRichness, Basal, Basal_a, Basal_b ) {
    Basal.effect <- exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
    PotRichness * Basal.effect
}, 
univaria_Densi_model <- function(PotRichness, Densi, Densi_a, Densi_b ) {
    Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
    PotRichness * Densi.effect
},
univaria_Quadr_model <- function(PotRichness, Quadr, Quadr_a, Quadr_b ) {
    Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
    PotRichness * Quadr.effect
},
univaria_DHeig_model <- function(PotRichness, DHeig, DHeig_a, DHeig_b ) {
    DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
    PotRichness * DHeig.effect
},
univaria_Cavit_model <- function(PotRichness, Cavit, Cavit_a, Cavit_b ) {
    Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
    PotRichness * Cavit.effect
},
univaria_Shrub_model <- function(PotRichness, Shrub, Shrub_a, Shrub_b ) {
    Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
    PotRichness * Shrub.effect
},

univaria_DVari_model <- function(PotRichness, DVari, DVari_a, DVari_b ) {
    DVari.effect <- exp(-0.5*((DVari-DVari_a)/DVari_b)^2)
    PotRichness * DVari.effect
},

univaria_Snags_model <- function(PotRichness, Snags, Snags_a, Snags_b ) {
    Snags.effect <- exp(-0.5*((Snags-Snags_a)/Snags_b)^2)
    PotRichness * Snags.effect
},

univaria_Herba_model <- function(PotRichness, Herba, Herba_a, Herba_b ) {
    Herba.effect <- exp(-0.5*((Herba-Herba_a)/Herba_b)^2)
    PotRichness * Herba.effect
},

univaria_Thick_model <- function(PotRichness, Thick, Thick_a, Thick_b ) {
    Thick.effect <- exp(-0.5*((Thick-Thick_a)/Thick_b)^2)
    PotRichness * Thick.effect
})


####################################
#  Alternate PDF's
####################################

pdf.gauss <- function(x,mean,sigma,log=TRUE)
{  sd <- sigma
   dnorm(x,mean,sd,log=T)}

pdf.gauss.prop <- function(x,mean,lambda)
{  sd <- mean*lambda
   dnorm(x,mean,sd,log=T)}

pdf.gauss.power <- function(x,mean,sigma)
{  sd <- mean^sigma
   dnorm(x,mean,sd,log=T)}

pdf.gamma<- function(x,mean, scale, log=FALSE)
{ shape <- mean/scale
  dgamma(x,shape=shape,scale=scale,log)}

pdf.poisson <- function(x, lambda)
{dpois(x,lambda, log=T)}

