#############################################################################
###
###     A set of alternative models to relate forest 
###     attributes (structure) to bird species richness and abundance
###   
###   
###     Aitor Ameztegui, UQAM
###     November 2014
#############################################################################


########### Data pooled together

# Full model
pool.full.model <- function(PotRichness,HOa, HOb, DWa, DWb, MGa, MGb, SHa,SHb,
                            HO,DW, MG,SH)
{
        HO.effect <- exp(-0.5*((HO-HOa)/HOb)^2)
        DW.effect <- exp(-0.5*((DW-DWa)/DWb)^2)
        MG.effect <- exp(-0.5*((MG-MGa)/MGb)^2)
        SH.effect <- exp(-0.5*((SH-SHa)/SHb)^2)
        PotRichness * HO.effect *  DW.effect *  MG.effect * SH.effect
}


pool.noho.model <- function(PotRichness,DW,MG,SH,
                            DWa, DWb, MGa, MGb, SHa,SHb)
{
        DW.effect <- exp(-0.5*((DW-DWa)/DWb)^2)
        MG.effect <- exp(-0.5*((MG-MGa)/MGb)^2)
        SH.effect <- exp(-0.5*((SH-SHa)/SHb)^2)
        PotRichness *  DW.effect *  MG.effect * SH.effect
}


pool.nodw.model <- function(PotRichness,HO,MG,SH,
                            HOa, HOb,  MGa, MGb, SHa,SHb)
{
        HO.effect <- exp(-0.5*((HO-HOa)/HOb)^2)
        MG.effect <- exp(-0.5*((MG-MGa)/MGb)^2)
        SH.effect <- exp(-0.5*((SH-SHa)/SHb)^2)
        PotRichness * HO.effect * MG.effect * SH.effect
}


pool.nomg.model <- function(PotRichness,HO, DW,SH,
                            HOa, HOb, DWa, DWb,SHa,SHb)
{
        HO.effect <- exp(-0.5*((HO-HOa)/HOb)^2)
        DW.effect <- exp(-0.5*((DW-DWa)/DWb)^2)
        SH.effect <- exp(-0.5*((SH-SHa)/SHb)^2)
        PotRichness * HO.effect * DW.effect * SH.effect
}

pool.nosh.model <- function(PotRichness,HO,DW,MG,
                            HOa, HOb,DWa, DWb,MGa, MGb)
{
        HO.effect <- exp(-0.5*((HO-HOa)/HOb)^2)
        DW.effect <- exp(-0.5*((DW-DWa)/DWb)^2)
        MG.effect <- exp(-0.5*((MG-MGa)/MGb)^2)
        PotRichness * HO.effect * DW.effect * MG.effect
}

pool.null.model <- function(PotRichness )
{
        PotRichness
}


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

