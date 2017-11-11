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
pool.full.model <- function(PotRichness,HOa, HOb, CVa, CVb, MGa, MGb, SHa,SHb,
                            HO,CV, MG,SH)
{
        HO.effect <- exp(-0.5*((HO-HOa)/HOb)^2)
        CV.effect <- exp(-0.5*((CV-CVa)/CVb)^2)
        MG.effect <- exp(-0.5*((MG-MGa)/MGb)^2)
        SH.effect <- exp(-0.5*((SH-SHa)/SHb)^2)
        PotRichness * HO.effect *  CV.effect *  MG.effect * SH.effect
}


pool.noho.model <- function(PotRichness,CV,MG,SH,
                            CVa, CVb, MGa, MGb, SHa,SHb)
{
        CV.effect <- exp(-0.5*((CV-CVa)/CVb)^2)
        MG.effect <- exp(-0.5*((MG-MGa)/MGb)^2)
        SH.effect <- exp(-0.5*((SH-SHa)/SHb)^2)
        PotRichness *  CV.effect *  MG.effect * SH.effect
}


pool.nocv.model <- function(PotRichness,HO,MG,SH,
                            HOa, HOb,  MGa, MGb, SHa,SHb)
{
        HO.effect <- exp(-0.5*((HO-HOa)/HOb)^2)
        MG.effect <- exp(-0.5*((MG-MGa)/MGb)^2)
        SH.effect <- exp(-0.5*((SH-SHa)/SHb)^2)
        PotRichness * HO.effect * MG.effect * SH.effect
}


pool.nomg.model <- function(PotRichness,HO, CV,SH,
                            HOa, HOb, CVa, CVb,SHa,SHb)
{
        HO.effect <- exp(-0.5*((HO-HOa)/HOb)^2)
        CV.effect <- exp(-0.5*((CV-CVa)/CVb)^2)
        SH.effect <- exp(-0.5*((SH-SHa)/SHb)^2)
        PotRichness * HO.effect * CV.effect * SH.effect
}

pool.nosh.model <- function(PotRichness,HO,CV,MG,
                            HOa, HOb,CVa, CVb,MGa, MGb)
{
        HO.effect <- exp(-0.5*((HO-HOa)/HOb)^2)
        CV.effect <- exp(-0.5*((CV-CVa)/CVb)^2)
        MG.effect <- exp(-0.5*((MG-MGa)/MGb)^2)
        PotRichness * HO.effect * CV.effect * MG.effect
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

