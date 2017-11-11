#################################################################################
##### Plots the best fit model for each group of species and explanatory variable
#####
#####           Aitor Ameztegui (UQAM)
#####                   January 2015
#################################################################################

# Load the scientific models and the parameters file
source("./Code/Scientificmodels2.R")
source("./Code/zz_AMZcolors.R")
source("./Code/zz_mf_labeller.R")

# Import all the data and define the required variables
ocells <- read.csv("./Data/data_analyze_unc_rich_abun_may_2016.csv")

library(lattice)


# Rename the variables so that it is easier to automathize processes (two letters each)
ocells$HO <- ocells$ALCDOM
ocells$BA <- ocells$G_m2_ha
ocells$DW <- ocells$PEUSMHA
ocells$MG <- ocells$p_G_gruesa
ocells$CV<- ocells$CAVTOTAL  #Not included (lacking on P.sylvestris database)
ocells$SH <- ocells$RECARBUSTIU
ocells$SD <- ocells$SD_maderaFMG

# Import fit and models depending on variables to plot

if(respuesta == "abundance") {
        parameters <-read.delim ("./Results/BestModel_Abu2.txt", dec=".", header=T)
} else if (respuesta == "richness") {
        parameters <- read.delim ("./Results/BestModel_Ric2.txt", dec=".", header=T)
}

# Define groups of species

total<- c("TOTA")
habitat<- c("SPEA", "GENA", "UBIA")
nesting<- c("OVNA", "CVNA", "GRNA", "UNNA")
foraging<- c("OVFA","TRFA", "GRFA", "UNFA")
cavity<- c("NOCA", "EXCA", "SECA")


# Create explanatory variables to plot
seqHO<-seq(from=quantile(ocells$HO,probs = 0.1), to= 1.2*quantile(ocells$HO, probs = 0.9), by= 0.4)
seqMG<-seq(from=quantile(ocells$MG, probs = 0.1), to= quantile(ocells$MG, probs = 0.9), by= 2)
seqCV<-seq(from=quantile(ocells$CV, probs = 0.1), to= quantile(ocells$CV, probs = 0.9), by= 1.5)
seqSH<-seq(from=quantile(ocells$SH, probs = 0.1), to= quantile(ocells$SH, probs = 0.9), by= 2.5)


# Function to plot --------------------------------------------------------

figura <- function (depvar, expvar, maxiabu, pos="none") { 
        
        iterations = length(expvar)
        variables = length(depvar)
        output <- matrix(ncol=variables, nrow=iterations)
        expname<-deparse(substitute(expvar))
        colors<-AMZcolors(length(depvar))    
      
        
        plot(expvar,expvar, ylim=c(0,maxiabu), type="n", ylab="Species abundance", xlab=mf_labeller(expname))
        for (i in 1: length (depvar)) {   
                # Generate new dataset
                vari <- depvar[i]
                print(vari)
                select = ocells[,vari]
                ocell = ocells[select>0,]
                print(nrow(ocell))
                # Create lists of parameters
                PotRichness <- rep(NA, times=length(depvar))
                HOa<- rep(NA, times=length(depvar))
                HOb<- rep(NA, times=length(depvar))
                CVa<- rep(NA, times=length(depvar))        
                CVb<- rep(NA, times=length(depvar))
                MGa<- rep(NA, times=length(depvar))
                MGb<- rep(NA, times=length(depvar))
                SHa<- rep(NA, times=length(depvar))
                SHb<- rep(NA, times=length(depvar))
        
                PotRichness[i] <- parameters[( parameters$Variable==depvar[i]),14]
                HOa[i] <- parameters[( parameters$Variable==depvar[i]),15]
                HOb[i] <- parameters[( parameters$Variable==depvar[i]),16]
                CVa[i] <- parameters[( parameters$Variable==depvar[i]),17]
                CVb[i] <- parameters[( parameters$Variable==depvar[i]),18]
                MGa[i] <- parameters[( parameters$Variable==depvar[i]),19]
                MGb[i] <- parameters[( parameters$Variable==depvar[i]),20]
                SHa[i] <- parameters[( parameters$Variable==depvar[i]),21]        
                SHb[i] <- parameters[( parameters$Variable==depvar[i]),22]
              
                if (expname=="seqHO")  {output[,i]<-pool.full.model(PotRichness[i],HOa= HOa[i],HOb= HOb[i], CVa= CVa[i], CVb=CVb[i],MGa[i],MGb[i],SHa[i],SHb[i],
                                                                HO=seqHO, CV=mean(ocell$CV), MG=mean(ocell$MG), SH=mean(ocell$SH))}
                else if (expname=="seqCV") {output[,i]<-pool.full.model(PotRichness[i],HOa= HOa[i],HOb= HOb[i], CVa= CVa[i], CVb=CVb[i],MGa[i],MGb[i],SHa[i],SHb[i],
                                                                HO=mean(ocell$HO),CV=seqCV,  MG=mean(ocell$MG), SH=mean(ocell$SH)) } 
                else if (expname=="seqMG") {output[,i]<-pool.full.model(PotRichness[i],HOa= HOa[i],HOb= HOb[i], CVa= CVa[i], CVb=CVb[i],MGa[i],MGb[i],SHa[i],SHb[i],
                                                              HO=mean(ocell$HO), CV=mean(ocell$CV),  MG=seqMG, SH=mean(ocell$SH))}
                else if (expname=="seqSH") {output[,i]<-pool.full.model(PotRichness[i],HOa= HOa[i],HOb= HOb[i], CVa= CVa[i], CVb=CVb[i],MGa[i],MGb[i],SHa[i],SHb[i],
                                                               HO=mean(ocell$HO), CV=mean(ocell$CV), MG=mean(ocell$MG), SH=seqSH) }               
                
                lines(expvar,output[,i], type="l", ylab="Species abundance", lwd=4,  col=colors[i])
                if(pos!= "none"){
                        legend(pos,legend=mf_labeller(depvar), col=colors, lwd=4, cex=0.5, bty="n", horiz=F)}           
        }     
  
        
}


figura_rel <- function (depvar, expvar, pos="none") {    
        variables = length(depvar)
        iterations = length(expvar)
        
        output <- matrix(ncol=variables, nrow=iterations)
        expname<-deparse(substitute(expvar))
        colors<-AMZcolors(length(depvar))    
        
        
        plot(expvar,expvar, ylim=c(0,1), type="n", ylab="Fraction of potential abundance", xlab=mf_labeller(expname))
        for (i in 1: length (depvar)) {        
                # Create lists of parameters
                PotRichness <- rep(NA, times=length(depvar))
                HOa<- rep(NA, times=length(depvar))
                HOb<- rep(NA, times=length(depvar))
                CVa<- rep(NA, times=length(depvar))        
                CVb<- rep(NA, times=length(depvar))
                MGa<- rep(NA, times=length(depvar))
                MGb<- rep(NA, times=length(depvar))
                SHa<- rep(NA, times=length(depvar))
                SHb<- rep(NA, times=length(depvar))
                
                PotRichness[i] <- parameters[( parameters$Variable==depvar[i]),14]
                HOa[i] <- parameters[( parameters$Variable==depvar[i]),15]
                HOb[i] <- parameters[( parameters$Variable==depvar[i]),16]
                CVa[i] <- parameters[( parameters$Variable==depvar[i]),17]
                CVb[i] <- parameters[( parameters$Variable==depvar[i]),18]
                MGa[i] <- parameters[( parameters$Variable==depvar[i]),19]
                MGb[i] <- parameters[( parameters$Variable==depvar[i]),20]
                SHa[i] <- parameters[( parameters$Variable==depvar[i]),21]        
                SHb[i] <- parameters[( parameters$Variable==depvar[i]),22]
                
                if (expname=="seqHO")  {output[,i]<-pool.full.model(1,HOa= HOa[i],HOb= HOb[i], CVa= 1000, CVb=100000,MGa=1000,MGb=100000,SHa=1000,SHb=1000000,
                                                                    HO=seqHO, CV=mean(ocells$CV), MG=mean(ocells$MG), SH=mean(ocells$SH))}
                else if (expname=="seqCV") {output[,i]<-pool.full.model(1,HOa= 1,HOb= 100000, CVa= CVa[i], CVb=CVb[i],MGa=1,MGb=1000000,SHa=1,SHb=1000000,
                                                                        HO=mean(ocells$HO),CV=seqCV,  MG=mean(ocells$MG), SH=mean(ocells$SH)) } 
                else if (expname=="seqMG") {output[,i]<-pool.full.model(1,HOa= 1000,HOb= 1000000, CVa= 1000, CVb=1000000,MGa[i],MGb[i],SHa=1000,SHb=1000000,
                                                                        HO=mean(ocells$HO), CV=mean(ocells$CV),  MG=seqMG, SH=mean(ocells$SH))}
                else if (expname=="seqSH") {output[,i]<-pool.full.model(1,HOa= 1,HOb= 100000, CVa= 1, CVb=100000,MGa=1,MGb=1000000,SHa=SHa[i],SHb=SHb[i],
                                                                        HO=mean(ocells$HO), CV=mean(ocells$CV), MG=mean(ocells$MG), SH=seqSH) }               
                
                lines(expvar,output[,i], type="o", ylab="Fraction of potential abundance", pch= 14+i, lwd=2,col=colors[i])
                if(pos!= "none"){
                legend(pos,legend=mf_labeller(depvar),pch=c(15,16,17,18),lwd=2,col=colors, cex=0.8, bty="n", horiz=F )}           
        }     
        
        
}





figura_3D <- function (depvar, expvar1,expvar2, zgir, xgir, lay=c(2,2)) {
        
        expname1<-deparse(substitute(expvar1))
        expname2<-deparse(substitute(expvar2))
        n_dep <- length (depvar)
        output<- data.frame(expand.grid(list(x=expvar1[seq(1, length(expvar1), by=2)], 
                                             y =expvar2[seq(1, length(expvar2), by=2)])))
        for (i in 1: length (depvar)) {        
                
                # Create lists of parameters
                PotRichness <- rep(NA, times=length(depvar))
                HOa<- rep(NA, times=length(depvar))
                HOb<- rep(NA, times=length(depvar))
                CVa<- rep(NA, times=length(depvar))        
                CVb<- rep(NA, times=length(depvar))
                MGa<- rep(NA, times=length(depvar))
                MGb<- rep(NA, times=length(depvar))
                SHa<- rep(NA, times=length(depvar))
                SHb<- rep(NA, times=length(depvar))
                
                PotRichness <- parameters[( parameters$Variable==depvar[i]),14]
                HOa[i] <- parameters[( parameters$Variable==depvar[i]),15]
                HOb[i] <- parameters[( parameters$Variable==depvar[i]),16]
                CVa[i] <- parameters[( parameters$Variable==depvar[i]),17]
                CVb[i] <- parameters[( parameters$Variable==depvar[i]),18]
                MGa[i] <- parameters[( parameters$Variable==depvar[i]),19]
                MGb[i] <- parameters[( parameters$Variable==depvar[i]),20]
                SHa[i] <- parameters[( parameters$Variable==depvar[i]),21]        
                SHb[i] <- parameters[( parameters$Variable==depvar[i]),22]
               
                if (expname1=="seqHO" & expname2 =="seqMG")  
                        {HO.effect <- exp(-0.5*((output$x-HOa[i])/HOb[i])^2)
                        MG.effect <- exp(-0.5*((output$y-MGa[i])/MGb[i])^2)
                        CV.effect <- 1
                        SH.effect <- 1}
                
                else if (expname1=="seqHO" & expname2 =="seqCV") 
                        {HO.effect <- exp(-0.5*((output$x-HOa[i])/HOb[i])^2)
                        MG.effect <- 1
                        CV.effect <- exp(-0.5*((output$y-CVa[i])/CVb[i])^2)
                        SH.effect <- 1}
                
                else if (expname1=="seqHO" & expname2 =="seqSH") 
                        {HO.effect <- exp(-0.5*((output$x-HOa[i])/HOb[i])^2)
                        MG.effect <- 1
                        CV.effect <- 1
                        SH.effect <- exp(-0.5*((output$y-SHa[i])/SHb[i])^2)}
                
                else if (expname1=="seqMG" & expname2 =="seqCV") 
                        {HO.effect <- 1
                        MG.effect <- exp(-0.5*((output$x-MGa[i])/MGb[i])^2)
                        CV.effect <- exp(-0.5*((output$y-CVa[i])/CVb[i])^2)
                        SH.effect <- 1}
                
                else if (expname1=="seqMG" & expname2 =="seqSH") 
                        {HO.effect <- 1
                        MG.effect <- exp(-0.5*((output$x-MGa[i])/MGb[i])^2)
                        CV.effect <- 1
                        SH.effect <- exp(-0.5*((output$y-SHa[i])/SHb[i])^2)}
                
                else if (expname1=="seqCV" & expname2 =="seqSH") 
                        {HO.effect <- 1
                        MG.effect <- 1
                        CV.effect <- exp(-0.5*((output$x-CVa[i])/CVb[i])^2)
                        SH.effect <- exp(-0.5*((output$y-SHa[i])/SHb[i])^2)}
                
                else if (expname1=="seqMG" & expname2 =="seqHO") 
                {HO.effect <- 1
                MG.effect <- 1
                CV.effect <- exp(-0.5*((output$x-CVa[i])/CVb[i])^2)
                SH.effect <- exp(-0.5*((output$y-SHa[i])/SHb[i])^2)}
                
                output[,i+2]<- 1 * HO.effect *  CV.effect *  MG.effect * SH.effect
                colnames(output)[i+2] <- depvar[i]
        
        }
        
        if (n_dep == 1) {
        wireframe(output[,3] ~ x*y, data = output, 
                  layout=lay,
                  xlab=list(mf_labeller(expname1), cex=1.8, rot=-15),  
                  ylab = list(mf_labeller(expname2), cex=1.8, rot=25),
                  zlab=list("% potential abundance", cex=1.8, rot=90),
                  scales=list(arrows=F, cex=1.2),
                  drape = T, colorkey = F, outer=F,  col.regions = colorRampPalette(c("white", "dark gray"))(100),
                  screen = list (z = zgir, x = xgir), 
                  par.settings = list(axis.line = list(col = "transparent"),strip.background=list(col="transparent")),
                  par.strip.text=list(cex=2, font=2), 
                  strip=strip.custom(factor.levels=mf_labeller(depvar)))}
        
        else if (n_dep == 2) {
        wireframe(output[,3] + output[,4] ~ x*y, data = output, 
                  layout=lay,
                  xlab=list(mf_labeller(expname1), cex=1.8,rot=-15),  
                  ylab = list(mf_labeller(expname2), cex=1.8, rot=25),
                  zlab=list("% potential abundance", cex=1.8, rot=90),
                  scales=list(arrows=F, cex=1.2),
                  drape = T, colorkey = F, outer=T,  col.regions = colorRampPalette(c("white", "dark gray"))(100),
                 screen = list (z = zgir, x = xgir), 
                 par.settings = list(axis.line = list(col = "transparent"),strip.background=list(col="transparent")),
                 par.strip.text=list(cex=2, font=2),
                 strip=strip.custom(factor.levels=mf_labeller(depvar)))}
        
        else if (n_dep==3){
        wireframe(output[,3] + output[,4] + output[,5]~ x*y, data = output, 
                  layout=lay,
                  xlab=list(mf_labeller(expname1), cex=1.8,rot=-15),
                  ylab = list(mf_labeller(expname2), cex=1.8,rot=25),
                  zlab=list("% potential abundance", cex=1.8, rot=90),
                  scales=list(arrows=F, cex=1.2),
                  drape = T, colorkey = F, outer=T,  col.regions = colorRampPalette(c("white", "dark gray"))(100),
                  screen = list (z = zgir, x = xgir), 
                  par.settings = list(axis.line = list(col = "transparent"),strip.background=list(col="transparent")),
                  par.strip.text=list(cex=2, font=2),
                  strip=strip.custom(factor.levels=mf_labeller(depvar)))}
        
        else if (n_dep==4){
        wireframe(output[,3] + output[,4] + output[,5] + output[,6] ~ x*y, data = output, 
                  layout=lay,
                  xlab=list(mf_labeller(expname1), cex=1.8,rot=-15),
                  ylab = list(mf_labeller(expname2), cex=1.8, rot=25),
                  zlab=list("% potential abundance", cex=1.8, rot=90),
                  scales=list(arrows=F, cex=1.2),
                  drape = T, colorkey = F, outer=T,  col.regions = colorRampPalette(c("white", "dark gray"))(100),
                  screen = list (z = zgir, x = xgir), 
                  par.settings = list(axis.line = list(col = "transparent"),strip.background=list(col="transparent")),
                  par.strip.text=list(cex=2, font=2), 
                  strip=strip.custom(factor.levels=mf_labeller(depvar)))}
}    
    
