#################################################################################
##### Plots the best fit model for each group of species and explanatory variable
#####
#####           Aitor Ameztegui (UQAM)
#####                   January 2015
#################################################################################

library(lattice)
library(forcats)
library(tidyverse)

# Load the scientific models and the parameters file
source("./Code/zz_AMZcolors.R")
source("./Code/zz_mf_labeller.R")

full_model <-  function (Basal = Basal, Densi = Densi, Quadr = Quadr, DHeig =DHeig, Cavit = Cavit, Shrub = Shrub, 
                         PotRichness = PotRichness, Basal_a = Basal_a, Basal_b = Basal_b, 
                         Densi_a = Densi_a, Densi_b = Densi_b, Quadr_a = Quadr_a, Quadr_b = Quadr_b, 
                         DHeig_a = DHeig_a, DHeig_b = DHeig_b, Cavit_a = Cavit_a, Cavit_b = Cavit_b, 
                         Shrub_a = Shrub_a, Shrub_b = Shrub_b){
        Basal.effect <-  exp(-0.5*((Basal-Basal_a)/Basal_b)^2)
        Densi.effect <- exp(-0.5*((Densi-Densi_a)/Densi_b)^2)
        Quadr.effect <- exp(-0.5*((Quadr-Quadr_a)/Quadr_b)^2)
        DHeig.effect <- exp(-0.5*((DHeig-DHeig_a)/DHeig_b)^2)
        Cavit.effect <- exp(-0.5*((Cavit-Cavit_a)/Cavit_b)^2)
        Shrub.effect <- exp(-0.5*((Shrub-Shrub_a)/Shrub_b)^2)
        PotRichness * Basal.effect * Densi.effect * Quadr.effect * DHeig.effect *  Cavit.effect  * Shrub.effect
}

# Import all the data and define the required variables
load("./Data/tidy_ocells.Rdata")

# Import fit and models depending on variables to plot
    if(model_group == "Densi") {
            parameters <-read.delim ("./Results/BestModel_Densi.txt", dec=".", header=T)
    } else if (model_group == "Basal") {
            parameters <- read.delim ("./Results/BestModel_Basal.txt", dec=".", header=T)
    }

# Define groups of species
total<- c("TOTA")
habitat<- c("TOTA","SPEA", "GENA", "UBIA")
nesting<- c("OVNA", "CVNA",  "UNNA","GRNA")
foraging<- c("OVFA","TRFA", "UNFA", "GRFA")
all <- c("TOTA", "SPEA", "GENA", "UBIA",
         "OVNA", "CVNA",  "UNNA","GRNA",
         "OVFA","TRFA", "UNFA", "GRFA")

# Create explanatory variables to plot
seq_Basal<-seq(from=quantile(ocells$Basal, probs = 0.1), to= quantile(ocells$Basal, probs = 0.9), by= 1.8)
seq_Densi<-seq(from=quantile(ocells$Densi,probs = 0.1), to= 1.2*quantile(ocells$Densi, probs = 0.9), by= 15)
seq_Quadr<-seq(from=quantile(ocells$Quadr, probs = 0.1), to= quantile(ocells$Quadr, probs = 0.9), by= 0.3)
seq_DHeig<-seq(from=quantile(ocells$DHeig,probs = 0.1), to= 1.2*quantile(ocells$DHeig, probs = 0.9), by= 0.3)
seq_Snags<-seq(from=quantile(ocells$Snags, probs = 0.1), to= quantile(ocells$Snags, probs = 0.9), by= 1.3)
seq_Cavit<-seq(from=quantile(ocells$Cavit, probs = 0.1), to= quantile(ocells$Cavit, probs = 0.9), by= 1.3)
seq_Herba<-seq(from=quantile(ocells$Herba, probs = 0.1), to= quantile(ocells$Herba, probs = 0.9), by= 1.3)
seq_Shrub<-seq(from=quantile(ocells$Shrub, probs = 0.1), to= quantile(ocells$Shrub, probs = 0.9), by= 2.2)
seq_Thick<-seq(from=quantile(ocells$Thick, probs = 0.1), to= quantile(ocells$Thick, probs = 0.9), by= 2.2)
seq_DVari<-seq(from=quantile(ocells$DVari, probs = 0.1), to= quantile(ocells$DVari, probs = 0.9), by= 2.2)


# Functions to plot --------------------------------------------------------

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
                DHeig_a<- rep(NA, times=length(depvar))
                DHeig_b<- rep(NA, times=length(depvar))
                Cavit_a<- rep(NA, times=length(depvar))        
                Cavit_b<- rep(NA, times=length(depvar))
                Basal_a<- rep(NA, times=length(depvar))
                Basal_b<- rep(NA, times=length(depvar))
                Shrub_a<- rep(NA, times=length(depvar))
                Shrub_b<- rep(NA, times=length(depvar))
        
                PotRichness[i] <- parameters[( parameters$Variable==depvar[i]),14]
                DHeig_a[i] <- parameters[( parameters$Variable==depvar[i]),15]
                DHeig_b[i] <- parameters[( parameters$Variable==depvar[i]),16]
                Cavit_a[i] <- parameters[( parameters$Variable==depvar[i]),17]
                Cavit_b[i] <- parameters[( parameters$Variable==depvar[i]),18]
                Basal_a[i] <- parameters[( parameters$Variable==depvar[i]),19]
                Basal_b[i] <- parameters[( parameters$Variable==depvar[i]),20]
                Shrub_a[i] <- parameters[( parameters$Variable==depvar[i]),21]        
                Shrub_b[i] <- parameters[( parameters$Variable==depvar[i]),22]
              
                if (expname=="seq_DHeig")  {output[,i]<-tetra.complete.model(PotRichness[i],DHeig_a= DHeig_a[i],DHeig_b= DHeig_b[i], Cavit_a= Cavit_a[i], Cavit_b=Cavit_b[i],Basal_a[i],Basal_b[i],Shrub_a[i],Shrub_b[i],
                                                                DHeig_=seq_DHeig, Cavit=mean(ocell$Cavit), Basal=mean(ocell$Basal), Shrub=mean(ocell$Shrub))}
                else if (expname=="seq_Cavit") {output[,i]<-tetra.complete.model(PotRichness[i],DHeig_a= DHeig_a[i],DHeig_b= DHeig_b[i], Cavit_a= Cavit_a[i], Cavit_b=Cavit_b[i],Basal_a[i],Basal_b[i],Shrub_a[i],Shrub_b[i],
                                                                DHeig=mean(ocell$DHeig),Cavit=seq_Cavit,  Basal=mean(ocell$Basal), Shrub=mean(ocell$Shrub)) } 
                else if (expname=="seq_Basal") {output[,i]<-tetra.complete.model(PotRichness[i],DHeig_a= DHeig_a[i],DHeig_b= DHeig_b[i], Cavit_a= Cavit_a[i], Cavit_b=Cavit_b[i],Basal_a[i],Basal_b[i],Shrub_a[i],Shrub_b[i],
                                                              DHeig=mean(ocell$DHeig), Cavit=mean(ocell$Cavit),  Basal=seq_Basal, Shrub=mean(ocell$Shrub))}
                else if (expname=="seq_Shrub") {output[,i]<-tetra.complete.model(PotRichness[i],DHeig_a= DHeig_a[i],DHeig_b= DHeig_b[i], Cavit_a= Cavit_a[i], Cavit_b=Cavit_b[i],Basal_a[i],Basal_b[i],Shrub_a[i],Shrub_b[i],
                                                               DHeig=mean(ocell$DHeig), Cavit=mean(ocell$Cavit), Basal=mean(ocell$Basal), Shrub=seq_Shrub) }               
                
                lines(expvar,output[,i], type="l", ylab="Species abundance", lwd=4,  col=colors[i])
                if(pos!= "none"){
                        legend(pos,legend=mf_labeller(depvar), col=colors, lwd=4, cex=0.5, bty="n", horiz=F)}           
        }     
  
        
}


figura_rel <- function (depvar, expvar, pos="n") {    
        variables = length(depvar)
        iterations = length(expvar)
        
        output <- matrix(ncol=variables, nrow=iterations)
        expname<-deparse(substitute(expvar))
        colors<-AMZcolors(length(depvar))    
        
                # Create dataframe of parameters
                for (i in 1: length (depvar)) {
                    PotRichness <- 1
                    Basal_a <- parameters[( parameters$Variable==depvar[i]),13]
                    Basal_b <- parameters[( parameters$Variable==depvar[i]),14]
                    Densi_a <- parameters[( parameters$Variable==depvar[i]),15]
                    Densi_b <- parameters[( parameters$Variable==depvar[i]),16]
                    Quadr_a <- parameters[( parameters$Variable==depvar[i]),17]
                    Quadr_b <- parameters[( parameters$Variable==depvar[i]),18]
                    DHeig_a <- parameters[( parameters$Variable==depvar[i]),19]
                    DHeig_b <- parameters[( parameters$Variable==depvar[i]),20]
                    Cavit_a <- parameters[( parameters$Variable==depvar[i]),21]
                    Cavit_b <- parameters[( parameters$Variable==depvar[i]),22]
                    Shrub_a <- parameters[( parameters$Variable==depvar[i]),23] 
                    Shrub_b <- parameters[( parameters$Variable==depvar[i]),24]
                
                    Basal = mean(ocells$Basal); Densi = mean(ocells$Densi); Quadr = mean(ocells$Quadr); 
                    DHeig = mean(ocells$DHeig); Cavit = mean (ocells$Cavit); Shrub = mean (ocells$Shrub)
                    if (expname=="seq_Basal")  {output[,i]<- full_model (seq_Basal, Densi, Quadr, DHeig, Cavit, Shrub,
                                                                         PotRichness,Basal_a, Basal_b, Densi_a = 0, Densi_b = 1e7,
                                                                         Quadr_a = 0, Quadr_b = 1e7, DHeig_a = 0, DHeig_b = 1e7, 
                                                                         Cavit_a = 0, Cavit_b = 1e7, Shrub_a = 0, Shrub_b = 1e7)
                    } else if (expname=="seq_Densi") {output[,i]<- full_model (Basal, seq_Densi, Quadr, DHeig, Cavit, Shrub,
                                                                              PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a, Densi_b,
                                                                              Quadr_a = 0, Quadr_b = 1e7, DHeig_a = 0, DHeig_b = 1e7,
                                                                              Cavit_a = 0, Cavit_b = 1e7, Shrub_a = 0, Shrub_b = 1e7)
                    } else if (expname=="seq_Quadr") {output[,i]<- full_model (Basal, Densi, seq_Quadr, DHeig, Cavit, Shrub,
                                                                               PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a = 0, Densi_b = 1e7,
                                                                               Quadr_a, Quadr_b, DHeig_a = 0, DHeig_b = 1e7, Cavit_a = 0, Cavit_b = 1e7,
                                                                               Shrub_a = 0, Shrub_b = 1e7)
                    } else if (expname=="seq_DHeig") {output[,i]<- full_model (Basal, Densi, Quadr, seq_DHeig, Cavit, Shrub,
                                                                               PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a = 0, Densi_b = 1e7,
                                                                               Quadr_a = 0, Quadr_b = 1e7, DHeig_a, DHeig_b, 
                                                                               Cavit_a = 0, Cavit_b = 1e7, Shrub_a = 0, Shrub_b = 1e7)
                    } else if (expname=="seq_Cavit") {output[,i]<- full_model (Basal, Densi, Quadr, DHeig, seq_Cavit, Shrub,
                                                                               PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a = 0, Densi_b = 1e7,
                                                                               Quadr_a = 0, Quadr_b = 1e7, DHeig_a = 0, DHeig_b = 1e7, 
                                                                               Cavit_a, Cavit_b, Shrub_a = 0, Shrub_b = 1e7)
                    } else if (expname=="seq_Shrub") {output[,i]<- full_model (Basal, Densi, Quadr, DHeig, Cavit, seq_Shrub,
                                                                               PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a = 0, Densi_b = 1e7,
                                                                               Quadr_a = 0, Quadr_b = 1e7, DHeig_a = 0, DHeig_b = 1e7, 
                                                                               Cavit_a = 0, Cavit_b = 1e7, Shrub_a, Shrub_b)
                    }
                }
        output <- data.frame(output)
        names(output) <- mf_labeller(depvar)
        output$exp <- expvar
        
        output <- gather(output, Species,abundance, -exp)
        
        ifelse(depvar==habitat,
               output$Species <- factor(output$Species, 
                                 levels = c("All species","Specialists","Generalists","Ubiquitous")) ,
        ifelse(depvar==nesting,
               output$Species <- factor(output$Species, 
                                       levels = c("Canopy nesters","Cavity nesters",
                                                  "Understory nesters","Ground nesters")), 
               output$Species <- factor(output$Species, 
                                        levels = c("Canopy foragers","Trunk foragers",
                                                   "Understory foragers","Ground foragers"))))
             
            

                plot_xy <- ggplot(data = output) +
                    ylab("Fraction of potential abundance") +
                    xlab(mf_labeller(expname)) +
                    ylim(0,1)  + 
                    geom_line(aes(exp, abundance, color=Species), size = 1.3)+
                    geom_point(aes(exp, abundance, color=Species, shape = Species), size=2) +
                    scale_colour_manual(values = colors) +
                    theme_bw() +
                    theme(axis.title.x= element_text(face="bold"),
                          legend.position = pos,
                          legend.title=element_blank(),
                          plot.margin=unit(c(-0.25,0.5,0.5,0.5), "cm"))
     
                plot_xy
        }     

figura_3D <- function (depvar, expvar1,expvar2, zgir, xgir, lay=c(2,2)) {
        
        expname1<-deparse(substitute(expvar1))
        expname2<-deparse(substitute(expvar2))
        n_dep <- length (depvar)
        output<- data.frame(expand.grid(list(x=expvar1[seq(1, length(expvar1), by=2)], 
                                             y =expvar2[seq(1, length(expvar2), by=2)])))
        for (i in 1: length (depvar)) {        
                
            # Create lists of parameters
            PotRichness <- 1
            Basal_a <- parameters[( parameters$Variable==depvar[i]),13]
            Basal_b <- parameters[( parameters$Variable==depvar[i]),14]
            Densi_a <- parameters[( parameters$Variable==depvar[i]),15]
            Densi_b <- parameters[( parameters$Variable==depvar[i]),16]
            Quadr_a <- parameters[( parameters$Variable==depvar[i]),17]
            Quadr_b <- parameters[( parameters$Variable==depvar[i]),18]
            DHeig_a <- parameters[( parameters$Variable==depvar[i]),19]
            DHeig_b <- parameters[( parameters$Variable==depvar[i]),20]
            Cavit_a <- parameters[( parameters$Variable==depvar[i]),21]
            Cavit_b <- parameters[( parameters$Variable==depvar[i]),22]
            Shrub_a <- parameters[( parameters$Variable==depvar[i]),23] 
            Shrub_b <- parameters[( parameters$Variable==depvar[i]),24]
            
            Basal = mean(ocells$Basal); Densi = mean(ocells$Densi); Quadr = mean(ocells$Quadr); 
            DHeig = mean(ocells$DHeig); Cavit = mean (ocells$Cavit); Shrub = mean (ocells$Shrub)
                 
            if (expname1=="seq_Densi" & expname2 =="seq_Quadr")  
                     {output[,i+2] <-  full_model (Basal, Densi = output$x, Quadr = output$y, DHeig, Cavit, Shrub,
                                 PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a , Densi_b,
                                 Quadr_a, Quadr_b, DHeig_a = 0, DHeig_b = 1e7, 
                                 Cavit_a = 0, Cavit_b = 1e7, Shrub_a = 0, Shrub_b = 1e7)}
            
                else if (expname1=="seq_DHeig" & expname2 =="seq_Densi")
                    {output[,i+2] <-  full_model (Basal, Densi= output$y, Quadr, DHeig = output$x, Cavit, Shrub,
                                          PotRichness,Basal_a = 0 , Basal_b = 1e7, Densi_a, Densi_b,
                                          Quadr_a = 0, Quadr_b = 1e7, DHeig_a, DHeig_b, 
                                          Cavit_a = 0, Cavit_b = 1e7, Shrub_a = 0, Shrub_b = 1e7)}
                
                else if (expname1=="seq_DHeig" & expname2 =="seq_Basal")
                    {output[,i+2] <-  full_model (Basal = output$y, Densi, Quadr, DHeig = output$x, Cavit, Shrub,
                                              PotRichness,Basal_a , Basal_b , Densi_a = 0 , Densi_b = 1e7,
                                              Quadr_a = 0, Quadr_b = 1e7, DHeig_a, DHeig_b, 
                                              Cavit_a = 0, Cavit_b = 1e7, Shrub_a = 0, Shrub_b = 1e7)}

                else if (expname1=="seq_DHeig" & expname2 =="seq_Cavit")
                    {output[,i+2] <-  full_model (Basal, Densi, Quadr, DHeig = output$x, Cavit = output$y, Shrub,
                                              PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a = 0 , Densi_b = 1e7,
                                              Quadr_a = 0, Quadr_b = 1e7, DHeig_a, DHeig_b, 
                                              Cavit_a, Cavit_b, Shrub_a = 0, Shrub_b = 1e7)}

                else if (expname1=="seq_DHeig" & expname2 =="seq_Shrub")
                {output[,i+2] <-  full_model (Basal, Densi, Quadr, DHeig = output$x, Cavit, Shrub = output$y,
                                              PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a = 0 , Densi_b = 1e7,
                                              Quadr_a = 0, Quadr_b = 1e7, DHeig_a, DHeig_b, 
                                              Cavit_a = 0, Cavit_b = 1e7, Shrub_a, Shrub_b)}

                else if (expname1=="seq_Basal" & expname2 =="seq_Cavit")
                {output[,i+2] <-  full_model (Basal = output$x, Densi, Quadr, DHeig, Cavit  = output$y, Shrub,
                                              PotRichness,Basal_a, Basal_b , Densi_a = 0 , Densi_b = 1e7,
                                              Quadr_a = 0, Quadr_b = 1e7, DHeig_a = 0, DHeig_b = 1e7, 
                                              Cavit_a, Cavit_b, Shrub_a = 0, Shrub_b = 1e7)}

                else if (expname1=="seq_Basal" & expname2 =="seq_Shrub")
                {output[,i+2] <-  full_model (Basal = output$x, Densi, Quadr, DHeig, Cavit, Shrub = output$y,
                                              PotRichness,Basal_a, Basal_b, Densi_a = 0 , Densi_b = 1e7,
                                              Quadr_a = 0, Quadr_b = 1e7, DHeig_a = 0, DHeig_b = 1e7, 
                                              Cavit_a = 0, Cavit_b = 1e7, Shrub_a, Shrub_b)}

                else if (expname1=="seq_Cavit" & expname2 =="seq_Shrub")
                {output[,i+2] <-  full_model (Basal, Densi, Quadr, DHeig, Cavit = output$x, Shrub = output$y,
                                              PotRichness,Basal_a = 0, Basal_b = 1e7, Densi_a = 0 , Densi_b = 1e7,
                                              Quadr_a = 0, Quadr_b = 1e7, DHeig_a = 0, DHeig_b = 1e7, 
                                              Cavit_a, Cavit_b, Shrub_a, Shrub_b)}
            
                else if (expname1=="seq_Basal" & expname2 =="seq_DHeig")
                {output[,i+2] <-  full_model (Basal = output$x, Densi, Quadr, DHeig = output$y, Cavit, Shrub,
                                              PotRichness,Basal_a, Basal_b, Densi_a = 0 , Densi_b = 1e7,
                                              Quadr_a = 0, Quadr_b = 1e7, DHeig_a, DHeig_b, 
                                              Cavit_a = 0, Cavit_b = 1e7, Shrub_a = 0, Shrub_b = 1e7)}
            colnames(output)[i+2] <- depvar[i]
            
                    
        
        }
        
        if (n_dep == 1) {
        wireframe(output[,3] ~ x*y, data = output, 
                  layout=lay,
                  xlab=list(mf_labeller(expname1), cex=1.7, rot=-15),  
                  ylab = list(mf_labeller(expname2), cex=1.7, rot=25),
                  zlab=list("% potential abundance", cex=1.7, rot=90),
                  scales=list(arrows=F, cex=1.2),
                  drape = T, colorkey = F, outer=F,  col.regions = colorRampPalette(c("white", "dark gray"))(100),
                  screen = list (z = zgir, x = xgir), 
                  par.settings = list(axis.line = list(col = "transparent"),strip.background=list(col="transparent")),
                  par.strip.text=list(cex=2, font=2), 
                  strip=strip.custom(factor.levels=mf_labeller(depvar)))}
        
        else if (n_dep == 2) {
        wireframe(output[,3] + output[,4] ~ x*y, data = output, 
                  layout=lay,
                  xlab=list(mf_labeller(expname1), cex=1.6,rot=-15),  
                  ylab = list(mf_labeller(expname2), cex=1.6, rot=25),
                  zlab=list("% potential abundance", cex=1.6, rot=90),
                  scales=list(arrows=F, cex=1.2),
                  drape = T, colorkey = F, outer=T,  col.regions = colorRampPalette(c("white", "dark gray"))(100),
                 screen = list (z = zgir, x = xgir), 
                 par.settings = list(axis.line = list(col = "transparent"),strip.background=list(col="transparent")),
                 par.strip.text=list(cex=2, font=2),
                 strip=strip.custom(factor.levels=mf_labeller(depvar)))}
        
        else if (n_dep==3){
        wireframe(output[,3] + output[,4] + output[,5]~ x*y, data = output, 
                  layout=lay,
                  xlab=list(mf_labeller(expname1), cex=1.6,rot=-15),
                  ylab = list(mf_labeller(expname2), cex=1.6,rot=25),
                  zlab=list("% potential abundance", cex=1.6, rot=90),
                  scales=list(arrows=F, cex=1.2),
                  drape = T, colorkey = F, outer=T,  col.regions = colorRampPalette(c("white", "dark gray"))(100),
                  screen = list (z = zgir, x = xgir), 
                  par.settings = list(axis.line = list(col = "transparent"),strip.background=list(col="transparent")),
                  par.strip.text=list(cex=2, font=2),
                  strip=strip.custom(factor.levels=mf_labeller(depvar)))}
        
        else if (n_dep==4){
        wireframe(output[,3] + output[,4] + output[,5] + output[,6] ~ x*y, data = output, 
                  layout=lay,
                  xlab=list(mf_labeller(expname1), cex=1.6,rot=-15),
                  ylab = list(mf_labeller(expname2), cex=1.6, rot=25),
                  zlab=list("% potential abundance", cex=1.6, rot=90),
                  scales=list(arrows=F, cex=1.2),
                  drape = T, colorkey = F, outer=T,  col.regions = colorRampPalette(c("white", "dark gray"))(100),
                  screen = list (z = zgir, x = xgir), 
                  par.settings = list(axis.line = list(col = "transparent"),strip.background=list(col="transparent")),
                  par.strip.text=list(cex=2, font=2), 
                  strip=strip.custom(factor.levels=mf_labeller(depvar)))}
}    
  


figura_box <- function (variable, seqvariable) {
    num_variable <- ocells[[variable]]
    expl <- ocells$Forest_Type
    if (variable == "Densi") {
        num_variable[expl=="Remov"] <- num_variable[expl=="Remov"]/1.8
    }
        
    fig_box <- ggplot() +
    geom_boxplot(aes(expl,num_variable ), fill="gray", outlier.shape = NA) +
    xlab("") + ylab("") +
    ylim(min(seqvariable), max(seqvariable)) +
    theme_bw() +
        theme(plot.margin=unit(c(0.5,0.5,0,0.5), "cm"),
              axis.text.y=element_text(size=7)) +
    coord_flip()
fig_box
}

figura_box2 <- function (variable) {
    ocells$Forest_Type <- ordered(ocells$Forest_Type, 
                                  levels = c("Unman","Prep","Regen","Remov"))
    
    ocells2 <- gather(ocells, Guild, Abun,TOTA:SECA ) %>%
        select(Forest_Type, Guild, Abun) %>%
        filter(Guild %in% all)
    
    ocells2$Guild <- ordered(ocells2$Guild, 
                                  levels = c("TOTA", "SPEA", "GENA", "UBIA",
                                             "OVNA", "CVNA",  "UNNA","GRNA",
                                             "OVFA","TRFA", "UNFA", "GRFA"))
    ocells2 <- ocells2 %>%
        filter (Guild %in% variable)
    ocells2$Guild <- droplevels (ocells2$Guild)
#        cool_names <-mf_labeller(variable)
        
    fig_box <- ggplot(ocells2) +
        geom_boxplot(aes(Forest_Type,Abun ), fill="gray", outlier.shape = NA) +
        xlab("") + ylab("") +
        ylim(min(ocells2$Abun, na.rm=T),1.1*max(ocells2$Abun, na.rm=T)) +
        facet_wrap(~Guild, labeller = as_labeller(mf_labeller)) +
        theme_bw() +
        theme(plot.margin=unit(c(0.5,0.5,0,0.5), "cm")) 
    fig_box
}


figura_univ <- function (depvar, expvar, pos="n") {    
    variables = length(depvar)
    iterations = length(expvar)
    
    output <- matrix(ncol=variables, nrow=iterations)
    expname<-deparse(substitute(expvar))
    colors<-AMZcolors(length(depvar))    
    
    
    # Create lists of parameters
    PotRichness <- rep(NA, times=length(depvar))
    DHeig_a<- rep(NA, times=length(depvar))
    DHeig_b<- rep(NA, times=length(depvar))
    Cavit_a<- rep(NA, times=length(depvar))        
    Cavit_b<- rep(NA, times=length(depvar))
    Basal_a<- rep(NA, times=length(depvar))
    Basal_b<- rep(NA, times=length(depvar))
    Shrub_a<- rep(NA, times=length(depvar))
    Shrub_b<- rep(NA, times=length(depvar))
    
    
    
    for (i in 1: length (depvar)) {
        
        
        PotRichness[i] <- parameters[( parameters$Variable==depvar[i]),14]
        DHeig_a[i] <- parameters[( parameters$Variable==depvar[i]),15]
        DHeig_b[i] <- parameters[( parameters$Variable==depvar[i]),16]
        Cavit_a[i] <- parameters[( parameters$Variable==depvar[i]),17]
        Cavit_b[i] <- parameters[( parameters$Variable==depvar[i]),18]
        Basal_a[i] <- parameters[( parameters$Variable==depvar[i]),19]
        Basal_b[i] <- parameters[( parameters$Variable==depvar[i]),20]
        Shrub_a[i] <- parameters[( parameters$Variable==depvar[i]),21]        
        Shrub_b[i] <- parameters[( parameters$Variable==depvar[i]),22]
        
        if (expname=="seq_DHeig")  {output[,i]<-tetra.complete.model(1,DHeig_a= DHeig_a[i],DHeig_b = DHeig_b[i], Cavit_a= 1000, Cavit_b=100000,Basal_a=1000,Basal_b=100000,Shrub_a=1000,Shrub_b=1000000,
                                                                     DHeig=seqDHeig, Cavit=mean(ocells$Cavit), Basal=mean(ocells$Basal), Shrub=mean(ocells$Shrub))
        }else if (expname=="seq_Cavit") {output[,i]<-tetra.complete.model(1,DHeig_a= 1,DHeig_b= 100000, Cavit_a= Cavit_a[i], Cavit_b=Cavit_b[i],Basal_a=1,Basal_b=1000000,Shrub_a=1,Shrub_b=1000000,
                                                                          DHeig=mean(ocells$DHeig),Cavit=seq_Cavit,  Basal=mean(ocells$Basal), Shrub=mean(ocells$Shrub)) 
        } else if (expname=="seq_Basal") {output[,i] <- tetra.complete.model(1,DHeig_a= 1000,DHeig_b= 1000000, Cavit_a= 1000, Cavit_b=1000000,Basal_a[i],Basal_b[i],Shrub_a=1000,Shrub_b=1000000,
                                                                             DHeig=mean(ocells$DHeig), Cavit=mean(ocells$Cavit),  Basal=seq_Basal, Shrub=mean(ocells$Shrub))
        } else if (expname=="seq_Shrub") {output[,i]<-tetra.complete.model(1,DHeig_a= 1,DHeig_b= 100000, Cavit_a= 1, Cavit_b=100000,Basal_a=1,Basal_b=1000000,Shrub_a=Shrub_a[i],Shrub_b=Shrub_b[i],
                                                                           DHeig=mean(ocells$DHeig), Cavit=mean(ocells$Cavit), Basal=mean(ocells$Basal), Shrub=seq_Shrub) }               
    }
    output <- data.frame(output)
    names(output) <- mf_labeller(depvar)
    output$exp <- expvar
    
    output <- gather(output, Species,abundance, -exp)
    
    ifelse(depvar==habitat,
           output$Species <- factor(output$Species, 
                                    levels = c("All species","Specialists","Generalists","Ubiquitous")) ,
           ifelse(depvar==nesting,
                  output$Species <- factor(output$Species, 
                                           levels = c("Canopy nesters","Cavity nesters",
                                                      "Understory nesters","Ground nesters")), 
                  output$Species <- factor(output$Species, 
                                           levels = c("Canopy foragers","Trunk foragers",
                                                      "Understory foragers","Ground foragers"))))
    
    
    
    plot_xy <- ggplot(data = output) +
        ylab("Fraction of potential abundance") +
        xlab(mf_labeller(expname)) +
        ylim(0,1) + 
        geom_line(aes(exp, abundance, color=Species), size = 1.3)+
        geom_point(aes(exp, abundance, color=Species, shape = Species), size=2)+
        scale_colour_manual(values = colors) +
        theme_bw() +
        theme(axis.title.x= element_text(face="bold"),
              legend.position = pos,
              legend.title=element_blank(),
              plot.margin=unit(c(-0.25,0.5,0.5,0.5), "cm"))
    
    plot_xy
}     
