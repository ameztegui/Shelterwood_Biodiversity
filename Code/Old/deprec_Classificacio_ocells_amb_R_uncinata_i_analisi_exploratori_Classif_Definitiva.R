#Date: 26th may 2016
#In this script we reclassify the species according to the definitive classifications set in may 2016
#We work with the following dataset and reclassify wrong species groups

setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Dades")
setwd("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Dades")

data_unc<-read.table("data_analyze_unc_rich_abun.txt",header=TRUE)

#Remove unnecessary categories which are in part replaced by foraging and nesting habitat classif ("GRE","GRI","PAR","EXC","SOT","SUT)
#We left the classifications regarding edge/core/inh, but we are not going to use them since in previous analyses we did not obtain straightforward results
data_unc<-data_unc[,-c(65:74)]
data_unc<-data_unc[,-c(71,72)]

#BUILT THE DATASETS FOR EACH FUNCTIONAL GROUP#######################

# Speacialization classes are based on Santos et al. 1998. Journal of Applied Ecology 35: 562-574. Jordi Camprodon and Assu Gil-Tena also supported this classification.
# Classification of nest requirements and horizontal habitat (core, edge, both) is based on Wade et al. 2013. Plosone 8(5): e64552.
##############################################################################################################################3333
#All the species
TOT<-data_unc[,c("COPA","PIVI","DRMA","DEMA","LUAR","ANTR","PRMO","TROG","ERRU","PHOC","TUTO","TUME","TUPH","TUVI","SYBO","SYAT","PHBO","PHCO","REGU","REIG","AECA","PACR","PAAT","PAMA","SIEU","CEFA","CEBR","GAGL","FRCO","SECI","LOCU","PYRA","EMCA")]

#############
#Habitat breadth (ALREADY DONE!!!!!!)
##############
#Specialists
SPE<-data_unc[,c("DRMA","DEMA","TUPH","SYBO","SYAT","PHBO","PHCO","REGU","REIG","AECA","PACR","PAAT","PAMA","SIEU","CEFA","CEBR","LOCU","PYRA")]
#Generalists
GEN<-data_unc[,c("COPA","PRMO","TROG","ERRU","TUTO","TUME","TUVI","GAGL","FRCO","SECI")]
#Ubiquitous
UBI<-data_unc[,c("PIVI","LUAR","ANTR","PHOC","EMCA")]

##############
#Nesting habitat (N SUFFIX)
##############
#Overstory
OVN<-data_unc[,c("COPA","TUPH","TUVI","REGU","REIG","GAGL","FRCO","SECI","LOCU","PYRA")]
#Tree cavity
CVN<-data_unc[,c("PIVI","DRMA","DEMA","PACR","PAAT","PAMA","SIEU","CEFA","CEBR")]
#Ground
GRN<-data_unc[,c("LUAR","ANTR","PHOC","TUTO","PHBO","EMCA")]
#Understory
UNN<-data_unc[,c("PRMO","TROG","ERRU","TUME","SYBO","SYAT","PHCO","AECA")]


##############
#Foraging habitat (F SUFFIX)
##############
#Overstory
OVF<-data_unc[,c("PHBO","PHCO","REGU","REIG","AECA","PACR","PAAT","PAMA","SIEU","CEFA","CEBR","GAGL","FRCO","SECI","LOCU","PYRA")]
#Trunk
TRF<-data_unc[,c("PIVI","DRMA","DEMA")]
#Ground
GRF<-data_unc[,c("COPA","LUAR","ANTR","PHOC","TUTO","TUME","TUPH","TUVI","EMCA")]
#Understory
UNF<-data_unc[,c("PRMO","TROG","ERRU","SYBO","SYAT")]


###############
#Cavity dependence
###############
#Non-dependence
NOC<-data_unc[,c("COPA","LUAR","ANTR","PRMO","TROG","ERRU","PHOC","TUTO","TUME","TUPH","TUVI","SYBO","SYAT","PHBO","PHCO","REGU","REIG","AECA","GAGL","FRCO","SECI","LOCU","PYRA","EMCA")]
#Excavators
EXC<-data_unc[,c("PIVI","DRMA","DEMA")]
#Secondary users
SEC<-data_unc[,c("PACR","PAAT","PAMA","SIEU","CEFA","CEBR")]

#EXC+SEC are CVN

#COMPUTE ABUNDANCE
TOT_A<-as.data.frame(rowSums(TOT))
colnames(TOT_A)[1]<-"TOTA"

SPE_A<-as.data.frame(rowSums(SPE))
colnames(SPE_A)[1]<-"SPEA"

GEN_A<-as.data.frame(rowSums(GEN))
colnames(GEN_A)[1]<-"GENA"

UBI_A<-as.data.frame(rowSums(UBI))
colnames(UBI_A)[1]<-"UBIA"

OVN_A<-as.data.frame(rowSums(OVN))
colnames(OVN_A)[1]<-"OVNA"

CVN_A<-as.data.frame(rowSums(CVN))
colnames(CVN_A)[1]<-"CVNA"

GRN_A<-as.data.frame(rowSums(GRN))
colnames(GRN_A)[1]<-"GRNA"

UNN_A<-as.data.frame(rowSums(UNN))
colnames(UNN_A)[1]<-"UNNA"


OVF_A<-as.data.frame(rowSums(OVF))
colnames(OVF_A)[1]<-"OVFA"

TRF_A<-as.data.frame(rowSums(TRF))
colnames(TRF_A)[1]<-"TRFA"

GRF_A<-as.data.frame(rowSums(GRF))
colnames(GRF_A)[1]<-"GRFA"

UNF_A<-as.data.frame(rowSums(UNF))
colnames(UNF_A)[1]<-"UNFA"


NOC_A<-as.data.frame(rowSums(NOC))
colnames(NOC_A)[1]<-"NOCA"

EXC_A<-as.data.frame(rowSums(EXC))
colnames(EXC_A)[1]<-"EXCA"

SEC_A<-as.data.frame(rowSums(SEC))
colnames(SEC_A)[1]<-"SECA"






#COMPUTE RICHNESS
TOT_R<-as.data.frame(rowSums(TOT > 0))
colnames(TOT_R)[1]<-"TOTR"

SPE_R<-as.data.frame(rowSums(SPE > 0))
colnames(SPE_R)[1]<-"SPER"

GEN_R<-as.data.frame(rowSums(GEN > 0))
colnames(GEN_R)[1]<-"GENR"

UBI_R<-as.data.frame(rowSums(UBI > 0))
colnames(UBI_R)[1]<-"UBIR"


OVN_R<-as.data.frame(rowSums(OVN > 0))
colnames(OVN_R)[1]<-"OVNR"

CVN_R<-as.data.frame(rowSums(CVN > 0))
colnames(CVN_R)[1]<-"CVNR"

GRN_R<-as.data.frame(rowSums(GRN > 0))
colnames(GRN_R)[1]<-"GRNR"

UNN_R<-as.data.frame(rowSums(UNN > 0))
colnames(UNN_R)[1]<-"UNNR"


OVF_R<-as.data.frame(rowSums(OVF > 0))
colnames(OVF_R)[1]<-"OVFR"

TRF_R<-as.data.frame(rowSums(TRF > 0))
colnames(TRF_R)[1]<-"TRFR"

GRF_R<-as.data.frame(rowSums(GRF > 0))
colnames(GRF_R)[1]<-"GRFR"

UNF_R<-as.data.frame(rowSums(UNF > 0))
colnames(UNF_R)[1]<-"UNFR"

NOC_R<-as.data.frame(rowSums(NOC > 0))
colnames(NOC_R)[1]<-"NOCR"

EXC_R<-as.data.frame(rowSums(EXC > 0))
colnames(EXC_R)[1]<-"EXCR"

SEC_R<-as.data.frame(rowSums(SEC > 0))
colnames(SEC_R)[1]<-"SECR"

#MERGE THE DATASETS AND SAVE IN A .TXT
#Already done
VDEP<-cbind(TOT_R,TOT_A,GEN_R,GEN_A,SPE_R,SPE_A,UBI_R,UBI_AEDG_R,EDG_A,COR_R,COR_A,INH_R,INH_A)
NEWVDEP<-cbind(OVN_R,OVN_A,CVN_R,CVN_A,GRN_R,GRN_A,UNN_R,UNN_A,OVF_R,OVF_A,TRF_R,TRF_A,GRF_R,GRF_A,UNF_R,UNF_A,NOC_R,NOC_A,EXC_R,EXC_A,SEC_R,SEC_A)
data_unc_newvdep<-cbind(data_unc,NEWVDEP)


write.table(data_unc_newvdep,"data_analyze_unc_rich_abun_may_2016.txt",quote=FALSE,row.names=FALSE)

data<-data_unc_newvdep

#HISTOGRAMES VARIABLES UNCINATA
setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/hist_v_dependents/noves_classificacions/gremis/Uncinata")
setwd("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/hist_v_dependents/noves_classificacions/gremis/Uncinata")
data<-read.table("data_analyze_unc_rich_abun_may_2016.txt",header=TRUE)

#histogrames riquesa, abundancia
#already done
ilist<-seq(57,70,1)

#new variables
ilist<-seq(71,92,1)
for(i in ilist)
{
jpeg(filename=paste("hist_",names(data[i]),".jpg",sep=""))
hist(data[data$Especie=="Uncinata",i],main="",xlab=paste(names(data[i])))
dev.off()
}


#Already done
setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/hist_v_dependents/noves_classificacions/especies/Uncinata")
#histogrames especies
ilist<-seq(5,37,1)
for(i in ilist)
{
jpeg(filename=paste("hist_",names(data[i]),".jpg",sep=""))
hist(data[data$Especie=="Uncinata",i],main="",xlab=paste(names(data[i])))
dev.off()
}

#Already done
setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/histograms_v_independents/Uncinata")
#histogrames v. independents
ilist<-seq(38,56,1)
for(i in ilist)
{
jpeg(filename=paste("hist_",names(data[i]),".jpg",sep=""))
hist(data[data$Especie=="Uncinata",i],main="",xlab=paste(names(data[i])))
dev.off()
}

ilist<-seq(38,56,1)
summary.results <- data.frame(matrix(nrow = length(ilist), ncol = 4))
colnames(summary.results) <- c('Variable', 'Mean','SD','Especie')
u<-1
for(i in ilist)
{
  summary.results[u,'Especie'] <-"Uncinata"
  summary.results[u,'Variable'] <- names(data[i])
  summary.results[u,'Mean'] <- mean(data[data$Especie=="Uncinata",i])
  summary.results[u,'SD'] <- sd(data[data$Especie=="Uncinata",i])
  u<-u+1

}
write.csv(summary.results,"summary.results.csv")

#correlacions
write.csv(cor(data[,c(38:56)],method="spearman"),"cor_vindep.csv",row.names=FALSE)


#SCATTERPLOT a nivell gremi
setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/scatterplots/Uncinata")
setwd("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/scatterplots/Uncinata")

#Already done
vdp_ilist<-seq(57,70,1)

vdp_ilist<-seq(71,92,1)
vindp_ilist<-seq(38,56,1)
for(ivdp in vdp_ilist)
{
for(ivindp in vindp_ilist)
{
jpeg(filename=paste(names(data[ivdp]),"_",names(data[ivindp]),".jpg",sep=""))
plot(data[data$Especie=="Uncinata",ivdp]~data[data$Especie=="Uncinata",ivindp],ylab=names(data[ivdp]),xlab=names(data[ivindp]))
dev.off()
}
}

#Not done now with the new classifs
#correlacions
vdp_ilist<-seq(57,82,1)
gremis_h_cor <- data.frame(matrix(nrow = length(vdp_ilist), ncol = 3))
colnames(gremis_h_cor) <- c('Response_Variable', 'cor','t')
u<-1

for(ivdp in vdp_ilist)
{
  
  gremis_h_cor[u,'Response_Variable']<-names(data[ivdp])
  gremis_h_cor[u,'cor']<-cor.test(data[data$Especie=="Uncinata",ivdp],data[data$Especie=="Uncinata",4],method="spearman")[4]
  gremis_h_cor[u,'t']<-cor.test(data[data$Especie=="Uncinata",ivdp],data[data$Especie=="Uncinata",4],method="spearman")[3]
  u<-u+1
  
}
write.csv(sp_h_cor,"gremis_h_cor2_gremis.csv",row.names=FALSE)



#SCATTERPLOT a nivell espècie ocell

setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/scatterplots/Especies_ocells/Uncinata")
vdp_ilist<-seq(5,37,1)
vindp_ilist<-seq(38,56,1)
for(ivdp in vdp_ilist)
{
for(ivindp in vindp_ilist)
{
jpeg(filename=paste(names(data[ivdp]),"_",names(data[ivindp]),".jpg",sep=""))
plot(data[data$Especie=="Uncinata",ivdp]~data[data$Especie=="Uncinata",ivindp],ylab=names(data[ivdp]),xylab=names(data[ivindp]))
dev.off()
}
}


#SCATTERPLOT a nivell espècie ocell & altitud

setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/scatterplots/Especies_ocells/Uncinata")
vdp_ilist<-seq(5,37,1)
for(ivdp in vdp_ilist)
{
    jpeg(filename=paste(names(data[ivdp]),"_ALTITUD",".jpg",sep=""))
    plot(data[data$Especie=="Uncinata",ivdp]~data[data$Especie=="Uncinata",4],ylab=names(data[ivdp]),xlab=names(data[4]))
    dev.off()
  }

#correlacions
vdp_ilist<-seq(5,37,1)
sp_h_cor <- data.frame(matrix(nrow = length(vdp_ilist), ncol = 3))
colnames(sp_h_cor) <- c('Response_Variable', 'cor','t')
u<-1


for(ivdp in vdp_ilist)
{
  
  sp_h_cor[u,'Response_Variable']<-names(data[ivdp])
  sp_h_cor[u,'cor']<-cor.test(data[data$Especie=="Uncinata",ivdp],data[data$Especie=="Uncinata",4],method="spearman")[4]
  sp_h_cor[u,'t']<-cor.test(data[data$Especie=="Uncinata",ivdp],data[data$Especie=="Uncinata",4],method="spearman")[3]
u<-u+1

}
write.csv(sp_h_cor,"sp_h_cor2.csv",row.names=FALSE)


library(mgcv)


setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata")
setwd("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata")

#Per gremis
#Already done
#vdp_ilist<-seq(57,70,1)

vdp_ilist<-seq(71,92,1)
vindp_ilist<-seq(38,56,1)
for(ivdp in vdp_ilist)
{
for(ivindp in vindp_ilist)
{
jpeg(filename=paste(names(data[ivdp]),"_",names(data[ivindp]),".jpg",sep=""))
plot(gam(data[data$Especie=="Uncinata",ivdp]~s(data[data$Especie=="Uncinata",ivindp],k=4),data=data,family=poisson),xlab=names(data[ivindp]),ylab=names(data[ivdp]))
dev.off()
}
}

#Already done
#vdp_ilist<-seq(57,70,1)

vdp_ilist<-seq(71,92,1)
vindp_ilist<-seq(38,56,1)
AIC.gams <- data.frame(matrix(nrow = ((length(vdp_ilist)*length(vindp_ilist))+length(vdp_ilist)), ncol = 4))
colnames(AIC.gams) <- c('Response_Variable', 'Predictor_Variable','AIC','Especie','r-sq')
u<-1
for(ivdp in vdp_ilist)
{
for(ivindp in vindp_ilist)
{
AIC.gams[u,'Response_Variable']<-names(data[ivdp])
AIC.gams[u,'Predictor_Variable']<-"Null"
AIC.gams[u,'Especie']<-"Uncinata"
AIC.gams[u,'AIC']<-AIC(gam(data[data$Especie=="Uncinata",ivdp]~1,data=data,family=poisson))
AIC.gams[u,'r-sq']<-summary(gam(data[data$Especie=="Uncinata",ivdp]~1,data=data,family=poisson))[10]
u<-u+1
AIC.gams[u,'Response_Variable']<-names(data[ivdp])
AIC.gams[u,'Predictor_Variable']<-names(data[ivindp])
AIC.gams[u,'Especie']<-"Uncinata"
AIC.gams[u,'AIC']<-AIC(gam(data[data$Especie=="Uncinata",ivdp]~s(data[data$Especie=="Uncinata",ivindp],k=4),data=data,family=poisson))
AIC.gams[u,'r-sq']<-summary(gam(data[data$Especie=="Uncinata",ivdp]~s(data[data$Especie=="Uncinata",ivindp],k=4),data=data,family=poisson))[10]
u<-u+1
}
}

write.csv(AIC.gams,"AICgams.csv")

#Not done now
#GAMs amb variables indepependents transformades (peusmha, cavtotal, cavpic, estaques)
setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/vindep_transform")

vdp_ilist<-seq(57,82,1)
vindp_ilist<-c(40,41,42,56)
for(ivdp in vdp_ilist)
{
  for(ivindp in vindp_ilist)
  {
    jpeg(filename=paste(names(data[ivdp]),"_",names(data[ivindp]),"logT.jpg",sep=""))
    plot(gam(data[data$Especie=="Uncinata",ivdp]~s(log(data[data$Especie=="Uncinata",ivindp]+1),k=4),data=data,family=poisson),xlab=names(data[ivindp]),ylab=names(data[ivdp]))
    dev.off()
  }
}

#windows()
pdf("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/SPEA_raw_gam.pdf")
par(mfrow=c(2,2),cex=0.7)
plot(gam(SPEA~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(RECARBUSTIU,k=4),data=data[data$Especie=="Uncinata",],family=poisson))
dev.off()

SPEA_GAM<-gam(SPEA~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(RECARBUSTIU,k=4),data=data[data$Especie=="Uncinata",],family=poisson)
summary(SPEA_GAM)

pdf("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/SPEA_response_gam.pdf")
par(mfrow=c(2,2),mar=c(3,3.5,1,0), mai = c(0.6, 0.6, 0.2, 0.2),cex=0.7)
plot(data$ALCDOM,fitted(SPEA_GAM))
plot(data$G_m2_ha,fitted(SPEA_GAM))
plot(data$CAVTOTAL,fitted(SPEA_GAM))
plot(data$RECARBUSTIU,fitted(SPEA_GAM))
dev.off()

pdf("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/GREA_raw_gam.pdf")
par(mfrow=c(2,2),cex=0.7)
plot(gam(GREA~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(RECARBUSTIU,k=4),data=data[data$Especie=="Uncinata",],family=poisson),pages=1,seWithMean=TRUE)
dev.off()

GREA_GAM<-gam(GREA~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(RECARBUSTIU,k=4),data=data[data$Especie=="Uncinata",],family=poisson)
summary(GREA_GAM)

pdf("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/GREA_response_gam.pdf")
par(mfrow=c(2,2),mar=c(3,3.5,1,0), mai = c(0.6, 0.6, 0.2, 0.2),cex=0.7)
plot(data$ALCDOM,fitted(GREA_GAM))
plot(data$G_m2_ha,fitted(GREA_GAM))
plot(data$CAVTOTAL,fitted(GREA_GAM))
plot(data$RECARBUSTIU,fitted(GREA_GAM))
dev.off()

pdf("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/UBIA_raw_gam.pdf")
par(mfrow=c(2,2),cex=0.7)
plot(gam(UBIA~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(RECARBUSTIU,k=4),data=data[data$Especie=="Uncinata",],family=poisson),pages=1,seWithMean=TRUE)
dev.off()

UBIA_GAM<-gam(UBIA~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(RECARBUSTIU,k=4),data=data[data$Especie=="Uncinata",],family=poisson)
summary(UBIA_GAM)

pdf("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/UBIA_response_gam.pdf")
par(mfrow=c(2,2),mar=c(3,3.5,1,0), mai = c(0.6, 0.6, 0.2, 0.2),cex=0.7)
plot(data$ALCDOM,fitted(UBIA_GAM))
plot(data$G_m2_ha,fitted(UBIA_GAM))
plot(data$CAVTOTAL,fitted(UBIA_GAM))
plot(data$RECARBUSTIU,fitted(UBIA_GAM))
dev.off()

vdep<-c("OVNA","CVNA","GRNA","UNNA","OVFA","TRFA","GRFA","UNFA","NOCA","EXCA","SECA")

for(i in 1:length(vdep)){
pdf(paste("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/",vdep[i],"_raw_gam.pdf",sep=""))
par(mfrow=c(2,2),cex=0.7,mai=c(0.8, 0.7, 0.2, 0.1))
plot(gam(data[,vdep[i]]~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(RECARBUSTIU,k=4),data=data,family=poisson))
dev.off()

GAM<-gam(data[,vdep[i]]~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(RECARBUSTIU,k=4),data=data,family=poisson)
summary(GAM)


pdf(paste("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/",vdep[i],"_response_gam.pdf",sep=""))
par(mfrow=c(2,2),mar=c(3,3.5,1,0), mai = c(0.6, 0.6, 0.2, 0.2),cex=0.7)
plot(data$ALCDOM,fitted(GAM))
plot(data$G_m2_ha,fitted(GAM))
plot(data$CAVTOTAL,fitted(GAM))
plot(data$RECARBUSTIU,fitted(GAM))
dev.off()
}


vdep<-c("SPEA","GENA","UBIA","TOTA","OVNA","CVNA","GRNA","UNNA","OVFA","TRFA","GRFA","UNFA","NOCA","EXCA","SECA")

vindep<-c("RECARBUSTIU","RECHERB")
for(i in 1:length(vdep)){
  for(t in 1:length(vindep)){
GAM<-gam(data[,vdep[i]]~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(data[,vindep[t]],k=4),data=data,family=poisson)
summary(GAM)

comparing<-as.data.frame(summary(GAM)[10])
comparing$vdepent<-vdep[i]
comparing$vindepent<-vindep[t]
if(!exists("comparing_all"))
  comparing_all<-comparing
else
  comparing_all<-rbind(comparing_all,comparing)
}
}


vindep<-c("CAVTOTAL","PEUSMHA")
for(i in 1:length(vdep)){
  for(t in 1:length(vindep)){
    GAM<-gam(data[,vdep[i]]~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(data[,vindep[t]],k=4)+s(RECARBUSTIU,k=4),data=data,family=poisson)
    summary(GAM)
    
    comparing<-as.data.frame(summary(GAM)[10])
    comparing$vdepent<-vdep[i]
    comparing$vindepent<-vindep[t]
    if(!exists("comparing_all"))
      comparing_all<-comparing
    else
      comparing_all<-rbind(comparing_all,comparing)
  }
}
write.csv(comparing_all,"comparing_all.csv",row.names=FALSE)
#With Dfina and not RECARBUSTIU
for(i in 1:length(vdep)){
  pdf(paste("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/",vdep[i],"Dfina_raw_gam.pdf",sep=""))
  par(mfrow=c(2,2),cex=0.7,mai=c(0.8, 0.7, 0.2, 0.1))
  plot(gam(data[,vdep[i]]~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(Dfina,k=4),data=data,family=poisson))
  dev.off()
  
  GAM<-gam(data[,vdep[i]]~s(ALCDOM,k=4)+s(G_m2_ha,k=4)+s(CAVTOTAL,k=4)+s(Dfina,k=4),data=data,family=poisson)
  summary(GAM)
  
  pdf(paste("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Resultats/gam_results/Uncinata/",vdep[i],"Dfina_response_gam.pdf",sep=""))
  par(mfrow=c(2,2),mar=c(3,3.5,1,0), mai = c(0.6, 0.6, 0.2, 0.2),cex=0.7)
  plot(data$ALCDOM,fitted(GAM))
  plot(data$G_m2_ha,fitted(GAM))
  plot(data$CAVTOTAL,fitted(GAM))
  plot(data$Dfina,fitted(GAM))
  dev.off()
}






setwd("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Dades")

windows()
par(mfrow=c(2,4))
boxplot(data$TOTA~factor(data$ESTRUCTURA),main="Total",names=c("Unman","Mid-aged","Seeding C","Final C"),ylab="Bird abundance",ylim=c(0,30))
boxplot(data$SPEA~factor(data$ESTRUCTURA),main="Specialists",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,30))
boxplot(data$GENA~factor(data$ESTRUCTURA),main="Generalists",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,30))
boxplot(data$UBIA~factor(data$ESTRUCTURA),main="Ubiquists",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,30))
boxplot(data$CVNA~factor(data$ESTRUCTURA),main="Cavity Nesters",names=c("Unman","Mid-aged","Seeding C","Final C"),ylab="Bird abundance",ylim=c(0,20))
boxplot(data$OVNA~factor(data$ESTRUCTURA),main="Overstory Nesters",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,20))
boxplot(data$UNNA~factor(data$ESTRUCTURA),main="Understory Nesters",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,20))
boxplot(data$GRNA~factor(data$ESTRUCTURA),main="Ground Nesters",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,20))


windows()
par(mfrow=c(2,4))
boxplot(data$NOCA~factor(data$ESTRUCTURA),main="Non-cavity dependents",names=c("Unman","Mid-aged","Seeding C","Final C"),ylab="Bird abundance",ylim=c(0,25))
boxplot(data$EXCA~factor(data$ESTRUCTURA),main="Excavators",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,25))
boxplot(data$SECA~factor(data$ESTRUCTURA),main="Secondary users",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,25))
plot(0,type='n',axes=FALSE,ann=FALSE)
boxplot(data$TRFA~factor(data$ESTRUCTURA),main="Trunk Foragers",names=c("Unman","Mid-aged","Seeding C","Final C"),ylab="Bird abundance",ylim=c(0,25))
boxplot(data$OVFA~factor(data$ESTRUCTURA),main="Overstory Foragers",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,25))
boxplot(data$UNFA~factor(data$ESTRUCTURA),main="Understory Foragers",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,25))
boxplot(data$GRFA~factor(data$ESTRUCTURA),main="Ground Foragers",names=c("Unman","Mid-aged","Seeding C","Final C"),ylim=c(0,25))



windows()
par(mfrow=c(3,2))
boxplot(data$G_m2_ha~factor(data$ESTRUCTURA),main="Basal area",ylab="m2/ha",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=TRUE)
boxplot(data$ALCDOM~factor(data$ESTRUCTURA),main="Top height",ylab="m",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=TRUE)
boxplot(data$CAVTOTAL~factor(data$ESTRUCTURA),main="Total cavities",ylab="#",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=TRUE)
boxplot(data$PEUSMHA~factor(data$ESTRUCTURA),main="Snags",ylab="#",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=TRUE)
boxplot(data$RECARBUSTIU~factor(data$ESTRUCTURA),main="Shrub cover", ylab="%",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=TRUE)
boxplot(data$ALT~factor(data$ESTRUCTURA),main="Elevation", ylab="m",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=TRUE)

#Figure for the appendix
windows()
par(mfrow=c(2,2))
boxplot(data$G_m2_ha~factor(data$ESTRUCTURA),main="Basal area",ylab="m2/ha",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=FALSE)
boxplot(data$ALCDOM~factor(data$ESTRUCTURA),main="Dominant height",ylab="m",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=FALSE)
boxplot(data$CAVTOTAL~factor(data$ESTRUCTURA),main="Total cavities",ylab="#",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=FALSE)
boxplot(data$RECARBUSTIU~factor(data$ESTRUCTURA),main="Shrub cover", ylab="%",names=c("Unman","Mid-aged","Seeding C","Final C"),notch=FALSE)


setwd("//serverprocess/Assu/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Dades")
setwd("F:/Estada_canada_2014_2015/Forest_birds_stand_level_analysis/Dades")

anova(lm(data$ALT~factor(data$ESTRUCTURA)))
anova(lm(data$G_m2_ha~factor(data$ESTRUCTURA)))
anova(lm(data$ALCDOM~factor(data$ESTRUCTURA)))
anova(lm(data$RECARBUSTIU~factor(data$ESTRUCTURA)))
anova(glm(formula=round(data$CAVTOTAL,0)~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=round(data$PEUSMHA,0)~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")



anova(glm(formula=data$TOTA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$SPEA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$GENA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$UBIA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")

anova(glm(formula=data$OVNA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$CVNA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$GRNA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$UNNA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")

anova(glm(formula=data$OVFA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$TRFA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$GRFA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$UNFA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")


anova(glm(formula=data$NOCA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$EXCA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")
anova(glm(formula=data$SECA~factor(data$ESTRUCTURA),family="poisson"),test="Chisq")

write.csv(data,"data_analyze_unc_rich_abun_may_2016.csv",row.names=F)


forest_struc<-levels(factor(data$ESTRUCTURA))
vindp<-c("ALT","DT","G_m2_ha","ALCDOM","CAVTOTAL","PEUSMHA","RECHERB","RECARBUSTIU","p_G_gruesa","SD_maderaFMG")
  for(ivindp in 1:length(vindp)){
    for(ifs in 1:length(forest_struc)){
      
structure<-as.data.frame(mean(data[data$ESTRUCTURA==forest_struc[ifs],vindp[ivindp]]))
colnames(structure)[1]<-"mean"
structure$variable<-vindp[ivindp]
structure$tallada<-forest_struc[ifs]
structure<-structure[,c(2,3,1)]
structure$sd<-sd(data[data$ESTRUCTURA==forest_struc[ifs],vindp[ivindp]])

if(!exists("structure_all"))
  structure_all<-structure
else
  structure_all<-rbind(structure_all,structure)
}
}

summary(data[,c(4,5,38:56)])

data<-read.table("data_analyze_unc_rich_abun_may_2016.txt",header=TRUE)

cor.test(data$TOTA,data$TOTR,method="spearman")[4]
#rho=0.7231109 
cor.test(data$SPEA,data$SPER,method="spearman")[4]
#rho=0.7459361 
cor.test(data$GENA,data$GENR,method="spearman")[4]
#rho=0.7623222 
cor.test(data$UBIA,data$UBIR,method="spearman")[4]
#rho=0.9925067

cor.test(data$CVNA,data$CVNR,method="spearman")[4]
#rho=0.6290853 
cor.test(data$OVNA,data$OVNR,method="spearman")[4]
#0.7386047 
cor.test(data$GRNA,data$GRNR,method="spearman")[4]
#0.9748859
cor.test(data$UNNA,data$UNNR,method="spearman")[4]
#0.8864746 

orientacions<-read.csv("Data_orientacions.csv",header=TRUE)
data_ori<-merge(data,orientacions,by="CODI",all.x=TRUE)


windows()
par(mfrow=c(2,4))
boxplot(data_ori$TOTA~factor(data_ori$ORI_RECLASS),main="Total",ylab="Bird abundance",ylim=c(0,30),varwidth=T)
boxplot(data_ori$SPEA~factor(data_ori$ORI_RECLASS),main="Specialists",ylim=c(0,30),varwidth=T)
boxplot(data_ori$GENA~factor(data_ori$ORI_RECLASS),main="Generalists",ylim=c(0,30),varwidth=T)
boxplot(data_ori$UBIA~factor(data_ori$ORI_RECLASS),main="Ubiquists",ylim=c(0,30),varwidth=T)
boxplot(data_ori$CVNA~factor(data_ori$ORI_RECLASS),main="Cavity Nesters",ylab="Bird abundance",ylim=c(0,20),varwidth=T)
boxplot(data_ori$OVNA~factor(data_ori$ORI_RECLASS),main="Overstory Nesters",ylim=c(0,20),varwidth=T)
boxplot(data_ori$UNNA~factor(data_ori$ORI_RECLASS),main="Understory Nesters",ylim=c(0,20),varwidth=T)
boxplot(data_ori$GRNA~factor(data_ori$ORI_RECLASS),main="Ground Nesters",ylim=c(0,20),varwidth=T)


windows()
par(mfrow=c(1,4))
boxplot(data_ori$TRFA~factor(data_ori$ORI_RECLASS),main="Cavity Nesters",ylab="Bird abundance",ylim=c(0,20),varwidth=T)
boxplot(data_ori$OVFA~factor(data_ori$ORI_RECLASS),main="Overstory Nesters",ylim=c(0,20),varwidth=T)
boxplot(data_ori$UNFA~factor(data_ori$ORI_RECLASS),main="Understory Nesters",ylim=c(0,20),varwidth=T)
boxplot(data_ori$GRFA~factor(data_ori$ORI_RECLASS),main="Ground Nesters",ylim=c(0,20),varwidth=T)

vdep<-c("TOTA","SPEA","GENA","UBIA","CVNA","OVNA","UNNA","GRNA")
for(i in 1:length(vdep)){
rho<-as.data.frame(cor.test(data[,vdep[i]],data$ALT,method="spearman")[4])
rho$vdep<-vdep[i]
rho<-rho[,c(2,1)]
colnames(rho)[2]<-"rho_tot"
rho$rho_snsBV<-cor.test(data[!data$ESTRUCTURA=="BV",vdep[i]],data[!data$ESTRUCTURA=="BV",]$ALT,method="spearman")[4]

if(!exists("rho_all"))
  rho_all<-rho
else
  rho_all<-rbind(rho_all,rho)
}

