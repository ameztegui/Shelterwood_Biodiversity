#### Extract data (AIC, R2, Slope and parameter values) from a set of models after performing
# maximum likelihood parameter estimation and model fittig

# Aitor Ameztegui UQAM
# November 2014
rm(list=ls()) 
library(plyr)




# Abundance ---------------------------------------------------------------


### Create a list with all the .txt files in the current directory

path <-"./Results/Model Fits/Abu3/"

files <-  list.files(path,pattern = "*.txt")

# Divide according to site/pool, null/full and size/inde
fullfiles <- list.files(path, pattern = "full.*")
nohofiles <- list.files(path,pattern = "noho.*")
nodwfiles <- list.files(path,pattern = "nodw.*")
nomgfiles <- list.files(path,pattern = "nomg.*")
nocvfiles <- list.files(path,pattern = "nocv.*")
nobafiles <- list.files(path,pattern = "noba.*")
noshfiles <- list.files(path,pattern = "nosh.*")
nullfiles <- list.files(path,pattern = "null.*")

all_names <- paste0(path, files)
fullnames <- paste0(path, fullfiles)
nohonames <- paste0(path, nohofiles)
nodwnames <- paste0(path, nodwfiles)
nomgnames <- paste0(path, nomgfiles)
nocvnames <- paste0(path, nocvfiles)
nobanames <- paste0(path, nobafiles)
noshnames <- paste0(path, noshfiles)
nullnames <- paste0(path, nullfiles)

# Extract AICc, R2 and LL from models -------------------------------------

modelcomp <- ldply(all_names,function(filename)    {
        dum <- read.table(filename, skip=4, nrows=1, header=F,  sep="\t")
        dum$filename = filename
        return(dum)})

modelcomp<-modelcomp[,1:8]
        names(modelcomp)<-c("LL","Param", "AICc", "AIC", "Slope", "R2","Null","File")
        modelcomp$Variable<- substr(modelcomp$File, 27, 30)
        modelcomp$PDF <- substr(modelcomp$File, 32, 36)
        modelcomp$Model<- substr(modelcomp$File, 43, 52)

write.table(modelcomp, file = "./Results/ModelComparison_Abu3.txt", sep="\t", col.names=NA)



# Extract parameters from the models --------------------------------------


par_pool_full <- ldply(fullnames,function(extract)    {
        PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")
        HOa <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")
        HOb <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")
        DWa <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")
        DWb <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")
        MGa <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")
        MGb <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")
        SHa <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")
        SHb <- read.table(extract, skip=19, nrows=1, header=F,  sep="\t")
        lambda <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")
        File <- extract
        parameters <- cbind(PotRichness[,2],HOa[,2],HOb[,2],DWa[,2],DWb[,2],
                           MGa[,2],MGb[,2], SHa[,2],SHb[,2],lambda[,2], File[1])
        return(parameters) })


par_pool_noho <- ldply(nohonames,function(extract)    {
        PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")
        DWa <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")
        DWb <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")
        MGa <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")
        MGb <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")
        SHa <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")
        SHb <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")
        lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")
        File <- extract
        parameters <- cbind(PotRichness[,2],0,10000000,DWa[,2],DWb[,2],
                            MGa[,2],MGb[,2],SHa[,2],SHb[,2],lambda[,2], File[1])
        return(parameters) })

par_pool_nodw <- ldply(nodwnames,function(extract)    {
        PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")
        HOa <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")
        HOb <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")
        MGa <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")
        MGb <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")
        SHa <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")
        SHb <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")
        lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")
        File <- extract
        parameters <- cbind(PotRichness[,2],HOa[,2],HOb[,2],0,10000000,
                            MGa[,2],MGb[,2],SHa[,2],SHb[,2],lambda[,2], File[1])
        return(parameters) })


par_pool_nomg <- ldply(nomgnames,function(extract)    {
        PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")
        HOa <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")
        HOb <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")
        DWa <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")
        DWb <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")
        SHa <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")
        SHb <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")
        lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")
        File <- extract
        parameters <- cbind(PotRichness[,2],HOa[,2],HOb[,2],DWa[,2],DWb[,2],
                            0,10000000, SHa[,2],SHb[,2],lambda[,2], File[1])
        return(parameters) })

par_pool_nosh <- ldply(noshnames,function(extract)    {
        PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")
        HOa <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")
        HOb <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")
        DWa <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")
        DWb <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")
        MGa <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")
        MGb <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")
        lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")
        File <- extract
        parameters <- cbind(PotRichness[,2],HOa[,2],HOb[,2],DWa[,2],DWb[,2],
                            MGa[,2],MGb[,2],0,10000000,lambda[,2], File[1])
        return(parameters) })


par_pool_null <- ldply(nullnames,function(extract)    {
        PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")
        File <- extract
        parameters <- cbind(PotRichness[,2],0,10000000,0,10000000,
                            0,10000000,0,10000000,10000, File[1])
        return(parameters) })

parameters <- rbind(par_pool_full, par_pool_noho, par_pool_nodw,
                    par_pool_nomg, par_pool_nosh,par_pool_null)
        names(parameters)<-c("PotRichness","HOa","HOb","DWa", "DWb",
                             "MGa", "MGb","SHa","SHb","Lambda","File")
        parameters$Variable<- substr(parameters$File, 27, 30)
        parameters$PDF<- substr(parameters$File, 32, 36)
        parameters$Model<- substr(parameters$File, 43, 52)


write.table(parameters, file = "./Results/ModelParameters_Abu3.txt", sep="\t", col.names=NA)



