#### Extract data (AIC, R2, Slope and parameter values) from a set of models after performing
# maximum likelihood parameter estimation and model fittig

# Aitor Ameztegui UQAM
# November 2014

rm(list=ls()) 
library(plyr)
library(purrr)

# Read files  ---------------------------------------------------------------

path <-"./Results/Model Fits/"

### Create a list with all the .txt files in the current directory, by type of model
 
all_names <- paste0(path, list.files(path,pattern = "*.txt"))

# Extract AICc, R2 and LL from models -------------------------------------
modelcomp <- ldply(all_names,function(filename)    {
    dum <- read.table(filename, skip=4, nrows=1, header=F,  sep="\t")
    dum$filename = filename
    return(dum)})

modelcomp<-modelcomp[,1:8]
names(modelcomp)<-c("LL","Param", "AICc", "AIC", "Slope", "R2","Null","File")
modelcomp$Variable<- substr(modelcomp$File, 22, 25)
modelcomp$Model<- substr(modelcomp$File, 27, 40)

write.table(modelcomp, file = "./Results/ModelComparison.txt", 
            sep="\t",  row.names = F)



# Extract parameters from the models --------------------------------------

# Create a list for each type of model
    # Pentavariant and tetravariant models
    penta_complete_names <- paste0(path, list.files(path, pattern = "penta_complete*"))
    tetra_no_Shrub_names <- paste0(path, list.files(path, pattern = "tetra_no_Shrub*"))
    tetra_no_Cavit_names <- paste0(path, list.files(path, pattern = "tetra_no_Cavit*"))
    tetra_no_DHeig_names <- paste0(path, list.files(path, pattern = "tetra_no_DHeig*"))
    tetra_no_Quadr_names <- paste0(path, list.files(path, pattern = "tetra_no_Quadr*"))
    tetra_no_Densi_names <- paste0(path, list.files(path, pattern = "tetra_no_Densi*"))
    tetra_complete_names <- paste0(path, list.files(path, pattern = "tetra_complete*"))
    
    # Trivariant models
    no_Shrub_Cavit_names <- paste0(path, list.files(path, pattern = "no_Shrub_Cavit*"))
    no_Shrub_DHeig_names <- paste0(path, list.files(path, pattern = "no_Shrub_DHeig*"))
    no_Shrub_Quadr_names <- paste0(path, list.files(path, pattern = "no_Shrub_Quadr*"))
    no_Shrub_Densi_names <- paste0(path, list.files(path, pattern = "no_Shrub_Densi*"))
    no_Cavit_DHeig_names <- paste0(path, list.files(path, pattern = "no_Cavit_DHeig*"))
    no_Cavit_Quadr_names <- paste0(path, list.files(path, pattern = "no_Cavit_Quadr*"))
    no_Cavit_Densi_names <- paste0(path, list.files(path, pattern = "no_Cavit_Densi*"))
    no_DHeig_Quadr_names <- paste0(path, list.files(path, pattern = "no_DHeig_Quadr*"))
    no_DHeig_Densi_names <- paste0(path, list.files(path, pattern = "no_DHeig_Densi*"))
    no_Quadr_Densi_names <- paste0(path, list.files(path, pattern = "no_Quadr_Densi*"))
    compl_no_Shrub_names <- paste0(path, list.files(path, pattern = "compl_no_Shrub*"))
    compl_no_Cavit_names <- paste0(path, list.files(path, pattern = "compl_no_Cavit*"))
    compl_no_DHeig_names <- paste0(path, list.files(path, pattern = "compl_no_DHeig*"))
    
    # Bivariant models
    bi_Densi_Quadr_names <- paste0(path, list.files(path, pattern = "bi_Densi_Quadr*"))
    bi_Densi_DHeig_names <- paste0(path, list.files(path, pattern = "bi_Densi_DHeig*"))
    bi_Densi_Cavit_names <- paste0(path, list.files(path, pattern = "bi_Densi_Cavit*"))
    bi_Densi_Shrub_names <- paste0(path, list.files(path, pattern = "bi_Densi_Shrub*"))
    bi_Quadr_DHeig_names <- paste0(path, list.files(path, pattern = "bi_Quadr_DHeig*"))
    bi_Quadr_Cavit_names <- paste0(path, list.files(path, pattern = "bi_Quadr_Cavit*"))
    bi_Quadr_Shrub_names <- paste0(path, list.files(path, pattern = "bi_Quadr_Shrub*"))
    bi_DHeig_Cavit_names <- paste0(path, list.files(path, pattern = "bi_DHeig_Cavit*"))
    bi_DHeig_Shrub_names <- paste0(path, list.files(path, pattern = "bi_DHeig_Shrub*"))
    bi_Cavit_Shrub_names <- paste0(path, list.files(path, pattern = "bi_Cavit_Shrub*"))
    bi_Basal_DHeig_names <- paste0(path, list.files(path, pattern = "bi_Basal_DHeig*"))
    bi_Basal_Cavit_names <- paste0(path, list.files(path, pattern = "bi_Basal_Cavit*"))
    bi_Basal_Shrub_names <- paste0(path, list.files(path, pattern = "bi_Basal_Shrub*"))
    
    # Univariant
    univaria_Basal_names <- paste0(path, list.files(path, pattern = "univaria_Basal"))
    univaria_Densi_names <- paste0(path, list.files(path, pattern = "univaria_Densi"))
    univaria_Quadr_names <- paste0(path, list.files(path, pattern = "univaria_Quadr"))
    univaria_DHeig_names <- paste0(path, list.files(path, pattern = "univaria_DHeig"))
    #univaria_Snags_names <- paste0(path, list.files(path, pattern = "univaria_Snags"))
    univaria_Cavit_names <- paste0(path, list.files(path, pattern = "univaria_Cavit"))
    univaria_Shrub_names <- paste0(path, list.files(path, pattern = "univaria_Shrub"))

    # Null model
    null_null_null_names <- paste0(path, list.files(path, pattern = "null_null_null"))


#  Read parameters from the list ------------------------------------------

## Pentavariant and tetravariant
par_penta_complete_names <- map_df(penta_complete_names, function(extract)    {
        PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
        Basal_a <- 0
        Basal_b <- 10000000
        Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
        Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
        Quadr_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
        Quadr_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
        DHeig_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
        DHeig_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
        Cavit_a <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
        Cavit_b <- read.table(extract, skip=19, nrows=1, header=F,  sep="\t")[,2]
        Shrub_a <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")[,2]
        Shrub_b <- read.table(extract, skip=21, nrows=1, header=F,  sep="\t")[,2]
        lambda <- read.table(extract, skip=22, nrows=1, header=F,  sep="\t")[,2]
        File <- extract
        parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                       Densi_a, Densi_b, Quadr_a, Quadr_b,
                                       DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                        Shrub_a,Shrub_b,lambda, "File"=File[1]))
        return(parameters) })
par_tetra_no_Shrub_names <- map_df(tetra_no_Shrub_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=19, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_tetra_no_Cavit_names <- map_df(tetra_no_Cavit_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=19, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_tetra_no_DHeig_names <-map_df(tetra_no_DHeig_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=19, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_tetra_no_Quadr_names <- map_df(tetra_no_Quadr_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=19, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_tetra_no_Densi_names <-map_df(tetra_no_Densi_names , function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=19, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_tetra_complete_names <- map_df(tetra_complete_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Basal_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=19, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })

## Trivariant
par_no_Shrub_Cavit_names <- map_df(no_Shrub_Cavit_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_Shrub_DHeig_names <- map_df(no_Shrub_DHeig_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_Shrub_Quadr_names <- map_df(no_Shrub_Quadr_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 1000000
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_Shrub_Densi_names <- map_df(no_Shrub_Densi_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_Cavit_DHeig_names <- map_df(no_Cavit_DHeig_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_Cavit_Quadr_names <- map_df(no_Cavit_Quadr_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_Cavit_Densi_names <- map_df(no_Cavit_Densi_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_DHeig_Quadr_names <- map_df(no_DHeig_Quadr_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_DHeig_Densi_names <- map_df(no_DHeig_Densi_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_no_Quadr_Densi_names <- map_df(no_Quadr_Densi_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_compl_no_Shrub_names <- map_df(compl_no_Shrub_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Basal_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_compl_no_Cavit_names <- map_df(compl_no_Cavit_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Basal_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_compl_no_DHeig_names <- map_df(compl_no_DHeig_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Basal_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=17, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=18, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })

## Bivariant

par_bi_Densi_Quadr_names <- map_df(bi_Densi_Quadr_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Densi_DHeig_names <- map_df(bi_Densi_DHeig_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Densi_Cavit_names <- map_df(bi_Densi_Cavit_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Densi_Shrub_names <- map_df(bi_Densi_Shrub_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Quadr_DHeig_names <- map_df(bi_Quadr_DHeig_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Quadr_Cavit_names <- map_df(bi_Quadr_Cavit_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Quadr_Shrub_names <- map_df(bi_Quadr_Shrub_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_DHeig_Cavit_names <- map_df(bi_DHeig_Cavit_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_DHeig_Shrub_names <- map_df(bi_DHeig_Shrub_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Cavit_Shrub_names <- map_df(bi_Cavit_Shrub_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Basal_DHeig_names <- map_df(bi_Basal_DHeig_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Basal_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Basal_Cavit_names <- map_df(bi_Basal_Cavit_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Basal_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=15, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=16, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_bi_Basal_Shrub_names <- map_df(bi_Basal_Shrub_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Basal_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=20, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=21, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=22, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })


## Univariant
par_univaria_Basal_names <- map_df(univaria_Basal_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Basal_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_univaria_Densi_names <- map_df(univaria_Densi_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Densi_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_univaria_Quadr_names <- map_df(univaria_Quadr_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Quadr_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_univaria_DHeig_names <- map_df(univaria_DHeig_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    DHeig_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_univaria_Cavit_names <- map_df(univaria_Cavit_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Cavit_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_univaria_Shrub_names <- map_df(univaria_Shrub_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    Shrub_b <- read.table(extract, skip=13, nrows=1, header=F,  sep="\t")[,2]
    lambda <- read.table(extract, skip=14, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })
par_null_null_null_names <- map_df(null_null_null_names, function(extract)    {
    PotRichness <- read.table(extract, skip=11, nrows=1, header=F,  sep="\t")[,2]
    Basal_a <- 0
    Basal_b <- 10000000
    Densi_a <- 0
    Densi_b <- 10000000
    Quadr_a <- 0
    Quadr_b <- 10000000
    DHeig_a <- 0
    DHeig_b <- 10000000
    Cavit_a <- 0
    Cavit_b <- 10000000
    Shrub_a <- 0
    Shrub_b <- 10000000
    lambda <- read.table(extract, skip=12, nrows=1, header=F,  sep="\t")[,2]
    File <- extract
    parameters <- data.frame(cbind(PotRichness,Basal_a, Basal_b,
                                   Densi_a, Densi_b, Quadr_a, Quadr_b,
                                   DHeig_a,DHeig_b,Cavit_a,Cavit_b,
                                   Shrub_a,Shrub_b,lambda, "File"=File[1]))
    return(parameters) })


parameters <- rbind(par_null_null_null_names,
                    # Pentavariant and tetravariant
                    par_penta_complete_names, par_tetra_no_Shrub_names, par_tetra_no_Cavit_names,
                    par_tetra_no_DHeig_names, par_tetra_no_Quadr_names, par_tetra_no_Densi_names, 
                    par_tetra_complete_names,
                    
                    # Trivariant
                    par_no_Shrub_Cavit_names, par_no_Shrub_DHeig_names, par_no_Shrub_Quadr_names, par_no_Shrub_Densi_names, par_no_Cavit_DHeig_names,
                    par_no_Cavit_Quadr_names, par_no_Cavit_Densi_names, par_no_DHeig_Quadr_names, par_no_DHeig_Densi_names,
                    par_no_Quadr_Densi_names, par_compl_no_Shrub_names, par_compl_no_Cavit_names, 
                    par_compl_no_DHeig_names,
                    
                    # Bivariant
                    par_bi_Densi_Quadr_names, par_bi_Densi_DHeig_names, par_bi_Densi_Cavit_names,
                    par_bi_Densi_Shrub_names, par_bi_Quadr_DHeig_names, par_bi_Quadr_Cavit_names,
                    par_bi_Quadr_Shrub_names, par_bi_DHeig_Cavit_names, par_bi_DHeig_Shrub_names,
                    par_bi_Cavit_Shrub_names, par_bi_Basal_DHeig_names, par_bi_Basal_Cavit_names,
                    par_bi_Basal_Shrub_names,
                    
                    # Univariant
                    par_univaria_Basal_names, par_univaria_Densi_names,
                    par_univaria_Quadr_names, par_univaria_DHeig_names, par_univaria_Cavit_names,
                    par_univaria_Shrub_names)
  
        parameters$Variable<- substr(parameters$File, 22, 25)
        parameters$Model<- substr(parameters$File, 27, 40)
        

write.table(parameters, file = "./Results/ModelParameters.txt", sep="\t", col.names=NA)



