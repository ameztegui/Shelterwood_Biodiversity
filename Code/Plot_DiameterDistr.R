# Script to generate the dia<meter distribution for all plots of Pinus uncinata used in the sampling
# Script created by Aitor Ameztegui, CREAF-CTFC
#
# ameztegui@gmail.com
#
# July 2016
#####_______________________


rm(list=ls())

ocellets <- read.delim("D:/Dropbox (FiDBosc)/Research/Forest_birds_stand_level_analysis/Dades/Puncinata_Silvan_Aitor_modificat.txt")

ocellets_long <- gather(ocellets, CD, dens, CD10:CD75)
ocellets_long$CD <- as.factor(ocellets_long$CD)

fm <- ocellets_long[ocellets_long$ESTRUCTURA=="FM",]
td <- ocellets_long[ocellets_long$ESTRUCTURA=="TD",]
tf <- ocellets_long[ocellets_long$ESTRUCTURA=="TF",]
bv <- ocellets_long[ocellets_long$ESTRUCTURA=="BV",]

fm$CODI <- factor(fm$CODI)
td$CODI <- factor(td$CODI)
tf$CODI <- factor(tf$CODI)
bv$CODI <- factor(bv$CODI)


plot_fm = list()
plot_td = list()
plot_tf = list()
plot_bv = list()

# Make plots.

# Fustal medio
    for (i in levels(fm$CODI)) {
        fm_data <- fm[fm$CODI==i,]    
        lb_g <- paste("G = ", mean(fm_data$G_m2_ha),"m2/ha")
        lb_n <- paste("N = ", mean(fm_data$DT),"peus/ha")
        lb_e <- paste("Age =", mean(fm_data$EDAT), "years")
        lb_h <- paste("Ho =", mean(fm_data$ALCDOM), "m")
        
        
        fig <- ggplot(aes(x=CD, y=dens), data=fm_data) +
            geom_bar(stat="identity") +
            ggtitle(i) +
            ylab("Stem density") +
            annotate("text", x=13, y = 0.95*max(fm_data$dens), label=lb_g) +
            annotate("text", x=13, y = 0.9*max(fm_data$dens), label=lb_n) +
            annotate("text", x=13, y = 0.85*max(fm_data$dens), label=lb_e) +
            annotate("text", x=13, y = 0.8*max(fm_data$dens), label=lb_h)
        
        
        plot_fm[[i]] = fig
    }

# Tallades disseminatories
    for (i in levels(td$CODI)) {
        td_data <- td[td$CODI==i,]
        lb_g <- paste("G = ", mean(td_data$G_m2_ha), "m2/ha")
        lb_n <- paste("N = ", mean(td_data$DT),"peus/ha")
        lb_e <- paste("Age =", mean(td_data$EDAT),"years")
        lb_h <- paste("Ho =", mean(td_data$ALCDOM), "m")
        
        
        fig <- ggplot(aes(x=CD, y=dens), data=td_data) +
            geom_bar(stat="identity") +
            ggtitle(i) +
            ylab("Stem density") +
            annotate("text", x=13, y = 0.95*max(td_data$dens), label=lb_g) +
            annotate("text", x=13, y = 0.9*max(td_data$dens), label=lb_n) +
            annotate("text", x=13, y = 0.85*max(td_data$dens), label=lb_e) +
            annotate("text", x=13, y = 0.8*max(td_data$dens), label=lb_h)
        
        
        plot_td[[i]] = fig
    }


# Tallades finals
    for (i in levels(tf$CODI)) {
        tf_data <- tf[tf$CODI==i,] 
        lb_g <- paste("G = ", mean(tf_data$G_m2_ha), "m2/ha")
        lb_n <- paste("N = ", mean(tf_data$DT),"peus/ha")
        lb_e <- paste("Age =", mean(tf_data$EDAT),"years")
        lb_h <- paste("Ho =", mean(tf_data$ALCDOM), "m")
        
    
        fig <- ggplot(aes(x=CD, y=dens), data=tf_data) +
            geom_bar(stat="identity") +
            ggtitle(i) +
            ylab("Stem density") +
            annotate("text", x=13, y = 0.95*max(tf_data$dens), label=lb_g) +
            annotate("text", x=13, y = 0.9*max(tf_data$dens), label=lb_n) +
            annotate("text", x=13, y = 0.85*max(tf_data$dens), label=lb_e) +
            annotate("text", x=13, y = 0.8*max(tf_data$dens), label=lb_h)
        
        
        
        plot_tf[[i]] = fig
    }

# Bosc vell
    for (i in levels(bv$CODI)) {
        bv_data <- bv[bv$CODI==i,] 
        lb_g <- paste("G = ", mean(bv_data$G_m2_ha), "m2/ha")
        lb_n <- paste("N = ", mean(bv_data$DT),"peus/ha")
        lb_e <- paste("Age =", mean(bv_data$EDAT),"years")
        lb_h <- paste("Ho =", mean(bv_data$ALCDOM), "m")
        
        
        fig <- ggplot(aes(x=CD, y=dens), data=bv_data) +
            geom_bar(stat="identity") +
            ggtitle(i) +
            ylab("Stem density") +
            annotate("text", x=13, y = 0.95*max(bv_data$dens), label=lb_g) +
            annotate("text", x=13, y = 0.9*max(bv_data$dens), label=lb_n) +
            annotate("text", x=13, y = 0.85*max(bv_data$dens), label=lb_e) +
            annotate("text", x=13, y = 0.8*max(bv_data$dens), label=lb_h)
        
        
        
        plot_bv[[i]] = fig
    }



# Create pdf where each page is a separate plot.
    pdf("./Figures/Exploratory/FM.pdf", width=8)
        for (i in levels(fm$CODI)) {
            print(plot_fm[[i]])
        }
    dev.off()
    
    pdf("./Figures/Exploratory/TD.pdf", width=8)
        for (i in levels(td$CODI)) {
            print(plot_td[[i]])
        }
    dev.off()
    
    pdf("./Figures/Exploratory/TF.pdf", width=8)
    for (i in levels(tf$CODI)) {
        print(plot_tf[[i]])
    }
    dev.off()
    
    pdf("./Figures/Exploratory/BV.pdf", width=8)
    for (i in levels(bv$CODI)) {
        print(plot_bv[[i]])
    }
    dev.off()

