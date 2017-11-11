####################################
#  Parameter vectors
####################################

numplots <- length(levels(ocells$Especie))
nplots<-numplots


########### Data pooled together

# pool.null,model
pool.null.par <- list(PotRichness = 1, lambda=1)
pool.null.par.lo <- list(PotRichness = 0, lambda=0.0001)
pool.null.par.hi <- list(PotRichness = 30, lambda=100)

# pool.full.model
pool.full.par<-list(PotRichness = 1,
                    HOa=10, HOb=2, 
                    DWa=81, DWb=2,
                    MGa=40,MGb=2,
                    SHa=50, SHb=20,
                    lambda=1)

pool.full.par.lo<-list(PotRichness = 0,
                       HOa=0.00001, HOb=0.00001,
                       DWa=0.00001, DWb=0.00001,
                       MGa=0.00001, MGb=0.00001, 
                       SHa= 0.00001,SHb=0.000001,
                       lambda=0.00001)

pool.full.par.hi<-list(PotRichness = 25,
                       HOa=30, HOb=100,
                       DWa=800, DWb=800,
                       MGa=100, MGb=200,  
                       SHa=100, SHb=200,
                       lambda=100)

# pool.noho.model
pool.noho.par<-list(PotRichness = 1,
                    DWa=82, DWb=2,
                    MGa=40,MGb=2,
                    SHa=50, SHb=20,
                    lambda=1)

pool.noho.par.lo<-list(PotRichness = 0,
                       DWa=0.00001, DWb=0.00001,
                       MGa=0.00001,MGb=0.00001,
                       SHa= 0.00001, SHb=0.000001,
                       lambda=0.00001)

pool.noho.par.hi<-list(PotRichness = 25,
                       DWa=800, DWb=800,
                       MGa=100, MGb=200, 
                       SHa=100, SHb=200,
                       lambda=100)


# pool.nodw.model
pool.nodw.par<-list(PotRichness = 1,
                    HOa=10, HOb=2, 
                    MGa=40,MGb=2,
                    SHa=50, SHb=20,
                    lambda=1)

pool.nodw.par.lo<-list(PotRichness = 0,
                       HOa=0.00001, HOb=0.00001,
                       MGa=0.00001,MGb=0.00001,
                       SHa= 0.00001, SHb=0.000001,
                       lambda=0.00001)

pool.nodw.par.hi<-list(PotRichness = 25,
                       HOa=30, HOb=100,
                       MGa=100, MGb=200, 
                       SHa=100, SHb=200,
                       lambda=100)


# pool.nomg.model
pool.nomg.par<-list(PotRichness = 1,
                    HOa=10, HOb=2, 
                    DWa=86, DWb=2, 
                    SHa=50, SHb=20,
                    lambda=1)

pool.nomg.par.lo<-list(PotRichness = 0,
                       HOa=0.00001, HOb=0.00001,
                       DWa=0.00001, DWb=0.00001,
                       SHa= 0.00001, SHb=0.000001,
                       lambda=0.00001)

pool.nomg.par.hi<-list(PotRichness = 25,
                       HOa=30, HOb=100,
                       DWa=800, DWb=800,
                       SHa=100, SHb=200,
                       lambda=1000)

# pool.nosh.model
pool.nosh.par<-list(PotRichness = 1,
                    HOa=10, HOb=2, 
                    DWa=87, DWb=2,
                    MGa=40,MGb=2,
                    lambda=1)

pool.nosh.par.lo<-list(PotRichness = 0,
                       HOa=0.00001, HOb=0.00001,
                       DWa=0.00001, DWb=0.00001,
                       MGa=0.00001,MGb=0.00001,
                       lambda=0.00001)

pool.nosh.par.hi<-list(PotRichness = 25,
                       HOa=30, HOb=100,
                       DWa=800, DWb=800,
                       MGa=100, MGb=200, 
                       lambda=100)




