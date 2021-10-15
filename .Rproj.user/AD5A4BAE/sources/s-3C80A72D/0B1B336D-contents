dta.Sam <- function(size, num){
        require(spatstat)
        require(raster)
        
         xrange=c(0, 500)
        yrange=c(0, 500)
        window<-owin(xrange, yrange)
        
        
        # Build maps from random points ----
        set.seed(53)
        elev  <- density(rpoispp(lambda=1.5, lmax= 5, win=window)) #
        elev <- elev*1000
        
        elev1 <- elev
        elev1[elev1<1490] <- NA
        elev1[elev1>1502] <- NA
        spp1 <- rpoint(500, elev1)
        
        elev2 <- elev
        elev2[elev2>1495] <- NA
        spp2 <- rpoint(300, elev2)
        
        
        xp <- rep(seq(50, 450, 100), each =5)
        yp <- rep(seq(50, 450, 100), 5)
        
        ventana <- function(pp,size=50,x=1,y=1){
                x <- x
                y <- y
                s2 <- size/2
                return(owin(c(x-s2, x+s2), c(y-s2, y+s2)))
                         }
        
        unSamp1 <- list()
        unSamp2 <- list()
        ##
        ##cALCULAR PRIMERO LOS PUNTOS A MUESTREAR
        ##
        set.seed(Sys.time())
        for(i in 1:num){
                unSamp1[[i]] <- spp1[ventana(spp1, size=as.numeric(size),x=xp[sample(1:25, 1)],y=yp[sample(1:25, 1)])]
                }
        
        set.seed(Sys.time()+5)
        for(i in 1:num){
                unSamp2[[i]] <- spp2[ventana(spp2, size=as.numeric(size),x=xp[sample(1:25, 1)],y=yp[sample(1:25, 1)])]
         }
        
        dta1 <- list()
        for(i in 1:num){
                dta1[[i]] <- unSamp1[[i]]$n
                        }

        dta2 <- list()
        for(i in 1:num){
                dta2[[i]] <- unSamp2[[i]]$n
        }
        
        dta <- data.frame(C.odorata=unlist(dta1), C.officinalis=unlist(dta2))
        
        dta.Tot <- list(unSamp1, unSamp2, dta)
        
        return( dta.Tot)
        
        }