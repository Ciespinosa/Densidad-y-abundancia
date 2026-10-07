library(shiny)
library(spatstat)
library(raster)

# -------------------------------------------------------------------
# 1. Datos base (se generan una sola vez)
# -------------------------------------------------------------------

generar_datos <- function() {
        xrange <- c(0, 500)
        yrange <- c(0, 500)
        window <- owin(xrange, yrange)
        
        set.seed(53)
        elev <- density(rpoispp(lambda = 1.5, lmax = 5, win = window))
        elev <- elev * 1000
        
        elev1 <- elev
        elev1[elev1 < 1490] <- NA
        elev1[elev1 > 1502] <- NA
        spp1 <- rpoint(500, elev1)
        
        elev2 <- elev
        elev2[elev2 > 1495] <- NA
        spp2 <- rpoint(300, elev2)
        
        set.seed(83)
        uso <- density(rpoispp(lambda = 1.5, lmax = 5, win = window))
        uso <- uso * 1000
        
        uso1 <- uso
        uso1[uso1 < 1477] <- NA
        uso1[uso1 > 1496] <- NA
        spp1B <- rpoint(500, uso1)
        
        uso2 <- uso
        uso2[uso2 < 1496] <- NA
        spp2C <- rpoint(250, uso2)
        
        list(
                elev1 = elev1, elev2 = elev2,
                spp1 = spp1, spp2 = spp2,
                uso1 = uso1, uso2 = uso2,
                spp1B = spp1B, spp2C = spp2C,
                area_total = 500 * 500  # 250.000 m²
        )
}

datos <- generar_datos()

# -------------------------------------------------------------------
# 2. Función de muestreo (corregida)
# -------------------------------------------------------------------

dta.Sam <- function(size, num, seed = 123, datos) {
        size <- as.numeric(size)
        s2 <- size / 2
        
        # Posiciones de parcela
        xp <- rep(seq(50, 450, 100), each = 5)
        yp <- rep(seq(50, 450, 100), 5)
        
        # Filtrar parcelas que caen completamente dentro del área
        dentro <- xp - s2 >= 0 & xp + s2 <= 500 &
                yp - s2 >= 0 & yp + s2 <= 500
        xp <- xp[dentro]
        yp <- yp[dentro]
        
        if (num > length(xp)) {
                num <- length(xp)
        }
        
        # Muestreo sin reemplazo, reproducible con seed
        set.seed(seed)
        idx1 <- sample(seq_along(xp), num, replace = FALSE)
        idx2 <- sample(seq_along(xp), num, replace = FALSE)
        
        ventana <- function(x, y) owin(c(x - s2, x + s2), c(y - s2, y + s2))
        
        unSamp1 <- lapply(idx1, function(i) {
                datos$spp1[ventana(xp[i], yp[i])]
        })
        unSamp2 <- lapply(idx2, function(i) {
                datos$spp2[ventana(xp[i], yp[i])]
        })
        
        dta <- data.frame(
                Cedrela_montana = sapply(unSamp1, function(x) x$n),
                Cinchona_officinalis = sapply(unSamp2, function(x) x$n)
        )
        
        list(unSamp1, unSamp2, dta)
}
