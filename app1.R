# app.R — Trabajo 1: Densidad y Abundancia
# Autor: Carlos Iván Espinosa (adaptado y reescrito)
# Fecha: 2026
# Una sola app con dos casos en pestañas

library(shiny)
library(spatstat)
library(raster)

# -------------------------------------------------------------------
# 1. Datos base — Caso 1
# -------------------------------------------------------------------

generar_datos_caso1 <- function() {
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
        
        area_total <- 500 * 500
        
        vals1 <- as.matrix(elev1$v)
        vals2 <- as.matrix(elev2$v)
        area_spp1 <- sum(!is.na(vals1)) / length(vals1) * area_total
        area_spp2 <- sum(!is.na(vals2)) / length(vals2) * area_total
        
        list(
                elev1 = elev1, elev2 = elev2,
                spp1 = spp1, spp2 = spp2,
                area_total = area_total,
                area_spp1 = area_spp1,
                area_spp2 = area_spp2
        )
}

# -------------------------------------------------------------------
# 2. Datos base — Caso 2
# -------------------------------------------------------------------

generar_datos_caso2 <- function() {
        xrange <- c(0, 500)
        yrange <- c(0, 500)
        window <- owin(xrange, yrange)
        
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
        
        area_total <- 500 * 500
        vals1 <- as.matrix(uso1$v)
        vals2 <- as.matrix(uso2$v)
        area_bosque  <- sum(!is.na(vals1)) / length(vals1) * area_total
        area_cultivo <- sum(!is.na(vals2)) / length(vals2) * area_total
        
        list(
                uso1 = uso1, uso2 = uso2,
                spp1B = spp1B, spp2C = spp2C,
                area_bosque = area_bosque,
                area_cultivo = area_cultivo,
                area_parcela = 75 * 75
        )
}

datos1 <- generar_datos_caso1()
datos2 <- generar_datos_caso2()

# -------------------------------------------------------------------
# 3. Función de muestreo — Caso 1
# -------------------------------------------------------------------

dta.Sam <- function(size, num, seed = 123, datos) {
        size <- as.numeric(size)
        s2 <- size / 2
        
        xp <- rep(seq(50, 450, 100), each = 5)
        yp <- rep(seq(50, 450, 100), 5)
        
        dentro <- xp - s2 >= 0 & xp + s2 <= 500 &
                yp - s2 >= 0 & yp + s2 <= 500
        xp <- xp[dentro]
        yp <- yp[dentro]
        
        if (num > length(xp)) num <- length(xp)
        
        set.seed(seed)
        idx1 <- sample(seq_along(xp), num, replace = FALSE)
        idx2 <- sample(seq_along(xp), num, replace = FALSE)
        
        ventana <- function(x, y) owin(c(x - s2, x + s2), c(y - s2, y + s2))
        
        unSamp1 <- lapply(idx1, function(i) datos$spp1[ventana(xp[i], yp[i])])
        unSamp2 <- lapply(idx2, function(i) datos$spp2[ventana(xp[i], yp[i])])
        
        dta <- data.frame(
                Cedrela_montana = sapply(unSamp1, function(x) x$n),
                Cinchona_officinalis = sapply(unSamp2, function(x) x$n)
        )
        
        list(unSamp1, unSamp2, dta)
}

# -------------------------------------------------------------------
# 4. UI — Un solo app con dos casos
# -------------------------------------------------------------------

ui <- fluidPage(
        titlePanel("Trabajo 1: Densidad y Abundancia"),
        
        tabsetPanel(
                id = "caso",
                
                # ------------------- CASO 1 -------------------
                tabPanel("Caso 1: Dos especies",
                         sidebarLayout(
                                 sidebarPanel(
                                         selectInput("size", "Tamaño de parcela (m):",
                                                     choices = c(20, 50, 100), selected = 50),
                                         sliderInput("numS", "Número de muestras:",
                                                     min = 3, max = 20, value = 10, step = 1),
                                         numericInput("seed", "Semilla (reproducibilidad):",
                                                      value = 123, min = 1),
                                         hr(),
                                         helpText("Cambia el tamaño de parcela o el número de ",
                                                  "muestras para ver cómo varían las estimaciones.")
                                 ),
                                 mainPanel(
                                         tabsetPanel(
                                                 type = "tabs",
                                                 tabPanel("Gráfico",
                                                          plotOutput("plot1", height = "500px")),
                                                 tabPanel("Tabla", tableOutput("table1")),
                                                 tabPanel("Resumen", verbatimTextOutput("resumen1"))
                                         )
                                 )
                         )
                ),
                
                # ------------------- CASO 2 -------------------
                tabPanel("Caso 2: Dos usos de suelo",
                         sidebarLayout(
                                 sidebarPanel(
                                         checkboxInput("mostrar_num",
                                                       "Mostrar numeración de parcelas",
                                                       value = FALSE),
                                         numericInput("parcela", "Número de parcela (1–25):",
                                                      value = 1, min = 1, max = 25),
                                         radioButtons("uso", "Uso de suelo:",
                                                      choices = list("Bosque" = 1, "Cultivo" = 2),
                                                      selected = 1),
                                         actionButton("agregar", "Agregar muestra"),
                                         actionButton("reiniciar", "Reiniciar muestreo"),
                                         hr(),
                                         helpText("Selecciona una parcela y su uso de suelo, luego",
                                                  "presiona 'Agregar muestra'."),
                                         helpText("Cada parcela mide 75 × 75 m (5.625 m²).")
                                 ),
                                 mainPanel(
                                         tabsetPanel(
                                                 type = "tabs",
                                                 tabPanel("Gráfico",
                                                          plotOutput("plot2", height = "550px")),
                                                 tabPanel("Muestras", tableOutput("tabla2")),
                                                 tabPanel("Resumen", verbatimTextOutput("resumen2"))
                                         )
                                 )
                         )
                )
        )
)

# -------------------------------------------------------------------
# 5. Server
# -------------------------------------------------------------------

server <- function(input, output, session) {
        
        # ============ CASO 1 ============
        muestreo1 <- reactive({
                dta.Sam(size = input$size, num = input$numS,
                        seed = input$seed, datos = datos1)
        })
        
        output$plot1 <- renderPlot({
                res <- muestreo1()
                par(mfcol = c(1, 2))
                
                plot(datos1$elev1, col = terrain.colors(10),
                     main = "Cedrela montana", xlab = "metros", ylab = "metros")
                points(datos1$spp1$x, datos1$spp1$y, pch = 21,
                       cex = 0.7, bg = rgb(0.8, 0, 0.1, 0.8))
                for (i in seq_along(res[[1]])) {
                        plot(res[[1]][[i]]$window, add = TRUE, lwd = 1)
                }
                
                plot(datos1$elev2, col = terrain.colors(10),
                     main = "Cinchona officinalis",
                     xlab = "metros", ylab = "metros")
                points(datos1$spp2$x, datos1$spp2$y, pch = 21,
                       cex = 0.7, bg = rgb(0.8, 0, 0.1, 0.8))
                for (i in seq_along(res[[2]])) {
                        plot(res[[2]][[i]]$window, add = TRUE, lwd = 1)
                }
        })
        
        output$table1 <- renderTable({
                dta <- muestreo1()[[3]]
                area_parcela <- as.numeric(input$size)^2
                dta$Densidad_Cedrela <- dta$Cedrela_montana / area_parcela
                dta$Densidad_Cinchona <- dta$Cinchona_officinalis / area_parcela
                dta
        })
        
        output$resumen1 <- renderPrint({
                dta <- muestreo1()[[3]]
                area_parcela <- as.numeric(input$size)^2
                
                cat("Área total del estudio:", datos1$area_total, "m²\n")
                cat("Área ocupada por Cedrela montana:", round(datos1$area_spp1, 0), "m²\n")
                cat("Área ocupada por Cinchona officinalis:", round(datos1$area_spp2, 0), "m²\n")
                cat("Área por parcela:", area_parcela, "m²\n")
                cat("Número de parcelas:", nrow(dta), "\n\n")
                
                cat("--- Cedrela montana ---\n")
                cat("  Abundancia media por parcela:", round(mean(dta$Cedrela_montana), 2), "\n")
                cat("  Densidad bruta media (ind/m²):",
                    round(mean(dta$Cedrela_montana) / datos1$area_total, 8), "\n")
                cat("  Densidad ecológica media (ind/m²):",
                    round(mean(dta$Cedrela_montana) / datos1$area_spp1, 8), "\n")
                cat("  Parcelas con cero individuos:",
                    sum(dta$Cedrela_montana == 0), "\n\n")
                
                cat("--- Cinchona officinalis ---\n")
                cat("  Abundancia media por parcela:",
                    round(mean(dta$Cinchona_officinalis), 2), "\n")
                cat("  Densidad bruta media (ind/m²):",
                    round(mean(dta$Cinchona_officinalis) / datos1$area_total, 8), "\n")
                cat("  Densidad ecológica media (ind/m²):",
                    round(mean(dta$Cinchona_officinalis) / datos1$area_spp2, 8), "\n")
                cat("  Parcelas con cero individuos:",
                    sum(dta$Cinchona_officinalis == 0), "\n")
        })
        
        # ============ CASO 2 ============
        muestras <- reactiveVal(data.frame(
                Parcela = integer(),
                Uso = character(),
                Abundancia = integer(),
                Densidad = numeric(),
                stringsAsFactors = FALSE
        ))
        
        posicion_parcela <- function(n) {
                xp <- rep(seq(50, 450, 100), each = 5)
                yp <- rep(seq(50, 450, 100), 5)
                list(x = xp[n], y = yp[n])
        }
        
        ventana_c2 <- function(x, y, size = 75) {
                s2 <- size / 2
                owin(c(x - s2, x + s2), c(y - s2, y + s2))
        }
        
        contar_individuos <- function(parcela, uso) {
                pos <- posicion_parcela(parcela)
                v <- ventana_c2(pos$x, pos$y)
                if (uso == 1) datos2$spp1B[v]$n else datos2$spp2C[v]$n
        }
        
        observeEvent(input$agregar, {
                uso_nombre <- ifelse(input$uso == 1, "Bosque", "Cultivo")
                abundancia <- contar_individuos(input$parcela, input$uso)
                densidad <- abundancia / datos2$area_parcela
                
                nueva <- data.frame(
                        Parcela = input$parcela,
                        Uso = uso_nombre,
                        Abundancia = abundancia,
                        Densidad = densidad,
                        stringsAsFactors = FALSE
                )
                muestras(rbind(muestras(), nueva))
        })
        
        observeEvent(input$reiniciar, {
                muestras(data.frame(
                        Parcela = integer(),
                        Uso = character(),
                        Abundancia = integer(),
                        Densidad = numeric(),
                        stringsAsFactors = FALSE
                ))
        })
        
        output$plot2 <- renderPlot({
                pos <- posicion_parcela(input$parcela)
                v <- ventana_c2(pos$x, pos$y)
                
                par(mar = c(4, 4, 2, 1))
                plot(datos2$uso1, col = rgb(0.4, 0.8, 0.2),
                     main = "", xlab = "metros", ylab = "metros")
                points(datos2$spp1B$x, datos2$spp1B$y, pch = 21, cex = 0.7,
                       bg = rgb(0.8, 0, 0.1, 0.8))
                plot(datos2$uso2, col = "grey80", add = TRUE)
                points(datos2$spp2C$x, datos2$spp2C$y, pch = 21, cex = 0.7,
                       bg = rgb(0.3, 0, 0.6, 0.8))
                
                segments(x0 = seq(0, 500, 100), y0 = rep(0, 6),
                         x1 = seq(0, 500, 100), y1 = rep(500, 6),
                         col = "grey40")
                segments(x0 = rep(0, 6), y0 = seq(0, 500, 100),
                         x1 = rep(500, 6), y1 = seq(0, 500, 100),
                         col = "grey40")
                
                plot(v, add = TRUE, lwd = 2, border = "blue")
                
                if (input$mostrar_num) {
                        xp <- rep(seq(50, 450, 100), each = 5)
                        yp <- rep(seq(50, 450, 100), 5)
                        text(x = xp, y = yp, 1:25, cex = 1.2, col = "black")
                }
                
                abundancia <- contar_individuos(input$parcela, input$uso)
                legend("topright",
                       legend = paste("Abundancia:", abundancia),
                       bty = "n", cex = 1.2)
                
                m <- muestras()
                if (nrow(m) > 0) {
                        for (i in seq_len(nrow(m))) {
                                pos_i <- posicion_parcela(m$Parcela[i])
                                v_i <- ventana_c2(pos_i$x, pos_i$y)
                                plot(v_i, add = TRUE, lwd = 1, border = "orange")
                        }
                }
        })
        
        output$tabla2 <- renderTable({
                m <- muestras()
                if (nrow(m) == 0) {
                        return(data.frame(Mensaje = "Aún no has agregado muestras"))
                }
                m
        })
        
        output$resumen2 <- renderPrint({
                m <- muestras()
                if (nrow(m) == 0) {
                        cat("Aún no has agregado muestras.\n")
                        return()
                }
                
                cat("Área por parcela:", datos2$area_parcela, "m²\n")
                cat("Área total bosque:", round(datos2$area_bosque, 0), "m²\n")
                cat("Área total cultivo:", round(datos2$area_cultivo, 0), "m²\n\n")
                
                for (u in c("Bosque", "Cultivo")) {
                        sub <- m[m$Uso == u, ]
                        if (nrow(sub) == 0) next
                        
                        cat("---", u, "---\n")
                        cat("  Muestras:", nrow(sub), "\n")
                        cat("  Abundancia media:", round(mean(sub$Abundancia), 2), "\n")
                        cat("  Densidad media (ind/m²):",
                            round(mean(sub$Densidad), 6), "\n")
                        
                        area_uso <- ifelse(u == "Bosque",
                                           datos2$area_bosque,
                                           datos2$area_cultivo)
                        abundancia_total <- mean(sub$Densidad) * area_uso
                        cat("  Abundancia total estimada:",
                            round(abundancia_total, 1), "\n\n")
                }
                
                total <- sum(m$Abundancia)
                m$AbundanciaRelativa <- m$Abundancia / total
                cat("Abundancia relativa por muestra:\n")
                print(m[, c("Parcela", "Uso", "Abundancia", "AbundanciaRelativa")])
        })
}

shinyApp(ui, server)