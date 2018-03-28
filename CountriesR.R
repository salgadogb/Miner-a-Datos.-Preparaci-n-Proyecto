# -------------------------------------------------
# PEC2 - MINERÍA DE DATOS - "Preparación de datos"
# -------------------------------------------------


# 1.- Cargar archivo: "countries.xlsx"
# -------------------------------------------------
  library(plyr)
  library(stringr)
  library(car)
  library(readxl)                               # Para leer archivo Excel
  tbl_countries <- read_excel("countries.xlsx") # Archivo original datos
  tbl_countries_w <- tbl_countries              # Archivo de trabajo para cambiar "NA"
  

# 2.- VARIABLES DE ESTUDIO
# -------------------------------------------------
    str(tbl_countries_w)

  
# 3.- TRANSFORMACIÓN DE DATOS
# -------------------------------------------------
  
  
#  3.1.- work file: Replacing NA's by 'na' - * Manipular los datos faltantes NA *
# -------------------------------------------------
  
    # a.- # Filtrar datos y LIMPIEZA.
  
  tbl_countries_w[is.na(tbl_countries_w[])]<- 'na'    # No tratar los casos no observados
  
  

# 3.2.- ANÁLISIS DE DATOS: "countries.xlsx" 
# -------------------------------------------------
  #   A.1- TRANSFORMACIÓN: Producción de crudo por países
  #   A.2- GRÁFICO: de los mayores productores.

  #   B.1- TRANSFORMACIÓN: Relación expectativas de vida entre: Hombres vs Mujeres.
  #   B.2- GRÁFICO:nube de puntos de las dos variables con la recta de regresión.

  #   C.1- TRANSFORMACIÓN: Relación entre `GDP_$_PER_CAPITA` & NATURAL_GROWTH. 
  #   C.2- GRÁFICO:nube de puntos de las dos variables con la recta de regresión.

  #   D.1- TRANSFORMACIÓN: AGRUPAR EN INTERVALOS PIP `GDP_$_PER_CAPITA`
  #   D.2- GRÁFICO: boxplot e histograma.
  
  #   E.1- TRANSFORMACIÓN: Relación entre  URBAN_POPULATION & RURAL_POPULATION
  #   E.2- GRÁFICO: Gráfico por sectores con "highchart".
  
  #   F.1- TRANSFORMACIÓN: RECODIFICACIÓN DE DATOS atributo "MAIN_SECTOR"
  #   F.2- GRÁFICO: # Gráfico por sectores con "highchart" 
  
  #   G.1- TRANSFORMACIÓN: Relación entre atributos a extraer c(1, 6, 4, 15, 14, 16, 20, 21, 22, 24) 
  #   G.2- ANÁLISIS DE LOS DATOS Y gráficos

  #   H.1- TRANSFORMACIÓN: NORMALIZACIÓN (Ratio_Crec_PIB) 
  #   H.2- GRÁFICOS
  #   H.3- ANÁLISIS DE LOS DATOS 
  
  
  #   A.1- PRODUCCIÓN DE CRUDO POR PAÍSES: gráfico de los mayores productores.
  # -------------------------------------------------
  
  
    #  a.- Se extraerán solo los datos de PRODUCCIÓN observados y PAÍS correspondiente.

      tbl_CRUDE_OIL_NAME <- tbl_countries_w[tbl_countries_w$CRUDE_OIL_BAR_DAY!='na', c("NAME","CRUDE_OIL_BAR_DAY")] 
      CRUDE_OIL_BAR_DAY2 <- as.numeric(tbl_CRUDE_OIL_NAME$CRUDE_OIL_BAR_DAY) # numero <- caracter

      
    #  b.- Se simplifican los datos de producción dividiendo por 1000
      
      CRUDE_OIL_BAR_X1000_DAY <- CRUDE_OIL_BAR_DAY2/1000

      
    #  c.- Se añade una nueva columna a tbla_CRUDE_OIL_NAME y elimina la obsoleta con caracteres.

     tbl_CRUDE_OIL_NAME <- cbind(tbl_CRUDE_OIL_NAME, CRUDE_OIL_BAR_X1000_DAY)
     tbl_CRUDE_OIL_NAME <- tbl_CRUDE_OIL_NAME[ ,-(2)] 
     
     
    #  d.- Se ordenan los datos de producción de crudo de mayor a menor

    tbl_CRUDE_OIL_NAME <- tbl_CRUDE_OIL_NAME[order(tbl_CRUDE_OIL_NAME$CRUDE_OIL_BAR_X1000_DAY, decreasing = T), ]
    
    
  #  A.2- GRÁFICO: de los mayores productores.
  # -------------------------------------------------
    
    
    #  a.-Gráfico de "BARRAS":"PRODUCCIÓN DE CRUDO POR PAÍSES: barriles/día X 1000"  

    tbl_CRUDE_OIL_NAME[(tbl_CRUDE_OIL_NAME$NAME=='United Arab Emirates'),1 ]<- 'U. Arab Emirates' #Nombre país muy grande

    barplot(tbl_CRUDE_OIL_NAME$CRUDE_OIL_BAR_X1000_DAY, main= "PRODUCCIÓN DE CRUDO POR PAÍSES: barriles/día X 1000",
        names.arg=tbl_CRUDE_OIL_NAME$NAME, col=rainbow(50),las=2, cex.names=0.6,horiz = F)
    

    #  b.- Gráfico de "COORD_POLAR":"PRODUCCIÓN DE CRUDO POR PAÍSES: barriles/día X 1000"  

    library(ggplot2)
    
    ggplot(data=tbl_CRUDE_OIL_NAME, width = 700, height = 700, aes(x=reorder(NAME,-CRUDE_OIL_BAR_X1000_DAY), y=CRUDE_OIL_BAR_X1000_DAY, fill=tbl_CRUDE_OIL_NAME$NAME)) +
    geom_bar(stat="identity") + ggtitle("PRODUCCIÓN DE CRUDO POR PAÍSES: barriles/día X 1000")+ theme_minimal()+
    coord_polar(theta = "x", direction=1 )   

    
  #  B.1- TRANSFORMACIÓN: Relación EXPECTATIVAS DE VIDA entre: Hombres vs Mujeres.
  # -------------------------------------------------    
    
    
    # a.- Se extraerán solo los datos expectativas de vida H y M, y nombre país correspondiente.       

    tbl_LIFE_EXP_M_W <- tbl_countries_w[tbl_countries_w$MEM_LIFE_EXP!='na'&tbl_countries_w$WOMEN_LIFE_EXP!='na', c("NAME","MEM_LIFE_EXP","WOMEN_LIFE_EXP")]
   
    
    # b.- Se convierten los caracteres en números para hacer cálculos. 
    
    MEM_LIFE_EXP2 <- as.numeric(tbl_LIFE_EXP_M_W$MEM_LIFE_EXP) # numero <- caracter
    WOMEN_LIFE_EXP2 <- as.numeric(tbl_LIFE_EXP_M_W$WOMEN_LIFE_EXP)

    
    # c.- Se añaden dos nuevas columnas a LIFE_EXP_M_W y eliminan otras dos obsoletas

    tbl_LIFE_EXP_M_W <- cbind(tbl_LIFE_EXP_M_W, MEM_LIFE_EXP2,WOMEN_LIFE_EXP2 )
    tbl_LIFE_EXP_M_W <- tbl_LIFE_EXP_M_W[ ,-(2:3)] # Elimina las filas con caracteres
    
    
    # d.- Se ordenan los datos de expectativas de vida por mujer

    tbl_LIFE_EXP_M_W <- tbl_LIFE_EXP_M_W[order(tbl_LIFE_EXP_M_W$WOMEN_LIFE_EXP2, decreasing = T), ]
    

  #  B.2- GRÁFICO:nube de puntos de las dos variables con la recta de regresión.
  # -------------------------------------------------
    
    
    # a.- Realizamos el gráfico de la nube de puntos de las dos variables con la recta de regresión
    
    plot(MEM_LIFE_EXP2~WOMEN_LIFE_EXP2, main="RELACIÓN EXPECTATIVA DE VIDA: HOMBRES & MUJERES", sub="Para cada país", 
          xlab="Expectativa Vida: MUJERES", ylab="Expectativa Vida: HOMBRES", data=tbl_LIFE_EXP_M_W, 
         col = c("orange", "blue"))
    
    
    # b.- Hallamos la recta de regresión entre ambos resultados.
    
    y <- abline(lm(formula=MEM_LIFE_EXP2~WOMEN_LIFE_EXP2, data=tbl_LIFE_EXP_M_W),col="red")
   
    
    #  c.- Obtenemos: pendiente de la recta, ordenada en el origen y coeficiente de determinación R2 
    
    RegModel.1 <- lm(formula=MEM_LIFE_EXP2~WOMEN_LIFE_EXP2, data=tbl_LIFE_EXP_M_W)
    summary(RegModel.1)

       # Resultados: Coeficiente Determinación R2=0.961 (Muy buen ajuste),
       #             Ordenada en el origen a=4.03783
       #             Pendiente b=0.87395
       #             Y = 0.87395X + 4.03783 (Permite calcular edad Hombre <- en función edad Mujer)


    
  #  C.1- TRANSFORMACIÓN: Relación entre: `GDP_$_PER_CAPITA` & NATURAL_GROWTH. 
  # -------------------------------------------------
    
    
    #  a.- Se extraerán solo los datos expectativas de: "NAME","GDP_$_PER_CAPITA","NATURAL_GROWTH"

    tbl_GDP_NGROWTH <- tbl_countries_w[tbl_countries_w$`GDP_$_PER_CAPITA`!='na'&tbl_countries_w$NATURAL_GROWTH!='na', c("NAME","GDP_$_PER_CAPITA","NATURAL_GROWTH")]
    `GDP_$_PER_CAPITA2` <- as.numeric(tbl_GDP_NGROWTH$`GDP_$_PER_CAPITA`) # numero <- caracter
    NATURAL_GROWTH2 <- as.numeric(tbl_GDP_NGROWTH$NATURAL_GROWTH)

    
    #  b.- Se añaden dos nuevas columnas a tbl_GDP_NGROWTH

    tbl_GDP_NGROWTH <- cbind(tbl_GDP_NGROWTH, `GDP_$_PER_CAPITA2`,NATURAL_GROWTH2 )
    tbl_GDP_NGROWTH <- tbl_GDP_NGROWTH[ ,-(2:3)] # Elimina las filas con caracteres deja números
   
    
    #  c.- Se ordenan los datos de producción `GDP_$_PER_CAPITA2`

    tbl_GDP_NGROWTH <- tbl_GDP_NGROWTH[order(tbl_GDP_NGROWTH$`GDP_$_PER_CAPITA2`, decreasing = T), ]
    

  #   C.2- GRÁFICO:"NUBE DE PUNTOS" y "RESIDUOS" de las dos variables
  # -------------------------------------------------
    
    
   # a.- Realizamos el gráfico de la nube de puntos de las dos variables con la recta de regresión

    par(mfrow = c(2, 2))
    plot(NATURAL_GROWTH2~`GDP_$_PER_CAPITA2`, main="RELACIÓN ENTRE:\n
         Tasa Crecimiento & Renta Per Cápita", sub="Gráfico 1", 
    xlab="GDP - RENTA PER CÁPITA", ylab="TASA CRECIMIENTO NATURAL", data=tbl_GDP_NGROWTH, col = c("orange", "blue"))

    
  # b.- Hallamos la recta de regresión entre ambos resultados.
       
    m0 <- lm(NATURAL_GROWTH2~`GDP_$_PER_CAPITA2`, data=tbl_GDP_NGROWTH)
    abline(m0,col="red")
    
    
  # c.- Hallamos residuos entre ambos resultados.   
    
    plot(tbl_GDP_NGROWTH$`GDP_$_PER_CAPITA2`, residuals(m0), xlab = "Gráfico 2",
         ylab = "Residuales", main="RESIDUALES", col = c("orange", "blue"))
    abline(h = 0, lty = 2, col="purple")

    
  # d.- Obtenemos: pendiente de la recta, ordenada en el origen y coeficiente de determinación R2 
    
    RegModel.1 <- lm(formula=NATURAL_GROWTH2~`GDP_$_PER_CAPITA2`, data=tbl_GDP_NGROWTH)
    summary(RegModel.1)

        # Resultados: Coeficiente Determinación R2=0.38 (Muy mal ajuste),
        #             Pendiente b=-9.542e-04 (NEGATIVA)
        #             Y = -9.542e-04X + 2.662e+01 
        # (NO Permite calcular la tasa de crecimiento poblacional conociendo la renta per cápita)

    
  #  e.- Diagrama de "TALLO Y HOJA"
    
    stem(tbl_GDP_NGROWTH$`GDP_$_PER_CAPITA2`)   
    
    
  #   f.- Boxplot

  #library(plotly)
  par(mfrow = c(1, 1))
  boxplot(NATURAL_GROWTH2~`GDP_$_PER_CAPITA2`,data=tbl_GDP_NGROWTH, col=rainbow(30))
  
  par(mfrow = c(1, 2))
  boxplot(NATURAL_GROWTH2,data=tbl_GDP_NGROWTH, main="TASA CRECIMIENTO NATURAL", sub="Gráfico 1", 
         ylab="TASA CRECIMIENTO NATURAL", col = c("orange"))
  boxplot(`GDP_$_PER_CAPITA2`,data=tbl_GDP_NGROWTH,main="RENTA PER CÁPITA",
         sub="Gráfico 2", ylab="GDP - RENTA PER CÁPITA", col = c("green"))

  
  #  g.- HISTOGRAMAS
  
  
    # g.1.- Seleccionamos solo los 10 primeros países mayor GDP
  
  tbl_GDP_10 <- tbl_GDP_NGROWTH[+(1:10),] # Selecciona 10 filas
  
  tbl_GDP_10[(tbl_GDP_10$NAME=='United Arab Emirates'),1 ]<- 'U. Arab Emirates' #Nombre país muy grande
  
  
  # g.2.- Seleccionamos solo los 10 primeros países mayor NGROWTH
  
  tbl_GDP_NGROWTH <- tbl_GDP_NGROWTH[order(tbl_GDP_NGROWTH$NATURAL_GROWTH2, decreasing = T), ]
  
  tbl_NGROWTH10<- tbl_GDP_NGROWTH[+(1:10),] # Selecciona 10 filas
  
  
  # g.3.- Gráficas: Histogramas (GDP & NATURAL_GROWTH)
  
  par(mfrow = c(1, 2))
  
  barplot(tbl_GDP_10$`GDP_$_PER_CAPITA2`, main= "GDP",
          names.arg=tbl_GDP_10$NAME, col=rainbow(17),las=2, cex.names=0.6,horiz = F)
  
  barplot(tbl_NGROWTH10$NATURAL_GROWTH2, main= "NATURAL_GROWTH",
          names.arg=tbl_NGROWTH10$NAME, col=rainbow(17),las=2, cex.names=0.6,horiz = F)
  
  
  #   D.1- TRANSFORMACIÓN: AGRUPAR EN INTERVALOS: `GDP_$_PER_CAPITA`
  
  #      *** Variable cuantitativa en clases (DISCRETIZACIÓN) **** #
  # ----------------------------------------------------------
  
  
   # a.- Se extraerán solo los datos expectativas de: "NAME","CONTINENT","GDP_$_PER_CAPITA")
  
  tbl_GDP <- tbl_countries_w[tbl_countries_w$`GDP_$_PER_CAPITA`!='na',c("NAME","CONTINENT","GDP_$_PER_CAPITA")]
  
  tbl_GDP <- tbl_GDP[order(tbl_GDP$`GDP_$_PER_CAPITA`, decreasing = T), ] # ordenado decreciente GDP_$_PER_CAPITA
  
  
   # b.- Se agruparán los datos "GDP_$_PER_CAPITA". Definimos clases, rango, amplitud.
  
   GDP <- tbl_GDP$`GDP_$_PER_CAPITA`  # vector
   num_clases <- nclass.scott(GDP) # = 7 clases y nclass.Sturges(GDP) = 9 Clases
   rango <- diff(range(GDP))
   amplitud <- round(rango/num_clases)
   
   
   #    c.- CODIFICACIÓN: fijar los valores de los extremos de los intervalos.
   # -------------------------------------------------
   
   
    # c.1.- Intervalos y marcas de clase
   
   L.al <-min(GDP)+amplitud*(0:num_clases) # vector L1_GDP con extremos intervalos
   int_GDP <- cut(GDP, breaks=L.al, right=F) # fija los niveles de los intervalos
   x.al<- num_clases-1
   MC <- (L.al[1]+L.al[2])/2+amplitud*(0:x.al)       # las marcas de clase
   int_MC <- cut(GDP, breaks=L.al, labels=MC, right=F) # fija los niveles de las marcas de clase
   
   
   # c.2.- Frecuencias absolutas y relativas, también acumuladas
   
   table(int_GDP) # frecuencias absolutas
   cumsum(table(int_GDP)) # frecuencias absolutas acumuladas
   prop.table(table(int_GDP)) # frecuencias relativas
   cumsum(prop.table(table(int_GDP))) # frecuencias relativas acumuladas 
   
   
   # c.3.- FUNCIÓN: histograma
   
    # c.3.i.- hist_real
   
   hist_real<- function(x,L){
     h<- hist(x, breaks=L, right=F, plot=F)
     t<- round(1.1*max(max(density(x)[[2]]), h$density), 2)
     plot(h, freq=F, col="yellow", 
      main="Histograma frec. relativas y\n
      curva distribución estimada",
      xaxt="n",  xlab="Intervalos", ylab="Densidades")
     axis(1, at=L)
     text(h$mids, h$density/2, 
       labels<- round(h$counts/length(x), 2), col="blue")
     lines(density(x), col="red", lwd=2)
   }
   
   # c.3.ii.- hist_real.cum
   
   hist_real.cum <- function(x,L){
     h<- hist(x, breaks=L, right=F, plot=F)
     h$density=cumsum(h$counts)/length(x)
     plot(h, freq=F, main="Histograma frec. rel. acumuladas\n
          y curva distr. estimada", xaxt="n", col="green",
          xlab="Intervalos", ylab="Frec. relativas acumuladas")
     axis(1, at=L)
     text(h$mids, h$density/2, 
          labels <- round(h$density, 2), col="blue")
     dens.x <- density(x)
     dens.x$y <- cumsum(dens.x$y)*(dens.x$x[2]-dens.x$x[1])
     lines(dens.x, col="red", lwd=2)
   }
   
  
   # c.4.- Gráfico: histograma de frecuencias relativas acumuladas
  
    par(mfrow = c(1, 2))
       hist(GDP, breaks=L.al, right=F, plot=T, main="Histograma frec. absolutas",
         xlab="Intervalos", ylab="Frecuencia absolutas", col=13)
    
       plot(density(GDP), col="red", lwd=2)
     
     par(mfrow = c(1, 2))
     hist_real(GDP, L.al)
     hist_real.cum(GDP, L.al)
   

    #   E.1- TRANSFORMACIÓN: Relación entre URBAN_POPULATION & RURAL_POPULATION
    #   --------------------------------------------- 
    
    
    # a.- Se extraerán solo los datos URBAN_POPULATION & RURAL_POPULATION y nombre país correspondiente.       
    
    tbl_POPULATION <- tbl_countries_w[tbl_countries_w$URBAN_POPULATION!='na'&tbl_countries_w$RURAL_POPULATION!='na', c("NAME","URBAN_POPULATION","RURAL_POPULATION")]
    
     
    # b.- Se convierten los caracteres en números para hacer cálculos. 
    
    URBAN_POPULATION2 <- as.numeric(tbl_POPULATION$URBAN_POPULATION) # numero <- caracter
    RURAL_POPULATION2 <- as.numeric(tbl_POPULATION$RURAL_POPULATION)
    
    
    # c.- Se añaden dos nuevas columnas y eliminan otras dos obsoletas
    
    tbl_POPULATION <- cbind(tbl_POPULATION, URBAN_POPULATION2,RURAL_POPULATION2 )
    tbl_POPULATION <- tbl_POPULATION[ ,-(2:3)] # Elimina las filas con caracteres
   
    
    # d.- Se cambian los nombres a las columnas
    
    names(tbl_POPULATION) <- c('País', 'Urbana', 'Rural') # Cambiar nombres 3 atributos
    
    
    # e.- Se calculan las frecuencias relativas acumuladas respectivas
    
    Population_Urbana <- mean(tbl_POPULATION$Urbana)
    Population_Rural <- mean(tbl_POPULATION$Rural)
   
    
    #   E.2- GRÁFICO: Gráfico por sectores con "highchart". 
    #   ---------------------------------------------
    
    library(highcharter)
   
     population <- data.frame(nombre = c('RURAL', 'URBANA'),
                 Freq = c(Population_Rural, Population_Urbana))
    
    hc_pie <- highchart(width = 550, height = 550) %>%
      hc_title(text = "Distribución de Población Mundial: RURAL & URBANA") %>%
      hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 40, beta =0)) %>%
      hc_plotOptions(pie = list(depth = 55)) %>%
      hc_add_series_labels_values(population$nombre, population$Freq)
    hc_pie
    
    
    #   F.1- TRANSFORMACIÓN: RECODIFICACIÓN DE DATOS atributo "MAIN_SECTOR"
    
    tbl_countries_w$MAIN_SECTOR = recode(tbl_countries_w$MAIN_SECTOR, 
          " 'Primary'='Agricultura'; 'Secondary'='Industria'; 'Tertiary'='Servicios'; else='S/N' " )
    
   
     #   F.2- GRÁFICO: # Gráfico por sectores con "highchart"
    
    tbl_sectores <- as.data.frame(table(tbl_countries_w$MAIN_SECTOR))
   
    hc_pie <- highchart(width = 550, height = 550) %>%
      hc_title(text = "Distribución Principales Sectores: RURAL & URBANA") %>%
      hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 40, beta =0)) %>%
      hc_plotOptions(pie = list(depth = 55)) %>%
      hc_add_series_labels_values(tbl_sectores$Var1, tbl_sectores$Freq)
    hc_pie
    
     
  #   G.1- TRANSFORMACIÓN: Relación entre atributos a extraer c(1, 6, 4, 15, 14, 16, 20, 21, 22, 24) 
     
    
    # a.- Se extraerán solo los datos de los atributos que interesan y cambia el orden presentación.       
    
    tbl_Total_df <- as.data.frame(tbl_countries_w)
    tbl_Total_df2 <- tbl_Total_df[ , c(1, 6, 4, 15, 14, 16, 20, 21, 22, 24)]
    tbl_Total_10c <-  tbl_Total_df2[tbl_Total_df2$DOCTORS!='na' & tbl_Total_df2$TERTIARY_SECTOR!='na' & tbl_Total_df2$PHONES_RATE!='na' &
                      tbl_Total_df2$NATURAL_GROWTH!='na' & tbl_Total_df2$WOMEN_LIFE_EXP!='na', c("NAME","CONTINENT","POPULATION","GDP_GROW_RATE",
                      "GDP_$_PER_CAPITA","DOCTORS","TERTIARY_SECTOR","PHONES_RATE", "NATURAL_GROWTH","WOMEN_LIFE_EXP" )]
    
    
    # b.- Se convierten los caracteres en números para hacer cálculos. 
    
    DOCTORS2 <- as.numeric(tbl_Total_10c$DOCTORS) # numero <- caracter
    TERTIARY_SECTOR2 <- as.numeric(tbl_Total_10c$TERTIARY_SECTOR) # numero <- caracter
    PHONES_RATE2 <- as.numeric(tbl_Total_10c$PHONES_RATE) # numero <- caracter
    NATURAL_GROWTH2 <- as.numeric(tbl_Total_10c$NATURAL_GROWTH) # numero <- caracter
    WOMEN_LIFE_EXP2 <- as.numeric(tbl_Total_10c$ WOMEN_LIFE_EXP) # numero <- caracter
    
    
    # c.- Se añaden nuevas columnas y eliminan las obsoletas
    
    tbl_Total_10c <- cbind(tbl_Total_10c, DOCTORS2, TERTIARY_SECTOR2, PHONES_RATE2, NATURAL_GROWTH2, WOMEN_LIFE_EXP2 )
    tbl_Total_10c <-  tbl_Total_10c[ ,-(6:10)] # Elimina las filas con caracteres
    
    
    # d.- Se cambian los nombres a las columnas
    
    names(tbl_Total_10c) <- c('País', 'Continente', 'Habitantes', 'Ratio_Crec_PIB', 'PIBP', 'Doctores',
                           'Servicios', 'Ratio_Tfno', 'Crec_Natural', 'Exp_Vida_M' ) 
   
    
  #  G.2- ANÁLISIS DE LOS DATOS y GRÁFICOS
    
    
    #  a.- Matriz de datos
    
    Total_10c <- as.data.frame(tbl_Total_10c)
    
    
    # b.- Análisis de correlación: Calcular la intensidad de la relación.
    
    cor(Total_10c[3:10])
    
    
    # c.- Matriz de gráficos de dispersión: Explorar la relación entre todas las parejas de variables
    
    library(car)
    scatterplotMatrix(Total_10c[3:10], diagonal = "hist")    
        
    
    # d.- Gráfico de barras para Correlación CP y Variables
    
    par(mfrow = c(1, 1))
    bulnesia.acp <- prcomp(Total_10c[3:10], scale = TRUE)
    bulnesia.cor <- bulnesia.acp$rotation %*% diag(bulnesia.acp$sdev)
    barplot(t(bulnesia.cor[, 1:3]), beside = TRUE, ylim = c(-0.85, 0.85), col = rainbow(20))

    
        
    #   H.1- TRANSFORMACIÓN: NORMALIZACIÓN 
    
     # a.- Ratio_Crec_PIB: De los gráficos se observa cierta normalidad
     
       tbl_Ratio_Crec_PIB <- Total_10c$Ratio_Crec_PIB
       tbl_Ratio_Crec_PIB
     
   
    # b.- Pruebas de Normalidad del Paquete "normtest"
          ###Prueba de Geary###
          ###Usa los valores acumulados muestrales, sus medias y desviaciones estándar.###
     
       library(normtest)
    
       geary.norm.test(tbl_Ratio_Crec_PIB)
     
     
    #   H.2- GRÁFICOS
    
    # a.- Histograma básico
     
      hist(tbl_Ratio_Crec_PIB)
    
    # b.- para conocer el ajuste a una distribución normal
    
     qqnorm(tbl_Ratio_Crec_PIB)
     qqline(tbl_Ratio_Crec_PIB)
    
    # c.- comparar la densidad de observada con la densidad teórica (normal)
    
    hist(tbl_Ratio_Crec_PIB, freq = F,
         ylab = "Densidad",
         xlab = "tbl_Ratio_Crec_PIB", main = "Ratio_Crec_PIB")
    
    dz <- density(tbl_Ratio_Crec_PIB)
    lines(dz, col = "red", lwd = 3)                                   # observada
    
    curve(dnorm(x, mean(tbl_Ratio_Crec_PIB), sd(tbl_Ratio_Crec_PIB)),
          col = "blue", lwd = 3, add = TRUE)                           # teórica 
    
   
    
    # H.3- ANÁLISIS DE LOS DATOS 
    
    
    # a.- Se añaden eliminan las columnas ajenas
    
    tbl_Total_10c1 <-  tbl_Total_10c
    tbl_Total_10c2 <-  tbl_Total_10c1[ ,-(5:10)] # Elimina las filas 
    tbl_Total_10c3 <-  tbl_Total_10c2[ ,-(2:3)] # Elimina las filas
    
    
    #  b.- Se normalizan los datos
    
    datos <- tbl_Total_10c3[ , 2]
    Ratio_Crec_PIB2 <- (datos-mean(datos))/sd(datos)
    
    tbl_Total_10c3 <- cbind(tbl_Total_10c3, Ratio_Crec_PIB2)
    
    par(mfrow = c(1, 2))
    
    # ORIGINAL
    
    hist(tbl_Total_10c3[ , 2], freq = F,
         ylab = "Densidad",
         xlab = "tbl_Ratio_Crec_PIB", main = "Ratio_Crec_PIB: ORIGINAL")
    
    dz <- density(tbl_Total_10c3[ , 2])
    lines(dz, col = "red", lwd = 3) 
    
    # NORMALIZADA
    
    hist(tbl_Total_10c3[ , 3], freq = F,
         ylab = "Densidad",
         xlab = "tbl_Ratio_Crec_PIB", main = "Ratio_Crec_PIB: NORMALIZADA")
    
    dz <- density(tbl_Total_10c3[ , 3])
    lines(dz, col = "BLUE", lwd = 3) 
    
    
    # c.- Comparar resultados de las Pruebas de Normalidad de Geary
    
    geary.norm.test(tbl_Total_10c3[ , 2])
    geary.norm.test(tbl_Total_10c3[ , 3])
   
    
    # d.- Observar diferencias en el ajuste a una distribución normal
    
      par(mfrow = c(1, 2))
    
      qqnorm(tbl_Total_10c3[ , 2])
      qqline(tbl_Total_10c3[ , 2])
    
      qqnorm(tbl_Total_10c3[ , 3])
      qqline(tbl_Total_10c3[ , 3])
    
# ....................... THE END .......................... 
    
    