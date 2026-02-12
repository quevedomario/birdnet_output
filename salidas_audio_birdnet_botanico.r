# DATASET ####
load("botanico_202312_202402_v24.RData")

# ANÁLISIS BIRDNET ALISEDA ####
# Librerías ####
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra) # grid.arrange() varios ggplot en mismo panel
library(xkcd);library(extrafont) # solo para extética xkcd 

# Leer datos de xlsx (readxl) ####
names(aliseda)

# eliminando columnas sobrantes ####
str(aliseda) 
aliseda <- aliseda[,c(1:8)]
head(aliseda)

# manipulando tiempos (lubridate) ####
# substr(x, caracter inicio, caracter fin)
head(aliseda$filename)
aliseda$date <- paste(substr(aliseda$filename,1,4),'-', substr(aliseda$filename,5,6),'-',
                      substr(aliseda$filename,7,8))
head(aliseda$date)
aliseda$date <- ymd(aliseda$date) #formato interno años, meses, días
head(aliseda$date)

aliseda$file_time <- paste(substr(aliseda$filename,10,11),':', substr(aliseda$filename,12,13),
                           ':',substr(aliseda$filename,14,15))
head(aliseda$file_time)
aliseda$file_time <- hms(aliseda$file_time) # formato interno horas, minutos, segundos

offset <- seconds_to_period(aliseda$start)
head(offset)
aliseda$time_utc <- aliseda$file_time + offset
aliseda$time <- aliseda$time_utc + hours(1)

# inspección datos ####
aliseda$duration <- aliseda$end-aliseda$start
(horas.detectando.aliseda <- sum(aliseda$duration)/3600) 
tapply(aliseda$duration, aliseda$scientific_name, sum)

# filtrando nivel de confianza #### 
browseURL("https://onlinelibrary.wiley.com/doi/10.1111/ibi.13193")
aliseda.60 <- subset(aliseda, aliseda$confidence >= 0.60)
aliseda.60.audit <- subset(aliseda.60, aliseda.60$aceptado == 1)

# resumen.aliseda ####
names(aliseda.60)
(duration.aliseda <- aggregate(duration ~ scientific_name, aliseda.60, sum))
(confidence.aliseda <- aggregate(confidence ~ scientific_name, aliseda.60, max))

resumen.aliseda <- merge(duration.aliseda, confidence.aliseda)
head(resumen.aliseda)
resumen.aliseda$contactos <- resumen.aliseda$duration / 3
resumen.aliseda$duration <- NULL
resumen.aliseda$especie <- resumen.aliseda$scientific_name
resumen.aliseda$scientific_name <- NULL
resumen.aliseda$props <- resumen.aliseda$contactos / sum(resumen.aliseda$contactos)
resumen.aliseda$orden <- rank(resumen.aliseda$contactos, ties.method="first")
(resumen.aliseda <- resumen.aliseda[order(resumen.aliseda$orden, decreasing = T),])

# de formato largo de datos a ancho, para usar libreria vegan ####
head(resumen.aliseda)
resumen.aliseda.ancho <- resumen.aliseda[,c(2,3)]
resumen.aliseda.ancho <- tidyr::pivot_wider(resumen.aliseda.ancho, names_from = "especie",
                                            values_from = "contactos")
head(resumen.aliseda.ancho)

## diversidad con vegan ####
(simpsonD.aliseda <- vegan::diversity (resumen.aliseda.ancho, index="invsimpson"))
((riqueza.aliseda <- vegan::specnumber(resumen.aliseda.ancho)))
(equita.aliseda <- simpsonD.aliseda / riqueza.aliseda)

# barras ####
# library(ggplot2) ; # library(gridExtra) # grid.arrange() varios ggplot en mismo panel
# library(xkcd);library(extrafont)
browseURL("http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization")
# browseURL("https://www.geeksforgeeks.org/how-to-change-the-order-of-bars-in-bar-chart-in-r/")
(aliseda.barras <- 
    ggplot(data=resumen.aliseda, aes(x = reorder(especie, +props), y=props)) +
    geom_bar(stat="identity", color="blue", fill="white") +
    geom_text(aes(label=contactos), hjust=-0.1, size=3) + labs(title="", x="", y="prop. contactos, Aliseda") +
    theme(text = element_text(size = 14, family = "xkcd")))
(aliseda.barras <- aliseda.barras + coord_flip())

# ANÁLISIS BIRDNET BOREALAM ####
names(borealam)

# eliminando columnas sobrantes ####
str(borealam) 
borealam <- borealam[,c(1:8)]
head(borealam)

# manipulando tiempos (lubridate) ####
# substr(x, caracter inicio, caracter fin)
head(borealam$filename)
borealam$date <- paste(substr(borealam$filename,1,4),'-', substr(borealam$filename,5,6),'-',
                       substr(borealam$filename,7,8))
head(borealam$date)
borealam$date <- ymd(borealam$date) #formato interno años, meses, días
head(borealam$date)

borealam$file_time <- paste(substr(borealam$filename,10,11),':', substr(borealam$filename,12,13),
                            ':',substr(borealam$filename,14,15))
head(borealam$file_time)
borealam$file_time <- hms(borealam$file_time) # formato interno horas, minutos, segundos

offset <- seconds_to_period(borealam$start)
head(offset)
borealam$time_utc <- borealam$file_time + offset
borealam$time <- borealam$time_utc + hours(1)

# inspección datos ####
borealam$duration <- borealam$end-borealam$start
(horas.detectando.borealam <- sum(borealam$duration)/3600) 
tapply(borealam$duration, borealam$scientific_name, sum)

# filtrando nivel de confianza #### 
# browseURL("https://onlinelibrary.wiley.com/doi/10.1111/ibi.13193")
borealam.60 <- subset(borealam, borealam$confidence >= 0.60)
borealam.60.audit <- subset(borealam.60, borealam.60$aceptado == 1)

# resumen.borealam ####
names(borealam.60)
(duration.borealam <- aggregate(duration ~ scientific_name, borealam.60, sum))
(confidence.borealam <- aggregate(confidence ~ scientific_name, borealam.60, max))

resumen.borealam <- merge(duration.borealam, confidence.borealam)
head(resumen.borealam)
resumen.borealam$contactos <- resumen.borealam$duration / 3
resumen.borealam$duration <- NULL
resumen.borealam$especie <- resumen.borealam$scientific_name
resumen.borealam$scientific_name <- NULL
resumen.borealam$props <- resumen.borealam$contactos / sum(resumen.borealam$contactos)
resumen.borealam$orden <- rank(resumen.borealam$contactos, ties.method="first")
(resumen.borealam <- resumen.borealam[order(resumen.borealam$orden, decreasing = T),])

# de formato largo de datos a ancho, para usar libreria vegan ####
head(resumen.borealam)
resumen.borealam.ancho <- resumen.borealam[,c(2,3)]
resumen.borealam.ancho <- tidyr::pivot_wider(resumen.borealam.ancho, names_from = "especie",
                                             values_from = "contactos")
head(resumen.borealam.ancho)

## diversidad con vegan ####
(simpsonD.borealam <- vegan::diversity (resumen.borealam.ancho, index="invsimpson"))
((riqueza.borealam <- vegan::specnumber(resumen.borealam.ancho)))
(equita.borealam <- simpsonD.borealam / riqueza.borealam)

# barras ####
# library(ggplot2) ; # library(gridExtra) # grid.arrange() varios ggplot en mismo panel
# library(xkcd);library(extrafont)
# browseURL("http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization")
# browseURL("https://www.geeksforgeeks.org/how-to-change-the-order-of-bars-in-bar-chart-in-r/")
borealam.barras <- 
  ggplot(data=resumen.borealam, aes(x = reorder(especie, +props), y=props)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  geom_text(aes(label=contactos), hjust=-0.1, size=3) +
  labs(title="",x="", y="prop. contactos, Boreal Americano") +
  theme(text = element_text(size = 14, family = "xkcd"))
# theme_xkcd()
(borealam.barras <- borealam.barras + coord_flip())

# ANÁLISIS BIRDNET CANTÁBRICO ####
names(cantabrico)

# elimando columnas sobrantes ####
str(cantabrico) 
cantabrico <- cantabrico[,c(1:8)]
head(cantabrico)

# manipulando tiempos (lubridate) ####
# substr(x, caracter inicio, caracter fin)
head(cantabrico$filename)
cantabrico$date <- paste(substr(cantabrico$filename,1,4),'-', substr(cantabrico$filename,5,6),'-',
                         substr(cantabrico$filename,7,8))
head(cantabrico$date)
cantabrico$date <- ymd(cantabrico$date) #formato interno años, meses, días
head(cantabrico$date)

cantabrico$file_time <- paste(substr(cantabrico$filename,10,11),':', substr(cantabrico$filename,12,13),
                              ':',substr(cantabrico$filename,14,15))

head(cantabrico$file_time)
cantabrico$file_time <- hms(cantabrico$file_time) # formato interno horas, minutos, segundos

offset <- seconds_to_period(cantabrico$start)
head(offset)
cantabrico$time_utc <- cantabrico$file_time + offset
cantabrico$time <- cantabrico$time_utc + hours(1)

# inspección datos ####
cantabrico$duration <- cantabrico$end-cantabrico$start
(horas.detectando.cantabrico <- sum(cantabrico$duration)/3600) 
tapply(cantabrico$duration, cantabrico$scientific_name, sum)

# filtrando nivel de confianza #### 
browseURL("https://onlinelibrary.wiley.com/doi/10.1111/ibi.13193")
cantabrico.60 <- subset(cantabrico, cantabrico$confidence >= 0.60)
cantabrico.60.audit <- subset(cantabrico.60, cantabrico.60$aceptado == 1)

# resumen.cantabrico ####
names(cantabrico.60)
(duration.cantabrico <- aggregate(duration ~ scientific_name, cantabrico.60, sum))
(confidence.cantabrico <- aggregate(confidence ~ scientific_name, cantabrico.60, max))

resumen.cantabrico <- merge(duration.cantabrico, confidence.cantabrico)
head(resumen.cantabrico)
resumen.cantabrico$contactos <- resumen.cantabrico$duration / 3
resumen.cantabrico$duration <- NULL
resumen.cantabrico$especie <- resumen.cantabrico$scientific_name
resumen.cantabrico$scientific_name <- NULL
resumen.cantabrico$props <- resumen.cantabrico$contactos / sum(resumen.cantabrico$contactos)
resumen.cantabrico$orden <- rank(resumen.cantabrico$contactos, ties.method="first")
(resumen.cantabrico <- resumen.cantabrico[order(resumen.cantabrico$orden, decreasing = T),])

# de formato largo de datos a ancho, para usar libreria vegan ####
head(resumen.cantabrico)
resumen.cantabrico.ancho <- resumen.cantabrico[,c(2,3)]
resumen.cantabrico.ancho <- tidyr::pivot_wider(resumen.cantabrico.ancho, names_from = "especie",
                                               values_from = "contactos")
head(resumen.cantabrico.ancho)

## diversidad con vegan ####
(simpsonD.cantabrico <- vegan::diversity (resumen.cantabrico.ancho, index="invsimpson"))
((riqueza.cantabrico <- vegan::specnumber(resumen.cantabrico.ancho)))
(equita.cantabrico <- simpsonD.cantabrico / riqueza.cantabrico)

# barras ####
# library(ggplot2) ; # library(gridExtra) # grid.arrange() varios ggplot en mismo panel
# library(xkcd);library(extrafont)
browseURL("http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization")
# browseURL("https://www.geeksforgeeks.org/how-to-change-the-order-of-bars-in-bar-chart-in-r/")
cantabrico.barras <- 
  ggplot(data=resumen.cantabrico, aes(x = reorder(especie, +props), y=props)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  geom_text(aes(label=contactos), hjust=-0.1, size=3) +
  labs(title="",x="", y="prop contactos, Cantábrico") +
  theme(text = element_text(size = 14, family = "xkcd"))
# theme_xkcd()
(cantabrico.barras <- cantabrico.barras + coord_flip())

# ANÁLISIS BIRDNET CARBAYEDA ####
names(carbayeda)

# elimando columnas sobrantes ####
str(carbayeda) 
carbayeda <- carbayeda[,c(1:8)]
head(carbayeda)

# manipulando tiempos (lubridate) ####
# substr(x, caracter inicio, caracter fin)
head(carbayeda$filename)
carbayeda$date <- paste(substr(carbayeda$filename,1,4),'-', substr(carbayeda$filename,5,6),'-',
                        substr(carbayeda$filename,7,8))
head(carbayeda$date)
carbayeda$date <- ymd(carbayeda$date) #formato interno años, meses, días
head(carbayeda$date)

carbayeda$file_time <- paste(substr(carbayeda$filename,10,11),':', substr(carbayeda$filename,12,13),
                             ':',substr(carbayeda$filename,14,15))

head(carbayeda$file_time)
carbayeda$file_time <- hms(carbayeda$file_time) # formato interno horas, minutos, segundos

offset <- seconds_to_period(carbayeda$start)
head(offset)
carbayeda$time_utc <- carbayeda$file_time + offset
carbayeda$time <- carbayeda$time_utc + hours(1)

# inspección datos ####
carbayeda$duration <- carbayeda$end-carbayeda$start
(horas.detectando.carbayeda <- sum(carbayeda$duration)/3600) 
tapply(carbayeda$duration, carbayeda$scientific_name, sum)

# filtrando nivel de confianza #### 
browseURL("https://onlinelibrary.wiley.com/doi/10.1111/ibi.13193")
carbayeda.60 <- subset(carbayeda, carbayeda$confidence >= 0.60)
carbayeda.60.audit <- subset(carbayeda.60, carbayeda.60$aceptado == 1)

# resumen.carbayeda ####
names(carbayeda.60)
(duration.carbayeda <- aggregate(duration ~ scientific_name, carbayeda.60, sum))
(confidence.carbayeda <- aggregate(confidence ~ scientific_name, carbayeda.60, max))

resumen.carbayeda <- merge(duration.carbayeda, confidence.carbayeda)
head(resumen.carbayeda)
resumen.carbayeda$contactos <- resumen.carbayeda$duration / 3
resumen.carbayeda$duration <- NULL
resumen.carbayeda$especie <- resumen.carbayeda$scientific_name
resumen.carbayeda$scientific_name <- NULL
resumen.carbayeda$props <- resumen.carbayeda$contactos / sum(resumen.carbayeda$contactos)
resumen.carbayeda$orden <- rank(resumen.carbayeda$contactos, ties.method="first")
(resumen.carbayeda <- resumen.carbayeda[order(resumen.carbayeda$orden, decreasing = T),])

# de formato largo de datos a ancho, para usar libreria vegan ####
head(resumen.carbayeda)
resumen.carbayeda.ancho <- resumen.carbayeda[,c(2,3)]
resumen.carbayeda.ancho <- tidyr::pivot_wider(resumen.carbayeda.ancho, names_from = "especie",
                                              values_from = "contactos")
head(resumen.carbayeda.ancho)

## diversidad con vegan ####
(simpsonD.carbayeda <- vegan::diversity (resumen.carbayeda.ancho, index="invsimpson"))
((riqueza.carbayeda <- vegan::specnumber(resumen.carbayeda.ancho)))
(equita.carbayeda <- simpsonD.carbayeda / riqueza.carbayeda)

# barras ####
# library(ggplot2) ; # library(gridExtra) # grid.arrange() varios ggplot en mismo panel
# library(xkcd);library(extrafont)
browseURL("http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization")
# browseURL("https://www.geeksforgeeks.org/how-to-change-the-order-of-bars-in-bar-chart-in-r/")
carbayeda.barras <- 
  ggplot(data=resumen.carbayeda, aes(x = reorder(especie, +props), y=props)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  geom_text(aes(label=contactos), hjust=-0.1, size=3) +
  labs(title="",x="", y="prop. contactos, Carbayeda") +
  theme(text = element_text(size = 14, family = "xkcd"))
# theme_xkcd()
(carbayeda.barras <- carbayeda.barras + coord_flip())

# los 4 plots de barras ####
gridExtra::grid.arrange(aliseda.barras, borealam.barras, cantabrico.barras, 
                        carbayeda.barras, ncol=2)