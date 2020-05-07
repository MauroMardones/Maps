rm(list =ls())
install.packages("rgdal")
install.packages("sf")
install.packages("maptools")
install.packages("ggsidekick")
install.packages("ggmap")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("vidiris")
install.packages("vidirisLite")
install.packages('ggspatial')
install.packages("marmap")
library(ggspatial)
library(RColorBrewer)
library(ggsidekick)
library(rgdal)
library(ggplot2)
library(maptools)
library(sf)
library(RColorBrewer)
library(tidyverse)
library(patchwork)
library(marmap)
library(mapproj)
library(maps)
library(ggmap)
library(patchwork)
library(viridis)
library(raster)

display.brewer.all()

setwd("C:/Users/mauricio.mardones/Documents/IFOP/Maps_R/Maps_2")
dir()
# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
mapzona<-readOGR(dsn="C:/Users/mauricio.mardones/Documents/IFOP/Maps_R/Maps_2",layer="10y11")
#mapzona <- spTransform(mapzona, CRS("+proj=robin"))
# other way to Load the shapefile and convert to a data frame for use in ggplot2
#mapzona2 <- readShapePoly("10y11.shp")
#mapzona_df2 <- fortify(mapzona2)

point <- read.csv2("geo_erizo_1996_2019.csv", sep=",", stringsAsFactors=FALSE, na.strings = T)
#miro las columnas de interes y su formato
head(point [c(3, 4, 8, 15 )], 10)
# PROCED   LATITUD  LONGITUD      X REGION PUERTO ANO_REG MES_REG
# 1    7777 -43.94778 -74.03067 173197     11    961    2015       5
# 2    7777 -43.94778 -74.03067 173193     11    961    2015       5
# 3    7777 -43.94778 -74.03067 163358     11    961    2016       8
# 4    7777 -43.94778 -74.03067 173145     11    961    2015       8
# 5    7777 -43.94778 -74.03067 162526     11    961    2016       5
# 6    7777 -43.94778 -74.03067 173196     11    961    2015       5
# 7    7777 -43.94778 -74.03067 173200     11    961    2015       6
# 8    7777 -43.94778 -74.03067 162411     11    961    2016       5
# 9    7777 -43.94778 -74.03067 173187     11    961    2015       5
# 10   7777 -43.94778 -74.03067 173317     11    961    2015      10

#estrucutura de los datos
glimpse(point)
class(mapzona_df2)
class(mapzona_df)

#cambio algunos formatos
point$LONGITUD = as.numeric(as.character(point$LONGITUD))
point$ANO_REG = as.numeric(point$ANO_REG)
point$MES_REG = as.numeric(point$MES_REG)
point$LATITUD = as.numeric(as.character(point$LATITUD))
point$CAPTURA_2 = as.numeric(as.character(point$CAPTURA_2))

# ejemplo de datos
#locs <- data.frame(long=c(-172,-173,-170,-170.5,-170.52), lat=c(62.1,62.2,62.3,63,65))
#Saco los datos mayo ar cero y cap a t.
point <- filter(point, LONGITUD < 0) %>% 
  mutate(CAPT = CAPTURA_2/1000) 
head(point)

# Next the shapefile has to be converted to a dataframe for use in ggplot2
mapzona_df <- fortify(mapzona)

head(mapzona_df)

# Magnitud de capturas 
# aqui filtro un año por que mi base es muy pesada y se demora en plotear. 
#Dejo solo el 2019

p <- ggplot()+ 
  geom_sf()+
  geom_polygon(data=mapzona_df, aes(long, lat, group = group), 
               size = 0.7, fill="black", alpha = .5)+
  geom_point(data=point %>% 
               filter(ANO_REG == 2018), aes(x=LONGITUD, y=LATITUD, 
                   colour=CAPT, size=CAPT, alpha=0.3))+
  scale_alpha(guide = 'none')+
  scale_size(guide = 'none')+
  scale_x_discrete()+
  scale_y_discrete()+
  annotation_north_arrow(location='tr',
                         height = unit(1.5, "cm"),
                         width = unit(1.5, "cm"))+
  #theme_bw()+coord_map() +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
  #scale_colour_gradientn(colours = heat.colors(10))+
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  theme(panel.background = element_rect(fill = "aliceblue"))+
  theme(legend.position = "right")+
  labs(color = "Capt (t.)")
p 


# Breaks para Legenda
mybreaks <- c(1, 15, 50)

#Aqui con fondo 
n <- ggplot() +
  geom_polygon(data = mapzona_df, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.8) +
  geom_point(data=point %>% 
               filter(ANO_REG == 2019), aes(x=LONGITUD, y=LATITUD, 
                                            size=CAPT, color=CAPT, alpha=CAPT),
             shape=20) +
  scale_size_continuous(name="Capt (t.)",  range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Capt (t.)",  range=c(1, 12), breaks=mybreaks) +
  scale_color_viridis(option="inferno", trans="log", breaks=mybreaks, name="Capt (t.)" ) +
  annotation_north_arrow(location='tr',
                         height = unit(1.5, "cm"),
                         width = unit(1.5, "cm"))+
  theme_bw(base_size=15) +  coord_map() + 
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  guides( colour = guide_legend()) +
  #ggtitle("Capturas Erizo (t) 2019") +
  theme(
    legend.position="right"
    #text = element_text(color = "#22211d"),
    #plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA)
    #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
    #                         margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
n 

#Aqui sin fondo con theme_void
sf <- ggplot() +
  geom_polygon(data = mapzona_df, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.8) +
  geom_point(data=point %>% 
               filter(ANO_REG == 2019), aes(x=LONGITUD, y=LATITUD, 
                                            size=CAPT, color=CAPT, alpha=CAPT),
             shape=20) +
  scale_size_continuous(name="Capt (t.)",  range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Capt (t.)",  range=c(1, 12), breaks=mybreaks) +
  scale_color_viridis(option="plasma", trans="log", breaks=mybreaks, name="Capt (t.)" ) +
  annotation_north_arrow(location='tr',
                         height = unit(1.5, "cm"),
                         width = unit(1.5, "cm"))+
  theme_void(base_size=15) +  coord_map() + 
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  guides( colour = guide_legend()) +
  #ggtitle("Capturas Erizo (t) 2019") +
  theme(
    legend.position="right"
    #text = element_text(color = "#22211d"),
    #plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA)
    #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
    #                         margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
sf + n +p 

#aqui se muestra la distribucion y magnituid a traves del tiempo, 
#en este caso, los meses que dura la temporada 

n <- ggplot() +
  geom_polygon(data = mapzona_df, aes(x=long, y = lat, group = group), 
               fill="grey", alpha=0.8) +
  geom_point(data=point %>% 
               filter(ANO_REG == 2019), aes(x=LONGITUD, y=LATITUD, 
                                            size=CAPT, color=CAPT, alpha=CAPT),
             shape=20) +
  scale_size_continuous(name="Capt (t.)",  range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Capt (t.)",  range=c(1, 12), breaks=mybreaks) +
  scale_color_viridis(option="cividis", trans="log", breaks=mybreaks, name="Capt (t.)" ) +
  annotation_north_arrow(ocation='tr',
                         height = unit(1.5, "cm"),
                         width = unit(1.5, "cm"))+
  theme_void(base_size=15) +  coord_map() + 
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  guides( colour = guide_legend()) +
  facet_wrap(~MES_ARR, ncol = 5)+
  #ggtitle("Capturas Erizo (t) 2019") +
  theme(
    legend.position="right"
    #text = element_text(color = "#22211d"),
    #plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA)
    #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
    #                         margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
n 

ggsave("2019.pdf", height = 20, width = 8, dpi = 200)


inset_map <- ggplot() +
  geom_sf() 
#Dealing with Overplotting
#Problem
#You have many points and they obscure each other
# hegagonos de cantifdad de registros
h <- ggplot()+ 
  geom_sf(fill= "antiquewhite")+
  geom_polygon(data=mapzona_df, aes(long, lat, group = group), 
  size = 0.7, fill="gray", alpha = .8)+
  #geom_point(data=point, aes(x=LONGITUD, y=LATITUD, size = CAPT, alpha=1/2), bins=30)+
  geom_hex(data=point, aes(x=LONGITUD, y=LATITUD), bins=30)+
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10")+
  #facet_wrap(~ANO_ARR)+
  scale_x_discrete()+
  scale_y_discrete()+
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  theme(panel.background = element_rect(fill = "aliceblue"))+
  theme(legend.position="right")

#Jutar los plot
h + p


#solo un año  2019

 point2019 <- point %>% 
   filter(ANO_REG == 2019)

 u <- ggplot()+ 
   geom_sf(fill= "antiquewhite" ,  color = "black")+
   geom_polygon(data=mapzona_df, aes(long, lat, group = group), 
                size = 0.7, fill = 8, alpha=0.8)+
   geom_point(data=point %>% 
                filter(ANO_REG == 2018), aes(x=LONGITUD, y=LATITUD, 
                                             colour=CAPT, size=CAPT, alpha=1/10))+
   facet_wrap(~MES_REG)+
   scale_x_discrete()+
   scale_y_discrete()+
   annotation_north_arrow(location='tr')+
   theme_bw() +  coord_map() +
   xlab(expression(paste(Longitude^o,~'O'))) +
   ylab(expression(paste(Latitude^o,~'S')))+
   theme(panel.background = element_rect(fill = 'aliceblue'),
         #panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank())+
   theme(legend.position = "none")
 u 
ggsave("2019_2.pdf", height = 8, width = 5, dpi = 200)


# otro tipo de plot con marmap
#hago la zona del mapa
bathy <- getNOAA.bathy(-75,-72,-46,-41, resolution=2, keep=TRUE)

mydata <- getData("SRTM", lon=5, lat=45)
mymap <- fortify(mydata)

plot(bathy, image = TRUE, land = TRUE, axes = FALSE, lwd=0.1,
     bpal = list(c(0, max(bathy), grey(.7), grey(.9), grey(.95)),
                 c(min(bathy), 0, "darkblue", "lightblue")))
plot(bathy, n = 1, lwd = 0.5, add = TRUE)

plot(bathy, image = TRUE, land = TRUE, lwd = 0.1,
     bpal = list(c(0, max(bathy), "grey"),
                 c(min(bathy),0,blues)))
# Making the coastline more visible
plot(bathy, deep = 0, shallow = 0, step = 0,
     lwd = 0.4, add = TRUE)

> # plot map
par(mai=c(1,1,1,1.5))
plot(bathy, deep = c(-4500, 0), shallow = c(-50, 0), step = c(500, 0),
       lwd = c(0.3, 1), lty = c(1, 1), col = c("grey", "black"),
       drawlabels = c(FALSE, FALSE))
scaleBathy(bathy, deg = 3, x = "bottomleft", inset = 5)
box()
# set color palette
max(-sp$depth, na.rm = TRUE) -> mx
colorRampPalette(c("white", "lightyellow", "lightgreen",
                     "blue", "lightblue1", "purple")) -> ramp
blues <- ramp(mx)
# plot points and color depth scale
points(sp[,1:2], col = "black", bg = blues[-sp$depth],
           pch = 21, cex = 1.5)
library(shape)
colorlegend(zlim = c(mx, 0), col = rev(blues), main = "depth (m)",
              posx = c(0.85, 0.88))
plot(bathy, deep = 0, shallow = 0, step = 0,
     lwd = 0.4, add = TRUE)

bathy <- fortify(bathy)

sizePoints <- point$CAPTURA_1/max(point$CAPTURA_1)
sizePoints <- point2019$CAPTURA_1/max(point2019$CAPTURA_1)


#creo paleta de colores
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
#ploteo y leo los datos
#map <-autoplot(bathy, geom=c("r", "b"),  show.legend=FALSE) + 
mapr <-autoplot(bathy, geom="tile", coast=TRUE,  show.legend=FALSE) + 
  scale_fill_gradient2(low = blues,   high = greys)+
  #scale_fill_gradient2(low="aliceblue", mid="white", high="white")+
  #scale_fill_etopo() + labs(x = "Longitude", y = "Latitude")+ 
  geom_point(data=point %>% 
               filter(ANO_REG == 2018), aes(x=LONGITUD, y=LATITUD,size=CAPT,  alpha=.8))+
  facet_wrap(~MES_REG ,ncol = 5)+
  annotate("text", x = -74, y = -41.5, label = "PMZC")+
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  theme(legend.position = "none")
mapr 

ggsave("mapCaperimes2019.pdf", width = 10, height = 5)


data(nw.atlantic)
atl <- as.bathy(nw.atlantic)


#ahora ploteo las 3 regiones

#hago la zona del mapa
bathy2 <- getNOAA.bathy(-75,-67,-55,-40, resolution=1, keep=TRUE)

#leo data
point3 <- read.csv2("Capt_geo_X_XII_2000_2018_suma.csv", sep=";", stringsAsFactors=FALSE, na.strings = T)
names(point3)

#transforme vairiables

point3$LONGITUD = as.numeric(as.character(point3$LONGITUD))
point$ANO_REG = as.numeric(point$ANO_REG)
point$MES_REG = as.numeric(point$MES_REG)
point3$LATITUD = as.numeric(as.character(point3$LATITUD))
point$CAPTURA_1 = as.numeric(as.character(point$CAPTURA_1))


##Saco los datos mayo ar cero y cap a t.
point3 <- filter(point3, LONGITUD < 0) %>% 
  mutate(CAPT = CAPTURA/1000) 
head(point)

bathy <- getNOAA.bathy(-75,-72,-46,-41, resolution=1, keep=TRUE)

#creo paleta de colores
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
#ploteo y leo los datos
#map <-autoplot(bathy, geom=c("r", "b"),  show.legend=FALSE) + 
mapr <-autoplot(bathy, geom=c("r", "b"), coast=TRUE,  show.legend=FALSE) + 
  scale_fill_gradient2(low = blues,   high = greys)+
  #scale_fill_gradient2(low="aliceblue", mid="white", high="white")+
  #scale_fill_etopo() + labs(x = "Longitude", y = "Latitude")+ 
  geom_point(data=point %>% 
               filter(ANO_REG == 2019), aes(x=LONGITUD, y=LATITUD,
                      size=CAPT, colour=CAPT, alpha=CAPT), shape=20)+
  scale_size_continuous(name="Capt (t.)",  range=c(1,12), breaks=mybreaks) +
  #scale_alpha_continuous(name="Capt (t.)",  range=c(1, 12), breaks=mybreaks) +
  scale_color_viridis(option="inferno", trans="log", 
                      breaks=mybreaks, name="Capt (t.)" ) +
  annotation_north_arrow(location='tr',
                         height = unit(1.5, "cm"),
                         width = unit(1.5, "cm"))+
  guides( colour = guide_legend()) +
  scale_alpha(guide = 'none')+
  #facet_wrap(~MES_REG ,ncol = 5)+
  #annotate("text", x = -74, y = -41.5, label = "PMZC")+
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  theme(legend.position = "right")
mapr 


ggsave("mapCaperimes2019.pdf", width = 10, height = 5)


#Saco datos de un raster

chile<-getData('GADM',country='CHL',level=1)
aysen <- subset(chile [1,])
pm <- subset(chile [c(1,9),])

#para conocer los npomres y codes de cada region
chile@data$CC_1

#selccionando la region del Bío-Bío
plot(pm)

pm <- fortify(pm)

t <- ggplot() +
  geom_polygon(data=pm,aes( x= long, y=lat, group= group),
               fill="gray", alpha=0.99)+
  geom_point(data=point %>% 
               filter(ANO_REG == 2019), aes(x=LONGITUD, y=LATITUD,
                        colour=CAPT, size=CAPT, alpha=1/10))+
  #facet_wrap(~MES_REG, ncol = 5)+
  annotation_north_arrow(location='tr')+
  scale_size_continuous(name="Capt (t.)",  range=c(1,12), breaks=mybreaks) +
  #scale_alpha_continuous(name="Capt (t.)",  range=c(1, 12), breaks=mybreaks) +
  scale_color_viridis_c(option="magma",trans="log", breaks=mybreaks, name="Capt (t.)")+
  theme_bw() +  coord_map() +
  scale_alpha(guide="none")+
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  guides( colour = guide_legend()) +
  theme(panel.background = element_rect(fill = 'aliceblue'))+
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank())+
  theme(legend.position = "right")
t


o <- ggplot() +
  geom_polygon(data = pm, aes(x=long, y = lat, group = group), 
               fill="black", alpha=0.3) +
  geom_point(data=point %>% 
               filter(ANO_REG == 2019), aes(x=LONGITUD, y=LATITUD, 
                 size=CAPT, color=CAPT, alpha=CAPT),shape=20) +
  scale_size_continuous(name="Capt (t.)",  range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Capt (t.)",  range=c(1, 12), breaks=mybreaks) +
  scale_color_viridis_c(trans="log", breaks=mybreaks, name="Capt (t.)")+
  #scale_color_viridis(option="plasma", trans="log", breaks=mybreaks, name="Capt (t.)" ) +
  annotation_north_arrow(location='tr',
                         height = unit(1.5, "cm"),
                         width = unit(1.5, "cm"))+
  theme_bw(base_size=15) +  coord_map() + 
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  guides( colour = guide_legend()) +
  #ggtitle("Capturas Erizo (t) 2019") +
  theme(
    legend.position="right"
    #text = element_text(color = "#22211d"),
    #plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA)
    #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
    #                         margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
o









y <- ggplot() +
  geom_polygon(data=pm,aes( x= long, y=lat, group= group), fill = 8, alpha=0.8)+
  geom_point(data=point, aes(x=LONGITUD, y=LATITUD, colour=CAPT, size=CAPT, alpha=CAPT))+
  scale_size_continuous(name="Capt (t.)",  range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Capt (t.)",  range=c(1, 12), breaks=mybreaks) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Capt (t.)" ) +
  annotation_north_arrow(location='tr')+
  theme_bw(base_size=15) +  coord_map() + 
  xlab(expression(paste(Longitude^o,~'O'))) +
  ylab(expression(paste(Latitude^o,~'S')))+
  guides( colour = guide_legend()) +
  facet_wrap(~MES_ARR, ncol = 5)+
  #ggtitle("Capturas Erizo (t) 2019") +
  theme(
    legend.position="right"
    #text = element_text(color = "#22211d"),
    #plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    #legend.background = element_rect(fill = "#f5f5f2", color = NA)
    #plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", 
    #                         margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

y + t
