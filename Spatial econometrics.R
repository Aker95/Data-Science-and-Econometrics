#mapas del modelo
library(sp); library(spdep); library(splm); library(plm); library(rgdal)
library(RColorBrewer); library(classInt); library(lattice); library(tmap)
library(stargazer); library(sf); library(maptools); library(tmap) 
library(tmaptools); library(tidyverse); library(usdm)
library(plm)
library(splm)
#Librería para las bases de datos
library(pder)
library(knitr)
library(gridExtra)

rm(list=ls())

setwd("C:/Users/Nick/Documents/Universidad/20211/Econometria Avanzada/Final/Codigo R")

data("Mafia")

Mafia<-Mafia[!(Mafia$year < 1988),]
Mafia<-subset(Mafia,select = -c(u1, u2 ) )


# Cargando el mapa 
map_Ita2 <- st_read("C:/Users/Nick/Documents/Universidad/20211/Econometria Avanzada/Final/Codigo R/Mapa/gadm36_ITA_2.shp") %>%
  rename(region=NAME_1, province=NAME_2)


#tm_shape(Ita_Map) +
#  tm_polygons(col = 'gray95', border.col = "gray90") +
#  tm_text("province", scale=.3)



# Pegando los datos al mapa, utilizando la llave FIPS
data90 <-Mafia[Mafia$year==1990,]
data91 <-Mafia[Mafia$year==1991,]
data92 <-Mafia[Mafia$year==1992,]
data93 <-Mafia[Mafia$year==1993,]
data94 <-Mafia[Mafia$year==1994,]
data95 <-Mafia[Mafia$year==1995,]
data96 <-Mafia[Mafia$year==1996,]
data97 <-Mafia[Mafia$year==1997,]
data98 <-Mafia[Mafia$year==1998,]
data99 <-Mafia[Mafia$year==1999,]

Ita_90_merge <- right_join(map_Ita2,data90)
Ita_91_merge <- right_join(map_Ita2,data91)
Ita_92_merge <- right_join(map_Ita2,data92)
Ita_93_merge <- right_join(map_Ita2,data93)
Ita_94_merge <- right_join(map_Ita2,data94)
Ita_95_merge <- right_join(map_Ita2,data95)
Ita_96_merge <- right_join(map_Ita2,data96)
Ita_97_merge <- right_join(map_Ita2,data97)
Ita_98_merge <- right_join(map_Ita2,data98)
Ita_99_merge <- right_join(map_Ita2,data99)

province<-as.data.frame(Ita_99_merge$province)

colnames(province)<-"province"

Ita_Map<-left_join(province, map_Ita2)
Ita_Map<-st_as_sf(Ita_Map)

Ita_merge <- right_join(map_Ita2,Mafia)
#View(Ita_merge)

# Mapiando rap
#tm_shape(Ita_99_merge) +
#  tm_polygons("y",palette="Reds", id="province") + 
#  tm_compass(type = "8star", position = c("right", "bottom")) +
#  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)


tmap_mode("view")
#tm_shape(Ita_99_merge) +
#  tm_polygons("y", title="Crecimiento PIB 1999", palette="Reds", id="province", n=4,style="equal") + 
#  tm_basemap(server="http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",alpha=0.5) +
#  tm_view(view.legend.position = c("right", "bottom"))

tmap_mode("plot")
#tmap_save(tm = mymap, filename = "mymap.png")

# Webs de mapas: https://geocompr.robinlovelace.net/adv-map.html
#                https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html

# Construyendo la matriz de pesos espaciales tipo Queen
nb <- poly2nb(Ita_Map,queen=T)
nb

We <- nb2listw(nb, style="W")
names(We)
We$weights

# Graficando la contiguidad
cnt <- st_centroid(Ita_Map, of_largest_polygon = T)
centroides <- cnt %>% st_coordinates()

line_nb <- nb2lines(nb, coords = centroides, proj4string = 4326, as_sf = T)

#tm_shape(Ita_Map)+tm_borders()+
#  tm_shape(cnt)+tm_dots(col = 'red', size = 0.5)+
#  tm_shape(line_nb) + tm_lines(col = 'navy')

# ESDA
# Contraste global de autocorrelación espacial
# I de Moran
moran.test_90<-moran.test(Ita_90_merge$y, We)
moran.test_91<-moran.test(Ita_91_merge$y, We)
moran.test_92<-moran.test(Ita_92_merge$y, We)
moran.test_93<-moran.test(Ita_93_merge$y, We)
moran.test_94<-moran.test(Ita_94_merge$y, We)
moran.test_95<-moran.test(Ita_95_merge$y, We)
moran.test_96<-moran.test(Ita_96_merge$y, We)
moran.test_97<-moran.test(Ita_97_merge$y, We)
moran.test_98<-moran.test(Ita_98_merge$y, We)
moran.test_99<-moran.test(Ita_99_merge$y, We)
MoranTest90<-c(moran.test_90$statistic,moran.test_90$p.value)
MoranTest91<-c(moran.test_91$statistic,moran.test_91$p.value)
MoranTest92<-c(moran.test_92$statistic,moran.test_92$p.value)
MoranTest93<-c(moran.test_93$statistic,moran.test_93$p.value)
MoranTest94<-c(moran.test_94$statistic,moran.test_94$p.value)
MoranTest95<-c(moran.test_95$statistic,moran.test_95$p.value)
MoranTest96<-c(moran.test_96$statistic,moran.test_96$p.value)
MoranTest97<-c(moran.test_97$statistic,moran.test_97$p.value)
MoranTest98<-c(moran.test_98$statistic,moran.test_98$p.value)
MoranTest99<-c(moran.test_99$statistic,moran.test_99$p.value)

mtest<-rbind(MoranTest90,MoranTest91,MoranTest92,MoranTest93,MoranTest94, MoranTest95,MoranTest96,MoranTest97,MoranTest98,MoranTest99)
kable(mtest, col.names = c("Moran's I", "P.Value"), caption = "Table .Moran Global Test", digits = 3)


# Scatterplot de Moran


##90
mp <- moran.plot(as.vector(scale(Ita_90_merge$y)), We,
                 labels=as.character(Ita_90_merge$province), pch=19)


Ita_90_merge$st_y <- scale(Ita_90_merge$y)
Ita_90_merge$lag_st_y <- lag.listw(We,Ita_90_merge$y)
scatt_imoran_90 <- ggplot(Ita_90_merge, aes(x=st_y, y=lag_st_y)) + 
  geom_point(shape=1, size=1) + 
  geom_smooth(formula=y ~ x, method="lm") + 
  geom_hline(yintercept=0, lty=2) + 
  geom_vline(xintercept=0, lty=2) + 
  theme_minimal() +
  geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=7, size=1) +
  geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1), size=2) +
  xlim(-2.5,4) + ylim(-4,3) +
  labs(title = "Moran Scatter Plot 1990")+
  xlab("Y") + 
  ylab("Spatial lag Y") + 
  theme(axis.title.y = element_text(size = rel(.6)),
        axis.title.x = element_text(size = rel(.6))) +
  annotate("text", x = 2, y = -3, label = "Moran's I =   6.798", size=2) +
  annotate("text", x=2, y=-3.5, label="P-value = 0.000", size=2)


##92
mp <- moran.plot(as.vector(scale(Ita_92_merge$y)), We,
                 labels=as.character(Ita_92_merge$province), pch=19)

Ita_92_merge$st_y <- scale(Ita_92_merge$y)
Ita_92_merge$lag_st_y <- lag.listw(We,Ita_92_merge$y)
scatt_imoran_92 <- ggplot(Ita_92_merge, aes(x=st_y, y=lag_st_y)) + 
  geom_point(shape=1, size=1) + 
  geom_smooth(formula=y ~ x, method="lm") + 
  geom_hline(yintercept=0, lty=2) + 
  geom_vline(xintercept=0, lty=2) + 
  theme_minimal() +
  geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=7, size=1) +
  geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1), size=2) +
  xlim(-2.5,4) + ylim(-4,3) +
  labs(title = "Moran Scatter Plot 1992")+
  xlab("Y") + 
  ylab("Spatial lag Y") + 
  theme(axis.title.y = element_text(size = rel(.6)),
        axis.title.x = element_text(size = rel(.6))) +
  annotate("text", x = 2, y = -3, label = "Moran's I =    4.034", size=2) +
  annotate("text", x=2, y=-3.5, label="P-value = 0.000", size=2)


##95
mp <- moran.plot(as.vector(scale(Ita_95_merge$y)), We,
                 labels=as.character(Ita_95_merge$province), pch=19)

Ita_95_merge$st_y <- scale(Ita_95_merge$y)
Ita_95_merge$lag_st_y <- lag.listw(We,Ita_95_merge$y)
scatt_imoran_95 <- ggplot(Ita_95_merge, aes(x=st_y, y=lag_st_y)) + 
  geom_point(shape=1, size=1) + 
  geom_smooth(formula=y ~ x, method="lm") + 
  geom_hline(yintercept=0, lty=2) + 
  geom_vline(xintercept=0, lty=2) + 
  theme_minimal() +
  geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=7, size=1) +
  geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1), size=2) +
  xlim(-2.5,4) + ylim(-4,3) +
  labs(title = "Moran Scatter Plot 1995")+
  xlab("Y") + 
  ylab("Spatial lag Y") + 
  theme(axis.title.y = element_text(size = rel(.6)),
        axis.title.x = element_text(size = rel(.6))) +
  annotate("text", x = 2, y = -3, label = "Moran's I =   6.888", size=2) +
  annotate("text", x=2, y=-3.5, label="P-value = 0.000", size=2)

##99
mp <- moran.plot(as.vector(scale(Ita_99_merge$y)), We,
                 labels=as.character(Ita_99_merge$province), pch=19)

Ita_99_merge$st_y <- scale(Ita_99_merge$y)
Ita_99_merge$lag_st_y <- lag.listw(We,Ita_99_merge$y)
scatt_imoran_99 <- ggplot(Ita_99_merge, aes(x=st_y, y=lag_st_y)) + 
  geom_point(shape=1, size=1) + 
  geom_smooth(formula=y ~ x, method="lm") + 
  geom_hline(yintercept=0, lty=2) + 
  geom_vline(xintercept=0, lty=2) + 
  theme_minimal() +
  geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=7, size=1) +
  geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1), size=2) +
  xlim(-2.5,4) + ylim(-4,3) +
  labs(title = "Moran Scatter Plot 1999")+
  xlab("Y") + 
  ylab("Spatial lag Y") + 
  theme(axis.title.y = element_text(size = rel(.6)),
        axis.title.x = element_text(size = rel(.6))) +
  annotate("text", x = 2, y = -3, label = "Moran's I =   1.228", size=2) +
  annotate("text", x=2, y=-3.5, label="P-value = 0.110", size=2)


#msp<-arrangeGrob(scatt_imoran_90, scatt_imoran_92, scatt_imoran_95, scatt_imoran_99, nrow = 2)

#ggsave("msp.jpg", msp, dpi=2000)

# Contrastes locales de autocorrelación espacial
# Local I de Moran
# A positive value for Ii indicates that the unit is surrounded by units with similar values

#90
lmoran_90 <- localmoran(Ita_90_merge$y, We)
# Plot local Moran
moran.map_90 <- cbind(Ita_90_merge, lmoran_90)


y_map_90 <- tm_shape(moran.map_90) +
  tm_polygons("y",
              border.col = "gray35",
              n = 5,
              style = "quantile",
              palette = "BuPu",
              title = "GDP Variation 90")
#y_map_90

summary(moran.map_90$Ii)
Ii_y_90 <- tm_shape(moran.map_90) +
  tm_polygons("Ii",
              border.col = "gray35",
              breaks = c(-2, 0, 2, 5),
              pal = c("white", "skyblue","blue"),
              labels = c("(-)", "(+)", "(++)"),
              title = "Local moran stat 90")
#Ii_y_90

y_map_90_ <- tmap_arrange(y_map_90,Ii_y_90)
y_map_90_
#tmap_save(tm = y_map_90_, 
 #            filename = "y_map_90.png",
#             height = 10)
#
# Plot LISA clusters
# builds a data quadrant
moran.map_90$quad_sig <- NA

# high-high quadrant
moran.map_90[(moran.map_90$st_y >= 0 & 
                moran.map_90$lag_st_y >= 0) & 
               (moran.map_90$"Pr.z...0." <= 0.05), "quad_sig"] <- "high-high"

#low-low quadrant
moran.map_90[(moran.map_90$st_y <= 0 & 
                moran.map_90$lag_st_y <= 0) & 
               (moran.map_90$"Pr.z...0." <= 0.05), "quad_sig"] <- "low-low"

# high-low quadrant
moran.map_90[(moran.map_90$st_y >= 0 & 
                moran.map_90$lag_st_y <= 0) & 
               (moran.map_90$"Pr.z...0." <= 0.05), "quad_sig"] <- "high-low"

# low-high quadrant
moran.map_90[(moran.map_90$st_y <= 0 & 
                moran.map_90$lag_st_y >= 0) & 
               (moran.map_90$"Pr.z...0." <= 0.05), "quad_sig"] <- "low-high"

# non-significant observations
moran.map_90[(moran.map_90$"Pr.z...0." > 0.05), "quad_sig"] <- "not signif."  


#cbind(table(moran.map_90$quad_sig))

Ii_sig_y_90 <- tm_shape(moran.map_90) +
  tm_polygons("quad_sig",
              border.col = "gray50",
              pal = c("red", "lightpink", "skyblue2", "white"),
              #pal = c("blue","white", "red"),
              labels = c("High-High", "High-Low", "Low-Low", "Not Signif."),
              #labels = c("Low-Low","Not Signif.", "High-High"),
              title = "LISA_90")
#Ii_sig_y_90

#y_map_90_ <- tmap_arrange(y_map_90,Ii_y_90,Ii_sig_y_90)
y_map_90_
#tmap_save(tm = y_map_90_, 
#          filename = "y_map_90.png",
#          height = 5)


#92
lmoran_92 <- localmoran(Ita_92_merge$y, We)
# Plot local Moran
moran.map_92 <- cbind(Ita_92_merge, lmoran_92)


y_map_92 <- tm_shape(moran.map_92) +
  tm_polygons("y",
              border.col = "gray35",
              n = 5,
              style = "quantile",
              palette = "BuPu",
              title = "GDP Variation 92")
#y_map_92

summary(moran.map_92$Ii)
Ii_y_92 <- tm_shape(moran.map_92) +
  tm_polygons("Ii",
              border.col = "gray35",
              breaks = c(-2, 0, 2, 5),
              pal = c("white", "skyblue","blue"),
              labels = c("(-)", "(+)", "(++)"),
              title = "Local moran stat 92")
#Ii_y_92

#y_map_92_ <- tmap_arrange(y_map_92,Ii_y_92)
#y_map_92_
#tmap_save(tm = y_map_92_, 
#             filename = "y_map_92.png",
#             height = 10)
 

# Plot LISA clusters
# builds a data quadrant
moran.map_92$quad_sig <- NA

# high-high quadrant
moran.map_92[(moran.map_92$st_y >= 0 & 
                moran.map_92$lag_st_y >= 0) & 
               (moran.map_92$"Pr.z...0." <= 0.05), "quad_sig"] <- "high-high"

#low-low quadrant
moran.map_92[(moran.map_92$st_y <= 0 & 
                moran.map_92$lag_st_y <= 0) & 
               (moran.map_92$"Pr.z...0." <= 0.05), "quad_sig"] <- "low-low"

# high-low quadrant
moran.map_92[(moran.map_92$st_y >= 0 & 
                moran.map_92$lag_st_y <= 0) & 
               (moran.map_92$"Pr.z...0." <= 0.05), "quad_sig"] <- "high-low"

# low-high quadrant
moran.map_92[(moran.map_92$st_y <= 0 & 
                moran.map_92$lag_st_y >= 0) & 
               (moran.map_92$"Pr.z...0." <= 0.05), "quad_sig"] <- "low-high"

# non-significant observations
moran.map_92[(moran.map_92$"Pr.z...0." > 0.05), "quad_sig"] <- "not signif."  


cbind(table(moran.map_92$quad_sig))

Ii_sig_y_92 <- tm_shape(moran.map_92) +
  tm_polygons("quad_sig",
              border.col = "gray50",
              pal = c("skyblue2", "white"),
              #pal = c("blue","white", "red"),
              labels = c( "Low-Low", "Not Signif."),
              #labels = c("Low-Low","Not Signif.", "High-High"),
              title = "LISA 92")
#Ii_sig_y_92

y_map_92_ <- tmap_arrange(y_map_92,Ii_y_92,Ii_sig_y_92)
y_map_92_

#95
lmoran_95 <- localmoran(Ita_95_merge$y, We)
# Plot local Moran
moran.map_95 <- cbind(Ita_95_merge, lmoran_95)


y_map_95 <- tm_shape(moran.map_95) +
  tm_polygons("y",
              border.col = "gray35",
              n = 5,
              style = "quantile",
              palette = "BuPu",
              title = "GDP Variation 95")
#y_map_95

summary(moran.map_95$Ii)
Ii_y_95 <- tm_shape(moran.map_95) +
  tm_polygons("Ii",
              border.col = "gray35",
              breaks = c(-2, 0, 2, 5),
              pal = c("white", "skyblue","blue"),
              labels = c("(-)", "(+)", "(++)"),
              title = "Local moran stat 95")
#Ii_y_95

#y_map_95_ <- tmap_arrange(y_map_95,Ii_y_95)
#y_map_95_
#tmap_save(tm = y_map_95_, 
#             filename = "y_map_95.png",
#             height = 10)


# Plot LISA clusters
# builds a data quadrant
moran.map_95$quad_sig <- NA

# high-high quadrant
moran.map_95[(moran.map_95$st_y >= 0 & 
                moran.map_95$lag_st_y >= 0) & 
               (moran.map_95$"Pr.z...0." <= 0.05), "quad_sig"] <- "high-high"

#low-low quadrant
moran.map_95[(moran.map_95$st_y <= 0 & 
                moran.map_95$lag_st_y <= 0) & 
               (moran.map_95$"Pr.z...0." <= 0.05), "quad_sig"] <- "low-low"

# high-low quadrant
moran.map_95[(moran.map_95$st_y >= 0 & 
                moran.map_95$lag_st_y <= 0) & 
               (moran.map_95$"Pr.z...0." <= 0.05), "quad_sig"] <- "high-low"

# low-high quadrant
moran.map_95[(moran.map_95$st_y <= 0 & 
                moran.map_95$lag_st_y >= 0) & 
               (moran.map_95$"Pr.z...0." <= 0.05), "quad_sig"] <- "low-high"

# non-significant observations
moran.map_95[(moran.map_95$"Pr.z...0." > 0.05), "quad_sig"] <- "not signif."  


#cbind(table(moran.map_95$quad_sig))

Ii_sig_y_95 <- tm_shape(moran.map_95) +
  tm_polygons("quad_sig",
              border.col = "gray50",
              pal = c("red", "lightpink", "skyblue2", "white"),
              #pal = c("blue","white", "red"),
              labels = c("High-High", "High-Low", "Low-Low", "Not Signif."),
              #labels = c("Low-Low","Not Signif.", "High-High"),
              title = "LISA 95")
#Ii_sig_y_95

y_map_95_ <- tmap_arrange(y_map_95,Ii_y_95,Ii_sig_y_95)
y_map_95_

#99
lmoran_99 <- localmoran(Ita_99_merge$y, We)
# Plot local Moran
moran.map_99 <- cbind(Ita_99_merge, lmoran_99)


y_map_99 <- tm_shape(moran.map_99) +
  tm_polygons("y",
              border.col = "gray35",
              n = 5,
              style = "quantile",
              palette = "BuPu",
              title = "GDP Variation 99")
#y_map_99

summary(moran.map_99$Ii)
Ii_y_99 <- tm_shape(moran.map_99) +
  tm_polygons("Ii",
              border.col = "gray35",
              breaks = c(-2, 0, 2, 5),
              pal = c("white", "skyblue","blue"),
              labels = c("(-)", "(+)", "(++)"),
              title = "Local moran stat 99")
#Ii_y_99

#y_map_99_ <- tmap_arrange(y_map_99,Ii_y_99)
#y_map_99_
#tmap_save(tm = y_map_99_, 
#             filename = "y_map_99.png",
#             height = 10)


# Plot LISA clusters
# builds a data quadrant
moran.map_99$quad_sig <- NA

# high-high quadrant
moran.map_99[(moran.map_99$st_y >= 0 & 
                moran.map_99$lag_st_y >= 0) & 
               (moran.map_99$"Pr.z...0." <= 0.05), "quad_sig"] <- "high-high"

#low-low quadrant
moran.map_99[(moran.map_99$st_y <= 0 & 
                moran.map_99$lag_st_y <= 0) & 
               (moran.map_99$"Pr.z...0." <= 0.05), "quad_sig"] <- "low-low"

# high-low quadrant
moran.map_99[(moran.map_99$st_y >= 0 & 
                moran.map_99$lag_st_y <= 0) & 
               (moran.map_99$"Pr.z...0." <= 0.05), "quad_sig"] <- "high-low"

# low-high quadrant
moran.map_99[(moran.map_99$st_y <= 0 & 
                moran.map_99$lag_st_y >= 0) & 
               (moran.map_99$"Pr.z...0." <= 0.05), "quad_sig"] <- "low-high"

# non-significant observations
moran.map_99[(moran.map_99$"Pr.z...0." > 0.05), "quad_sig"] <- "not signif."  


#cbind(table(moran.map_99$quad_sig))

Ii_sig_y_99 <- tm_shape(moran.map_99) +
  tm_polygons("quad_sig",
              border.col = "gray50",
              pal = c("red", "lightpink", "skyblue2", "white"),
              #pal = c("blue","white", "red"),
              labels = c("High-High", "High-Low", "Low-Low", "Not Signif."),
              #labels = c("Low-Low","Not Signif.", "High-High"),
              title = "LISA 99")
#Ii_sig_y_99

y_map_99_ <- tmap_arrange(y_map_99,Ii_y_99,Ii_sig_y_99)
y_map_99_

Moran_total <- tmap_arrange(y_map_90,Ii_y_90,Ii_sig_y_90,y_map_92,Ii_y_92,Ii_sig_y_92,y_map_95,Ii_y_95,Ii_sig_y_95,y_map_99,Ii_y_99,Ii_sig_y_99)
tmap_save(tm = Moran_total , 
          filename = "Moran.png",
          height = 30)


##Modelación


# Modelos de regresión
data("Mafia")
Mafia$l.cds2<-lag(Mafia$cds2, 1)
Mafia<-Mafia[!(Mafia$year < 1988),]
Mafia<-subset(Mafia,select = -c(u1, u2 ) )

# Ya que se va a trabajar con estructura panel, es más conveniente utilizar la función pdata.frame
Mafia$l.g<-lag(Mafia$g, 1)
Mafia$l.l.g<-lag(Mafia$g, 2)

datapanel <- pdata.frame(Mafia, c("province","year"))

datapanel$g.est<-predict(plm(g ~cds1+ l.cds2+ mafiosi + extortion + corruption1 + corruption2 + murder,  data = datapanel, model= "pooling"))
#summary(datapanel)
summary(plm(g ~cds1+ l.cds2+ mafiosi + extortion + corruption1 + corruption2 + murder,  data = datapanel, model= "pooling"))
datapanel<-as.data.frame(datapanel)

Mafia$g.est<-datapanel$g.est
Mafia$l.g.est<-lag(Mafia$g, 1)
Mafia$l.l.g.est<-lag(Mafia$g, 2)
Mafia$d.g<-diff(Mafia$g.est, lag=1, differences = 1)
Mafia$l.d.g<-lag(Mafia$d.g, 1)
Mafia$l.l.d.g<-lag(Mafia$d.g, 2)
Mafia<-Mafia[!(Mafia$year < 1990),]
Mafiad<-Mafia[!(Mafia$year < 1991),]

datapanel <- pdata.frame(Mafia, c("province","year"))
datapaneld<- pdata.frame(Mafiad, c("province","year"))

stargazer(datapanel, type = "text")
stargazer(datapanel[, c("y","g")], type = "text", 
          covariate.labels = c("Variantion GDP", "Change in Public Spending"))
#View(datapanel)

# Estimación panel sin efectos espaciales
# Pool
pooling2 <- plm(y ~  mafiosi + extortion + corruption1 + corruption2 + murder,
               data = datapanel, model= "pooling")
  summary(pooling)


# Efectos aleatorios
re2 <- plm(y ~   mafiosi + extortion + corruption1 + corruption2 + murder, effect = "individual",
          data = datapanel, model = "random")
summary(re)
#valores estimados de variable dependiente
#y <-predict(pooling)

# Efectos fijos
fe2 <- plm(y ~ mafiosi + extortion + corruption1 + corruption2 + murder, effect = "individual",
          data = datapanel, model = "within")
summary(fe)
summary(fixef(fe))

# Efectos aleatorios
fd2 <- plm(y ~  mafiosi + extortion + corruption1 + corruption2 + murder, 
          data = datapanel, model = "fd")
summary(fd)


stargazer(pooling2, re2, fe2, fd2, title="Results", type = "text", 
          column.labels = c("Pool", "re", "fe", "fd"), align=TRUE)

pooling <- plm(y ~ g +l.g +l.l.g | cds1+l.cds2 + year+ mafiosi + extortion + corruption1 + corruption2 + murder,
               data = datapanel, model= "pooling")
summary(pooling)

# Efectos aleatorios
re <- plm(y ~ g +l.g +l.l.g | cds1+l.cds2 + mafiosi + extortion + corruption1 + corruption2 + murder, effect = "individual",
           data = datapanel, model = "random")
summary(re)
#valores estimados de variable dependiente
#y <-predict(pooling)

# Efectos fijos
fe <- plm(y ~ g +l.g +l.l.g |cds1+l.cds2 + mafiosi + extortion + corruption1 + corruption2 + murder, effect = "individual",
           data = datapanel, model = "within")
summary(fe)
#summary(fixef(fe))

# Efectos aleatorios
fd <- plm(y ~ g +l.g +l.l.g |cds1+l.cds2 + mafiosi + extortion + corruption1 + corruption2 + murder, 
           data = datapanel, model = "fd")
summary(fd)

stargazer(pooling, re, fe, fd, title="Results", type = "text", 
          column.labels = c("Pool", "re", "fe", "fd"), align=TRUE)


# Hausman test (plm)
phtest(fe,re)

phtest(fd, fe)





mopwfdtest(fd, h0=c("fd"))


datapanel <- pdata.frame(Mafia, c("province","year"))



#calcilo de 

sarfe <- spml(y ~  g.est +l.g.est+l.l.g.est,
              data = datapanel, listw = We, spatial.error="none", lag=TRUE, 
              model = "within", effect = "individual", method = "eigen")
summary(sarfe)
print(effects(sarfe))

# Modelo SEM
semfe <- spml(y ~ g.est +l.g.est+l.l.g.est,
              data = datapanel, listw = We, lag = FALSE, 
              spatial.error = "b", model = "within",
              effect = "individual", method = "eigen")
summary(semfe)
print(effects(semfe))

# Modelo SAC-SARAR-SARMA
sararfe <- spml(y ~ g.est +l.g.est+l.l.g.est,
                data = datapanel, listw = We, lag = TRUE, 
                spatial.error = "b", model = "within",
                effect = "individual", method = "eigen")
summary(sararfe)
print(effects(sararfe))

# Modelo Spatial Durbin model
datapanel$wg<-slag(datapanel$g.est, We, 1)
datapanel$wl.g<-slag(datapanel$l.g.est, We, 1)
datapanel$wl.l.g<-slag(datapanel$l.l.g.est, We, 1)
sdmfe <- spml(y ~ g.est +l.g.est+l.l.g.est+ 
                wg + wl.g + wl.l.g,
              data = datapanel, listw = We, lag = TRUE, 
              spatial.error = "none", model = "within",
              effect = "individual", method = "eigen")
summary(sdmfe)

print(effects(sdmfe))

# Modelo Spatial Durbin error model
sdemfe <- spml(y ~ g.est +l.g.est+l.l.g.est+ 
                   wg + wl.g + wl.l.g,
               data = datapanel, listw = We, lag = FALSE, 
               spatial.error = "b", model = "within",
               effect = "individual", method = "eigen")
summary(sdemfe)
print(effects(sdemfe))

# Modelo SLX
slxfe <- plm(y ~ g.est +l.g.est+l.l.g.est+ 
                wg + wl.g + wl.l.g,
             data = datapanel, model = "within",
             effect = "individual")
summary(slxfe)
summary(fixef(slxfe))


filen <- tempfile(pattern = "table_aov")
xtable(x=smry, file=filen)



# Efectos aleatorios
# Modelo SAR
sarre <- spml(y ~ g.est +l.g.est+l.l.g.est,
              data = datapanel, listw = We, spatial.error="none", lag=TRUE, 
              model = "random")
summary(sarre)

# Modelo SEM
semre <- spml(y ~ g.est +l.g.est+l.l.g.est,
              data = datapanel, listw = We, lag = FALSE, 
              spatial.error = "b", model = "random")
summary(semre)

# Modelo SAC-SARAR-SARMA
sararre <- spml(y ~ g.est +l.g.est+l.l.g.est,
                data = datapanel, listw = We, lag = TRUE, 
                spatial.error = "b", model = "random")
summary(sararre)

# Modelo Spatial Durbin model
sdmre <- spml(y ~ g.est +l.g.est+l.l.g.est+ 
                wg + wl.g + wl.l.g,
              data = datapanel, listw = We, lag = TRUE, 
              spatial.error = "none", model = "random")
summary(sdmre)

# Modelo Spatial Durbin error model
sdemre <- spml(y ~ g.est +l.g.est+l.l.g.est+ 
                 wg + wl.g + wl.l.g,
               data = datapanel, listw = We, lag = FALSE, 
               spatial.error = "b", model = "random")
summary(sdemre)

# Modelo SLX
slxre <- plm(y ~ g.est +l.g.est+l.l.g.est+ 
               wg + wl.g + wl.l.g,
             data = datapanel, model = "random")
summary(slxre)

#First difference spatial
# Modelo SAR
sarfd <- spml(y ~ d.g +l.d.g+l.l.d.g,
              data = datapaneld, listw = We, spatial.error="none", lag=TRUE, 
              model = "pooling")
summary(sarfd)

# Modelo SEM
semfd <- spml(y ~ d.g +l.d.g+l.l.d.g,
              data = datapanel, listw = We, lag = FALSE, 
              spatial.error = "b", model = "pooling")
summary(semfd)

# Modelo SAC-SARAR-SARMA
sararfd <- spml(y ~ d.g +l.d.g+l.l.d.g,
                data = datapanel, listw = We, lag = TRUE, 
                spatial.error = "b", model = "pooling")
summary(sararfd)

datapanel$w.d.g<-slag(datapanel$g.est, We, 1)
datapanel$wl.d.g<-slag(datapanel$l.g.est, We, 1)
datapanel$wl.l.d.g<-slag(datapanel$l.l.g.est, We, 1)

# Modelo Spatial Durbin model
sdmfd <- spml(y ~ g.est +l.g.est+l.l.g.est+ 
                w.d.g + wl.d.g + wl.l.d.g,
              data = datapanel, listw = We, lag = TRUE, 
              spatial.error = "none", model = "pooling")
summary(sdmfd)

# Modelo Spatial Durbin error model
sdemfd <- spml(y ~ g.est +l.g.est+l.l.g.est+ 
                 w.d.g + wl.d.g + wl.l.d.g,
               data = datapanel, listw = We, lag = FALSE, 
               spatial.error = "b", model = "pooling")
summary(sdemfd)

# Modelo SLX
slxfd <- plm(y ~ g.est +l.g.est+l.l.g.est+ 
               w.d.g + wl.d.g + wl.l.d.g,
             data = datapanel, model = "pooling")
summary(slxfd)

coefficients(slxfd, level=0.95) # CIs for model parameters
slxfd$p.value


summary(pooling)
summary(fe)
summary(re)

# Hausman test (plm)
stargazer_htest(phtest(fe, re))

# Hausman test robust to spatial autocorrelation (splm)
# SAR
sphtest(x = sarre, x2 = sarfe)

# SEM
sphtest(x = semre, x2 = semfe)

# Los resultados del test de Hausman estándar y el test de Hausman robusto a la
# autocorrelación de los errores y lag lleva a rechazar la hipótesis nula de
# ausencia de correlación entre los efectos individuales y las variables explicatorias
# Para el resto del análisis empírico, el modelo de efectos fijos es escogido

# Tests LM
slmtest(y ~ g.est +l.g.est+l.l.g.est, data=datapanel, listw = We, test="lml",
        model="within")

slmtest(y ~ g.est +l.g.est+l.l.g.est, data=datapanel, listw = We, test="lme",
        model="within")

slmtest(y ~ g.est +l.g.est+l.l.g.est, data=datapanel, listw = We, test="rlml",
        model="within")

slmtest(y ~ g.est +l.g.est+l.l.g.est, data=datapanel, listw = We, test="rlme",
        model="within")

# Los test LM en un modelo de efectos fijos favorecen una espeficación SAR
# Los test1 (SAR) y test2 (SEM) confirman el rechazo de la hipótesis que
# estos dos términos (tomados independientemente) son nulos. Sin embargo,
# se puede notar que el estadístico para el SAR es mayor que para el SEM.
# Para poder concluir de una forma más creíble, los test robustos son usados
# en la presencia de la especificación de la autocorrelación espacial.
# Se observa que RLMlag y RLMerr son altamente significantes por lo que no es
# posible 

