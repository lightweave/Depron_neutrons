# задаем пути к библиотекам----
.libPaths("D:/RStudio/libraries")
.libPaths()

#подключаем библиотеки----
library(rgdal)         # for readOGR(...)
library(ggplot2)
library(RColorBrewer)  # for brewer.pal(...)
library('readr')
library('maps')
library(ggmap)

# задаем рабочую директорию----
setwd('D:/RStudio')

#считываем данные----
datan <- read_tsv('y16_d1_neu.tsv')
summary(datan)
#----

datafilter = datan[(datan$l < 2.5) &
                     (datan$b > 0.22) &
                     (datan$n2 != 0),]

sample <-
  data.frame(
    Longitude = datafilter$longeo.center,
    Latitude = datafilter$latgeo,
    Counts = datafilter$n2
  )

world <- map_data("world")
map.df <- fortify(world)

ggplot(sample, aes(x = Longitude, y = Latitude)) +
  stat_density2d(aes(fill = ..level..), alpha = 0.5, geom = "polygon") +
  #  geom_point(colour = "red") +
  geom_path(data = map.df,
            aes(x = long, y = lat, group = group),
            colour = "grey50") +
  scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral"))) +
  xlim(-200, +200) +
  coord_fixed()