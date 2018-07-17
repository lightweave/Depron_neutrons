# задаем пути к библиотекам----
.libPaths("D:/RStudio/libraries")
.libPaths()

#подключаем библиотеки----
library('readr')
library('dplyr')
library('tidyr')
library('maps')
library(lattice)
library(RColorBrewer)
library(colorspace)
library(ggplot2)

# задаем рабочую директорию----
setwd('D:/RStudio')

#считываем данные----
datan <- read_tsv('y16_d1_neu.tsv')
summary(datan)
#----

df_hm = datan[(datan$l < 2.5) &
                (datan$b > 0.22), ]

df_hm$latgeo <- (as.integer(df_hm$latgeo) %/% 1)
df_hm$longeo <- abs(as.integer(df_hm$longeo) %/% 360)


df_hm %>% group_by(latgeo, longeo) %>% summarise(n1 = sum(n1),
                                                 n2 = sum(n2))
df_hm.group <-
  (df_hm %>% group_by(latgeo, longeo) %>% summarise(n1 = sum(na.exclude(n1)),
                                                    n2 = sum(na.exclude(n2))))

ggplot(data = df_hm.group, aes(x = longeo, y = latgeo)) +
  geom_tile(aes(fill = n2))