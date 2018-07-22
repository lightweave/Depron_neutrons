# Задаем пути к библиотекам----
.libPaths("D:/RStudio/libraries")
.libPaths()

# Подключаем библиотеки----
library('readr')
library('dplyr')
library('tidyr')
library('maps')
library(lattice)
library(RColorBrewer)
library(colorspace)
library(ggplot2)
library(plyr)

# Задаем рабочую директорию----
setwd('D:/Rstudio/combinedn')

# Считываем данные----
datan <- read_tsv('y16_d1_neu.tsv')
summary(datan)

#----

tables <- sprintf("  (%001d).tsv", 1:202)

# Обработка первого дня----
n = 1
datan <- read_tsv(tables[n])
datan = na.exclude(datan)
df_hm = datan[(datan$b > 0.23),]

df_hm$latgeo <-    (as.integer(df_hm$latgeo) %/% 1)
df_hm$longeo <- abs(as.integer(df_hm$longeo) %/% 360)

df_hm %>% group_by(latgeo, longeo) %>% summarise(n1 = sum(n1),
                                                 n2 = sum(n2))
df_hm.group <-
  (df_hm %>% group_by(latgeo, longeo) %>% summarise(n1 = sum(na.exclude(n1)),
                                                    n2 = sum(na.exclude(n2))))

lat_count <- count(df_hm, latgeo)

t = 1
while (t < nrow(df_hm.group)) {
  df_hm.group$n2[t] <- df_hm.group$n2[t] / lat_count$n[t]
  df_hm.group$n1[t] <- df_hm.group$n1[t] / lat_count$n[t]
  t = t + 1
}

# Обработка остальных дней----
n = n + 1
while (n < 203) {
  
  datan <- read_tsv(tables[n])
  datan = na.exclude(datan)
  df_hm = datan[(datan$b > 0.23),]
  
  df_hm$latgeo <-    (as.integer(df_hm$latgeo) %/% 1)
  df_hm$longeo <- abs(as.integer(df_hm$longeo) %/% 360)
  
  df_hm %>% group_by(latgeo, longeo) %>% summarise(n1 = sum(as.numeric(n1)),
                                                   n2 = sum(as.numeric(n2)))
  df_hm.group1 <-
    (df_hm %>% group_by(latgeo, longeo) %>% summarise(n1 = sum(as.numeric(n1)),
                                                      n2 = sum(as.numeric(n2))))
  
  lat_count <- count(df_hm, latgeo)
  
  t = 1
  while (t < nrow(df_hm.group1)) {
    df_hm.group1$n2[t] <- df_hm.group1$n2[t] / lat_count$n[t]
    df_hm.group1$n1[t] <- df_hm.group1$n1[t] / lat_count$n[t]
    t = t + 1
  }
  
  df_hm.group = union_all(df_hm.group, df_hm.group1)
  
  n = n + 1
}

df_hm.group %>% group_by(latgeo, longeo) %>% summarise(n1 = sum(as.numeric(n1)),
                                                       n2 = sum(as.numeric(n2)))
df_hm.group2 <-
  (df_hm.group %>% group_by(latgeo, longeo) %>% summarise(n1 = sum(as.numeric(n1)),
                                                          n2 = sum(as.numeric(n2))))

lat_count <- count(df_hm.group, latgeo)

t = 1
while (t < nrow(df_hm.group2)) {
  df_hm.group2$n2[t] <- df_hm.group2$n2[t] / lat_count$n[t]
  df_hm.group2$n1[t] <- df_hm.group2$n1[t] / lat_count$n[t]
  t = t + 1
}

# Финальное построение----
df_hm.final = df_hm.group2[(df_hm.group2$n1 < 2) &
                           (df_hm.group2$n2 < 2) &
                       (df_hm.group2$longeo < 1),]

ggplot(data = df_hm.final, aes(x = longeo, y = latgeo)) +
  geom_tile(aes(fill = n2))

# Гистограмма широтного хода----
plot(
  data = df_hm.final,
  df_hm.final$latgeo,
  df_hm.final$n2,
  type = 'h',
  main = 'Гистограмма широтного хода нейтронов на 2 детекторе \n(данные за все дни)',
  xlab = 'Широта',
  ylab = 'Усредненный счет'
)