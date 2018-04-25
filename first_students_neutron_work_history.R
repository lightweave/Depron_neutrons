library('readr')
library('dplyr')
library('tidyr')
library('hexbin')
library('maps')
library('modeest')

library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(colorspace)

library(ggplot2)


# your path need to be here:
setwd()
setwd('D:/Ivan/_flash backup 2014/SINP/DEPRON/lomonosov/oneday/student/Depron_neutrons')



datan <- read_tsv('y16_d  1_neu.tsv')
View(datan)
summary(datan)

plot(datan,dt,n1)
datan <- read_tsv('y16_d  1_neu.tsv')

datan <- read_tsv('y16_d214_neu.tsv')


plot(data=datan,dt,n1)

plot(data=datan,datan$dt,datan$n1)
plot(data=datan,datan$dt,datan$n1,type='h')

map(database="world")
# points(0,0)
points(datan$longeo.center,datan$n1)


map(database="world")
points(datan$longeo.center,datan$latgeo)
points(datan$longeo.center,datan$latgeo,cex = datn$n1)
points(datan$longeo.center,datan$latgeo,cex = datan$n1)


map(database="world")
points(datan$longeo.center,datan$latgeo,cex = datan$n1)
plot(datan$l,datan$b)
plot(datan$l,datan$b,x->c(0,100))
plot(datan$l,datan$b,c(0,100))
plot(data = datan[datan$l<20,],
     datan$l,
     datan$b)

datan[datan$l<20,]


plot(datafilter,datafilter$l,datafilter$b)

points(datan$longeo.center,datan$latgeo,cex = datan$n1)

datafilter = datan[datan$l<20,]

plot(datafilter,datafilter$l,datafilter$b)
plot(datafilter$l,datafilter$b)
plot(datafilter$l,datafilter$b,cex=datafilter$n1)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,bg=datafilter$n1+1)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,bg=datafilter$n1+1)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,bg=datafilter$n1)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,bg=datafilter$n1+50)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,color=datafilter$n1+50)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=datafilter$n1+50)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=datafilter$n1)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=datafilter$n1+50)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=datafilter$n1)


n=20
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1])
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1])


n=5
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
pie(rep(1,n), col=colr)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1])



plot(datafilter$l,datafilter$b,col=colr[datafilter$n1])   # doesnt work!
plot(datafilter$l,datafilter$b,col=colr[datafilter$n1+1]) # works fine!


plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1+1])
points(datafilter$l,datafilter$b,cex=datafilter$n2,col=colr[datafilter$n2+1])
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1+1])
points(datafilter$l,datafilter$b,cex=datafilter$n2,bg=colr[datafilter$n2+1])
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1+1])
points(datafilter$l,datafilter$b,cex=datafilter$n2,bg=colr[datafilter$n2+1],pch=0)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1+1])
points(datafilter$l,datafilter$b,cex=datafilter$n2,pch=0,bg=colr[datafilter$n2+1])
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1+1])
points(datafilter$l,datafilter$b,cex=datafilter$n2,pch=0,col=colr[datafilter$n2+1])
datafilter = datan[datan$l<10,]

n=5
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
pie(rep(1,n), col=colr)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1+1])
points(datafilter$l,datafilter$b,cex=datafilter$n2,pch=0,col=colr[datafilter$n2+1])

n=5
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
pie(rep(1,n), col=colr)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1+1])
points(datafilter$l,datafilter$b,cex=datafilter$n2,pch=0,col=colr[datafilter$n2+1])
datafilter = datan[(datan$l<10)&(datan$b>0.23),]

n=5
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
pie(rep(1,n), col=colr)
plot(datafilter$l,datafilter$b,cex=datafilter$n1,col=colr[datafilter$n1+1])
points(datafilter$l,datafilter$b,cex=datafilter$n2,pch=0,col=colr[datafilter$n2+1])
points(datanfilter$longeo.center,datanfilter$latgeo,cex = datan$n1)
points(datafilter$longeo.center,datafilter$latgeo,cex = datan$n1)
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n1)

map(database="world")
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n1)
datafilter = datan[(datan$l<10)&(datan$b>0.23),]
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n1)
points(datafilter$longeo.center,datafilter$latgeo)

map(database="world")
points(datafilter$longeo.center,datafilter$latgeo)
datafilter = datan[(datan$l<10)&(datan$b>0.22),]
points(datafilter$longeo.center,datafilter$latgeo)
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n1)

map(database="world")
datafilter = datan[(datan$l<10)&(datan$b>0.22),]
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n1)
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n1,col=colr[datafilter$n2+1])
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n1,col=colr[datafilter$n1+1])

summary(datafilter)

n=3
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
pie(rep(1,n), col=colr)
map(database="world")
datafilter = datan[(datan$l<10)&(datan$b>0.22),]
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n1,col=colr[datafilter$n1+1])

n=5
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n2,col=colr[datafilter$n2+1])

n=4
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
pie(rep(1,n), col=colr)
map(database="world")
datafilter = datan[(datan$l<10)&(datan$b>0.22),]
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n2,col=colr[datafilter$n2+1])



n=5
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
pie(rep(1,n), col=colr)
map(database="world")
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n2,col=colr[datafilter$n2+1])


n=4
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
map(database="world")
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n2,col=colr[datafilter$n2+1])


11%/%5

datan$minut<-(as.integer(datan$dt)%/%300)
datan %>% group_by(minut) %>% summarise(sum1=sum(n1))
datan.minutgr<-(datan %>% group_by(minut) %>% summarise(sum1=sum(na.exclude(n1))))
plot(data=datan.minutgr,datan.minutgr$minut,datan.minutgr$sum1,type='h')

datan.minutgr<-(datan %>% group_by(minut) %>% mutate(sum1=sum(n1)))

plot(data=datan,datan$dt,SMOOTH_Triangle(datan$n1,60),type='h')


map(database="world")
points(datan$longeo.center,datan$latgeo,cex = SMOOTH_Triangle(datan$n1,60))
map(database="world")
points(datan$longeo.center,datan$latgeo,cex = datan$n1)





#poisson test-------------------------------------------------------------------
datafilter = datan[(datan$l<2)&(datan$b>0.23),]
summary(datafilter)
datafilter <- na.omit(datafilter)

n=4
colr = rev(rainbow(n, start = 0, end = 4/6, alpha = 1))
map(database="world")
points(datafilter$longeo.center,datafilter$latgeo,cex = datafilter$n2,col=colr[datafilter$n2+1])


lambda<-mean(na.omit(datafilter$n1))
x<- max(na.omit(datafilter$n1))
dpois(2, lambda, log = FALSE)*nrow(datafilter)
# [1] 0.6103154 
nrow(datafilter[datafilter$n1==x,])
# [1] 2

# 0.007522084 2

lambda<-mean(na.omit(datafilter$n2))
x<- max(na.omit(datafilter$n2))
dpois(x, lambda, log = FALSE)*nrow(datafilter)
# [1] 24.1618
nrow(datafilter[datafilter$n2==x,])
# [1] 46
# 0.3843149 1
