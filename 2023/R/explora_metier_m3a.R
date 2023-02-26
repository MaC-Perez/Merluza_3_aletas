## estandarizacion ARRASTRE PDA - Merluza del Sur

rm(list=ls(all=TRUE))


library(dplyr)
library(lattice)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(lubridate)
library(psych)
library(tidyverse)

datos <- read.csv('2023/PDA_ARRASTRE_2021.csv', header=T, sep=',')
#datos <- select(datos, -X)
names(datos)
datos$zona    <- as.numeric(cut(datos$LATITUD, breaks=c(460000, 480000, 500000, 520000, 540000, 560000, 580000, 660000), labels=c('1','2','3','4','5','6','7')))
datos$lat     <- cut(datos$LATITUD/10000, quantile(datos$LATITUD/10000, (0:8)/8, na.rm=T))
datos$catch   <- datos$Merluza_3_a/1000
datos$cpue    <- datos$catch/datos$h_a
datos$prop    <- datos$Merluza_3_a/datos$CAPTURA_TOTAL
datos$logcpue <- log(datos$cpue)

datos<- datos%>% rename(year="anoLance", mes="mesLance", HA="h_a")

x11()
par(mfcol=c(2,3))
nbb = 30
hist( datos$cpue, nbb, main='CPUE')
hist( log(datos$cpue), nbb, main='log CPUE')
hist( datos$catch, nbb, main='Catch')
hist( log(datos$catch), nbb, main='log Catch')
hist( datos$HA, nbb, main='Effort')
hist( log(datos$HA), nbb, main='log Effort')


dim(datos)
# 131784     87
# 138697     87 
## --------------- sorting data ----------------------------
#data     <- datos %>% filter( zona=='ZN' | zona=='ZS')  %>%  filter(catch>0 & catch<30) %>% 
 #                    filter( cpue>0 & cpue<10 & !is.infinite(cpue) & !is.na(cpue)) %>% 
  #                   filter( !is.na(lat) )  %>% filter( HA>0 & HA<15 ) %>%   filter(year > 1997)#%>% filter (PROFUNDIDAD_MINIMA_AR>0 & PROFUNDIDAD_MAXIMA_AR<600) # 

#131784     87

dim(data)
data <- datos %>% filter(cpue<500 & !is.infinite(cpue) & !is.na(cpue)) %>% 
  # 27279    87
  # 28340    87
  #filter(zona>=1, zona<7) %>%
  # 24452    
  # 25607
  filter(catch > 2) %>% 
  # 12298    87
  # 12603
  filter(HA>0.15 & HA<3.5) # %>%
  # 7944   87
  # 8066 
 
  # filter(year>=1989) %>%
  # filter(mes>=6, mes<12) %>%
  # 6053   87
  # filter(COD_BARCO==400017| COD_BARCO==400018 | COD_BARCO==940034)
  # 4785   87

dim(data)
# 8192

dat.ope <- data %>% select(c(1:32,67:87))
names(dat.ope)
dat.esp <- data %>% select(c(33:66))
dat.esp <- dat.esp %>% mutate(total = rowSums(.,na.rm=TRUE))
names(dat.esp)

x11()
par(mfcol=c(2,3))
nbb = 30
hist( (dat.ope$cpue), nbb, main='CPUE')
hist( log(dat.ope$cpue), nbb, main='log CPUE')
hist( (dat.ope$catch), nbb, main='Catch')
hist( log(dat.ope$catch), nbb, main='log Catch')
hist( (dat.ope$HA), nbb, main='Effort')
hist( log(dat.ope$HA), nbb, main='log Effort')


#write.csv(data, file = "data_m3a_arrastre_2022.csv", row.names = T)

## --------------- cpue x fishery ----------------------------
#dat.ope$COD_BARCO[which(dat.ope$COD_BARCO == "400018")] = "UNION SUR 1"
#dat.ope$COD_BARCO[which(dat.ope$COD_BARCO == "940034")] = "UNION SUR 2"
#dat.ope$COD_BARCO[which(dat.ope$COD_BARCO == "400017")] = "UNZEN MARU"

a <- ggplot( dat.ope, aes( x=catch, y = (after_stat(count))/sum(after_stat(count))) )
a <- a + geom_histogram(aes(), bins=25, fill='white', color="black" ) 
a <- a + facet_grid(COD_BARCO ~ year, scale='free_y') 
a <- a + scale_y_continuous(labels = scales::percent) + ylab('Freuqnecy')
a

a <- ggplot(dat.ope, aes( x=logcpue) )
a <- a + geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count))), bins=50, fill='white', color="black"  ) 
a <- a + facet_grid(COD_BARCO ~ zona, scale='free_y') 
a <- a + scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = c(-8,-4,0,4,8)) 
a

a <- ggplot( data, aes( x=HA) )
a <- a + geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count))), bins=50, fill='white', color="black"  ) 
a <- a + facet_grid(year~zona, scale='free_y') 
a <- a + scale_y_continuous(labels = scales::percent)
a

x11()
a <- ggplot( data, aes( x=catch, y = (after_stat(count))/sum(after_stat(count))) )
a <- a + geom_histogram(aes(), bins=25, fill='white', color="black" ) 
a <- a + facet_wrap(year ~ zona, scale='free_y') 
a

a <- ggplot( data, aes( x=logcpue) )
a <- a + geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count))), bins=50, fill='white', color="black"  ) 
a <- a + facet_wrap(year ~ zona, scale='free_y') 
a

a <- ggplot( data, aes( x=HA) )
a <- a + geom_histogram(aes(y = (after_stat(count))/sum(after_stat(count))), bins=50, fill='white', color="black"  ) 
a <- a + facet_wrap(year~zona, scale='free_y') 
a

## --------------- cpue x estrato latitud ----------------------------

######
#DATOS LISTOS
########

library(tidyverse)
library(reshape2)
data <- read.csv('data_m3a_arrastre_2022.csv', header=T, sep=',')
data <- select(data, -X)
names(data)


dat.ope <- data %>% select(c(1:32,67:87))
names(dat.ope)
dat.esp <- data %>% select(c(33:66))
dat.esp <- dat.esp %>% mutate(total = rowSums(.,na.rm=TRUE))
names(dat.esp)

#library(reshape2)
#dat.ope$COD_BARCO[which(dat.ope$COD_BARCO == "400018")] = "UNION SUR 1"
#dat.ope$COD_BARCO[which(dat.ope$COD_BARCO == "940034")] = "UNION SUR 2"
#dat.ope$COD_BARCO[which(dat.ope$COD_BARCO == "400017")] = "UNZEN MARU"


catch.zona    <- melt(dcast(dat.ope, year + COD_BARCO ~ zona, median, value.var=c('cpue')), id=c(1,2))

##toneladas por hora de arrastre

library(ggplot2)
x11()

a <- ggplot( catch.zona, aes( x=year, y=value, color=as.factor(COD_BARCO) ) )
a <- a + geom_point(size=2) + geom_line()
a <- a + facet_wrap(~variable, scale='free_x')
a <- a + theme_minimal(10)
a

## --------------- Metiers analysis ----------------------------
library(ade4)
library(stats)
library(RColorBrewer)
library(pixmap)
library(tidyr)
library(WVPlots)

source("2023/R/some_fun.R")

especies <- select( dat.esp, -total )
names(especies)
especies <- especies[,!!colSums( especies[grep( 'X', names(especies) )] )]  # Removing all columns summing to zero

esp.sub   <- select(filter(cbind(year=dat.ope$year, especies), year>=1997), -year)
dim(esp.sub)
# 4785  25
#8192   28

#Reducir el numero especies a aquellas que expliquen el 85% de la varianza
esp.sub   <- as.matrix(esp.sub)
pcr       <- prcomp(esp.sub, center = FALSE, scale. = FALSE)
pcr2      <- princomp(esp.sub)

#plot(pcr$sdev)

dotplot_identity(frame = data.frame(pc=1:length(pcr$sdev), magnitude=pcr$sdev), 
                 xvar="pc",yvar="magnitude") + ggtitle("Unscaled case: Magnitudes of singular values")

#decidir el numero de CP para el corte (85% var) 80%

projectedTrainIdeal <- as.data.frame(esp.sub %*% extractProjection(5,pcr)[,1:5], stringsAsFactors = FALSE)
projectedTrainIdeal$cpue <- dat.ope$cpue
#cambia a ortogonal
#se le agregan los datos de cupe para ver si tb separa los componentes

#scatter.hist(projectedTrainIdeal, pch=(19+as.numeric(dat.msur.ope$zona)), col=c("blue","red")[dat.msur.ope$zona], grid=TRUE)

ttg <- projectedTrainIdeal

ScatterHistN(projectedTrainIdeal, xvar='PC1', yvar='PC2', zvar='cpue', title="Datos proyectados en los PC 1-2", nclus = 2)
#explicacion del grafico

rot5U <- extractProjection(5,pcr)
rot5U = as.data.frame(rot5U)
rot5U$varName = rownames(rot5U)
rot5U = gather(rot5U, "PC", "loading", starts_with("PC"))
rot5U$vartype = ifelse(grepl("\\bX2\\b|\\bX3\\b|\\bX4\\b|\\bX6\\b|\\bX37\\b|\\bX94\\b", 
                             as.character(rot5U$varName)),
                       "target", "bycatch")
rot5U$vartype = ifelse(grepl("\\bX2\\b", as.character(rot5U$varName)),
                       "Msur", 
                       ifelse(grepl("\\bX6\\b", as.character(rot5U$varName)),
                              "Congrio", 
                              ifelse(grepl("\\bX3\\b", as.character(rot5U$varName)),
                                     "M3A", 
                                     ifelse(grepl("\\bX4\\b", as.character(rot5U$varName)),
                                            "Mcola", 
                                            ifelse(grepl("\\bX5\\b", as.character(rot5U$varName)),
                                                   "Brotula", 
                                                       ifelse(grepl("\\bX94\\b", as.character(rot5U$varName)),
                                                          "Cojinoba", "bycatch"))))))

# ifelse(grepl("\\bX2\\b", as.character(rot5U$varName)),
#        "target", "bycatch")
x11()
dotplot_identity(filter(rot5U, !(loading>=-0.001 & loading<=0.001) ), "varName", "loading", "vartype") + 
  facet_wrap(~PC,nrow=1) + coord_flip() + 
  ggtitle("unscaled variable loadings, first 5 principal components") +
  scale_color_pander() + theme_minimal() + geom_hline(aes(yintercept=0), colour="#d95f02") +
  geom_point(size=3) + geom_linerange(aes(ymin=0), size=1.5)




pcl       <- unclass(pcr$rotation)  # pcl       <- extractProjection(dim(esp.sub)[2],pcr)
pcl2      <- unclass(pcr2$loadings)
pclperc   <- 100*(pcr$sdev)/sum(pcr$sdev)
pclperc2  <- 100*(pcr2$sdev)/sum(pcr2$sdev)

#componenetes que suman el 95% de 100*(pcr$sdev)/sum(pcr$sdev)

supe99    <- cumsum( pclperc ) <= 95
supe992   <- cumsum( pclperc2[grep( 'Comp.', names(pclperc2) )] ) <= 95

# que es pcr$x
ingre     <- data.frame( pcr$x[,supe99] )
ingre2    <- data.frame( pcr2$scores[,supe992] )

# Analisis de agrupamiento no jerarquico ya que el gran numero de lances no es manjeable a traves de agrupamiento jerarquico
#cuyo objetivo fue agrupar el total del lances en 2500

cl        <- kmeans( ingre, 1500 )
cl2       <- kmeans( ingre2, 1500 )

#utilizando distancia euclidiana

Y         <- dist( cl$centers, "euclidean" )
Y2        <- dist( cl2$centers, "euclidean" )

# despues se realiza el agrupamiento jerarquico (dendograma) a estas 2500 agrupaciones no jerarquicas metodo ward

Z         <- hclust( Y, "ward.D" )
Z2        <- hclust( Y2, "ward.D" )

dendo     <- as.dendrogram(Z)  # 1500 centroides
dendo2    <- as.dendrogram(Z2)  

plot(dendo)
plot(dendo2)

hcut      <- quantile( Z$height, c(0.998) ) 
ngrp      <- length( cut(dendo,hcut)$lower ) 
groupes   <- cutree( Z, k=ngrp )

hcut2     <- quantile( Z2$height, c(0.998) ) 
ngrp2     <- length( cut(dendo2,hcut2)$lower ) 
groupes2  <- cutree( Z2, k=ngrp2 )

# Rescate de datos

vector   <- matrix(cl$cluster, ncol=1) * NA
vector2  <- matrix(cl2$cluster, ncol=1) * NA

for (j in 1:1500)
{
  vector[which( cl$cluster==j ), 1] = groupes[j]
  #vector2[which( cl2$cluster==j ), 1] = groupes2[j]
}

#proporcion de especies por cluster cada cluster suma 1

prop.cluster <- NULL
for (i in 1:ngrp)
{
  cluster      <- esp.sub[vector==i,]
  prop.cluster <- as.data.frame(rbind(prop.cluster, c(colMeans(cluster, na.rm = FALSE), i, dim(cluster)[1])))
} 

prop.cluster2 <- NULL
for (i in 1:ngrp2)
{
  cluster2      <- esp.sub[vector2==i,]
  prop.cluster2 <- as.data.frame(rbind(prop.cluster2, c(colMeans(cluster2, na.rm = FALSE), i, dim(cluster2)[1])))
} 

colnames(prop.cluster) <- c(names(as.data.frame(cluster)), 'Metier', 'Nmuestras')
prop.sp <- select( prop.cluster, c(-Metier, -Nmuestras))

colnames(prop.cluster2) <- c(names(as.data.frame(cluster2)), 'Metier', 'Nmuestras')
prop.sp2 <- select( prop.cluster2, c(-Metier, -Nmuestras))


ki        <- apply(prop.sp, 1, cumsum)
pie.data  <- NULL
for (k in 1:ngrp)
{
  tmp       <- ki[,k]
  ubi       <- tmp[grep( 'X', names(tmp) )] <= 0.90
  esp.me    <- data.frame(SP=prop.sp[k, ubi], otras=1-sum(prop.sp[k, ubi]), 
                          Metier=paste0('Metier: ',prop.cluster$Metier[k]), 
                          muestras=paste0('nmuestra: ',prop.cluster$Nmuestras[k]), n=prop.cluster$Nmuestras[k])
  esp.me    <- esp.me[,esp.me!=0]
  pie.data  <- rbind(pie.data, melt(esp.me, id.vars = c('Metier','muestras','n')))
}


bb <- filter(pie.data)#, Metier!='Metier: 1')
ggplot(bb, aes(Metier, value, fill=variable, width=n/(2*max(n)) )) +
  geom_bar(stat = "identity", position="fill") + theme_minimal(14) 


## -----------------  analisis years 2014-2015 ---------------------------


bp <- ggplot(pie.data, aes(x="", y=value, fill=variable)) + 
  geom_bar(width = 1, stat = "identity") + facet_wrap(~Metier)


pie <- bp + coord_polar("y", start=0)
#pie <- pie + scale_fill_grey() + theme_minimal()

pie


#save(list = ls(all=TRUE), file = "Cluster.RData")
#write.csv(vector, file = "cluster_arrastre.csv", row.names = T)

metiers <- read.csv('cluster_arrastre.csv')

data$metier <- metiers[,2]
ope <- cbind(dat.ope, metier=metiers[,2])

names(data)

library(dplyr)
library(lattice)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(lubridate)
library(psych)


catch.zona    <- melt(dcast(ope, year + mes  ~ metier , geometric.mean, value.var=c('cpue')), id=c(1,2))
catch.zona$ts <- make_datetime(catch.zona$year, catch.zona$mes)
x11()

a <- ggplot( catch.zona, aes( x=ts, y=value, color=as.factor(variable) ) )
a <- a + geom_line() 
a <- a + facet_wrap(~year, scale='free_x')
a <- a + theme_igray() + scale_color_discrete(name="") + scale_shape_discrete(name="")
a <- a + scale_x_datetime(breaks = date_breaks("60 days"), labels = date_format("%m"))
a


grp.1 <- group_by(data, year, mes) 
ff.1  <- summarise(grp.1,
                   TOTAL  = n(),
                   catch  = sum(Merluza_3_a/1000, na.rm = TRUE),
                   effort = sum(HA, na.rm = TRUE),
                   cpue   = mean(cpue, na.rm = TRUE),
                   cpue.m = median(cpue, na.rm = TRUE),
                   cpue.r = catch/effort
)

ff.1
ff.1$ts <- make_datetime(ff.1$year, ff.1$mes)
ff.1.m  <- melt(ff.1, id.vars = c('year','mes','ts', 'TOTAL'))

fig.1 <- ggplot(ff.1.m, aes(x = mes, y = value)) +
  #fig.1 <- ggplot(filter(ff.1.m, mes!=8), aes(x = mes, y = value)) +
  geom_point(aes(size=TOTAL, colour=as.factor(year)), alpha=I(0.35)) + geom_line(aes(colour=as.factor(year))) +
  facet_wrap(~variable, ncol = 3, scales = 'free') + theme_minimal(14) + 
  labs(x = "Mes", y = "Valor") + ggtitle("Captura / Esfuerzo / CPUE")

fig.1

grp.2 <- group_by(data, year) 
ff.2  <- summarise(grp.2,
                   TOTAL  = n(),
                   catch  = sum(Merluza_3_a/1000, na.rm = TRUE),
                   effort = sum(HA, na.rm = TRUE),
                   cpue   = mean(cpue, na.rm = TRUE),
                   cpue.m = median(cpue, na.rm = TRUE),
                   cpue.r = catch/effort
)


ff.2.m  <- melt(ff.2, id.vars = c('year','TOTAL'))

fig.2 <- ggplot(ff.2.m, aes(x = year, y = value)) +
  geom_point(aes(size=as.factor(TOTAL)), alpha=I(0.35)) + geom_line() +
  facet_wrap(~variable, ncol = 3, scales = 'free') + theme_minimal(14) + 
  labs(x = "Mes", y = "Valor") + ggtitle("Captura / Esfuerzo / CPUE")

fig.2


grp.3 <- group_by(ope, year, metier) 
ff.3  <- summarise(grp.3,
                   TOTAL  = n(),
                   catch  = sum(Merluza_3_a/1000, na.rm = TRUE),
                   effort = sum(HA, na.rm = TRUE),
                   cpue   = mean(cpue, na.rm = TRUE),
                   cpue.m = median(cpue, na.rm = TRUE),
                   cpue.r = catch/effort
)

ff.3.m  <- melt(ff.3, id.vars = c('year','metier','TOTAL'))

fig.3 <- ggplot(ff.3.m, aes(x = year, y = value)) +
  geom_point(aes(size=TOTAL, colour=as.factor(metier)), alpha=I(0.35)) + geom_line(aes(colour=as.factor(metier))) +
  facet_wrap(~variable, ncol = 3, scales = 'free') + theme_minimal(14) + 
  labs(x = "A?o", y = "Valor") + ggtitle("Captura / Esfuerzo / CPUE")

fig.3



library(lmtest)

PseudoR <- function (objeto)
{
  salida <-  1-(objeto$deviance/objeto$null.deviance)
  return(salida)
  end
}

data$year<-as.factor(data$year)
data$mes<-as.factor(data$mes)
data$zona<-as.factor(data$zona)
data$COD_BARCO<-as.factor(data$COD_BARCO)
data$metier<-as.factor(data$metier)
summary(data)

data_n0 <- subset(data,log(data$cpue) > 0)
mod6 <- glm(log(cpue) ~ year +  metier + I(COD_BARCO:mes), data = data_n0, family="gaussian", na.action = na.exclude)

summary(mod6)
anova(mod6)
PseudoR(mod6)
coeftest(mod6)
mod6
drop1(mod6, test="F")
