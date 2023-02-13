######
#DATOS LISTOS
########

###EMPEZAR DESDE ACA CON LA BASE DE DATOS LISTA

rm(list=ls(all=TRUE))

source('2023/R/some_fun.R')

data <- read.csv('data_m3a_arrastre_2022.csv', header=T, sep=',')
data <- select(data, -X)

library(dplyr)


#########################
###### TABLAS ###########
#########################

#creamos un archivo temporal con las variables que necesitamos para hacer el resumen

tserie <- data %>% tbl_df() %>% 
  dplyr::select(year, HA, Merluza_3_a, NOMBRE_BARCO,zona,mes,COD_BARCO,COD_PESQUERIA) %>% 
  filter(HA>0, Merluza_3_a > 0, year>=1997)#  filter(!(year==2015 &  COD_BARCO==400075)) 
dim(tserie)
# 4497 7
#  8192  8

# ------------ agrupamos las horas de arrastre por a?o ----------------
nhook <- tserie %>% group_by(year) %>% #filter(COD_PESQUERIA %in% c(5,92)) %>%
  summarise (hook = mean(HA, na.rm = TRUE),
             hooksd = sd(HA, na.rm = TRUE),
             hookn = n(),
             hooke = qnorm(0.975)*hooksd/sqrt(hookn),
             hookl = hook - hooke,
             hookr = hook + hooke)

#write.csv(nhook, file = "nhook.csv", row.names = T)

# ------------ agrupamos las horas de arrastre por a?o y por barco ----------------
nhook1 <- tserie %>% group_by(year, COD_BARCO) %>% 
  summarise (hook = mean(HA, na.rm = TRUE),
             hooksd = sd(HA, na.rm = TRUE),
             hookn = n(),
             hooke = qnorm(0.975)*hooksd/sqrt(hookn),
             hookl = hook - hooke,
             hookr = hook + hooke)

#write.csv(nhook1, file = "nhook1.csv", row.names = T)

#ploteamos horas de arrastre prom por a?o
x11()
p1 <- ggplot(nhook, aes(year)) +# facet_wrap(~buque) + 
  geom_line(aes(y=hook), colour="blue") + 
  geom_ribbon(aes(ymin=hookl, ymax=hookr), alpha=0.2)
p1

# ------------ captura promedio por a?o  ----------------
ncatch <- tserie %>% group_by(year) %>% 
  summarise (catch = mean(Merluza_3_a, na.rm = TRUE),   #catch  = sum(cap_msur/1000, na.rm = TRUE),
             catchsd = sd(Merluza_3_a, na.rm = TRUE),
             catchn = n(),
             catche = qnorm(0.975)*catchsd/sqrt(catchn),
             catchl = catch - catche,
             catchr = catch + catche)

#write.csv(ncatch, file = "ncatch_prom.csv", row.names = T)

#######################################################################3
# por a?o y buque

ncatch4 <- tserie %>% group_by(year,COD_BARCO) %>% 
  summarise (catch = mean(cap_m3a, na.rm = TRUE),
             catchsd = sd(cap_m3a, na.rm = TRUE),
             catchn = n(),
             catche = qnorm(0.975)*catchsd/sqrt(catchn),
             catchl = catch - catche,
             catchr = catch + catche)

#write.csv(ncatch4, file = "ncatch1_sum_buque.csv", row.names = T)
######################################################################
#ploteamos captura promedio de msur por a?o

x11()
p2 <- ggplot(ncatch, aes(year)) +  #facet_wrap(~year) + 
  geom_line(aes(y=catch), colour="blue") + 
  geom_ribbon(aes(ymin=catchl, ymax=catchr), alpha=0.2) 
p2

p7 <- ggplot(ncatch4, aes(year)) +  facet_wrap(~COD_BARCO) + 
  geom_line(aes(y=catch), colour="blue") + 
  geom_ribbon(aes(ymin=catchl, ymax=catchr), alpha=0.2) 
p7

# ------------ cpue por a?o y buque ----------------
ncpue1 <- tserie %>% group_by(year) %>% 
  summarise (f1 = mean(Merluza_3_a, na.rm = TRUE),
             f2 = mean(HA, na.rm = TRUE),
             cpue2 = f1/f2,
             cpue2sd = sd(Merluza_3_a/HA, na.rm = TRUE),
             cpue2n = n(),
             cpue2e = qnorm(0.975)*cpue2sd/sqrt(cpue2n),
             cpue2l = cpue2 - cpue2e,
             cpue2r = cpue2 + cpue2e)

#write.csv(ncpue1, file = "ncpue1.csv", row.names = T)

# cpue por a?o 

ncpue <- tserie %>% group_by(year) %>% 
  summarise (f1 = mean(cap_m3a/1000, na.rm = TRUE),
             f2 = mean(HA, na.rm = TRUE),
             cpue = f1/f2,
             cpuesd = sd(cap_m3a/HA, na.rm = TRUE),
             cpuen = n(),
             cpuee = qnorm(0.975)*cpuesd/sqrt(cpuen),
             cpuel = cpue - cpuee,
             cpuer = cpue + cpuee)

#write.csv(ncpue, file = "ncpue.csv", row.names = T)

# ------------ hook time serie ----------------

ncpue3 <- tserie %>% group_by(year) %>%
  summarise (cpue = mean((Merluza_3_a/1000)/(HA), na.rm = TRUE),
             cpuesd = sd((Merluza_3_a/1000)/(HA), na.rm = TRUE),
             cpuen = n(),
             cpuee = qnorm(0.975)*cpuesd/sqrt(cpuen),
             cpuel = cpue - cpuee,
             cpuer = cpue + cpuee)

#write.csv(ncpue3, file = "ncpue3_ano.csv", row.names = T)


ncpue4 <- tserie %>% group_by(year,mes) %>% 
  summarise (f1 = mean((cap_m3a/1000), na.rm = TRUE),
             f2 = mean(HA, na.rm = TRUE),
             cpue = f1/f2,
             cpuesd = sd((cap_m3a/1000)/HA, na.rm = TRUE),
             cpuen = n(),
             cpuee = qnorm(0.975)*cpuesd/sqrt(cpuen),
             cpuel = cpue - cpuee,
             cpuer = cpue + cpuee)

#ploteamos cpue
x11()
p6 <- ggplot(ncpue4, aes(mes)) +  facet_wrap(~year) + 
  geom_point(aes(y=cpue, size=cpuen), colour="blue", alpha=0.5, shape=21) +
  geom_line(aes(y=cpue), colour="blue") + 
  geom_ribbon(aes(ymin=cpuel, ymax=cpuer), alpha=0.2) 
p6


ncpue5<- tserie %>% group_by(COD_BARCO,year) %>% #filter(NOM_BUQ %in% ("ANTARTIC ENDEAVOUR", "FRIO SUR VI", "JIN YANG 1", "GUALAS"))%>% 
  summarise (f1 = mean((cap_m3a/1000), na.rm = TRUE),
             f2 = mean(HA, na.rm = TRUE),
             cpue = f1/f2,
             cpuesd = sd(cap_m3a/HA, na.rm = TRUE),
             cpuen = n(),
             cpuee = qnorm(0.975)*cpuesd/sqrt(cpuen),
             cpuel = cpue - cpuee,
             cpuer = cpue + cpuee)

#write.csv(ncpue5, file = "cpue_bueque_ano.csv", row.names = T)

p5 <- ggplot(ncpue3, aes(year)) +  facet_wrap(~zona) + 
  geom_point(aes(y=cpue, size=cpuen), colour="blue", alpha=0.5, shape=21) +
  geom_line(aes(y=cpue), colour="blue") + 
  geom_ribbon(aes(ymin=cpuel, ymax=cpuer), alpha=0.2) 
p5



p4 <- ggplot(ncpue3, aes(year)) + 
  geom_point(aes(y=cpue, size=cpuen), colour="blue", alpha=0.5, shape=21) +
  geom_line(aes(y=cpue), colour="blue") + 
  geom_ribbon(aes(ymin=cpuel, ymax=cpuer), alpha=0.2) 
p4

x11()
multiplot(p1, p2, p4, cols=1)


#######################################################
