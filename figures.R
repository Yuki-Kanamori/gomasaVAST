require(tidyverse)
require(plyr)


# 資源評価表用 --------------------------------------------------------
# 

# ---------------------------------------------------------------

# Standardized by VAST ------------------------------------------------------
setwd("/Users/Yuki/Dropbox/saVAST_egg/vast2019-11-13_lnorm_log100spotted_forRankei2")
#vast_c = read.csv("Table_for_SS3.csv") %>% mutate(type = "Standardized \n(chub's effects +)")
vast_c = read.csv("Table_for_SS3.csv") %>% mutate(type = "Standardized (chub's effects +; AIC = 6773.005)")
vast_c = vast_c %>% mutate(scaled = vast_c$Estimate_metric_tons/mean(vast_c$Estimate_metric_tons))
cons_c = exp(qnorm(0.975)*sqrt(log(1+(vast_c$SD_log)^2)))
vast_c = vast_c %>% mutate(kukan_u = vast_c$scaled*cons_c, kukan_l = vast_c$scaled/cons_c)
vast_c = vast_c %>% mutate(kukan_u2 = abs(scaled - kukan_u), kukan_l2 = scaled - kukan_l)
#log = vast_c %>% mutate(log_scaled = log(scaled), log_u = log(kukan_u), log_l = log(kukan_l))

setwd("/Users/Yuki/Dropbox/saVAST_egg/vast2019-11-13_lnorm_log100spottedfixed")
#vast = read.csv("Table_for_SS3.csv") %>% mutate(type = "Standerdized \n(chub's effects -)")
vast = read.csv("Table_for_SS3.csv") %>% mutate(type = "Standerdized (chub's effects -; AIC = 7407.779)")
vast = vast %>% mutate(scaled = vast$Estimate_metric_tons/mean(vast$Estimate_metric_tons))
cons = exp(qnorm(0.975)*sqrt(log(1+(vast$SD_log)^2)))
vast = vast %>% mutate(kukan_u = vast$scaled*cons_c, kukan_l = vast$scaled/cons_c)
vast = vast %>% mutate(kukan_u2 = abs(scaled - kukan_u), kukan_l2 = scaled - kukan_l)

head(vast_c)
head(vast)
a_vast = rbind(vast_c, vast)
a_vast = a_vast %>% select(Year, kukan_u2, kukan_l2, type, scaled)

# Nominal -------------------------------------------------------
setwd("/Users/Yuki/Dropbox/saVAST_egg")
nominal = read.csv("nominal.csv")
nominal = nominal %>% mutate(scaled = nominal$mean/mean(nominal$mean))
head(nominal)
nominal = nominal %>% select(Year, kukan_u2, kukan_l2, type, scaled)

trend = rbind(a_vast, nominal)


# figure --------------------------------------------------------
g = ggplot(trend, aes(x = Year, y = scaled, colour = type))
#cbPalette = c("gray50", "#ff8082", "#4dc4ff", "gold")
#cbPalette = c("#ff8082", "#4dc4ff", "gray50")
#cbPalette = c("plum3", "salmon1", "darkolivegreen3")
cbPalette = c("gray50", "tan1", "plum3")
pd = position_dodge(.3)
p = geom_point(size = 4, aes(colour = type), position = pd)
e = geom_errorbar(aes(ymin = scaled - kukan_l2, ymax = scaled + kukan_u2), width = 0.3, size = .7, position = pd)
#r = geom_ribbon(aes(ymin = scaled - kukan_l, ymax = scaled + kukan_u, fill = type), alpha = 0.3, color = NA)
l = geom_line(aes(colour = type), size = 1.5, position = pd)
#l2 = geom_line(aes(y = scaled - kukan_l, color = type), linetype="twodash")
#l3 = geom_line(aes(y = scaled + kukan_u, color = type), linetype="longdash")
#j = geom_jitter(height = 0, width = 0.5)
lb = labs(x = "Year", y = "Index", color = NULL, title = "(A) Spotted mackerel")
th = theme(#legend.position = c(0.18, 0.8),
  legend.position = c(0.7, 0.9),
  legend.key = element_blank(),
  legend.background = element_blank(),
  axis.text.x = element_text(size = rel(1.5)), #x軸メモリ
  axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
  axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
  axis.title.y = element_text(size = rel(1.5)),
  legend.title = element_text(size = 13), #凡例タイトル
  legend.text = element_text(size = rel(1.5)),
  strip.text = element_text(size = rel(1.3)), #ファセットのタイトル
  plot.title = element_text(size = rel(1.5))) #タイトル
cpue = g+p+e+l+lb+theme_bw()+th+scale_x_continuous(breaks = seq(2005, 2019, 2))+scale_colour_manual(values = cbPalette)


###ノミナル産卵量（マサバ）
setwd("/Users/Yuki/Dropbox/saVAST_egg")
egg = read.csv("df_egg_saba.csv") %>% filter(area_no < 4, between(month, 1, 6)) %>% select(-area, -area_no, -mean_SST, -mean_salinity) %>% mutate(l_dens = log(dens+0.1))
head(egg)
summary(egg)
egg2 = ddply(egg, .(year, species), summarize, sum_dens = sum(dens))

g = ggplot(egg2 %>% filter(species == "chub"), aes(x = year, y = sum_dens))
p = geom_bar(stat = "identity", colour = "gray30")
lb = labs(x = "Year", y = "Egg density", title = "(B) Nominal egg density of chub mackerel")
s = scale_fill_discrete(name = "Month")
#f = facet_wrap( ~ species, ncol = 1, scales = "free")
th = theme(axis.text.x = element_text(size = rel(1.5)), #x軸メモリ
           axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
           axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13), #凡例タイトル
           legend.text = element_text(size = rel(1.3)),
           strip.text = element_text(size = rel(1.3)), #ファセットのタイトル
           plot.title = element_text(size = rel(1.5))) #タイトル
#x = scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2019))
c_egg = g+p+x+lb+s+theme_bw()+th+scale_x_continuous(breaks = seq(2005, 2019, 2))

###規格化したマサバCPUE
setwd("/Users/Yuki/Dropbox/saVAST_egg")
egg = read.csv("df_egg_saba.csv") %>% filter(area_no < 4, between(month, 1, 6), species == "chub") %>% select(-area, -area_no, -mean_SST, -mean_salinity) %>% mutate(l_dens = log(dens+0.1))
head(egg)
summary(egg)
egg2 = ddply(egg, .(year, species), summarize, sum_dens = sum(dens))
egg2 = egg2 %>% mutate(scaled = egg2$sum_dens/mean(egg2$sum_dens))
head(egg2)

g = ggplot(egg2, aes(x = year, y = scaled))
p = geom_bar(stat = "identity", colour = "gray30")
lb = labs(x = "Year", y = "Index", title = "(B) Chub mackerel")
s = scale_fill_discrete(name = "Month")
#f = facet_wrap( ~ species, ncol = 1, scales = "free")
th = theme(axis.text.x = element_text(size = rel(1.5)), #x軸メモリ
           axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
           axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13), #凡例タイトル
           legend.text = element_text(size = rel(1.3)),
           strip.text = element_text(size = rel(1.3)), #ファセットのタイトル
           plot.title = element_text(size = rel(1.5))) #タイトル
#x = scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2019))
c_egg = g+p+x+lb+s+theme_bw()+th+scale_x_continuous(breaks = seq(2005, 2019, 2))



require(gridExtra)
#blankPanel = grid.rect(gp=gpar(col="white"))
grid.arrange(cpue, c_egg, ncol = 1)

# map -----------------------------------------------------------
require(maps)
require(mapdata)
require(ggplot2)

setwd("/Users/Yuki/Dropbox/saVAST_egg/vast2019-11-13_lnorm_log100spotted_forRankei2")
load("Save.RData")

n_c = Save$TmbData$n_c #category (month)
n_t = Save$TmbData$n_t #year
n_x = Save$TmbData$n_x #knot
est_d = c()

for(i in 1:n_c){
  m = log(Save$Report$D_xcy[,i,])
  #m = log(Save$Report$Index_xcyl[,i,,]) #if abundance index needed
  #m = log(Save$Report$Index_xcyl[,i,,]*1000) #if not tons
  est_d = rbind(est_d, m)
}
est_d = data.frame(est_d)

est_d = est_d %>% mutate(knot_i = rep(1:n_x, n_c))
est_d = est_d %>% tidyr::gather(key = x_year, value = Index, 1:n_t)
tag = data_frame(x_year = paste0("X", rep(1:n_t)), year = rep(2005:2019)) #change here
est_d = merge(est_d, tag, by = "x_year")

###Data_Geostat data
DG = read.csv("Data_Geostat.csv") %>% distinct(knot_i, Lon, Lat, .keep_all = T) %>% select(Lon, Lat, knot_i)
DG = merge(DG, est_d, by = "knot_i")
#DG = DG %>% distinct(knot_i, Lon, Lat, .keep_all = T) %>% select(Lon, Lat, knot_i)
head(DG)

labs = labs(title = "", x = "Longitude", y = "Latitude", colour = "Log density")
ncol = 5 #number of figures in line side by side (max is no. of "Category")
shape = 16 #16 is closed dot
size = 1.9 #size of shape

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
jap = subset(world_map, world_map$region == "Japan")
jap_map = map + geom_polygon(data = jap, aes(x = long, y = lat, group = group), colour = "gray 50", fill = "gray 50") + coord_map(xlim = c(130, 150), ylim = c(26, 43))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           #axis.text.x = element_text(size = rel(0.7), angle = 90),
           axis.text.x = element_blank(),
           #axis.text.y = element_text(size = rel(0.7)),
           axis.text.y = element_blank(),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           strip.text = element_text(size = rel(1.3)),
           legend.title = element_text(size = 13))
p = geom_point(data = DG, aes(x = Lon, y = Lat, colour = Index), shape = shape, size = size)
f = facet_wrap( ~ year, ncol = ncol)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
jap_map+theme_bw()+th+p+f+c+labs

# knotの位置情報 -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/saVAST_egg/vast2019-11-13_lnorm_log100spotted_forRankei2")
###Data_Geostat data
DG = read.csv("Data_Geostat.csv") %>% distinct(knot_i,Lon,Lat, .keep_all = T) %>% select(Lon, Lat, knot_i)
head(DG)
labs = labs(title = "", x = "Longitude", y = "Latitude", colour = "Knot")
ncol = 5 #number of figures in line side by side (max is no. of "Category")
shape = 16 #16 is closed dot
size = 1.9 #size of shape

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
jap = subset(world_map, world_map$region == "Japan")
jap_map = map + geom_polygon(data = jap, aes(x = long, y = lat, group = group), colour = "gray 50", fill = "gray 50") + coord_map(xlim = c(130, 150), ylim = c(26, 43))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           #axis.text.x = element_text(size = rel(0.7), angle = 90),
           axis.text.x = element_blank(),
           #axis.text.y = element_text(size = rel(0.7)),
           axis.text.y = element_blank(),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           strip.text = element_text(size = rel(1.3)),
           legend.title = element_text(size = 13))
p = geom_point(data = DG, aes(x = Lon, y = Lat, colour = as.factor(knot_i)), shape = shape, size = size)
f = facet_wrap( ~ year, ncol = ncol)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
jap_map+theme_bw()+th+p+labs



# 論文用 --------------------------------------------------------
# 海区を1-4に
# knotを200に
# ---------------------------------------------------------------
# Standardized by VAST ------------------------------------------------------
setwd("/Users/Yuki/Dropbox/saVAST_egg/vast2019-11-22_lnorm_log200spotted4+")
#vast_c = read.csv("Table_for_SS3.csv") %>% mutate(type = "Standardized \n(chub's effects +)")
vast_c = read.csv("Table_for_SS3.csv") %>% mutate(type = "Estimated index (chub's effects +; AIC = 8250.12)")
vast_c = vast_c %>% mutate(scaled = vast_c$Estimate_metric_tons/mean(vast_c$Estimate_metric_tons))
cons_c = exp(qnorm(0.975)*sqrt(log(1+(vast_c$SD_log)^2)))
vast_c = vast_c %>% mutate(kukan_u = vast_c$scaled*cons_c, kukan_l = vast_c$scaled/cons_c)
vast_c = vast_c %>% mutate(kukan_u2 = abs(scaled - kukan_u), kukan_l2 = scaled - kukan_l)
#log = vast_c %>% mutate(log_scaled = log(scaled), log_u = log(kukan_u), log_l = log(kukan_l))

setwd("/Users/Yuki/Dropbox/saVAST_egg/vast2019-11-22_lnorm_log200spotted4-")
#vast = read.csv("Table_for_SS3.csv") %>% mutate(type = "Standerdized \n(chub's effects -)")
vast = read.csv("Table_for_SS3.csv") %>% mutate(type = "Estimated index (chub's effects -; AIC = 8978.81)")
vast = vast %>% mutate(scaled = vast$Estimate_metric_tons/mean(vast$Estimate_metric_tons))
cons = exp(qnorm(0.975)*sqrt(log(1+(vast$SD_log)^2)))
vast = vast %>% mutate(kukan_u = vast$scaled*cons_c, kukan_l = vast$scaled/cons_c)
vast = vast %>% mutate(kukan_u2 = abs(scaled - kukan_u), kukan_l2 = scaled - kukan_l)

head(vast_c)
head(vast)
a_vast = rbind(vast_c, vast)
a_vast = a_vast %>% select(Year, kukan_u2, kukan_l2, type, scaled)

# Nominal -------------------------------------------------------
setwd("/Users/Yuki/Dropbox/saVAST_egg")
nominal = read.csv("nominal.csv")
nominal = nominal %>% mutate(scaled = nominal$mean/mean(nominal$mean))
head(nominal)
nominal = nominal %>% select(Year, kukan_u2, kukan_l2, type, scaled)

trend = rbind(a_vast, nominal)
head(trend)
trend$type = factor(trend$type, levels = c("Nominal", "Estimated index (chub's effects +; AIC = 8250.12)", "Estimated index (chub's effects -; AIC = 8978.81)"))

# figure --------------------------------------------------------
g = ggplot(trend, aes(x = Year, y = scaled, colour = type))
#cbPalette = c("gray50", "#ff8082", "#4dc4ff", "gold")
#cbPalette = c("#ff8082", "#4dc4ff", "gray50")
#cbPalette = c("plum3", "salmon1", "darkolivegreen3")
#cbPalette = c("gray50", "tan1", "plum3")
cbPalette = c("gray50", "red", "blue")
pd = position_dodge(.3)
p = geom_point(size = 4, aes(colour = type), position = pd)
e = geom_errorbar(aes(ymin = scaled - kukan_l2, ymax = scaled + kukan_u2), width = 0.3, size = .7, position = pd)
#r = geom_ribbon(aes(ymin = scaled - kukan_l, ymax = scaled + kukan_u, fill = type), alpha = 0.3, color = NA)
l = geom_line(aes(colour = type), size = 1.5, position = pd)
#l2 = geom_line(aes(y = scaled - kukan_l, color = type), linetype="twodash")
#l3 = geom_line(aes(y = scaled + kukan_u, color = type), linetype="longdash")
#j = geom_jitter(height = 0, width = 0.5)
lb = labs(x = "Year", y = "Index", color = NULL, title = "")
th = theme(#legend.position = c(0.18, 0.8),
  legend.position = c(0.7, 0.9),
  legend.key = element_blank(),
  legend.background = element_blank(),
  axis.text.x = element_text(size = rel(1.5)), #x軸メモリ
  axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
  axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
  axis.title.y = element_text(size = rel(1.5)),
  legend.title = element_text(size = 13), #凡例タイトル
  legend.text = element_text(size = rel(1.5)),
  strip.text = element_text(size = rel(1.3)), #ファセットのタイトル
  plot.title = element_text(size = rel(1.5))) #タイトル
g+p+e+l+lb+theme_bw()+th+scale_x_continuous(breaks = seq(2005, 2019, 2))+scale_colour_manual(values = cbPalette)
#cpue = g+p+e+l+lb+theme_bw()+th+scale_x_continuous(breaks = seq(2005, 2019, 2))+scale_colour_manual(values = cbPalette)


# ###ノミナル産卵量（マサバ）
# setwd("/Users/Yuki/Dropbox/saVAST_egg")
# egg = read.csv("df_egg_saba.csv") %>% filter(area_no < 5, between(month, 1, 6)) %>% select(-area, -area_no, -mean_SST, -mean_salinity) %>% mutate(l_dens = log(dens+0.1))
# head(egg)
# summary(egg)
# egg2 = ddply(egg, .(year, species), summarize, sum_dens = sum(dens))
# 
# g = ggplot(egg2 %>% filter(species == "chub"), aes(x = year, y = sum_dens))
# p = geom_bar(stat = "identity", colour = "gray30")
# lb = labs(x = "Year", y = "Egg density", title = "(B) Nominal egg density of chub mackerel")
# s = scale_fill_discrete(name = "Month")
# #f = facet_wrap( ~ species, ncol = 1, scales = "free")
# th = theme(axis.text.x = element_text(size = rel(1.5)), #x軸メモリ
#            axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
#            axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
#            axis.title.y = element_text(size = rel(1.5)),
#            legend.title = element_text(size = 13), #凡例タイトル
#            legend.text = element_text(size = rel(1.3)),
#            strip.text = element_text(size = rel(1.3)), #ファセットのタイトル
#            plot.title = element_text(size = rel(1.5))) #タイトル
# #x = scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2019))
# c_egg = g+p+x+lb+s+theme_bw()+th+scale_x_continuous(breaks = seq(2005, 2019, 2))

###規格化したマサバCPUE
setwd("/Users/Yuki/Dropbox/saVAST_egg")
egg = read.csv("df_egg_saba.csv") %>% filter(area_no < 5, between(month, 1, 6), species == "chub") %>% select(-area, -area_no, -mean_SST, -mean_salinity) %>% mutate(l_dens = log(dens+0.1))
head(egg)
summary(egg)
egg2 = ddply(egg, .(year, species), summarize, sum_dens = sum(dens))
egg2 = egg2 %>% mutate(scaled = egg2$sum_dens/mean(egg2$sum_dens))
head(egg2)

g = ggplot(egg2, aes(x = year, y = scaled))
p = geom_bar(stat = "identity", colour = "gray30")
lb = labs(x = "Year", y = "Index", title = "(B)")
s = scale_fill_discrete(name = "Month")
#f = facet_wrap( ~ species, ncol = 1, scales = "free")
th = theme(axis.text.x = element_text(size = rel(1.5)), #x軸メモリ
           axis.text.y = element_text(size = rel(1.5)), #y軸メモリ
           axis.title.x = element_text(size = rel(1.5)), #x軸タイトル
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = 13), #凡例タイトル
           legend.text = element_text(size = rel(1.3)),
           strip.text = element_text(size = rel(1.3)), #ファセットのタイトル
           plot.title = element_text(size = rel(1.5))) #タイトル
#x = scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2019))
c_egg = g+p+lb+s+theme_bw()+th+scale_x_continuous(breaks = seq(2005, 2019, 2))



require(gridExtra)
#blankPanel = grid.rect(gp=gpar(col="white"))
grid.arrange(cpue, c_egg, ncol = 1)

# map -----------------------------------------------------------
require(maps)
require(mapdata)
require(ggplot2)

setwd("/Users/Yuki/Dropbox/saVAST_egg/vast2019-11-22_lnorm_log200spotted4+")
load("Save.RData")

n_c = Save$TmbData$n_c #category (month)
n_t = Save$TmbData$n_t #year
n_x = Save$TmbData$n_x #knot
est_d = c()

for(i in 1:n_c){
  m = log(Save$Report$D_xcy[,i,])
  #m = log(Save$Report$Index_xcyl[,i,,]) #if abundance index needed
  #m = log(Save$Report$Index_xcyl[,i,,]*1000) #if not tons
  est_d = rbind(est_d, m)
}
est_d = data.frame(est_d)

est_d = est_d %>% mutate(knot_i = rep(1:n_x, n_c))
est_d = est_d %>% tidyr::gather(key = x_year, value = Index, 1:n_t)
tag = data_frame(x_year = paste0("X", rep(1:n_t)), year = rep(2005:2019)) #change here
est_d = merge(est_d, tag, by = "x_year")

###Data_Geostat data
DG = read.csv("Data_Geostat.csv") %>% distinct(knot_i, Lon, Lat, .keep_all = T) %>% select(Lon, Lat, knot_i)
DG = merge(DG, est_d, by = "knot_i")
#DG = DG %>% distinct(knot_i, Lon, Lat, .keep_all = T) %>% select(Lon, Lat, knot_i)
head(DG)
summary(DG)

labs = labs(title = "", x = "Longitude", y = "Latitude", colour = "Log density")
ncol = 5 #number of figures in line side by side (max is no. of "Category")
shape = 16 #16 is closed dot
size = 1.9 #size of shape

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
jap = subset(world_map, world_map$region == "Japan")
jap_map = map + geom_polygon(data = jap, aes(x = long, y = lat, group = group), colour = "gray 50", fill = "gray 50") + coord_map(xlim = c(119, 151), ylim = c(23, 44))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           #axis.text.x = element_text(size = rel(0.7), angle = 90),
           axis.text.x = element_blank(),
           #axis.text.y = element_text(size = rel(0.7)),
           axis.text.y = element_blank(),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           strip.text = element_text(size = rel(1.3)),
           legend.title = element_text(size = 13))
p = geom_point(data = DG, aes(x = Lon, y = Lat, colour = Index), shape = shape, size = size)
f = facet_wrap( ~ year, ncol = ncol)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
jap_map+theme_bw()+th+p+f+c+labs

# knotの位置情報 -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/saVAST_egg/vast2019-11-13_lnorm_log100spotted_forRankei2")
###Data_Geostat data
DG = read.csv("Data_Geostat.csv") %>% distinct(knot_i,Lon,Lat, .keep_all = T) %>% select(Lon, Lat, knot_i)
head(DG)
labs = labs(title = "", x = "Longitude", y = "Latitude", colour = "Knot")
ncol = 5 #number of figures in line side by side (max is no. of "Category")
shape = 16 #16 is closed dot
size = 1.9 #size of shape

map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
jap = subset(world_map, world_map$region == "Japan")
jap_map = map + geom_polygon(data = jap, aes(x = long, y = lat, group = group), colour = "gray 50", fill = "gray 50") + coord_map(xlim = c(130, 150), ylim = c(26, 43))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           #axis.text.x = element_text(size = rel(0.7), angle = 90),
           axis.text.x = element_blank(),
           #axis.text.y = element_text(size = rel(0.7)),
           axis.text.y = element_blank(),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           strip.text = element_text(size = rel(1.3)),
           legend.title = element_text(size = 13))
p = geom_point(data = DG, aes(x = Lon, y = Lat, colour = as.factor(knot_i)), shape = shape, size = size)
f = facet_wrap( ~ year, ncol = ncol)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))