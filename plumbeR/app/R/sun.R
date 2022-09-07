#library(devtools)
#install_github("https://github.com/yaroslavtsevam/ttalkR")
#library(ttalkR)
# install.packages("ggsci")
# install.packages("simplevis")
library(simplevis)
library(ggsci)

source("R/ttscrape.R")
source("R/ttexport.R")

#source("R/ttbandgapTcorrection.R")

#Apatity

TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/CAC51001/ttcloud.txt",
  to_process = T)
sundata = TTdat$mydata_exp

#SPB
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/CAC78001/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

#Rostov
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/C18A0630/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

#Troitsk
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/C18A0027/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

#RUDN 1
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/C18A0031/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

#RUDN 2
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/C18A0025/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

#TIM1
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/C0200136/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

#TIM2
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/C0200138/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)


# Botanics MSU
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/CAC99CAD/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

# Grozny
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/CAC20CAD/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

# Salt experiment
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/ACAD7701/ttcloud.txt",
  to_process = T)
sundata = rbind(sundata,TTdat$mydata_exp)

#SERBIA
TTdat = ttscrape(
  metadata_file = "ttdata_examples/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/C19B0079/ttcloud.txt",
  to_process = T)
ЕЕsundata = rbind(sundata,TTdat$mydata_exp)




ggplotly(TTdat$mydata_f %>% ggplot()+geom_point(aes(Timestamp,Fdh,color=TT_ID)))


library(openair)
library(plotly)








dt = sundata %>% mutate(date = Timestamp) %>%
                 filter(doys < 330, years >2020, !(TT_ID %in% c("22ACCA13","22ACCA16", "55ACCAD1")),
                        !(hours %in% c(2,5,8,11,14,17,20,23)))
dt = dt %>% group_by(TT_ID) %>% mutate(Tair = Tair)
noTTR = function(data){
  return(data %>% filter(Species != "TTR"))
}

dt$Tair[dt$Tair < -19]=NA


mdt = dt %>% filter(Site %in% c("Troitsk","RUDN campus","Moscow Timiryazev Forest",
                                "Botanical garden MSU"))


mdt = mdt %>% mutate(SName = case_when(
                                Site == "Troitsk" ~ "Троицк",
                                Site == "Moscow Timiryazev Forest" ~ "ЛОД",
                                Site == "RUDN campus" ~ "РУДН",
                                Site == "Botanical garden MSU" ~ "МГУ",
))

rdt = rdt %>% mutate(SName = case_when(
  Site == "Apatity campus" ~ "Апатиты",
  Site == "Saint Peterburg" ~ "Санкт-Петербург",
  Site == "Rostov" ~ "Ростов-на-Дону",
  Site == "Troitsk" ~ "Москва",
))


rdt = dt %>% filter(Site %in% c("Apatity campus","Saint Peterburg",
                                "Rostov","Troitsk"))


ggplotly(dt %>% filter(Site == "Botanical garden MSU") %>% ggplot()+geom_point(aes(date,Fdh,color=TT_ID)))



#SPB check

data = TTdat$mydata_f
year(data$Timestamp) %>% unique()
data$Stime = dmy_hms(paste(data$SDate,data$STime, sep=" "))
ggplot(data %>% filter(year(Stime)==2022))+
  geom_point(aes(Stime,Theat_0, color=TT_ID))+
  theme_bw()


#  AirT
mdt = TTdat$mydata_exp
mdt_s = mdt %>% filter(months<10 & months >4)
#mdt_s$Tair = savgol_filt_ts(mdt_s$Tair)
mdt_s$Tair[mdt_s$Tair<0]=NA
mdt_s$date = mdt_s$Timestamp

tairplot_d = timeVariation(mdt_s,
              pollutant = c("Tair"),
              normalise = F, group="Species")



plot(tairplot_d, subset = "weekday")

tairplot_m = timeVariation(noTTR(mdt),
                         pollutant = c("Tair"),
                         normalise = F, group="SName")
plot(tairplot_m, subset = "month", ylab="Температура воздуха, С")



# RH

mdt$RH[mdt$SName=="Троицк"] = mdt$RH[mdt$SName=="Троицк"]-20
mdt$VPD = 0.6108*exp((17.27*mdt$Tair) /(mdt$Tair+273.15)) * (1 - mdt$RH/100)
rhplot_d = timeVariation(noTTR(mdt),
                           pollutant = c("VPD"),type="months",
                           normalise = F, group="SName")

rhplot_d = timeVariation(mdt_s %>% filter(TT_ID !="22ACCC01"),
                         pollutant = c("RH"),type="Species",
                         normalise = F, group="TT_ID", key=F,
                         ylab="Относительная влажность, %",
                         xlab="Время дня, ч")

graphics::layout(mat=matrix(c(1,2,3,3),nrow=2,ncol=2,byrow=T))
plot(rhplot_d, subset = "day.hour")
plot(rhplot_d, subset = "hour")
plot(rhplot_d, subset = "day")
layout.show(l)
tairplot_d = timeVariation(mdt_s %>% filter(TT_ID !="22ACCC01"),
                         pollutant = c("Tair"),type="Species",
                         normalise = F, group="TT_ID", key=T,
                         ylab="Температура воздуха, С",
                         xlab="Время дня, ч")


Fdhplot_d = timeVariation(mdt_s %>% filter(TT_ID !="22ACCC01"),
                           pollutant = c("Flux"),type="Species",
                           normalise = F, group="TT_ID", key=T,
                          ylab="Сокотечение, л/ч",
                          xlab="Даты")

Gplot_d = timeVariation(mdt_s %>% filter(TT_ID !="22ACCC01"),
                          pollutant = c("Bat_mV"),type="Species",
                          normalise = F, group="TT_ID", key=T)
StWCplot_d = timeVariation(mdt_s %>% filter(TT_ID !="22ACCC01"),
                          pollutant = c("StWC"),type="Species",
                          normalise = F, group="TT_ID", key=T)

mdt_s = mdt_s %>% group_by(doys) %>% mutate(NDVIn = quantile(NDVI,0.75, na.rm=T))

ggplot(noTTR(mdt_s),aes(date, NDVIn, color=TT_ID)) +
  geom_point( aes(date, NDVI, color=TT_ID), alpha=1/2)+
  geom_smooth(aes(date, NDVI, color=TT_ID), method="lm",span=2, se=F)+
  ylab("NDVI")+
  xlab("Дата")+
  theme_bw()

plot(rhplot_d, subset = "hour")
rhplot_m = timeVariation(noTTR(mdt_s),
                           pollutant = c("VPD"),
                           normalise = F, group="SName")
plot(rhplot_m, subset = "month", ylab="Температура воздуха, С")




#Fdh
mdt_s = mdt
mdt_s = mdt %>% filter(months<10 & months >5)
mdt_s$date = mdt_s$date+3600*12
mdt_s$Fdh[mdt_s$Fdh>60]=NA
mdt_s$Fdh[mdt_s$Fdh>20 & mdt_s$months > 8]=NA
mdt_s$Fdh[ mdt_s$months == 10 & mdt_s$SName=="МГУ"]=3
mdt_s$Fdh[mdt_s$Fdh>20 & mdt_s$months >10]=NA
Fluxplot_d = timeVariation(noTTR(mdt_s),
                         pollutant = c("Fdh"),type="months",
                         normalise = F, group="SName")

plot(Fluxplot_d, subset = "hour")
Fluxplot_m = timeVariation(noTTR(mdt_s),
                         pollutant = c("Fdh"),
                         normalise = F, group="SName")
plot(Fluxplot_d, subset = "month")
 mdt_s %>% group_by(TT_ID, SName) %>% filter(Flux < 60, Species=="Tilia cordata") %>%
              summarise(mFlux = mean(Flux)*30*24) %>% mutate(SName = as.factor(SName)) %>%
              ggplot()+geom_boxplot(aes(x=SName, y=mFlux),)+
              ylim(c(0,1000))+theme_bw()

#StWc

WCplot_d = timeVariation(noTTR(mdt),
                           pollutant = c("StWC"),
                           normalise = F, group="SName")


#NDVI

NDVIplot_m = timeVariation(mdt,
                           pollutant = c("NDVI"),type="Species",
                           normalise = F, group="SName")
plot(NDVIplot_m, subset = "month")

NDVIplot_m = timeVariation(noTTR(mdt),
                           pollutant = c("NDVI"),type="Species",
                           normalise = F, group="SName")
plot(NDVIplot_m, subset = "month")

mdt = mdt %>% group_by(doys) %>% mutate(NDVIn = quantile(NDVI,0.75, na.rm=T))

ggplot(mdt,aes(date, NDVIn, color=Species)) +
  geom_point( aes(date, NDVI, color=Species), alpha=1/30)+
  geom_smooth(aes(date, NDVIn, color=Species), method="loess",span=.5, se=F)+
  ylab("NDVI")+
  xlab("Дата")+
  theme_bw()


#Stability
mdt=mdt %>% mutate(weeks = week(Timestamp)) %>% group_by(TT_ID,weeks) %>% mutate(dPhi = mean(Phi)-Phi)


Stabplot_m = timeVariation(noTTR(mdt),
                           pollutant = c("dPhi"),
                           normalise = F, group="SName")
plot(Stabplot_m, subset = "hour")

ggplotly(dt  %>% ggplot()+geom_point(aes(date,Phi,color=TT_ID)))




NDVIg = dt %>% as.data.frame %>% select(date, NDVI, TT_ID) %>%
  ungroup() %>% pivot_wider(
    names_from = TT_ID,values_from = NDVI, values_fn = max
  ) %>% as.data.frame()
names(NDVIg) = c("date","01","02","03","22AC9905","22AC9906",
                 "22AC9907","22AC9908","22AC9909","22AC9910")
smoothTrend(dt, pollutant = "NDVI")

scatterPlot(NDVIg, x = "date", y = "22AC9909",  smooth = TRUE,
            avg.time="week", statistic = "sd")
scatterPlot(dt, x = "date", y = "NDVI", z="RH")
linearRelation(dt, x = "date", y = "NDVI")

















library(rbokeh)
library(plotly)

data  = TTdat$mydata_exp


#Bokeh
z <- lm(dist ~ speed, data = cars)
p <- figure(width = 600, height = 600) %>%
  ly_points(cars, hover = cars) %>%
  ly_lines(lowess(cars), legend = "lowess") %>%
  ly_abline(z, type = 2, legend = "lm") %>%
  ly_lines(lowess(cars), color = "red", width = 2)
p

p <- figure() %>%
  ly_points(Sepal.Length, Sepal.Width, data = iris,
            color = Species, glyph = Species,
            hover = list(Sepal.Length, Sepal.Width))
p

NDVIgraph = figure(data = TTdat$mydata_exp) %>%
  ly_points(x = Timestamp, y = NDVI, color = TT_ID) %>%
   x_axis(label = "Dates",
          number_formatter = "basic",
          format = list(days = "%d",
                        months = "%B",
                        years = "%Y"),
          use_scientific = T) %>%
  set_theme(bk_default_theme)

  ly_lines(lowess(Timestamp,NDVI, delta=1),
           color = TT_ID, width = 2)
NDVIgraph


NDVIgraph = ggplot(TTdat$mydata_exp)+
  geom_point(aes(Timestamp,NDVI, color=TT_ID))+
  theme_h_gridlines()






NDVIgraph =  gg_point_col(TTdat$mydata_exp,
             x_var = Timestamp,
             y_var = NDVI,
             col_var = TT_ID)+
            geom_smooth(aes(x= Timestamp, y = NDVI, color=TT_ID))
plotly::ggplotly(NDVIgraph)



ggplot(mydata_f, aes(Timestamp, RcppRoll::roll_maxl(growt_sens,4)))+
  geom_point(aes(color = TT_ID))+
  #ggdemetra::geom_arima(aes(color = TT_ID))
  geom_smooth(aes(color = TT_ID), method = "loess", span=1)

ggplot(mydata_f %>% filter(TT_ID =="22AC5110" ), aes(Timestamp, AS7263_810_Rc))+
  geom_point(aes(color = TT_ID))+
  geom_line(aes(color = TT_ID))

z=ggplot(TTdat$mydata_f, aes(Rec_Nr, Bat_mV))+
  geom_point(aes(color = TT_ID))
  #geom_line(aes(color = as.factor(charge)))
plotly::ggplotly(z)

ggplot(mydata_f, aes(StWC, Fd))+
  geom_point(aes(color = TT_ID))
#ggdemetra::geom_arima(aes(color = TT_ID))
#geom_smooth(aes(color = TT_ID), method = "loess", span=1)


ggplot(TTdat$mydata_exp, aes(Timestamp,Bat_mV))+
  geom_point(aes(color = TT_ID))









ggplot(mydata_f %>% filter(TT_ID =="22AC5115" ))+
  geom_line(aes(x=Timestamp,y = Fd*1000000, color = "1"))+
  geom_line(aes(x=Timestamp,y = VPD*10, color = "2"))+
  geom_line(aes(x=Timestamp,y = Tair, color = "3"))+
  geom_line(aes(x=Timestamp,y = StWC*100, color = "4"))
#geom










##################################    SPB


#  AirT
mdt = TTdat$mydata_exp
mdt$date = mdt$Timestamp
mdt_s = mdt %>% filter(months<10 & months >5)
#mdt_s$Tair = savgol_filt_ts(mdt_s$Tair)
mdt_s$Tair[mdt_s$Tair<0]=NA
mdt$Species %>% unique()

tairplot_d = timeVariation(mdt_s %>% filter(Species =="TTR"),
                           pollutant = c("Tair"),type="months",
                           normalise = F, group="Species")



plot(tairplot_d, subset = "hour")
tairplot_m = timeVariation(mdt_s,
                           pollutant = c("Tair"),
                           normalise = F, group="Species")
plot(tairplot_m, subset = "day", ylab="Температура воздуха, С")



# RH


mdt$VPD = 0.6108*exp((17.27*mdt$Tair) /(mdt$Tair+273.15)) * (1 - mdt$RH/100)
rhplot_d = timeVariation(noTTR(mdt),
                         pollutant = c("VPD"),type="months",
                         normalise = F, group="Species")

plot(rhplot_d, subset = "hour")
rhplot_m = timeVariation(noTTR(mdt_s),
                         pollutant = c("VPD"),
                         normalise = F, group="SName")
plot(rhplot_m, subset = "month", ylab="Температура воздуха, С")




#Fdh
mdt_s = mdt
mdt_s = mdt %>% filter(months<10 & months >5)
mdt_s$date = mdt_s$date+3600*12
mdt_s$Fdh[mdt_s$Fdh>60]=NA
mdt_s$Fdh[mdt_s$Fdh>20 & mdt_s$months > 8]=NA
mdt_s$Fdh[ mdt_s$months == 10 & mdt_s$SName=="МГУ"]=3
mdt_s$Fdh[mdt_s$Fdh>20 & mdt_s$months >10]=NA
Fluxplot_d = timeVariation(noTTR(mdt_s),
                           pollutant = c("Fdh"),type="months",
                           normalise = F, group="Species")

plot(Fluxplot_d, subset = "hour")
Fluxplot_m = timeVariation(noTTR(mdt_s),
                           pollutant = c("Fdh"),
                           normalise = F, group="Species")
plot(Fluxplot_m, subset = "month")
mdt_s %>% group_by(TT_ID, Species, months) %>% filter(Flux < 60) %>%
  summarise(mFlux = mean(Flux)*30*24) %>% mutate(Species = as.factor(Species)) %>%
  ggplot()+geom_boxplot(aes(x=Species, y=mFlux, fill=Species),)+
  ylim(c(0,1000))+facet_wrap(~months)+theme_bw()

#StWc

WCplot_d = timeVariation(noTTR(mdt),
                         pollutant = c("StWC"),
                         normalise = F, group="Species")
plot(WCplot_d, subset = "month")

#NDVI

NDVIplot_m = timeVariation(noTTR(mdt_s),
                           pollutant = c("NDVI"),type="Species",
                           normalise = F, group="Species")
plot(NDVIplot_m, subset = "month")

NDVIplot_m = timeVariation(noTTR(mdt),
                           pollutant = c("NDVI"),
                           normalise = F, group="Species")
plot(NDVIplot_m, subset = "month")

mdt = mdt %>% group_by(doys) %>% mutate(NDVIn = quantile(NDVI,0.75, na.rm=T))

ggplot(mdt,aes(date, NDVI, color=Species)) +
  geom_point( aes(date, NDVI, color=Species), alpha=1/30)+
  geom_smooth(aes(date, NDVI, color=Species), method="loess",span=.5, se=)+
  ylab("NDVI")+
  xlab("Дата")+
  theme_bw()


#Stability
mdt=mdt %>% mutate(weeks = week(Timestamp)) %>% group_by(TT_ID,weeks) %>% mutate(dPhi = mean(Phi)-Phi)


Stabplot_m = timeVariation(noTTR(mdt),
                           pollutant = c("dPhi"),
                           normalise = F, group="SName")
plot(Stabplot_m, subset = "hour")

ggplotly(dt  %>% ggplot()+geom_point(aes(date,Phi,color=TT_ID)))








ggplot(data %>% filter(month(Timestamp)<8))+
  geom_point(aes(x= Timestamp, y=Bat_mV, color=TT_ID, shape=Species))+
  facet_wrap(~Species)+
  theme_bw()







