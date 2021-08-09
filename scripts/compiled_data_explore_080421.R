###########################
## Compiled data from Marc Weber
##  for NLA 2017 stable isotope calculations
##
## Data received 7/19/21
###########################

remove(list=ls())

library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)

##################
## READ DATA - NLA 2017 n = 1030 obs

dat <-read_csv("~/stable_isotope/data/NLA17_E_I_results_weber_071921.csv")


#############
## FUNCTIONS
# Multiple histograms by category
# https://stackoverflow.com/questions/6957549/overlaying-histograms-with-ggplot2-in-r
plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.7) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
}

###
# PROCESS DATA

dat$date <- as.Date(dat$DATE_COL, "%m/%d/%Y")
summary(dat$date)

length(unique(dat$WRS_ID1)) # 1030
length(unique(dat$SITE_ID))
length(unique(dat$UID))

# ORDER REVISED ECOREGIONS - already in order from west to east but if want to make sure
dat$ECOREG_use<-ordered(dat$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW",
                                                 "NAP","SAP","CPL"))
##################
## DATA EXPLORATION

names(dat)

table(dat$ECOREG_use)
#CPL NAP NPL SAP SPL TPL UMW WMT XER 
#131 108  74 113  97 148 121 155  83 

summary(dat$dD)

################
# Exploratory plots of monthly climate data

#Histogram distributions of climate variables by ecoregion

# Precipitation by month
plot_multi_histogram(dat, 'PRECIP_201610_PT', 'ECOREG_use')
plot_multi_histogram(dat, 'PRECIP_201611_PT', 'ECOREG_use')

# mean temperature by month
plot_multi_histogram(dat, 'TMEAN_201610_PT', 'ECOREG_use')
plot_multi_histogram(dat, 'TMEAN_201612_PT', 'ECOREG_use')

# Vapor pressure min
plot_multi_histogram(dat, 'VPDMIN_201610_PT', 'ECOREG_use')

# Vapor pressure max
plot_multi_histogram(dat, 'VPDMAX_201610_PT', 'ECOREG_use')

# PET
plot_multi_histogram(dat, 'PET_201610_PT', 'ECOREG_use')

# Runoff RUN_201501_PT
plot_multi_histogram(dat, 'RUN_201503_PT', 'ECOREG_use')

# Precipitaiton isotope (O)
plot_multi_histogram(dat, 'D18O_01_PT', 'ECOREG_use')

# Precipitaiton isotope (H) D2H_04_PT
plot_multi_histogram(dat, 'D2H_04_PT', 'ECOREG_use')



###############
# Manipulate data table to plot climate over month
## PRECIPITATION
#First subset data to columns of interest
myvars <-c("WRS_ID1","ECOREG_use","PRECIP_201610_PT","PRECIP_201611_PT","PRECIP_201612_PT",
           "PRECIP_201701_PT","PRECIP_201702_PT","PRECIP_201703_PT","PRECIP_201704_PT",
           "PRECIP_201705_PT","PRECIP_201706_PT","PRECIP_201707_PT","PRECIP_201708_PT",
           "PRECIP_201709_PT","PRECIP_201710_PT")
precip<-dat[myvars]

############
# Melt dataframe using WRS_ID1 to look at monthly precipitation values
# https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame
precip<-setDT(precip)
precip_melt <- melt(precip, id=c("WRS_ID1","ECOREG_use"))

#############
# GRAB MONTH FROM "variable" name
# https://www.datasciencemadesimple.com/extract-substring-of-the-column-in-r-dataframe-2/
precip_melt$month <- substr(precip_melt$variable, 12,13)
precip_melt$year <- substr(precip_melt$variable, 8,11)
head(precip_melt)

################
# PLOT PRECIP ACROSS MONTHS AND BY ECOREGION
plot(precip_melt$month, precip_melt$value)

ggplot(precip_melt, aes(x=month, y=value, group=ECOREG_use)) +
  geom_line(aes(color=ECOREG_use),size=1)+
  theme_bw()+
  theme(legend.position = c(0.8, 0.8)) +
  #scale_color_manual(values = c("#86BC25","#0076A8","#BBBCBC","#E3E48D","#A0DCFF"))+
  #scale_y_continuous(labels = scales::percent)+
  xlab("Months")+
  ylab("Cumulative precipitation (mm)")

# Boxplot of monthly precip NLA 2017 lakes by Ecoregion
precip_month<-ggplot(precip_melt, aes(x=month, y=value, fill=factor(ECOREG_use)))+
  geom_boxplot() +
  facet_wrap(~ECOREG_use)+
  xlab("Month")+
  ylab("Cumulative precipitation (mm)")


####################
## MEAN TEMPERATURE
#First subset data to columns of interest
Tmean<-dat[c(1,13,45:57)]
head(Tmean)

# Melt dataframe using WRS_ID1 to look at monthly precipitation values
# https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame
Tmean<-setDT(Tmean)
Tmean_melt <- melt(Tmean, id=c("WRS_ID1","ECOREG_use"))

# GRAB MONTH FROM "variable" name
# https://www.datasciencemadesimple.com/extract-substring-of-the-column-in-r-dataframe-2/
Tmean_melt$month <- substr(Tmean_melt$variable, 11,12)
Tmean_melt$year <- substr(Tmean_melt$variable, 7,10)
head(Tmean_melt)

# Boxplot of monthly precip NLA 2017 lakes by Ecoregion
Tmean_month<-ggplot(Tmean_melt, aes(x=month, y=value, fill=factor(ECOREG_use)))+
  geom_boxplot() +
  facet_wrap(~ECOREG_use)+
  xlab("Month")+
  ylab("Mean temperature (degC)")



####################
## TD MEAN ??????? TEMPERATURE
#First subset data to columns of interest - missing October 2016
TDmean<-dat[c(1,13,58:69)]
head(TDmean)

# Melt dataframe using WRS_ID1 to look at monthly precipitation values
# https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame
TDmean<-setDT(TDmean)
TDmean_melt <- melt(TDmean, id=c("WRS_ID1","ECOREG_use"))

# GRAB MONTH FROM "variable" name
# https://www.datasciencemadesimple.com/extract-substring-of-the-column-in-r-dataframe-2/
TDmean_melt$month <- substr(TDmean_melt$variable, 12,13)
TDmean_melt$year <- substr(TDmean_melt$variable, 8,11)
head(TDmean_melt)

# Boxplot of monthly precip NLA 2017 lakes by Ecoregion
TDmean_month<-ggplot(TDmean_melt, aes(x=month, y=value, fill=factor(ECOREG_use)))+
  geom_boxplot() +
  facet_wrap(~ECOREG_use)+
  xlab("Month")+
  ylab("Mean dew point (degC)")


####################
## VAPOR PRESSURE MIN
#First subset data to columns of interest
VPD<-dat[c(1,13,70:82)]
head(VPD)

# Melt dataframe using WRS_ID1 to look at monthly precipitation values
# https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame
VPD<-setDT(VPD)
VPD_melt <- melt(VPD, id=c("WRS_ID1","ECOREG_use"))

# GRAB MONTH FROM "variable" name
# https://www.datasciencemadesimple.com/extract-substring-of-the-column-in-r-dataframe-2/
VPD_melt$month <- substr(VPD_melt$variable, 12,13)
VPD_melt$year <- substr(VPD_melt$variable, 8,11)
head(VPD_melt)

# Boxplot of monthly precip NLA 2017 lakes by Ecoregion
VPD_month<-ggplot(VPD_melt, aes(x=month, y=value, fill=factor(ECOREG_use)))+
  geom_boxplot() +
  facet_wrap(~ECOREG_use)+
  xlab("Month")+
  ylab("Min vapor pressure deficit (hPa)")


####################
## PET
#First subset data to columns of interest
PET<-dat[c(1,13,96:108)]
head(PET)

# Melt dataframe using WRS_ID1 to look at monthly precipitation values
# https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame
PET<-setDT(PET)
PET_melt <- melt(PET, id=c("WRS_ID1","ECOREG_use"))

# GRAB MONTH FROM "variable" name
# https://www.datasciencemadesimple.com/extract-substring-of-the-column-in-r-dataframe-2/
PET_melt$month <- substr(PET_melt$variable, 9,10)
PET_melt$year <- substr(PET_melt$variable, 5,8)
head(PET_melt)

# Boxplot of monthly precip NLA 2017 lakes by Ecoregion
PET_month<-ggplot(PET_melt, aes(x=month, y=value, fill=factor(ECOREG_use)))+
  geom_boxplot() +
  facet_wrap(~ECOREG_use)+
  xlab("Month")+
  ylab("Potential Evapotranspiration")

####################
## RUNOFF for 2015??
#First subset data to columns of interest
RUN<-dat[c(1,13,109:120)]
head(RUN)

# Melt dataframe using WRS_ID1 to look at monthly precipitation values
# https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame
RUN<-setDT(RUN)
RUN_melt <- melt(RUN, id=c("WRS_ID1","ECOREG_use"))

# GRAB MONTH FROM "variable" name
# https://www.datasciencemadesimple.com/extract-substring-of-the-column-in-r-dataframe-2/
RUN_melt$month <- substr(RUN_melt$variable, 9,10)
RUN_melt$year <- substr(RUN_melt$variable, 5,8)
head(RUN_melt)

# Boxplot of monthly precip NLA 2017 lakes by Ecoregion
RUN_month<-ggplot(RUN_melt, aes(x=month, y=value, fill=factor(ECOREG_use)))+
  geom_boxplot() +
  facet_wrap(~ECOREG_use)+
  xlab("Month")+
  ylab("Runoff")


####################
## D18O point
#First subset data to columns of interest
D18O<-dat[c(1,13,121:132)]
head(D18O)

# Melt dataframe using WRS_ID1 to look at monthly precipitation values
# https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame
D18O<-setDT(D18O)
D18O_melt <- melt(D18O, id=c("WRS_ID1","ECOREG_use"))

# GRAB MONTH FROM "variable" name
# https://www.datasciencemadesimple.com/extract-substring-of-the-column-in-r-dataframe-2/
D18O_melt$month <- substr(D18O_melt$variable, 6,7)
#D18O_melt$year <- substr(D18O_melt$variable, 5,8)
head(D18O_melt)

# Boxplot of monthly precip NLA 2017 lakes by Ecoregion
D18O_month<-ggplot(D18O_melt, aes(x=month, y=value, fill=factor(ECOREG_use)))+
  geom_boxplot() +
  facet_wrap(~ECOREG_use)+
  xlab("Month")+
  ylab("Precipitation isotope D18O")

####################
## D2H point
#First subset data to columns of interest
D2H<-dat[c(1,13,133:144)]
head(D2H)

# Melt dataframe using WRS_ID1 to look at monthly precipitation values
# https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame
D2H<-setDT(D2H)
D2H_melt <- melt(D2H, id=c("WRS_ID1","ECOREG_use"))

# GRAB MONTH FROM "variable" name
# https://www.datasciencemadesimple.com/extract-substring-of-the-column-in-r-dataframe-2/
D2H_melt$month <- substr(D2H_melt$variable, 5,6)
#D2H_melt$year <- substr(D2H_melt$variable, 5,8)
head(D2H_melt)

# Boxplot of monthly precip NLA 2017 lakes by Ecoregion
D2H_month<-ggplot(D2H_melt, aes(x=month, y=value, fill=factor(ECOREG_use)))+
  geom_boxplot() +
  facet_wrap(~ECOREG_use)+
  xlab("Month")+
  ylab("d2H precipitation")


############################
## EXPLORATION OF LAKE VOLUME
summary(dat$LAKE_VOL)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#     4345     28181     81426   1918458    244576 514016656         1


plot(dat$AREA_HA~dat$LAKE_VOL)
plot(dat$MAX_DEPTH~dat$LAKE_VOL)
