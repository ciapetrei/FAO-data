setwd ("C:/Users/Apetrei-Admin/Documents/!Local Only/pers/FAO project/My Docs/data analysis")

### file to do some graphs for the Food Systems Report Romania
### v.1, June 2021


##clear environment
rm(list=ls())

## import packages

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(FAOSTAT)

prec<-read.csv("WB Climate Knowledge precipitation pr_1901_2020_ROU.csv")
temp<-read.csv("WB Climate Knowledge Temperatures tas_1901_2020_ROU.csv")

dashRO<- read_excel("BSEC INDICATORS per country 19.4.21.xlsx", sheet = "ROMANIA")
str(dashRO) 
summary(dashRO)

#remove white spaces in Statistics strings
prec$Statistics<-as.factor(trimws(prec$Statistics))
temp$Statistics<-as.factor(trimws(temp$Statistics))

#rename columns
colnames(prec)[1]<-"Rainfall"
colnames(temp)[1]<-"Temp"


#FAOSTAT data
#create folder to store data
data_folder <- "FAO_raw"
dir.create(data_folder)


#create dataframe with metadata about all datasets
fao_metadata <-FAOsearch()
#create dataframew with metadata World Bank
wb_metadata<-getWDImetaData 


#Romania codes
FAOcountryProfile[FAOcountryProfile$OFFICIAL_FAO_NAME=="Romania",]

#search dataset with title containing "temp" (from temperature)
FAOsearch(dataset="temp", full = FALSE)

temp_FAO<-get_faostat_bulk(code = "ET", data_folder = data_folder)
str(temp_FAO)

#save object COMPLETE temp for later reuse
saveRDS(temp_FAO, "data_raw/temp_FAO_all_data.rds")
temp_FAO <- readRDS("data_raw/temp_FAO_all_data.rds")


#make data set for Greece and Moldova
temp_FAO_GR<-temp_FAO %>% filter (area == "Greece")
temp_FAO_MD<-temp_FAO %>% filter (area == "Republic of Moldova")

#retain only Ro data in original variable
temp_FAO<- temp_FAO %>% filter (area=="Romania")






########### PRECIPITATION

#summarize and plot average precipitation / month/ decade
prec_dec <- as.data.frame(
  prec %>%
  mutate(Decade = floor(Year/10)*10) %>% 
  group_by(Decade) %>% 
  summarise(mean_annual_prec_dec = mean(Rainfall))
)

ggplot(prec_dec, aes(x=Decade, y=mean_annual_prec_dec))+
  geom_line()+ geom_smooth(method="lm")

# plot means/month by year
prec %>%
  group_by(Year) %>%
  summarise(mean_annual_prec = mean(Rainfall)) %>%
  ggplot(aes(x=Year, y=mean_annual_prec))+ geom_line()+
    geom_smooth(method="lm")


# plot means by year (Add all precp in one year and calculate mean per year)
prec %>%
  group_by(Year) %>%
  summarise(sum_annual_prec = sum(Rainfall)) %>%
  ggplot(aes(x=Year, y=sum_annual_prec))+ geom_line()+
  geom_smooth(method="lm")


#plot lines for Dec, Jan, Feb, Jun, JUl, Aug  
prec %>% 
  filter(Statistics %in% c("Dec Average", "Jan Average", "Feb Average", "Jun Average", "Jul Average","Aug Average")) %>%
  ggplot(aes(x=Year, y=Rainfall, group=Statistics, color=Statistics)) +
  geom_line() + 
  geom_smooth(method="lm")


### PROCESS DATA BY SEASON

sum_months<-c("Jun Average", "Jul Average", "Aug Average")
win_months<-c("Dec Average", "Jan Average", "Feb Average")

#make data frame with averages for summer and winter (AS SUMS!!!)
prec_seasons<-as.data.frame(
  prec %>%
  filter (Statistics %in% sum_months) %>%
  group_by(Year) %>%
  summarise(Summer = mean(Rainfall)))

prec_win<- as.data.frame(
  prec %>%
  filter (Statistics %in% win_months) %>%
  group_by(Year) %>%
  summarise(Winter = mean(Rainfall))
)
prec_seasons<-cbind(prec_seasons, "Winter"=prec_win$Winter)


#make wide data long, then plot prec per season all years
prec_seasons_long<-as.data.frame(
  prec_seasons %>%
  gather(Season, Rainfall, -Year)
)

#plot by season FINAL
prec_seasons_long  %>%
  ggplot(aes(x=Year, y=Rainfall, group=Season, color=Season)) +
    geom_line() +
    geom_smooth(method="lm")+
    ggtitle("Mean rainfall (mm/month) for summer (Jun-Jul-Aug) and winter (Dec-Jan-Feb) months")


########################

#Temperatures

#summarize and plot average temperature / decade
temp_dec <- as.data.frame(
  temp %>%
    mutate(Decade = floor(Year/10)*10) %>% 
    group_by(Decade) %>% 
    summarise(mean_annual_temp_dec = mean(Temp))
)


ggplot(temp_dec, aes(x=Decade, y=mean_annual_temp_dec))+
  geom_line()+ geom_smooth(method="lm")

#calculate change from one decade to another and average change per decade # WRONG because no reference to climatological baseline.
temp_dec_ch<- as.data.frame(
  temp_dec %>% 
    mutate(Change = mean_annual_temp_dec - lag(mean_annual_temp_dec))
)



# TEMP change FAO (Is the value in the raw data a change relative to reference period average before 1960?? 
#YES see http://www.fao.org/faostat/en/#data/ET/metadata)

temp_FAO %>% 
  filter(months=="Meteorological year", element=="Temperature change", year>=2010) %>%
  ggplot(aes(x=year, y=value))+ geom_line() + geom_smooth(method="lm") +
       scale_x_continuous(breaks=seq(2010,2019,by=1))+
       ggtitle("Temperature change per meteorological year 2010-2020")

temp_FAO %>% 
  filter(months=="Meteorological year", element=="Temperature change", year>=2010) %>%
  summarise(avg=mean(value))

##TEMP change FAO GREECE and MOLDOVA
temp_FAO_GR %>% 
  filter(months=="Meteorological year", element=="Temperature change", year>=2010) %>%
  ggplot(aes(x=year, y=value))+ geom_line() + geom_smooth(method="lm") +
  scale_x_continuous(breaks=seq(2010,2019,by=1))+
  ggtitle("Greece - Temperature change per meteorological year 2010-2020")

temp_FAO_GR %>% 
  filter(months=="Meteorological year", element=="Temperature change", year>=2010) %>%
  write.table (file = "GR_temp_change_FAOdata.csv")

temp_FAO_GR %>%   
  filter(months=="Meteorological year", element=="Temperature change", year>=2010) %>%
  summarise(avg=mean(value))

temp_FAO_MD %>% 
  filter(months=="Meteorological year", element=="Temperature change", year>=2010) %>%
  ggplot(aes(x=year, y=value))+ geom_line() + geom_smooth(method="lm") +
  scale_x_continuous(breaks=seq(2010,2019,by=1))+
  ggtitle("Republic of Moldova - Temperature change per meteorological year 2010-2020")

temp_FAO_MD %>% 
  filter(months=="Meteorological year", element=="Temperature change", year>=2010) %>%
  write.table (file = "MD_temp_change_FAOdata.csv")

temp_FAO_MD %>% 
  filter(months=="Meteorological year", element=="Temperature change", year>=2010) %>%
  summarise(avg=mean(value))



################

FAOsearch(dataset="emissions", full=TRUE)

em_shares_FAO<-get_faostat_bulk(code = "EM", data_folder = data_folder)
str(em_shares_FAO)

#only RO
em_shares_FAO<- em_shares_FAO %>% filter (area=="Romania")

str(em_shares_FAO)
unique(em_shares_FAO$element)


par(mfrow=c(2,1))


em_shares_FAO %>%
  filter (element=="Share of total emissions (SAR)", 
          item %in% c("Agriculture total", "Agricultural land use", "Energy", "Industrial processes and product use", "Waste", "Other n.e.c.")) %>%
  ggplot(aes(x=year, y=value, fill=item)) + geom_area() +
    ggtitle("Share of total emissions (SAR)")


em_shares_FAO %>%
  filter (element=="Share of total emissions (SAR)", 
          item %in% c("Agriculture total", "Agricultural land use", "Energy", "Industrial processes and product use", "Waste", "Other n.e.c.")) %>%
  ggplot(aes(x=year, y=value, group=item, fill=item)) + geom_area() +
    facet_wrap(~item)+
    ggtitle("Share of total emissions (SAR) per sector of activity")


em_shares_FAO %>%
  filter (element=="Share of total emissions (SAR)", 
          item %in% c("Agriculture total", "Agricultural land use")) %>%
  ggplot(aes(x=year, y=value, group=item, fill=item)) + geom_area() +
   ggtitle("Share of total emissions (SAR) - agriculture")



############ MACRO ECON INDICATORS
macroind<-get_faostat_bulk(code= "MK", data_folder = data_folder)

RO_GDP <- as.data.frame(
  macroind %>% 
    filter (area == "Romania", item == "Gross Domestic Product")
)



RO_GDP %>%
  filter (element=="Annual growth US$, 2015 prices", year>=2010) %>%
  mutate (value = round(value, digits=2)) %>%
  ggplot (aes(x=year, y=value, label=value)) + geom_line() +
    scale_x_continuous(breaks=seq(2010,2019,by=1))+
    #geom_text(aes(label=value), hjust=1, vjust=1, color="blue") +
    geom_label()+
    ggtitle("Romania annual GDP growth in US%, 2015 prices")


RO_GDP %>%
  filter (element=="Annual growth US$", year>=2010) %>%
  mutate (value = round(value, digits=2)) %>%
  ggplot (aes(x=year, y=value, label=value)) + geom_line() +
  scale_x_continuous(breaks=seq(2010,2019,by=1))+
  #geom_text(aes(label=value), hjust=1, vjust=1, color="blue") +
  geom_label()+
  ggtitle("Romania annual GDP growth in US%")

RO_GDP %>%
  filter (element=="Value US$ per capita, 2015 prices", year>=2000) %>%
  mutate (value = round(value, digits=2)) %>%
  ggplot (aes(x=year, y=value, label=value)) + geom_line() +
  scale_x_continuous(breaks=seq(2000,2019,by=1))+
  #geom_text(aes(label=value), hjust=1, vjust=1, color="blue") +
  geom_label()+
  ggtitle("Romania annual GDP value per capita in US%, 2015 prices")

RO_GDP %>%
  filter (element=="Value US$ per capita", year>=2010) %>%
  mutate (value = round(value, digits=2)) %>%
  ggplot (aes(x=year, y=value, label=value)) + geom_line() +
  scale_x_continuous(breaks=seq(2000,2019,by=1))+
  #geom_text(aes(label=value), hjust=1, vjust=1, color="blue") +
  geom_label()+
  ggtitle("Romania annual GDP value per capita in US%, 2010-2019 data")


#Mean annual GDP growth in the last decade
RO_GDP %>%
  filter (element=="Annual growth US$, 2015 prices", year>=2011) %>%
  summarize (mean(value))



library(WDI)
WDI_metadata<-WDI_data

#dashboard DATA

dashRO$TimePeriod<- as.numeric(dashRO$TimePeriod)

dashRO %>%
  filter (Indicator == "Long-term average annual precipitation") %>%
  ggplot (aes(x=TimePeriod, y=DataValue)) + geom_line() + ggtitle("Long-term average annual precipitation")

dashRO %>%
  filter (Indicator == "Population growth (annual)", TimePeriod>=1989) %>%
  mutate (DataValue = round(DataValue, digits=2)) %>%
  ggplot (aes(x=TimePeriod, y=DataValue, label=DataValue)) + 
  geom_line() + 
  ggtitle("Population growth, annual (%)") + 
  geom_label() +
  scale_x_continuous(breaks=seq(1990,2020,by=2))

########## LANDUSE FAO
FAOsearch(dataset="land use", full=FALSE)

landuse_FAO<-get_faostat_bulk(code = "EL", data_folder = data_folder)
str(landuse_FAO)

landuse_FAO<-as.data.frame(
  landuse_FAO %>%
    filter (area=="Romania")
)

landuse_FAO %>%
  filter (item == "Agricultural land", element=="Share in Land area", year>=2010) %>%
  mutate (value = round(value, digits=2)) %>%
  ggplot (aes(x=year, y=value, label=value)) + 
  geom_line() + 
  ggtitle("Share of Agricultural land in Total land area (%)") + 
  geom_label() +
  scale_x_continuous(breaks=seq(2010,2020,by=1))