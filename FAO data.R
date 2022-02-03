setwd ("C:/Users/Apetrei-Admin/Documents/!Local Only/pers/FAO project/My Docs/data analysis")

### file to do some graphs for the Food Systems Report Romania
### v.2, Oct 2021


##clear environment
rm(list=ls())

## import packages

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(FAOSTAT)
library(stringr)

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

#FAO2 bc FAOSTAT package not working
landuse_FAO2<-read.csv("Environment_LandUse_E_All_Data/Environment_LandUse_E_All_Data_NOFLAG.csv")

landuse_FAO2<-as.data.frame(landuse_FAO)

landuse_FAO2<-landuse_FAO %>%
  gather(Year, Value, starts_with("Y")) %>%
  mutate (Year = sub("Y", "", Year))

landuse_FAO2<-as.data.frame(
  landuse_FAO2 %>%
    filter (Area=="Romania")
)

landuse_FAO2$Value<-as.numeric(landuse_FAO2$Value)
landuse_FAO2$Year<-as.numeric(landuse_FAO2$Year)

landuse_FAO<-landuse_FAO2
#end of FAO2

#agricultural land as share in land area over time
landuse_FAO %>%
  filter (Item == "Agricultural land", Element=="Share in Land area", Year>=2010) %>%
  mutate (Value = round(Value, digits=2)) %>%
  ggplot (aes(x=Year, y=Value, label=Value)) + 
  geom_line() + 
  ggtitle("Share of Agricultural land in Total land area (%)") + 
  geom_label() +
  scale_x_continuous(breaks=seq(2010,2020,by=1))+
  geom_smooth(method="lm")


#visualize also what percentages of land area are for crops vs. pastures 
landuse_FAO %>%
  filter (Item %in% c("Cropland", "Land under perm. meadows and pastures"), Element=="Share in Land area", Year>=2010) %>%
  mutate (Value = round(Value, digits=2)) %>%
  ggplot (aes(fill=Item, x=Year, y=Value, label=Value)) + 
  geom_bar(position="stack", stat = "identity") + 
  ggtitle("Share of Agricultural land in Total land area (%)") + 
  geom_label() +
  scale_x_continuous(breaks=seq(2010,2020,by=1))


#remaking calculations with RL land use parent data, so that I can calculate share relatively to total surface, not just land area


landuse_FAO_RL<- read_excel("FAOSTAT_data_10-27-2021_RL_landuse_parent.xlsx")
landuse_FAO_RL2<- landuse_FAO_RL %>%
  filter (Item %in% c("Agricultural land", "Cropland", "Arable land", "Land under permanent crops", "Land under perm. meadows and pastures", "Forest land", "Other land", "Inland waters")) %>%
  mutate ("Shares in total country area" = round(`Value` / 23008 * 100, digits=3))


landuse_FAO_RL2 %>%
  filter (Item =="Agricultural land") %>%
  mutate ("Shares in total country area" = round(`Shares in total country area`, digits=1)) %>%
  ggplot (aes(x=Year, y=`Shares in total country area`, label=`Shares in total country area`)) + 
  geom_line() +
  ggtitle("Share of Agricultural land in Total country area (%)") + 
  geom_label()+
  scale_x_continuous(breaks=seq(2010,2020,by=1))+
  geom_smooth(method="lm")



### ancheta structurala

#exploatatii dupa statut si dimensiune (numar si suprafata SAU)

holdings1<- read_excel("Tab4-S-vol1 CA.xls", sheet="Tab5-s no", skip=3)
holdings1<-holdings1[-15, ]

holdings2<- read_excel("Tab4-S-vol1 CA.xls", sheet="Tab5-s area", skip=3)
holdings2<-holdings2[-15, ]

holdings1_long<-holdings1 %>%
  gather(Holding_area, Value_count, 3:15)

holdings2_long<-holdings2 %>%
  gather(Holding_area, Value_area, 3:15)

holdings<-cbind(holdings1_long, "Value_area" = holdings2_long$Value_area)
holdings<-holdings %>%
  mutate (Holding_area = ifelse(Holding_area == "Total (number)", "Total", Holding_area)) 

holdings$Holding_area<-as.factor(holdings$Holding_area)
holdings$Holding_area <- factor(holdings$Holding_area, levels = c("<0.1", "0.1-0.3", "0.3-0.5", "0.5-1", "1-2", "2-5", "5-10", "10-20", "20-30", "30-50", "50-100", ">100", "Total"))
holdings$Value_area<-round(as.numeric(holdings$Value_area), digits=2)
holdings$Value_count<-as.numeric(holdings$Value_count)


total_no_holdings<-holdings[182, "Value_count"]
total_area_holdings<-holdings[182, "Value_area"]

options(scipen=7)



holdings_nototal<-holdings %>%
  filter(Category != "TOTAL") %>%
  filter(Status == "Total") %>%
  filter (Holding_area != "Total")

#calculate scaling factor for 2 axes in plot
sf<-max(holdings_nototal$Value_area)/max(holdings_nototal$Value_count)
sf<-round(sf, digits=0)

library(gridExtra)

g_area<-holdings_nototal%>%
 mutate(Share_count = Value_count/total_no_holdings)%>%
 gather(Indicator, Value, c("Value_area", "Value_count")) %>%
 mutate(Value=Value/1000)%>%
  mutate(Indicator=ifelse(Indicator=="Value_area", "Total utilised agricultural area (1000 ha)", "Number of holdings (x1000)"))%>%
 group_by(Holding_area, Indicator) %>%
 summarise_at(c("Value","Share_count"), list(Sum = sum)) %>%  
 filter(Indicator=="Total utilised agricultural area (1000 ha)")%>% 
  ggplot (aes(x=Holding_area,y=Value_Sum, fill=Indicator)) +
          geom_bar(position = "dodge", stat="identity", show.legend=FALSE) +
          scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE),
                             limits=c(0,6500), breaks=seq(0, 6000, 1000)) +
          xlab("\nHolding size (ha)")+
          ylab("Area (1000 ha)\n")+
          scale_fill_manual(values = "#1d6996")+
          facet_wrap(~Indicator)+
          geom_text(aes(label = round(Value_Sum, 1)), vjust = -0.4)
          #ggtitle("Total utilized agricultural area by holding size")
         

g_counts<-holdings_nototal%>%
  mutate(Share_count = Value_count/total_no_holdings)%>%
  gather(Indicator, Value, c("Value_area", "Value_count")) %>%
  mutate(Value=Value/1000)%>%
  mutate(Indicator=ifelse(Indicator=="Value_area", "Total utilised agricultural area (1000 ha)", "Number of holdings (x1000)"))%>%
  group_by(Holding_area, Indicator) %>%
  summarise_at(c("Value","Share_count"), list(Sum = sum)) %>%  
  filter(Indicator=="Number of holdings (x1000)")%>% 
  ggplot (aes(x=Holding_area, y=Value_Sum, fill=Indicator)) +
  geom_bar(position = "dodge", stat="identity", show.legend=FALSE) +
  scale_y_continuous(labels=function(x) paste0("   ",x),
                     limits= c(0,750), breaks=seq(0, 700, 100)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Count (x1000)\n")+
  scale_fill_manual(values = "#5f4690")+
  facet_wrap(~Indicator)+
  geom_text(aes(label = round(Value_Sum, 1)), vjust = -0.4)+
  ggtitle("2016")

#gA = ggplotGrob(g_area)
#gB = ggplotGrob(g_counts)
#gB$widths <-gA$widths

grid.arrange(g_counts, g_area, nrow = 2)

##########

#Most exported plant crops (previously in Excel)

subgroup<-as.factor(c("Wheat and products", "Maize and products", "Barley and products", "Rice and products",
            "Cereals, Other", "Sorghum and products", "Sunflower seed", "Rape and Mustardseed",
            "Soyabeans", "Oilcrops, Other"))
group_e<-c(rep("Cereals", times=6), rep("Oilcrops", times=4))
value_e<-c(49.7, 38.7, 11.1, 0.2, 0.2, 0.1, 54.4, 41.3, 4.2, 0.1)

df_exports<-data.frame(subgroup, group_e, value_e)
df_exports$group_e<-as.factor(df_exports$group_e)


df_exports %>%
  ggplot (aes(y=value_e, x= reorder(subgroup, -value_e, sum), fill=group_e))+
  geom_bar(position="dodge", stat="identity", show.legend = FALSE) +
  ylab("Share in total exports")+
  #scale_fill_manual(values = "#5f4690")+
  facet_wrap(~group_e, dir="v", scales="free")+
 geom_text(aes(label = round(value_e, 1)), vjust = -0.4)+
  ggtitle("2018")+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 5), limits=c("Wheat and products", "Maize and products", "Barley and products", "Rice and products",
                                                                        # "Cereals, Other", "Sorghum and products", "Oats", "Sunflower seed", "Rape and Mustardseed",
                                                                        # "Soyabeans", "Oilcrops, Other")) 
  scale_fill_manual(values = c("#5f4690", "#1d6996"))+
  xlab("Crops")+
  scale_y_continuous(limits= c(0,60), breaks=seq(0, 60, 10)) 



### livestock no

livestock<- read_excel("faostat livestock no.xls")

library(tidyverse)
library(ggrepel)

livestock2<-livestock %>% filter (`Item` %in% c("Sheep", "Pigs", "Cattle", "Goats", "Horses", "Chickens"))
livestock2$Item<-factor(livestock2$Item, levels=c("Sheep", "Pigs", "Cattle", "Goats", "Horses", "Chickens"))

data_ends <- livestock2 %>% 
  mutate (Value= ifelse(`Item`=="Chickens", Value/10, Value/1000)) %>%
  filter(Year == 2019)

myfacet_names <- c(
  `Sheep` = "Sheep (x 1,000)",
  `Pigs` = "Pigs (x 1,000)",
  `Cattle` = "Cattle (x 1,000)",
  `Goats` = "Goats (x 1,000)",
  `Horses`= "Horses (x 1,000)",
  `Chickens`="Chickens (x 10,000)"
)
  


livestock2 %>%
  mutate (Value= ifelse(`Item`=="Chickens", Value/10, Value/1000)) %>%
  ggplot (aes(y=Value, x= Year, fill=Item)) +
  geom_bar(position="dodge", stat="identity", show.legend = FALSE) +
  ylab("Head counts (x1000)")+
  #scale_fill_manual(values = "#5f4690")+
  facet_wrap(~Item, labeller = as_labeller(myfacet_names))+
  geom_label(aes(label = round(Value, 0)), data=data_ends, show.legend = FALSE)+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 5), limits=c("Wheat and products", "Maize and products", "Barley and products", "Rice and products",
  # "Cereals, Other", "Sorghum and products", "Oats", "Sunflower seed", "Rape and Mustardseed",
  # "Soyabeans", "Oilcrops, Other")) 
  scale_fill_manual(values = c("#5f4690", "#8774ac", "#1d6996", "#568fb0", "#38a6a5", "#6abcbc"))+
  xlab("Year")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE),
                     limits=c(0,10500), breaks=seq(0, 10500, 1000))+
  scale_x_continuous(breaks=seq(2010,2020,by=1))
  #scale_y_continuous(limits= c(0,60), breaks=seq(0, 60, 10)) 

