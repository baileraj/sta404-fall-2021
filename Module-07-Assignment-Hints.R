# Module-07-Assignment-Hints.R
# 28 March 2021

# Preparing data set for plotting cases/1000 in population for the last 30 days involves some data management work . . . 
# Download Ohio COVID Data (DF1)
#   + Filter for last 30 days
#   + Summarize for case counts
#
# Download Ohio County Population Estimates (DF2)
#    + Filter to keep county name and popn estimate
#    + merge / dplyr::inner_join (DF12)
#
# Make sure county names are same - you may need string function
# stringr::str_to_lower
# Stringr::str_remove_all  - 
#   e.g. str_remove_all(“Butler County”,” County”)
# Create CaseRate variable (mutate)
# Could consider a categorical variable for rates
# Merge Covid case-Pop data (DF12) with the Ohio County map data (DF3)
# 

# Ohio COVID Data
# Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv

# libraries ...........
library(tidyverse)
library(lubridate)

# OHIO county pop sizes
# Source: https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv

popnDF <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")

names(popnDF)
# variables ...
#     STNAME
#     CTYNAME
#     POPESTIMATE2019

OhioPopnDF <- popnDF %>% 
  filter(STNAME == "Ohio") %>% 
  filter(CTYNAME != "Ohio") %>% 
  select(CTYNAME, POPESTIMATE2019)

dim(OhioPopnDF)


#
# Reading the COVID case data from Ohio ................
#

OhioCovidData2 <- read_csv(
  file="https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
  col_names = c("County","Sex","AgeRange","OnsetDate","HospDate",
                "DeathDate","CaseCount","HospCount",
                "DeathCount"),
  col_types = "cccDDDiii",
  skip=1)  # start reading at second line

str(OhioCovidData2)
dim(OhioCovidData2)


# remove 'unknown'
OhioCovidData2 <- OhioCovidData2 %>% 
  filter(County != "Unknown")

OhioTotalCase2 <- OhioCovidData2 %>% 
  group_by(County) %>% 
  summarize(Cases = sum(CaseCount))

OhioTotalHosp2<- OhioCovidData2 %>% 
  group_by(County) %>% 
  summarize(Hosps = sum(HospCount))

OhioTotalDeath2 <- OhioCovidData2 %>% 
  group_by(County) %>% 
  summarize(Deaths = sum(DeathCount))

OhioTotal2 <-OhioTotalCase2 %>% 
  inner_join(OhioTotalHosp2) %>% 
  inner_join(OhioTotalDeath2) 

dim(OhioTotal2)


# combine data from counts and then from population estimates

unique(OhioTotal2$County)
unique(OhioPopnDF$CTYNAME)

str_remove(OhioPopnDF$CTYNAME," County")

OhioPopnDF <- OhioPopnDF %>% 
  mutate(County = str_remove(CTYNAME," County"))

head(OhioTotal2)
head(OhioPopnDF)

OhioCountyData <- merge(OhioTotal2, OhioPopnDF)
head(OhioCountyData)

OhioCountyData <- OhioCountyData %>% 
  mutate(CaseRate1K = Cases/POPESTIMATE2019*1000)

head(OhioCountyData)

# but there is overall ... how do you restrict to last 30 days

# 2. Generate a choropleth map of Ohio and shade each county with the rate 
# (# / 1000 residents)  of COVID cases over the last 30 days.


names(OhioCovidData2)

today()-30

OhioCovidData2 <- OhioCovidData2 %>% 
  filter(County != "Unknown")

Ohiolast30 <- OhioCovidData2 %>% 
  filter(OnsetDate >= (today()-30) ) %>% 
  group_by(County) %>% 
  summarize(Cases = sum(CaseCount))

Ohiolast30Rate <- Ohiolast30 %>%
  inner_join(OhioPopnDF) %>% 
  mutate(CaseRate1K = Cases/POPESTIMATE2019*1000)


# -----------------------
# Other code
# ------------------------

# load packages:
library(maps)             # Chang 13.17
library(ggplot2)
library(ggmap)            # note citation('ggmap') if you use it
library(mapproj)
library(ggthemes)

# library(rgdal) # other packages of possible interest for  mapping
# library(rgeos) #
# library(maptools)
# library(tmap)  #

# CONSTRUCTING BASIC MAP ==============================================
# maps built into ggplot2
#  global (world, world2), country (usa, nz, ...),  USA (county, state)

states_map <- map_data("state")
str(states_map)
head(states_map)
unique(states_map$region)

# longitude :=  east/west direction (relative to Greenwich, England)
# latitude  :=  north/south direction (relative to the equator)

ohio_map <- subset(states_map, states_map$region=="ohio")
unique(ohio_map$region)
str(ohio_map)
summary(ohio_map)


ggplot(ohio_map, aes(x=long,y=lat))+
  geom_point()



mw_map <- map_data("state", region=c("ohio","kentucky","indiana",
                                     "michigan","west virginia",
                                     "pennsylvania"))
ggplot(mw_map, aes(x=long,y=lat,group=group,fill=region))+
  geom_polygon(fill="white", colour="black")

ggplot()+
  geom_polygon(data=mw_map, aes(x=long,y=lat,
                                group=group,fill=region),
               fill="white", colour="black") +
  geom_point(data=oxford_df,aes(x=long,y=lat)) +
  annotate(geom="text",x=-84.7452,y=39.5070,
           label="Oxford",adj=0, color="blue") +
  coord_map() +       # map projection correction
  theme_nothing()


# Cincinnati 
# 39.1031° N, 84.5120° W
# 41.4993° N, 81.6944° W clevand
# Columbus
# 39.9612° N, 82.9988° W


ohio_city_DF <- data.frame(
  city = c("Cincinnati", "Cleveland", "Columbus"),
  long = c(-84.512, -81.6944, -82.9988),
  lat  = c(39.1031,  41.4993,  39.9612)
)

ggplot()+
  geom_polygon(data=ohio_map, 
               aes(x=long, y=lat, group=group), 
               fill="white", colour="black") +
  geom_text(data=ohio_city_DF, 
            aes(x=long, y=lat, label=city)) +
  coord_map() +       # map projection correction
  theme_nothing()

ggplot(grad_map, aes(x=long,y=lat, group=group, fill=rate)) +
  geom_polygon(colour="black") +
  scale_fill_gradient2(low="#559999",mid="grey90",high="#BB650B",
                       midpoint=median(college.grad$rate)) +
  coord_map("polyconic") +
  labs(fill="College Grads %\n(ACS 2015)") +
  theme_clean()





###############
###############  Question about ordering counties for dashboard
###############

# consider https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards
# Key metrics for each county summarized in 3 horizontal bar graphs
#       with order determined by cases

# load required packages ............
library(tidyverse)
library(lubridate)
#library(plotly)

# download the Ohio COVID data ......

##
## OhioDF <- read_csv(file="https://coronavirus.ohio.gov/static/Dashboard/COVIDSummaryData.csv")
OhioDF <- read_csv(
  file="https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
  col_types = "cccDDDiii")
TODAY <- Sys.Date()


# 
OhioDF <- OhioDF %>% 
  filter(Sex != "Total") %>% 
  mutate(AgeFactor = factor(`Age Range`))

# data frame with counts by County ......
OhioCountySums <- OhioDF %>% 
  group_by(County) %>% 
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due To Illness Count - County Of Residence`,
                        na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,
                        na.rm=TRUE))
View(OhioCountySums)

# plotting
GC <- ggplot(OhioCountySums,
             aes(y=reorder(County, ncases),
                 x=ncases)) +
  geom_col()
GC

GD <- ggplot(OhioCountySums,
             aes(y=reorder(County, ncases),
                 x=ndead)) +
  geom_col()
GD

GH <- ggplot(OhioCountySums,
             aes(y=reorder(County, ncases),
                 x=nhosp)) +
  geom_col()
GH

library(patchwork)
GC | GH | GD 

# fill in categories vs. continuously ...

summary(OhioCountySums$ncases)

table(cut(OhioCountySums$ncases,
          breaks = c(0, 1000, 2500, 5000, 
                     15000, 30000, 500000 )))

OhioCountySums <- OhioCountySums %>% 
  mutate(CaseCat = cut(ncases,
                       breaks = c(0, 1000, 2500, 5000, 
                                  15000, 30000, 500000 )))

#

ggplot(OhioCountySums,
       aes(y=reorder(County, ncases),
           x=ncases,
           fill=CaseCat)) +
  geom_col() +
  scale_fill_brewer() +
  scale_x_continuous(expand=c(0,0)) +
  guides(fill="none")

