# app-1.R
# based on adding Shiny features to 
#            Static-Graphs-Covid-Data.R
# Directory (Surface)): 
#  C:/Users/John Bailer/Documents/Classes/STA404class/COVID-App
# Revised: 25 Apr 21
# Author: John Bailer
#
# Implementation Goals:
# 1. add county population sizes for 
#      for 18+ and 16+ as well as total
# 2. built basic Shiny app to select % vac
# 3. extend to allow for selection of 
#      county (app-2)
# 4. extend to allow for plotly interaction
#      (app-2)
# 5. add other plots (app-3a)
#      using tabs (app-3b)
# 6. reactive objects + explore other 
#      themes (app-3c)###
###
### Data Sources
###   https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv
###   https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv
###   https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv
###   https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv
###     (for age-specific levels)
### Data Sets 
###
### OhioDF (used to construct other DF for plots)
###     joins ...
###        OhioSummaryOnset
###        OhioSummaryHosp
###        OhioSummaryDeath
### OhioDateTotalDF - date, ncases, ndead, nhosp
### OhioTotalDF - county, ncases, ndead, nhosp
### OhioCountyDF
###      County date ncases ndead nhosp 
### OhioVacCumul - county, date, Vaccines
###                started, completed, cumulative
### OhioPop - County, Pop2019 {for rate calc}
###

# load libraries 
library(tidyverse)
library(lubridate)
library(tidyquant)
#library(plotly)
library(Shiny)

###
### load in and clean COVID dataset .......................................................................
###

OhioCovid <- read_csv(
  file="https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
  col_types = "cccccciii")

#names(OhioCovid)

OhioCovid <- OhioCovid %>%
  filter(Sex != "Total") %>%
  mutate(AgeFactor = factor(`Age Range`),
         OnsetDate = ymd(`Onset Date`),
         HospDate = ymd(`Admission Date`),
         DeathDate = ymd(`Date Of Death`))

OhioSummaryOnset <- OhioCovid %>%
  group_by(OnsetDate,County,Sex,AgeFactor) %>%
  summarize(`Case Count` = sum(`Case Count`, na.rm=TRUE)) %>%
  filter(`Case Count` > 0) %>%
  mutate(date = OnsetDate)

OhioSummaryHosp <- OhioCovid %>%
  group_by(HospDate,County,Sex,AgeFactor) %>%
  summarize(`Hospitalized Count` = sum(`Hospitalized Count`,
                                       na.rm=TRUE))  %>%
  mutate(date = HospDate)

OhioSummaryDeath <- OhioCovid %>%
  group_by(DeathDate,County,Sex,AgeFactor) %>%
  summarize(`Death Due To Illness Count - County Of Residence` =
              sum(`Death Due To Illness Count - County Of Residence`,
                  na.rm=TRUE)) %>%
  filter(`Death Due To Illness Count - County Of Residence` > 0) %>%
  mutate(date = DeathDate)

# filter HospDate < first OnsetDate -
#    values as small as 1963 observed!

minOnset <- min(OhioSummaryOnset$OnsetDate, na.rm=TRUE)

OhioSummaryHosp <- OhioSummaryHosp %>%
  filter(HospDate >= minOnset)

#
# put everything back together

OhioDF <- OhioSummaryOnset %>%
  left_join(OhioSummaryHosp) %>%
  left_join(OhioSummaryDeath)

# Ohio Totals by date ...
OhioDateTotalDF <- OhioDF %>%   #### was OhioTotalDF <- OhioDF %>%
  group_by(date) %>%
  summarize(ncases = sum(`Case Count`),
            #            ndead = sum(`Death Due to Illness Count`),
            ndead = sum(`Death Due To Illness Count - County Of Residence`, na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`, na.rm=TRUE))

#used for total DF, no unknowns removed
# input DF and name of death variable changed [09 apr 2021]
OhioTotalDF <- OhioDF %>%   #### was OhioTotalDF <- OhioDF %>%
  group_by(County) %>%
  summarize(ncases = sum(`Case Count`),
            #            ndead = sum(`Death Due to Illness Count`),
            ndead = sum(`Death Due To Illness Count - County Of Residence`, na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`, na.rm=TRUE))

# used for County specific plots
OhioCountyDF <- OhioDF %>%   #### was OhioTotalDF <- OhioDF %>%
  group_by(County, date) %>%
  summarize(ncases = sum(`Case Count`),
            #            ndead = sum(`Death Due to Illness Count`),
            ndead = sum(`Death Due To Illness Count - County Of Residence`,na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,na.rm=TRUE))

###
### load in and clean population dataset ........................................................................
###

OhioCountyPop <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
OhioPop <- OhioCountyPop %>%
  filter(STNAME == "Ohio") %>%
  filter(CTYNAME != "Ohio") %>%
  mutate(County = str_remove_all(CTYNAME," County"),
         Pop2019 = POPESTIMATE2019) %>%
  select(County, Pop2019)

# adult population in Ohio counties?
# REF: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-agesex.pdf
# YEAR = 12 <-> July 1, 2019 estimate
# AGE16PLUS_TOT Total population age 16 years and over
# AGE18PLUS_TOT Total population age 18 years and over

OhioAgeCountyPop <- 
  read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv") %>% 
  filter(YEAR==12) %>% 
  select(CTYNAME, POPESTIMATE, 
         AGE16PLUS_TOT,AGE18PLUS_TOT)
  


# dim(OhioCountyPop)
# dim(OhioPop)
#saveRDS(OhioPop, file="OhioPop.RDS")



###
### READ the Ohio Vaccine Data  .......................................................................
###      Based on: Ohio-vaccine-13apr2021.R
#

URL <- "https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv"

OhioVac <- read_csv(file=URL)

#str(OhioVac)

OhioVac <- OhioVac %>%
  mutate(Date = ymd(date))

#str(OhioVac)

# add Cumulative Counts ...................
OhioVacCumul <- OhioVac %>%
  group_by(county) %>%
  arrange(Date) %>%
  mutate(cumulVacStart = cumsum(vaccines_started),
         cumulVacCompl = cumsum(vaccines_completed))

###
### Static plots - develop before Shiny
###
sum(OhioDateTotalDF$ncases)

# basic plot
ggplot(data=OhioDateTotalDF,
       aes(x=date,y=ncases)) +
  geom_col(width=1)

# add MA smoother
ggplot(data=OhioDateTotalDF,
       aes(x=date,y=nhosp)) +
  geom_col(width=1) +
  geom_ma(n=7)

# restrict date range
ggplot(data=OhioDateTotalDF,
       aes(x=date,y=ndead)) +
  geom_col(width=1) +
  geom_ma(n=7) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d",
               limits = c(ymd("2020-04-01"),
                          TODAY()))
# clean up
#   line type, width, color for MA
#   don't expand scale
#   lighten bars
ggplot(data=OhioDateTotalDF,
       aes(x=date,y=ncases)) +
  geom_col(fill="grey") +
  geom_ma(n=7, linetype=1, size=1.25,
          color="blue") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d",
               limits = c(ymd("2020-04-01"),
                          TODAY())) +
  coord_cartesian(expand=FALSE)

#
# percentage of county vaccinated plot
#
#  OhioVacCumul
#  OhioPop

names(OhioVacCumul)
names(OhioPop)

OHVacRate <- merge(OhioVacCumul, 
      OhioPop,
      by.x="county",
      by.y="County")

OHVacRate <- OHVacRate %>% 
  mutate(pct_vac_start = cumulVacStart/Pop2019*100,
         pct_vac_completed = cumulVacCompl/Pop2019*100)

# test with Butler Co
OHVacRate %>% 
  filter(county == "Butler") %>% 
  ggplot(aes(x=date)) +
    geom_line(aes(y=pct_vac_start)) +
    geom_line(aes(y=pct_vac_completed)) 
  
ggplot(OHVacRate,
       aes(x=date,
           y=pct_vac_completed,
           group=county)) +
  geom_line() 

ggplot() +
  geom_line(data=OHVacRate,
            aes(x=date,
                y=pct_vac_completed,
                group=county),
            color="grey") +
  geom_line(data=filter(OHVacRate,
                        county=="Butler"),
            aes(x=date,
                y=pct_vac_completed,
                group=county),
            color="blue")


###
### other plots?
###

# OhioCountyDF)
# [1] "County" "date"   "ncases" "ndead"  "nhosp" 
# 
# ggplot(filter(OhioCountyDF, County=="Butler"),

ggplot(OhioCountyDF,
       aes(x=date, y=ncases, group=County)) +
  geom_ma(n=7) +
  theme(legend.position = "none")

###
### still need to add rates
###
