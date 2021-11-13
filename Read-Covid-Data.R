# Read-Covid-Data.R

# load libraries 
library(tidyverse)
library(lubridate)
library(plotly)

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

# names(OhioCovid)
# str(OhioCovid)

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

# Debugging .....
# View(OhioSummaryOnset)
# View(OhioSummaryHosp)
# View(OhioSummaryDeath)

# filter HospDate < first OnsetDate -
#    values as small as 1963 observed!
# approx 8 dates removed [09 apr 2021]

minOnset <- min(OhioSummaryOnset$OnsetDate, na.rm=TRUE)
#minOnset

OhioSummaryHosp <- OhioSummaryHosp %>%
  filter(HospDate >= minOnset)

#
# put everything back together

OhioDF <- OhioSummaryOnset %>%
  left_join(OhioSummaryHosp) %>%
  left_join(OhioSummaryDeath)


#used for total DF, no unknowns removed
# input DF and name of death variable changed [09 apr 2021]
OhioTotalDF <- OhioCovid %>%   #### was OhioTotalDF <- OhioDF %>%
  group_by(County) %>%
  summarize(ncases = sum(`Case Count`),
            #            ndead = sum(`Death Due to Illness Count`),
            ndead = sum(`Death Due To Illness Count - County Of Residence`),
            nhosp = sum(`Hospitalized Count`))

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

###
### READ the Ohio Vaccine Data  .......................................................................
###      Based on: Ohio-vaccine-13apr2021.R
#

URL <- "https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv"

OhioVac <- read_csv(file=URL)

str(OhioVac)

OhioVac <- OhioVac %>%
  mutate(Date = mdy(date))

str(OhioVac)

# add Cumulative Counts ...................
OhioVacCumul <- OhioVac %>%
  group_by(county) %>%
  arrange(Date) %>%
  mutate(cumulVacStart = cumsum(vaccines_started),
         cumulVacCompl = cumsum(vaccines_completed))

###
### Put them all together ...
###



