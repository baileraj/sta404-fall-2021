# Static-Graphs-Covid-Data.R
# Revised: 21 Apr 21
###
###
### Data Sources
###   https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv
###   https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv
###   https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv
###
###
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

# can check if dates in one set but not another
### dates in Onset but not in Hosp
#setdiff(OhioSummaryOnset$date, OhioSummaryHosp$date)
#setdiff(OhioSummaryHosp$date, OhioSummaryOnset$date)

### dates in Onset but not in Hosp
#setdiff(OhioSummaryOnset$date, OhioSummaryDeath$date)
#setdiff(OhioSummaryDeath$date, OhioSummaryOnset$date)




# head(OhioDF)
# View(OhioDF)

# Ohio Totals by date ...
OhioDateTotalDF <- OhioDF %>%   #### was OhioTotalDF <- OhioDF %>%
  group_by(date) %>%
  summarize(ncases = sum(`Case Count`),
            #            ndead = sum(`Death Due to Illness Count`),
            ndead = sum(`Death Due To Illness Count - County Of Residence`,na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,na.rm=TRUE))

#used for total DF, no unknowns removed
# input DF and name of death variable changed [09 apr 2021]
OhioTotalDF <- OhioDF %>%   #### was OhioTotalDF <- OhioDF %>%
  group_by(County) %>%
  summarize(ncases = sum(`Case Count`),
            #            ndead = sum(`Death Due to Illness Count`),
            ndead = sum(`Death Due To Illness Count - County Of Residence`,na.rm=TRUE),
            nhosp = sum(`Hospitalized Count`,na.rm=TRUE))

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
  mutate(Date = mdy(date))

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

# basic plot
ggplot(data=OhioDateTotalDF,
       aes(x=date,y=ncases)) +
  geom_col(width=1)

# add MA smoother
ggplot(data=OhioDateTotalDF,
       aes(x=date,y=ndead)) +
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
  geom_col(fill="lightgrey") +
  geom_ma(n=7, linetype=1, size=1.25,
          color="blue") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d",
               limits = c(ymd("2020-04-01"),
                          TODAY())) +
  coord_cartesian(expand=FALSE)

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
