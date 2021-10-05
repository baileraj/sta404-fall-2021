# Ohio-COVID-plot.R
# Updated: 07 March 2021
# 
# created: 27 Sept 2020

# Ohio COVID dashboard
# https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards/overview
# CSV 
#      https://coronavirus.ohio.gov/static/COVIDSummaryData.csv
# Data Dictionary
#   https://coronavirus.ohio.gov/static/docs/COVID-19-Data-Term-Definitions.pdf
#     
# https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards/overview

# BC Health District - general web site + recent report
# REF: http://health.bcohio.us/
# REF: http://health.bcohio.us/CovidReportButlerCounty_09212020.pdf

library(tidyverse)
OhioDF <- read_csv(file = "https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv")


head(OhioDF)
tail(OhioDF)
glimpse(OhioDF)

# remove Total from the Ohio CSV file with covid cases ....
OhioDF <- OhioDF %>% 
  filter(Sex != "Total")

tail(OhioDF)

# need to load lubridate library for date functions it contains ...
library(lubridate)
OhioDF <- OhioDF %>% 
  mutate(AgeFactor = factor(`Age Range`),
         OnsetDate = mdy(`Onset Date`))

# save information about the day I downloaded and ran this

TODAY <- Sys.Date()
TODAY

View(OhioDF)

# produce analog of Figure 1 in the Butler County report
# need to accumulate case counts over ages and sex for each date

OhioCountyDF <- OhioDF %>% 
  group_by(County, OnsetDate) %>% 
  summarize(ncases = sum(`Case Count`),
            ndead = sum(`Death Due to Illness Count`, na.rm=TRUE))

dim(OhioDF)
dim(OhioCountyDF)

BC_DF <- OhioCountyDF %>% 
  filter(County == "Butler")
unique(BC_DF$County)
head(BC_DF)

totalCases <- sum(BC_DF$ncases)
totalCases
totalDead <- sum(BC_DF$ndead)
firstCase <- min(BC_DF$OnsetDate)
lastCase <- max(BC_DF$OnsetDate)

totalDead
firstCase
lastCase

ggplot(BC_DF, aes(x=OnsetDate, y=ncases)) +
  geom_col() +
  labs(x = "Onset Date",
       y = "Number of New Cases",
       title = "Butler County Case Counts",
       subtitle = paste("Updated: ",TODAY),
       source = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
  annotate(geom="text", 
           x=date("2020-02-27"),
           y = 125,
           label = paste("Total Cases: ", totalCases), 
           hjust=0) +
  theme_minimal()

# - added annotations ..............................

GGFig1 <- ggplot(BC_DF, aes(x=OnsetDate, y=ncases)) +
  geom_col() +
  labs(x = "Onset Date",
       y = "Number of New Cases",
       title = "Butler County Case Counts",
       subtitle = paste("Updated: ",TODAY),
       source = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
  annotate(geom="text", 
           x=date("2020-02-27"),
           y = 125,
           label = paste("Total Cases: ", totalCases), 
           hjust=0) +
  annotate(geom="text", x=date("2020-02-27"), y=420, 
           label=paste("Deaths:",totalDead), hjust=0) +
  annotate(geom="text", x=date("2020-02-27"), y=370, 
           label=paste("First Case Reported:",firstCase), hjust=0) +
  annotate(geom="text", x=date("2020-02-27"), y=320, 
           label=paste("Last Case Reported:",lastCase), hjust=0) +
  theme_minimal() 

GGFig1

format(Sys.time(), "%a %b %d %X %Y %Z")

library(tidyquant)
GGFig1 +
  geom_ma(n=7, linetype=1, color="blue", size=1.25) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d") 

OhioBC <- OhioCountyDF %>% 
  filter(County == "Butler") %>% 
  arrange(OnsetDate) %>% 
  mutate(CumulCase = cumsum(ncases))

ggplot(OhioBC, aes(x=OnsetDate, y=CumulCase)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b %d") +
  labs(x="Onset Date", y="Case Count",
       title="Figure 2. Cumulative Cases Reported to Butler County by Date Reported",
       subtitle=paste("Updated: ",TODAY),
       caption="Source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv") +
  theme_minimal()


# theme_few()
# theme_tufte()
# theme_wsj()
