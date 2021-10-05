# Module 04 Extended Example
# 
# REVISITING: Ohio Labor Day gas data
# redraw Ohio Labor Day gas data
# ref: https://www.eia.gov/todayinenergy/detail.php?id=41133
# https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_w.htm
# https://www.eia.gov/opendata/qb.php?sdid=PET.EMM_EPM0_PTE_SOH_DPG.W

# loading libraries .....
library(tidyverse)


# labor day -- 
#   2020 - 7 sep
#   2019 - 2 sep
#   2018 - 3 sep
#   2017 - 4 sep
#   2016 - 5 sep
#   2015 - 7 sep

years <- 2020:2003

# extract weekly avg for dates before 9 sep but after 1 sep
prices <- c(2.11, 2.61, 2.76, 2.65, 2.26, 2.21, 3.48, 3.67,
            3.89, 3.73, 2.75, 2.46, 3.60, 3.03, 2.47, 3.06,
            1.81, 1.72)

gas_DF <- data.frame(year=rev(years), 
                     price=rev(prices))
gas_DF

gas_DF <- gas_DF %>% 
  mutate(pricediff = price - mean(price),
         pricediffcat = cut(pricediff, 
                            breaks=(seq(from=-1.25,
                                        to = 1.25,
                                        by = .25))),
         diff_ind = ifelse(pricediff>0,"greater","less"))


ggplot(gas_DF, aes(x=year, y=pricediff, fill=pricediffcat)) +
  geom_col() +
  scale_fill_brewer(type="div", palette="PuOr", guide=FALSE) +
  labs(x="Year", y="",
       title="Ohio's Labor Day week average gap prices",
       subtitle="Difference from series average ($2.79)",
       caption="Data source: U.S. Energy Information Admin.") +
  theme_minimal()


# ?scale_fill_brewer 
# 
# Palettes
# The following palettes are available for use with these scales:
#   
#   Diverging
# BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# 
# Qualitative
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
# 
# Sequential
# Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd



# ================================================

# Ohio-COVID-plot.R
# revised: 28 Sept 2020
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


read.csv()

library(tidyverse)
OhioDF <- read_csv(file = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv")

OhioDF2 <- read.csv(file = "https://coronavirus.ohio.gov/static/COVIDSummaryData.csv",
                    header=TRUE)

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

# 30 Sept 2020
# - added annotations

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
  annotate(geom="text", x=date("2020-02-27"), y=120, 
           label=paste("Deaths:",totalDead), hjust=0) +
  annotate(geom="text", x=date("2020-02-27"), y=115, 
           label=paste("First Case Reported:",firstCase), hjust=0) +
  annotate(geom="text", x=date("2020-02-27"), y=110, 
           label=paste("Last Case Reported:",lastCase), hjust=0) +
  theme_minimal() 

GGFig1

format(Sys.time(), "%a %b %d %X %Y %Z")

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



# ALTERNATIVE:  Slope graph 1950 vs 2007

GDPcombo %>% 
  filter(year==1952 | year==2007) %>% 
  ggplot(aes(x=year, y=PropWorldGDP, color=continent)) +
  geom_line(size=2) + 
  theme_minimal()

# 1. Move continent names to graph
# 2. Identify 1952 and 2007 on x-axis
# 3. Check y-axis label

# heat map 
ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  coord_flip()

ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  guides(fill=FALSE) +
  labs(title="Continent Share of World GDP",
       subtitle="[<0.1 (light grey) to >0.4 (black)]",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_fivethirtyeight()





