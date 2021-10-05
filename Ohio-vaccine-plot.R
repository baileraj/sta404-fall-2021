##
## Ohio-vaccine-plot.R
## Revised: 26 Sept 2021
##
## Note:
## Ohio Population: 11,714,618
## https://worldpopulationreview.com/states/ohio-population
## 


library(tidyverse)
URL <- "https://coronavirus.ohio.gov/static/dashboards/vaccine_data.csv"

OhioVac <- read_csv(file=URL)

str(OhioVac)

#  Calculate summary data frame for total cases

OhioVacSummary <- OhioVac %>% 
  group_by(date) %>% 
  summarize(VacStart = sum(vaccines_started),
            VacDone = sum(vaccines_completed)) %>% 
  mutate(VacStartCTot = cumsum(VacStart),
         VacDoneCTot  = cumsum(VacDone))


names(OhioVacSummary)
head(OhioVacSummary)
tail(OhioVacSummary)

minDate <- min(OhioVacSummary$date)

# daily counts ..........................
ggplot(OhioVacSummary) +
  geom_line(aes(x=date, y=VacStart), color="darkgrey") +
  geom_line(aes(x=date, y=VacDone), color="blue") +
  theme_minimal()

# cumulative counts 
ggplot(OhioVacSummary) +
  geom_line(aes(x=date, y=VacStartCTot), color="darkgrey") +
  geom_line(aes(x=date, y=VacDoneCTot), color="blue") +
  theme_minimal()

#
# clean up
#

# Clean up labels ........................
ggplot(OhioVacSummary, aes(x=date)) +
  geom_line(aes(y=VacStartCTot), color="darkgrey") +
  geom_line(aes(y=VacDoneCTot), color="blue") +
  scale_x_date(name="") +
  scale_y_continuous(name="",
                     breaks= c(0,.5,1,1.5)*1000000,
                     labels= c("0","0.5M","1.0M","1.5M")) +
  coord_cartesian(expand = FALSE) +
  theme_minimal()

ggplot(OhioVacSummary, aes(x=date)) +
  geom_line(aes(y=VacStartCTot)) +
  geom_line(aes(y=VacDoneCTot), color="blue") +
  scale_x_date(name="") +
  scale_y_continuous(name="",
                     breaks= 1:6*1000000,
                     labels= paste0(1:6,"M") ) +
  coord_cartesian(ylim = c(0, 7000000),
                  expand = FALSE) +
  annotate("text",
           x=minDate,
           y=6.5*1000000,
           label = paste0("Total Vaccines Administered\n",
                          "[Ohio Population: 11.7M]"),
           hjust=0,
           size=7) +
  annotate("text", 
           x=as.Date("2021-03-15"),
           y=3550000,
           label="Vaccine\nStarted",
           hjust=1) +
  annotate("text", 
           x=as.Date("2021-05-10"),
           y=2300000,
           label="Vaccine\nFinished",
           hjust=1, color="blue") +
  theme_minimal()

##
## Comparing Ohio counties
##

OhioCountyVacSummary <- OhioVac %>% 
  group_by(county, date) %>% 
  summarize(VacStart = sum(vaccines_started),
            VacDone = sum(vaccines_completed)) %>% 
  mutate(VacStartCTot = cumsum(VacStart),
         VacDoneCTot  = cumsum(VacDone))

##
##
##

ggplot(OhioCountyVacSummary, 
       aes(x=date,y=VacStartCTot, group=county)) +
  geom_line(color="grey") +
  geom_line(data=filter(OhioCountyVacSummary,
                        county == "Butler"),
            aes(x=date,y=VacStartCTot),
            color="blue",
            size=1.2) +
  theme_minimal()

##
## Homework: Plot the percentage of each county
##           population that has started vaccination
##


## 

