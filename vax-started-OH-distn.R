## vax-started-OH-distn.R
## 13 sep 21

library(tidyverse)

###################################################################
## Ohio Vaccination data
##https://coronavirus.ohio.gov/wps/portal/gov/covid-19/dashboards/covid-19-vaccine/covid-19-vaccination-dashboard

## COMMENT: different web site for vaccine dashboard BUT csv located in same location as covid case data
##          (put tool tip / cursor over download link and you can see file)
vaxDF <- read_csv(paste0(OhioURL,"vaccine_data.csv"))

View(vaxDF)

## Task: construct a table with with counties ordered by the number of vaccines given
## SOLUTION:

vaxCountyDF <- vaxDF %>% 
  group_by(county) %>% 
  summarize(vaxStarted = sum(vaccines_started),
            vaxCompleted = sum(vaccines_completed))

vaxCountyDF %>% 
  arrange(desc(vaxStarted))

dim(vaxCountyDF)   # should be 88 counties!

vaxCountyDF <- vaxCountyDF %>%
  filter(county != "Out of State") %>%
  filter(county != "Unknown")

dim(vaxCountyDF)

View(OhioCountyDF)


## What is true about these counties?
##   
## How can we put them on a common scale?

###################################################################
## Ohio County Population data

OhioPop <- 
  read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-39.csv") 

# use the most recent population estimate - YEAR == 12
# don't need all of these variables - only need CTYNAME and POPESTIMATE
#    
OhioCountyPop <- OhioPop %>% 
  filter(YEAR == 12) %>% 
  select(CTYNAME, POPESTIMATE) 
dim(OhioCountyPop)

## COMMENT: we want to merge the data frames
##          need a common *key* - variable that can be used to connect observations in the two data frames
## County works

unique(vaxCountyDF$county)
unique(OhioCountyPop$CTYNAME)

## need to strip ' County'
OhioCountyPop <- OhioCountyPop %>% 
  mutate(county = str_remove(CTYNAME, " County")) %>% 
  select(-CTYNAME)
dim(OhioCountyPop)   
View(OhioCountyPop)


## now we can put the two data sets together 
dim(vaxCountyDF)


VaxCombo <- merge(vaxCountyDF, OhioCountyPop)
dim(VaxCombo)
head(VaxCombo)
## COMMENT: can merge w/o specifying key since 'county' is common in both variables
## COMMENT: also could do an inner join in dplyr

## Question:  what county has the largest proportion of its population started on vaccinations?

VaxCombo <- VaxCombo %>% 
  mutate(PctOneVax = 100*vaxStarted/POPESTIMATE)
dim(VaxCombo)

VaxCombo %>% 
  arrange(desc(PctOneVax))

## Distribution of % started across Ohio counties?
ggplot(VaxCombo, aes(x=PctOneVax)) +
  geom_histogram(binwidth = 2.5)

