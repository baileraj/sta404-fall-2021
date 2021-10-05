##
## sta404-hw04-Solution-Notes.R
##

OhioCovid <- read_csv(
  file="https://coronavirus.ohio.gov/static/dashboards/COVIDDeathData_CountyOfResidence.csv",
  col_types = "cccccciii")

names(OhioCovid)

OhioCovid %>%
  group_by(`Age Range`) %>% 
  summarize(TotalCases = sum(`Case Count`))

##
##
##

unique(OhioCovid$`Onset Date`)

OhioCovid <- OhioCovid %>% 
  mutate(OnsetDate = as.Date(`Onset Date`),
         monthOnset = months(OnsetDate),
         yearOnset = year(OnsetDate)) %>% 
  filter(!is.na(OnsetDate)) %>% 
  mutate(monthYear = factor(
           x=paste(monthOnset,yearOnset),
           levels = 
             paste(c("January", "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", "December",
                     "January", "February", "March", "April", "May", "June", 
                     "July", "August", "September"),
                   c(rep(c("2020", "2021"), c(12, 9)))
             )
  )
  )

           
             