#  STA308-sim-HT-example.R
#  Suppose we wanted to estimate 
#     Pr(height of randomly selected women >=
#           height of randomly selected women  )
#
#
# Where do you start?
#   How about some data from the CDC/NCHS?
#
# REF: https://www.cdc.gov/nchs/fastats/body-measurements.htm
#   age 20-39 y.o. in US in 2015-16 (Table 4)
#      men:   69.3 (SE = 0.1)  based on n=884 (Table 1 Technical notes)
#    women:   64.0 (SE = 0.2)  based on n=907 (unweighted)


#
# inefficient with looping
# 

p1start <- proc.time()
numTaller <- 0
for (person in 1:2000) {
  htF <- rnorm(n=1, mean=64.0, sd=0.2*sqrt(907))
  htM <- rnorm(n=1, mean=69.3, sd=0.1*sqrt(884))
  numTaller <- ifelse(htF >= htM, numTaller+1, numTaller)
}
numTaller/2000
p1stop <- proc.time()
execution1 <- p1stop - p1start
execution1

#
# more efficient with vectorized calculations
#

set.seed(20211023)
p2start <- proc.time()
menHT   <- rnorm(n=2000, mean=69.3, sd=0.1*sqrt(884))
womenHT <- rnorm(n=2000, mean=64.0, sd=0.2*sqrt(907))

mean(womenHT >= menHT)  # 
p2stop <- proc.time()
execution2 <- p2stop - p2start
execution2


execution1/execution2
## 16.5x more elapsed time with looping
##  5.0x more system time
## 29.0x more user time


#
# using tidyverse tools faster than looping
#   but a little slower (user, elapsed times) than vectorized calc
#   

library(tidyverse)
p3start <- proc.time()
data.frame(menHT   = rnorm(n=2000, mean=69.3, sd=0.1*sqrt(884)),
           womenHT = rnorm(n=2000, mean=64.0, sd=0.2*sqrt(907))) %>% 
  summarize(PrWHtGEMHt = mean(womenHT>=menHT))
p3stop <- proc.time()
execution3 <- p3stop - p3start
execution3