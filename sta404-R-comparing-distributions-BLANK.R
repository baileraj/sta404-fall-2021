# sta404-R-comparing-distributions-BLANK.R
# revised: 29 Aug 2021 (history: 31 Jan 2021, 22 August 2020)
# 
# Comment: Strategy for exploring new methods, whether visual or
#          modeling, is to simulate a variable with a known characteristics
#          and then to see if your methods recover these known traits

# save work in class subdirectory
# SERVER:   /home/baileraj/sta404
#           /opt/courses/sta404-2020-fall (created by B. Koby - group:R access)
# MacBook:  /Users/baileraj/Desktop/Classes/sta404-fall-2021

# What is a distribution?
#    What are some of the key / salient features of a distribution?
#
# EXAMPLES: How might you describe the distribution of? 
#
# a. Salaries in a large company?
#
# b. Times for the bus to go from the heart of campus to Walmart?
#
# c. Heights of entering students?



# loading packages
library("tidyverse")   # includes ggplot2, dplyr and other tools
library("gapminder")   # dataset with country, may need to install this if it is not available

# create test data set for exploration ===============================
# if not interested in the details, go to line #62
# 4 different groups are being being generated

# bimodal ....................................................
 
vec1 <- c(rnorm(n=400,mean=54,sd=10),rnorm(n=200,mean=79,sd=8))
mean(vec1)
median(vec1)
sd(vec1)

# What is the appearance of this distribution?
# vec1 <- c(rnorm(n=400,mean=54,sd=10),rnorm(n=200,mean=79,sd=8))
xxx <- seq(from=20, to=100, length=1000)
y1 <- dnorm(x=xxx, mean=54,sd=10)
y2 <- dnorm(x=xxx, mean=79,sd=8)
ex1.df <- data.frame(xtest=xxx, y1=y1, y2=y2, ytest =4/6*y1 + 2/6*y2)
ggplot() +
  geom_line(data=ex1.df, aes(x=xtest, y=ytest)) +
  geom_line(data=ex1.df, aes(x=xtest, y=4/6*y1), color="darkblue") +
  geom_line(data=ex1.df, aes(x=xtest, y=2/6*y2), color="red")


# normal ...................................................

vec2 <- c(rnorm(n=600, mean=62.75275, sd=15.28787))
mean(vec2)
median(vec2)
sd(vec2)

# Technical aside (ignore unless you are interested):  
#                   Gamma variate generation with particular mean/sd
#                   requires reparameterization
#
# mean = a*s = 62.75275 => a= 62.75275/s
# var = a*s^2 = 15.28787^2  =>  (62.75275/s)*s^2 = 15.28787^2
#                     =>  62.75275*s = 15.28787^2
#                     =>  s = 15.28787^2  / 62.75275
#                     =>  s = 3.724442
#                     =>  a (shape) = 62.75275 / 3.724442 = 16.8489

set.seed(8675309)
vec3 <- rgamma(n=600, shape = 16.8489, scale=3.724442)
mean(vec3)
median(vec3)
sd(vec3)

# Technical aside (ignore unless you are interested):  
#        Uniform requires reparameterization too
# (b+a)/2 = 62.75275
# (b-a)^2/12 = 15.28787^2
#
# b+a = 125.5055 => b = 125.5055 - a
# (b-a)^2/12 = (125.5055 - a -a)^2/12 = 15.28787^2
#           (125.5055 - 2 a)^2 = 2804.628
#             125.5055 - 2a = 59.95874
#                  2a = 65.54676
#                   a = 32.77338
#                   b = 125.5055 - a = 125.5055 - 32.77338 = 92.73212
#

vec4 <- runif(n=600, min=32.77338, max=92.73212)
mean(vec4)
median(vec4)
sd(vec4)

myDF <- data.frame(group=rep(c(1,2,3,4), c(600,600,600,600)),
                   response = c(vec1, vec2, vec3, vec4))

myDF <- myDF %>% 
  mutate(fgroup=factor(group, 
                       labels = c("bimodal", "normal", "gamma", "uniform")))

str(myDF)

# dyanmite plot =========================================================

# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
# https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
# Beware of the dynamite plot <- AVOID!!! 
#   http://biostat.mc.vanderbilt.edu/wiki/pub/Main/TatsukiRcode/Poster3.pdf

sumstatDF <- myDF %>% 
  group_by(fgroup) %>% 
  summarise(ybar=mean(response),sdev=sd(response))

sumstatDF

sumstatDF %>% 
  ggplot(aes(x=fgroup, y=ybar, color=fgroup)) +
  geom_col()

# what does 'color' do as an aesthetic?
# how does it compare to 'fill'?

sumstatDF %>% 
  ggplot(aes(x=fgroup, y=ybar, fill=fgroup)) +
  geom_col()

# what if you map 'fill' aesthetic to a numeric variable
#         versus a factor?
sumstatDF %>% 
  ggplot(aes(x=fgroup, y=ybar, fill=fgroup)) +
  geom_col()


# NOTE:
#    data frames can be identified in the ggplot() or in levels added

ggplot(data=sumstatDF, aes(x=fgroup, y=ybar, fill=fgroup)) + 
  geom_col() +
  geom_errorbar(aes(ymin=ybar, ymax=ybar+sdev, col=fgroup),
                width=.2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# geom_col == geom_bar(stat="identity")

ggplot(data=sumstatDF, aes(x=fgroup, y=ybar, fill=fgroup)) + 
  geom_col() +
  geom_errorbar(aes(ymin=ybar, ymax=ybar+sdev, col=fgroup),
                width=.2) +
  labs(x="", y="Mean (with SD)") +
  guides(fill=FALSE, color=FALSE)     # one way to remove legend
  

# boxplots =====================================================

ggplot(data=myDF, aes(x=fgroup, y=response, fill=fgroup)) + 
  geom_boxplot() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


ggplot(data=myDF, aes(x=fgroup, y=response, fill=fgroup)) + 
  geom_boxplot() +
  labs(x="", y="Mean (with SD)") +
  guides(fill=FALSE, color=FALSE)

# can add data points
ggplot(data=myDF, aes(x=fgroup, y=response, color=fgroup)) + 
  geom_boxplot(size=1.25) +
  geom_jitter(alpha=.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# histograms ========================================================

# look at 'bimodal' case to start .....................
# compare different # of bins
myDF %>% 
  filter(group==1) %>% 
  ggplot(aes(x=response)) +
    geom_histogram(bins=40)


## 
## Before reading further, IN CLASS .......
##
## look at other options for # bins - say 10, 20, 80
## preference?








myDF %>% 
  filter(group==1) %>% 
  ggplot(aes(x=response)) +
  geom_histogram(bins=80)

myDF %>% 
  filter(group==1) %>% 
  ggplot(aes(x=response)) +
  geom_histogram(bins=20)

# What's going on underneath the hood? ...................
# 


# can use binwidth as another option ......
myDF %>% 
  filter(group==1) %>% 
  ggplot(aes(x=response)) +
  geom_histogram(binwidth = 10)
# TRY THIS NOW: experiment with
#               different binwidths ... what do you prefer?


# comparing all of the distributions .....................
ggplot() + geom_histogram(data=myDF, 
                          aes(x=response, color=fgroup, fill=fgroup),
                          binwidth=1)
# what's happening here?

ggplot() + geom_histogram(data=myDF, 
                          aes(x=response, color=fgroup, fill=fgroup),
                          position="dodge",binwidth=1)

ggplot() + geom_histogram(data=myDF, 
                          aes(x=response, color=fgroup, fill=fgroup),
                          position="dodge",binwidth=1,
                          alpha=.2)

# small multiple comparison of the distributions
ggplot() + geom_histogram(data=myDF, 
                          aes(x=response, color=fgroup, fill=fgroup),
                          binwidth=1) +
  facet_grid(fgroup ~ .)

# density plots =====================================================

# let's look at the density plot for bimodal case
myDF %>% 
  filter(group==1) %>% 
  ggplot(aes(x=response)) +
  geom_density()

myDF %>% 
  filter(group==1) %>% 
  ggplot(aes(x=response)) +
  geom_density(adjust=1/2)    # use 1/2 of default bandwidth

myDF %>% 
  filter(group==1) %>% 
  ggplot(aes(x=response)) +
  geom_density(adjust=2)    # use 2x of default bandwidth


# comparing with 'truth'
myDF %>% 
  filter(group==1) %>% 
  ggplot(aes(x=response)) +
  geom_density()
ggplot() +
  geom_density(data=filter(myDF,group==1), 
               aes(x=response),
               size=1.5) +
  geom_line(data=ex1.df, aes(x=xtest, y=ytest), 
            color="blue",
            size=1.5) +
  theme_minimal()



# let's look at all of the groups now ............................

ggplot() + geom_density(data=myDF, 
                        aes(x=response, color=fgroup, fill=fgroup))

ggplot() + geom_density(data=myDF, 
                        aes(x=response, color=fgroup, fill=fgroup),
                        alpha = .4)

ggplot() + geom_density(data=myDF, 
                        aes(x=response, color=fgroup, fill=fgroup)) +
  facet_grid(fgroup ~ .)







# how about both histogram and density?
# Chang 6.3
# first convert from frequency to relative frequency / density
ggplot(myDF) + 
  geom_histogram(aes(x=response, y=..density..),binwidth = 2) +
  facet_grid(fgroup ~ .)

ggplot(myDF) + 
  geom_histogram(aes(x=response, y=..density..),binwidth=2) +
  geom_density(aes(x=response)) +
  facet_grid(fgroup ~ .)

# what happens if you only do the color= aesthetic?
# what happens when you add the fill=  aesthetic?

ggplot(myDF, aes(color=fgroup)) + 
  geom_histogram(aes(x=response, y=..density..,fill=fgroup),alpha=.5) +
  geom_density(aes(x=response)) +
  facet_grid(fgroup ~ .)

# violin plots are an interesting option =============================
# Chang 6.9

ggplot(myDF, aes(x=fgroup, y=response, color=fgroup)) +
  geom_violin() +
  guides(color=FALSE )

ggplot(myDF, aes(x=fgroup, y=response, color=fgroup)) +
  geom_violin() +
  geom_jitter(alpha=0.4) +
  guides(color=FALSE)

#
# can you be led astray?
#     exploring your reasoning from different visualizations ...
#


test.df <- data.frame(group=rep(1:5,c(rep(100,5))), 
                      resp=rnorm(500),
                      resp2 = c(rnorm(100,mean=2),
                                rnorm(100,sd=sqrt(2)),
                                rnorm(300)))
ggplot(test.df,aes(x=as.factor(group),y=resp)) + 
  geom_boxplot()
ggplot(test.df,aes(x=as.factor(group),y=resp2)) + 
  geom_boxplot()

#
#  Suppose we wanted to explore the height distribution question
#

# REF: https://www.cdc.gov/nchs/fastats/body-measurements.htm
# age 20-39 y.o. in US in 2015-16 (Table 4)
# men:   69.3 (SE = 0.1) based on n=884 (Table 1 Technical notes)
# women: 64.0 (SE= 0.2)  based on n=907 (unweighted)

set.seed(20210203)
menHT   <- rnorm(n=2000, mean=69.3, sd=0.1*sqrt(884))
womenHT <- rnorm(n=2000, mean=64.0, sd=0.2*sqrt(907))

HT_DF <- data.frame(height = c(menHT, womenHT), 
                    rgender=rep(c("M","F"),c(2000,2000)))

mean(HT_DF$height)

HT_DF <- HT_DF %>% 
  mutate(fgroup = factor(ifelse(rgender=="M","Male","Female")))

ggplot(HT_DF) +
  geom_histogram(aes(x=height), binwidth = 0.5)

ggplot(HT_DF) +
  geom_histogram(aes(x=height), binwidth = 0.5) +
  facet_grid(fgroup ~ .)

ggplot(HT_DF) +
  geom_histogram(aes(x=height, fill=rgender), 
                 binwidth = 0.5, alpha=.5) 

ggplot(HT_DF) +
  geom_density(aes(x=height, fill=rgender, color=rgender), 
                alpha=.5) +
  theme_minimal() +
  labs(caption="Data from Table 4 -https://www.cdc.gov/nchs/fastats/body-measurements.htm ")

#
# In class lab exercise - using the gapminder data set
# 

#  How has the average life expectancy changed in Asia from 1957 to
#      1982 to 2007?

library(gapminder)
library(dplyr)


#  1. create a data frame with these 3 years of data and Asia

labDF <- gapminder %>% 
  filter(continent == "Asia") %>% 
  filter(year %in% c(1957, 1982, 2007))

View(labDF)

#  2. generate plot of the data for the three years - add jitter to the plot



#  3. generate a boxplot to compare life expectancy between the 3 years

ggplot(labDF, aes(x=as.factor(year), y=lifeExp)) +
  geom_boxplot(color="grey") +
  geom_jitter(width=.2) +
  theme_minimal()

names(labDF)

ggplot(labDF, aes(x=as.factor(year), y=lifeExp,
                  label=country)) +
  geom_boxplot(color="grey") +
  geom_text() +
  theme_minimal()

install.packages("ggrepel")
library(ggrepel)
ggplot(labDF, aes(x=as.factor(year), y=lifeExp,
                  label=country)) +
  geom_boxplot(color="grey") +
  geom_text_repel() +
  theme_minimal()

ggplot() +
  geom_boxplot(data=labDF, aes(x=as.factor(year), y=lifeExp)) +
  geom_jitter()  

# what happens here?

#  4. generate a violin plot to compare life expectancy distribution changes

#  5. superimpose violin plot on the jittered points

#  6. clean up the plot to make it better for sharing

#  7.  what is your conclusion from your analysis?

# ADDENDUM
# OLD SCHOOL Base R exploration of these ideas
# "Shutting Down the Bars: Options for Displaying and Comparing
#              Data Distributions" 
# http://www.users.miamioh.edu/baileraj/talks/ecolunch-talk-23jan07.pdf
