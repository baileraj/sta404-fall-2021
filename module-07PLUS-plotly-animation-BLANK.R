# module-07PLUS-plotly-animation-BLANK.R
# 07 Nov 2021
#   - modified examples / better tooltip labeling
#   - Palmer Penguins added as foundational example
#   REF: https://plotly.com/ggplot2/
# Apr 2021 updates
#   - tooltip customization + additional class exercises
# Oct 2020 updates
#   - updated with class exercises
#   - updated with refs and docs for DT crosstalk
#   - adding subregion to plotly with gapminder
#
# REF: https://plotly.com/ggplot2/extending-ggplotly/
# REF: C. Sievert (2020) Interactive Web-Based Data
#        Visualization with R, plotly, and shiny
#        CRC Press
#  https://plot.ly/ggplot2/
# "Plotly is an R package for creating interactive web-based graphs via the
#     open source JavaScript graphing library plotly.js. As of version 2.0 
#     (November 17, 2015), Plotly graphs are rendered locally through the 
#     htmlwidgets framework."

# Examples from previous class work ...
# sta404-R-exploring-relationships-BLANK-09sep17.R
# sta404-R-categorical-data-BLANK-14sep17.R

library(dplyr)
library(ggplot2)
library(plotly)


##############################################################
# 
# can add a custom tooltip .....................
#     https://plotly.com/ggplot2/interactive-tooltip/
#     (example modified)
library(gapminder)

p <- ggplot(filter(gapminder, year==2007), 
            aes(x = gdpPercap, y = lifeExp, 
                color = continent, 
                text = paste("country:", country))) +
  geom_point() + 
  scale_x_log10() +
  labs(x="GDP per capita ($)",
       y="Life Expectancy (years)",
       caption="J. Bryan (2017)") +
  theme_minimal()

ggplotly(p)                  # by default ALL aes() mappings shown
#  'including unofficial "text" aesthetic'

ggplotly(p, tooltip="text")  # can select component to display in tool tip

##
## can further customize tooltips through an expanded *text* aes()
##    scales::dollar()  <-> format for currency
##    round(x, digits=) <-> control # decimal places
## REF: https://plotly-r.com/controlling-tooltips.html
##
p1 <- ggplot(filter(gapminder, year==2007), 
            aes(x = gdpPercap, y = lifeExp, 
                color = continent, 
                text = paste("country:", country,"\n",
                             "Continent:", continent,"\n",
                             "GDP:",scales::dollar(gdpPercap),"\n",
                             "Life Expectancy:",round(lifeExp,digits=1)))) +
  geom_point() + 
  scale_x_log10() +
  labs(x="GDP per capita ($)",
       y="Life Expectancy (years)",
       caption="J. Bryan (2017)") +
  theme_minimal()

ggplotly(p1, tooltip="text")

##############################################################
## 
## In-class: produce a display with the Palmer Penguins
##           that can be explored using ggplotly with a
##           customized tooltip
##
## example: Plot Body mass vs. Flipper length 
##############################################################

library(palmerpenguins)
# install.packages("palmerpenguins")

# using a different data set

# a little exploration of the data set ...........
# check out pairs() function

# plotly workflow ....
#    a. build static display
#    b. add ggplotly wrapper with tooltip control
 
# looking at flipper length and body mass ........


# add ggplotly do with tool tip query of values of points
# reminder: tool tip displays variables mapped in aes() by default


# color based on species ........................


##############################################################

# with maps =====================================================

library(maps)             # Chang 13.17
library(ggplot2)
library(ggmap)            # note citation('ggmap') if you use it
library(mapproj)
library(ggthemes)
library(plotly)

college.grad <- data.frame(statename=c("ohio","kentucky","indiana",
                                       "michigan","west virginia",
                                       "pennsylvania"),
                           rate=c(26.1, 22.3, 24.1, 26.9, 19.2, 28.6))

mw_map <- map_data("state", region=c("ohio","kentucky","indiana",
                                     "michigan","west virginia",
                                     "pennsylvania"))

grad_map <- merge(mw_map, 
                  college.grad, 
                  by.x="region", 
                  by.y="statename")

head(grad_map)

gm <- ggplot(grad_map, aes(x=long,y=lat, 
                           group=group, 
                           fill=rate,
                           text=paste(str_to_title(region),"\n",
                                      "College Grads:",rate,"%\n"))) +
  geom_polygon(colour="black") +
  coord_map("polyconic") +
  theme_map()

ggplotly(gm, tooltip="text")

###
### Class Exercise - World Map with countries identified
### from: sta404-inset-graph-outline-SOLUTION.R
###
### Remove Antarctica


library(maps)             # Chang 13.17
library(ggplot2)
library(ggthemes)
library(tidyverse)
world_map <- map_data("world")
str(world_map)

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="red",color="white") +
  coord_map("polyconic") +
  theme_map()

ggworld <- ggplot(filter(world_map, region != "Antarctica"), 
                  aes(x=long, y=lat, 
                      group=group,
                      text=paste(region))) +
  geom_polygon(fill="grey",color="black") +
  theme_map() +
  theme(plot.background = element_rect(fill="lightblue"))

ggworld  # can control bakcground in ggplot using theme
         #   but ggplotly needs different controls 

ggplotly(ggworld, tooltip = "text")


?ggplotly

####################################################################
##
## Playing with gapminder again
##
####################################################################

# example with gapminder ===========================================

library(gapminder)
library(dplyr)
library(ggplot2)
library(plotly)

myGapData <- gapminder %>% 
  mutate(TotalGDP = pop*gdpPercap)

# calculate continent-specific annual GDP ...................
GDPsummaryDF <- myGapData %>% 
  group_by(continent, year) %>% 
  summarise(ContinentTotalGDP = sum(TotalGDP), ncountries = n())

# calculate world annual GDP ................................

GDPyearDF <- myGapData %>% 
  group_by(year) %>% 
  summarise(YearTotalGDP = sum(TotalGDP))

# combine the table with continent annual GDP with the world annual GDP

GDPcombo <- left_join(GDPsummaryDF, GDPyearDF, by="year")

GDPcombo <- GDPcombo %>% mutate(PropWorldGDP = ContinentTotalGDP / YearTotalGDP,
                                PctWorldGDP = 100*PropWorldGDP,
                                ContGDPBillions = ContinentTotalGDP/1000000000)

# extract 2007 data ......................................
gapminder2007 <- GDPcombo %>% 
  filter(year==2007)

# add factor to order continents ..........................
gapminder2007 <- gapminder2007 %>% 
  mutate(order_continent = factor(continent,
                                  levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

GDPcombo <- GDPcombo %>% 
  mutate(order_continent = factor(continent,
                                  levels=c("Oceania", "Africa", "Europe", "Americas", "Asia")))

# STACK this response
ggplot(GDPcombo, aes(x=year,y=ContinentTotalGDP,
                     fill=order_continent)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "top")

plotGDP <- ggplot(GDPcombo, 
                  aes(x=year,
                      y=ContinentTotalGDP,
                      fill=order_continent,
                      text = paste("GDP Total",
                                   dollar(ContinentTotalGDP)))) + 
  geom_col() +
  labs(y="Total GDP") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
  coord_cartesian(expand=FALSE)

plotGDP

ggplotly(plotGDP, tooltip="text")
# note that other controls needed for legend position with ggplotly



###########################################
# IN CLASS / Live Coding  .........
# clean up GDP display
#   - plot GDP in Trillion $ units
###########################################

# 2         1 
# 09876543210987654321
#       10000000000000
#       10,000,000,000,000
# divide by 1 x 10^12

GDPcombo <- GDPcombo %>% 
  mutate(TotalGDPTrill = ContinentTotalGDP/1000000000000,
         TotalGDPTrill2= ContinentTotalGDP/(10^12))


# restricting # of decimal places?

##
## Aside: could also modify the variables in the DF
##        and plot them directly
##

GDPcombo <- GDPcombo %>% 
  mutate(GDP = round(TotalGDPTrill,digits=1),
         Continent = order_continent)

######################################################################################
# Example highlighting time-trend countries
# ================================================================================

library(gapminder)
library(tidyverse)
library(plotly)
library(DT)
library(gganimate)

rwanda_japan <- gapminder %>%
  filter(country %in% c("Rwanda", "Japan"))

ggplot() +
  geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +
  geom_line(data=rwanda_japan, aes(year,lifeExp, color=country), lwd=1.1) +
  theme_minimal() 

# cleaning up annotations
ggplot() +
  geom_line(data=gapminder, aes(x=year, y=lifeExp, group=country), color="lightgrey") +
  geom_line(data=rwanda_japan, aes(year,lifeExp, color=country), lwd=1.1) +
  guides(color="none") +
  labs(x="Year", y="Life Expectancy",
       caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version0.2.0.") +
  annotate("text", x=1985, y=80, label="Japan") +
  annotate("text", x=1985, y=30, label="Rwanda") +
  theme_minimal()

# revisiting the plot from module 1

myplot <- ggplot() +
  geom_line(data=gapminder, 
            aes(x=year, y=lifeExp, 
                group=country,
                text = paste0(country,"\n",
                              "Avg. Life Expectancy: ",
                              round(lifeExp,digits=1))),
            color="lightgrey") +
#  geom_line(data=rwanda_japan, 
            # aes(year,lifeExp, color=country), 
            # lwd=1.1) +
  guides(color="none") +
  labs(x="Year", y="Life Expectancy",
       caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version0.2.0.") +
  # annotate("text", x=1985, y=80, label="Japan") +
  # annotate("text", x=1985, y=30, label="Rwanda") +
  theme_minimal() +
  coord_cartesian(expand=FALSE)

myplot
# what if we could do this interactively?

# start with plotly

ggplotly(myplot, tooltip="text")



# 
# PLOTLY  once again to the rescue!
#
# Packages that are needed:
#    plotly
#    DT
#         A Wrapper of the JavaScript Library 'DataTables'
#         Data objects in R can be rendered as HTML tables using the JavaScript library 'DataTables' 
#         (typically via R Markdown or Shiny). The 'DataTables' 
#         library has been included in this R package. 
#         The package name 'DT' is an abbreviation of 'DataTables'
#         (REF: https://www.rdocumentation.org/packages/DT/versions/0.16)
#    crosstalk
#          Crosstalk is a package for R that enhances 
#          the htmlwidgets package. It extends htmlwidgets 
#          with a set of classes, functions, and conventions
#          for implementing cross-widget interactions (currently, 
#          linked brushing and filtering).
#      bscols == Arrange HTML elements or widgets in Bootstrap columns
#      (REF https://www.rdocumentation.org/packages/crosstalk/versions/1.1.0.1)
#   htmlwidgets: HTML Widgets for R
#      A framework for creating HTML widgets that render in various contexts 
#      including the R console, 'R Markdown' documents, and 'Shiny' web applications.
#      (REF: https://cran.r-project.org/web/packages/htmlwidgets/index.html)
#     other refs: https://www.htmlwidgets.org/
#                
#library(plotly)
ggplotly(myplot, tooltip="text")

# apply example to Gapminder

# create object of class crosstalk::SharedData
m <- highlight_key(gapminder, ~country)

MYplot <- ggplot() +
  geom_line(data=m, 
            aes(x=year, y=lifeExp, 
                group=country,
                text = paste0(country,"\n",
                              "Avg. Life Expectancy: ",
                              round(lifeExp,digits=1))),
            col="lightgrey") +
  labs(x="Year", y="Life Expectancy",
       caption="Source:  Jennifer Bryan (2015). gapminder: Data from Gapminder. R package version0.2.0.") +
  theme_minimal()
gg <- ggplotly(MYplot, 
               tooltip = "text")
highlight(gg, dynamic = TRUE)

# check out what you can do if plots and
# tables are linked 
library(crosstalk)
library(DT)

# possible to add table with values
gg1 <- highlight(gg, dynamic = TRUE)
crosstalk::bscols(gg1, DT::datatable(m))


# apply example to Gapminder
m <- highlight_key(gapminder, ~country)
p <- ggplot(m, 
            aes(x=year, y=lifeExp, 
                group=country)) +
  geom_line(col="lightgrey") +
  theme_minimal()
gg <- ggplotly(p, tooltip = "country")
highlight(gg, dynamic = TRUE)

# possible to add table with values
gg1 <- highlight(gg, dynamic = TRUE)
crosstalk::bscols(gg1, DT::datatable(m))

#
# CLASS EXERCISE 
# 
#
# 1. Read in country-continent data
#    from https://www.kaggle.com/statchaitya/country-to-continent
#   countryContinent.csv on Canvas
# 2. Create new data set by merging with 
#    gapminder to add continent **sub_region**
#    to each country
# 3. Generate a scatterplot for the 2007
#    data with 
#      color = **sub_region**
#      size = sqrt(population)
#      x = log10(income)
#      y = life expectancy
# 4. Wrap this is ggplotly() to query data
#      for countries on the plot

# 0. packages used ...
library(readr)
library(gapminder)
library(dplyr)
library(ggplot2)
library(plotly)


# 1. reading continents
conts <- read_csv("countryContinent.csv")

# 2. merge continent info with gapminder
# check what first?
names(gapminder)
length(unique(gapminder$country))
names(conts)
length(unique(conts$country))

# ................... clean up some mismatches of names
myconts <- conts %>% 
  mutate(ncountry = country) %>% 
  mutate(ncountry = ifelse(
    country == "Yemen", "Yemen, Rep.",
    ifelse(country == "Viet Nam", "Vietnam",
           ifelse(country == "Venezuela (Bolivarian Republic of)","Venezuela", ncountry)
           ))) %>% 
  mutate(ncountry = ifelse(
    country == "United States of America", "United States",
    ifelse(country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom",
           ifelse(country == "Tanzania, United Republic of","Tanzania", ncountry)
    )))  %>% 
  mutate(ncountry = ifelse(
    country == "Taiwan, Province of China", "Taiwan",
    ifelse(country == "Syrian Arab Republic", "Syria",
           ifelse(country == "Slovak Republic","Slovakia", ncountry)
    ))) %>% 
  mutate(ncountry = ifelse(
    country == "Taiwan, Province of China", "Taiwan",
    ifelse(country == "Syrian Arab Republic", "Syria",
           ifelse(country == "Korea (Republic of)", "Korea, Rep.)", ncountry)
    )))  %>% 
  mutate(ncountry = ifelse(
    country == "Korea (Democratic People's Republic of)", "Korea, Dem. Rep.",
    ifelse(country == "Hong Kong, China", "Hong Kong",
           ifelse(country == "Iran (Islamic Republic of)", "Iran", ncountry)
    ))) %>% 
  mutate(ncountry = ifelse(
    country == "Congo (Democratic Republic of the)", "Congo, Dem. Rep.",
           ifelse(country == "Bolivia (Plurinational State of)", "Bolivia", ncountry)
    ))

myconts %>% select(country, ncountry) %>%  View()


# ..............................   now ready for merge

firstMerge <- merge(gapminder, myconts,
      by.x = "country",
      by.y = "ncountry")
length(unique(firstMerge$country))

# ISSUE now only missing 7 countries in gapminder
# hard code solution?
View(firstMerge)

# 3. graph

gGap <- ggplot(
  data=filter(firstMerge, year==2007), 
       aes(x=gdpPercap,
           y=lifeExp,
           size = sqrt(pop),
           shape = continent.x,
           color = sub_region)) +
  geom_point() + 
  scale_x_log10()

gGap

# 4. add plotly

ggplotly(gGap)


# always can do more ...
#
# ISSUE (question from class)
#  a. including other variable in tool tip
#     add 'bogus' aesthetic with variable
#  b. use hide_legend()
#     to remove legend from plotly
#  REF: https://rdrr.io/cran/plotly/man/hide_legend.html
Facet.gGap <- ggplot(
  data=filter(firstMerge, year==2007), 
  aes(x=gdpPercap,
      y=lifeExp,
      size = sqrt(pop),
      color = sub_region,
      cname = country)) +
  geom_point() + 
  scale_x_log10() + 
  facet_wrap( ~ continent.x) 

#  can be used to suppress legend
#  + theme(legend.position='none')

FGPL <- ggplotly(Facet.gGap,
         tooltip = c("country","sub_region",
                     "lifeExp","gdpPercap"))
FGPL
hide_legend(FGPL)

# A N I M A T I O N ..................................
# gganimate
# REF: https://gganimate.com/
# gganimate extends the grammar of graphics as implemented by ggplot2 to include the description of animation. It does this by providing a range of new grammar classes that can be added to the plot object in order to customise how it should change with time.
# 
# transition_*() defines how the data should be spread out and how it relates to itself across time.
# view_*() defines how the positional scales should change along the animation.
# shadow_*() defines how data from other points in time should be presented in the given point in time.
# enter_*()/exit_*() defines how new data should appear and how old data should disappear during the course of the animation.
# ease_aes() defines how different aesthetics should be eased during transitions

# .......................................
# animation can be another interesting 
#           tools

# packages needed:
#    gganimate
#    gifski


library(gapminder)
library(ggplot2)
library(gganimate)
library(gifski)


# Example 2: https://gganimate.com/ .....

library(gapminder)

gapAnim <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year)

gapAnim + ease_aes('linear')

gapAnim + 
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')

gapAnim + ease_aes('linear')

gapAnimSmallDF <- gapminder %>% 
  filter(year %in% c(1952,1962,1972,1982,
                     1992,2002))
gapAnimSmall <- ggplot(gapAnimSmallDF, 
                       aes(gdpPercap, 
                           lifeExp, 
                           size = pop, 
                           colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year)

gapAnimSmall + ease_aes('linear')

# lots of other controls such as shadow trail
# https://gganimate.com/reference/shadow_trail.html

# Change distance between points
gapAnimSmall +
  shadow_trail(0.02)

# Style shadow differently
# gapAnimSmall +
#   shadow_trail(alpha = 0.3, shape = 2)
# 
# Restrict the shadow to 10 frames
# gapAnimSmall +
#   shadow_trail(max_frames = 10)


# NOTE:
#   You can remove files from your 
#   directory using the file.remove() fcn
#   E.g. to delete all files with a pattern
# file.remove(dir(pattern="gganim_plot"))

#
# CLASS EXERCISE
# 
# 1. Create an animation with countries
#    colored by continent
# 2. Try at least one option in gganimate
#    that we haven't explored yet in class


##
## old code .....................
##

ggplotly(p1c)

fev_DF2 <- fev_DF %>% 
  mutate(Gender = ifelse(ind.Male==1,"Male","Female"))

table(fev_DF2$Gender, fev_DF2$ind.Male)

p1c2 <- ggplot(fev_DF2,aes(x=age.yrs, y=ht.in, 
                           color=Gender)) + 
  geom_point(alpha=.1)

ggplotly(p1c2)

str(fev_DF2)

# you can display variables that are not used in the aes() but 
#    using a dummy aes() feature
# REF https://stackoverflow.com/questions/36325154/how-to-choose-variable-to-display-in-tooltip-when-using-ggplotly#36345358
# Note: 
#       similar solution used on map in the COVID data set
#

p1c3 <- ggplot(fev_DF2,aes(x=age.yrs, y=ht.in, 
                           color=Gender,
                           text = paste("Age (years)",age.yrs,
                                        "<br>Height (in)",ht.in,
                                        "<br>FEV1 (L)",fev.L,
                                        "<br>Gender",Gender)))  + 
  geom_point(alpha=.4) +
  scale_color_brewer(type="qual",palette="Set3") +
  labs(x="Age (years)", y="Height (in)") +
  theme_dark()

ggplotly(p1c3, tooltip=c("text"))




