# sta404-inset-graph-outline-SOLUTION.R
# 01 Nov 2021
# add inset with map of USA where this region is highlighted
#
# from: sta404-R-intro-spatial-data-display-BLANK.R
# /home/baileraj/sta404-fall2021
# UPDATED: 29 Oct 2021 / 28 Mar 2021 / 17 Oct 2020

# need to install packages:  ggmap, mapproj

# load packages:
library(maps)             # Chang 13.17
library(ggplot2)
library(ggthemes)
library(tidyverse)

# CONSTRUCTING BASIC MAP ==============================================
# maps built into ggplot2
#  global (world, world2), country (usa, nz, ...),  USA (county, state)
# longitude :=  east/west direction (relative to Greenwich, England)
# latitude  :=  north/south direction (relative to the equator)

## start with World map
world_map <- map_data("world")
str(world_map)

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon() 

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon() +
  theme_map()

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon() +
  coord_map("polyconic") +
  theme_map()

##
## shade USA
##

unique(world_map$region)  # so USA = United States

world_map <- world_map %>% 
  mutate(indicatorUSA = 
           ifelse(region == "USA",
                  "USA","not"))

ggWORLD <- ggplot(world_map, aes(x=long, y=lat, 
                                 group=group,
                                 fill = indicatorUSA)) +
  geom_polygon() +
  scale_fill_manual(values = c("grey","black")) +
  coord_map("polyconic") +
  theme_void() +
  theme(legend.position = "none")


## start with a map of the US (continental USA) .................
states_map <- map_data("state")
unique(states_map$region)

## shade states in the midwest
## first define variablestates_map <- states_map %>% 
states_map <- states_map %>% 
  mutate(indicatorMW = 
           ifelse(
             region %in% c("ohio","kentucky","indiana",
                           "michigan","west virginia",
                           "pennsylvania"),
             "MW","not"))

## draw map with fill
ggUSA <- ggplot(states_map, aes(x=long, y=lat, 
                                group=group,
                                fill = indicatorMW)) +
  geom_polygon() +
  scale_fill_manual(values = c("black","grey")) +
  coord_map("polyconic") +
  theme_void() +
  theme(legend.position = "none")


## Now map of the midwest
## REF: https://statisticsglobe.com/change-background-color-of-ggplot2-plot-in-r
##      ( changing the background )

## college grad updated - from factfinder part of census
#
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_S1501&prodType=table
# updated CPS data reported in
#    https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_educational_attainment

mw_map <- map_data("state", region=c("ohio","kentucky","indiana",
                                     "michigan","west virginia",
                                     "pennsylvania"))

college.grad <- data.frame(region=c("ohio","kentucky","indiana",
                                    "michigan","west virginia",
                                    "pennsylvania"),
                           rate=c(27.2, 23.2, 25.3, 28.1, 19.9, 30.1))

grad_map <- merge(mw_map, college.grad, 
                  by.x="region", 
                  by.y="region")

ggMW <- ggplot(grad_map, aes(x=long,y=lat,group=group,
                             fill=rate))+
  geom_polygon(colour="black") +
  coord_map() +       # map projection correction
  theme_void() +
  theme(legend.position = "bottom")
ggMW

##
## want: plot of Midwest with USA/world next
## 

library(patchwork)
ggMW + ggUSA + ggWORLD

# want:  MW map on left and world/USA stacked on right

ggMW | (ggWORLD / ggUSA)


(ggMW | (ggWORLD / ggUSA)) + plot_layout(ncol=2,
                                         widths=c(2,1))

##
## add the inset
##

ggMW + inset_element(ggUSA, .5, .5, 1, 1)

