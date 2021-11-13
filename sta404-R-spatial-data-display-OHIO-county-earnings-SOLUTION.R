# sta404-R-intro-spatial-data-display-OHIO-county-earnings-SOLUTION.R
# /home/baileraj/sta404
# UPDATED: 05 Nov 2021 

# US Counties - from "maps" package and ggplot2::map_data
library(tidyverse)
library(ggthemes)

map.county <- map_data('county')  # ggplot2 function - turns maps pkg data into DF

ohio.county <- subset(map.county, region=="ohio")

# extract income data ...................................................
library(here)   # directory info
library(Hmisc)  # access to %nin% for NOT %in%
Earnings <- read_csv(file=here("education_level_median_earnings.csv")) 
# View(Earnings)
US_earnings <- Earnings %>% 
  filter(geographic_area == "United States")
OH_earnings <- Earnings %>% 
  filter(geographic_area == "Ohio")
OH_County_earnings <- Earnings %>% 
  filter(geographic_area %nin% c("United States", "Ohio")) %>% 
  mutate(county = str_to_lower(str_remove(geographic_area," County, Ohio")))

glimpse(OH_County_earnings)

# merge earnings with map data .................................

ohio_county_map <- ohio.county %>% 
  inner_join(OH_County_earnings, by=c("subregion" = "county"))

glimpse(ohio_county_map)

# start mapping .................................................

ggplot(ohio_county_map, aes(x=long,y=lat,
                        group=group,
                        fill=all_education_levels)) +
  geom_polygon() +
  scale_fill_gradient(low="lightgrey", high="black") +
  coord_map() +
  labs(fill="Average Income\n(ACS 2020?)") 

##
## playing with setting theme using Chang 13.19 example
##    (note that some aspects deprecated)
##

theme_clean <- function(base_size=12)  {
  require(grid)
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length=unit(0,"cm"),
      axis.ticks.margin=unit(0,"cm"),
      panel.margin=unit(0,"lines"),
      plot.margin=unit(c(0,0,0,0),"lines"),
      complete=TRUE
    )
}

ggplot(ohio_county_map, aes(x=long,y=lat,
                            group=group,fill=all_education_levels)) +
  geom_polygon() +
  #  guides(fill='none') +
  scale_fill_gradient(low="lightgrey", high="black") +
  coord_map() +
  labs(fill="Average Income\n(ACS 2020?)") +
  theme_clean()


##
## In-class assignment: Use Fill that is different from median county income
##


## add diff ...
OH_County_earnings2 <- Earnings %>% 
  filter(geographic_area %nin% c("United States", "Ohio")) %>% 
  mutate(county = str_to_lower(str_remove(geographic_area," County, Ohio"))) %>% 
  mutate(diffMedian = all_education_levels - 
           median(all_education_levels))

## explore new DF with diff .....
summary(OH_County_earnings2)

OH_County_earnings2 %>% 
  arrange(desc(diffMedian)) %>% 
  select(county, all_education_levels, diffMedian) %>% 
  View()


ohio_county_map2 <- ohio.county %>% 
  inner_join(OH_County_earnings2, by=c("subregion" = "county"))



#=======================================================================
#=======================================================================
# B O N U S  MAP  functions (and a little follow-up to JJ Fain's talk)
#=======================================================================
#=======================================================================

#=======================================================================
# Cartograms
# https://www.r-graph-gallery.com/cartogram.html
# cartogram_cont is the function

# Draw your own Rectangular Statistical Cartogram with recmap
# https://cran.r-project.org/web/packages/recmap/vignettes/recmap.html


#=======================================================================
# Maps from Shapefiles (from DataCamp)
# Although the built-in maps from the maps package are
# very convenient, using shapefiles is a more flexible
# way of accessing geographic and political boundaries.
#
# Shapefiles can be used to describe points, polylines
# or polygons - here you'll focus on polygons for
# drawing maps.
#
# A single shapefile actually consists of several files,
# each describing a specific aspect of the overall
# geometry. The three core file types are:
#
# .shp: the shape, the feature geometry itself.
# .shx: the shape index, a positional index.
# .dbf: the attribute, attributes for each shape arranged in columns.
# The prefix name of these files must be consistent and they must be kept in the same directory.
#
# readOGR
