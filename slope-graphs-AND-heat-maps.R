## slope-graphs-AND-heat-maps.R
## 05 oct 2021

# A couple of alternative displays of Continent GDP share


# packages needed
library(gapminder)
library(tidyverse)

#
# data prep from categorical data module ...
#
myGapData <- gapminder %>%
  mutate(TotalGDP = pop*gdpPercap)
GDPsummaryDF <- myGapData %>%
  group_by(continent, year) %>%
  summarise(ContinentTotalGDP = sum(TotalGDP),
            ncountries = n())
GDPyearDF <- myGapData %>%
  group_by(year) %>%
  summarise(YearTotalGDP = sum(TotalGDP))
GDPcombo <- left_join(GDPsummaryDF, GDPyearDF,
                      by="year")  # what does left_join do?
GDPcombo <- GDPcombo %>% 
  mutate(PropWorldGDP = ContinentTotalGDP / YearTotalGDP,
         PctWorldGDP = 100*PropWorldGDP,
         ContGDPBillions = ContinentTotalGDP/1000000000)
GDPcombo <- GDPcombo %>% 
  mutate(n.cont = factor(continent, 
                         levels = c("Oceania", "Africa", "Asia", "Europe","Americas")))

# ALTERNATIVE 1:  Slope graph 1950 vs 2007
# slope graphs ...............................................
#

GDPcombo %>% 
  filter(year==1952 | year==2007) %>% 
  ggplot(aes(x=year, y=PropWorldGDP, color=continent)) +
  geom_line(size=2) + 
  theme_minimal()

# 1. Move continent names to graph
# 2. Identify 1952 and 2007 on x-axis
# 3. Check y-axis label

# ALTERNATIVE 2:  Heat Maps
# heat map ................................................
#
ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  coord_flip()

ggplot(GDPcombo, aes(x=year, y=n.cont, fill=PropWorldGDP)) +
  geom_tile() +
  scale_fill_gradient(low="lightgrey", high="black") +
  guides(fill="none") +
  labs(title="Continent Share of World GDP",
       subtitle="[<10% (light grey) to >40% (black)]",
       caption="Data source: Jennifer Bryan (2015) Gapminder package") +
  theme_fivethirtyeight()

