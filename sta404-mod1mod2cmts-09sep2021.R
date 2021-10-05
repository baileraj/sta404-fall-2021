## sta404-mod1mod2cmts-09sep2021.R 

## Use the gapminder data and produce a figure that elaborates on the 
## story of the relationship between life expectancy and GDP for countries 
## in Asia in 2007.

library(tidyverse)
library(gapminder)

gapAsia2007 <- gapminder %>% 
  filter(continent == "Asia", year == 2007)

ggplot(gapAsia2007, aes(x=gdpPercap, y=lifeExp)) +
  geom_point()

# could add line ...
ggplot(gapAsia2007, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

# fit linear regr model ...
fitlm <- lm(lifeExp ~ gdpPercap, data=gapAsia2007)
summary(fitlm)   ## sqrt(MSE) = 5.861, R2 = 0.475

# fit more complicated model (segmented)
#      y = beta0 + beta1*x + beta2*(x-5000)*I(x>5000) + e
# so if x <= 5000,
#                 y = beta0 + beta1*x + beta2*(x-5000)*I(x>5000) + e
# and if x > 5000,
#                 y = (beta0-beta2*5000) + (beta1+beta2)*x + e

gapAsia2007 <- gapAsia2007 %>% 
  mutate(GDPover5K = (gdpPercap - 5000)*as.numeric(gdpPercap > 5000))
fitseg <- lm(lifeExp ~ gdpPercap + GDPover5K, data=gapAsia2007)
summary(fitseg)  ## sqrt(MSE) = 4.975, R2 = 0.634 
                 ## impact of the outlier add lots of residual variability

# a little more work to add fit of the segmented model ....
xx <- seq(from=0, to=max(gapAsia2007$gdpPercap), length=1000)
yy <- predict(fitseg, newdata=data.frame(
  gdpPercap = xx,
  GDPover5K = (xx - 5000)*as.numeric(xx > 5000)
))

DFsegfit <- data.frame(gdpPercap = xx,
                       predLifeExp = yy)

ggplot(gapAsia2007, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_line(data=DFsegfit, aes(x=gdpPercap, y=predLifeExp))

## highlight points
DF2pts <- gapAsia2007 %>%
  filter(lifeExp < 50 | (lifeExp<60 & gdpPercap>4000))
DF2pts

library(ggrepel)
ggplot(gapAsia2007, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_line(data=DFsegfit, aes(x=gdpPercap, y=predLifeExp),
            color="darkgrey") +
  geom_point(data=DF2pts, 
             aes(x=gdpPercap,y=lifeExp),
             color="red") +
  geom_text_repel(data=DF2pts, 
            aes(x=gdpPercap,y=lifeExp, label=country),
            color="red") +
  theme_minimal()

## what happens to the fit if Afghanistan and Iraq omitted?
gapAsia2007WO2 <- gapAsia2007 %>% 
  filter(country != "Afghanistan") %>% 
  filter(country != "Iraq") 
fitsegWO2 <- lm(lifeExp ~ gdpPercap + GDPover5K, 
                data=gapAsia2007WO2)
summary(fitsegWO2)  ## sqrt(MSE) = 3.08, R2 = 0.766 
## impact of the outliers add lots of residual variability

View(gapAsia2007WO2)

## ================================================================
##
##
## consider different number of colors on a plot
## exploring claim in Few's chapter about attending to colors
##

Gcountries <- unique(gapminder$country)   # 142 countries

set.seed(20210910)
pick4 <- sample(x=Gcountries, size=4)
pick8 <- sample(x=Gcountries, size=8)
pick12 <- sample(x=Gcountries, size=12)

gapminder %>% 
  filter(country %in% pick4) %>% 
  ggplot(aes(x=year,y=lifeExp,color=country)) + geom_line(lwd=1.2)

gapminder %>% 
  filter(country %in% pick8) %>% 
  ggplot(aes(x=year,y=lifeExp,color=country)) + geom_line(lwd=1.2)

gapminder %>% 
  filter(country %in% pick12) %>% 
  ggplot(aes(x=year,y=lifeExp,color=country)) + geom_line(lwd=1.2)
