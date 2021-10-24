#  TOPICS-expand-timing-labeling.R
#  includes more on ...
#           axis control (finer control of expand options)
#           system timing and using summary data
#           facet labeling
# 
# revised: 24oct21

library(tidyverse)
library(ggplot2)

# diamonds data frame built into ggplot2

summary(diamonds)
sum(diamonds$carat>=3.5)  # only 10 diamonds bigger than 3.5 carats

# Add new factor variable Gcarat

myDiamonds <- diamonds %>%  
  filter(carat <= 3.5) %>% 
  mutate(Gcarat = cut(carat, breaks=seq(from=0,to=3.5,by=.25)))
  
# names(myDiamonds)
# [1] "carat"   "cut"     "color"   "clarity" "depth"   "table"   "price"  
# [8] "x"       "y"       "z"       "Gcarat"

ggplot(myDiamonds, aes(x=cut)) +
  geom_bar()

# color ... seq palette since ordered levels
# no need to have legend since x-axis has the information
# REF: https://ggplot2.tidyverse.org/reference/theme.html
# expanding axes
# REF: https://ggplot2.tidyverse.org/reference/expansion.html

pRAWstart <- proc.time()   # let's also see how long this takes ...
ggplot(myDiamonds, aes(x=cut, fill=cut)) +
  geom_bar() +
  scale_fill_brewer(type="seq", palette="PuBu") +
  theme_dark() +
  scale_x_discrete(expand = expansion(add = 1)) +
  # Expand to space to left and right of data
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  # No space below the bars but 10% above them
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotate(geom="text", x=0.4, y=22500,
           label="Count", color="white")
pRAWstop <- proc.time()
executionR <- pRAWstop - pRAWstart
executionR

#
# Suppose you didn't want to do calculations
# - calculate summary data data frame
#
myDiamondsSUM <- myDiamonds %>%
  group_by(cut) %>%
  summarize(cutSUM = n())

myDiamondsSUM

pSUMMARYstart <- proc.time()
ggplot(myDiamondsSUM, aes(x=cut, y=cutSUM, fill=cut)) +
  geom_col() +
  scale_fill_brewer(type="seq", palette="PuBu") +
  theme_dark() +
  scale_x_discrete(expand = expansion(add = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotate(geom="text", x=0.4, y=22500,
           label="Count", color="white")
pSUMMARYstop <- proc.time()
executionSum <- pSUMMARYstop - pSUMMARYstart
executionSum



#
# Pie charts - removing the NA's
#

# starting with bars ...
ggplot(filter(myDiamonds, !is.na(Gcarat)),
       aes(x=Gcarat, fill=cut)) +
  geom_bar()

ggplot(filter(myDiamonds, !is.na(Gcarat)),
       aes(x=Gcarat, fill=cut)) +
  geom_bar()+
  coord_flip()

# moving to pies

myDiamonds2way <- myDiamonds %>%
  filter(!is.na(Gcarat)) %>%
  group_by(Gcarat, cut) %>%
  summarize(count = n())

View(myDiamonds2way)

# ...
ggplot(myDiamonds2way,
       aes(x=1, y=count, fill=cut)) +
  geom_col() +
  coord_polar(theta="y") +
  facet_wrap(. ~ Gcarat) +
  theme(axis.title.x = element_blank(),        
        axis.title.y = element_blank(),        
        legend.title = element_blank(),        
        axis.ticks.y = element_blank(),        
        axis.text = element_blank())

# ... need to use position="fill" ...
ggplot(myDiamonds2way,
       aes(x=1, y=count, fill=cut)) +
  geom_col(position="fill") +
  coord_polar(theta="y") +
  facet_wrap(. ~ Gcarat) +
  theme(axis.title.x = element_blank(),        
        axis.title.y = element_blank(),        
        legend.title = element_blank(),        
        axis.ticks.y = element_blank(),        
        axis.text = element_blank())

#
# identify the sample size on the different facets
# REF: https://community.rstudio.com/t/using-strip-text-to-print-several-bits-of-information-ggplot2-facet-labller/10232/2

# get the totals and build a label
CaratSum <- myDiamonds2way %>%
  group_by(Gcarat) %>%
  summarize(Totalcount=sum(count))  %>%
  mutate(clabel = paste0("Carat: ",Gcarat,"\n{n=", Totalcount,"}"))


head(CaratSum)
head(myDiamonds2way)

myDiamonds2way <- myDiamonds2way %>%
  left_join(CaratSum) %>%
  mutate(withinGrpProp = count/Totalcount,
         amongGrpProp = count/max(CaratSum$Totalcount))

head(myDiamonds2way)


ggplot(myDiamonds2way,
       aes(x=1, y=count, fill=cut)) +
  geom_col(position="fill") +
  coord_polar(theta="y") +
  facet_wrap(. ~ clabel) +
  theme(axis.title.x = element_blank(),        
        axis.title.y = element_blank(),        
        legend.title = element_blank(),        
        axis.ticks.y = element_blank(),        
        axis.text = element_blank())


##
## a look at structure
##

ggplot(diamonds, aes(x=carat, y=price)) + 
  geom_point(alpha=.005)
