# challenger-logistic-reg.R
# 22 Sept. 2021
#
# what if you want to add a model fit to an existing plot?
# Using Challenger disaster data
# http://www.asktog.com/books/challengerExerpt.html
# https://archive.ics.uci.edu/ml/datasets/Challenger+USA+Space+Shuttle+O-Ring
# Temp forecast at launch of Challenger = 31 deg. F

library(tidyverse)

# data set is text with spaces separating values - may need to edit to make sure filename is ok
oring <- read_table(file="https://archive.ics.uci.edu/ml/machine-learning-databases/space-shuttle/o-ring-erosion-only.data",
                    col_names=c("Nring","Ndistress","TempF","LeakCheckPSI","LaunchOrder"))

# str(oring)
# mode(oring)                        # so what type of object did read_table produce?

oring_DF <- as.data.frame(oring)   # convert to a data frame
oring_DF
View(oring_DF)

# adding variable indicating distress to at least one O-ring
oring_DF <- oring %>% 
  mutate(ind_distress = ifelse(Ndistress>0, 1, 0))    # adding variable indicating distress 

# oring_DF <- oring_DF %>% 
#   mutate(ind_distress = as.numeric(Ndistress>0))  


# suppose a simple logistic regression model was fit
# log(odds of O-ring distress) = b0 + b1 TempF

oring_logreg <- glm(ind_distress ~ TempF, 
                    data=oring_DF, family=binomial)

summary(oring_logreg)

# predictions are a little jagged - clean this up

pred_DF <- data.frame(TempF = seq(from=min(oring_DF$TempF),
                                  to=max(oring_DF$TempF),
                                  length=1000))

pred_DF <- pred_DF %>% 
  mutate(phat = predict(oring_logreg, newdata=pred_DF, type="response"))

# cleaning up figure
# http://ggplot2.tidyverse.org/reference/annotate.html

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
  geom_point() +
  geom_line(data=pred_DF, aes(x=TempF,y=phat), color="darkgrey")+
  scale_x_continuous(name="Temperature (F)", limits=c(50,84)) +
  scale_y_continuous(name="O-ring distress observed", breaks=c(0,1),
                     labels=c("No","Yes")) +
  annotate("text", x=71,y=.25, 
           label="No problems for\nlaunch Temp > 70 F",
           hjust=0, color="blue") +
  annotate("rect", xmin = 71, xmax = 83, ymin = 0, ymax = 1,
           alpha = .2, fill="blue") +
  annotate("text", x=50, y=.50,
           label="Challenger launch\ntemperature forecast\nwas 31 F!",
           hjust=0, color="red") +
  annotate("text",x=60,y=.9,label="Logistic Regression Prediction",
           hjust=0, color="darkgrey") +
  labs(caption="Source: UCI Machine Learning Repository") +
  theme_minimal()


##
## thoughts / suggestions ......................................
##
## * overplotting - change symbol to an open circle { shape = 1 }
##            and jitter (only in horiz direction)

table(oring_DF$TempF)

## * increase plotting symbol size { size=5 in geom_point or geom_jitter }
## REF: vignette("ggplot2-specs")
## REF: https://rafalab.github.io/dsbook/ggplot2.html#annotation-shapes-and-adjustments
##
## * degree symbol
## REF: https://github.com/r-lib/scales/issues/159 -- Unicode symbol vs 
##                          plotmath expression
## RE; unicode list https://unicode-table.com/en/
## 
## ?plotmath  ## <- Math annotation in R 

library(ggthemes)   # also can play with more themes

ggplot(oring_DF, aes(x=TempF, y=ind_distress)) +
#  geom_point(size=5, shape=1 ) +
  geom_jitter(width=.2,height=0, size=5, shape=1) +
  geom_line(data=pred_DF, aes(x=TempF,y=phat), color="darkgrey")+
  scale_x_continuous(name=paste0("Temperature (",
                                 "\u00b0F)"), 
limits=c(48,84)) +
  scale_y_continuous(name="O-ring distress observed", breaks=c(0,1),
                     labels=c("No","Yes")) +
  annotate("text", x=48, y=1, label="Yes",hjust=0) +
  annotate("text", x=48, y=0, label="No",hjust=0) +
  annotate("text", x=72,y=.25, 
           label=paste0("No O-ring distress observed\nwhen launch Temp > 70",
                        "\u00b0F"),
           hjust=0, color="blue") +
  annotate("rect", xmin = 70, xmax = 83, ymin = 0, ymax = 1,
           alpha = .2, fill="blue") +
  annotate("text", x=51, y=.70,
           label=paste0("Challenger launch\ntemperature forecast\nwas 31", 
                        "\u00b0F"),
           hjust=0, color="red") +
  annotate("text",x=60,y=.9,label="Logistic Regression Prediction",
           hjust=0, color="darkgrey") +
  labs(caption="Source: UCI Machine Learning Repository") +
  ggtitle("O-ring distress observed") +
#  theme_fivethirtyeight() +
#  theme_tufte() +
#  theme_wsj() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())  # remove y axis title and text

##  QUESTIONS / COMMENTS:
##     Do you like the plot with or without the model fit?
##     Do you prefer one theme vs. another?
##     