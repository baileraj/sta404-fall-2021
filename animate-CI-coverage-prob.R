#  animate-CI-coverage-prob.R
#  13 nov 2021
#  CI intervals

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


##
## REFS:
## https://cran.r-project.org/web/packages/gganimate/index.html
## https://ugoproto.github.io/ugo_r_doc/pdf/gganimate.pdf
## https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
## { pretty cool case study in next ref }
## https://evamaerey.github.io/rstudio_education_blog/unemployment/unemployment.html#47


##
## generate samples Normal data and construct CI
##

nsims <- 200
nsample <- 15

# generate *nsims* of size *nsample* from N(mu=500, sigma=100)
set.seed(112021)
gen.data <- matrix(rnorm(
  nsims*nsample,
  mean=500,
  sd=100),
  nrow=nsims)

# build matrix with columns: ybar, LCL, UCL
resMat <- apply(gen.data,
                1,
                function(yy) c(t.test(yy)$estimate, t.test(yy)$conf.int))

resMat[,1:3]

# convert to data frame for later plotting

resDF <- data.frame(sample = 1:nsims,
                    ybar = resMat[1,],
                    LCL = resMat[2,],
                    UCL = resMat[3,])

library(tidyverse)
resDF <- resDF %>%
  mutate(mu.in.CI = as.numeric(UCL >= 500 & 500 >= LCL),
         totalCover = cumsum(mu.in.CI),
         propCover = totalCover / sample)

# check results - about 5% of intervals should *not* contain mu
#                      95% will contain mu
head(resDF)
table(resDF$mu.in.CI)/nsims

#
# Let's start plotting - first the static version ....
#

ggplot(resDF,
       aes(x=sample,y=ybar)) +
  geom_errorbar(aes(ymin=LCL,
                    ymax=UCL,
                    color=as.factor(mu.in.CI))) +
  geom_hline(yintercept=500,
             color="black",
             size=1.5) +
  coord_cartesian(expand=FALSE) +
  guides(color="none") +
  theme_minimal()


# how about Coverage Probability properties?
ggplot(resDF,
       aes(x=sample,y=propCover)) +
  geom_line() +
  geom_hline(yintercept=0.95,
             color="black",
             size=1.5) +
  geom_rug(aes(color=mu.in.CI),sides="b") +
  coord_cartesian(expand=FALSE) +
  guides(color="none") +
  theme_minimal()



##
## now can we add animation ...
##


library(ggplot2)
library(gganimate)
library(gifski)

CI_Anim <- ggplot(resDF,
                  aes(x=sample,y=ybar)) +
  geom_errorbar(aes(ymin=LCL,
                    ymax=UCL,
                    color=as.factor(mu.in.CI))) +
  geom_hline(yintercept=500,
             color="black",
             size=1.5) +
  coord_cartesian(expand=FALSE) +
  guides(color="none") +
  theme_minimal() +
  # Here comes the gganimate specific bits
  labs(title = 'Sample: {frame_time}',
       x = 'Sample',
       y = 'Confidence Interval') +
  transition_time(sample)


# animation with each successive interval considered ...
CI_Anim + ease_aes('linear')
#      probably not what we wanted ......

# animation that includes previous values ...
CI_Anim +
  shadow_trail(0.02)




# how about Animating Coverage Probability properties?
ggplot(resDF,
       aes(x=sample,y=propCover)) +
  geom_line() +
  geom_hline(yintercept=0.95,
             color="black",
             size=1.5) +
  geom_rug(aes(color=mu.in.CI),sides="b") +
  coord_cartesian(expand=FALSE) +
  guides(color="none") +
  theme_minimal()

#
#
Anim_coverplot <- ggplot(resDF,
                    aes(x=sample,y=propCover)) +
  geom_line() +
  geom_hline(yintercept=0.95,
             color="black",
             size=1.5) +
  geom_rug(aes(color=mu.in.CI),sides="b") +
  coord_cartesian(expand=FALSE) +
  guides(color="none") +
  theme_minimal() +
  labs(title = 'Sample: {frame_time}',
       x = 'Sample',
       y = 'CI Coverage') +
  transition_reveal(sample)

Anim_coverplot

##
## other explorations
##

# can save animations (open with a browser - 'preview' shows the individ plots)
anim_save(filename = "CIprob-anim.gif", animation = Anim_coverplot)

# can investigate other options too if interested
animate(Anim_coverplot, width = 700, height = 432, fps = 2, 
        duration = 15, rewind = FALSE)





