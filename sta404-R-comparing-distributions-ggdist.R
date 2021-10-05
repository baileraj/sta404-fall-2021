# sta404-R-comparing-distributions-ggdist.R
# sta404-R-comparing-distributions-BLANK.R
# revised: 29 Aug 2021 (09 Feb 2021 -adding ggdist layers;
# revised: 31 Jan 2021; 22 August 2020)

# save work in class subdirectory
# SERVER:   /home/baileraj/sta404
#           /opt/courses/sta404-2020-fall (created by B. Koby - group:R access)
# MacBook:  /Users/baileraj/Desktop/Classes/sta404-fall-2021

# loading packages - may need to install if not available
# library("tidyverse")   # includes ggplot2, dplyr and other tools
library("gapminder")   # dataset with country, may need to install this if it is not available

library(ggdist)
library(dplyr)
library(tidyr)
library(distributional)
library(ggdist)
library(ggplot2)
library(cowplot)  # annotate, arrange, mix plots

# REF:   https://mjskay.github.io/ggdist/

# vignettes and more from
#     https://rdrr.io/cran/ggdist/f/vignettes/slabinterval.Rmd


## ggdist
##  from https://mjskay.github.io/ggdist/articles/slabinterval.html
##  Geoms starting with geom_ are meant to be used on
##          already-summarized data (typically data summarized
##          into intervals): things like geom_pointinterval()
##          and geom_interval().
##  Stats starting with stat_ are meant to be used on sample
##          data; e.g. draws from a posterior distribution (or
##          any other distribution, really). These stats compute
##          relevant summaries (densities, CDFs, points, and/or
##          intervals) before forwarding the summaries to their geom.


set.seed(1234)
df = tribble(
  ~group, ~subgroup, ~value,
  "a",          "h", rnorm(1000, mean = 5),
  "b",          "h", rnorm(1000, mean = 7, sd = 1.5),
  "c",          "h", rnorm(1000, mean = 8),
  "c",          "i", rnorm(1000, mean = 9),
  "c",          "j", rnorm(1000, mean = 7)
) %>%
  unnest(value)

df

# stat_eye == violins .............
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_eye() +
  ggtitle("stat_eye()")

# half_eye == density
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_halfeye() +
  ggtitle("stat_halfeye()")

# more control .............................
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()     # cowplot::panel_border

plot_grid(ncol = 3, align = "hv",
          p + stat_eye(side = "left") + labs(title = "stat_eye()", subtitle = "side = 'left'"),
          p + stat_eye(side = "both") + labs(subtitle = "side = 'both'"),
          p + stat_eye(side = "right")  + labs(subtitle = "side = 'right'")
)

# more from the vignette .............
#    distributional::dist_beta
data.frame(alpha = seq(5, 100, length.out = 10)) %>%
  ggplot(aes(y = "", dist = dist_beta(alpha, 10), color = alpha)) +
  stat_dist_slab(fill = NA) +
  coord_cartesian(expand = FALSE) +
  scale_color_viridis_c() +
  labs(
    title = "stat_dist_slab()",
    subtitle = "aes(dist = dist_beta(alpha, 10), color = alpha)",
    x = "Beta(alpha,10) distribution",
    y = NULL
  )

p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

ph = df %>%
  ggplot(aes(y = group, x = value)) +
  panel_border()

plot_grid(ncol = 2, align = "hv",
          p + stat_histinterval() + labs(title = "stat_histinterval()", subtitle = "horizontal"),
          ph + stat_histinterval() + labs(subtitle = "vertical")
)

plot_grid(ncol = 2, align = "hv",
          ph + stat_histinterval(slab_color = "gray45", outline_bars = FALSE) +
            labs(title = "stat_histinterval", subtitle = "outline_bars = FALSE (default)"),
          ph + stat_histinterval(slab_color = "gray45", outline_bars = TRUE) +
            labs(subtitle = "outline_bars = TRUE")
)

# cumulative distribution functions ....
p = df %>%
  ggplot(aes(x = group, y = value)) +
  panel_border()

ph = df %>%
  ggplot(aes(y = group, x = value)) +
  panel_border()

plot_grid(ncol = 2, align = "hv",
          p + stat_ccdfinterval() + labs(title = "stat_ccdfinterval()", subtitle = "vertical"),
          ph + stat_ccdfinterval() + labs(subtitle = "horizontal"),
          p + stat_cdfinterval() + labs(title = "stat_cdfinterval()", subtitle = "vertical"),
          ph + stat_cdfinterval()  + labs(subtitle = "horizontal")
)


# gradients .........................
df %>%
  ggplot(aes(x = group, y = value, fill = subgroup)) +
  stat_gradientinterval(position = "dodge") +
  labs(title = "stat_gradientinterval(position = 'dodge')")

# ==========================================================

# create test data set for exploration ===============================
# if not interested in the details, go to line #62
# 4 different groups are being being generated

# set up example data sets ........................

set.seed(86753092)

vec1 <- c(rnorm(n=400,mean=54,sd=10),rnorm(n=200,mean=79,sd=8))

vec2 <- c(rnorm(n=600, mean=62.75275, sd=15.28787))

vec3 <- rgamma(n=600, shape = 16.8489, scale=3.724442)

vec4 <- runif(n=600, min=32.77338, max=92.73212)

myDF <- data.frame(group=rep(c(1,2,3,4),
                             c(600,600,600,600)),
                   response = c(vec1, vec2, vec3, vec4))

myDF <- myDF %>%
  mutate(fgroup=factor(group,
                       labels = c("bimodal", "normal", "gamma", "uniform")))

# str(myDF)

# stat_eye for our simulation data .............
myDF %>%
  ggplot(aes(y = fgroup, x = response)) +
  stat_eye() +
  ggtitle("stat_eye for 404/504 class examples")

# stat_halfeye for our simulation data .............
myDF %>%
  ggplot(aes(y = fgroup, x = response)) +
  stat_halfeye() +
  ggtitle("stat_halfeye for 404/504 class examples")

# more control ...........................
p <- myDF %>%
  ggplot(aes(x = fgroup, y = response)) +
  panel_border()

plot_grid(ncol = 3, align = "hv",
          p + stat_eye(side = "left") + labs(title = "stat_eye()", subtitle = "side = 'left'"),
          p + stat_eye(side = "both") + labs(subtitle = "side = 'both'"),
          p + stat_eye(side = "right")  + labs(subtitle = "side = 'right'")
)


# gradients .........................
myDF %>%
  ggplot(aes(x = fgroup, y = response,
             fill = fgroup)) +
  stat_gradientinterval(position = "dodge") +
  labs(title = "stat_gradientinterval(position = 'dodge')") +
  guides(fill=FALSE) +
  theme_minimal()

