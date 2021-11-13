# other_plots.R

# multivariate plots ...
str(state.x77)
dimnames(state.x77)
rownames(state.x77)

stateDF <- data.frame(name = rownames(state.x77))
stateDF <- cbind(stateDF,
                 as.data.frame(state.x77))
rownames(stateDF) <- NULL
stateDF

names(stateDF)

stateSubDF <- stateDF[,c(1,4,5,6,7)]
names(stateSubDF)

stateSubDF <- stateSubDF %>% 
  mutate(Literacy = 100 - Illiteracy)

stateSubDF <- cbind(stateSubDF, state.region)


# parallel coordinate plots
# https://www.r-graph-gallery.com/parallel-plot.html

library(GGally)
?parcoord

# starting with the variables
#   Illiteracy, Life Exp, Murder, HS Grad
#   also grouping by region
ggparcoord(data=stateSubDF,
           column=2:5,
           groupColumn = 7)

# using 'literacy' vs. 'illiteracy'
#   better to have 'small' mean small thing
ggparcoord(data=stateSubDF,
           column=3:6,
           groupColumn = 7)

# changing the order ..............
ggparcoord(data=stateSubDF,
           column=3:6,
           order = c(5,6,4,3),
           groupColumn=7)

ggparcoord(data=stateSubDF,
           column=3:6,
           order = c(5,6,3,4),
           groupColumn=7)

# can select a different scaling .....
ggparcoord(data=stateSubDF,
           column=3:6,
           order = c(5,6,3,4),
           groupColumn=7,
           scale="uniminmax")

##
##  Star (Spider/Radar) plots
##

stars(stateSubDF)
stars(stateSubDF, labels=state.abb)


##
##  Chernoff Faces
##  https://www.rdocumentation.org/packages/aplpack/versions/1.3.3/topics/faces

# install.packages("aplpack", dependencies=TRUE)
library(aplpack)

faces(stateSubDF)  # what doesn't this work?

faces(state.x77)

names(state.x77)
dimnames(state.x77)

myStateMat <- state.x77[,c(2,4,5,6)]
myStateMat
?state.x77

#
# suppose we want 'big' to be 'bad'
#

myStateMat2 <- cbind(
  1/state.x77[,"Life Exp"],
  state.x77[,"Illiteracy"],
  state.x77[,"Murder"],
  100-state.x77[,"HS Grad"]
)
myStateMat2
colnames(myStateMat2) <- 
  c("1/Life Exp",
    "Illiteracy",
    "Murder",
    "100-HS Grad")
myStateMat2

faces(myStateMat2)

# REF: EXPLORING OTHER {GGPLOT2} GEOMS
#  https://ivelasq.rbind.io/blog/other-geoms/
#
# Streamgraphs:    ggstream::geom_stream
# Ridgeline plots: ggridges::geom_density_ridges
# Sankey diagrams: ggsankey::geom_sankey & ggalluvial::geom_alluvial
# Bump charts:     ggbump::geom_bump
# Waffle charts:   waffle::geom_waffle
# Beeswarm charts: ggbeeswarm::geom_beeswarm
# Mosaic charts:   ggmosaic::geom_mosaic
