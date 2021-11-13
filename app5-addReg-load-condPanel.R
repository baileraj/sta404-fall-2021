# app5-addReg-load-condPanel.R
# Rev: 02 may 21
# + load saved data
# + add conditional panel selection of option
# REFs
#  https://www.r-bloggers.com/2019/05/how-to-save-and-load-datasets-in-r-an-overview/
#  https://shiny.rstudio.com/reference/shiny/0.11/conditionalPanel.html
#
# same as app5.R but with conditional input control
# generalized from class-app-CLEAN.R from 
#     /home/baileraj/sta-404-OLD/sta404-summer17
#
# 19 Apr 21:  add linear reg fit

# A shiny app always needs shiny + load other libraries 
library(shiny)

library(ggplot2)    # don't need anything else
# library(tidyverse)

# correspondence vector between ui variable names and server variable names
# varnames <- c("Age (years)" = "age.yrs",
#               "FEV (L)" = "fev.L",
#               "Height (in)" = "ht.in")

# read in the data and do any modifications
# fev_DF <- read.table("https://www.users.miamioh.edu/baileraj/classes/sta363/fev_data.txt", header=T)
# fev_DF <- read.table("/home/baileraj/sta404/Data/fev_data.txt", header=T)
# fev_DF <- fev_DF %>% 
#   mutate(gender = ifelse(ind.Male==0,"Female","Male"))

##
## support we wanted to use previously save data
##
##
## first, let's save the objects, restart R and
##        then load this
# mydir <- "C:/Users/John Bailer/Documents/Classes/STA404class/ShinyModule08Apps"
# save(fev_DF, varnames, 
#      file=paste0(mydir,"/fevData.Rdata"))

load("fevData.Rdata")
##
## now we can omit the read.csv and varnames
##     def'n in the app BUT need to have 
##     fevData.Rdata in folder with the Shiny app.R




### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "FEV Scatterplot Explorer"),
  sidebarLayout(
    
    # Sidebar typically used to house input controls
    sidebarPanel(
      
      selectInput(inputId = "yvar", label= "Select an y-variable", 
                  choices = varnames),
      
      # selectInput(inputId = "xvar", label= "Select an x-variable", 
      #             choices = varnames),

      uiOutput(outputId = "xvar"),
      
      checkboxInput(inputId= "color_gender", label = "Color by Gender"),
      checkboxInput(inputId = "AddReg", label = "Add Reg Fit"),
      
      # JavaScript expression to determine
      # whether panel displayed
      #    input.AddReg <- JavaScript object
      #      corresponding to input$AddReg Shiny object
      conditionalPanel(   
        condition = "input.AddReg == 1",  
        selectInput("model",
                    "Regression Model",
                    c("Linear","Quadratic","Cubic","LOESS"))
      )
    ),
    
    # Main panel typically used to display outputs
    mainPanel(
      plotOutput(outputId = "myscatterplot")
    )
    
  )
)

### Define server behavior for application here
#  Expressions such as in renderPlot MUST be in {} 

server <- function(input, output) {
  
  output$xvar <- renderUI({
    xvarsubset <- varnames[varnames!=input$yvar]
    selectInput(inputId = "xvar",
                label = "Select an X-variable",
                choices = xvarsubset)
  })
  
  output$myscatterplot <-
    renderPlot({
      p1 <- ggplot(aes_string(x=input$xvar,y=input$yvar),data=fev_DF) + 
        labs(x=names(varnames)[varnames==input$xvar],
             y=names(varnames)[varnames==input$yvar],
             title(paste("Scatterplot of ",input$yvar,"by",input$xvar))) +
        theme_bw()
      if(input$AddReg) {
        if (input$model == "Linear") 
          p1 <- p1 + geom_smooth(method="lm")
        else if (input$model == "Quadratic")
          p1 <- p1 + 
            geom_smooth(method="lm",
                        formula = y ~ poly(x,2))
        else if (input$model == "Cubic")
          p1 <- p1 + 
            geom_smooth(method="lm",
                        formula = y ~ poly(x,3))
        else if (input$model == "LOESS")
          p1 <- p1 + geom_smooth(method="loess")
      }
      if(input$color_gender) {
        p1 + 
          geom_point(aes_string(color="gender"))
      } else {
        p1 + 
          geom_point()
      }
    }
  )
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)

##
##  poly(x, 2) generates orthogonal polynomials
##  - good for fitting polynomial functions
##
##
# 
# names(fev_DF)
# 
# cfit <- lm(ht.in ~ age.yrs + 
#              I(age.yrs^2) +
#              I(age.yrs^3), data=fev_DF)
# cfitO <- lm(ht.in ~ poly(age.yrs,3), data=fev_DF)
# summary(cfit)
# summary(cfitO)
# cor(predict(cfit),
#     predict(cfitO))
# plot(x=fev_DF$age.yrs,
#      y=predict(cfit))
# plot(x=fev_DF$age.yrs,
#      y=predict(cfitO))
# plot(x=predict(cfit),
#      y=predict(cfitO))
# 
# dd <- 1:5
# demoDF <- data.frame(d=dd,d2=dd^2,d3=dd^3)
# 
# ggplot(demoDF,aes(x=d)) +
#   geom_line(aes(y=d)) +
#   geom_line(aes(y=d2)) +
#   geom_line(aes(y=d3))
# 
# cor(demoDF)
# 
# polyData <- poly(dd,3)
# demoDF2 <- as.data.frame(polyData)
# demoDF2 <- cbind(demoDF2, dd)
# names(demoDF2) <- c("lin","quad","cubic","dd")
# ggplot(demoDF2,aes(x=dd)) +
#   geom_line(aes(y=lin)) +
#   geom_line(aes(y=quad)) +
#   geom_line(aes(y=cubic))
# cor(demoDF2)



