library(shiny)

source("init.R")
source("shiny.R")

runApp(port=8101,list(ui=shinyUI,server=shinyServer),launch.browser=F)

