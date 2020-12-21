library(shiny)
#library(tidyverse)
import::from(magrittr, '%>%')
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
#import::from(kableExtra, 'kable')
#import::from(kableExtra, 'kable_styling')
#import::from(DescTools, 'ContCoef')
#import::from(rel, 'spi')
#import::from(VGAM, 'kendall.tau')
src_ui <- dir(pattern = '^ui_')
sapply(src_ui, source)

source('server.R')
options(scipen = 999)
useShinyjs()

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin = 'blue'
)
