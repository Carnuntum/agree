library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(irr)
import::from(kableExtra, 'kable')
import::from(kableExtra, 'kable_styling')
import::from(DescTools, 'ContCoef')
import::from(rel, 'spi')
src_ui <- dir(pattern = '^ui_')
sapply(src_ui, source)

source('server.R')
options(scipen = 999)


ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin = 'blue'
)
