library(shiny)
library(tidyverse)
#import::from(magrittr, '%>%')
library(magrittr)
library(irr)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(kableExtra)
library(DescTools)
library(rel)
library(irr)
library(VGAM)
library(sklarsomega)
library(BlandAltmanLeh)
library(snow)
library(rlist)
library(boot) #for cis
library(krippendorffsalpha)
library(shinyBS) #for popovers
library(mclust) #for adjusted rand index
library(multilevel) #for rwg
library(irrCAC) #for several funs
#import::from(kableExtra, 'kable')
#import::from(kableExtra, 'kable_styling')
#import::from(DescTools, 'ContCoef')
#import::from(rel, 'spi')
#import::from(VGAM, 'kendall.tau')

source('exampleData.R')
source('style.R')
source('text.R')
source('src_functions.R')

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
