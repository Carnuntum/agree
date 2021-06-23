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
#library(rel) #removed from cran
library(vcd) #bangdiwala
library(irr)
library(emojifont) #for emoji error plots
library(png)
library(gridExtra)
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

options(shiny.maxRequestSize = 30*1024^2)

source('exampleData.R')
source('style.R')
source('text.R')
source('src_functions.R')

src_ui <- dir(pattern = '^ui_')
sapply(src_ui, source)

source('server.R')
options(scipen = 999)
useShinyjs()

ui <- shiny::tagList(
  dashboardPage(header = header,
                sidebar = sidebar,
                body = body,
                skin = 'blue',
                
                # tags$head(tags$style(
                #   type="text/css",
                #   "#timelineImage img {max-width: 100%; width: 100%; height: auto;}"
                # ),
                tags$head(
                  tags$style(
                    type="text/css",
                    "#timelineImage img {max-width: 100%; width: 100%; height: auto;}"
                  )
                )
  ),
  # tags$footer(p(tagList("CC BY-NC", HTML('&copy')), style = "
  #             text-align: right;
  #             position: relative;
  #             bottom: 0;
  #             left: 0;
  #             right: 0;
  #             width:100%;
  #             font-size: 15px;
  #             height: 20px;   /* Height of the footer */
  #             padding: 0px;
  #             "))
)
