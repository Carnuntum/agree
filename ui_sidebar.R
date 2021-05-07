source('ui_sidebarMenuItems.R')

#'*----------------------------------------------------------------------------*
#'*----------------------- put here all menuItems -----------------------------*
#'*----------------------------------------------------------------------------*
sidebar <- dashboardSidebar(
  sidebarMenu(id = 'dbSidebar',
    docMenu,
    gettingStarted,
    allMethods
  )
)

