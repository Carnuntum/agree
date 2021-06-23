source('ui_sidebarMenuItems.R')

#'*----------------------------------------------------------------------------*
#'*----------------------- put here all menuItems -----------------------------*
#'*----------------------------------------------------------------------------*
sidebar <- dashboardSidebar(
  width = 270,
  sidebarMenu(id = 'dbSidebar',
    docMenu,
    gettingStarted,
    allMethods,
    references
  ),
  p(tagList("CC BY-NC", HTML('&copy')), style = "
              position: relative;
              bottom: 0;
              left: 0;
              right: 0;
              padding-left: 10px;
              ")
)

