library(shinythemes); library(shinyjs) ; library(shinydashboard)

#install.packages(c("shinyBS"))

#============================================================================================================
#                                                     UI
#============================================================================================================

ui <- dashboardPage(
  dashboardHeader(
    title = "D-Text",
    titleWidth = 250
    
  ),
  dashboardSidebar(
    disable = FALSE,
    width = 150,
    
    sidebarMenu(
      menuItem("Email feed", tabName = "email", icon = icon("envelope-square")),
      menuItem("Follow ups", tabName = "follow", icon = icon("eye"))
    )
  ),
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      tabItem(tabName = "email",
              h2("D-Text - Email Monitoring Daily Report"
                 
                 ,img(src='pictures/evs_logo.png'
                      ,align='right'
                      ,width = 150
                      ,height = 46.416938110749186)
                 
                 ,align = "left"),
              
              h5(paste0("Last update: ", Sys.Date() - 1), align = "left"),
              
              #hr(), 
              br(),
              
              fluidRow(
                DT::dataTableOutput('email_feed')
              )
      ),
      
      tabItem(tabName = "follow",
              h2("Email Follow ups"
                 
                 ,img(src='pictures/evs_logo.png'
                      ,align='right'
                      ,width = 150
                      ,height = 46.416938110749186)
                 
                 ,align = "left"),
              
              br(),
              
              fluidRow(
                DT::dataTableOutput('follow_ups')
              )
      )
    )  
  )
)







