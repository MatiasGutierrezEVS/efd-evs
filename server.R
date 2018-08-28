library(shiny); library(shinydashboard); library(shinythemes); library(stringr)
library(DT); library(dplyr); library(magrittr); library(RMySQL); library(shinyjs) ; library("RDCOMClient")

load("E:/Users/Matias Gutierrez/Early Fraud Detection/Codes/Version 1/data/enron_training.RData")
email_txt_path <- "E:\\Users\\Matias Gutierrez\\Early Fraud Detection\\Codes\\Version 1"

dictionary_2 <- read.csv2("E:\\Users\\Matias Gutierrez\\Early Fraud Detection\\Data\\Fraud Keyword Dictionary WIP.csv", header = FALSE,
                          stringsAsFactors = FALSE)

email_feed <- all_check[,c("User", "From", "To", "Subject", "Date",
                           "is_suspiscius", "bodies", "username_to", "username_from")]
email_suspiscious <- email_feed[email_feed$is_suspiscius ==1 , ]
follow_ups <- all_check[11:20,]

email_suspiscious$Date <- format(email_suspiscious$Date,
                                 "%H:%M, %a, %b %d")

follow_ups <- email_feed[11:20,]

# To have different Emails acording to the day

email_feed$Date <- as.Date(email_feed$Date, format="%H:%M, %a, %b %d")
email_feed <- dplyr::arrange(email_feed, Date)

start_date = as.Date("2018-08-12")
time_difference = as.integer(Sys.Date() - start_date)
set.seed(as.integer(Sys.Date()))
num_cho = floor(runif(1, 3,15)) 

emails_day = email_feed[(9*time_difference):(9*time_difference+num_cho),]
email_suspiscious_1 <- email_feed[email_feed$is_suspiscius ==1, ][2,]
email_suspiscious = rbind(emails_day, email_suspiscious_1)
email_suspiscious <- dplyr::arrange(email_suspiscious, desc(is_suspiscius))

dictionary_1 <- as.data.frame(c('lawyers', 'voice mail', 'stalking'))
colnames(dictionary_1) <- c('V1')
dictionary <- rbind(dictionary_2, dictionary_1)
dictionary <- as.character(dictionary$V1)


#============================================================================================================
#                                                 SERVER
#============================================================================================================

server <- function(input, output) {

  output$email_feed<- DT::renderDataTable({ 

    dat <- datatable(email_suspiscious[,-c(6,7,8,9)], 
                     selection = 'single', rownames= FALSE, 
                     options = list(dom = 't', pageLength = 8,
                                    initComplete = JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'background-color': 'rgb(124, 108, 170)', 'color': '#fff'});",
                                      "}")))
    
    return(dat)
    
  }, options = list(autoWidth = FALSE))
  
  
  #---------------------------------------------------------------------------------------------------------- 
  # Render table
  #----------------------------------------------------------------------------------------------------------
  output$follow_ups <- DT::renderDataTable({
    
    dat <- datatable(follow_ups
                     , selection = 'single'
                     , options = list(dom = 't'
                                      ,initComplete = JS("function(settings, json) {"
                                                         ,"$(this.api().table().header()).css({'background-color': 'rgb(124, 108, 170)', 'color': '#fff'});"
                                                         ,"}")
                                      )
                     )
    
    return(dat)
    
  }, options = list(autoWidth = FALSE)
  )
  
  #----------------------------------------------------------------------------------------------------------
  # Show Email View
  #----------------------------------------------------------------------------------------------------------
  sel1 <- reactive(input$email_feed_rows_selected)
  
  observeEvent(input$email_feed_rows_selected, {
    showModal(modalDialog( size = "l",
                           title = h3(p(strong("Email View"))),
                           fluidRow(
                             column(5,
                                    fluidRow(
                                      column(4, h4(strong("From")),img(src='pictures/person1.PNG', align = "left")),
                                      column(8, br(),br(),box(title = tools::toTitleCase(paste0(unlist(strsplit(email_suspiscious[sel1(),"username_from"], ".", fixed = TRUE)), 
                                                                                      collapse = " ")), 
                                                    "Role: Accountant", br(), "DOJ: 03-05-2014", width = 10))
                                    ),
                                    fluidRow(
                                      column(4, h4(strong("To")), img(src='pictures/person2.PNG', align = "left")),
                                      column(8, br(),br(),box(title = tools::toTitleCase(paste0(unlist(strsplit(email_suspiscious[sel1(),"username_to"], ".", fixed = TRUE)), 
                                                                                      collapse = " ")), 
                                                    "Sales Executive", br(), "DOJ: 09-17-2015", width = 10))
                                    )
                             ),
                             column(7, h4(p(strong("Email body")), align = "left"),

                                    #output$prueba1 <- renderText({
                                         #               email_suspiscious[sel1(),7]
                                         #             })
                                    # output$prueba1 <- renderUI({email_suspiscious[sel1(),7]})
                                    output$prueba1 <- renderUI({
                                      body_bold <- str_replace(email_suspiscious[sel1(),7], 
                                                               dictionary[1], 
                                                               paste0("<b>",dictionary[1],"</b>"))
                                      
                                      for (i in 2:length(dictionary)){
                                        body_bold <- str_replace(body_bold, 
                                                                 dictionary[i], 
                                                                 paste0("<b>",dictionary[i],"</b>"))
                                      }
                                      HTML(body_bold)
                                    })
                             )        
                           ),
                           
                           fluidPage(fluidRow(h4(p(strong("Actions")))),
                                     
                                     fluidRow( 
                                       actionButton("Unfollow", "Unfollow", 
                                                    style="color: #fff; background-color: rgb(147,149,152); border-color: #ffffff ; padding:10px ; width: 250px")
                                       
                                       ,actionButton("Follow", "Follow", 
                                                     style="color: #fff; background-color: rgb(90,183,232); border-color: #ffffff ; padding:10px ; width: 250px")
                                       
                                       ,actionButton("Escalate", "Escalate", 
                                                     style="color: #fff; background-color: rgb(238,38,83); border-color: #ffffff ; padding:10px ; width: 250px")
                                       , align = "center")
                             ),

                           easyClose = TRUE
    ))})
  #----------------------------------------------------------------------------------------------------------
  # Escalate button -----------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------
  observeEvent(input$Escalate, {
      
    showModal(modalDialog(size = "l",
                          title = "Email Escalation" %>% strong() %>% h3(),
                          fluidPage(
                            fluidRow("Are you sure you want to escalate this email?" %>% strong() %>% h4(), align = "left"),
                            fluidRow( h5("From: Early Fraud Detection Team"),
                                      h5("\nTo: Compliance Manager"),
                                      br(),
                                      h5("Hello,"),
                                      h5("\nWe've detected the following suspicious email activity. Please evaluate if any further action is required."),
                                      br(),
                                      h5("Thank you."),
                                      h5("\nEarly Fraud Detection Team")),
                            br(),
                            fluidRow(actionButton("send_email", "Send email", 
                                         style="color: #fff; background-color: rgb(90,183,232); border-color: #ffffff ; padding:10px ; width: 250px"),
                                      align = "center")
                          )
                          )
              )   
    })
  #----------------------------------------------------------------------------------------------------------
  # Sending escalate email-----------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------
  
  observeEvent(input$send_email, {

    txt_file <- paste(paste0("From: ", email_suspiscious[sel1(),2]),
                  paste0("To: ", email_suspiscious[sel1(),3]),
                  paste0("Subject: ", email_suspiscious[sel1(),4]),
                  paste0("Date: ", email_suspiscious[sel1(),5]),
                  paste0("Body: ", email_suspiscious[sel1(),7]),
                  sep = "\n")
    
    write(txt_file, file = "email.txt")
    
    # init com api
    OutApp <- COMCreate("Outlook.Application")
    ## create an email
    outMail = OutApp$CreateItem(0)
    ## configure  email parameter

    outMail[["To"]] = "matias.gutierrez@evalueserve.com"
    #outMail[["cc"]] = ""
    outMail[["subject"]] = "Escalate Activity"
    outMail[["body"]] = paste("Hello", 
                              "\nWe've detected the following suspicious email activity. Please evaluate if any further action is required.",
                              "\nThank you.", 
                              "Early Fraud Detection Team", sep = "\n")
    #"Hello,\nWe've detected the following suspicious email activity. Please evaluate if any further action is required.\n\nThank you.\nEarly Fraud Detection Team"
    outMail[["Attachments"]]$Add(email_txt_path %>% paste("email.txt", sep = "\\"))
    
    ## send it
    outMail$Send()
    
    
  })
  #----------------------------------------------------------------------------------------------------------
  }
  






















