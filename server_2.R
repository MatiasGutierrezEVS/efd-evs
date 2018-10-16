library(shiny); library(shinydashboard); library(shinythemes); library(stringr)
library(DT); library(dplyr); library(magrittr); library(RMySQL); library(shinyjs) ; 
library("RDCOMClient"); library(visNetwork)

email_txt_path <- dirname(rstudioapi::getSourceEditorContext()$path)

load(paste0(email_txt_path, "/data/enron_training.RData"))

load(paste0(email_txt_path, "/data/email_feed_day1.RData"))
load(paste0(email_txt_path, "/data/email_feed_day1_detail.RData"))
# load(paste0(email_txt_path, "/data/data.RData"))
load(paste0(email_txt_path, "/data/class_mails.RData"))
load(paste0(email_txt_path, "/data/viz.RData"))

Previus_suspicious <- read.csv(paste0(email_txt_path,"/data/Previus_suspicious.csv"))


dictionary_2 <- read.csv2(paste0(email_txt_path, "/data/Fraud Keyword Dictionary WIP.csv"), header = FALSE,
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

dictionary_1 <- as.data.frame(c('lawyers', 'voice mail', 'stalking', 'special fees', 'don’t seem right',
                                'get rid of it', 'amount is convincing'))
colnames(dictionary_1) <- c('V1')
dictionary <- rbind(dictionary_2, dictionary_1)
dictionary <- as.character(dictionary$V1)

sus_value <- 87

str(nodes)

edge <- data %>% filter(id_f == 1 | id_t == 1)
node <- tibble(id = union(unique(edge$id_f), unique(edge$id_t)))
node <- left_join(node, nodes)
second_level <- node %>% filter(id != 1)
second_level_edges <- filter(data, id_f %in% second_level$id, is_s == 1)
sec_edge <- rbind(second_level_edges, edge)
node <- tibble(id = union(unique(sec_edge$id_f), unique(sec_edge$id_t)))
node <- left_join(node, nodes)
#Ordering and changing color
aux <- node %>% filter(nodos == "Charles McGill")
auxC <- node %>% filter(nodos != "Charles McGill")


nododos <- data.frame(id = as.integer(nodes$id),
                      label = nodes$nodos)

edgeges <- data.frame(from = data$id_f, to = data$id_t, is_s = data$is_s, color = data$color)

edgeges <- edgeges[order(edgeges$is_s, decreasing = TRUE), ]

edges0 <- edgeges %>% group_by(from, to) %>% summarise(value=n())

edges1 <- distinct(edgeges, from, to, .keep_all = T)

edges3 <- as.data.frame(merge(edges1,edges0))


  
  #Filtering the social network by worker
  id_selected <- 1
  edge <- edges3 %>%  filter(id_selected == from | id_selected == to)
  node <- data.frame(id=union(unique(edge$from),unique(edge$to)))
  node <- left_join(node,nododos)
  second_level <- node[!(node$id == id_selected),]
  second_level_edges <- filter(edges3,from %in% second_level$id)
  second_level_edges <- second_level_edges[!(second_level_edges$is_s == 0),]
  sec_edge <- rbind(second_level_edges,edge)
  node <- data.frame(id=union(unique(sec_edge$from),unique(sec_edge$to)))
  node <- left_join(node,nododos) %>% unique()
  # aux <- node %>%  filter("Charles McGill" == nodos)
  # auxC <- node %>%  filter("Charles McGill" != nodos)
  # node<- rbind(aux,auxC) %>% data.frame(color=sapply("lightBlue", function (x) rep(x,nrow(node)-1)) %>% rbind("pink", .) %>% as.vector() )
  # 

#============================================================================================================
#                                                 SERVER
#============================================================================================================



server <- function(input, output) {
  

  
  output$email_feed<- DT::renderDataTable({ 
    
    dat <- datatable(email_feed_day1, 
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
  
  observeEvent(c(input$email_feed_rows_selected,input$back_email), {
    showModal(modalDialog( size = "l",
                           title = fluidRow(column(9, h3(p(strong("Email View")))),
                                            column(3, paste0("Suspucious index: ", data2[sel1(),"sus_value"], "%"))),
                           fluidRow(
                             column(5,
                                    fluidRow(
                                      column(4, h4(strong("From")),img(src=paste0("pictures/", data2[sel1(),"pic_from"], ".PNG"), align = "left")),
                                      column(8, br(),br(),box(title = tools::toTitleCase( data2[sel1(),"name_from"]), 
                                                              data2[sel1(),"Role_From"], br(), data2[sel1(),"DOJ_From"], width = 10,
                                                              actionButton("Profile", "Click for Profile", width = 8,
                                                                           style="color: #66ccff; background-color: rgb(255, 255, 255); border-color: #ffffff ; padding:6px ; width: 130px"))
                                            
                                             )
                                    ),
                                    fluidRow(
                                      column(4, h4(strong("To")), img(src=paste0("pictures/", data2[sel1(),"pic_to"], ".PNG"), align = "left")),
                                      column(8, br(),br(),box(title = tools::toTitleCase(data2[sel1(),"name_to"]), 
                                                              data2[sel1(),"Role_To"], br(), data2[sel1(),"DOJ_To"], width = 10,
                                                              actionButton("Profile", "Click for Profile", width = 8,
                                                                           style="color: #66ccff; background-color: rgb(255, 255, 255); border-color: #ffffff ; padding:6px ; width: 130px")
                                                              )
                                             )
                                      )
                                    
                             ),
                             column(7, h4(p(strong("Email body")), align = "left"),
                                    
                                    #output$prueba1 <- renderText({
                                    #               email_suspiscious[sel1(),7]
                                    #             })
                                    # output$prueba1 <- renderUI({email_suspiscious[sel1(),7]})
                                    output$prueba1 <- renderUI({
                                      body_bold <- str_replace(data2[sel1(),"bodies"], 
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
  # Profile button -----------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------
  observeEvent(input$Profile, ignoreInit = TRUE, {
    
    showModal(modalDialog(size = "l",
                          title = "User Profile" %>% strong() %>% h3(),
                          fluidPage(
                            fluidRow(
                             
                                     
                               
                                       column(6, img(src=paste0("pictures/", data2[sel1(),"pic_from"], ".PNG"), align = "center"),
                                              br(), br(),hr(),
                                              box( width = 12,
                                                title = tools::toTitleCase( data2[sel1(),"name_from"]),  
                                                solidHeader = TRUE, status = "primary",
                                                tags$b("Role: "), data2[sel1(),"Role_From"], br(), 
                                                tags$b("Manager: "), data2[sel1(),"manager_from"], br(),
                                                tags$b("Department: "), data2[sel1(),"department_from"], br(),
                                                tags$b("DOJ: "), data2[sel1(),"DOJ_From"], br(),
                                                tags$b("Total Emails since DOJ: "), data2[sel1(),"total_emails_from"], br(),
                                                tags$b("Suspicious Emails since DOJ: "), data2[sel1(),"total_suspicious_emails_from"], br(),
                                                actionButton("suspicious_user", "Click Here to view",
                                                             style="color: #66ccff; background-color: rgb(255, 255, 255); 
                                                             border-color: rgb(255, 255, 255) ; padding:10px ; width: 150px"),
                                                br(),
                                                tags$b("Most Connected User: "), data2[sel1(),"most_connected_user_from"], br(), br(),
                                                tags$b("Connectedness: "), data2[sel1(),"Connectedness_from"],
                                                data2[sel1(),"Connectedness_from_summary"], br(), 
                                                tags$b("Betweeness: "), data2[sel1(),"Betweeness_from"],
                                                data2[sel1(),"Betweeness_from_summary"], br(),
                                                tags$b("Closeness: "), data2[sel1(),"Closeness_from"],
                                                data2[sel1(),"Closeness_from_summary"]
                                              )
                                       
                                       ),
                                       column(6, h3("Email behavior:"),
                                              selectInput(inputId= "range", 
                                                          label= "Range", 
                                                          choices=c("Last 7 days, Among Week", "Last 7 days, Among Day", 
                                                                    "Historical, Among Week", "Historical, Among Day"),
                                                          selected="Historical, Among Week"),
                                              renderPlot({
                                                
                                                
                                                if(input$range=="Last 7 days, Among Week"){    
                                                  filter_mail <- class_mails %>% filter(From=="phillip.allen@enron.com")
                                                  max_Date <-  max(filter_mail$Date)
                                                  filter_mail <-filter_mail %>% filter(Date > max_Date-(7*60*60*24) )
                                                  
                                                  x <- filter_mail$Weekday %>% ordered(levels=c("Monday", "Tuesday", 
                                                                                                "Wednesday", "Thursday", 
                                                                                                "Friday", "Saturday", "Sunday"))
                                                }
                                                if(input$range=="Last 7 days, Among Day"){
                                                  filter_mail <- class_mails %>% filter(From=="phillip.allen@enron.com")
                                                  max_Date <-  max(filter_mail$Date)
                                                  filter_mail <-filter_mail %>% filter(Date > max_Date-(7*60*60*24) )
                                                  x <- paste(filter_mail$Hour, ":00", sep = "")
                                                  x <- x %>% ordered(levels=paste(seq(max(24)), ":00", sep = ""))      
                                                }
                                                
                                                if(input$range=="Historical, Among Week"){
                                                  filter_mail <- class_mails %>% filter(From=="phillip.allen@enron.com")
                                                  x <- filter_mail$Weekday %>% ordered(levels=c("Monday", "Tuesday", 
                                                                                                "Wednesday", "Thursday", 
                                                                                                "Friday", "Saturday", "Sunday"))
                                                }
                                                if(input$range=="Historical, Among Day"){
                                                  filter_mail <- class_mails %>% filter(From=="phillip.allen@enron.com")
                                                  x <- paste(filter_mail$Hour, ":00", sep = "")
                                                  x <- x %>% ordered(levels=paste(seq(max(24)), ":00", sep = ""))      
                                                }
                                                
                                                
                                                barplot(height=table(x),
                                                        col = "#75AADB",
                                                        border = "white",
                                                        xlab = input$range %>% str_sub( start = 7, end = -1))
                                              })
                                     
                              )),
                              fluidRow(
                              column(12,
                                     
                                       column(12, h3("Social Network Map:"),
                                              renderVisNetwork({
                                                visNetwork(node, sec_edge, main = "Social Network Bank", width = "100%") %>%
                                                  visNodes(color = list(background = "lightblue", highlight = 'pink')) %>% 
                                                  visEdges(arrows = list(to = list(enabled = TRUE,  scaleFactor = 2, type = 'arrow'))) %>% 
                                                  visOptions( highlightNearest = list(enabled = TRUE, degree = 1, labelOnly = FALSE, hover = TRUE))
                                                
                                              })
                                       )
                                          







                                              )
             
                                       
                                              )
                                     )
                                     )
                            
                          
    )
    
    })
  
  #----------------------------------------------------------------------------------------------------------
  # suspicious_user button -----------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------
  observeEvent(input$suspicious_user, {
    showModal(modalDialog
              (size = "l", footer = "",
                          title = "Suspicious Emails: Charles McGill" %>% strong() %>% h3(),
                
                renderDataTable(Previus_suspicious),
                actionButton("back_email", "Back to Email",
                             style="color: #66ccff; background-color: rgb(255, 255, 255); 
                                                             border-color: rgb(255, 255, 255) ; padding:10px ; width: 150px")

    )
    )   
  })
                                              
  #----------------------------------------------------------------------------------------------------------
  # Escalate button -----------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------
  observeEvent(input$Escalate, {
    
    showModal(modalDialog(size = "l", footer = modalButton("Back"),
                          title = "Email Escalation" %>% strong() %>% h3(),
                          fluidPage(
                            fluidRow("Are you sure you want to escalate this email?" %>% strong() %>% h4(), align = "left"),
                            fluidRow( h5("From: Early Fraud Detection Team"),
                                      h5("\nTo: Compliance Manager"),
                                      br(),
                                      h5("Hello,"),
                                      h5("\nWe’ve detected the following suspicious email activity. Please evaluate if any further action is required."),
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
    #"Hello,\nWe’ve detected the following suspicious email activity. Please evaluate if any further action is required.\n\nThank you.\nEarly Fraud Detection Team"
    outMail[["Attachments"]]$Add(email_txt_path %>% paste("email.txt", sep = "\\"))
    
    ## send it
    outMail$Send()
    
    
  })
  #----------------------------------------------------------------------------------------------------------
}







