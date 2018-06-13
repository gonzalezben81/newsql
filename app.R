#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(RMySQL)
library(xlsx)
library(sqldf)


killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}

on.exit(killDbConnections())


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
                  dashboardHeader(title = "Sql Interface"),
   
   # Application title
   dashboardSidebar(
     sidebarMenu(
       ##Tab One
       menuItem(tabName = "sqlconnect",text = "Database Connection",icon = icon("gears"),selected = "True"),
       ##Tab Two
       menuItem(tabName = "sqlinput",text = "SQL Input:"),
       ##Tab Three
       menuItem(tabName = "sqloutput",text = "SQL Output:"),
       ##Tab Four
       menuItem(tabName = "sqltable",text = "List of Database Tables:"),
       ##Tab Five
       menuItem(tabName = "csv",text = "Upload Table Data")
       
     )
     
     
   ),
   
  
      # Show a plot of the generated distribution
      dashboardBody(
        tabItems(
        ##Tab One
         tabItem(tabName = "sqlconnect",
                 helpText("Enter database information here:"),
                 textInput(inputId = "database",label = "Database",value = "rstudio"),
                 textInput(inputId = "url",label = "Enter database url:",placeholder = "www.sql.com"),
                 textInput(inputId = "username",label = "Username:",placeholder = "kevin"),
                 passwordInput(inputId = "password",label = "Password:",placeholder = "secret"),
                 actionButton(inputId = "button", label="Connect to Database", class = "butt"),
                 actionButton(inputId = "button2",label = "Disconnect from Database",class ="butt")),
         ##Tab Two
         tabItem(tabName = "sqlinput",
                 textInput(inputId = "inputone",label = "Enter SQL Input:",placeholder = "Insert () Into"),
                 actionButton(inputId = "button3",label = "Upload Data")
                 ),
         ##Tab Three
         tabItem(tabName = "sqltable",
                 tableOutput("tables")),
         ##Tab Four
         tabItem(tabName = "sqloutput",
                 textInput(inputId = "inputtwo",label = "Enter SQL Output:",placeholder = "Select * From"),
                 actionButton(inputId = "button4",label = "Query"),
                 br(),
                 br(),
                 textInput(inputId = "filename",label = "File Name:",placeholder = "SQL Data"),
                 br(),
                 downloadButton(outputId = "download",label = "Download Query"),
                 br(),
                 br(),
                 #tableOutput("tbl"))
                 dataTableOutput("tbl")),
         ##Tab Five
         tabItem(tabName = "csv",
                 fileInput(inputId = "file1",label =  "Choose File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv",
                             ".xlsx")
                 ),
                 br(),
                 textInput(inputId = "dbname",label = "Table Name:"),
                 br(),
                 actionButton(inputId = "go",label = "Upload Data!"),
                 br(),
                 br(),
                 DT::dataTableOutput("yellow"))
         
      ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  ford <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath)
  })
  
       output$yellow<-DT::renderDataTable({
         DT::datatable(
           ford(),extensions = 'Buttons', options = list(
             dom = 'Bfrtip',
             buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
           ))

      })
       
    observeEvent(input$go,{
      
      
      withProgress(message = 'Writing Table',
                   value = 0, {
                     for (i in 1:3) {
                       incProgress(1/3)
                       Sys.sleep(0.75)
                     }
                   },env = parent.frame(n=1))
      db <- dbConnect(MySQL(), dbname = input$database, host = input$url, 
                      user = input$username, 
                      password = input$password)
      df2 <- data.frame( 
        ford(), 
        row.names = NULL)
      
      dbWriteTable(conn = db,name = input$dbname,value = df2)
    })
       
  
  # output$yellow <- renderTable({
  #   inFile <- input$file1
  #   
  #   if (is.null(inFile))
  #     return(NULL)
  #   
  #   read.csv(inFile$datapath)
  # })
  
  ##Connect to MySQL Database
   
  observeEvent(input$button,{
    
    withProgress(message = 'Connecting to Database',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.75)
                   }
                 },env = parent.frame(n=1))
    db <- dbConnect(MySQL(), dbname = input$database, host = input$url, 
                    user = input$username, 
                    password = input$password)
    dbDisconnect(conn = db)
    
  })
  
  
  
  ##Disconnect from MySQL Database
  observeEvent(input$button2,{
    all_cons <- dbListConnections(MySQL())
    withProgress(message = paste(length(all_cons), " connections removed."),
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.75)
                   }
                 },env = parent.frame(n=1))
    killDbConnections()
  })
  
  
  ##SQL Input
  
  ##SQL Output
  tbl <- eventReactive(input$button4, {
    
    
    withProgress(message = 'Querying the Database',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.75)
                   }
                 },env = parent.frame(n=1))
    con <- dbConnect(MySQL(), dbname = input$database, host = input$url, 
                     user = input$username, 
                     password = input$password)
    
    query <- paste0(input$inputtwo,";")
    dbGetQuery(con,query)
    #dbDisconnect(conn = con)
  })
  
  ##Ouput SQL Query Results
  
  tables<- eventReactive(input$button,{
    db <- dbConnect(MySQL(), dbname = input$database, host = input$url, 
                    user = input$username, 
                    password = input$password)
    
    dbListTables(db)
    # db<- matrix(data = db,ncol = 1)
    # db
  })
  

  
  #output$table <- renderTable(tables())

   output$tbl<-DT::renderDataTable({

     DT::datatable(
       tbl(),extensions = 'Buttons', options = list(
         dom = 'Bfrtip',lengthMenu = c(10, 15, 20),
         buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
       ))

   })
 
  
  ##Download SQL Query Results

   tbl2 <- reactive({
   con <- dbConnect(MySQL(), dbname = input$database, host = input$url, 
                    user = input$username, 
                    password = input$password)
   query <- paste0(input$inputtwo,";")
   dbGetQuery(con,query)})
   
   
   output$download <- downloadHandler(
     filename = function() { paste(input$filename, sep='',".csv") },
     content = function(file) {
       # con <- dbConnect(MySQL(), dbname = input$database, host = input$url, 
       #                  user = input$username, 
       #                  password = input$password)
       # query <- paste0(input$inputtwo,";")
       # tbl2<- dbGetQuery(con,query)
       # tbl2 <- as.matrix(tbl2)
       
       
       write.csv(x = tbl2(),file = file)
       
     })
   
   

   ##SQL Tables in the Database
   
   # tables <- eventReactive(input$button5, {
   #   con <- dbConnect(MySQL(), dbname = input$database, host = input$url,
   #                  user = input$username,
   #                  password = input$password)
   #  dbListTables(con)
   #  
   #  
   # })
   # 
   # 
   output$tables <-renderTable(tables())
   
    #tables <- reactive(as.matrix.data.frame(tables()))
# 
#      output$table<-DT::renderDataTable({
#        DT::datatable(
#          tables,extensions = 'Buttons', options = list(
#            dom = 'Bfrtip',
#            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
#          ))
#    
#     })
   
   
}











# Run the application 
shinyApp(ui = ui, server = server)

