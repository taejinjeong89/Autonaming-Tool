#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(stringr)
library(readxl)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plyr)
library(dplyr)
library(xlsx)
library(plotly)

## Database1

saveData1 = function(data) {
  data = as.data.frame(t(data))
  if (exists("responses1")) {
    responses1 <<- rbind(responses1, data)
    responses1 <<- data.frame(na.omit(responses1))
  } else {
    responses1 <<- data
  }
}

saveData2 = function(data) {
  data = as.data.frame(t(data))
  if (exists("responses2")) {
    responses2 <<- rbind(responses2, data)
    responses2 <<- data.frame(na.omit(responses2))
  } else {
    responses2 <<- data
  }
}

saveData3 = function(data) {
  data = as.data.frame(t(data))
  if (exists("responses3")) {
    responses3 <<- rbind(responses3, data)
    responses3 <<- data.frame(na.omit(responses3))
  } else {
    responses3 <<- data
  }
}

addData = function(data){
  data = as.character(t(data))
  addstd = sort(append(std_total[[data[1]]][,data[3]], data[2]))
  exsstd = std_total[[data[1]]]
  if (length(addstd) > nrow(exsstd)){
    exsstd = rbind(exsstd, rep(NA, ncol(exsstd)))
    exsstd[,data[3]][seq_len(length(addstd))] = addstd
  }else{
    exsstd[,data[3]][seq_len(length(addstd))] = addstd
  }
  std_total[[data[1]]] <<- exsstd
}


dropData1 = function(data) {
  
  if (exists("responses1")) {
    responses1 <<- responses1[!(responses1$V1 == data[,1] 
                              & responses1$V2 == data[,2] 
                              & responses1$V3 == data[,3]),]
    responses1 <<- data.frame(na.omit(responses1))
  }
}

dropData2 = function(data) {
  
  if (exists("responses2")) {
    responses2 <<- responses2[!(responses2$V1 == data[,1] 
                                & responses2$V2 == data[,2] 
                                & responses2$V3 == data[,3]),]
    responses2 <<- data.frame(na.omit(responses2))
  }
}

dropData3 = function(data) {
  
  if (exists("responses3")) {
    responses3 <<- responses3[!(responses3$V1 == data[,1] 
                                & responses3$V2 == data[,2] 
                                & responses3$V3 == data[,3]
                                & responses3$V4 == data[,4]),]
    responses3 <<- data.frame(na.omit(responses3))
  }
}

loadData1 = function() {
  if (exists("responses1")) {
    responses1
  }
}

loadData2 = function() {
  if (exists("responses2")) {
    responses2
  }
}

loadData3 = function() {
  if (exists("responses3")) {
    responses3
  }
}

comb_new1 = c()
comb_new2 = c()



## UI

ui = dashboardPage(
  skin = "black",
  dashboardHeader(title = "Dashboard",
                  dropdownMenu(type = "notifications")),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "search-text", buttonId = "serchButton", label = "Search..."),
      menuItem("Main", tabName = "main", icon = icon("dashboard")),
      menuItem("Reports", tabName = "reports", icon = icon("bar-chart-o")),
      menuItem("System", tabName = "system", icon = icon("cog"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # Tab 1 content
      tabItem(tabName = "main",
              fluidPage(
                shinyjs::useShinyjs(),
                sidebarLayout(
                  sidebarPanel(
                    img(
                      src = "download.png", 
                      height = "100%", width = "100%"
                    ),
                    textInput(
                      "t1_text1",
                      h3("Search for Standardization")
                    ),
                    textOutput(
                      "t1_guess"
                    ),
                    selectInput(
                      "t1_select", 
                      h3("Standardization"),
                      choices = choice
                    ),
                    textInput(
                      "t1_text2", 
                      h3("KTS")
                    )
                  ),
                  mainPanel(
                    uiOutput("t1_result1"),
                    uiOutput("t1_result2")
                  )
                )
              )
      ),
      
      # Tab 2 content
      tabItem(tabName = "reports",
              fluidPage(
                shinyjs::useShinyjs(),
                fluidRow(
                  box(
                    width = "100%",
                    selectInput(
                      width = "100%",
                      "t2_select",
                      h3("Select"),
                      choices = append("TOTAL",choice)
                    ),
                    sliderInput(
                      width = "100%",
                      "t2_slider",
                      h3("Number of Words"),
                      min = 1, max = 50, value = 20
                    ),
                    numericInput(
                      width = "100%",
                      "t2_numeric",
                      h3("Minimum Frequency"),
                      value = 1
                    )#,
                    #div(actionButton("t2_refresh", label = "Refresh"),
                    #    align = "right"
                    #)
                  )
                ),
                fluidRow(
                  box(
                    width = "100%",
                    tabsetPanel(
                      tabPanel("Raw New Words",
                               br(),
                               box(
                                 title = "Abbreviation Escalated",
                                 solidHeader = TRUE,
                                 status = "warning",
                                 DT::dataTableOutput("t2_responses1", width = "auto")
                                 ),
                               box(
                                 title = "Standardization Escalated",
                                 solidHeader = TRUE,
                                 status = "warning",
                                 DT::dataTableOutput("t2_responses2", width = "auto")
                                 )
                               ),
                      tabPanel("Abbreviation Update",
                               br(),
                               box(
                                 title = "Abbreviation Report",
                                 status = "success",
                                 solidHeader = TRUE,
                                 div(style="display:inline-block", DT::dataTableOutput("t2_table1"))
                                 ),
                               box(
                                 title = "Histogram for Abbreviation",
                                 status = "success",
                                 solidHeader = TRUE,
                                 plotOutput(outputId = "t2_plot1", height = "605")
                                 ),
                               tableOutput("t2_abbtable"),
                               div(
                                 div(style="display:inline-block",downloadButton(outputId = "t2_download2", 
                                                                                 label = "Abbreviation Download")),
                                 align = 'right'
                                 )
                               ),
                      tabPanel("Standardization Update",
                               br(),
                               box(
                                 title = "Standardization Report",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 div(style="display:inline-block", DT::dataTableOutput("t2_table2"))
                                 ),
                               box(
                                 title = "Histogram for Standardization",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 plotOutput(outputId = "t2_plot2", height = "605")
                                 ),
                               br(),
                               tableOutput("t2_stdtable"),
                               div(
                                 div(style="display:inline-block",downloadButton(outputId = "t2_download1", 
                                                                                 label = "Standardization Download")),
                                 align = 'right'
                                 )
                               ),
                      tabPanel("Error Report Update",
                               br(),
                               box(
                                 title = "Error Report",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 div(style="display:inline-block", DT::dataTableOutput("t2_table3")),
                                 width = "100%"
                               ),
                               box(
                                 title = "Pie Chart for Error",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 plotlyOutput(outputId = "t2_plot3", height = "605")
                               ),
                               box(
                                 title = "Issue",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 plotlyOutput(outputId = "t2_plot4", height = "605")
                               )
                      )
                    )
                  )
                )
              )
      ),

      # tab 3 content
      tabItem(tabName = "system",
              fluidPage(
                fluidRow(
                  box(
                    "This page will be updated"
                  )
                ),
                fluidRow(
                  box(
                    
                  )
                  )
                )
              )

      )
    )
  )

server = function(input,output){
  
  ## Database
  
  # Whenever a field is filled, aggregate all form data
  
  formdata1 = reactive({
    rawnew1 = word(toupper(input[["escabb1"]]),1:10,sep = ",")[!is.na(word(toupper(input[["escabb1"]]),1:10,sep = ","))]
    cornew1 = word(toupper(input[["escabb2"]]),1:10,sep = ",")[!is.na(word(toupper(input[["escabb2"]]),1:10,sep = ","))]
    if (length(rawnew1) == 0){
      data1 = c()
    }else if (length(rawnew1) == 1){
      data1 = c(input[["t1_select"]], toupper(input[["escabb1"]]), toupper(input[["escabb2"]]))
    }else{
      for (i in 1:length(rawnew1)){
        new1 = c(input[["t1_select"]], 
                rawnew1[i], 
                cornew1[i])
        comb_new1 = rbind(comb_new1, new1)
      }
      data1 = t(comb_new1)
    }
    data1
  })
  
  formdata2 = reactive({
    rawnew2 = word(toupper(input[["escstd1"]]),1:10,sep = ",")[!is.na(word(toupper(input[["escstd1"]]),1:10,sep = ","))]
    cornew2 = word(toupper(input[["escstd2"]]),1:10,sep = ",")[!is.na(word(toupper(input[["escstd2"]]),1:10,sep = ","))]
    if (length(rawnew2) == 0){
      data2 = c()
    }else if (length(rawnew2) == 1){
      data2 = c(input[["t1_select"]], toupper(input[["escstd1"]]), toupper(input[["escstd2"]]))
    }else{
      for (i in 1:length(rawnew2)){
        new2 = c(input[["t1_select"]], 
                rawnew2[i], 
                cornew2[i])
        comb_new2 = rbind(comb_new2, new2)
      }
      data2 = t(comb_new2)
    }
    data2
  })
  
  formdata3 = reactive({
    report = toupper(input[["escreport"]])
    if (length(report) == 0){
      data3 = c()
    }else{
      data3 = c(input[["t1_select"]], toupper(input[["t1_text2"]]), 
                FinalFunc(input$t1_text2, input$t1_select)[[3]], toupper(input[["escreport"]]))
    }
    data3
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$t1_submit1, {
    if (input$escabb1 != ""){
      saveData1(formdata1())
    }
    if (input$escstd1 != ""){
      saveData2(formdata2())
    }
    if (input$escreport != ""){
      saveData3(formdata3())
    }
  })
  
  ## UI
  
  ## Tabs
  
  # Tab 1
  
  observe({
    shinyjs::toggleState("t1_submit1", !is.null(input$t1_text2) && input$t1_text2 != "")
  })
  
  output$warning1 = renderText("If you want to add more than two words in one time,")
  output$warning2 = renderText("please seperate words by ','")
  output$warning3 = renderText("(EX, RING,BALL into RINGS,BALLS)")
  output$warning4 = renderText("Use format as MAINWORD/NOTES (EX, BAO/BAO is duplicated)")
  
  observe({
    if (is.null(input$t1_text2) || input$t1_text2 == ""){
      shinyjs::hide("t1_result2")
    }else{
      shinyjs::show("t1_result2")
    }
  })
  
  observe({
    if (is.null(input$t1_text1) || input$t1_text1 == ""){
      shinyjs::hide("t1_result1")
    }else{
      shinyjs::show("t1_result1")
    }
  })
  
  checkstd = function(word){
    check.num = c()
    word = toupper(word)
    for (i in seq_len(length(check.std))){
      if (any(word == check.std[[i]])){
        check.num = append(check.num, i)
      }
    }
    return(cat[,1:3][check.num,])
  }
  
  Guessstd = reactive({
    checkstd(input$t1_text1)
  })
 
  output$t1_result1 = renderUI({
    fluidPage(
      shinyjs::useShinyjs(),
      box(
        width = "100%",
        tableOutput("t1_table"),
        div(
          actionButton("t1_clear", label = "Clear"),
          align = "Right"
        )
      )
    )
  })
  
  
  output$t1_table = renderTable({
    Guessstd()
  })

  observeEvent(input$t1_clear, {
    shinyjs::reset("t1_text1")
  })
  
  output$t1_result2 = renderUI({
    fluidPage(
      shinyjs::useShinyjs(),
      fluidRow(
        box(title = "Guessname",
            status = "primary",
            solidHeader = TRUE,
            strong(FinalFunc(input$t1_text2, input$t1_select)[[3]]),
            width = "100%"
            )
      ),
      
      #fluidRow(
      #  box(title = "Notes",
      #      status = "primary",
      #      solidHeader = TRUE,
      #      strong(Note_total[[input$t1_select]]),
      #      width = "100%"
      #      )
      #),
      
      fluidRow(
        box(title = "Details",
            status = "primary",
            solidHeader = TRUE,
            width = "100%",
            
            box("Standardized Word",
                br(),
                strong(paste(FinalFunc(input$t1_text2, input$t1_select)[[4]], collapse = ", ")),
                br(),
                br(),
                "Word Added",
                br(),
                strong(paste(FinalFunc(input$t1_text2, input$t1_select)[[6]], collapse = ", ")),
                br(),
                br(),
                "Word Dropped",
                br(),
                strong(paste(FinalFunc(input$t1_text2, input$t1_select)[[5]], collapse = ", "))
                ),
            box("IPC Category",
                br(),
                strong(FinalFunc(input$t1_text2, input$t1_select)[[2]]),
                br(),
                br(),
                "Standardization Used",
                br(),
                strong(FinalFunc(input$t1_text2, input$t1_select)[[1]])
                )
            )
        ),
      fluidRow(
        box(title = "Notes Used",
            status = "primary",
            solidHeader = TRUE,
            width = "100%",
            strong(paste(FinalFunc(input$t1_text2, input$t1_select)[[9]], collapse = ", "))
        )
      ),
      
      fluidRow(
        box(title = "Word to escalate",
            status = "primary",
            solidHeader = TRUE,
            width = "100%",
            
            box("Word not in Standardization",
                br(),
                br(),
                strong(paste(FinalFunc(input$t1_text2, input$t1_select)[[7]], collapse = ", ")),
                br(),
                br(),
                textOutput('warning1'),
                textOutput('warning2'),
                textOutput('warning3'),
                tags$style("#warning1{color: red}","#warning2{color: red}","#warning3{color: red}")
            ),
            box(bootstrapPage(
                  div(style="display:inline-block",textInput(inputId = "escabb1", label="Raw New Words")),
                  div(style="display:inline-block",textInput(inputId = "escabb2", label="Abbreviation"))
                ),
                bootstrapPage(
                  div(style="display:inline-block",textInput(inputId = "escstd1", label="Standardization")),
                  div(style="display:inline-block",selectInput(inputId = "escstd2", label = "Definition Column", 
                                                               choices = append(colnames(std_total[[input$t1_select]]),"ETC"),
                                                               width = 160))
                ),
                textInput(inputId = "escreport", label = "Report Note"),
                textOutput('warning4'),
                tags$style("#warning4{color: red}")
            ),
            div(actionButton("t1_done", label = "Done"),
                actionButton("t1_submit1", label = "Submit"),
                align = "right"
                )
        )
      )
    )
  })
  
  observeEvent(input$t1_done, {
    shinyjs::reset("t1_select")
    shinyjs::reset("t1_text")
    shinyjs::reset("t1_text2")
    shinyjs::reset("t1_result2")
  }) 
  
  # Tab 2
  
  output$t2_responses1 = DT::renderDataTable(
    datatable(rownames = FALSE,
              class = 'cell-border stripe',
              colnames = c("Standardization", "NewWord", "Correction", "Frequency"),
              filter = 'top',
              options = list(
                autoWidth = TRUE
              ),
              {input$t1_submit1
                plyr::count(loadData1())
              }
    )
  )
  
  output$t2_responses2 = DT::renderDataTable(
    datatable(rownames = FALSE,
              class = 'cell-border stripe',
              colnames = c("Standardization", "NewSTD", "Column", "Frequency"),
              filter = 'top',
              options = list(
                autoWidth = TRUE
              ),
              {input$t1_submit1
                plyr::count(loadData2())
              }
    )
  )
  
  shinyInput = function(FUN, len, id, ...){
    inputs = character(len)
    for (i in seq_len(len)){
      inputs[i] = as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  shinynum = function(len, id, ...){
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(paste0(id,i), ...)
    }
    inputs
  }
  
  filterData = function(data){
    if (input$t2_select == "TOTAL"){
      head(data[data[,4] >= input$t2_numeric,][order(
        data[data[,4] >= input$t2_numeric,]$Frequency, decreasing = TRUE),], 
        input$t2_slider)
    }else{
      head(data[data[,1] == input$t2_select & data[,4] >= input$t2_numeric,][order(
        data[data[,1] == input$t2_select & data[,4] >= input$t2_numeric,]$Frequency, decreasing = TRUE),], 
        input$t2_slider)
    }
  }
  
  splitNote1 = function(data){
    if (is.na(word(data,1:2,sep = "/")[2])){
      "ETC" 
    }else{
      word(data,1:2,sep = "/")[1]
      }
  }
  splitNote2 = function(data){
    if (is.na(word(data,1:2,sep = "/")[2])){
      word(data,1:2,sep = "/")[1]
    }else{
      word(data,1:2,sep = "/")[2]
    }
  }
  
  df1 = reactiveValues(data = data.frame(
    STD = plyr::count(loadData1())$V1,
    NewWord = plyr::count(loadData1())$V2,
    Correction = plyr::count(loadData1())$V3,
    Frequency = plyr::count(loadData1())$freq,
    Update = shinyInput(actionButton, nrow(plyr::count(loadData1())), 
                              'button_', label = "Update", onclick = 'Shiny.onInputChange(\"select_button11\",  this.id)' ),
    Delete = shinyInput(actionButton, nrow(plyr::count(loadData1())), 
                        'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"select_button12\",  this.id)' ),
    Num = shinynum(nrow(plyr::count(loadData1())), 'num_')
  ))
  
  df2 = reactiveValues(data = data.frame(
    STD = plyr::count(loadData2())$V1,
    NewSTD = plyr::count(loadData2())$V2,
    Request = plyr::count(loadData2())$V3,
    Frequency = plyr::count(loadData2())$freq,
    Update = shinyInput(actionButton, nrow(plyr::count(loadData2())), 
                              'button_', label = "Update", onclick = 'Shiny.onInputChange(\"select_button21\",  this.id)' ),
    Delete = shinyInput(actionButton, nrow(plyr::count(loadData2())), 
                        'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"select_button22\",  this.id)' ),
    Num = shinynum(nrow(plyr::count(loadData2())), 'num_')
  ))
  
  df3 = reactiveValues(data = data.frame(
    STD = loadData3()$V1,
    KTS = loadData3()$V2,
    Guessname = loadData3()$V3,
    Main = sapply(loadData3()$V4, splitNote1),
    Note = sapply(loadData3()$V4, splitNote2),
    Delete = shinyInput(actionButton, nrow(loadData3()), 
                        'button_', label = "Delete", onclick = 'Shiny.onInputChange(\"select_button32\",  this.id)' ),
    Num = shinynum(nrow(loadData3()), 'num_'),
    Total = loadData3()$V4
  ))
  
  output$t2_table1 = DT::renderDataTable(
    filterData(df1$data)[,c(-1,-7)],
    rownames = FALSE,
    server = TRUE, escape = FALSE, selection = "none"
  )
  
  output$t2_table2 = DT::renderDataTable(
    filterData(df2$data)[,c(-1,-7)],
    rownames = FALSE,
    server = TRUE, escape = FALSE, selection = "none"
  )
  
  output$t2_table3 = DT::renderDataTable(
    df3$data[,c(-7,-8)],
    rownames = FALSE,
    server = TRUE, escape = FALSE, selection = "none"
  )

  observeEvent(input$select_button11, {
    selectedRow1 = as.numeric(strsplit(input$select_button11, "_")[[1]][2])
    checknum1 = paste0("num_", selectedRow1)
    if (df1$data[df1$data[,7] == checknum1, ][2] != "" & df1$data[df1$data[,7] == checknum1, ][3] != ""){
      Abb_total[[input$t2_select]] <<- unique(rbind(Abb_total[[input$t2_select]], df1$data[df1$data[,7] == checknum1, 2:3]))
    }
    dropData1(df1$data[df1$data[,7] == checknum1, ])
    df1$data <<- df1$data[df1$data[,7] != checknum1, ]
  })
  
  observeEvent(input$select_button12, {
    selectedRow1 = as.numeric(strsplit(input$select_button12, "_")[[1]][2])
    checknum1 = paste0("num_", selectedRow1)
    dropData1(df1$data[df1$data[,7] == checknum1, ])
    df1$data <<- df1$data[df1$data[,7] != checknum1, ]
  })
  
  observeEvent(input$select_button21, {
    selectedRow2 = as.numeric(strsplit(input$select_button21, "_")[[1]][2])
    checknum2 = paste0("num_", selectedRow2)
    addData(df2$data[df2$data[,7] == checknum2,])
    dropData2(df2$data[df2$data[,7] == checknum2, ])
    df2$data <<- df2$data[df2$data[,7] != checknum2, ]
  })
  
  observeEvent(input$select_button22, {
    selectedRow2 = as.numeric(strsplit(input$select_button22, "_")[[1]][2])
    checknum2 = paste0("num_", selectedRow2)
    dropData2(df2$data[df2$data[,7] == checknum2, ])
    df2$data <<- df2$data[df2$data[,7] != checknum2, ]
  })
  
  observeEvent(input$select_button32, {
    selectedRow3 = as.numeric(strsplit(input$select_button32, "_")[[1]][2])
    checknum3 = paste0("num_", selectedRow3)
    dropData3(df3$data[df3$data[,7] == checknum3, ][,c(1,2,3,8)])
    df3$data <<- df3$data[df3$data[,7] != checknum3, ]
  })
  
  output$t2_plot1 = renderPlot({
    ggplot(filterData(df1$data)[,-7], aes(x = NewWord, y = Frequency)) +
      geom_bar(stat = "identity", fill = "dark green") +
      labs(title = "Frequencies of Selected New Words",
           subtitle = "New Words VS Frequencies") +
      theme(axis.text.x = element_text(angle = 45, size = 15,vjust = 0.6))
    
  })
  
  output$t2_plot2 = renderPlot({
    
    ggplot(filterData(df2$data)[,-7], aes(x = NewSTD, y = Frequency)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Frequencies of Selected STD requests",
           subtitle = "STD requests VS Frequencies") +
      theme(axis.text.x = element_text(angle = 45, size = 15,vjust = 0.6))
  })

  
  output$t2_plot3 = renderPlotly({
    
    plot_ly(plyr::count(loadData3()[,1]), labels = ~x, values = ~freq, type = 'pie') %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
#  output$t2_plot4 = renderPlotly({
#    
#    a = df3$data[df3$data == input$t2_select,]
#    plot_ly(plyr::count(a), labels = ~Main, values = ~freq, type = 'pie') %>%
#      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#  })

  
  datasetInput1 = reactive({
    showdata1 = std_total[[input$t2_select]]
    showdata1[is.na(showdata1)] = ""
    showdata1
  })
  
  datasetInput2 = reactive({
    showdata2 = Abb_total[[input$t2_select]]
    showdata2 = data.frame(cbind(Abb_total[[input$t2_select]][seq_len(ceiling(nrow(showdata2)/4)),], 
                                 Abb_total[[input$t2_select]][seq_len(ceiling(nrow(showdata2)/4)) + ceiling(nrow(showdata2)/4),],
                                 Abb_total[[input$t2_select]][seq_len(ceiling(nrow(showdata2)/4)) + 2 * ceiling(nrow(showdata2)/4),], 
                                 Abb_total[[input$t2_select]][seq_len(ceiling(nrow(showdata2)/4)) + 3 * ceiling(nrow(showdata2)/4),]))
    showdata2[is.na(showdata2)] = ""
    colnames(showdata2) = rep(c("Raw Words", "Abbreviations"), 4)
    showdata2
  })
  
  output$t2_stdtable = renderTable({
    datasetInput1()
  })
  
  output$t2_abbtable = renderTable({
    datasetInput2()
  })
  
  output$t2_download1 = downloadHandler(
    
    filename = function(){
      paste("Standardization ", input$t2_select, " ", format(Sys.time(), "%m-%d-%y"),".csv", sep = "")
    },
    content = function(file){
      write.csv(datasetInput1(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$t2_download2 = downloadHandler(
    
    filename = function(){
      paste("Abbreviation ",input$t2_select, " ", format(Sys.time(), "%m-%d-%y"),".csv", sep = "")
    },
    content = function(file){
      write.csv(datasetInput2(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  # tab 3

}

shinyApp(ui, server)