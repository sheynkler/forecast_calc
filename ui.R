


library(shinythemes)
library(shiny)
library(shinyjs)
#library(lubridate)
library(dygraphs)

list_hh <- paste(c(paste0("0", c(0:9)), c(10:23)), "00", sep = ":")
list_hh_selected <- paste0(substr(Sys.time(), 12, 13), ":00")
width_input <- "100px"
shinyUI(
  fluidPage(
    theme = shinytheme("cerulean"),
    includeCSS("css/mycss.css"),
    tags$head(includeScript("google-analytics.js")),
    useShinyjs(),
    
    # Application title
    titlePanel("Time Serial Prediction Modeling: TV Viewing Prognose Calculator"),
    br(),
    
    # Sidebar with a slider input for number of bins
    
    fluidRow(
      column(
        2,
        div(
          id = "login_div",
          textInput("login", label = h3("Login:"), value = NULL),
          passwordInput("password", "Password:"),
          textOutput("login_error"),
          actionButton("login_button", "Login")
          
          
        ),
        selectInput(
          "names_lm",
          label = h3("Choose a group"),
          choices = as.list(names_target),
          selectize = F
        ),
        h3("Start"),
        div(
          div(class = "enter_data",
              dateInput(
                "date1", label = NULL, value = Sys.Date()
              )),
          div(
            class = "enter_time",
            selectInput(
              "hh1",
              label = NULL,
              choices = as.list(list_hh),
              selectize = F,
              selected = list_hh_selected,
              width = width_input
            )
          )
        ),
        h3("Stop"),
        div(
          div(class = "enter_data",
              dateInput(
                "date2", label = NULL, value = Sys.Date() + 6
              )),
          div(
            class = "enter_time",
            selectInput(
              "hh2",
              label = NULL,
              choices = as.list(list_hh),
              selectize = F,
              selected = list_hh_selected
            )
          )
        ),
        
        
        
        
        h3("Selected:"),
        div(h4(textOutput("value")))
        
        
      ),
      
      
      column(10,
             
             shinyjs::hidden(div(
               id = "content",
               fluidRow(
                 column(
                   12,
                   align = "center",
                   fluidRow(
                     column(6,
                                               selectInput(
                     "select_plot",
                     label = NULL,
                     choices = list(
                       "R plot" = 1,
                       "Dygraph plot" = 2 #, "All group" = 3
                     ),
                     selected = 2
                   )
                            ),
                     column(6,
                            conditionalPanel("input.select_plot == 1",
                                             radioButtons("plot_t", label = NULL, inline = T,
                                                          choices = list("Balken" = 1, "Linie" = 2), 
                                                          selected = 2))
                     )
                   ),


                   conditionalPanel("input.select_plot == 1",
                                    plotOutput("forcast_plot")),
                   conditionalPanel("input.select_plot == 2",
                                    dygraphOutput("dygraph"),
                                    br()),
                   #plotOutput("forcast_plot"),
                   #dygraphOutput("dygraph"),
                   
                   div(
                     id = "download_button",
                     downloadButton("download_table", label = "Download predictive values as CSV file")
                   ),
                   br()
                   ,
                   downloadButton("report_pdf", "Generate report PDF")
                 )
               )
             )))
      
      
    ),
    fluidRow(div(
      class = "show_table",
      checkboxInput("show_table", label = strong("Show table")),
      conditionalPanel("input.show_table",
                       tableOutput("forcast"))
    )),
    singleton(tags$head(tags$script(src = "message-handler.js")))
  )
)
