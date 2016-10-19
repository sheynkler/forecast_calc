

library(shinythemes)
library(shiny)
library(shinyjs)

list_hh <- paste(c(paste0("0",c(0:9)), c(10:23)), "00", sep = ":")
list_hh_selected <- "03:00"
width_input <- "100px"
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  includeCSS("css/mycss.css"),
                  tags$head(includeScript("google-analytics.js")),
                  useShinyjs(),

  # Application title
  titlePanel("Time Serial Prediction Modeling: TV Viewing Prognose Calculator"),
  br(),

  # Sidebar with a slider input for number of bins
  
    fluidRow(
      column(3,
             div(id = "login_div",
               textInput("login", label = h3("Login:"), value = NULL),
             passwordInput("password", "Password:"),
             textOutput("login_error"),
             actionButton("login_button", "Login")
              
                      
                      ),
             selectInput("names_lm", label = h3("Choose a group"), choices = as.list(names_target), selectize = F),
             h3("Start"),
             div(
               div(class = "enter_data",
                 dateInput("date1", label = NULL, value = Sys.Date())),
             div(class = "enter_time",
               selectInput("hh1", label = NULL, choices = as.list(list_hh),  selectize = F, selected = list_hh_selected, width = width_input))),
             h3("Stop"),
             div(
               div(class = "enter_data",
                   dateInput("date2", label = NULL, value = Sys.Date() + 1)),
               div(class = "enter_time",
                   selectInput("hh2", label = NULL, choices = as.list(list_hh), selectize = F, selected = list_hh_selected))),

             
             
             
             h3("Selected:"),
               div(
                 h4(textOutput("value")))
             ),

             
      column(9,
             
             shinyjs::hidden(div(id = "content",
               fluidRow(
               column(3,
                      div(tableOutput("forcast")
                          )
                      ),
               column(9, align="center",
                      plotOutput("forcast_plot"),
                      
                      div(id = "download_button",
                        downloadButton("download_table", label = "Download predictive values as CSV file"))
               )
             )))
             
      )
      
    
  ),
  singleton(
    tags$head(tags$script(src = "message-handler.js"))
  )
))
