library(shinyjs)
source("timeFunctions.R")
source("jhfjzfjf.R")
library(shiny)
library(rmarkdown)
#load("fit_step.RData")


shinyServer(function(input, output, session) {
  
  time1 <- reactive({
    time1 <- input$date1
    hh1 <- input$hh1
    hhh1 <- paste(hh1, "00",  sep = ":")
    time_hh <- paste(time1, hhh1)
    time1 <- as.POSIXct(strptime(time_hh, "%Y-%m-%d %H:%M:%S"))
    #time1 <- time1 + hh1*3600
    time1
  })
  time2 <- reactive({
    time2 <- input$date2
    hh2 <- input$hh2
    hhh2 <- paste(hh2, "00",  sep = ":")
    time_hh <- paste(time2, hhh2)
    time2 <- as.POSIXct(strptime(time_hh, "%Y-%m-%d %H:%M:%S"))
    #time1 <- time1 + hh1*3600
    if(time2 < time1()) time2 <- time1()
    time2
  })


  output$value <- renderText({ 
    
    paste(substr(time1(), 1, 16) , substr(time2(), 1, 16), sep = " -- ")
    })
  
  predictors <- reactive({
    data_predictor_end <- create_predictor_2(time1(), time2())
    data_predictor_end
  })
  output$value_predictors <- renderPrint({ 
    
    summary(fit())
  })
  coefficients <- reactive({
    if(is.null(input$names_lm)) return(NULL)
    group <- input$names_lm
    print(group)
    names_coeff <- paste0("coeff/coeff_", group , ".RData")
    print(names_coeff)
    load(names_coeff)
    coefficients
  })
  forcast <- reactive({
    coefficients <- coefficients()
    coefficients_names <- names(coefficients)
    data_predictor_end <- predictors()
    
    data_predictor_end <- data_predictor_end[, names(data_predictor_end) %in% coefficients_names]
    
    data_predictor_end_int <- data.frame(int = rep(1, nrow(data_predictor_end)))
    data_predictor_end <- cbind(data_predictor_end_int, data_predictor_end)
    
    data_temp <- data_predictor_end
    
    for(i in 1:ncol(data_temp)){
      data_temp[,i] <- data_temp[,i] * coefficients[i]
    }
    
    as.numeric(apply(data_temp, 1, sum))
    #predict(fit(), predictors())
  })
  data_show <- reactive({
    forcast <- round(as.numeric(forcast()), digits = 2)
    forcast <- ifelse(forcast < 0, 0, forcast)
    t <- seq(time1(), time2(), by = "hour")
    data_show <- data.frame(Time = substr(as.character(t),1,13), Value = forcast)
    data_show
      
    })
  output$forcast <- renderTable({ 

    data_show()
  }, options = list(include.rownames = FALSE)
  )
  output$forcast_plot <- renderPlot({ 
    data_show <- data_show()
    if(nrow(data_show) > 1){
      barplot(data_show$Value, col = "cornflowerblue", names.arg = data_show$Time, main = "Predictive viewing time" )
      'x <- barplot(data_show$Value, col = "cornflowerblue", main = "Predictive viewing time" , xaxt="n")
      labs <- data_show$Time
      text(cex=1, x=x-.25, y=-1.25, labs, xpd=TRUE, srt=45, pos=2)'
'      for(i in 1:length(data_show$Time)){
        if(substr(data_show$Time[i], 12,13) == "00"){
          print(data_show$Time[i])
          abline(v = data_show$Time[i], col = "gray60")
        } 
      }'
      
    }  else NULL
    
  })
  output$download_table <- downloadHandler(
    filename <- "Prognose.csv",
    content = function(con){
      write.csv(data_show(), con)
    }
  )
  login_pw <- reactive({
    if (input$login_button == 0) return()
    isolate({
      paste(input$login, input$password)
    })
  })
  
  
  observe({
    if(is.null(login_pw()) == F){
          if (input$login_button > 0 & login_pw() == login_pw_orig) {
      shinyjs::show(id = "content", anim = TRUE)
      shinyjs::hide(id = "login_div", anim = TRUE)
      session$sendCustomMessage(type = 'cookie_write',
                                message = "")
    }
    }
  })
  observe({
    if (is.null(input$mydata_2) == F) {
      print(input$mydata_2)
      mydata_2 <- input$mydata_2
      mydata_2_TF <- grepl("5326", mydata_2)
      if (mydata_2_TF) {
        print("da")
        shinyjs::show(id = "content", anim = TRUE)
        shinyjs::hide(id = "login_div", anim = TRUE)
      }
    }
  })
  output$login_error <- renderText({
    if (input$login_button == 0) return (NULL)
    if (input$login_button > 0 & login_pw() != login_pw_orig) {
      "Login or password are false"
    }
  })
  observe({
    if (input$login_button == 0) {
      isolate(session$sendCustomMessage(type = 'cookie_out',
                                        message = ""))
    }
  })
  output$report_pdf <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report2.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "new.Rmd")
      file.copy("new.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(group = input$names_lm,
                     time1 = time1(),
                     time2 = time2(),
                     data_show = data_show()
                     )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

})
