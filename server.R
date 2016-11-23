library(shinyjs)
source("timeFunctions.R")
source("jhfjzfjf.R")
library(shiny)
library(rmarkdown)
#load("fit_step.RData")
library(dygraphs)
require(xts)

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
    if (time2 < time1())
      time2 <- time1()
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
    if (is.null(input$names_lm))
      return(NULL)
    group <- input$names_lm
    #print(group)
    names_coeff <- paste0("coeff/coeff_", group , ".RData")
    print(names_coeff)
    load(names_coeff)
    coefficients
  })
  sd <- reactive({
    if (is.null(input$names_lm))
      return(NULL)
    group <- input$names_lm
    #print(group)
    sd <- data_sd_residials$sd[data_sd_residials$lm_name == group]
    print(sd)
    sd
  })
  forcast <- reactive({
    coefficients <- coefficients()
    coefficients_names <- names(coefficients)
    data_predictor_end <- predictors()
    
    data_predictor_end <-
      data_predictor_end[, names(data_predictor_end) %in% coefficients_names]
    
    data_predictor_end_int <-
      data.frame(int = rep(1, nrow(data_predictor_end)))
    data_predictor_end <-
      cbind(data_predictor_end_int, data_predictor_end)
    
    data_temp <- data_predictor_end
    
    for (i in 1:ncol(data_temp)) {
      data_temp[, i] <- data_temp[, i] * coefficients[i]
    }
    
    as.numeric(apply(data_temp, 1, sum))
    #predict(fit(), predictors())
  })
  forcast_all <- reactive({
    coefficients_all <- coefficients_list
    data_predictor_end <- predictors()
    i = 1
    for(i in 1:length(dir)){
      coefficients <- coefficients_all[[i]]
      coefficients_names <- names(coefficients)
      data_predictor_end <- data_predictor_end[, names(data_predictor_end) %in% coefficients_names]
      data_predictor_end_int <-  data.frame(int = rep(1, nrow(data_predictor_end)))
      data_predictor_end <- cbind(data_predictor_end_int, data_predictor_end)  
      data_temp <- data_predictor_end
          for (j in 1:ncol(data_temp)) {
      data_temp[, j] <- data_temp[, j] * coefficients[j]
          }
      forcast_ <- list(as.numeric(apply(data_temp, 1, sum)))
      if(i == 1) all_list <- forcast_
      else all_list <- c(all_list, forcast_)
    }

    
    all_list
    #predict(fit(), predictors())
  })
  data_show <- reactive({
    forcast <- round(as.numeric(forcast()), digits = 2)
    forcast <- ifelse(forcast < 0, 0, forcast)
    t <- seq(time1(), time2(), by = "hour")
    data_show <-
      data.frame(Time = substr(as.character(t), 1, 13), Value = forcast)
    data_show
    
  })
  output$forcast <- renderTable({
    data_show()
  }, options = list(include.rownames = FALSE))
  
  
  ts_data <- reactive({
    sd <- sd()
    forcast <- round(as.numeric(forcast()), digits = 2)
    forcast <- ifelse(forcast < 0, 0, forcast)
    t <- seq(time1(), time2(), by = "hour")
    forcast_lwr <- forcast - sd * 2
    forcast_lwr <- ifelse(forcast_lwr < 0, 0, forcast_lwr)
    forcast_upr <- forcast + sd * 2
    m3 <-
      matrix(c(forcast_lwr, forcast, forcast_upr), length(forcast), 3)
    ts_data <- xts(m3, order.by = t)
    colnames(ts_data) <- c("lwr", "value", "upr")
    ts_data
  })
  
  output$forcast_plot <- renderPlot({
    print(input$select_plot)
    data_show <- data_show()
    t <- seq(time1(), time2(), by = "hour")
    t_str <- as.character(t)
    sd <- sd()
    forcast_lwr <- data_show$Value - sd * 2
    forcast_lwr <- ifelse(forcast_lwr < 0, 0, forcast_lwr)
    forcast_upr <- data_show$Value + sd * 2
    #ts_data <- ts_data()
    #print(ts_data)
    if (nrow(data_show) > 1) {
      if (input$select_plot == "1" & input$plot_t == 1) {
        x <-
          barplot(
            data_show$Value,
            col = "cornflowerblue",
            names.arg = data_show$Time,
            main = "Average TV viewing time per person (min.)" ,
            xlab = "Time (day)",
            ylab = NULL
          )
        
        for (i in 1:length(data_show$Time)) {
          if (substr(data_show$Time[i], 12, 13) == "00") {
            #print(data_show$Time[i])
            abline(v = x[i], col = "gray60")
          }
        }
      }
      if (input$select_plot == "1" & input$plot_t == 2) {
        x <-
          plot(
            t,
            data_show$Value,
            lwd = 2,
            type = "l",
            xaxt = "n",
            col = "cornflowerblue",
            main = "Average TV viewing time per person (min.)" ,
            xlab = "Time (day)",
            ylab = ""
          )
        axis.POSIXct(1, t, format = "%y-%m-%d %H")
        grid (1, NULL, lty = 6, col = "cornsilk2")
        lines(t, forcast_upr, type = "l", lty = 2)
        lines(t, forcast_lwr, type = "l", lty = 2)
        
        for (i in 1:length(t)) {
          if (substr(t_str[i], 12, 13) == "00") {
            #print(t_str[i])
            abline(v = t[i],
                   col = "gray60",
                   lty = "dotted")
          }
        }
        legend(
          "topleft",
          legend = c("Value", "min, max"),
          col = c("blue", "black"),
          lty = 1:2,
          cex = 0.8
        )
      }
      if (input$select_plot == "3") {
        NULL
      }
      
    }  else
      NULL
    
  })
  output$dygraph <- renderDygraph({
    #data_show <- data_show()
    ts_data <- ts_data()
    nrow_tsdata <- nrow(ts_data)
    if (nrow_tsdata >= 10) {
      leftRange <- floor(0.2 * nrow(ts_data))
      rightRange <- floor(0.8 * nrow(ts_data))
    } else {
      leftRange <- 1
      rightRange <- 1
    }
    
    #print(ts_data)
    if (nrow(ts_data) > 1) {
      if (input$select_plot == 2) {
        dygraph(ts_data, main = "Average TV viewing time per person (min.)")  %>%
          dySeries(c("lwr", "value", "upr"), label = "value") %>%
          dyRangeSelector(dateWindow = c(ts_data[leftRange, 1], ts_data[rightRange, 1]))
      }
      
    }  else
      NULL
    
  })
  output$download_table <- downloadHandler(
    filename <- "Prognose.csv",
    content = function(con) {
      write.csv(data_show(), con)
    }
  )
  login_pw <- reactive({
    if (input$login_button == 0)
      return()
    isolate({
      paste(input$login, input$password)
    })
  })
  
  observe({
    if (is.null(login_pw()) == F) {
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
    if (input$login_button == 0)
      return (NULL)
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
      params <- list(
        group = input$names_lm,
        time1 = time1(),
        time2 = time2(),
        data_show = data_show(),
        plot_t = input$plot_t
        ,
        sd = sd()
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
})
