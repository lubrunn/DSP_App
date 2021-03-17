function(input, output, session) {

  ############################################################# Stocks
  # load stock dataset
  stockdata_DE <- reactive({
    validate(need(correct_path() == T, "Please choose the correct path"))


    load_all_stocks_DE()
  })

  stockdata_US <- reactive({
    validate(need(correct_path() == T, "Please choose the correct path"))


    load_all_stocks_US()
  })


  output$stock_choice <- renderUI({
    validate(need(correct_path() == T, "Please choose the correct path"))


    input <- selectizeInput("Stock","Choose Companies:",
                            c(COMPONENTS_DE()[["Company.Name"]],"DAX" = "GDAXI",
                              COMPONENTS_US()[["Company.Name"]],"DJI" = "DJI"),
                            selected = "Bayer ",multiple = TRUE)



  })



  # reset button for stock selection
  observeEvent(input$reset,{
    updateSelectizeInput(session,"Stock",selected = "")
  })
  # plot of the stocks
  output$plot_DE <- renderPlot({
    req(input$Stock)
    validate(need(correct_path() == T, "Please choose the correct path"))

    if (input$country_stocks == "Germany"){
      plotdata <- filter(stockdata_DE(),
                         .data$name %in% (c(COMPONENTS_DE()[["Symbol"]],"GDAXI")[c(COMPONENTS_DE()[["Company.Name"]],"GDAXI") %in% .env$input$Stock]) &
                           .data$Dates >= .env$input$dates[1] & .data$Dates <= .env$input$dates[2])
    } else {
      plotdata <- filter(stockdata_US(),
                         .data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DJI")[c(COMPONENTS_US()[["Company.Name"]], "DJI") %in% .env$input$Stock]) &
                           .data$Dates >= .env$input$dates[1] & .data$Dates <= .env$input$dates[2])
    }

    if (!is.null(ranges$x)) {
      ranges$x <- as.Date(ranges$x, origin = "1970-01-01")
    }
    ggplot(plotdata,aes_string("Dates",input$stock_outcome,color = "name"))+
      geom_line()+
      theme_classic()+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  # hover info box
  output$hover_info_DE <- renderUI({
    req(input$hovering)
    create_hover_info_DE(input$plot_hover_DE,stockdata_DE())
  })
  # zoom functionality
  ranges <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  #####################################################################

  ##################################################################### Corona

  corona_data <- reactive({
    validate(need(correct_path() == T, "Please choose the correct path"))

    CORONA(input$CoronaCountry,input$dates_corona[1],input$dates_corona[2])
  })

  output$corona_plot <- renderPlot({
    if (!is.null(ranges2$x)) {
      ranges2$x <- as.Date(ranges2$x, origin = "1970-01-01")
    }

    ggplot(corona_data(), aes_string("date",input$corona_measurement,color = "location"))+
      geom_line() +
      theme_classic() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })

  # hover info box
  output$hover_info_corona <- renderUI({
    req(input$hovering_corona)
    create_hover_info_corona(input$plot_hover_corona, corona_data(),input$corona_measurement)
  })

  # zoom functionality
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  observeEvent(input$plot_corona_dblclick, {
    brush <- input$plot_corona_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })

  ##################################################################################### Granger

  output$gra_con_check <- renderText({
    ##### date input
    test <- input$Controls_GRANGER
    if(("DAX" %in% test)==TRUE) {
      if (input$Stock_Granger == "GDAXI"){
        ##### formulate a validation statement
        validate("Index control not feasible if Index chosen as dependent variable")
      }
    }else if(("DJI" %in% test)==TRUE) {
      if (input$Stock_Granger == "DJI"){
        ##### formulate a validation statement
        validate("Index control not feasible if Index chosen as dependent variable")
      }
    }


  })


  ##### check if the date has at leas 30 days as input
  output$gra_date_check <- renderText({
    ##### date input
    if(length(input$date_granger) > 1){
      ##### calculate the difference of the dates
      days_inrange <- difftime(as.Date(input$date_granger[2]) ,as.Date(input$date_granger[1]) , units = c("days"))
      if (days_inrange < 30){
        ##### formulate a validation statement
        validate("Less than 30 days selected. Please choose more days.")

      }
      #### also check if no date is selected
    } else if (is.null(input$date_granger)){
      ##### formulate a validation statement
      validate("Need to select at least one day.")
    }
  })


  output$Stock_Granger <- renderUI({
    validate(need(correct_path() == T, "Please choose the correct path"))
    if (input$country_granger == "Germany"){
      input <- selectizeInput("Stock_Granger","Choose company or Index:",
                              #c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              company_terms_stock_ger,
                              selected = "DAX",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Granger","Choose company or Index:",
                              #c(COMPONENTS_US()[["Company.Name"]],"DJI"),
                              company_terms_stock_us,
                              selected = "Dow Jones Industrial",multiple = FALSE)
    }

  })


  output$ControlsGranger <- renderUI({
    if (input$country_granger == "Germany"){
      input <- selectizeInput("Controls_GRANGER","Choose control variables:",
                              c("Google-Trends Coronavirus"="coronavirus",
                                "VIX"="VIX",
                                "Financial Distress Index"="OFR.FSI",
                                "Economic Uncertainty Index"="WLEMUINDXD",
                                "DAX"="DAX"),selected = "VIX",multiple = FALSE)
                              #c(colnames(global_controls_test_DE())[-1],"DAX"),selected = "VIX",multiple = FALSE)
      #c(colnames(res[3:length(res)])),multiple = TRUE
    }else{
      input <- selectizeInput("Controls_GRANGER","Choose control variables:",
                              c("Google-Trends Coronavirus"="coronavirus",
                                "VIX"="VIX",
                                "Financial Distress Index"="OFR.FSI",
                                "Economic Uncertainty Index"="WLEMUINDXD",
                                "DJI"="DJI"),selected = "VIX",multiple = FALSE)
    }
  })


  observeEvent(req(input$corona_measurement_granger != ""), {                         #Observe event from input (model choices)
    updateSelectizeInput(session, "Controls_GRANGER", selected = "")
    updateSwitchInput(session, "senti_yesno_gra", value = FALSE)
  })
  observeEvent(req(input$Controls_GRANGER!=""), {                         #Observe event from input (model choices)
    updateSelectizeInput(session, "corona_measurement_granger", selected = "")
    updateSwitchInput(session, "senti_yesno_gra", value = FALSE)
  })

  observeEvent(req(input$senti_yesno_gra==TRUE), {                         #Observe event from input (model choices)
    updateSelectizeInput(session, "corona_measurement_granger", selected = "")
    updateSelectizeInput(session, "Controls_GRANGER", selected = "")
  })






  granger_data <- reactive({
    validate(need(correct_path() == T, "Please choose the correct path"))
    validate(need(input$senti_yesno_gra==TRUE | input$corona_measurement_granger != ""|input$Controls_GRANGER!="", "Choose the second variable!"))
    req(input$Stock_Granger)
    if (input$country_granger == "Germany"){
      granger1 <- dplyr::filter(stockdata_DE(),
                                #.data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Granger]) &
                                .data$name %in% .env$input$Stock_Granger &
                                  .data$Dates >= .env$input$date_granger[1] & .data$Dates <= .env$input$date_granger[2])[c("Dates", input$Granger_outcome)]
    } else {
      granger1 <-dplyr:: filter(stockdata_US(),
                                #.data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DJI")[c(COMPONENTS_US()[["Company.Name"]], "DJI") %in% .env$input$Stock_Granger]) &
                                .data$name %in% .env$input$Stock_Granger &
                                  .data$Dates >= .env$input$date_granger[1] & .data$Dates <= .env$input$date_granger[2])[c("Dates", input$Granger_outcome)]

    }

    if (input$country_granger == "Germany"){
      if(input$Controls_GRANGER!=""){
        global_controls <- global_controls_test_DE()   #load controls
        global_controls$Date <- as.Date(global_controls$Date) #transform date
        dax <- dplyr::filter(stockdata_DE(),.data$name %in% c("GDAXI")&
                               .data$Dates >= min(global_controls$Date) & .data$Dates <= max(global_controls$Date))[c("Dates","Close")]
        colnames(dax)[1]<-"Date"
        # dax <- GDAXI()  #load dax
        # dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
        # dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
        colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
        global_controls <- dplyr::left_join(dax,global_controls,by = c("Date")) #join final
      }else if (input$corona_measurement_granger!=""){
        global_controls <- CORONA_neu("Germany")[c("date",input$corona_measurement_granger)]
        colnames(global_controls)[1]<-"Dates"
      }

    }else {
      if(input$Controls_GRANGER!=""){
        global_controls <- global_controls_test_US() #same procedure as above
        global_controls$Date <- as.Date(global_controls$Date)
        dji <- dplyr::filter(stockdata_US(),.data$name %in% c("DJI")&
                               .data$Dates >= min(global_controls$Date) & .data$Dates <= max(global_controls$Date))[c("Dates","Close")]
        colnames(dji)[1]<-"Date"

        # dow <- DOW()
        # dow$Date <- as.Date(dow$Date, " %b %d, %Y")
        # dow <- missing_date_imputer(dow,"Close.")
        colnames(dji)[2] <- "DJI"
        global_controls <- dplyr::left_join(dji,global_controls,by = c("Date"))
      }else if (input$corona_measurement_granger!=""){
        global_controls <- CORONA_neu("United States")[c("date",input$corona_measurement_granger)]
        colnames(global_controls)[1]<-"Dates"
      }

    }

    if (input$Controls_GRANGER!=""|input$corona_measurement_granger!=""){
    names(global_controls)[1] <- "Dates"
    granger <- left_join(granger1,global_controls,by = c("Dates"))
    }else{
      granger<- granger1
    }

    if (input$senti_yesno_gra == TRUE){
      #res <- aggri_select()
      res <- get_sentiment_granger()
    } else {
      #res <- aggri_select()[1]
      res <- get_sentiment_granger()[1]
    }
    res$created_at <- as.Date(res$created_at)
    granger <- dplyr::left_join(granger,res,by=c("Dates" = "created_at"))


    if (input$senti_yesno_gra == TRUE){
      granger <- granger[c("Dates",input$Granger_outcome,colnames(get_sentiment_granger())[2])]
    }else if (input$Controls_GRANGER!=""){
      granger <- granger[c("Dates",input$Granger_outcome,input$Controls_GRANGER)]
    }else{
      granger <- granger[c("Dates",input$Granger_outcome,input$corona_measurement_granger)]
    }





    # ifelse(input$Controls_GRANGER!="",
    #        granger <- granger[c("Dates",input$Granger_outcome,input$Controls_GRANGER)],
    #        granger <- granger[c("Dates",input$Granger_outcome,input$corona_measurement_granger)])

    granger[is.na(granger)]<-0
    granger

  })

  observeEvent(input$Sentiment_type_gra, {                         #Observe event from input (model choices)
    req(input$Sentiment_type_gra)
    updateTabsetPanel(session, "params_gra", selected = input$Sentiment_type_gra)
  })

  observeEvent(input$industry_sentiment_gra, {                         #Observe event from input (model choices)
    req(input$industry_sentiment_gra)
    updateTabsetPanel(session, "industry_tab_gra", selected = input$industry_sentiment_gra)
  })


  ################################################################# sql data granger
  dates_gra <- reactive({
    if (length(input$date_granger) > 1){
      input$date_granger
    } else {
      c(input$date_granger, input$date_granger)
    }
  })


  querry_sentiment_model_gra <- reactive({

    #### check which tweet length
    if (input$tweet_length_gra == T){
      tweetLength <- 81
    } else {
      tweetLength <- 0
    }



    dates <- dates_gra()



    ###### table name
    ### get language
    if (input$sentiment_company_granger == "NoFilter"){
      test <- glue('select created_at, {input$aggregation_gra} from sum_stats_{tolower(input$language_gra)} where
    created_at >= "{dates[1]}" and created_at <= "{dates[2]}" and
    retweets_count = {input$minRetweets_gra} and likes_count = {input$minLikes_gra} and
    tweet_length = {tweetLength}')
    } else {
      comp <- gsub("ö","Ã¶", input$sentiment_company_granger)
      comp <- gsub("ü", "Ã¼", comp)

    test<-glue('SELECT created_at, {input$aggregation_gra}  FROM sum_stats_companies WHERE
      created_at >= "{dates[1]}" and created_at <= "{dates[2]}" and
         retweets_count = {input$minRetweets_gra} and likes_count = {input$minLikes_gra} and
         tweet_length = {tweetLength} and company  = "{comp}" and
             language = "{tolower(input$language_gra)}"' )
    }


    test

  })


  get_sentiment_granger <- reactive({
    ###### need correct path
    validate(need(correct_path() == T, "Please choose the correct path"))
    ###### need database connection
    validate(need(database_connector(), "Could not connect to database"))
    ###### need at least one date selected
    validate(need(!is.null(input$date_granger), "Please select a date."))

    ####### store database connection
    con <- database_connector()


    ###### querry data from sql
    df_need <- DBI::dbGetQuery(con,  querry_sentiment_model_gra())

    #### for companies replace umlaute
    if ("company" %in% names(df_need)){
      df_need$company <- gsub("Ã¶", "ö", df_need$company)
      df_need$company <- gsub("Ã¼", "ü", df_need$company)
    }
    #### return df
    df_need
  })








  optlags <- reactive({
    #library(vars)
    req(is.null(granger_data())==FALSE)
    vars::VARselect(granger_data()[-1],lag.max = 7, type = "const")$selection[["AIC(n)"]]
  })

  dickey_fuller <- reactive({
    data <- granger_data()
    while (tseries::adf.test(data[[2]],k=optlags())$p.value > 0.1 | tseries::adf.test(data[[3]],k=optlags())$p.value > 0.1){
      data[2] <- c(diff(data[[2]],1),NA)
      data[3] <- c(diff(data[[3]],1),NA)
      data <- tidyr::drop_na(data)
    }
    data
  })

  granger_result <- reactive({
    varobject <- vars::VAR(dickey_fuller()[-1], p = optlags(), type = "const")
    cause <- NULL
    if(input$Controls_GRANGER!=""){
      ifelse(input$direction_granger == TRUE,cause <- colnames(granger_data())[3],cause <- input$Granger_outcome)
    }else{
      ifelse(input$direction_granger == TRUE,cause <- colnames(granger_data())[3],cause <- input$Granger_outcome)
    }
    granger <- vars::causality(varobject, cause = cause)
    granger$Granger
  })

  output$granger_result <- renderPrint({
    granger_result()})

  # output$stocks_granger <- renderPlot({
  #   req(input$Granger_outcome)
  #   ggplot(granger_data(),aes_string("Dates",input$Granger_outcome))+
  #     geom_line()
  # })

  output$stocks_granger <- dygraphs::renderDygraph({

    plotdata <- xts::xts(granger_data()[input$Granger_outcome],order.by=granger_data()[["Dates"]])
    dygraphs::dygraph(plotdata,
                      ylab = colnames(granger_data()[2]),
                      main = glue::glue("Plot of the first variable"))%>%
      dygraphs::dyShading(from = min(granger_data()[["Dates"]]), to = max(granger_data()[["Dates"]]), color = "white")
  })



  output$second_granger <- dygraphs::renderDygraph({

    plotdata <- xts::xts(granger_data()[colnames(granger_data())[3]],order.by=granger_data()[["Dates"]])

    dygraphs::dygraph(plotdata,
                      ylab = colnames(granger_data()[3]),
                      main = glue::glue("Plot of the second variable"))%>%
      dygraphs::dyShading(from = min(granger_data()[["Dates"]]), to = max(granger_data()[["Dates"]]), color = "white")
  })

  output$grangertext1 <- renderUI({

    str1 <- paste("The optimal lag order for the VAR model using the Akaike information criterium (AIC)  is ",optlags()," lags.")
    htmltools::HTML(paste(str1))
  })

  output$optimallags <- renderPrint({

    vars::VARselect(granger_data()[-1],lag.max = 7, type = "const")
  })

  output$grangertext2 <- renderUI({

    if (nrow(dickey_fuller()) != nrow(granger_data())){
      str2 <- paste("The Dickey Fuller test found one of the timeseries to be non-stationary:")
    }else{
      str2 <-paste("The Dickey Fuller test found both timeseries to be stationary.
                   Hence, the granger causality analysis can be performed without tranformations:")
    }
  })


  #first variable
  output$dickey_fuller <- renderPrint({

    tseries::adf.test(granger_data()[[2]],k=optlags())
  })
  #second variable
  output$dickey_fuller_second <- renderPrint({

    tseries::adf.test(granger_data()[[3]],k=optlags())
  })

  output$grangertext3 <- renderUI({

    req(nrow(dickey_fuller()) != nrow(granger_data()))
    str3 <- paste("Differencing the series ",nrow(granger_data()) - nrow(dickey_fuller()),"times achieved stationarity:")
  })


  #first variable after differencing
  output$dickey_fuller_diff <- renderPrint({

    req(nrow(dickey_fuller()) != nrow(granger_data()))
    tseries::adf.test(dickey_fuller()[[2]],k=optlags())
  })
  #second variable after differencing
  output$dickey_fuller_second_diff <- renderPrint({

    req(nrow(dickey_fuller()) != nrow(granger_data()))
    tseries::adf.test(dickey_fuller()[[3]],k=optlags())
  })



  # output$dickey <- renderUI({
  #   str1 <- paste("The optimal lag order for the VAR model using the Akaike information criterium (AIC)  is ",optlags()," lags")
  #   if (nrow(dickey_fuller()) != nrow(granger_data())){
  #     str2 <- paste("The Dickey Fuller test found one of the timeseries to be non-stationary.")
  #     str3 <- paste("Differencing the series ",nrow(granger_data()) - nrow(dickey_fuller()),"times achieved stationarity")
  #   } else {
  #     str2 <-paste("The Dickey Fuller test found both timeseries to be stationary.")
  #     str3 <-paste("Hence, the granger causality analysis can be performed without tranformations")
  #   }
  #   HTML(paste(str1,str2,str3, sep = '<br/>'))
  # })

  output$granger_satz <- renderUI({

    if(input$direction_granger == TRUE){
      if (granger_result()["p.value"] < 0.1){
        str1 <- paste(htmltools::em(colnames(granger_data())[3]), " granger causes ",htmltools::em(input$Granger_outcome))
      } else {
        str1 <- paste(htmltools::em(colnames(granger_data())[3]), " does not granger cause ",htmltools::em(input$Granger_outcome))
      }
    } else {
      if (granger_result()["p.value"] < 0.1){
        str1 <- paste(htmltools::em(input$Granger_outcome),"of",input$Stock_Granger, "granger causes ",htmltools::em(colnames(granger_data())[3]))
      } else {
        str1 <- paste(htmltools::em(input$Granger_outcome),"of",input$Stock_Granger, "does not granger cause ",htmltools::em(colnames(granger_data())[3]))
      }
    }

    htmltools::HTML(paste(str1))
  })

  output$info_granger <- renderUI({
    htmltools::HTML(paste(htmltools::h1(htmltools::strong("Granger Causality Analysis"), align="center", style = "font-family: 'Times', serif;
                  font-weight: 30px; font-size: 30px; line-height: 1;"),
                          htmltools::p("In this section, the user is able to perform a Granger causality test, which is a statistical hypothesis test for determining whether one time series is useful in forecasting another.
                  The term ", htmltools::em("causality"), " in this context means nothing more than predictive causality and should not be mistaken for ",
                                       htmltools::em("true causality"),". It rather measures the ability of past values of one time series to predict future values of another time series.",htmltools::tags$br(),
                                       "To test the null hypothesis that time series ", htmltools::em("x")," does not Granger cause", htmltools::em("y"), ", one first finds the optimal lagged values of ", htmltools::em("y")," to include in a autoregression of ", htmltools::em("y:")
                                       ,style = "font-weight: 18px; font-size: 18px; line-height: 1;"),
                          withMathJax("$$y_t = \\alpha_0 + \\alpha_1y_{t-1} + \\alpha_2y_{t-1} + ... + \\alpha_my_{t-m} + error_t$$"),
                          htmltools::p("In the next step, lagged values of ", htmltools::em("x"),"are added to the regression: ",style = "font-weight: 18px; font-size: 18px; line-height: 1;"),
                          withMathJax("$$y_t = \\alpha_0 + \\alpha_1y_{t-1} + \\alpha_2y_{t-1} + ... + \\alpha_my_{t-m} + \\beta_1x_{t-1} + \\beta_qx_{t-q} + error_t$$"),
                          htmltools::p("The lagged values of ", htmltools::em("x")," are kept as long as they add explanatory power to the regression according to an F-test.
          The null hypothesis that ", htmltools::em("x")," does not Granger cause", htmltools::em("y"), "is accepted if and only if no lagged values of ", htmltools::em("x")," are included.",style = "font-weight: 18px; font-size: 18px; line-height: 1;"),
                          htmltools::h2(htmltools::strong("Instructions:") ,style = "font-family: 'Times', serif; font-weight: 20px; font-size: 20px; line-height: 1;"),
                          htmltools::p("In order to perform the Granger causality Analysis, built the model using the panel on the left: ",htmltools::tags$br(),
                                       htmltools::div("- select the first variable",htmltools::tags$br(),
                                                      "- select the second variable",htmltools::tags$br(),
                                                      "- choose the direction of the causality test using the checkbox",htmltools::tags$br(),
                                                      "- the tab ",htmltools::em("Visualize"),"contains plots of both series for comparison",
                                                      "- the tab ",htmltools::em("Background-steps")," contains all important steps required in the analysis",htmltools::tags$br(),
                                                      "- the results can be accessed on the tab ",htmltools::em("Results"), style="margin-left: 1em;font-weight: 18px; font-size: 18px; line-height: 1;"),style = "font-weight: 18px; font-size: 18px; line-height: 1;"),
                          htmltools::h2(htmltools::strong("Analysis steps:") ,style = "font-family: 'Times', serif; font-weight: 20px; font-size: 20px; line-height: 1;"),
                          htmltools::p("The following steps are automatically performed after the user selects two time series: ",htmltools::tags$br(),
                                       htmltools::div("1. The optimal number of lags is calculated",htmltools::tags$br(),
                                                      "2. Stationarity is repeatedly tested and the series are differenced until sationarity is achieved",htmltools::tags$br(),
                                                      "3. A VAR model is estimated with the optimal number of lags and the (if necessary) transformed series",htmltools::tags$br(),
                                                      "4. A granger causality test is performed.",style="margin-left: 1em;font-weight: 18px; font-size: 18px; line-height: 1;"),style = "font-weight: 18px; font-size: 18px; line-height: 1;")))
  })

  ################################################################################################### Regression

  output$info_regression <- renderUI({
    HTML(paste(htmltools::h1(htmltools::strong("Regression Analysis"), align="center", style = "font-family: 'Times', serif;
                  font-weight: 30px; font-size: 30px; line-height: 1;"),
               htmltools::p("In this section, the user is able to perform a simple linear regression and a quantile regression. Here, one can test which variables
                help to explain the stock prices of a specific company. By adding and dropping the variables, one can observe their potential of adding explanatory
                power to the regression.
                The linear regression estimates the conditional mean of the dependent variable and is of the form:",style = "font-weight: 18px; font-size: 18px; line-height: 1;"),
               withMathJax("$$y_i = \\beta_0 + \\beta_1x_{i1} + ... + \\beta_px_{ip} + \\epsilon_i$$"),
               htmltools::p("Quantile regressions estimate the conditional median (or quantile) of the dependet variable. They allow to quantify the effect
               of the independent variables at specified parts of the distribution. For example, in this application one can verify if companies
               with lower (or higher) stock returns are significantly more (or less) affected by the explanatory variables. The regression is fitted, by minimizing the median
               absolute deviation of the following equation: ",style = "font-weight: 18px; font-size: 18px; line-height: 1;"),
               withMathJax("$$Q_{\\tau}(y_i) = \\beta_0(\\tau) + \\beta_1(\\tau)x_{i1} + ... + \\beta_p(\\tau)x_{ip} + \\epsilon_i$$"),


               #Blablablabalabalabalaballbabalaaballabaal  Motivation, intention, warum regression? dependent variable nur stocks möglich?
               #möglichkeit sentiment rein und rauszunemehen"
               #,style = "font-weight: 18px; font-size: 18px; line-height: 1;"),

               htmltools::h2(htmltools::strong("Instructions:") ,style = "font-family: 'Times', serif; font-weight: 20px; font-size: 20px; line-height: 1;"),
               htmltools::p("In order to perform the regression analysis, built the model using the panel on the left: ",htmltools::tags$br(),
                            htmltools::div("- select the dependent variable",htmltools::tags$br(),
                                           "- select the control variable(s)",htmltools::tags$br(),
                                           "- choose whether sentiment variable should be included",htmltools::tags$br(),
                                           "- if sentiment is added, switch to the tab ",htmltools::em("Filter sentiment input")," on top of the sidebar and specify the sentiment",htmltools::tags$br(),
                                           "- the tab ",htmltools::em("Summary Statistics")," contains information on the selected variables",htmltools::tags$br(),
                                           "- the results can be accessed on the tab ",htmltools::em("Linear Regression")," and ",htmltools::em("Quantile Regression")," respectively.",htmltools::tags$br(),
                                           "- on the tab ",htmltools::em("Quantile Regression")," specify the desired quantile for which to compute the regression", style="margin-left: 1em;font-weight: 18px; font-size: 18px; line-height: 1;"),
                            style = "font-weight: 18px; font-size: 18px; line-height: 1;")))

  })

  output$reg_con_check <- renderText({
    ##### date input
    test <- input$Controls
    if(("DAX" %in% test)==TRUE) {
      if (input$Stock_Regression == "GDAXI"){
        ##### formulate a validation statement
        validate("Index control not feasible if Index chosen as dependent variable")
       }
    }else if(("DJI" %in% test)==TRUE) {
      if (input$Stock_Regression == "DJI"){
        ##### formulate a validation statement
        validate("Index control not feasible if Index chosen as dependent variable")
      }
    }


  })


  ##### check if the date has at leas 30 days as input
  output$reg_date_check <- renderText({
    ##### date input
    if(length(input$date_regression) > 1){
      ##### calculate the difference of the dates
      days_inrange <- difftime(as.Date(input$date_regression[2]) ,as.Date(input$date_regression[1]) , units = c("days"))
      if (days_inrange < 30){
        ##### formulate a validation statement
        validate("Less than 30 days selected. Please choose more days.")

      }
      #### also check if no date is selected
    } else if (is.null(input$date_regression)){
      ##### formulate a validation statement
      validate("Need to select at least one day.")
    }
  })



  ###flexible input for stocks: show either german or us companies
  output$stock_regression <- renderUI({
    validate(need(correct_path() == T, "Please choose the correct path"))
    if (input$country_regression == "Germany"){
      input <- selectizeInput("Stock_Regression","Choose company or Index:",
                              #c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              company_terms_stock_ger,
                              selected = "DAX",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Regression","Choose company or Index:",
                              #c(COMPONENTS_US()[["Company.Name"]],"DJI"),
                              company_terms_stock_us,
                              selected = "Dow Jones Industrial",multiple = FALSE)
    }
  })

  output$Controls <- renderUI({
    #res <- dataset()
    #res$name <- NULL
    validate(need(correct_path() == T, "Please choose the correct path"))
    if (input$country_regression == "Germany"){
      input <- selectizeInput("Controls","Control variables:",
                              c("Google-Trends Coronavirus"="coronavirus",
                                "VIX"="VIX",
                                "Financial Distress Index"="OFR.FSI",
                                "Economic Uncertainty Index"="WLEMUINDXD",
                                "DAX"="DAX"),selected = "VIX",multiple = TRUE)
      #c(colnames(res[3:length(res)])),multiple = TRUE
    }else{
      input <- selectizeInput("Controls","Control variables:",
                              c("Google-Trends Coronavirus"="coronavirus",
                                "VIX"="VIX",
                                "Financial Distress Index"="OFR.FSI",
                                "Economic Uncertainty Index"="WLEMUINDXD",
                                "DJI"="DJI"),selected = "VIX",multiple = TRUE)
    }

  })

  dataset <- reactive({
    validate(need(correct_path() == T, "Please choose the correct path"))
    req(input$country_regression)
    if (input$country_regression == "Germany"){
      data_reg <- dplyr::filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                                #.data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression]) &
                                .data$name %in% .env$input$Stock_Regression &
                                  .data$Dates >= .env$input$date_regression[1] & .data$Dates <= .env$input$date_regression[2])[c("Dates",input$regression_outcome,"name")] #hier später noch CLose flexibel machen
    } else {
      data_reg <- dplyr::filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                                #.data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DJI")[c(COMPONENTS_US()[["Company.Name"]], "DJI") %in% .env$input$Stock_Regression]) &
                                .data$name %in% .env$input$Stock_Regression &
                                  .data$Dates >= .env$input$date_regression[1] & .data$Dates <= .env$input$date_regression[2])[c("Dates",input$regression_outcome,"name")] #hier später noch CLose flexibel machen

    }

    if (input$country_regression == "Germany"){
      global_controls <- global_controls_test_DE()   #load controls
      global_controls$Date <- as.Date(global_controls$Date) #transform date
      dax <- dplyr::filter(stockdata_DE(),.data$name %in% c("GDAXI")&
                             .data$Dates >= min(global_controls$Date) & .data$Dates <= max(global_controls$Date))[c("Dates","Close")]
      colnames(dax)[1]<-"Date"
      # dax <- GDAXI()  #load dax
      # dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
      # dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
      colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
      global_controls <- dplyr::left_join(dax,global_controls,by = c("Date")) #join final
      if(input$corona_measurement_regression!=""){
        help <- CORONA_neu("Germany")[c("date",input$corona_measurement_regression)]
        colnames(help)[1]<-"Date"
        global_controls <- dplyr::left_join(global_controls,help,by=c("Date"))
      } else {}


    }else {
      global_controls <- global_controls_test_US() #same procedure as above
      global_controls$Date <- as.Date(global_controls$Date)
      dji <- dplyr::filter(stockdata_US(),.data$name %in% c("DJI")&
                             .data$Dates >= min(global_controls$Date) & .data$Dates <= max(global_controls$Date))[c("Dates","Close")]
      colnames(dji)[1]<-"Date"
      # dow <- DOW()
      # dow$Date <- as.Date(dow$Date, " %b %d, %Y")
      # dow <- missing_date_imputer(dow,"Close.")
      colnames(dji)[2] <- "DJI"
      global_controls <- dplyr::left_join(dji,global_controls,by = c("Date"))
      if(input$corona_measurement_regression!=""){
        help <- CORONA_neu("United States")[c("date",input$corona_measurement_regression)]
        colnames(help)[1]<-"Date"
        global_controls <- dplyr::left_join(global_controls,help,by=c("Date"))
      } else {}
    }

    names(global_controls)[1] <- "Dates"
    datareg2 <- dplyr::left_join(data_reg,global_controls,by = c("Dates"))

    datareg2[is.na(datareg2)]<-0
    datareg2
  })

  df_selected_controls <- reactive({

    #req(input$Controls_var | input$corona_measurement_var)
    res <- dataset()
    if(is.null(input$Controls)==TRUE && input$corona_measurement_regression==""){
      res <- res[c("Dates",input$regression_outcome)]
    }else if (is.null(input$Controls)==FALSE && input$corona_measurement_regression!=""){
      res <- res[c("Dates",input$regression_outcome,input$Controls,input$corona_measurement_regression)]
    } else if (is.null(input$Controls)==FALSE && input$corona_measurement_regression==""){
      res <- res[c("Dates",input$regression_outcome,input$Controls)]
    } else if (is.null(input$Controls)==TRUE && input$corona_measurement_regression!=""){
      res <- res[c("Dates",input$regression_outcome,input$corona_measurement_regression)]
    }
    res
  })
  # df_selected_controls <- reactive({
  #   #req(input$Controls)
  #   res <- dataset()
  #   res <- res[c("Dates",input$regression_outcome,input$Controls,input$corona_measurement_regression)]
  #   res
  # })

  observeEvent(input$Sentiment_type, {                         #Observe event from input (model choices)
    req(input$Sentiment_type)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type)
  })

  observeEvent(input$industry_sentiment, {                         #Observe event from input (model choices)
    req(input$industry_sentiment)
    updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment)
  })

  dataset_senti <- reactive({
    req(input$Sentiment_type)
    validate(need(correct_path() == T, "Please choose the correct path"))
    if(input$Sentiment_type == "NoFilter"){

      res <- En_NoFilter_0_0_yes()   # still fix as it is not clear yet if sql or csv
      #res <- eval(parse(text = paste('En', '_NoFilter_',input$minRetweet,'_',
      #                               input$minminLikes,'_',input$tweet_length,'()', sep='')))
      #input$language
    }else{
      req(input$Stock_reg)
      ticker <- ticker_dict(input$Stock_reg) # dict for a few stock
      res <- eval(parse(text = paste(ticker,'()', sep=''))) # example: ADS.DE()

    }


  })
  # filter
  filtered_df <- reactive({
    validate(need(correct_path() == T, "Please choose the correct path"))
    req(input$Sentiment_type)
    req(input$minRetweet_stocks1)
    req(input$minRetweet_stocks2)

    if(input$Sentiment_type == "NoFilter"){

      res <- dataset_senti()
    }else{ # live filtering
      req(input$industry_sentiment)
      res <- dataset_senti()
      if(input$industry_sentiment == "no"){
        res <- dataset_senti()
        if(input$tweet_length_stock1 == "yes"){

          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1)) &
                                  (tweet_length > 81))}
        else{
          res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1)))
        }
      }#else{
      #res <- dataset_senti()
      #if(input$tweet_length_stock2 == "yes"){
      # res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks2)) &
      #                          (tweet_length > 81))
      #}else{
      #  res <- res %>% filter(retweets_count > as.numeric(input$minRetweet_stocks2))
      #}
      #}
    }
  })

  # aggregate dataset to get one sentiment per day
  aggri_select <- reactive({

    if(input$Sentiment_type == "NoFilter"){ # NoFilter files already aggregated
      res <- filtered_df()
      aggregation <- key(input$aggregation)  # select aggregation type: Mean, mean weighted by,...
      res <- res %>% tidyr::gather("id", "aggregation", aggregation)
      res <- res[c("date","aggregation")]
    }else{
      if(input$industry_sentiment == "no"){
        res <- filtered_df()
        res <- aggregate_sentiment(res) # function to aggregate sentiment per day
        res <- res %>% filter(language == input$language1)
        aggregation <- key(input$aggregation1)
        res <- res %>% tidyr::gather("id", "aggregation", aggregation)
        res <- res[c("date","aggregation")]
      }else{
        res <- get_industry_sentiment(COMPONENTS_DE(),input$industry,input$minRetweet_stocks2,
                                      input$tweet_length_stock2)      #function to gather all stock in certain industry
        aggregation <- key(input$aggregation2)                          #--> also calculates aggregation inside function
        res <- res %>% tidyr::gather("id", "aggregation", aggregation)
        res <- res[c("date","aggregation")]
      }
    }

  })

  observeEvent(input$reset_regression,{
    updateSelectizeInput(session,"Controls",selected = "")
    updateSelectizeInput(session,"corona_measurement_regression",selected = "")
  })



  #merge sentiment with control+dep vars
  final_regression_df <- reactive ({
    if (input$senti_yesno_reg == TRUE){
      #res <- aggri_select()
      res <- get_sentiment_regression()
    } else {
      #res <- aggri_select()[1]
      res <- get_sentiment_regression()[1]
    }

    res$created_at <- as.Date(res$created_at)
    res_c <- df_selected_controls()
    res <- dplyr::left_join(res_c,res, by=c("Dates" = "created_at"))
    res <- res[-1]
    res
  })


  ########################### sql data
  dates_reg <- reactive({
    if (length(input$date_regression) > 1){
      input$date_regression
    } else {
      c(input$date_regression, input$date_regression)
    }

  })


  querry_sentiment_model_reg <- reactive({

    #### check which tweet length
    if (input$tweet_length == T){
      tweetLength <- 81
    } else {
      tweetLength <- 0
    }



    dates <- dates_reg()


    ###### table name
    ### get language
    if (input$sentiment_company_regression == "NoFilter"){
      test <- glue('select created_at, {input$aggregation} from sum_stats_{tolower(input$language)} where
    created_at >= "{dates[1]}" and created_at <= "{dates[2]}" and
    retweets_count = {input$minRetweets} and likes_count = {input$minLikes} and
    tweet_length = {tweetLength}')
    } else {
      comp <- gsub("ö","Ã¶", input$sentiment_company_regression)
      comp <- gsub("ü", "Ã¼", comp)

      test <- glue('SELECT created_at, {input$aggregation}  FROM sum_stats_companies WHERE
      created_at >= "{dates[1]}" and created_at <= "{dates[2]}" and
         retweets_count = {input$minRetweets} and likes_count = {input$minLikes} and
         tweet_length = {tweetLength} and company  = "{comp}" and
             language = "{tolower(input$language)}"' )
    }

  })


  get_sentiment_regression <- reactive({
    ###### need correct path
    validate(need(correct_path() == T, "Please choose the correct path"))
    ###### need database connection
    validate(need(database_connector(), "Could not connect to database"))
    ###### need at least one date selected
    validate(need(!is.null(input$date_regression), "Please select a date."))

    ####### store database connection
    con <- database_connector()



    ###### querry data from sql
    df_need <- DBI::dbGetQuery(con,  querry_sentiment_model_reg())

    #### for companies replace umlaute
    if ("company" %in% names(df_need)){
      df_need$company <- gsub("Ã¶", "ö", df_need$company)
      df_need$company <- gsub("Ã¼", "ü", df_need$company)
    }
    #### return df
    df_need
  })



  ####################################################Summary statistics  Regression #####################################################

  df_need_reg <- reactive({
    df_need <- round(psych::describe(final_regression_df())[c(3, 4, 5, 8, 9)], 2)
    test <- nrow(df_need)
    test2 <- nrow(df_need)==1
    if (nrow(df_need == 1)) {
      row.names(df_need)[1] <- input$regression_outcome
    } else{
      df_need <- df_need
    }
    names(df_need) <- names(df_need) %>% toupper()

    df_need


  })


  output$reg_summary <- function(){
    #colnames(df_need)<- "value"
    #names(df_need) <- names(df_need) %>% toupper()

    knitr::kable(df_need_reg(),colnames = NULL) %>%
      column_spec(1:6, color = "lightgrey") %>%
      column_spec(1, bold = T, color = "white") %>%
      row_spec(1, bold = T) %>%
      kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                position = "center",
                                font_size = 16)
  }

  # output$correlation_reg <- renderPlot({
  #   GGally::ggpairs(final_regression_df())
  # })
  output$correlation_reg <- renderPlot({
    res <- final_regression_df()
    #help_df <- res
    help_df <- res %>% select_if(~ !any(is.na(.)))
    if(any(is.na(res))){
      names_missing <- colnames(res)[ncol(res)]
      showNotification(glue("Removed {names_missing} for plot due to missing values"),
                       type = "message")}

    GGally::ggpairs(help_df, upper = list(continuous = wrap(ggally_cor, size = 8)), lower = list(continuous = 'smooth'))
    #ggpairs(final_regression_df_var()[-1])
  })


  ###################################################################################
  #regression
  regression_result <- reactive({
    req(ncol(final_regression_df())>=2)
    model <- stats::lm(stats::reformulate(".",input$regression_outcome), data = final_regression_df())
    #summary(model)
    lmtest::coeftest(model, vcov = sandwich::vcovHC(model, "HC1"))
  })

  #Qregression
  regression_result_Qreg <- reactive({
    req(ncol(final_regression_df())>=2)
    model <- quantreg::rq(stats::reformulate(".",input$regression_outcome),tau = input$Quantiles,data = final_regression_df())
    #summary(model,se = "ker")
    model
  })


  # output$testi_table <- renderPrint ({
  #   head(dataset())
  # })

  # output$senti <- renderPrint ({
  #   head(df_selected_controls())
  # })

  # output$senti_agg <- renderPrint ({
  #   head(final_regression_df())
  # })
  reg_table <- reactive({
     test <- regression_result()
     test <- broom::tidy(test)
     colnames(test)<-c("variable","estimate","std.error","test-statistic","p-value")
     test[5] <- round(test[5],4)
     real<- test
     test[5]<- "help"
     for (i in 1:nrow(test)){
       if (real[i,"p-value"]<0.01){
       test[i,"p-value"]<- paste0(real[i,"p-value"],'***')
       }else if (real[i,"p-value"]<0.05){
         test[i,"p-value"]<- paste0(real[i,"p-value"],'**')
       } else if (real[i,"p-value"]<0.05){
         test[i,"p-value"]<- paste0(real[i,"p-value"],'*')
       } else {
         test[i,"p-value"]<- paste0(real[i,"p-value"])
       }
     }

     test
  })

  output$regression_result <- function(){

  knitr::kable(reg_table(),colnames = NULL) %>%
      column_spec(1:5, color = "lightgrey") %>%
      column_spec(1, bold = T, color = "white") %>%
      row_spec(1, bold = T) %>%
      kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                position = "center",
                                font_size = 16)
    }

  output$regression_equation <- renderUI({
    str1 <- paste("Linear regression: ",input$regression_outcome,"~",paste(input$Controls,collapse = " + "),"<br/>")
    htmltools::HTML(paste(str1,sep = '<br/>'))
  })


  # output$plot_dens_Qreg <- renderPlot({
  #
  #   density_plot_reg(dataset())
  # })

   qreg_table <- reactive({
    test <- broom::tidy(regression_result_Qreg(),se.type="nid")

    colnames(test)<-c("variable","estimate","std.error","test-statistic","p-value","quantile")
    test[5] <- round(test[5],4)
    real<- test
    test[5]<- "help"
    for (i in 1:nrow(test)){
      if (real[i,"p-value"]<0.01){
        test[i,"p-value"]<- paste0(real[i,"p-value"],'***')
      }else if (real[i,"p-value"]<0.05){
        test[i,"p-value"]<- paste0(real[i,"p-value"],'**')
      } else if (real[i,"p-value"]<0.05){
        test[i,"p-value"]<- paste0(real[i,"p-value"],'*')
      } else {
        test[i,"p-value"]<- paste0(real[i,"p-value"])
      }
    }

    test
    })
   output$regression_result_Qreg <- function(){
     knitr::kable(qreg_table(),colnames = NULL) %>%
       column_spec(1:6, color = "lightgrey") %>%
       column_spec(1, bold = T, color = "white") %>%
       row_spec(1, bold = T) %>%
       kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                 position = "center",
                                 font_size = 16)
   }


  ###############################################################################
  ########################   VAR    #############################################
  ###############################################################################
  output$info_var <- renderUI({
    htmltools::HTML(paste(htmltools::h1(htmltools::strong("VAR-Forecasting"), align="center", style = "font-family: 'Times', serif;
                  font-weight: 30px; font-size: 30px; line-height: 1;"),
                          htmltools::p("In this section, the user is able to calculate forecasts of the stock variable using Vector-Autoregressions (VAR).
             VAR models are especially usefull for forecasting a collection of related variables where no explicit interpretation is required.
             Similar to the concept of Granger causality, it can be observed whether a timeseries is useful in forecasting another.
               In a VAR model each variable has an equation including its own lagged values and the lagged values of the other variables.
               For example, a VAR model with 2 variables and 1 lag is of the following form:",style = "font-weight: 18px; font-size: 18px; line-height: 1;"),
                          withMathJax("$$y_{1,t} = \\alpha_{1} +  \\beta_{11}y_{1,t-1} + \\beta_{12}y_{2,t-1}+ \\epsilon_{i,t}$$"),
                          withMathJax("$$y_{2,t} = \\alpha_{2} +  \\beta_{21}y_{1,t-1} + \\beta_{22}y_{2,t-1}+ \\epsilon_{2,t}$$"),
                          htmltools::p("A VAR is able to understand and use the relationships of several variables, allowing better description of dynamic behavior
               and better forecasting results. Here, different variable combinations can be assessed and used for forecasting.
               If only one variable is chosen, a univariate autoregressive model (AR) is applied and the variable is explained by its own lags only.",style = "font-weight: 18px; font-size: 18px; line-height: 1;"),

                          htmltools::h2(htmltools::strong("Analysis steps:") ,style = "font-family: 'Times', serif; font-weight: 20px; font-size: 20px; line-height: 1;"),
                          htmltools::p("The analysis consists of the following steps, which are performed automatically: ",htmltools::tags$br(),
                                       htmltools::div("1. The optimal number of lags is calculated",htmltools::tags$br(),
                                                      "2. Stationarity is repeatedly tested and the series are differenced until sationarity is achieved",htmltools::tags$br(),
                                                      "3. A VAR model is estimated with the optimal number of lags and the (if necessary) transformed series",htmltools::tags$br(),
                                                      "4. The residuals of the model are tested for serial correlation",htmltools::tags$br(),
                                                      "5. The series is forcasted n-steps ahead",style="margin-left: 1em;font-weight: 18px; font-size: 18px; line-height: 1;"),style = "font-weight: 18px; font-size: 18px; line-height: 1;"),


                          htmltools::h2(htmltools::strong("Instructions:") ,style = "font-family: 'Times', serif; font-weight: 20px; font-size: 20px; line-height: 1;"),
                          htmltools::p("In order to perform the regression analysis, built the model using the panel on the left: ",htmltools::tags$br(),
                                       htmltools::div("- select the dependent variable",htmltools::tags$br(),
                                                      "- select the control variables (optional)",htmltools::tags$br(),
                                                      "- choose whether sentiment variable should be included",htmltools::tags$br(),
                                                      "- if sentiment is added, switch to the tab ",htmltools::em("Filter sentiment input")," on top of the sidebar and specify the sentiment",htmltools::tags$br(),
                                                      "- the tab ",htmltools::em("Summary Statistics")," contains information on the selected variables",htmltools::tags$br(),
                                                      "- the tab ",htmltools::em("Validity")," performs a robustness check, including performance measurements for the model",htmltools::tags$br(),
                                                      "- the tab ",htmltools::em("Actual Forecast"),"displays the results for future-forecasts", style="margin-left: 1em;font-weight: 18px; font-size: 18px; line-height: 1;"),
                                       style = "font-weight: 18px; font-size: 18px; line-height: 1;")))
  })
  ###################################################### dataset ###############################################################

  output$var_con_check <- renderText({
    ##### date input
    test <- input$Controls_var
    if(("DAX" %in% test)==TRUE) {
      if (input$Stock_Regression_var == "GDAXI"){
        ##### formulate a validation statement
        validate("Index control not feasible if Index chosen as dependent variable")
      }
    }else if(("DJI" %in% test)==TRUE) {
      if (input$Stock_Regression_var == "DJI"){
        ##### formulate a validation statement
        validate("Index control not feasible if Index chosen as dependent variable")
      }
    }


  })






  ##### check if the date has at leas 30 days as input
  output$var_date_check <- renderText({
    ##### date input
    if(length(input$date_regression_var) > 1){
      ##### calculate the difference of the dates
      days_inrange <- difftime(as.Date(input$date_regression_var[2]) ,as.Date(input$date_regression_var[1]) , units = c("days"))
      if (days_inrange < 30){
        ##### formulate a validation statement
        validate("Less than 30 days selected. Please choose more days.")

      }
      #### also check if no date is selected
    } else if (is.null(input$date_regression_var)){
      ##### formulate a validation statement
      validate("Need to select at least one day.")
    }
  })






  ###flexible input for stocks: show either german or us companies
  output$stock_regression_var <- renderUI({
    validate(need(correct_path() == T, "Please choose the correct path"))
    if (input$country_regression_var == "Germany"){
      input <- selectizeInput("Stock_Regression_var","Choose company or Index:",
                              #c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              company_terms_stock_ger,
                              selected = "DAX",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Regression_var","Choose company or Index:",
                              #c(COMPONENTS_US()[["Company.Name"]],"DJI"),
                              company_terms_stock_us,
                              selected = "Dow Jones Industrial",multiple = FALSE)
    }
  })


  output$Controls_var <- renderUI({
    validate(need(correct_path() == T, "Please choose the correct path"))
    if (input$country_regression_var == "Germany"){
      input <- selectizeInput("Controls_var","Control variables:",
                              c("Google-Trends Coronavirus"="coronavirus",
                                "VIX"="VIX",
                                "Financial Distress Index"="OFR.FSI",
                                "Economic Uncertainty Index"="WLEMUINDXD",
                                "DAX"="DAX"),selected = "VIX",multiple = TRUE)
      #c(colnames(res[3:length(res)])),multiple = TRUE
    }else{
      input <- selectizeInput("Controls_var","Control variables:",
                              c("Google-Trends Coronavirus"="coronavirus",
                                "VIX"="VIX",
                                "Financial Distress Index"="OFR.FSI",
                                "Economic Uncertainty Index"="WLEMUINDXD",
                                "DJI"="DJI"),selected = "VIX",multiple = TRUE)
    }

  })

  dataset_var <- reactive({
    validate(need(correct_path() == T, "Please choose the correct path"))
    #validate(need(input$senti_yesno_var==TRUE | input$corona_measurement_var != ""|input$Controls_var!="", "Choose the second variable!"))
    if (input$country_regression_var == "Germany"){
      data_reg <- dplyr::filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                                #.data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression_var]) &
                                .data$name %in% .env$input$Stock_Regression_var &
                                  .data$Dates >= .env$input$date_regression_var[1] & .data$Dates <= .env$input$date_regression_var[2])[c("Dates",input$regression_outcome_var,"name")] #hier später noch CLose flexibel machen
    } else {
      data_reg <- dplyr::filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                                #.data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DJI")[c(COMPONENTS_US()[["Company.Name"]], "DJI") %in% .env$input$Stock_Regression_var]) &
                                .data$name %in% .env$input$Stock_Regression_var &
                                  .data$Dates >= .env$input$date_regression_var[1] & .data$Dates <= .env$input$date_regression_var[2])[c("Dates",input$regression_outcome_var,"name")] #hier später noch CLose flexibel machen
    }

    if (input$country_regression_var == "Germany"){
      global_controls <- global_controls_test_DE()   #load controls
      global_controls$Date <- as.Date(global_controls$Date) #transform date
      dax <- dplyr::filter(stockdata_DE(),.data$name %in% c("GDAXI")&
                             .data$Dates >= min(global_controls$Date) & .data$Dates <= max(global_controls$Date))[c("Dates","Close")]
      colnames(dax)[1]<-"Date"
      # dax <- GDAXI()  #load dax
      # dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
      # dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
      colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
      global_controls <- dplyr::left_join(dax,global_controls,by = c("Date")) #join final
      if(input$corona_measurement_var!=""){
        help <- CORONA_neu("Germany")[c("date",input$corona_measurement_var)]
        colnames(help)[1]<-"Date"
        global_controls <- dplyr::left_join(global_controls,help,by=c("Date"))
      } else {}

    }else {
      global_controls <- global_controls_test_US() #same procedure as above
      global_controls$Date <- as.Date(global_controls$Date)
      dji <- dplyr::filter(stockdata_US(),.data$name %in% c("DJI")&
                             .data$Dates >= min(global_controls$Date) & .data$Dates <= max(global_controls$Date))[c("Dates","Close")]
      colnames(dji)[1]<-"Date"
      # dow <- DOW()
      # dow$Date <- as.Date(dow$Date, " %b %d, %Y")
      # dow <- missing_date_imputer(dow,"Close.")
      colnames(dji)[2] <- "DJI"
      global_controls <- dplyr::left_join(dji,global_controls,by = c("Date"))
      if(input$corona_measurement_var!=""){
        help <- CORONA_neu("United States")[c("date",input$corona_measurement_var)]
        colnames(help)[1]<-"Date"
        global_controls <- dplyr::left_join(global_controls,help,by=c("Date"))
      } else {}
    }
    names(global_controls)[1] <- "Dates"
    data_reg2 <- dplyr::left_join(data_reg,global_controls,by = c("Dates"))
    data_reg2[is.na(data_reg2)]<-0
    data_reg2
  })


  df_selected_controls_var <- reactive({
    res <- dataset_var()
    if(input$Controls_var =="" && input$corona_measurement_var==""){
      res <- res[c("Dates",input$regression_outcome_var)]
    }else if (input$Controls_var !="" && input$corona_measurement_var!=""){
      res <- res[c("Dates",input$regression_outcome_var,input$Controls_var,input$corona_measurement_var)]
    } else if (input$Controls_var !="" && input$corona_measurement_var==""){
      res <- res[c("Dates",input$regression_outcome_var,input$Controls_var)]
    } else if (input$Controls_var =="" && input$corona_measurement_var!=""){
      res <- res[c("Dates",input$regression_outcome_var,input$corona_measurement_var)]
    }
    res
  })

  observeEvent(input$Sentiment_type_var, {                         #Observe event from input (model choices)
    req(input$Sentiment_type_var)
    updateTabsetPanel(session, "params", selected = input$Sentiment_type_var)
  })

  observeEvent(input$industry_sentiment_var, {                         #Observe event from input (model choices)
    req(input$industry_sentiment_var)
    updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment_var)
  })



  observeEvent(input$reset_regression_var,{
    updateSelectizeInput(session,"Controls_var",selected = "")
    updateSelectizeInput(session,"corona_measurement_var",selected = "")

  })

  #merge sentiment with control+dep vars
  final_regression_df_var <- reactive ({
    if (input$senti_yesno_var == TRUE){
      res <- get_sentiment_var()
    } else {
      res <- get_sentiment_var()[1]
    }
    res$created_at <- as.Date(res$created_at)
    res_c <- df_selected_controls_var()
    res <- dplyr::left_join(res_c,res, by=c("Dates" = "created_at"))
    #res <- res[-1]
    res
  })

  ################################################################# sql data var
  dates_var <- reactive({
    if (length(input$date_regression_var) > 1){
      input$date_regression_var
    } else {
      c(input$date_regression_var, input$date_regression_var)
    }
  })


  querry_sentiment_model_var <- reactive({

    #### check which tweet length
    if (input$tweet_length_var == T){
      tweetLength <- 81
    } else {
      tweetLength <- 0
    }



    dates <- dates_var()



    ###### table name
    ### get language
    if (input$sentiment_company_var == "NoFilter"){
      test <- glue('select created_at, {input$aggregation_var} from sum_stats_{tolower(input$language_var)} where
    created_at >= "{dates[1]}" and created_at <= "{dates[2]}" and
    retweets_count = {input$minRetweets_var} and likes_count = {input$minLikes_var} and
    tweet_length = {tweetLength}')
    } else {
      comp <- gsub("ö","Ã¶", input$sentiment_company_var)
      comp <- gsub("ü", "Ã¼", comp)

      test<-glue('SELECT created_at, {input$aggregation_var}  FROM sum_stats_companies WHERE
      created_at >= "{dates[1]}" and created_at <= "{dates[2]}" and
         retweets_count = {input$minRetweets_var} and likes_count = {input$minLikes_var} and
         tweet_length = {tweetLength} and company  = "{comp}" and
             language = "{tolower(input$language_var)}"' )
    }


    test

  })


  get_sentiment_var <- reactive({
    ###### need correct path
    validate(need(correct_path() == T, "Please choose the correct path"))
    ###### need database connection
    validate(need(database_connector(), "Could not connect to database"))
    ###### need at least one date selected
    validate(need(!is.null(input$date_regression_var), "Please select a date."))

    ####### store database connection
    con <- database_connector()


    ###### querry data from sql
    df_need <- DBI::dbGetQuery(con,  querry_sentiment_model_var())

    #### for companies replace umlaute
    if ("company" %in% names(df_need)){
      df_need$company <- gsub("Ã¶", "ö", df_need$company)
      df_need$company <- gsub("Ã¼", "ü", df_need$company)
    }
    #### return df
    df_need
  })

  ####################################################Summary statistics #####################################################

  df_need <- reactive({
    df_need <- round(psych::describe(final_regression_df_var()[-1])[c(3, 4, 5, 8, 9)], 2)
    test <- nrow(df_need)
    test2 <- nrow(df_need)==1
    if (nrow(df_need == 1)) {
      row.names(df_need)[1] <- input$regression_outcome_var
    } else{
      df_need <- df_need
    }
    df_need

  })


  output$var_summary <- function(){
    #colnames(df_need)<- "value"
    knitr::kable(df_need(),colnames = NULL) %>%
      column_spec(1:6, color = "lightgrey") %>%
      column_spec(1, bold = T, color = "white") %>%
      row_spec(1, bold = T) %>%
      kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                position = "center",
                                font_size = 16)
  }

  output$correlation_var <- renderPlot({
    res <- final_regression_df_var()[-1]
    #help_df <- res
    help_df <- res %>% select_if(~ !any(is.na(.)))
    if(any(is.na(res))){
      names_missing <- colnames(res)[ncol(res)]
      showNotification(glue("Removed {names_missing} for plot due to missing values"),
                       type = "message")}

   GGally::ggpairs(help_df, upper = list(continuous = wrap(ggally_cor, size = 8)), lower = list(continuous = 'smooth'))
    #ggpairs(final_regression_df_var()[-1])
  })




  ##################################################   Validity    ######################################################
  output$datensatz_var <- renderPrint ({
    head(final_regression_df_var())
  })

  forecast_data <- reactive({
    final_regression_df_var()[1:(nrow(final_regression_df_var())-input$ahead),-1,drop=FALSE]
  })

  actual_values <- reactive({
    final_regression_df_var()[((nrow(final_regression_df_var())+1)-input$ahead):nrow(final_regression_df_var()),2]
  })



  stationary <- reactive({
    data <- forecast_data()
    if (tseries::adf.test(data[[1]],k=2)$p.value > 0.1){
      for (i in 1:ncol(data)){
        data[i] <- c(diff(data[[i]],1),NA)
      }
      data <- drop_na(data)
    }else{}
    data
  })


  #optimal lags
  optlags_var <- reactive({
    vars::VARselect(stationary(),lag.max = 10, type = "none")$selection[["SC(n)"]]
  })

  #fit model
  var_model <- reactive({
    if (ncol(forecast_data()) == 1) {
      model <- stats::arima(stationary(), order = c(optlags_var(), 0, 0))
    } else {
      model <- vars::VAR(stationary(), p = optlags_var(), type = "none")
    }
    model
  })

  #test for autocorrelation: rejection = bad (means presence of correlated errors)
  serial_test <- reactive({
    if (ncol(forecast_data()) == 1) {
      test <- stats::Box.test(var_model()$residuals,type= "Box-Pierce" )
    } else {
      test <- vars::serial.test(var_model(), type="BG",lags.bg = optlags_var())
    }
    test
  })

  #forecast
  forecast_var <- reactive({
    fcast <- stats::predict(var_model(), n.ahead = input$ahead)
    if(nrow(stationary())!=nrow(forecast_data())){
      if (ncol(forecast_data()) == 1) {
        x <- fcast$pred[1:input$ahead]
        x <- cumsum(x) + forecast_data()[nrow(forecast_data()),1]
      }else {
        x <- fcast$fcst[[1]]
        x <- x[,1]
        x <- cumsum(x) + forecast_data()[nrow(forecast_data()),1]
      }
    }else{
      if (ncol(forecast_data()) == 1) {
        x <- fcast$pred[1:input$ahead]
      }else {
        x <- fcast$fcst[[1]]
        x <- x[,1]
      }
    }
    x
  })

  #plot the actual vs. the predicted forecast
  output$plot_forecast <- dygraphs::renderDygraph({
    if (input$var_which_plot == "Forecasted period only"){
      plot <- data.frame(final_regression_df_var()$Dates[(nrow(forecast_data())+1):(nrow(forecast_data())+input$ahead)],#Dates
                         forecast_var(),                                                              #forecasted values
                         actual_values())#actual values
      colnames(plot) <- c("a","forecast","actual")
      # ggplot(plot1) +
      #   geom_line(aes(a,b),color="red")+
      #   geom_line(aes(a,c),color="gold")+
      #   labs(x="Date",y="StockPrice",title = "forecasted vs. actual")
      help <- plot
      plot <- xts::xts(plot[c("forecast","actual")],order.by=plot[["a"]])
      dygraphs::dygraph(plot)%>%
        dygraphs::dyShading(from = min(help$a), to = max(help$a), color = "white")
    }else{
      plot <- data.frame(final_regression_df_var()$Dates,
                         c(forecast_data()[[1]],forecast_var()),
                         final_regression_df_var()[2])
      colnames(plot) <- c("a","forecast","actual")
      help <- plot
      # ggplot(plot2) +
      #   geom_line(aes(a,b))+
      #   geom_line(aes(a,c))+
      #   labs(x="Date",y="StockPrice",title = "forecasted vs. actual, full series")
      plot <- xts::xts(plot[c("forecast","actual")],order.by=plot[["a"]])

      dygraphs::dygraph(plot) %>%
        dyEvent(final_regression_df_var()$Dates[(nrow(forecast_data()))], "Start of prediction", labelLoc = "bottom")%>%
        dygraphs::dyShading(from = min(help$a), to = max(help$a), color = "white")

    }

  })


  insample_var <- reactive({
    fcast <- stats::predict(var_model(), stationary())
    if(nrow(stationary())!=nrow(forecast_data())){
      if (ncol(forecast_data()) == 1) {
        x <- NA
      }else {
        x <- fcast$model$varresult[[1]]$fitted.values
        x <- cumsum(x) + forecast_data()[1,1]
      }
    }else{
      if (ncol(forecast_data()) == 1) {
        x <- NA
      }else {
        x <- fcast$model$varresult[[1]]$fitted.values
      }
    }
    x
  })





  output$var_metrics <- function(){
    if (ncol(forecast_data()) == 1){
      test <- c("Not available for ARIMA","Not available for ARIMA","Not available for ARIMA")
    }else{
      forecast_data <- forecast_data()[1:length(insample_var()),1]

      test <- c(sqrt(mean((insample_var()-forecast_data)^2)),
                mean(abs(insample_var()-forecast_data)),
                mean(abs((forecast_data-insample_var())/forecast_data * 100)))
    }

    df_need <- data.frame(c(sqrt(mean((forecast_var()-actual_values())^2)),
                            mean(abs(forecast_var()-actual_values())),
                            mean(abs((actual_values()-forecast_var())/actual_values()) * 100)),
                          row.names = c("RMSE","MAE","MAPE"))

    colnames(df_need)<- "forecast"
    df_need$insample <- test
    knitr::kable(df_need,colnames = NULL) %>%
      column_spec(1:3, color = "lightgrey") %>%
      column_spec(1, bold = T, color = "white") %>%
      row_spec(1, bold = T) %>%
      kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                position = "center",
                                font_size = 16)
  }


  output$serial_test <- renderPrint({
    serial_test()
  })

  output$var <- renderUI({
    if (ncol(forecast_data()) == 1) {
      str1 <- paste("Box-Pierce test statistic to test for autocorrelation in the AR-residuals:")
      if (serial_test()$p.value > 0.1){
        str2 <- paste("The hypothesis of serially uncorrelated residuals cannot be rejected.")
      } else{
        str2 <- paste("The hypothesis of serially uncorrelated residuals can be rejected.")
      }
    } else {
      str1 <- paste("Breusch-Godfrey LM-statistic to test for autocorrelation in the AR-residuals:")
      if (serial_test()$serial$p.value > 0.1){
        str2 <- paste("The hypothesis of serially uncorrelated residuals cannot be rejected.")
      } else {
        str2 <- paste("The hypothesis of serially uncorrelated residuals can be rejected.")
      }
    }
    htmltools::HTML(paste(str1,str2, sep = '<br/>'))
  })



  ##################################################   actual forecast    ######################################################
  forecast_data_real <- reactive({
    final_regression_df_var()[,-1,drop=FALSE]
  })



  stationary_real <- reactive({
    data <- forecast_data_real()
    if (tseries::adf.test(data[[1]],k=2)$p.value > 0.1){
      for (i in 1:ncol(data)){
        data[i] <- c(diff(data[[i]],1),NA)
      }
      data <- drop_na(data)
    }else{}
    data
  })


  #optimal lags
  optlags_var_real <- reactive({
    vars::VARselect(stationary_real(),lag.max = 10, type = "none")$selection[["SC(n)"]]
  })

  #fit model
  var_model_real <- reactive({
    if (ncol(forecast_data_real()) == 1) {
      model <- stats::arima(stationary_real(), order = c(optlags_var_real(), 0, 0))
    } else {
      model <- vars::VAR(stationary_real(), p = optlags_var_real(), type = "none")
    }
    model
  })

  serial_test_real <- reactive({
    if (ncol(forecast_data()) == 1) {
      test <- stats::Box.test(var_model_real()$residuals,type= "Box-Pierce" )
    } else {
      test <- vars::serial.test(var_model_real(), type="BG",lags.bg = optlags_var_real())
    }
    test
  })

  #forecast
  forecast_var_real <- reactive({
    fcast <- stats::predict(var_model_real(), n.ahead = input$ahead)
    if(nrow(stationary())!=nrow(forecast_data())){
      if (ncol(forecast_data_real()) == 1) {
        x <- fcast$pred[1:input$ahead]
        x <- cumsum(x) + forecast_data_real()[nrow(forecast_data_real()),1]
      }else {
        x <- fcast$fcst[[1]]
        x <- x[,1]
        x <- cumsum(x) + forecast_data_real()[nrow(forecast_data_real()),1]
      }
    }else{
      if (ncol(forecast_data_real()) == 1) {
        x <- fcast$pred[1:input$ahead]
      }else {
        x <- fcast$fcst[[1]]
        x <- x[,1]
      }

    }
    x
  })

  output$plot_forecast_real <- dygraphs::renderDygraph({

    plot <- data.frame(c(final_regression_df_var()[["Dates"]],seq(as.Date(tail(final_regression_df_var()$Dates,1))+1,by = "day",length.out = input$ahead)),
                       c(forecast_data_real()[[1]],forecast_var_real()))
    colnames(plot) <- c("a","forecast")
    # ggplot(plot2) +
    #   geom_line(aes(a,b))+
    #   labs(x="Date",y="StockPrice",title = "forecasted series")
    help<-plot
    plot <- xts::xts(plot["forecast"],order.by=plot[["a"]])

    dygraphs::dygraph(plot) %>%
      dyEvent(max(final_regression_df_var()$Dates), "Start of prediction", labelLoc = "bottom")%>%
      dygraphs::dyShading(from = min(help$a), to = max(help$a), color = "white")

  })

  output$serial_test_real <- renderPrint({
    serial_test_real()
  })

  output$var_real <- renderUI({
    if (ncol(forecast_data()) == 1) {
      str1 <- paste("Box-Pierce test statistic to test for autocorrelation in the AR-residuals:")
      if (serial_test_real()$p.value > 0.1){
        str2 <- paste("The hypothesis of serially uncorrelated residuals cannot be rejected.")
      } else{
        str2 <- paste("The hypothesis of serially uncorrelated residuals can be rejected.")
      }
    } else {
      str1 <- paste("Breusch-Godfrey LM-statistic to test for autocorrelation in the AR-residuals:")
      if (serial_test_real()$serial$p.value > 0.1){
        str2 <- paste("The hypothesis of serially uncorrelated residuals cannot be rejected.")
      } else {
        str2 <- paste("The hypothesis of serially uncorrelated residuals can be rejected.")
      }
    }
    htmltools::HTML(paste(str1,str2, sep = '<br/>'))
  })


  #################################################################################################### twitter


  ###########################################################################################
  ################################### Instructions #########################################
  ###########################################################################################

  # start introjs when button is pressed with custom options and events
  observeEvent(input$instrucitons_desc,{

    guide1$init()$start()

  })

  guide1 <- cicerone::Cicerone$
    new()$
    step(
      el = "lang_instr",
      title = "Language",
      description = "Here you can select the language of the tweets."
    )$
    step(
      "comp_instr",
      "Select a company",
      "Here you can either select company specific tweets or tweets that were randomly scraped
                              without any search term"
    )$
    step(
      "date_instr",
      "Date range",
      "Here you can select the date range to analyse. We have data starting from 2018-11-30 until yesterday.
      The app is updated daily. You can always reset your choice to the entire date range."
    )$
    step(
      "desc_filter_instr",
      "Filters",
      "Here you may choose to filter the tweets according to minimum number of retweets or likes a tweet needs to have.
      You can also display only 'Long Tweets' which are tweets with at least 80 characters"
    )$
    step(
      "metric_desc_instr",
      "Time series",
      "In this section you can select a metric to display in the time series plots. Sentiment is also available as weighted metrics.
      You can choose between the mean, the standard deviation and the median. You can also choose to show the number of tweets per day.
      Multiple selections are possible. Once multiple metrics are selected all values get scaled with mean 0 and SD 1 in order to allow
      for plotting on a similar scale"
    )$
    step(
      "time_series1_instr",
      "Time series",
      "Here the time series for the current selection are depicted. You can zoom in (select area with mouse) and out (double click) of the plot.
      The ribbon below is only shown when a single metric is selected. It is green when the values are above and red when they are below the average over
      the selected period."
    )$
    step(
      "plot_saver_button",
      "Save button",
      "You can temporarily store a plot in the area below the first plot. This may help you with comparing plots from different companies
      or comparing different metrics without overcrowding the plot."
    )$
    step(
      "time_series2_instr",
      "Saved time series",
      "In this area the temporarily saved plot will appear."
    )$
    step(
      "sum_stats_table_instr",
      "Summary Statistics",
      "Here you can see the summary statistics for the currently selected inputs."
    )$
    step(
      "histo_instr",
      "Histogram",
      "Here you can select for which metric you would like to see the histogram. You may choose one of sentiment, retweets, likes or tweet length.
      You can adjust the number of bins of the histogram.
      "
    )$
    step(
      "log_scale_instr",
      "Log scale",
      "For retweets, likes and tweet length it may be more interesting to show the log transformed distribution as these values have many outliers.
      For sentiment this option is blocked as a log transformation of negative values is not possible (sentiment goes from -1 to 1)."
    )$
    step(
      "histo_plot_instr",
      "Histogram output",
      "Here the histogram for the current selection is shown. Note that the histogram also depends on the tweet type, date range, retweets, likes
        and tweets length filters from above."
    )


  observeEvent(input$instructions_comp,{
    guide2$init()$start()

  })


  guide2 <- cicerone::Cicerone$
    new()$
    step(
      el = "stock_instr",
      title = "Stocks",
      description = "Here you can select any stock from the DAX or DJI or the indeces themselves. You may choose to show either the
      returns or the adjusted closing prices. Multiple selection are possible. Once more than one stock is selected and the metric
      'Adjusted Close' is selected the values will be automatically be set to base value of 100 at the start of the period."
    )$
    step(
      "stock_roll_instr",
      "Moving Average",
      "With the switch you can depict a 7-day moving average instead of the normal daily values."
    )$
    step(
      "stocks_comp_plot_instr",
      "Stock Plot",
      "Here the stock values are depicted. Note that you can zoom in and out (through double click) of the plot. The zooming will affect
      all plots in this tab."
    )$
    step(
      "twitter_comp_instr",
      "Twitter",
      "Here you have the same options as on the previous tabs. You can also choose to see the 7-day moving average by toggling
      the switch at the bottom."
    )$
    step(
      "twitter_comp_plot_instr",
      "Twitter Plot",
      "Like above the plot has a zooming ability and is scaled once mutliple metrics are selected."
    )$
    step(
      "covid_comp_instr",
      "Control variables",
      "Here you can select different COVID-19 related metrics or other financial control variables
      for Germany and the US (can plot both simultaneously). Again you may choose
      to depict a 7-day moving average."
    )$
    step(
      "covid_plot_comp_instr",
      "Control Variables Plot",
      "Here the plot for the control variables is shown. Note that COVID-19 data is only available starting in
      March 2020. Again, you can zoom in and out of the plot."
    )



  observeEvent(input$instrucitons_expl, {
    guide_expl$init()$start()
  })


  guide_expl <- cicerone::Cicerone$
    new()$
    step(
      el = "tweets_all_expl",
      title = "Tweets",
      description = "For the tweets you have the same filter options as on the previous tab."
    )$
    step(
      "emoji_instr",
      "Emoji filter",
      "Here you can omit words the stem form emoji replacements. Click the question mark for more info."
    )$
    step(
      "plot_type_expl_instr",
      "Plot type",
      "You can either show a bar plot with the most frequent words or a word cloud. Both outputs have a hovering ability.
      As the option for the word cloud and bar plot vary simply click on the question marks for further information on their
      controls. Note that you have an additional search option when selecting bigrams."
    )$
    step("ngram_sel_instr",
         "NGram Selection",
         "You can decide between showing single words or bigrams. For bigrams you also have the option to search for bigrams
         containing specific words."
         )$
    step(
      "num_words_expl_instr",
      "Info",
      "Here we tell you the number of total umber of tweets for the current selection as well the the number of unique words/bigrams.
      Note that it can happen that 0 words/bigrams are found despite the info showing a positive amount of tweets
      for the current selection. This is due to the pre-processing of the data where we set an abosulte minimum frequency requirement of 5 and 2 (or
      1% and 0.1% of total tweets per day depending on which is higher) occurences per day for words and bigrams respectively. "
    )$
    step(
      "expl_plots_instr",
      "Outputs",
      "Here the bar plot or wordcloud are shown. Note that the wordcloud can have a bug that it disappears when resizing the window. This bug can be avoided
      by installing the github version of the wordcloud2 package with : devtools::install_github('lchiffon/wordcloud2')",
      position = "bottom"
    )




  ##### instructions for network plot
  observeEvent(input$net_instr, {
    guide_net$init()$start()
  })

  guide_net <- cicerone::Cicerone$
    new()$
    step(
      el = "tweet_net_instr",
      title = "Tweets",
      description = "Here you have similar controls for selecting tweets as beofre. However, now you can select and arbitaray number of minimum
      likes, retweets and can also filter for specific sentiments. However, a maximum of only 2 days can analysed at a time."
    )$
    step(
      "sentiment_net_instr",
      "Sentiment",
      "Here you can select to show tweets for certain range of sentiments."
    )$
    step(
      "emoji_net_instr",
      "Emoji words",
      "You can again omit emoji words from your analysis."
    )$
    step(
      "search_net_instr",
      "Search terms",
      "Here you can look for tweets containing a certain word or tweets from specific users. Note that in both cases the search is not case sensitive and search
      terms will be stemmed to fit the cleaned tweet data. You can not only look for exact matches in usernames but also partial. For example you may search
      for all tweets from users that have the word trump in their usernames."
    )$
    step(
      "net_type_instr",
      "Word Combinations",
      "Here you can either select to analyse bigrams or word pairs."
    )$
    step(
      "adv_settings_net_instr",
      "Adv. Settings",
      "Toggling this button allows you to access the advanced settings. Here you can adjust the minimum thresholds of word occruences and correlations."
    )$
    step(
      "buttons_net_instr",
      "Buttons",
      "As the network analysis takes some time we want you to actively ask for the computation. You may cancel the process (takes a few seconds) or remove the
      already computed network."
    )$
    step(
      "num_tweets_info_net_instr",
      "Info",
      "Here you get information about the number of available tweets for your current selection. This updates in realtime."
    )$
    step(
      "placeholder",
      "Network plot",
      "Here the network will appear once it has been computed after the button has been pressed"
    )$
    step(
      "data_table_instr",
      "Data",
      "Here you can take a look at the tweets contained in the network. This also updates in realtime."
    )


  ############################################################################
  ################# Directory ###############################################
  ###########################################################################
  # selecting directory
  # find home direcoty of user
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
  # allow for searching directories
  shinyFiles::shinyDirChoose(input, "directory", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)


  ##### set wd with shinyfiles
  observeEvent(input$directory,{
    ##### when directory button hasnt been pressed set wd to home directory and tell user
    if (is.integer(input$directory)) {
      setwd(volumes)

      #cat(glue("No directory has been selected. Current directory {getwd()})"))

    } else {
      ##### when button is pressed set wd to choosen dirfwefe
      path <- shinyFiles::parseDirPath(volumes, input$directory)

      setwd(path)



    }
  })


  ####### manually entered path
  observeEvent(input$dir_path_man_btn, {


    if (!dir.exists(input$dir_path_man)){
      output$path_checker_man <- renderText({
        "Please enter a valid path"
      })
    }

    #### require that path exists
    validate(need(dir.exists(input$dir_path_man), "Please enter a valid path"))
    ### when manual path button is pressed set wd to enterd path
    setwd(input$dir_path_man)
  })

  #### checks if sqlitstudio dir exists, if yes then wd is set correctly
  correct_path <- reactive({
    input$dir_path_man_btn
    input$directory
    dir.exists("SQLiteStudio")
  })

  ####### output text as feedback for user whether directory seems corrct
  output$directorypath <- renderText({
    input$directory
    input$dir_path_man_btn

    #### when sqlitestudio dir exists everything correct
    if(dir.exists("SQLiteStudio")) {
      glue("Current path is set to: {getwd()}. This seems correct.")

    } else {
      #### if sqlitstudio does not exist something wrong probably
      glue("Current path is set to: {getwd()}. Data could not be found in this  \n
      directory. Are you sure it is set correctly?")
    }
  })




  ###### connect to database when path has been set correctly
  database_connector <- function(){


    if(dir.exists("SQLiteStudio")) {
      con <- DBI::dbConnect(RSQLite::SQLite(), "SQLiteStudio/databases/clean_database.db")

      con
    }
  }




  ###############################################################################
  ##################### twitter logo directory page ############################
  ###############################################################################

  output$twitter_logo <- renderImage({
    ###### correct path needs to be chosen
    req( correct_path()== T)
    ##### path to png
    filename <- "shiny/images/twitter_logo_wordcloud2.png"


    ##### image output
    list(src = filename,
         alt = "This is the Twitter Logo",
         contentType = "Images/png",
         height = "100%", width = "80%")
  }, deleteFile = F)


  ###############################################################################
  ############################### twitter descriptive ###########################
  ###############################################################################
  ### reset daterange
  observeEvent(input$reset_dates_desc,{
    #### when reset button is pressed, reset date range to entire date range available
    shinyWidgets::updateAirDateInput(session, "dates_desc",
                                     clear = T,
                                     value = c("2018-11-30", date_avail))

  })



  ######## disconnect from database after exit
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    #validate(need(correct_path() == T, "Please choose the correct path"))
    #req(database_connector())
    con <- database_connector()
    if (!is.null(con)){
      DBI::dbDisconnect(con)
    }
  })


  ###### create reactive long variable for filtering from user input
  long <- reactive({
    if (input$long == T){
      long <- 81
    } else{
      long <- 0
    }
    long
  })




  ################################## date_variable that accounts for single dates
  dates_desc <- reactive({
    ###### need correct path
    validate(need(correct_path() == T, "Please choose the correct path"))
    ######## need at least one date
    validate(need(!is.null(input$dates_desc), "Please select a date."))

    ##### if date range selected, used date range
    if (length(input$dates_desc) > 1){
      input$dates_desc
    } else {
      ##### if single date selceted, create daterange with same date twice --> for easier computing
      # otherwise need to control for cases where input is single date vs. list of dates
      c(input$dates_desc, input$dates_desc)
    }
  })


  ################################### path finder for histo files
  querry_histo <- reactive({
    ##### need correct wd
    validate(need(correct_path() == T, "Please choose the correct path"))
    ##### need at least one date selected
    validate(need(!is.null(input$dates_desc), "Please select a date."))


    #### convert long input to name addon in file
    if (input$long == T){
      long_name <- "long_only"
    } else{
      long_name <- "all"
    }
    #### get langauge addon
    lang <- lang_converter()


    ### account for case where sentiment is selected

    # replace sentiment with senti because refernced with senti in file
    value_var <- stringr::str_replace(input$histo_value,"sentiment", "senti")
    # replace tweet_length with long becuase refernced with long in file
    value_var <- stringr::str_replace(value_var, "tweet_length", "long")


    # for no filter
    if (input$comp == "NoFilter"){
      #### filename for nofilter data
      glue("histo_{value_var}_{lang}_NoFilter_rt_{input$rt}_li_{input$likes}_lo_{long_name}.csv")
    } else { #for chosen company
      req(!is.null(input$comp))
      #### filename for companies
      glue("histo_{value_var}_{input$comp}_rt_{input$rt}_li_{input$likes}_lo_{long_name}.csv")

    }
  }) #reactive closed




  ##################### summary statistics table data and time series data
  querry_sum_stats_table <- reactive({
    ##### set up querry string for sql

    #### get tweet_length filter, 81 for long==T, 0 for long==F
    long <- long()
    ##### for unfitlered tweets
    if (input$comp == "NoFilter"){
      table_name <- glue("sum_stats_{tolower(input$lang)}")

      glue('SELECT *  FROM {table_name}  WHERE
         retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long}' )


    } else { #if company is chosen
      ### replace umlaute from input, 1233

      ### control for münchner rück and deutsch börse which are stored with normal
      # and corrupted umlaute


      if( input$comp == "Münchener Rück"){
        glue('SELECT *  FROM sum_stats_companies WHERE
         retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long} and company  = "Münchener Rück" or company ="MÃ¼nchener RÃ¼ck"
         and
             language = "{tolower(input$lang)}"' )
      } else if (input$comp == "Deutsche Börse") {


        glue('SELECT *  FROM sum_stats_companies WHERE
         retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long} and company  = "Münchener Rück" or company ="MÃ¼nchener RÃ¼ck"
         and
             language = "{tolower(input$lang)}"' )

      } else {

        glue('SELECT *  FROM sum_stats_companies WHERE
         retweets_count = {input$rt} and likes_count = {input$likes} and
         tweet_length = {long} and company  = "{input$comp}" and
             language = "{tolower(input$lang)}"' )
      }
    }


  })



  #########################################################################
  ############################# get data for sum stats table
  get_data_sum_stats_tables <- reactive({
    ###### need correct path
    validate(need(correct_path() == T, "Please choose the correct path"))
    ###### need database connection
    validate(need(database_connector(), "Could not connect to database"))
    ###### need at least one date selected
    validate(need(!is.null(input$dates_desc), "Please select a date."))

    ####### store database connection
    con <- database_connector()
    ##### check if connection is null
    string_value <- is.null(con)
    req(!string_value)

    ###### querry data from sql
    df_need <- DBI::dbGetQuery(con,  querry_sum_stats_table())

    #### account for case where there is no data available for filters
    validate(need(dim(df_need)[1] > 0, "No data available for current selection" ))

    #### for companies replace umlaute
    if ("company" %in% names(df_need)){
      df_need$company <- gsub("Ã¶", "ö", df_need$company)
      df_need$company <- gsub("Ã¼", "ü", df_need$company)
    }

    #### drop duplicates
    df_need <- unique(df_need)

    #### return df
    df_need
  })


  #########################
  ################################# sum stats table
  output$sum_stats_table <- function(){

    ###### use data from sql from previous step
    df_need <- get_data_sum_stats_tables()
    ##### create summary stats table with function
    df_need <- sum_stats_table_creator(df_need, dates_desc()[1], dates_desc()[2])

    ### need to have data
    validate(need(!is.null(df_need), "No data available for current selection" ))
    df_need
  }





  ##################################
  ################################################### output time series


  ###### title for dygraphs
  number_tweets_info_desc <- reactive({
    ##### get data from sql
    df_need <- get_data_sum_stats_tables()

    #convert to date
    df_need$created_at <- as.Date(df_need$created_at)

    ###### filter for dates input from user
    df_need <- df_need %>%
      filter(between(created_at, as.Date(dates_desc()[1]), as.Date(dates_desc()[2])))
    ### need to have data
    validate(need(dim(df_need)[1] > 0, "No data available for current selection" ))

    ##### for tweet type input get nice company name accroding to named list
    if (input$comp == "NoFilter"){
      comp_name <- names(purrr::flatten(company_terms))[purrr::flatten(company_terms) == input$comp]
    } else {
      comp_name <- glue("{names(purrr::flatten(company_terms))[purrr::flatten(company_terms) == input$comp]} Tweets")
    }

    num_tweets <- as.integer(round(sum(df_need$N)))
    num_tweets <- formatC(num_tweets, format="f", big.mark = ",", digits=0)

    num_tweets_avg <- formatC(round(mean(df_need$N)), format="f", big.mark = ",", digits=0)

    ##### set up string for header of time series graphs
    glue("{comp_name} ({num_tweets} tweets total;
           {num_tweets_avg} on average per day)")
  })


  ###### title for summary statistics
  output$sum_stats_table_header <- renderText({
    header <- number_tweets_info_desc()
    header <- sub( "total.*$", "", header )
    glue("Summary Statistics for {header} total)")
  })



  #################################################
  ############################## time series plot
  ###################################################
  output$sum_stats_plot <- dygraphs::renderDygraph({

    #### need at least one inputalue(likes/retweets etc.)
    req(!is.null(input$value) | input$num_tweets_box == T)

    ###### need at least two days selected for time series plot
    validate(need(length(input$dates_desc) != 1, "Cannot plot time series for single date. Please select more dates."))

    ##### need date input to not be null
    validate(need(!is.null(input$dates_desc), "Please select a date."))

    ##### header for plots
    input_title <- number_tweets_info_desc()

    ### get data for plots
    df <- get_data_sum_stats_tables()

    ### need to have at least to obs for time series
    validate(need(dim(df)[1] > 1, "Only one date found for current selection. Cannot plot time series for single date"))


    #### when number of tweets should not be plotted
    if (input$num_tweets_box == F){
      p <-  time_series_plotter2(df, input$metric, input$value, num_tweets = F,
                                 input$dates_desc[1], input$dates_desc[2],
                                 input_title = input_title,
                                 group = "twitter_desc")

      ### need to have data
      validate(need(!is.null(p), "No data available for current selection" ))
      p
    } else { ##### when number of tweets should be plotted

      p <- time_series_plotter2(df, input$metric, input$value, num_tweets = T,
                                input$dates_desc[1], input$dates_desc[2],
                                input_title = input_title,
                                group = "twitter_desc")

      ### need to have data
      validate(need(!is.null(p), "No data available for current selection" ))
      p
    }

  })



  #####################################################################
  ################ for second time series plot from saving plot button
  ##################################
  save_plot <- reactiveValues(data = NULL)

  ##### if button is clicked store time series plot in serperate part
  observeEvent(input$plot_saver_button, {

    #### date input cannot be null
    validate(need(!is.null(input$dates_desc), "Please select a date."))
    ###### need at leat ne value selected
    req(!is.null(input$value) | input$num_tweets_box == T)
    ####### need at least dates selceted for time series plot
    validate(need(length(input$dates_desc) > 1, "Cannot plot time series for single day"))

    ##### get plot header
    input_title <- number_tweets_info_desc()

    # get df
    df <- get_data_sum_stats_tables()


    ###### in case where number of tweets should not be included in plot
    if (input$num_tweets_box == F){
      save_plot$plot <- time_series_plotter2(df, input$metric, input$value, num_tweets = F,
                                             input$dates_desc[1], input$dates_desc[2], r,
                                             date_range = F,
                                             input_title = input_title,
                                             group = "twitter_desc")
    } else { ## in case where number of tweets should be included in plot
      save_plot$plot <- time_series_plotter2(df, input$metric, input$value, num_tweets = T,
                                             input$dates_desc[1], input$dates_desc[2], r,
                                             date_range = F,
                                             input_title = input_title,
                                             group = "twitter_desc")
    }

  })

  ######## time series plot when pressing save plot button
  output$sum_stats_plot2 <-dygraphs::renderDygraph({

    req(!is.null(save_plot$plot))
    save_plot$plot
    # dygraphs::dygraph(don) %>%
    #   dygraphs::dyRangeSelector( input$dates_desc + 1, retainDateWindow = T
    #   )
  })





  ##############################################################################
  ############################### data retriever for histogram
  data_histo <- reactive({
    #validate(need(path_setter()[[3]] == "correct_path", "Please select the correct path"))
    validate(need(!is.null(input$dates_desc), "Please select a date."))
    validate(need(correct_path() == T, "Please choose the correct path"))


    lang <- lang_converter()




    # for case no company selected
    if (input$comp == "NoFilter"){
      file_path <- file.path(glue("Twitter/plot_data/{lang}_NoFilter/{querry_histo()}"))
      exists <- file.exists(file_path)
      shinyFeedback::feedbackDanger("histo_plot", !exists, "Please make sure you picked the correct path. The \n
                                file cannot be found in the current directory")
      req(exists)
      df_need <- data.table::fread(file_path,
                                   select = 1:3)

      ### need to have data
      validate(need(dim(df_need)[1] > 0, "No data available for current selection" ))

      #### drop duplicates
      df_need <- unique(df_need)

      df_need
    } else { #for case of choosen company
      file_path <- file.path(glue("Twitter/plot_data/Companies/{input$comp}/{querry_histo()}"))
      df_need <- data.table::fread(file_path,
                                   select = 1:4) %>% filter(language == tolower(input$lang)) %>%
        select(-language)
      ### need to have data
      validate(need(dim(df_need)[1] > 0, "No data available for current selection" ))


      #### drop duplicates
      df_need <- unique(df_need)

      df_need

    }
  })



  ###########################################################
  ######################################## histogram output
  output$histo_plot <- plotly::renderPlotly({
    validate(need(!is.null(input$dates_desc), "Please select a date."))

    req(input$histo_value)


    p <- histogram_plotter(data_histo(), date_input1 = dates_desc()[1], date_input2 = dates_desc()[2],
                           input_bins = input$bins, input_log = input$log_scale)

    #### check if data in plot
    validate(need(!is.null(p), "No data available for current selection" ))

    # plot the plot
    p

  })


  ##################### disable log scale option for sentiment because as negative values
  observeEvent(input$histo_value, {

    if (grepl("sentiment",input$histo_value)) {
      shinyWidgets::updateSwitchInput(session = session,
                                      "log_scale",
                                      disabled = T,
                                      value = F)
    } else {
      shinyWidgets::updateSwitchInput(session = session,
                                      "log_scale",
                                      disabled = F)
    }
  })



  ####################### histogram title
  output$histo_plot_info <- renderText({

    selected_value <- input$value[1]

    selected_value <- stringr::str_replace(selected_value, "sentiment_rt", "Retweets weighted Sentiment")
    selected_value <- stringr::str_replace(selected_value, "sentiment_likes", "Likes weighted Sentiment")
    selected_value <- stringr::str_replace(selected_value, "sentiment_length", "Tweet Length weighted Sentiment")
    selected_value <- stringr::str_replace(selected_value, "likes", "Likes")
    selected_value <- stringr::str_replace(selected_value, "rt", "Retweets")
    selected_value <- stringr::str_replace(selected_value, "tweet_length", "Tweet Length")
    selected_value <- stringr::str_replace(selected_value, "sentiment", "Sentiment")


    glue("Distribution of {selected_value} for indivdual tweets")

  })




  ######################################################
  ########################## Word Frequencies ###########
  #######################################################
  lang_converter <- reactive({

    lang <- stringr::str_to_title(input$lang)
  })

  data_expl <- reactive({

    lang <- lang_converter()



    if (input$long == T){
      long <- "long_only"
      tweet_length_filter <- 81
    } else{
      long <- "all"
      tweet_length_filter <- 0
    }

    validate(need(correct_path() == T, "Please choose the correct path"))

    # if (correct_path == "correct_path"){
    #   Sys.sleep(0.2)
    # } else{
    #   return()
    # }

    # go into specified folder and load dataframe


    if (input$ngram_sel == "Unigram"){
      subfolder <- "uni_appended"
      add_on <- "uni"
    } else {
      subfolder <- "bi_appended"
      add_on <- "bi"
    }


    if (input$comp != "NoFilter") {
      folder <- file.path("Companies")
      file_name <- glue("term_freq_{input$comp}_all_rt_{input$rt}_li_{input$likes}_lo_{long}.csv")
      file_path <- file.path("Twitter/term_freq",folder, subfolder, file_name)
      # read file
      dt <- data.table::fread(file_path, select = c("date_variable",
                                                    "language_variable",
                                                    "word",
                                                    "N",
                                                    "emo"),
                              colClasses = c("date_variable" = "Date"))

      #### drop duplicates
      dt <- unique(dt)

      #### check if data in dt
      validate(need(dim(dt)[1] > 0, "No data available for current selection"))




      ### return dt
      dt
    } else {
      folder <- glue("{lang}_NoFilter")
      file_name <- glue("{add_on}_{lang}_NoFilter_rt_{input$rt}_li_{input$likes}_lo_{long}.csv")
      file_path <- file.path("Twitter/term_freq",folder, subfolder, file_name)
      # read file
      dt <- data.table::fread(file_path, select = c("date",
                                                    "language",
                                                    "word",
                                                    "N",
                                                    "emo"),
                              colClasses = c("date" = "Date"))

      #### drop duplicates
      dt <- unique(dt)

      #### check if data in dt
      validate(need(dim(dt)[1] > 0, "No data available for current selection"))

      ### return dt
      dt
    }






    #%>%
    # filter(between(date_variable, input$dates[1], input$dates[2]))








  })



  word_freq_df <- reactive({
    #validate(need(path_setter()[[3]] == "correct_path", "Please select the correct path"))
    validate(need(!is.null(input$dates_desc), "Please select a date."))

    if (input$ngram_sel == "Unigram"){
      input_word_freq_filter <- ""
    } else {
      input_word_freq_filter <- input$word_freq_filter
    }
    df <- word_freq_data_wrangler(data_expl(), dates_desc()[1], dates_desc()[2],
                                  input$emo, emoji_words,
                                  input_word_freq_filter,
                                  tolower(input$lang),
                                  input$comp)



    df
  })



  ######################### freq_plot
  output$freq_plot <- plotly::renderPlotly({
    # dynamically change height of plot
    #height = function() input$n * 30 + 400,



    #### account for empty df
    validate(need(!is.null(word_freq_df()), "No data available for current selection"))


    df <- df_filterer(word_freq_df() , input$n_freq)

    term_freq_bar_plot(df)


  })

  ################## wordcloud
  output$cloud <- renderUI({
    wordcloud2::wordcloud2Output("wordcloud", width = (8/12) * 0.925 * input$dimension[1], height = 1000) %>%
      shinycssloaders::withSpinner(type = 5)

  })

  output$wordcloud <- wordcloud2::renderWordcloud2({
    req(input$plot_type_expl == "Word Cloud")

    #### account for empty df
    validate(need(!is.null(word_freq_df()), "No data available for current selection"))

    df <- df_filterer(word_freq_df(), input$n_freq_wc)



    word_cloud_plotter(df, input$size_wordcloud)

  })



  ####################################### number unique words
  output$number_words <- reactive({

    ###### number of total tweets
    df_need <- get_data_sum_stats_tables()
    #convert to date
    df_need$created_at <- as.Date(df_need$created_at)
    # filter dates
    df_need <- df_need %>%
      filter(between(created_at, as.Date(dates_desc()[1]), as.Date(dates_desc()[2])) &
               language == tolower(input$lang))
    # get number of total tweets
    number_tweets <- round(sum(df_need$N))


    #### number of unqiue words/bigrams
    number_words <-  unique_words(word_freq_df())

    if (is.null(number_words)){
      number_words <- 0
    }

    HTML(glue("Number of unique {tolower(input$ngram_sel)}s available for current selection: {number_words} <br>
         Number of tweets for current selection: {number_tweets}"))

  })




  ############################## time series bigram plot
  # output$word_freq_time_series <- plotly::renderPlotly({
  #   req(length(input$dates_desc) > 1)
  #   df <- word_freq_data_wrangler(data_expl(), dates_desc()[1], dates_desc()[2],
  #                                 input$emo, emoji_words,
  #                                 input$word_freq_filter, input$lang,
  #                                 input$comp)
  #
  #    word_filter_time_series_plotter(df)
  # })



  ###########################################################################
  ###########################################################################
  ###########################################################################
  ######################### GOING DEEPER ####################################
  ###########################################################################
  ###########################################################################
  # path to markdown files for helpers
  shinyhelper::observe_helpers(withMathJax = TRUE, help_dir = "shiny/helpers")


  ###### network plot

  data_getter_net_react <- reactive({
    if(length(input$dates_net) > 1){
      days_inrange <- difftime(as.Date(input$dates_net[2]) ,as.Date(input$dates_net[1]) , units = c("days"))
      validate(need(days_inrange <= 1,"More than 2 days selected. Please choose a maximum of 2 days."))
    }

    lang <- stringr::str_to_title(input$lang_net)
    df <- network_plot_datagetter(lang, input$dates_net[1], input$dates_net[2], input$comp_net)



    df
  })

  data_filterer_net_react <- reactive({
    df <- data_getter_net_react()
    if (dim(df)[1] > 0){

      df <- network_plot_filterer(df, input$rt_net, input$likes_net, input$long_net,
                                  input$sentiment_net, input$search_term_net,
                                  input$username_net, input$lang_net)
    }
    df

  })


  observe({

    ######### disable render plot button if incorrect path, no date or too many dates selected
    if (length(input$dates_net) > 1){
      if (difftime(as.Date(input$dates_net[2]) ,
                   as.Date(input$dates_net[1]) , units = c("days")) > 1) {
        removeUI("#network_plot")
        shinyjs::disable("button_net")
      } else {
        shinyjs::enable("button_net")
      }
    } else if (correct_path() == F | is.null(input$dates_net)){

      removeUI("#network_plot")
      shinyjs::disable("button_net")
    } else if (input$username_net != "" & nchar(input$username_net) < 4){
      removeUI("#network_plot")
      shinyjs::disable("button_net")
    } else {
      shinyjs::enable("button_net")
    }
  })


  ###### date checker
  ##### validate that a maximum of 2 days have been selected

  output$date_checker_net <- renderText({

    if(length(input$dates_net) > 1){
      days_inrange <- difftime(as.Date(input$dates_net[2]) ,as.Date(input$dates_net[1]) , units = c("days"))
      if (days_inrange >= 2){

        validate("More than 2 days selected. Please choose a maximum of 2 days.")

      }
    } else if (is.null(input$dates_net)){

      validate("Need to select at least one day.")
    }
  })


  ###### username checker
  ##### validate that more than 3 chars are put in
  output$username_checker <- renderText({
    if (input$username_net != "" & nchar(input$username_net) < 4){
      validate("Usernames must have at least 4 characters.")
    }


  })

  ############################################################
  ############################ BUTTON RENDER PLOT ############
  ############################################################
  # if button is clicked compute correlations and plot the plot
  observeEvent(input$button_net,{
    removeUI("#network_plot")
    ##### need correct path
    validate(need(correct_path() == T, "Please choose the correct path"))

    ###### need at least one date selected
    validate(need(!is.null(input$dates_net), "Please select a date."))






    ##### disable render plot button so no mutliple firing possible
    shinyjs::disable("button_net")
    ### enable the cancel computation button only during rendering
    shinyjs::enable("cancel_net")




    #### start waitress for progress bar
    waitress <- waiter::Waitress$new("nav", max = 4,  theme = "overlay")
    #Automatically close it when done
    on.exit(waitress$close())

    waitress$notify()

    ### progress bar elements
    #hostess <- waiter::Hostess$new("load")




    ################################






    initial.ok <- input$cancel_net


    shinyjs::showElement(id = "loading")
    # disable the button after computation started so no new computation can
    # be startedd





    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

    ### read all files for the dates

    df <- data_getter_net_react()

    #hostess$set(2 * 10)
    waitress$inc(1)


    if(is.null(df) | dim(df)[1] == 0){
      showNotification("Tweets not available for this date yet", type = "error")
      enable("button_net")
      removeUI("#network_plot")
      return()
    }

    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }




    network <- data_filterer_net_react()



    if(is.null(network) | dim(network)[1] == 0){
      showNotification("No tweets found", type = "error")
      enable("button_net")
      removeUI("#network_plot")
      return()
    }

    ##### compute minimum n which is set to 0.05% of the number of tweets for the current dataset
    min_n <- round(0.001 * dim(network)[1])

    #hostess$set(2 * 10)
    waitress$inc(1)


    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

    if (input$word_type_net == "word_pairs_net"){
      network <- network_unnester(network, df, input$emo_net)
    } else{
      network <- network_unnester_bigrams(network, input$emo_net)
    }
    validate(need(dim(network)[1] > 0, "No data found for current selection"))
    if(is.null(network) | dim(network)[1] == 0){
      enable("button_net")
      return()
    }
    #hostess$set(2 * 10)
    waitress$inc(1)


    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

    if (input$word_type_net == "word_pairs_net"){
      df <- network_word_corr(network, input$n_net,
                              input$corr_net, min_n,
                              input$username_net)
    } else {
      df <- network_bigrammer(df, network, input$n_net, input$n_bigrams_net,
                              min_n, input$username_net)
    }

    if(is.null(df) | length(df) == 0){
      showNotification("No tweets found or thresholds too high", type = "error")
      enable("button_net")
      removeUI("#network_plot")
      return()
    }



    # hostess$set(2 * 10)
    waitress$inc(1)




    if (initial.ok < input$cancel_net) {
      initial.ok <<- initial.ok + 1
      validate(need(initial.ok == 0, message = "The computation has been aborted."))
    }

    insertUI("#placeholder", "beforeEnd", ui = networkD3::forceNetworkOutput("network_plot", height ="1000px"))


    # render the network plot
    if (input$word_type_net == "word_pairs_net"){
      output$network_plot <- networkD3::renderForceNetwork({





        req(input$button_net)
        #if (is.null(df)) return()
        validate(need(!is.null(df), message = "No data found for current selection"))
        if (initial.ok < input$cancel_net) {
          initial.ok <<- initial.ok + 1
          validate(need(initial.ok == 0, message = "The computation has been aborted."))
        }


        #hostess$set(2 * 10)
        # waitress$inc(1)
        network_plot_plotter(df)



      })
    } else {


      output$network_plot <- networkD3::renderForceNetwork({
        req(input$button_net)
        #if (is.null(df)) return()
        validate(need(!is.null(df), message = "No data found for current selection"))
        if (initial.ok < input$cancel_net) {
          initial.ok <<- initial.ok + 1
          validate(need(initial.ok == 0, message = "The computation has been aborted."))
        }

        #hostess$set(2 * 10)
        #waitress$inc(1)

        ###### plot the network plot
        network_plot_plotter_bigrams(df)




      })



    }
    ##### when process has run successfully enable render plot button again
    # and disable cancel button again
    shinyjs::enable("button_net")
    shinyjs::disable("cancel_net")

    ### enable remove plot button
    shinyjs::enable("reset_net")

  })

  ########## when button is pressed remove ui element (network plot)
  observeEvent(input$reset_net,{

    removeUI("#network_plot")
  })

  # observeEvent(input$button_net, {
  #
  #
  # })
  # ##

  ######## message for aborting process
  observeEvent(input$cancel_net, {
    ### when button is pressed show error and abort process
    showNotification("Computation has been aborted", type = "error")
  })




  output$raw_tweets_net <- DT::renderDataTable({

    ##### only work when correct path is choosen
    validate(need(correct_path() == T, "Please choose the correct path"))

    #### need at least one date selected
    validate(need(!is.null(input$dates_net), "Please select a date."))

    #### get filtered data
    dt <- data_filterer_net_react()

    # change to nicer names
    dt <- dt[, c("created_at", "tweet", "text", "username","sentiment", "retweets_count", "likes_count")]
    names(dt) <- c("Date", "Orig.Tweet", "CleanTweet", "Username","Sentiment", "Retweets", "Likes")

    ### set up shiny datatable with custom style
    DT::datatable(dt, options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ), rownames = F
    ) %>% DT::formatStyle(columns = c(1), width='75px')
  })


  ##### number tweets info network
  output$number_tweets_net <- renderText({
    req(correct_path() == T)
    req(!is.null(input$dates_net))
    glue("Found {dim(data_filterer_net_react())[1]} tweets for current selection")
  })


  ####### description of network analysis
  output$network_description <- renderUI({
    HTML(network_description_text)


  })


  #########################################################################
  #########################################################################
  #############################comparison tab #############################
  #########################################################################
  #########################################################################


  #### get stock data for comparison tab
  get_stock_data_comp <- reactive({
    validate(need(correct_path() == T, "Please choose the correct path"))

    data.table::fread("Yahoo/Full/all_full.csv")
  })


  ###### plot stocks
  output$stocks_comp <- dygraphs::renderDygraph({
    req(input$stocks_comp)

    stock_plotter(get_stock_data_comp(), input$stocks_metric_comp, input$stocks_comp, input$roll_stock_comp)
  })



  ##### get control variables
  df_controls_comp <- reactive({


    ## validity checks
    req(!is.null(input$ControlCountry))
    validate(need(correct_path() == T, "Please choose the correct path"))


    #### only load data when control is slected (and not covid)
    if (input$controls_comp %in% c("coronavirus","OFR.FSI",
                                   "VIX", "WLEMUINDXD")) {


      if (length(input$ControlCountry) == 2) {


        ### load both dfs and join them to one
        df1 <- data.table::fread("Twitter/sentiment/Model/controls_DE.csv") %>%
          select(Date, input$controls_comp)
        names(df1) <- c("Date", "Germany")





        df2 <- data.table::fread("Twitter/sentiment/Model/controls_US.csv") %>%
          select(Date, input$controls_comp)
        names(df2) <- c("Date", "USA")


        ## join dfs
        df <- df1 %>% dplyr::inner_join(df2)



      } else if (input$ControlCountry == "Germany"){
        lang <- "DE"

        df <- data.table::fread(glue("Twitter/sentiment/Model/controls_{lang}.csv")) %>%
          select(Date, input$controls_comp)

      } else if (input$ControlCountry == "United States") {
        lang <- "US"

        df <- data.table::fread(glue("Twitter/sentiment/Model/controls_{lang}.csv")) %>%
          select(Date, input$controls_comp)

      }






    } else {
      df <- data.table::fread("Corona/owid.csv")
    }
    ### make sure df not empty
    req(!is.null(df) & dim(df)[1] > 0)

    df


  })


  ###### plot controls
  output$covid_comp <- dygraphs::renderDygraph({


    if (input$controls_comp %in% c("coronavirus","OFR.FSI",
                                   "VIX", "WLEMUINDXD")) {


      controls_plotter(df_controls_comp(), input$controls_comp, input$ControlCountry, input$roll_control_comp)

    } else {

      covid_plotter(df_controls_comp(), input$controls_comp, input$ControlCountry, input$roll_control_comp)
    }







  })



  ######### get data for twitter time series
  get_querry_twitter_comparison <- reactive({

    ##### set up querry string for sql

    #### get tweet_length filter, 81 for long==T, 0 for long==F
    if (input$long_comp == T){
      long <- 81
    } else{
      long <- 0
    }



    ##### for unfitlered tweets
    if (input$twitter_comp_comp == "NoFilter"){
      table_name <- glue("sum_stats_{tolower(input$lang_comp)}")

      glue('SELECT *  FROM {table_name}  WHERE
         retweets_count = {input$rt_comp} and likes_count = {input$likes_comp} and
         tweet_length = {long}' )


    } else { #if company is chosen
      ### replace umlaute from input, 1233

      comp <- gsub("ö","Ã¶", input$twitter_comp_comp)
      comp <- gsub("ü", "Ã¼", comp)

      glue('SELECT *  FROM sum_stats_companies WHERE
         retweets_count = {input$rt} and likes_count = {input$likes_comp} and
         tweet_length = {long} and company  = "{comp}" and
             language = "{tolower(input$lang_comp)}"' )
    }


  })


  get_twitter_comp_data <- reactive({
    ###### need correct path
    validate(need(correct_path() == T, "Please choose the correct path"))
    ###### need database connection
    validate(need(database_connector(), "Could not connect to database"))
    ###### need at least one date selected


    ####### store database connection
    con <- database_connector()


    ###### querry data from sql
    df <- DBI::dbGetQuery(con,  get_querry_twitter_comparison())

    #### for companies replace umlaute
    if ("company" %in% names(df)){
      df$company <- gsub("Ã¶", "ö", df$company)
      df$company <- gsub("Ã¼", "ü", df$company)
    }
    #### return df
    df
  })



  ####### get header for plot
  ###### title for dygraphs
  get_header_twitter_comp <- reactive({
    ##### get data from sql
    df_need <- get_twitter_comp_data ()

    #convert to date
    df_need$created_at <- as.Date(df_need$created_at)

    ##### for tweet type input get nice company name according to named list
    if (input$twitter_comp_comp == "NoFilter"){
      comp_name <- names(purrr::flatten(company_terms))[purrr::flatten(company_terms) == input$twitter_comp_comp]
    } else {
      comp_name <- glue("{names(purrr::flatten(company_terms))[purrr::flatten(company_terms) == input$twitter_comp_comp]} Tweets")
    }

    ##### set up string for header of time series graphs
    glue("{comp_name} ({round(sum(df_need$N))} tweets total;
           {round(mean(df_need$N))} on average per day)")
  })



  ######## twitter plot
  output$twitter_comp <- dygraphs::renderDygraph({
    ##### get df
    df <- get_twitter_comp_data()

    #### set up header of plot

    title = get_header_twitter_comp()
    ###### plot the data
    time_series_plotter2(df, "mean", input$value_comp, num_tweets = F,
                         min(as.Date(df$created_at)), max(as.Date(df$created_at)), dates = NA, date_range =F,
                         title, group = "comp_plots",
                         input_roll = input$roll_twitter_comp,
                         ribbon = F)
  })



  ###############################################################################
  ########################   XGboost    #########################################
  ###############################################################################
  ###flexible input for stocks: show either german or us companies
  #observe_helpers(withMathJax = TRUE, help_dir = "helpers")


  output$stock_regression_xgb <- renderUI({
    req( correct_path()== T)
    if (input$country_regression_xgb == "Germany"){
      input <- selectizeInput("Stock_Regression_xgb","Choose company or Index:",
                              #c(COMPONENTS_DE()[["Company.Name"]],"GDAXI"),
                              company_terms_stock_ger,
                              selected = "DAX",multiple = FALSE)
    } else {
      input <- selectizeInput("Stock_Regression_xgb","Choose company or Index:",
                              #c(COMPONENTS_US()[["Company.Name"]],"DJI"),
                              company_terms_stock_us,
                              selected = "Dow Jones Industrial",multiple = FALSE)
    }
  })


  output$Controls_xgb <- renderUI({
    #res <- dataset()
    #res$name <- NULL
    req( correct_path()== T)
    if (input$country_regression_xgb == "Germany"){
      input <- selectizeInput("Controls_xgb","Control variables:",
                              c("Google-Trends Coronavirus"="coronavirus",
                                "VIX"="VIX",
                                "Financial Distress Index"="OFR.FSI",
                                "Economic Uncertainty Index"="WLEMUINDXD",
                                "DAX"="DAX"),selected = "VIX",multiple = TRUE)
      #c(colnames(res[3:length(res)])),multiple = TRUE
    }else{
      input <- selectizeInput("Controls_xgb","Control variables:",
                              c("Google-Trends Coronavirus"="coronavirus",
                                "VIX"="VIX",
                                "Financial Distress Index"="OFR.FSI",
                                "Economic Uncertainty Index"="WLEMUINDXD",
                                "DJI"="DJI"),selected = "VIX",multiple = TRUE)
    }

  })


  dataset_xgb <- reactive({
    req( correct_path()== T)
    if (input$country_regression_xgb == "Germany"){
      data_reg <- filter(stockdata_DE(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                         #.data$name %in% (c(COMPONENTS_DE()[["Symbol"]], "GDAXI")[c(COMPONENTS_DE()[["Company.Name"]], "GDAXI") %in% .env$input$Stock_Regression_xgb]) &
                         .data$name %in% .env$input$Stock_Regression_xgb &
                           .data$Dates >= .env$input$date_regression_xgb[1] & .data$Dates <= .env$input$date_regression_xgb[2])[c("Dates",input$regression_outcome_xgb,"name")] #hier später noch CLose flexibel machen
    } else {
      data_reg <- filter(stockdata_US(),                                                                               #nur hier nach datum filtern, rest wird draufgemerged
                         #.data$name %in% (c(COMPONENTS_US()[["Symbol"]], "DJI")[c(COMPONENTS_US()[["Company.Name"]], "DJI") %in% .env$input$Stock_Regression_xgb]) &
                         .data$name %in% .env$input$Stock_Regression_xgb &
                           .data$Dates >= .env$input$date_regression_xgb[1] & .data$Dates <= .env$input$date_regression_xgb[2])[c("Dates",input$regression_outcome_xgb,"name")] #hier später noch CLose flexibel machen
    }

    if (input$country_regression_xgb == "Germany"){
      global_controls <- global_controls_test_DE()   #load controls
      global_controls$Date <- as.Date(global_controls$Date) #transform date
      dax <- dplyr::filter(stockdata_DE(),.data$name %in% c("GDAXI")&
                             .data$Dates >= min(global_controls$Date) & .data$Dates <= max(global_controls$Date))[c("Dates","Close")]
      colnames(dax)[1]<-"Date"
      # dax <- GDAXI()  #load dax
      # dax$Date <- as.Date(dax$Date, "%d %b %Y") #transform date
      # dax <- missing_date_imputer(dax,"Close.") #transform time series by imputing missing values
      colnames(dax)[2] <- "DAX"  #rename ->   !! is not renamed in final dataset !! -> dont know why
      global_controls <- dplyr::left_join(dax,global_controls,by = c("Date")) #join final

    }else {
      global_controls <- global_controls_test_US() #same procedure as above
      global_controls$Date <- as.Date(global_controls$Date)
      dji <- dplyr::filter(stockdata_US(),.data$name %in% c("DJI")&
                             .data$Dates >= min(global_controls$Date) & .data$Dates <= max(global_controls$Date))[c("Dates","Close")]
      colnames(dji)[1]<-"Date"
      # dow <- DOW()
      # dow$Date <- as.Date(dow$Date, " %b %d, %Y")
      # dow <- missing_date_imputer(dow,"Close.")
      colnames(dji)[2] <- "DJI"
      global_controls <- dplyr::left_join(dji,global_controls,by = c("Date"))
    }
    names(global_controls)[1] <- "Dates"
    data_reg2 <- left_join(data_reg,global_controls,by = c("Dates")) #hierdurch kommt die varible "global" in den datensatz
    ##diesen datensatz filtern wir dann nochmal mit dem sliderinput für die kontrollvariablen(eine/keine/mehrere möglich)
    data_reg2
  })



  df_selected_controls_xgb <- reactive({
    #req(input$Controls_var)
    res <- dataset_xgb()
    res <- res[c("Dates",input$regression_outcome_xgb,input$Controls_xgb)]
    res
  })


  observeEvent(input$Sentiment_type_xgb, {                         #Observe event from input (model choices)
    req(input$Sentiment_type_xgb)
    updateTabsetPanel(session, "params_xgb", selected = input$Sentiment_type_xgb)
  })

  observeEvent(input$industry_sentiment_xgb, {                         #Observe event from input (model choices)
    req(input$industry_sentiment_xgb)
    updateTabsetPanel(session, "industry_tab", selected = input$industry_sentiment_xgb)
  })


  # dataset_senti_xgb <- reactive({
  #   req( correct_path()== T)
  #   req(input$Sentiment_type_xgb)
  #   if(input$Sentiment_type_xgb == "NoFilter"){
  #
  #     res <- En_NoFilter_0_0_yes()   # still fix as it is not clear yet if sql or csv
  #     #res <- eval(parse(text = paste('En', '_NoFilter_',input$minRetweet,'_',
  #     #                               input$minminLikes,'_',input$tweet_length,'()', sep='')))
  #     #input$language
  #   }else{
  #     req(input$Stock_reg)
  #     ticker <- ticker_dict(input$Stock_reg) # dict for a few stock
  #     res <- eval(parse(text = paste(ticker,'()', sep=''))) # example: ADS.DE()
  #
  #   }
  #
  #
  # })
  #
  #
  # filtered_df_xgb <- reactive({
  #   req( correct_path()== T)
  #   req(input$Sentiment_type_xgb)
  #   req(input$minRetweet_stocks1_xgb)
  #   req(input$minRetweet_stocks2_xgb)
  #
  #   if(input$Sentiment_type_xgb == "NoFilter"){
  #
  #     res <- dataset_senti_xgb()
  #   }else{ # live filtering
  #     req(input$industry_sentiment_xgb)
  #     res <- dataset_senti_xgb()
  #     if(input$industry_sentiment_xgb == "no"){
  #       res <- dataset_senti_xgb()
  #       if(input$tweet_length_stock1_xgb == "yes"){
  #
  #         res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_xgb)) &
  #                                 (tweet_length > 81))}
  #       else{
  #         res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks1_xgb)))
  #       }
  #     }#else{
  #     #res <- dataset_senti()
  #     #if(input$tweet_length_stock2 == "yes"){
  #     # res <- res %>% filter((retweets_count > as.numeric(input$minRetweet_stocks2)) &
  #     #                          (tweet_length > 81))
  #     #}else{
  #     #  res <- res %>% filter(retweets_count > as.numeric(input$minRetweet_stocks2))
  #     #}
  #     #}
  #   }
  # })
  #
  #
  # aggri_select_xgb <- reactive({
  #
  #   if(input$Sentiment_type_xgb == "NoFilter"){ # NoFilter files already aggregated
  #     res <- filtered_df_xgb()
  #     aggregation <- key(input$aggregation_xgb)  # select aggregation type: Mean, mean weighted by,...
  #     res <- res %>% tidyr::gather("id", "aggregation", aggregation)
  #     res <- res[c("date","aggregation")]
  #   }else{
  #     if(input$industry_sentiment_xgb == "no"){
  #       res <- filtered_df_xgb()
  #       res <- aggregate_sentiment(res) # function to aggregate sentiment per day
  #       res <- res %>% filter(language == input$language1_xgb)
  #       aggregation <- key(input$aggregation1_xgb)
  #       res <- res %>% tidyr::gather("id", "aggregation", aggregation)
  #       res <- res[c("date","aggregation")]
  #     }else{
  #       res <- get_industry_sentiment(COMPONENTS_DE(),input$industry_xgb,input$minRetweet_stocks2_xgb,
  #                                     input$tweet_length_stock2_xgb)      #function to gather all stock in certain industry
  #       aggregation <- key(input$aggregation2_xgb)                          #--> also calculates aggregation inside function
  #       res <- res %>% tidyr::gather("id", "aggregation", aggregation)
  #       res <- res[c("date","aggregation")]
  #     }
  #   }
  #
  # })


  observeEvent(input$reset_regression_xgb,{
    updateSelectizeInput(session,"Controls_xgb",selected = "")
  })

  #merge sentiment with control+dep vars
  final_regression_df_xgb <- reactive ({
    if (input$senti_yesno_xgb == TRUE){
      res <- get_sentiment_xgb()
    } else {
      res <- get_sentiment_xgb()[1]
    }
    res$created_at <- as.Date(res$created_at)
    res_c <- df_selected_controls_xgb()

    res <- left_join(res_c,res, by=c("Dates" = "created_at"))
    res_corona <- df_selected_corona_xgb()
    res_corona$date <- as.Date(res_corona$date)
    res <- left_join(res,res_corona,by=c("Dates" = "date"))
    res
  })


  ############################################################################# sql data xgb
  dates_xgb <- reactive({
    if (length(input$date_regression_xgb) > 1){
      input$date_regression_xgb
    } else {
      c(input$date_regression_xgb, input$date_regression_xgb)
    }
  })


  querry_sentiment_model_xgb <- reactive({

    #### check which tweet length
    if (input$tweet_length_xgb == T){
      tweetLength <- 81
    } else {
      tweetLength <- 0
    }



    dates <- dates_xgb()



    ###### table name
    ### get language
    if (input$sentiment_company_xgb == "NoFilter"){
      test <- glue('select created_at, {input$aggregation_xgb} from sum_stats_{tolower(input$language_xgb)} where
    created_at >= "{dates[1]}" and created_at <= "{dates[2]}" and
    retweets_count = {input$minRetweets_xgb} and likes_count = {input$minLikes_xgb} and
    tweet_length = {tweetLength}')
    } else {
      comp <- gsub("ö","Ã¶", input$sentiment_company_xgb)
      comp <- gsub("ü", "Ã¼", comp)

      test<-glue('SELECT created_at, {input$aggregation_xgb}  FROM sum_stats_companies WHERE
      created_at >= "{dates[1]}" and created_at <= "{dates[2]}" and
         retweets_count = {input$minRetweets_xgb} and likes_count = {input$minLikes_xgb} and
         tweet_length = {tweetLength} and company  = "{comp}" and
             language = "{tolower(input$language_xgb)}"' )
    }

    test

  })


  get_sentiment_xgb <- reactive({
    ###### need correct path
    validate(need(correct_path() == T, "Please choose the correct path"))
    ###### need database connection
    validate(need(database_connector(), "Could not connect to database"))
    ###### need at least one date selected
    validate(need(!is.null(input$date_regression_xgb), "Please select a date."))

    ####### store database connection
    con <- database_connector()


    ###### querry data from sql
    df_need <- DBI::dbGetQuery(con,  querry_sentiment_model_xgb())

    #### for companies replace umlaute
    if ("company" %in% names(df_need)){
      df_need$company <- gsub("Ã¶", "ö", df_need$company)
      df_need$company <- gsub("Ã¼", "ü", df_need$company)
    }
    #### return df
    df_need
  })

#####################################################################################
  df_selected_corona_xgb <- reactive({
    #req(input$Controls_var)
    res <- corona_data_xgb()
    ###### clean input df
    res <- res %>% dplyr::select(-X,-location)
    ###### extract column based on input
    ifelse(input$corona_measurement_xgb=="",res <- res[c("date")],res <- res[c("date",input$corona_measurement_xgb)])
    #res <- res[c("date",input$corona_measurement_xgb)]

    res
  })

  corona_data_xgb <- reactive({
    ##### require database
    req( correct_path()== T)
    req(input$country_regression_xgb)
    ##### call function: extract corona file for specific country
    test <- input$country_regression_xgb
    ifelse(test=="USA",test <- "United States",test <- test)
    #CORONA_xgb(input$country_regression_xgb)
    CORONA_xgb(test)
  })


  # output$corona_vars_xgb <- renderUI({
  #   req( correct_path()== T)
  #   res <- corona_data_xgb()
  #   #### delete redundant columns
  #   res <- res %>% dplyr::select(-X,-location,-date)
  #   input <- selectizeInput("corona_xgb","Choose a corona related variable:",
  #                           names(res),multiple = TRUE)
  #
  # })

  observeEvent(input$reset_corona_xgb,{
    #### reset selection of corona dropdown
    updateSelectizeInput(session,"corona_vars_xgb",selected = "")
  })


  output$xgb_summary <- function(){
    ##### summary table
    knitr::kable(df_need_xgb(), caption = glue("Summary statistics"),colnames = NULL) %>%
      column_spec(1:6, color = "lightgrey") %>%
      column_spec(1, bold = T, color = "white") %>%
      row_spec(1, bold = T) %>%
      kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                position = "center",
                                font_size = 16)
  }


  df_need_xgb <- reactive({
    ##### calculate summary statistics
    df_need <- round(describe(final_regression_df_xgb()[-1])[c(3, 4, 5, 8, 9)], 2)
    test <- nrow(df_need)
    test2 <- nrow(df_need)==1
    if (nrow(df_need == 1)) {
      row.names(df_need)[1] <- input$regression_outcome_xgb
    } else{
      df_need <- df_need
    }
    df_need

  })

  output$correlation_xgb <- renderPlot({
    ##### ggpairs plot
    ggpairs(final_regression_df_xgb()[-1])
  })




  output$acf_plot_xgb <- renderPlot({
    req(input$correlation_xgb_plot)
    req(input$correlation_type)
    ##### call function to calculate autocorrelation plot
    #### based on input from user
    acf_plot_xgb(final_regression_df_xgb(),input$correlation_xgb_plot)
  })


  output$pacf_plot_xgb <- renderPlot({
    req(input$correlation_xgb_plot)
    req(input$correlation_type)
    ##### call function to calculate autocorrelation plot
    ##### based on input from user
    pacf_plot_xgb(final_regression_df_xgb(),input$correlation_xgb_plot)
  })


  output$correlation_plot_choice <- renderUI({
    ##### selection of variables limited to variable selection of user
    res <- final_regression_df_xgb() %>% dplyr::select(-Dates)
    input <- selectInput("correlation_xgb_plot","Select variable for plot",
                         names(res))

  })


  ##### check if the date has at leas 30 days as input
  output$xgb_date_check <- renderText({
    ##### date input
    if(length(input$date_regression_xgb) > 1){
      ##### calculate the difference of the dates
      days_inrange <- difftime(as.Date(input$date_regression_xgb[2]) ,as.Date(input$date_regression_xgb[1]) , units = c("days"))
      if (days_inrange < 30){
        ##### formulate a validation statement
        validate("Less than 30 days selected. Please choose more days.")

      }
      #### also check if no date is selected
    } else if (is.null(input$date_regression_xgb)){
      ##### formulate a validation statement
      validate("Need to select at least one day.")
    }
  })



  output$add_features <- renderUI({
    ##### selection of variables limited to variable selection of user
    res <- final_regression_df_xgb() %>% dplyr::select(-Dates)
    input <- selectInput("var_1","Chose variable to add AR and/or MA features",
                         names(res))

  })


  observeEvent(input$lag_tabs, {                         #Observe event from input (model choices)
    req(input$lag_tabs)
    ##### change sidebar structure based on tab selection
    updateTabsetPanel(session, "lag_tab", selected = input$lag_tabs)
  })
  ######################################Custom dataset############################

  ##### initiate a reactive value and initiate sub values in this reactive values
  #### will be filled based on input from user to "save" the selection
  xchange <- reactiveValues()
  xchange$df_full <- NULL
  xchange$df_full2 <- NULL
  xchange$df_full3 <- NULL
  xchange$df_full4 <- NULL
  xchange$df_full5 <- NULL

  xchange$df1 <- NULL
  xchange$df2 <- NULL

#####Disclaimer: storing this whole reactive function in separate functions did not work

##### create dataframe out of text input for moving average
Ma_part1 <- reactive({
    res <- final_regression_df_xgb()
    ##### lag feature columns by one to avoid leakage of information
    res <- lag_cols(res,input$regression_outcome_xgb)
    ##### call function to check for stationarity
    ##### returns differenced dataframe for columns which did not pass the test
    res <- make_ts_stationary(res)
    ##### extract digits from text input
    list_nums <- regmatches(input$ma_select, gregexpr("[[:digit:]]+", input$ma_select))
    ##### convert
    num_vec <- as.numeric(unlist(list_nums))
    ##### unlist to create vector of numeric values
    single_nums <- unlist(stringr::str_split(input$ma_select, ","))
    ##### create a name vector for name creation
    variables_list <- rep(input$var_1, length(num_vec))
    ##### second part of name
    helpi <- rep("MA",length(num_vec))
    ##### paste the two string vectors together
    names_vec <- paste0(helpi,variables_list, single_nums)

    ##### create a matrix to loop over to create the columns
    ##### matrix stores the input and the variable name
    help_matrix <- mapply(c,num_vec, variables_list, SIMPLIFY = T)
    ##### init
    lies <- NULL
    ##### go through matrix
    for(i in 1:length(num_vec)){
      ##### only for input greater than zero (validation is included later as safty)
      if(help_matrix[1,i] > 0){
        avg_len <- as.numeric(help_matrix[1,i])
        ##### init zoo object
        x <- zoo::zoo(res[,help_matrix[2,i]])
        ##### calculate the rolling mean based on input from matrix
        x <- as.data.frame(zoo::rollmean(x, k = avg_len, fill = NA))
        # names(x)[1] <- paste("MA_",help_matrix[2,i],sep = "")
        lies[[i]] <- x
      }else{
        res
      }


    }
    ##### create a df from list
    lies <- as.data.frame(do.call(cbind, lies))
    ##### assign custom names
    colnames(lies) <- names_vec
    lies

})


Ma_part2 <- reactive({

  res <- final_regression_df_xgb()
  ##### lag feature columns by one to avoid leakage of information
  res <- lag_cols(res,input$regression_outcome_xgb)
  ##### call function to check for stationarity
  ##### returns differenced dataframe for columns which did not pass the test
  res <- make_ts_stationary(res)
  ##### extract digits from text input
  list_nums <- regmatches(input$ma_select2, gregexpr("[[:digit:]]+", input$ma_select2))
  ##### convert
  num_vec <- as.numeric(unlist(list_nums))
  ##### create a name vector for name creation
  single_nums <- unlist(stringr::str_split(input$ma_select2, ","))
  ##### create a name vector for name creation
  variables_list <- rep(input$var_1, length(num_vec))
  ##### second part of name: EMA = exponential moving average
  helpi <- rep("EMA",length(num_vec))
  ##### paste the two string vectors together
  names_vec <- paste0(helpi,variables_list, single_nums)

  ##### create a matrix to loop over to create the columns
  ##### matrix stores the input and the variable name
  help_matrix <- mapply(c,num_vec, variables_list, SIMPLIFY = T)
  lies <- NULL
  ##### go through matrix
  for(i in 1:length(num_vec)){
    ##### only for input greater than zero (validation is included later as safty)
    if(help_matrix[1,i] > 0){
      avg_len <- as.numeric(help_matrix[1,i])
      ##### init zoo object
      x <- res[,help_matrix[2,i]]
      ##### calculate the rolling mean based on input from matrix
      x <- as.data.frame(TTR::EMA(x, avg_len))
      lies[[i]] <- x
    }else{
      res
    }


  }
  ##### create a df from list
  lies <- as.data.frame(do.call(cbind, lies))
  ##### assign custom names
  colnames(lies) <- names_vec
  lies


})

##### create a dataframe for the autoregressive part
final_regression_diff <- reactive({

  res <- final_regression_df_xgb()
  ##### lag feature columns by one to avoid leakage of information
  res <- lag_cols(res,input$regression_outcome_xgb)
  ##### call function to check for stationarity
  ##### returns differenced dataframe for columns which did not pass the test
  res <- make_ts_stationary(res)
  res
})

################# create a set of control function to avoid wrong input

##### create a message to inform user not to keep the field empty
output$error_text <- renderText({
  if(input$ma_select == "" | input$ma_select2 == ""){
    b <- "If you dont want to add a moving average - set  input to 1"
  }else{
    b <- ""
  }
})
##### only allow regular numeric expressions
v_1a <- reactive({
  validate(validate_iregulars(input$ma_select))
})
v_1b <- reactive({
  validate(validate_iregulars(input$ma_select2))
})

##### prevent any decimal numbers
v_2a <- reactive({
  validate(validate_no_decimals(input$ma_select))
})
v_2b <- reactive({
  validate(validate_no_decimals(input$ma_select2))
})
##### prevent negative values
v_3a <- reactive({
  validate(validate_negatives(input$ma_select))
})
v_3b <- reactive({
  validate(validate_negatives(input$ma_select2))
})
##### prevent input of too large numbers
v_4a <- reactive({
  validate(validate_Large_numbers(input$ma_select))
})
v_4b <- reactive({
  validate(validate_Large_numbers(input$ma_select2))
})
##### prevent input of zeros
v_5a <- reactive({
  validate(validate_no_zeros(input$ma_select))
})
v_5b <- reactive({
  validate(validate_no_zeros(input$ma_select2))
})

#################

##### init reactive value to keep track when action button is pressed
rv_action_button <- reactiveValues(i = 0)
##### set to zero if action button is pressed
observeEvent(input$addButton,{
  rv_action_button$i <- 0
})

##### isolate the increase of the reactive value from reactivity
observe({
  isolate({rv_action_button$i = rv_action_button$i + 1})
})


observe({
  ##### only observe if action button is 0
  if(rv_action_button$i == 0){
    ###### place all the validation controls before combining/calculating the datasets
    validate(
      need(input$ma_select != "","dont")) ##### control for no input
    validate(
      need(input$ma_select2 != "","dont"))
    v_1a()##### only allow regular numeric expressions
    v_1b()
    v_2a()##### prevent any decimal numbers
    v_2b()
    v_3a()##### prevent negative values
    v_3b()
    v_4a()##### prevent input of too large numbers
    v_4b()
    v_5a()##### prevent input of zeros
    v_5b()
    if((input$var_1 == "Close") | (input$var_1 == "Return")){
      ##### assign the created moving average datasets in isolation
      isolate(c <- Ma_part2())
      isolate(b <- Ma_part1())
      ##### add a one to the reactive value since action button was pressed
      rv_action_button$i <- 1
      ##### call function to create autoregressive features
      ##### input are the stationary df, variable, and number of wanted lages
      Ar_part <- AR_creator(final_regression_diff() ,input$var_1,input$num_2)
      #### combine everything to a reactive value
      #### use isolation argument to save dataframe when the user is working on a different variable
      isolate(xchange$df_full <-  cbind(final_regression_diff(),Ar_part,c,b))}

    else if(input$var_1 == "VIX"){

      rv_action_button$i <- 1
      isolate(b <- Ma_part1())
      isolate(c <- Ma_part2())
      Ar_part <- AR_creator(final_regression_diff() ,input$var_1,input$num_2)
      isolate(xchange$df_full2 <-  cbind(final_regression_diff(),Ar_part,c,b))}
    else if(input$var_1 == "coronavirus"){

      rv_action_button$i <- 1
      isolate(b <- Ma_part1())
      isolate(c <- Ma_part2())
      Ar_part <- AR_creator(final_regression_diff() ,input$var_1,input$num_2)
      isolate(xchange$df_full3 <-  cbind(final_regression_diff(),Ar_part,c,b))}
    else{

      rv_action_button$i <- 1
      isolate(b <- Ma_part1())
      isolate(c <- Ma_part2())
      Ar_part <- AR_creator(final_regression_diff() ,input$var_1,input$num_2)
      isolate(xchange$df_full4 <-  cbind(final_regression_diff(),Ar_part,c,b))
    }
  }

})


##### create logic of showing the finish button
##### only show finish button if at least one dataframe was created
output$finish_button <- renderUI({
  req(!is.null(xchange$df_full) | !is.null(xchange$df_full2) | !is.null(xchange$df_full3)
      | !is.null(xchange$df_full4) | !is.null(xchange$df_full5))
  actionButton("finish", "Finish")

})

##### create an observe statement to delete all the saved dataframes (Reset button)
  observe({
    if(input$reset_cus > 0) {
      xchange$df_full <- NULL
      xchange$df_full2 <- NULL
      xchange$df_full3 <- NULL
      xchange$df_full4 <- NULL
      xchange$df_full5 <- NULL}
  })


  ###### combine the single dataframes from each variable in on large dataframe
  ##### start combination based on action button
  custom_df <- eventReactive(input$finish, {
    ##### combine in list
    list_dfs <- c(xchange$df_full,xchange$df_full2,xchange$df_full3,xchange$df_full4)
    ##### create dataframe from list of dataframes
    df <- data.frame((sapply(list_dfs,c)))
    ###### delete duplicates based on row values: This duplicated is created when putting 1 as text input
    df <- df[!duplicated(as.list(df))]
    ##### delete duplicates detected by pattern: "."
    df <- df %>% dplyr::select(-contains("."))
    ##### create date column
    df$Dates <- as.Date(df$Dates,origin = "1970-01-01")
    ##### delete wrong date columns
    cols <- setdiff(colnames(df), "date")
    df <- df[,cols]
    ##### add corona dummy if user selects "yes"
    if(input$corona_dummy == "yes"){
      df <- corona_dummy(df)
    }else{
      df
    }
    df
  })


  output$tableCustom <- DT::renderDataTable({
    #### create datable based on final dataset created by the user
    DT::datatable(custom_df(),options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    ), rownames = F
    ) %>% DT::formatStyle(columns = c(1), width='85px')
  })




  ######################################Default dataset###########################
  df_xgb <- reactive({

    res <- final_regression_df_xgb()
    ##### call function to create AR and MA features based
    res <- ARMA_creator(res,input$regression_outcome_xgb)
  })

  ##### inform user if no further variable is selected
  observeEvent(input$lag_tabs, {
    req(input$lag_tabs)
    if((ncol(final_regression_df_xgb()) == 2)){
      showNotification("No further variables selected. Model will be trained on features from dep. variable", type = "warning")}

  })

  output$df_xgb_default <- DT::renderDataTable({
    ##### create datatable
    DT::datatable(df_xgb(),options = list(
    scrollX = T,
    autoWidth = T,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}")
  ), rownames = F
  ) %>% DT::formatStyle(columns = c(1), width='75px')
  })





  # options = list(
  #   autoWidth = FALSE, scrollX = TRUE)) %>% DT::formatStyle(names(df_xgb()),
  #                                                           lineHeight = '80%',
  #                                                           lineWidth = '80%')
########################### Validity tab #######################################

  df_xgb_train <- reactive({
    if(input$lag_tabs == "default"){ ##### deafult dataset is based on automatic AR/MA creation
      res <- final_regression_df_xgb()
      ##### lag feature columns by one to avoid leakage of information
      res <- lag_cols(res,input$regression_outcome_xgb)
      ##### call function to check for stationarity
      ##### returns differenced dataframe for columns which did not pass the test
      res <- make_ts_stationary(res)
      ##### call function to split data based on input:
      ##### df,forecast period, type for forcast dataframe, variable
      ##### returns a list with train, forecast, date,and dependent variable
      list_dfs <- split_data_for(res,input$n_ahead,input$ftpye,input$regression_outcome_xgb)
      ##### call function to create AR and MA features
      res <- ARMA_creator(res,input$regression_outcome_xgb)
      ##### remove duplicate rows
      cols <- setdiff(colnames(res),colnames(list_dfs$df_train))
      res <- res[,c("date",cols)]
      ##### join AR MA features on dataframe and back to list element
      list_dfs$df_train <- left_join(list_dfs$df_train,res)
    }else{ ##### dataframe when user created his/her own dataframe
      res <- custom_df()
      ##### create train and forecast dataset
      list_dfs <- split_data_for(res,input$n_ahead,input$ftpye,input$regression_outcome_xgb)
    }
      ##### create the selected features also for the forecast dataset
      ##### xgboost cannot predict a dataframe with a differenct column structure
    res <- ARMA_creator_for(list_dfs$df_forecast,list_dfs$df_train)

    #rename with columns from train
    list_dfs$df_forecast <- res

    list_dfs
  })




###### create reactive model
model_xgbi <- eventReactive(input$run,{

    req(input$model_spec)
    if(input$model_spec == "default"){ ##### model trained on default parameters
      res <- df_xgb_train()
      ##### call function model_xgb
      model1 <- model_xgb(res$df_train)
      model1


    }else if(input$model_spec == "custom"){ ##### model trained on custom parameters
      res <- df_xgb_train()
      ##### call function model_xgb
      model2 <- model_xgb_custom(res$df_train,input$mtry,input$trees,input$min_n,input$tree_depth,
                                 input$learn_rate,input$loss_reduction,input$sample_size)
      model2
    }else{ ##### model preforms hyperparameter tuning
      res <- df_xgb_train()
      ##### call function model_xgb
      model3 <- model_xgb_hyp(res$df_train,input$trees_hyp,input$grid_size)
      model3
    }



  })
#
#   output$model_xgb <- renderPrint({
#     model_xgbi()[[1]]
#   })

  ##### update sidebar structure based on input (validity tab)
  observeEvent(input$model_spec, {                         #Observe event from input (model choices)
    req(input$model_spec)
    updateTabsetPanel(session, "mod_spec", selected = input$model_spec)
  })

  ##### update sidebar structure based on input (actual forecast tab)
  observeEvent(input$model_spec_for, {                         #Observe event from input (model choices)
    req(input$model_spec_for)
    updateTabsetPanel(session, "mod_spec_for", selected = input$model_spec_for)
  })


  ####### create logic when action buttons are activated
  ####### Goal: prevent user from prediciting when the inputs have changed
  ######          --> force user to rerun model when parameters are changed

  ##### init random value
  rv_disable <- reactiveValues(i = 1)
  ##### set to zero if user runs model
  observeEvent(input$run,{
    rv_disable$i <- 0
  })
  ##### keep track of value
  observe({
    isolate({rv_disable$i = rv_disable$i + 1})
  })
  ##### enable prediction button after model was created
  observe({
    if(rv_disable$i == 0){
      shinyjs::enable("pred")}
    else{ ##### disable
      shinyjs::disable("pred")
      shinyjs::show("text2")

    }
  })

  ##### create a reactive value to track if the selected forecast changes
  rv_prev_input <- reactiveValues(prev_input = NULL)
  ##### observe input
  observeEvent(input$n_ahead, {
    ###### compare if previous and current selection differ. If yes disable prediction
    rv_prev_input$prev_input <- c(rv_prev_input$prev_input , input$n_ahead)
    if(rv_prev_input$prev_input[1] != input$n_ahead){
      rv_disable$i <- 1
    }
  })

  #### create the same logic for the model selection
  #### prevent that user can predict if he/she changes a different model set up
  rv_disable_2 <- reactiveValues(i = 1)

  observeEvent(input$run,{
    rv_disable_2$i <- 0
  })

  observe({
    isolate({rv_disable_2$i = rv_disable_2$i + 1})
  })

  observe({
    if(rv_disable_2$i == 0){
      shinyjs::enable("pred")}
    else{
      shinyjs::disable("pred")
      shinyjs::show("text2")

    }
  })

  rv_prev_input_2 <- reactiveValues(prev_input = NULL)
  #### observe if user changes the model
  observeEvent(input$mod_spec, {
    ##### prevent prediction if model changes by setting reactive value to 1
    rv_prev_input_2$prev_input <- c(rv_prev_input_2$prev_input , input$mod_spec)
    if((rv_prev_input_2$prev_input[1] != input$mod_spec)){
      rv_disable_2$i <- 1
    } else{
      ##### enable if pre-model calculation selection is in line again
      if(rv_prev_input$prev_input[1] == input$n_ahead){
        rv_disable_2$i <- 0}
      else{
        rv_disable_2$i <- 1
      }
    }
  })




######################### Prediction

prediction_xgb <-  eventReactive(input$pred,{

    ##### load dataframe
    res <- df_xgb_train()
    ##### rename dependent variable as y in train and forecast dataset
    colnames(res$df_forecast)[which(names(res$df_forecast) == input$regression_outcome_xgb)] <- "y"
    colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"

    ##### load model
    model <- model_xgbi()[[1]]
    ##### predict
    preds <- model %>%
      parsnip::fit(formula = y ~ .,data = res$df_train[,c(-1)]) %>%
      predict(new_data = res$df_forecast[,c(-1)])

    #### load original df
    df_orig <- final_regression_df_xgb()
    #### if dependent variable was differenced apply cumsum to restore original values
    if(adf.test(df_orig[,2],k=2)$p.value > 0.1){
      preds <- cumsum(preds) + df_orig[(nrow(res$df_train)),2]
    }
    preds
  })

##### disable run and prediction button while model trains
observeEvent(input$run, {
  shinyjs::disable("run")
  shinyjs::show("text1")
  shinyjs::disable("pred")
  shinyjs::hide("text2")

})


output$model_fit <- function(){

  if(rv_prev_input$prev_input[1] != input$n_ahead){ #### if model parameters are not in line
    return()                                        #### remove
  }else{
    ##### load dataframe
    res <- df_xgb_train()
    #### rename variable
    colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
    #### fit model
    model_xgboost <-  model_xgbi()[[1]] %>%
      parsnip::fit(formula = y ~ .,data = res$df_train[,c(-1)])
    #### get fitted values
    fits <- predict(model_xgboost,res$df_train[,c(-1)])

    #########calculate metrics
    diff <- (fits[,1]-res$df_train[,"y"])
    mean_diff <- mean(diff[,1]^2)
    rsme <- sqrt(mean_diff)
    mean_abs <- mean(abs(diff[,1]))
    diff_per <- ((res$df_train[,"y"]-fits[,1])/res$df_train[,"y"])
    diff_per <- diff_per[!is.infinite(rowSums(diff_per)),]
    mape <- mean(abs(diff_per)*100)
    #########

    ##### collect metrics in dataframe
    df_need <- data.frame(c(rsme,
                            mean_abs,
                            mape),
                          row.names = c("RMSE","MAE","MAPE"))
    colnames(df_need)<- "value"
    knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
      column_spec(1:2, color = "lightgrey") %>%
      column_spec(1, bold = T, color = "white") %>%
      row_spec(1, bold = T) %>%
      kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                position = "center",
                                font_size = 16)
  }
}


output$xgb_metrics <- function(){
  ####load prediction
  preds <- prediction_xgb()
  #### load prepared dataframe
  res <- df_xgb_train()
  #### load original values to compare prediction
  df_orig <- final_regression_df_xgb()
  #### select correct time horizon for observed values
  y <- df_orig  %>% filter(Dates >= min(res$f_dates) & Dates <= max(res$f_dates))

   #########calculate metrics
  diff <- (preds[,1]-y[,2])
  mean_diff <- mean(diff[,1]^2)
  rsme <- sqrt(mean_diff)
  mean_abs <- mean(abs(diff[,1]))
  diff_per <- ((y[,2]-preds[,1])/y[,2])
  mape <- mean(abs(diff_per[,1])*100)
  #########

  ##### collect metrics in dataframe
  df_need <- data.frame(c(rsme,
                          mean_abs,
                          mape),
                        row.names = c("RMSE","MAE","MAPE"))
  colnames(df_need)<- "value"
  knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
    column_spec(1:2, color = "lightgrey") %>%
    column_spec(1, bold = T, color = "white") %>%
    row_spec(1, bold = T) %>%
    kableExtra::kable_styling(c("striped","hover"), full_width = F,
                              position = "center",
                              font_size = 16)

}


box_test <- reactive({

  ##### load dataframe
  res <- df_xgb_train()
  ##### rename variable
  colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
  ##### fit model
  model_xgboost <- model_xgbi()[[1]] %>%
    parsnip::fit(formula = y ~ .,data = res$df_train[,c(-1)])
  ##### predict trainings dataset
  fits <- predict(model_xgboost,res$df_train[,c(-1)])
  ##### calculate residuals
  resids <- (res$df_train$y - fits)
  ##### check for autocorrelation
  m <- stats::Box.test(resids, lag = 12, type="Box-Pierce")
  m
})

output$serial_out_xgb <- function(){

  ###### enable action buttons (this is the last calcualtion when run button is pressed)
  shinyjs::enable("run")
  shinyjs::hide("text1")
  shinyjs::enable("pred")
  shinyjs::hide("text2")
  ######


  if(rv_prev_input$prev_input[1] != input$n_ahead){ #### if model parameters are not in line -> remove
    return()
  }else{

    # load test
    m <- box_test()
    ##### collect in dataframe
    df_need <- data.frame(c(round(m$statistic,5),
                            round(m$p.value,5),
                            m$method),
                          row.names = c("statistic","p.value","method"))
    colnames(df_need)<- "Summary"
    #### table output
    knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
      column_spec(1:2, color = "lightgrey") %>%
      column_spec(1, bold = T, color = "white") %>%
      row_spec(1, bold = T) %>%
      kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                position = "center",
                                font_size = 16)


  }
}

#### create a dynamic text output to summarize test
output$test_text_xgb <- renderUI({
    str1 <- paste("Box-Pierce test statistic to test for autocorrelation in the AR-residuals:")
    ####check p value
    if (box_test()$p.value > 0.1){
      str2 <- paste("The hypothesis of serially uncorrelated residuals cannot be rejected.")
    } else{
      str2 <- paste("The hypothesis of serially uncorrelated residuals can be rejected.")
    }
  ##### wrap text in html output
  htmltools::HTML(paste(str1,str2, sep = '<br/>'))
})


##### create plots with dygraphs
output$forecast_xgb <- renderDygraph({
  ##### load original dataframe
  full_df <- final_regression_df_xgb()
  ##### load prepared dataframe
  res <- df_xgb_train()
  #### load predictions
  preds <- prediction_xgb()
  #### create zoo object
  preds <- preds %>%
    zoo::zoo(seq(from = as.Date(min(res$f_dates)), to = as.Date(max(res$f_dates)), by = "day"))

  ##### select plot based on input
  if(input$forecast_plot_choice == "Full"){
    ##### create zoo object for orig values
    ts <- full_df %>% pull(input$regression_outcome_xgb) %>%
      zoo::zoo(seq(from = as.Date(min(full_df$Dates)), to = as.Date(max(full_df$Dates)), by = "day"))

    ##### create dygraph
    {cbind(actuals=ts, predicted=preds)} %>% dygraphs::dygraph() %>%
      dygraphs::dyEvent(as.Date(min(res$f_dates)), "Start of prediction", labelLoc = "bottom",color = "red") %>%  dyOptions(colors = c("white","green"))

  }else if(input$forecast_plot_choice == "Forecasted"){ #### zoomed graph
    #### create zoo object for prediction horizion
    ts <- full_df %>% pull(input$regression_outcome_xgb) %>%
      zoo::zoo(seq(from = as.Date(min(res$f_dates)), to = as.Date(max(res$f_dates)), by = "day"))
    #####create graph
    {cbind(actuals=ts, predicted=preds)} %>% dygraphs::dygraph() %>%  dygraphs::dyOptions(colors = c("white","green"))
  }
  #
  # }else{
  #   colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
  #
  #   model_xgboost <-  model_xgbi()[[1]] %>%
  #     fit(formula = y ~ .,data = res$df_train[,c(-1)])
  #   plot <- xgb.importance(model=model_xgboost$fit) %>% xgb.ggplot.importance(
  #     top_n=20, measure=NULL, rel_to_first = F)
  #   plot
  # }

})

############################ Actual forecast tab ###############################

##### this preparation only differes in the way that the complete dataset is used for
# training the model
df_xgb_train_for <- reactive({
  if(input$lag_tabs == "default"){ ##### deafult dataset is based on automatic AR/MA creation
    res <- final_regression_df_xgb()
    ##### lag feature columns by one to avoid leakage of information
    res <- lag_cols(res,input$regression_outcome_xgb)
    ##### call function to check for stationarity
    ##### returns differenced dataframe for columns which did not pass the test
    res <- make_ts_stationary(res)
    ##### call function to split data based on input:
    ##### df,forecast period, type for forecast dataframe, variable
    ##### returns a list with train, forecast dataframe
    list_dfs <- split_data_for_ahead(res,input$n_ahead2,input$ftpye2)
    ##### call function to create AR and MA features
    res <- ARMA_creator(res,input$regression_outcome_xgb)
    ##### remove duplicate rows
    cols <- setdiff(colnames(res),colnames(list_dfs$df_train))
    res <- res[,c("date",cols)]
    ##### join AR MA features on dataframe and back to list element
    list_dfs$df_train <- left_join(list_dfs$df_train,res)
  }else{##### dataframe when user created his/her own dataframe
    res <- custom_df()
    ##### create train and forecast dataset
    list_dfs <- split_data_for_ahead(res,input$n_ahead2,input$ftpye2)
  }
  ##### create the selected features also for the forecast dataset
  ##### xgboost cannot predict a dataframe with a differenct column structure
  res <- ARMA_creator_for(list_dfs$df_forecast,list_dfs$df_train)

  #rename with columns from train
  list_dfs$df_forecast <- res

  list_dfs
})


####### create logic when action buttons are activated
####### Goal: prevent user from prediciting when the inputs have changed
######          --> force user to rerun model when parameters are changed

##### init random value
rv_disable_act <- reactiveValues(i = 1)
##### set to zero if user runs model
observeEvent(input$run2,{
  rv_disable_act$i <- 0
})
##### keep track of value
observe({
  isolate({rv_disable_act$i = rv_disable_act$i + 1})
})
##### enable prediction button after model was created
observe({
  if(rv_disable_act$i == 0){
    shinyjs::enable("pred2")}
  else{ ##### disable
    shinyjs::disable("pred2")
    shinyjs::show("text2_act")

  }
})

##### create a reactive value to track if the selected forecast changes
rv_prev_input_act <- reactiveValues(prev_input = NULL)
##### observe input
observeEvent(input$n_ahead2, {
  ###### compare if previous and current selection differ. If yes disable prediction
  rv_prev_input_act$prev_input <- c(rv_prev_input_act$prev_input , input$n_ahead2)
  if(rv_prev_input_act$prev_input[1] != input$n_ahead2){
    rv_disable_act$i <- 1
  }
})

#### create the same logic for the model selection
#### prevent that user can predict if he/she changes a different model set up
rv_disable_2_act <- reactiveValues(i = 1)

observeEvent(input$run2,{
  rv_disable_2_act$i <- 0
})

observe({
  isolate({rv_disable_2_act$i = rv_disable_2_act$i + 1})
})

observe({
  if(rv_disable_2_act$i == 0){
    shinyjs::enable("pred2")}
  else{
    shinyjs::disable("pred2")
    shinyjs::show("text2_act")

  }
})

rv_prev_input_2_act <- reactiveValues(prev_input = NULL)
#### observe if user changes the model
observeEvent(input$mod_spec_for, {
  ##### prevent prediction if model changes by setting reactive value to 1
  rv_prev_input_2_act$prev_input <- c(rv_prev_input_2_act$prev_input , input$mod_spec_for)
  if((rv_prev_input_2_act$prev_input[1] != input$mod_spec_for)){
    rv_disable_2_act$i <- 1
  } else{
    ##### enable if pre-model calculation selection is in line again
    if(rv_prev_input_act$prev_input[1] == input$n_ahead2){
      rv_disable_2_act$i <- 0}
    else{
      rv_disable_2_act$i <- 1
    }
  }
})


###### create reactive model
  model_xgbi2 <- eventReactive(input$run2,{
    req(input$model_spec_for)
    if(input$model_spec_for == "default"){ ##### model trained on default parameters
      res <- df_xgb_train_for()
      ##### call function model_xgb
      model1 <- model_xgb(res$df_train)
      model1
    }else if(input$model_spec_for == "custom"){ ##### model trained on custom parameters
      res <- df_xgb_train_for()
      ##### call function model_xgb
      model2 <- model_xgb_custom(res$df_train,input$mtry1,input$trees1,input$min_n1,input$tree_depth1,
                                 input$learn_rate1,input$loss_reduction1,input$sample_size1)
      model2
    }else{ ##### model preforms hyperparameter tuning
      res <- df_xgb_train_for()
      ##### call function model_xgb
      model3 <- model_xgb_hyp(res$df_train,input$trees_hyp1,input$grid_size1)
      model3
    }
  })

 #################### Prediction

  prediction_xgb_actual <-  eventReactive(input$pred2,{
    ##### load dataframe
    res <- df_xgb_train_for()
    ##### rename dependent variable as y in train and forecast dataset
    colnames(res$df_forecast)[which(names(res$df_forecast) == input$regression_outcome_xgb)] <- "y"
    colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
    ##### predict
    preds <- model_xgbi2()[[1]]  %>%
      parsnip::fit(formula = y ~ .,data = res$df_train[,c(-1)]) %>%
      stats::predict(new_data = res$df_forecast[,c(-1)])

    #### load original df
    df_orig <- final_regression_df_xgb()
    #### if dependent variable was differenced apply cumsum to restore original values
    if(adf.test(df_orig[,2],k=2)$p.value > 0.1){
      preds <- cumsum(preds) + df_orig[(nrow(res$df_train)),2]
    }
    preds
  })


  ##### disable run and prediction button while model trains
  observeEvent(input$run2, {
    shinyjs::disable("run2")
    shinyjs::show("text1_act")
    shinyjs::disable("pred_act")
    shinyjs::hide("text2_act")

  })


  output$model_fit_act <- function(){

    if(rv_prev_input_act$prev_input[1] != input$n_ahead2){ #### if model parameters are not in line
      return()                                        #### remove
    }else{
      ##### load dataframe
      res <- df_xgb_train_for()
      #### rename variable
      colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
      #### fit model
      model_xgboost <-  model_xgbi2()[[1]] %>%
        parsnip::fit(formula = y ~ .,data = res$df_train[,c(-1)])
      #### get fitted values
      fits <- predict(model_xgboost,res$df_train[,c(-1)])

      #########calculate metrics
      diff <- (fits[,1]-res$df_train[,"y"])
      mean_diff <- mean(diff[,1]^2)
      rsme <- sqrt(mean_diff)
      mean_abs <- mean(abs(diff[,1]))
      diff_per <- ((res$df_train[,"y"]-fits[,1])/res$df_train[,"y"])
      diff_per <- diff_per[!is.infinite(rowSums(diff_per)),]
      mape <- mean(abs(diff_per)*100)
      #########

      ##### collect metrics in dataframe
      df_need <- data.frame(c(rsme,
                              mean_abs,
                              mape),
                            row.names = c("RMSE","MAE","MAPE"))
      colnames(df_need)<- "value"
      knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
        column_spec(1:2, color = "lightgrey") %>%
        column_spec(1, bold = T, color = "white") %>%
        row_spec(1, bold = T) %>%
        kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                  position = "center",
                                  font_size = 16)
    }
  }


  box_test_act <- reactive({

    ##### load dataframe
    res <- df_xgb_train_for()
    ##### rename variable
    colnames(res$df_train)[which(names(res$df_train) == input$regression_outcome_xgb)] <- "y"
    ##### fit model
    model_xgboost <- model_xgbi2()[[1]] %>%
      parsnip::fit(formula = y ~ .,data = res$df_train[,c(-1)])
    ##### predict trainings dataset
    fits <- predict(model_xgboost,res$df_train[,c(-1)])
    ##### calculate residuals
    resids <- (res$df_train$y - fits)
    ##### check for autocorrelation
    m <- stats::Box.test(resids, lag = 12, type="Box-Pierce")
    m
  })


  output$serial_out_xgb <- function(){

    ###### enable action buttons (this is the last calcualtion when run button is pressed)
    shinyjs::enable("run2")
    shinyjs::hide("text1_act")
    shinyjs::enable("pred2")
    shinyjs::hide("text2_act")
    ######


    if(rv_prev_input_act$prev_input[1] != input$n_ahead2){ #### if model parameters are not in line -> remove
      return()
    }else{

      # load test
      m <- box_test_act()
      ##### collect in dataframe
      df_need <- data.frame(c(round(m$statistic,5),
                              round(m$p.value,5),
                              m$method),
                            row.names = c("statistic","p.value","method"))
      colnames(df_need)<- "Summary"
      #### table output
      knitr::kable(df_need, caption = glue("Performance metrics"),colnames = NULL) %>%
        column_spec(1:2, color = "lightgrey") %>%
        column_spec(1, bold = T, color = "white") %>%
        row_spec(1, bold = T) %>%
        kableExtra::kable_styling(c("striped","hover"), full_width = F,
                                  position = "center",
                                  font_size = 16)


    }
  }


  #### create a dynamic text output to summarize test
  output$test_text_xgb_act <- renderUI({
    str1 <- paste("Box-Pierce test statistic to test for autocorrelation in the AR-residuals:")
    ####check p value
    if (box_test_act()$p.value > 0.1){
      str2 <- paste("The hypothesis of serially uncorrelated residuals cannot be rejected.")
    } else{
      str2 <- paste("The hypothesis of serially uncorrelated residuals can be rejected.")
    }
    ##### wrap text in html output
    htmltools::HTML(paste(str1,str2, sep = '<br/>'))
  })


  output$plot_1_xgb_actual <- renderDygraph({
    ##### load original dataframe
    full_df <- final_regression_df_xgb()
    ##### load prepared dataframe
    res <- df_xgb_train_for()
    #### load predictions
    preds <- prediction_xgb_actual()
    #### create zoo object
    preds <- preds %>%
      zoo::zoo(seq(from = as.Date(max(full_df$Dates)) +1,
                   to = as.Date(max(full_df$Dates)) + input$n_ahead2, by = "day"))

    ts <- full_df %>% pull(Close) %>%
      zoo::zoo(seq(from = as.Date(min(full_df$Dates)), to = as.Date(max(full_df$Dates)), by = "day"))

    {cbind(actuals=ts, predicted=preds)} %>% dygraphs::dygraph() %>%
      dyEvent(as.Date(max(full_df$Dates)), "Start forecast", labelLoc = "bottom",color = "red") %>%  dyOptions(colors = c("white","green"))
    #%>% dyCSS("C:/Users/simon/Desktop/WS_20_21/Git_tracked_Sentiment_App/DSP_Sentiment_Covid_App/test_simon/SimonApp/css/dygraph.css")


  })



}
