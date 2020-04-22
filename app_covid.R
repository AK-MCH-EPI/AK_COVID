options(stringsAsFactors = FALSE)
options(scipen=999) # remove scientific notation

#load libraries
library(httr)
library(jsonlite)
library(rjson)
library(jsonlite)
library(RCurl)
library(dplyr)
library(ggplot2)
library(deSolve)
library(lubridate)
library(incidence)
library(tidyr)
library(plotly)



####app####
ui<- function(req) {
  fluidPage(
  navbarPage(
    " ",
    
    tabPanel("Projected Epidemic Curve",
             sidebarPanel(
               helpText("IMPORTANT:",
                        br(),
                        "The decreasing trend is likely a result of adherence to the many health",
                        "mandates. As mandates are lifted we will tract this closely to monitor any changes",
                        "in COVID-19 cases.",
                        br(),
                        br(),
                        "NOTE:",
                        br(),    
                        "Please review the methods tab regularly to learn about any changes or updates to the model",
                        "and/or measurements."), width=3),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plots", 
                          br(),
                          plotlyOutput("Plot3"),  
                          br(),
                          h4("Projection Statistics"),
                          tableOutput("table3")
                          ),
                 tabPanel("Interpretation",
                          helpText("This graph represents the daily COVID case count in Alaska (yellow).",
                                   "Date of symptom onset was used. Grey bars represent data from the most recent",
                                   "7 days, which was not included in the analysis due to incomplete data/delay in reporting.",
                                   "The blue dotted line represents the predicted daily case trajectory (assumed exponential),",
                                   "with the grey band representing the 95% Confidence Interval (estimate range) of the projection.",
                                   "For a complete description of the methods please view the Methods tab",
                                   br(),
                                   br(),
                                   "Projection statistics:",
                                   br(),
                                   "The modeled exponential trajectory can be one of growth (getting bigger) or decay (getting smaller).",
                                   "If in growth, the doubling time represents the projected amount of time it will take for the counts to",
                                   "double in size. This assumes a constant growth rate (r) from the observed data under an exponential trajectory.",
                                   "If in decline, the halving time represents the proejcted amount of time it will take for the counts to reduce",
                                   "in half. This also assumes a constant halving rate (r) from the observed data under an exponential trajectory.",
                                   "The 95% confidence intervals (CI) for these estimates are also provided.",
                                   br(),
                                   br(),
                                   "Confidence intervals (CI) provide an indication of precision of an estimated parameter. Specifically, a 95% CI",
                                   "means that if we sampled the same population an infinite number of times, the generated CI's for each sample",
                                   "would contain the true population parameter in 95% of the cases, assuming no systematic (bias) error.", 
                                   "Generally speaking however, we can interpret the CI as a range around a point estimate",
                                   "within which the true value is likely to lie with a specified degree of probability, assuming there is no",
                                   "systematic error (bias or confounding).", 
                                   "If the sample size is small and subject to more random error, then the estimate will not be as precise, and the",
                                   "confidence interval would be wide, indicating a greater amount of random error. In contrast, with a large",
                                   "sample size, the width of the confidence interval is narrower, indicating less random error and greater",
                                   "precision. One can, therefore, use the width of confidence intervals to indicate the amount of random error",
                                   "in an estimate.")
                          )))),
    
    tabPanel("Projected Cumulative Cases",
             sidebarPanel(
               helpText("IMPORTANT:",
                        br(),
                        "The flattening of our trajectory is likely a result of adherence to the many health",
                        "mandates. As mandates are lifted we will tract this closely to monitor any changes",
                        "in COVID-19 cases.",
                        br(),
                        br(),
                        "NOTE:",
                        br(),    
                        "Please review the methods tab regularly to learn about any changes or updates to the model",
                        "and/or measurements."), width=3),
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Plots", width = "100%",
                          br(),
                          plotlyOutput("Plot"),  
                          br(),
                          h4("Projection Statistics"),
                          tableOutput("table")
                          ),
                 tabPanel("Interpretation",
                          helpText("This graph represents the cumulative COVID case counts in Alaska (yellow).",
                                   "Date of symptom onset was used. Grey dots represent data from the most recent",
                                   "7 days, which was not included in the analysis due to incomplete data/delay in reporting.",
                                   "The blue dotted line represents the predicted cumulative daily case trajectory (assumed exponential),",
                                   "with the grey band representing the 95% Confidence Interval (estimate range) of the projection.",
                                   "For a complete description of the methods please view the Methods tab",
                                   br(),
                                   br(),
                                   "Projection statistics:",
                                   br(),
                                   "The modeled exponential trajectory can be one of growth (getting bigger) or decay (getting smaller).",
                                   "If in growth, the doubling time represents the projected amount of time it will take for the counts to",
                                   "double in size. This assumes a constant growth rate (r) from the observed data under an exponential trajectory.",
                                   "If in decline, the halving time represents the proejcted amount of time it will take for the counts to reduce",
                                   "in half. This also assumes a constant halving rate (r) from the observed data under an exponential trajectory.",
                                   "The 95% confidence intervals (CI) for these estimates are also provided.",
                                   br(),
                                   br(),
                                   "Confidence intervals (CI) provide an indication of precision of an estimated parameter. Specifically, a 95% CI",
                                   "means that if we sampled the same population an infinite number of times, the generated CI's for each sample",
                                   "would contain the true population parameter in 95% of the cases, assuming no systematic (bias) error.", 
                                   "Generally speaking however, we can interpret the CI as a range around a point estimate",
                                   "within which the true value is likely to lie with a specified degree of probability, assuming there is no",
                                   "systematic error (bias or confounding).", 
                                   "If the sample size is small and subject to more random error, then the estimate will not be as precise, and the",
                                   "confidence interval would be wide, indicating a greater amount of random error. In contrast, with a large",
                                   "sample size, the width of the confidence interval is narrower, indicating less random error and greater",
                                   "precision. One can, therefore, use the width of confidence intervals to indicate the amount of random error",
                                   "in an estimate.")
                          )))),
    
    # tabPanel("Key Interventions",
    #          sidebarPanel(
    #            helpText("NOTE: The flattening of our trajectory is likely a result of adherence to the many health",
    #                     "mandates. As mandates are lifted we will tract this closely to monitor any changes",
    #                     "in COVID-19 cases.",
    #                     br(),
    #                     br(),
    #                     "03-17",
    #                     br(),
    #                     "Mandate 4: Quarantine for Travelers",
    #                     br(),
    #                     "03-18",
    #                     br(),
    #                     "Mandate 3: Close Restaurants/Bars/Entertainment",
    #                     br(),
    #                     "03-19 ",
    #                     br(),
    #                     "Mandate 5: Cancel elective medical procedures",
    #                     br(),
    #                     "03-20",
    #                     br(),
    #                     "Mandate 8: School Closure",
    #                     br(),
    #                     "03-25 ",
    #                     br(),
    #                     "Mandate 10: Quarantine for all out of state travelers",
    #                     br(),
    #                     "03-28",
    #                     br(),
    #                     " Mandate 11: Social distancing",
    #                     br(),
    #                     "Mandate 12: Intrastate Travel"
    #            ), width=3),
    #          
    #          mainPanel(
    #            tabsetPanel(
    #              tabPanel("Plots",
    #                       br(),
    #                       plotOutput("Plot1")),
    #              tabPanel("Interpretation",
    #                       helpText("This graph represents the daily COVID case count in Alaska (yellow).",
    #                                "Date of symptom onset was used. Grey bars represent data from the most recent",
    #                                "7 days, which was not included in the analysis due to incomplete data/delay in reporting.",
    #                                "Key mandates are listed as blue dotted lines."
    #                       )
    #              )))),
    
    tabPanel("Methods",
         
             includeMarkdown("methods.Rmd")
             )
    
))
}
  
  
server <- shinyServer(function(input, output) {
  options(stringsAsFactors = FALSE)
  options(scipen=999) # remove scientific notation
  
  #load libraries
  library(httr)
  library(jsonlite)
  library(rjson)
  library(jsonlite)
  library(RCurl)
  library(dplyr)
  library(ggplot2)
  library(deSolve)
  library(lubridate)
  library(incidence)
  library(tidyr)
  library(plotly)
  
  ## connect to Alaksa data hub through the API
  url <- "https://opendata.arcgis.com/datasets/375f5ee129834fd9833bd92af54cd8bc_0.geojson"
  
  da <- jsonlite::fromJSON(url)
  base::class(da)
  da %>% str()
  
  # set data imported data as an object
  dat1 <- tibble::as_tibble(da$features$properties) 
  
  # Fix dates
  dat1$ReportDate <- as.Date(substr(dat1$ReportDate,1,10))
  dat1$OnsetDate <- as.Date(substr(dat1$OnsetDate,1,10))
  dat1$HospDate <- as.Date(substr(dat1$HospDate,1,10))
  dat1$RecoverDate <- as.Date(substr(dat1$RecoverDate,1,10))
  dat1$DeceasedDate <- as.Date(substr(dat1$DeceasedDate,1,10))
  
  
  dat1$HospDate[dat1$HospDate == "1900-01-01"] <- NA
  dat1$RecoverDate[dat1$RecoverDate == "1900-01-01"] <- NA
  dat1$DeceasedDate[dat1$DeceasedDate == "1900-01-01"] <- NA
  
  
  output$Plot <- renderPlotly({
    io_1 <- incidence(dat1$OnsetDate)
    io_1cum <- cumulate(io_1) # cumulative incidence
    
    #subset and create incidence object on the 10 days before truncation.
    
    io_2cum <- subset(io_1cum, from = max(io_1$dates-20), to = max(io_1$dates-7))
    
    # for plotting 7 days truncated.
    tnk <- as.data.frame(subset(io_1cum, from = max(io_1cum$dates-6), to = max(io_1cum$dates)))
    
    #Fit log linear model on the 10 day subset
    i_fit <- incidence::fit(io_2cum)
    get_info(i_fit, "pred")
    #set forecasting range for data
    
    dtr <- data.frame(date = as.Date(min(io_1cum$dates):(max(io_1cum$dates)+10), origin = "1970-01-01"))
    
    dts <- c((i_fit$model$model$dates.x), ((max(i_fit$model$model$dates.x)+1): 31)) # create range for prediction
    tIn <- data.frame(dates.x = as.numeric(dts)) #turn into data frame
    
    #prediction from fitted model
    extFit <- predict(i_fit$model, tIn,interval = "confidence", level = 0.95)
    
    y <- exp(extFit) # put on natural scale
    
    drg <- c(((max(dtr$date))-max(tIn$dates.x)):(max(dtr$date))) # set dates for predicted range
    
    #get_info(i_fit, "pred")
    
    #create dataframe of predictions
    projs <- data.frame(y, date = as.Date(drg, origin = "1970-01-01"))
    
    #`Short term projection with 95% confidence bands` <- "red"
    
    #plot epi curve and prediction window
    io_1cum_df <- as.data.frame(subset(io_1cum, 
                                       from = min(io_1$dates), 
                                       to = max(io_1$dates-7)))
    t1 <- ggplot() +
      geom_line(data=io_1cum_df, aes(x = dates, y = counts),size = 1, color = "#ffb923") +
      geom_point(data=io_1cum_df, aes(x = dates, y = counts),size = 3, color = "#ffb923") +
      
      geom_point(data = tnk, aes(x = dates, y = counts), color = "#999999", size = 2) +
      
      geom_ribbon(data = projs, aes(x = date, ymin = lwr, ymax = upr), 
                  alpha = 0.15, fill = "#0a306a")+
      
      scale_x_date(breaks = pretty(dtr$date, n = 10), date_labels = "%d %b",
                   limits = c(min(dtr$date), max(dtr$date))) +
      coord_cartesian(ylim = c(0, max(io_1cum_df$counts)+300)) +
      geom_line(aes(date, fit), 
                projs,linetype="dashed", size = 1, color = "#0a306a") +
      theme_minimal() +
      labs(title = "Cumulative incidence curve by onset date, Alaska",
           subtitle = "(log-linear model: short term forecast with 95% confidence band)",
           caption = "*Indicated by gray dots: Illnesses that began during this period may not yet be reported resulting in incomplete data and excluded from model fit.") +
      theme(legend.justification = c(0, 1), 
            legend.position = c(0, 0.95),
            axis.title.x=element_blank())

    
    ### plotly version
  
    ggplotly(t1) %>%
      layout(title = list(text = paste0('Cumulative incidence curve by onset date, Alaska',
                                        '<br>',
                                        '<sup>',
                                        '(log-linear model: short term forecast with 95% confidence band)',
                                        '<br>',
                                        '<sup>',
                                        '*Note: Analyses truncated by 7 days (gray bars) due to delay in reporting, resulting in incomplete data',
                                        '</sup>')))
  
  })
  
  output$Plot1 <- renderPlot({
    io_1 <- incidence(dat1$OnsetDate)
    
    #subset and create incidence object on the 10 days before truncation.
    
    io_2 <- subset(io_1, from = max(io_1$dates-20), to = max(io_1$dates-7))
    
    # for plotting end of non measured
    tnk <- as.data.frame(subset(io_1, from = max(io_1$dates-6), to = max(io_1$dates)))
    
    #Fit log linear model on the 10 day subset
    i_fit <- incidence::fit(io_2)
    get_info(i_fit, "pred")
    #set forecasting range for data
    
    dtr <- data.frame(date = as.Date(min(io_1$dates):(max(io_1$dates)+10), origin = "1970-01-01"))
    
    #dts <- c((i_fit$model$model$dates.x), ((max(i_fit$model$model$dates.x)+1): 31)) # create range for prediction
    dts <- c(min(i_fit$model$model$dates.x): 31)
    tIn <- data.frame(dates.x = as.numeric(dts)) #turn into data frame
    
    #prediction from fitted model
    extFit <- predict(i_fit$model, tIn,interval = "confidence", level = 0.95)
    
    y <- exp(extFit) # put on natural scale
    
    drg <- c(((max(dtr$date))-max(tIn$dates.x)):(max(dtr$date))) # set dates for predicted range
    
    #get_info(i_fit, "pred")
    
    #create dataframe of predictions
    projs <- data.frame(y, date = as.Date(drg, origin = "1970-01-01"))
    
   
    io_1dta <- as.data.frame(io_1)

    ggplot() +
      geom_bar(data = io_1dta, aes(x = dates, y = counts), stat = "identity",
               fill = "#ffb923", color = "white", width = 1) +
      geom_bar(data = tnk, aes(x = dates, y = counts), stat = "identity", 
               fill = "#999999", color = "white", width = 1) +
      #geom_line(aes(date, fit), projs,linetype="dashed", size = 1, color = "#0a306a") +
      #geom_ribbon(data = projs, aes(x = date, ymin = lwr, ymax = upr), 
      #            alpha = 0.15,fill = "#0a306a") +
      geom_vline(xintercept=as.Date("2020-03-17"), linetype="dashed", size=1, color= "#0a306a")+ #level 3 travel, close restaurants/bars/entertainment
      geom_vline(xintercept=as.Date("2020-03-18"), linetype="dashed", size=1, color= "#0a306a")+
      geom_vline(xintercept=as.Date("2020-03-19"), linetype="dashed", size=1, color= "#0a306a")+ #elective med
      geom_vline(xintercept=as.Date("2020-03-20"), linetype="dashed", size=1, color= "#0a306a")+ #schools
      geom_vline(xintercept=as.Date("2020-03-25"), linetype="dashed", size=1, color= "#0a306a")+ #all outer travel
      geom_vline(xintercept=as.Date("2020-03-28"), linetype="dashed", size=1, color= "#0a306a")+ #distancing, intrastate
      xlim(min(dtr$date),max(dtr$date)) +
      scale_y_continuous(limits = c(0,20)) +
      scale_x_date(breaks = pretty(dtr$date, n = 10),date_labels = "%d %b") +
      theme_minimal() +
      #theme(plot.caption = element_text(hjust = 0)) +
      labs(title="Epidemic curve by onset date, Alaska",
           subtitle = "Key Interventions",
           caption = "*Indicated by gray bars: Illnesses that began during this period may not yet be reported resulting in incomplete data and excluded from model fit.") +
      theme(legend.justification = c(0, 1), 
            legend.position = c(0, 0.95),
            axis.title.x=element_blank(),
            plot.title = element_text(size=20))
    
  })
  
  output$Plot3 <- renderPlotly({
    io_1 <- incidence(dat1$OnsetDate)
    
    #subset and create incidence object on the 10 days before truncation.
    
    io_2 <- subset(io_1, from = max(io_1$dates-20), to = max(io_1$dates-7))
    
    # for plotting end of non measured
    tnk <- as.data.frame(subset(io_1, from = max(io_1$dates-6), to = max(io_1$dates)))
    
    #Fit log linear model on the 10 day subset
    i_fit <- incidence::fit(io_2)
    get_info(i_fit, "pred")
    #set forecasting range for data
    
    dtr <- data.frame(date = as.Date(min(io_1$dates):(max(io_1$dates)+10), origin = "1970-01-01"))
    
    #dts <- c((i_fit$model$model$dates.x), ((max(i_fit$model$model$dates.x)+1): 31)) # create range for prediction
    dts <- c(min(i_fit$model$model$dates.x): 31)
    tIn <- data.frame(dates.x = as.numeric(dts)) #turn into data frame
    
    #prediction from fitted model
    extFit <- predict(i_fit$model, tIn,interval = "confidence", level = 0.95)
    
    y <- exp(extFit) # put on natural scale
    
    drg <- c(((max(dtr$date))-max(tIn$dates.x)):(max(dtr$date))) # set dates for predicted range
    
    #get_info(i_fit, "pred")
    
    #create dataframe of predictions
    projs <- data.frame(y, date = as.Date(drg, origin = "1970-01-01"))
    

    ### plotly version ###
    
    io_1dta <- as.data.frame(io_1)
    
    library(plotly)
    
    p1<-ggplot() +
      geom_bar(data = io_1dta, aes(x = dates, y = counts), stat = "identity",
               fill = "#ffb923", color = "white", width = 1) +
      geom_bar(data = tnk, aes(x = dates, y = counts), stat = "identity", 
               fill = "#999999", color = "white", width = 1) +
      geom_line(aes(date, fit), projs,linetype="dashed", size = 1, color = "#0a306a") +
      geom_ribbon(data = projs, aes(x = date, ymin = lwr, ymax = upr), 
                  alpha = 0.15,fill = "#0a306a") +
      xlim(min(dtr$date),max(dtr$date)) +
      coord_cartesian(ylim = c(0, max(io_1$counts)+10)) +
      scale_x_date(breaks = pretty(dtr$date, n = 10),date_labels = "%d %b") +
      theme_minimal() +
      #theme(plot.caption = element_text(hjust = 0)) +
      labs(title="Epidemic curve by onset date, Alaska",
           subtitle = "(log-linear model: short term forecast with 95% confidence band)",
           caption = "*Indicated by gray bars: Illnesses that began during this period may not yet be reported resulting in incomplete data and excluded from model fit.") +
      theme(legend.justification = c(0, 1), 
            legend.position = c(0, 0.95),
            axis.title.x=element_blank()) 
    
    ggplotly(p1) %>%
      layout (title = list(text = paste0('Epidemic curve by onset date, Alaska',
                                         '<br>',
                                         '<sup>',
                                         '(log-linear model: short term forecast with 95% confidence band)',
                                         '<br>',
                                         '<sup>',
                                         '*Note: Analyses truncated by 7 days (gray bars) due to delay in reporting, resulting in incomplete data',
                                         '</sup>')))
    # annotations = list(x = 18324, y = 19, text = "Truncated", showarrow = F,
    #                    size = 0.5))
    
    
  })

  output$table <- renderTable({

    io_1 <- incidence(dat1$OnsetDate)
    io_1cum <- cumulate(io_1) # cumulative incidence
    
    #subset and create incidence object on the 10 days before truncation.
    
    io_2cum <- subset(io_1cum, from = max(io_1$dates-20), to = max(io_1$dates-7))
    
    # for plotting 7 days truncated.
    tnk <- as.data.frame(subset(io_1cum, from = max(io_1cum$dates-6), to = max(io_1cum$dates)))
    
    #Fit log linear model on the 10 day subset
    i_fit <- incidence::fit(io_2cum)
    #get_info(i_fit, "pred")
    
    tab <- as.data.frame(c(i_fit$info[4], i_fit$info[5]))
    colnames(tab)[1] <- "Doubling Time"
    colnames(tab)[2] <- "Doubling Time Lower CI"
    colnames(tab)[3] <- "Doubling Time Upper CI"
    tab
  })
  
  

  output$table3 <- renderTable({
  io_1 <- incidence(dat1$OnsetDate)
  
  #subset and create incidence object on the 10 days before truncation.
  
  io_2 <- subset(io_1, from = max(io_1$dates-20), to = max(io_1$dates-7))
  
  # for plotting end of non measured
  tnk <- as.data.frame(subset(io_1, from = max(io_1$dates-6), to = max(io_1$dates)))
  
  #Fit log linear model on the 10 day subset
  i_fit <- incidence::fit(io_2)
  #get_info(i_fit, "pred")
  
  tab <- as.data.frame(c(i_fit$info[4], i_fit$info[5]))
  colnames(tab)[1] <- "Halving Time"
  colnames(tab)[2] <- "Halving Time Lower CI"
  colnames(tab)[3] <- "Halving Time Upper CI"
  tab
  
  
})
  

})


# Run the application 
shinyApp(ui = ui, server = server)