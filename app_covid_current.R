options(stringsAsFactors = FALSE)
options(scipen=999) # remove scientific notation

#load libraries
library(httr)
library(jsonlite)
library(rjson)
library(RCurl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(incidence)
library(tidyr)
library(plotly)
library(EpiEstim)

## Create password log-in scrip ##
library(shiny)
library(shinymanager)

####app####
ui<- function(req) {
  fluidPage(
    navbarPage(
      " ",
      
      tabPanel("Projected Epidemic Curve",
               sidebarPanel(
                 helpText("IMPORTANT:",
                          br(),
                          "The decreasing trend is likely the result of Alaskan's hard work at personal",
                          "environmental and community mitigation efforts. As mandates are lifted, we will", 
                          "monitor this closely, and along with other metrics help inform our public heath response.",
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
                            helpText("This graph represents the daily COVID-19 case count in Alaska (yellow).",
                                     "Date of symptom onset was used. Gray bars represent data from the most recent",
                                     "7 days, which was not included in the analysis due to incomplete data/delay in reporting.",
                                     "The blue dotted line represents the predicted daily case trajectory (assumed exponential),",
                                     "with the gray band representing the 95% Confidence Interval (estimate range) of the projection.",
                                     "For a complete description of the methods please view the Methods tab.",
                                     br(),
                                     br(),
                                     "Projection statistics:",
                                     br(),
                                     "The modeled exponential trajectory can be one of growth (getting bigger) or decay (getting smaller).",
                                     "If in growth, the doubling time represents the projected amount of time it will take for the counts to",
                                     "double in size. This assumes a constant growth rate (r) from the observed data under an exponential trajectory.",
                                     "If in decline, the halving time represents the projected amount of time it will take for the counts to reduce",
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
                                     "in an estimate.")),
                   tabPanel("Methods",
                            includeMarkdown("methods.Rmd")
                   )
                 ))),
      
      tabPanel("Projected Cumulative Cases",
               sidebarPanel(
                 helpText("IMPORTANT:",
                          br(),
                          "The flattening of our trajectory is likely the result of Alaskan's hard work at personal",
                          "environmental and community mitigation efforts. As mandates are lifted, we will", 
                          "monitor this closely, and along with other metrics help inform our public heath response.",
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
                            helpText("This graph represents the cumulative COVID-19 case counts in Alaska (yellow).",
                                     "Date of symptom onset was used. Gray dots represent data from the most recent",
                                     "7 days, which was not included in the analysis due to incomplete data/delay in reporting.",
                                     "The blue dotted line represents the predicted cumulative daily case trajectory (assumed exponential),",
                                     "with the gray band representing the 95% Confidence Interval (estimate range) of the projection.",
                                     "For a complete description of the methods please view the Methods tab.",
                                     br(),
                                     br(),
                                     "Projection statistics:",
                                     br(),
                                     "The modeled exponential trajectory can be one of growth (getting bigger) or decay (getting smaller).",
                                     "If in growth, the doubling time represents the projected amount of time it will take for the counts to",
                                     "double in size. This assumes a constant growth rate (r) from the observed data under an exponential trajectory.",
                                     "If in decline, the halving time represents the projected amount of time it will take for the counts to reduce",
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
                                     "in an estimate.")),    
                   tabPanel("Methods",
                            includeMarkdown("methods.Rmd")
                   )
                 ))),
      tabPanel("Key Interventions",
               sidebarPanel(
                 checkboxGroupInput("lines", "Restrictions (Red line):",
                                    choices=c(
                                      "Mandate 1" = "mandate1", 
                                      "Mandate 3" = "mandate3", 
                                      "Mandate 4" = "mandate4",
                                      "Mandate 5" = "mandate5", 
                                      "Mandate 10" = "mandate10",
                                      "Mandates 11 & 12" = "mandate11_12"),
                                    selected = "mandate1"),
                 checkboxGroupInput("lifts", "Restrictions lifted (Green line):",
                                    choices=c(
                                      "Mandate 15" = "mandate15",
                                      "Mandate 16" = "mandate16",
                                      "Mandate 16 - II" = "mandate16_2",
                                      "Mandate 18" = "mandate18"),
                                    selected=("mandate15")),
                 
                 helpText("NOTE: This graphic depicts when the various health mandates were",
                          "implemented or relaxed (i.e. lifted) in relation to our observed", 
                          "epidemic curve. This is not an exhaustive list, but rather highlights",
                          "health mandates that have broad impacts.",
                          br(),
                          br(),
                          "The epidemic curve is based on date of onset of",
                          "symptoms. When the onset date is not yet known, the report date or",
                          "hospitalization date (whichever is first) is used as a temporary",
                          "substitution."), width=3),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Plots",
                            br(),
                            plotOutput("Plot1"),
                            br(),
                            tableOutput("table1"),
                            uiOutput("link")),
                   tabPanel("Details",
                            helpText("This graph represents the daily new COVID-19 cases in Alaska by type of disease acquisition.",
                                     "Under Investigation (gray) means the case type has not yet been determined.",
                                     "The epidemic curve is based on date of onset of symptoms. When the onset date",
                                     "is not yet known, the report date or hospitalization date (whichever is first)",
                                     "is used as a temporary substitution.",
                                     br(),
                                     br(),
                                     "Key mandates for restrictions are listed as red dotted lines, and mandates for",
                                     "lifting restrictions are green dotted lines."
                            )
                   )))),
      
      tabPanel("Reproductive number",
               sidebarPanel(
                 selectInput("inSelect", 
                             label = "Select input",
                             choices = c("Total Cases", "Local Cases"),
                             selected = "Total Cases"),
                 
                 radioButtons("MSI", "Mean Serial Interval:",
                              c("Parametric" = "para",
                                "MCMC estimation" = "mcmc")),
                 
                 helpText("NOTE:",
                          br(),
                          "Selecting the MCMC method for paramatizing the mean serial interval",
                          "will take a few seconds to run due to the simulations being", 
                          "executed, please be patient.",
                          br(),
                          "The mean serial interval is the average duration (in days) between the onset",
                          "of disease in an index case, and the onset of disease in a contact.",
                          br(),
                          br(),
                          "IMPORTANT:",
                          br(),
                          "The method used for estimating the effective reproductive number",
                          "or Rt is based on those suggested by the imperial College, for", 
                          "more information please see the Methods tab.",
                          br(),
                          br(),
                          "Cori, A. et al. A new framework and software to estimate",
                          "time-varying reproduction numbers during epidemics (AJE 2013)"
                          
                 ), width=3),
               mainPanel(
                 tabsetPanel(
                   tabPanel("7 day sliding window", 
                            br(),
                            plotlyOutput("Plot_r"),
                            br(),
                            uiOutput("link1"),
                            br(),
                            h4("Estimated R values (7 day window)"),
                            DT::dataTableOutput("table_r")
                   ),
                   tabPanel("Methods",
                            includeMarkdown("methodsEffectiveR.Rmd")
                   ))))
      
      
    ))
}

server <- shinyServer(function(input, output) {
  
  options(stringsAsFactors = FALSE)
  options(scipen=999) # remove scientific notation
  
  #load libraries
  library(httr)
  library(jsonlite)
  library(rjson)
  library(RCurl)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(incidence)
  library(tidyr)
  library(plotly)
  
  ## connect to Alaksa data hub through the API
  url <- "https://opendata.arcgis.com/datasets/c0e6fd4aaa734e0e892d08189b76df4e_0.geojson"
  
  da <- jsonlite::fromJSON(url)
  base::class(da)
  da %>% str()
  
  # set data imported data as an object update to attributes 
  dat1 <- tibble::as_tibble(da$features$properties) 
  
  # Fix dates 
  # dat1$OnsetDate <- as.Date(as.POSIXct(dat1$OnsetDate/1000, origin = "1970-01-01", tz = "America/Anchorage"))
  
  dat1$OnsetDate <- as.Date(substr(dat1$OnsetDate,1,10))
  
  
  ### Onset date imputation  
  # calcuate duration in days.
  
  dat1$ReportDate <- as.Date(with(dat1, ifelse(OnsetDateReplaced == 2, dat1$OnsetDate,
                                               NA)), origin = "1970-01-01")
  
  # Had to simplify due to changes in data structure
  #  dat1$dur <- with(dat1, ifelse(duration %in% c(1:14), "Y","N"))
  #  sts <- tapply(as.numeric(dat1$duration), dat1$dur, summary )
  #  dat1$duration <- dat1$ReportDate - dat1$OnsetDate
  #  dat1$duration <- with(dat1, ifelse(duration <0 , 0, dat1$duration))
  
  dat1$duration <- dat1$ReportDate - dat1$OnsetDate
  
  
  ## set distribution for median among most recent 14 days duration for Gamma dist
  ## had to modify and hard code this due to change in data structure, and assume it is constant.
  set.seed(2020)
  #dist <- round(rgamma(1000, shape =  sts$Y[3])) 
  dist <- round(rgamma(1000, shape =  7)) 
  
  #zro <- table(dat1$duration<=0)[2] # isolate duration = 0 cases and impute values
  zro <- table(dat1$duration)
  
  set.seed(19) 
  smp1 <- sample(dist, zro)
  
  dat1$duration_imp <- ifelse(dat1$duration == 0, smp1, dat1$duration)
  
  # create onset date that subtracts out the imputed duration
  #dat1$OnsetDate1 <- as.Date(ifelse(dat1$duration == 0,
  #                                  dat1$ReportDate - dat1$duration_imp,
  #                                  dat1$OnsetDate),origin = "1970-01-01")
  dat1$OnsetDate1 <- as.Date(ifelse(is.na(dat1$duration), dat1$OnsetDate,
                                    dat1$OnsetDate - dat1$duration_imp),
                             origin = "1970-01-01")
  
  
  dat1$type <- with(dat1, ifelse(SpecificAcquisition %in% c("Community","Secondary"), "local",
                                 ifelse(SpecificAcquisition == "Travel", "imported","Under Investigation")))
  
  
  # should modernize this code with dplyr!
  
  dt <- as.Date(as.Date(min(dat1$OnsetDate)):as.Date(max(dat1$OnsetDate)),origin = "1970-01-01")
  
  osd <- as.data.frame(table(dat1$OnsetDate)) # recorded onset
  osd$Var1 <- as.Date(osd$Var1)
  osd1 <- as.data.frame(table(dat1$OnsetDate1)) # imputed onset
  osd1$Var1 <- as.Date(osd1$Var1)
  
  #create base summary dataframe  
  
  dat2 <- data.frame(date = dt,days = 0:(length(dt)-1))
  dat2 <- merge(dat2, osd, by.x = "date", by.y = "Var1", all.x = T)
  colnames(dat2)[3] <-"onsetcnt"
  dat2 <- merge(dat2, osd1, by.x = "date", by.y = "Var1", all.x = T)
  colnames(dat2)[4] <-"onsetcnt1" 
  
  dat2[is.na(dat2)] <- 0 
  dat2$day1 <- 1:(length(dt))
  
  # Cumulative sum
  dat2$onsetsum <- cumsum(dat2$onsetcnt)
  dat2$onsetsum1 <- cumsum(dat2$onsetcnt1)
  
  
  ### code to set end date to deal with if no cases are reported.
  #### this uses the max date from the testing data.
  
  url_x <- "https://opendata.arcgis.com/datasets/f2b5073959c247368e4cd28e54cd0cff_0.geojson"
  
  da_x <- jsonlite::fromJSON(url_x)
  base::class(da_x)
  da_x %>% str()
  
  # set data imported data as an object : date switched back
  dat1_x <- tibble::as_tibble(da_x$features$properties) 
  #dat1_x$Date <- as.Date(as.POSIXct(dat1_x$Date_/1000, origin = "1970-01-01", tz = "America/Anchorage"))
  dat1_x$Date<- as.Date(substr(dat1_x$Date_,1,10))
  max_date <- max(dat1_x$Date)
  
  #### End data inport and set-up ### 
  
  
  
  output$Plot <- renderPlotly({
    io_1 <- incidence(dat1$OnsetDate, last_date = max_date)
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
      labs(title = "Cumulative daily incidence by onset date, Alaska",
           subtitle = "(log-linear model: short term forecast with 95% confidence band)",
           caption = "*Indicated by gray dots: Illnesses that began during this period may not yet be reported resulting in incomplete data and excluded from model fit.") +
      theme(legend.justification = c(0, 1), 
            legend.position = c(0, 0.95),
            axis.title.x=element_blank())
    
    
    ### plotly version
    
    ggplotly(t1) %>%
      layout(title = list(text = paste0('Cumulative daily incidence by onset date, Alaska',
                                        '<br>',
                                        '<sup>',
                                        '(log-linear model: short term forecast with 95% confidence band)',
                                        '<br>',
                                        '<sup>',
                                        '*Note: Analyses truncated by 7 days (gray bars) due to delay in reporting, resulting in incomplete data',
                                        '</sup>')))
    
  })
  
  output$Plot1 <- renderPlot({
    io_1 <- incidence(dat1$OnsetDate, last_date = max_date)
    
    io_1dta <- as.data.frame(io_1)
    io_1dta$dates <- as.Date(io_1$dates, origin = "1970-01-01")
    io_1dta$None <- NA
    #lines
    io_1dta$mandate1 <- as.Date("2020-03-14")
    io_1dta$mandate4 <- as.Date("2020-03-17")
    io_1dta$mandate3 <- as.Date("2020-03-18")
    io_1dta$mandate5 <- as.Date("2020-03-19")
    io_1dta$mandate10 <- as.Date("2020-03-25")
    io_1dta$mandate11_12 <- as.Date("2020-03-28")
    
    #lifts
    io_1dta$mandate15 <- as.Date("2020-04-20")
    io_1dta$mandate16 <- as.Date("2020-04-24")
    io_1dta$mandate16_2 <- as.Date("2020-05-08")
    io_1dta$mandate18 <- as.Date("2020-05-12")
    
    shiny::validate(need(!is.null(input$lines), 'Please select at least one Mandate from the "Restrictions" list, and one Mandate from the "Restrictions lifted" list, to show a plot.'))
    
    data <- gather(select(io_1dta, input$lines), variable, value)
    
    shiny::validate(need(!is.null(input$lifts), 'Please select at least one Mandate from the "Restrictions" list, and one Mandate from the "Restrictions lifted" list, to show a plot.'))
    
    data2 <- gather(select(io_1dta, input$lifts), variable, value)
    
    #type
    dat1$type <- with(dat1, ifelse(SpecificAcquisition %in% c("Community","Secondary"), "Community and Secondary",
                                   ifelse(SpecificAcquisition == "Travel", "Travel","Under Investigation"))) 
    
    dat_type <- dat1[,c("OnsetDate","type")]
    colnames(dat_type)[1] <- "dates"
    dat_type$count <- 1
    
    ggplot() +
      geom_bar(data = dat_type, aes(x = dates, y=count, fill=type), stat = "identity",
               position="stack") +
      scale_fill_manual(values = c("#0a306a","#ffb923", "#999999"))+
      xlim(min(io_1$dates),max(io_1$dates)) +
      scale_y_continuous(limits = c(0,20)) +
      scale_x_date(breaks = pretty(io_1$dates, n = 10),date_labels = "%d %b") +
      theme_minimal() +
      labs(title="Epidemic curve by onset date, Alaska",
           subtitle = "Key Interventions") +
      
      geom_vline(xintercept = data$value, linetype="dotdash", size = 1.5, color = "red")+ #distancing, intrastate
      geom_vline(xintercept = data2$value, linetype="dotdash", size = 1.5, color = "#a7c636")+
      theme(legend.justification = c(0, 1), 
            legend.position = c(0, 0.95),
            axis.title.x=element_blank(),
            plot.title = element_text(size=20))
  })
  
  output$Plot3 <- renderPlotly({
    io_1 <- incidence(dat1$OnsetDate, last_date = max_date)
    
    #subset and create incidence object on the 10 days before truncation.
    
    io_2 <- subset(io_1, from = max(io_1$dates-20), to = max(io_1$dates-7))
    
    # for plotting end of non measured
    tnk <- as.data.frame(subset(io_1, from = max(io_1$dates-6), to = max(io_1$dates)))
    
    #Fit log linear model on the 14 day subset
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
    io_1 <- incidence(dat1$OnsetDate, last_date = max_date)
    io_1cum <- cumulate(io_1) # cumulative incidence
    
    #subset and create incidence object on the 10 days before truncation.
    
    io_2cum <- subset(io_1cum, from = max(io_1$dates-20), to = max(io_1$dates-7))
    
    # for plotting 7 days truncated.
    tnk <- as.data.frame(subset(io_1cum, from = max(io_1cum$dates-6), to = max(io_1cum$dates)))
    
    #Fit log linear model on the 10 day subset
    i_fit <- incidence::fit(io_2cum)
    #get_info(i_fit, "pred")
    
    tab <- as.data.frame(c(i_fit$info[4], i_fit$info[5]))
    colnames(tab)[2] <- "lower CI"
    colnames(tab)[3] <- "upper CI"
    
    tab
  })
  
  output$table3 <- renderTable({
    io_1 <- incidence(dat1$OnsetDate, last_date = max_date)
    
    #subset and create incidence object on the 10 days before truncation.
    
    io_2 <- subset(io_1, from = max(io_1$dates-20), to = max(io_1$dates-7))
    
    # for plotting end of non measured
    tnk <- as.data.frame(subset(io_1, from = max(io_1$dates-6), to = max(io_1$dates)))
    
    #Fit log linear model on the 10 day subset
    i_fit <- incidence::fit(io_2)
    #get_info(i_fit, "pred")
    
    tab <- as.data.frame(c(i_fit$info[4], i_fit$info[5]))
    colnames(tab)[2] <- "lower CI"
    colnames(tab)[3] <- "upper CI"
    tab
    
    
  })
  
  output$table1 <- renderTable({
    
    Mandate <- c("Mandate 1", "Mandate 4", "Mandate 3", "Mandate 5", "Mandate 10", "Mandate 11", "Mandate 12", 
                 "Mandate 15"  ,"Mandate 16", "Mandate 16-II", "Mandate 18")
    Effective_Date <- c("3/14", "3/17", "3/18", "3/19", "3/25", "3/28", "3/28", "4/20", "4/24", "5/08", "5/12")
    Description <- c("School Closure/Suspend Visitation at State Facilities", "Quarantine for Travelers from Level 3 Areas", 
                     "Close Restaurants/Bars/Entertainment",
                     "Cancel elective medical procedures",  
                     "Quarantine for all out of state travelers", "Social distancing/Closure of Non-Essential Business",
                     "Intrastate Travel restricted", "Open Non-Essential Medical Providers", 
                     "Reopen Alaska Responsibly - Phase 1-A", "Reopen Alaska Responsibly - Phase II",
                     "Intrastate Travel")
    table <- data.frame(Mandate, Effective_Date, Description)
    
    table  
    
  })
  
  url2 <- a("click here", href="https://www.covid19.alaska.gov/health-mandates")
  output$link <- renderUI({
    tagList("For more information about Health Mandates:", url2)
  })
  
  url3 <- a("click here", href="https://rt.live/")
  output$link1 <- renderUI({
    tagList("To see how Alaska's Rt compares with other States, visit https://rt.live or", url3)
  })
  
  output$Plot_r <- renderPlotly({
    #set up data  
    ip <- incidence(dat1$OnsetDate)
    ipI <- incidence(dat1$OnsetDate1)
    TC <- subset(ipI, from = min(ipI$dates), to = max(ip$dates-5))
    
    io <- incidence(dat1$OnsetDate, groups = dat1$type, na_as_group = T)
    ioI <- incidence(dat1$OnsetDate1,groups = dat1$type, na_as_group = T)
    LC <- subset(ioI, from = min(ioI$dates), to = max(io$dates-5))
    
    l1 <- list(`Total Cases` = TC, `Local Cases` = LC)
    
    d1 <- l1[[input$inSelect]]
    
    
    if (input$MSI == "para") {
      
      si7 <- estimate_R(d1,
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 5.011976, 
                          std_si = 4.10405)))
    } else {
      SL = c(1,3,1,3,1,6,3,5,4,7,3,4,10,8,4,3,1,2,5,3,
             2,3,3,3,5,4,3,15,1,6,4,2,4,1,2,9,6,9,1,11,
             1,4,6,6,6,6,6,3,1,5,1,2,11,3,1,3,8,9,2,4,
             5,3,2,2,13,1,1,7,6,4,1,7,5,2,1,1,6,7,1,5,
             3,1,2,3,3,2,9,14,4,16,7,18,4,10,9,3,8,2,15,8,
             5,1,2,1,8,10,6,1,1,3,1,15,10,1,6,2,18,3,1,3,
             4,10,10,18,1,4,4,3,6,3,6,4,5,5,7,8,1,1,4,5,
             2,4,12,16,6,3,2,1,8,1,7,3,9,15,18,3,1,2,1,1,
             3,1,8,2,5,7,9)
      
      si_data_AK <- data.frame(EL = as.integer(rep(0, 167)), 
                               ER = as.integer(rep(1,167)), 
                               SL = as.integer(SL), SR = as.integer(SL + 1))
      
      ## fixing the random seeds
      MCMC_seed <- 1
      overall_seed <- 2
      mcmc_control <- make_mcmc_control(seed = MCMC_seed, burnin = 100)
      dist <- "G"  # fitting a Gamma distribution for the SI
      
      empirical_si_config <- make_config(list(si_parametric_distr = dist, 
                                              mcmc_control = mcmc_control, 
                                              seed = overall_seed, 
                                              n1 = 100, 
                                              n2 = 100))
      
      si7 <- estimate_R(d1, 
                        method = "si_from_data", 
                        si_data = si_data_AK, 
                        config = empirical_si_config)
      
    }
    
    
    mean <- si7$R$`Mean(R)`
    low <- si7$R$`Quantile.0.025(R)`
    high <- si7$R$`Quantile.0.975(R)`
    date <- (si7$dates[-c(1:7)])
    
    dfx <- data.frame(mean = mean, low = low, high = high, date = as.Date(date, origin = "1970-01-01"))
    
    a <- ggplot(dfx, aes(x=date, y=mean))+
      geom_hline(yintercept =1, linetype="dotdash", size = 1, color = "red")+
      geom_line()+
      ylab("Rt")+
      xlab(" ")+
      ylim(0,4)+
      theme_minimal()+
      geom_ribbon(data = dfx, aes(x = date, ymin = low, ymax = high), 
                  alpha = 0.15,fill = "#0a306a")+
      labs(title="Time-varying reproductive number (Rt), Alaska")  +
      scale_x_date(breaks = pretty( dfx$date, n = 10),date_labels = "%d %b") 
    
    
    ggplotly(a)
    
    
  })
  
  output$table_r <- DT::renderDataTable({
    
    #set up data  
    ip <- incidence(dat1$OnsetDate)
    ipI <- incidence(dat1$OnsetDate1)
    TC <- subset(ipI, from = min(ipI$dates), to = max(ip$dates-5))
    
    io <- incidence(dat1$OnsetDate, groups = dat1$type, na_as_group = T)
    ioI <- incidence(dat1$OnsetDate1,groups = dat1$type, na_as_group = T)
    LC <- subset(ioI, from = min(ioI$dates), to = max(io$dates-5))
    
    l1 <- list(`Total Cases` = TC, `Local Cases` = LC)
    
    d1 <- l1[[input$inSelect]]
    
    
    if (input$MSI == "para") {
      
      si7 <- estimate_R(d1,
                        method="parametric_si",
                        config = make_config(list(
                          mean_si = 5.011976, 
                          std_si = 4.10405)))
    } else {
      SL = c(1,3,1,3,1,6,3,5,4,7,3,4,10,8,4,3,1,2,5,3,
             2,3,3,3,5,4,3,15,1,6,4,2,4,1,2,9,6,9,1,11,
             1,4,6,6,6,6,6,3,1,5,1,2,11,3,1,3,8,9,2,4,
             5,3,2,2,13,1,1,7,6,4,1,7,5,2,1,1,6,7,1,5,
             3,1,2,3,3,2,9,14,4,16,7,18,4,10,9,3,8,2,15,8,
             5,1,2,1,8,10,6,1,1,3,1,15,10,1,6,2,18,3,1,3,
             4,10,10,18,1,4,4,3,6,3,6,4,5,5,7,8,1,1,4,5,
             2,4,12,16,6,3,2,1,8,1,7,3,9,15,18,3,1,2,1,1,
             3,1,8,2,5,7,9)
      
      si_data_AK <- data.frame(EL = as.integer(rep(0, 167)), 
                               ER = as.integer(rep(1,167)), 
                               SL = as.integer(SL), SR = as.integer(SL + 1))
      
      ## fixing the random seeds
      MCMC_seed <- 1
      overall_seed <- 2
      mcmc_control <- make_mcmc_control(seed = MCMC_seed, burnin = 100)
      dist <- "G"  # fitting a Gamma distribution for the SI
      
      empirical_si_config <- make_config(list(si_parametric_distr = dist, 
                                              mcmc_control = mcmc_control, 
                                              seed = overall_seed, 
                                              n1 = 100, 
                                              n2 = 100))
      
      si7 <- estimate_R(d1, 
                        method = "si_from_data", 
                        si_data = si_data_AK, 
                        config = empirical_si_config)
      
    }
    
    
    td1 <- si7$R[,c(3,5,11)]
    td1$dates <-   d1$dates[-c(1:7)]
    
    td2 <- as.data.frame(td1[,c(4,1:3)])
    #td2
    td3 <- td2[order(-as.numeric(rownames(td2))),]
    td3
    
    
  })
  
})


# Run the application 
shinyApp(ui = ui, server = server)