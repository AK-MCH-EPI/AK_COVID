library(shiny)

server <- function(input,output,session){

source("global.R")
  
### Epi data - projections
  
    global_epi_data <- reactive({
        
        if(input$inSelectReg == "Statewide") {
            dat1 <- dat1
        } else {
            dat1 <- dat1 %>% filter(BHR_Name == input$inSelectReg)
        }
        
        
        if(input$inSelectRes == "Resident" & input$inCheckbox == FALSE) {
            e_dat1 <- dat1 %>% filter(Resident == "Y" & Occurrence == "Y")
        }
        if(input$inSelectRes == "Resident & non-Resident" & input$inCheckbox == FALSE) {
            e_dat1 <- dat1 %>% filter(Occurrence == "Y")
        }
        if(input$inSelectRes == "Resident" & input$inCheckbox == TRUE) {
            e_dat1 <- dat1 %>% filter(Resident == "Y")
        }
        if(input$inSelectRes == "Resident & non-Resident" & input$inCheckbox == TRUE) {
            e_dat1 <- dat1
        }
        
        # Code for plot and table
        
        locate <- input$inSelectReg
        c_ex <- nrow(dat1) - nrow(e_dat1)
        
        # Incidence objects #
        io_1 <- incidence(e_dat1$OnsetDate, last_date = max_date)
        io_1a <- incidence(e_dat1$OnsetDate1, last_date = max_date)
        
        #subset and create incidence object on the 21 days before truncation.
        io_2 <- subset(io_1a, from = max(io_1a$dates-27), to = max(io_1a$dates-7))
        #Fit log linear model on the 14 day subset
        i_fit <- incidence::fit(io_2)

        list(io_1 = io_1, io_1a = io_1a, io_2 = io_2,i_fit = i_fit, c_ex = c_ex, locate = locate)
        
    })    
    
    output$value_ex <- renderText({
        paste("Number of cases excluded based on case type selection:",global_epi_data()$c_ex)
    })
    
    output$epi.plot <- renderPlotly({
      
        io_1 <- global_epi_data()$io_1
        io_1a <- global_epi_data()$io_1a
        i_fit <- global_epi_data()$i_fit
        io_2 <- global_epi_data()$io_2

        if(input$cumcntCheckbox == FALSE) {
        
        # for plotting end of non measured
        tnk <- as.data.frame(subset(io_1, from = max(io_1$dates-6), to = max(io_1$dates)))

        #set forecasting range for data
        dtr <- data.frame(date = as.Date(min(io_1$dates):(max(io_1$dates)+10), origin = "1970-01-01"))
        #dtra <- data.frame(date = as.Date(min(io_1a$dates):(max(io_1a$dates)+10), origin = "1970-01-01"))
        
        #dts <- c((i_fit$model$model$dates.x), ((max(i_fit$model$model$dates.x)+1): 31)) # create range for prediction
        dts <- c(min(i_fit$model$model$dates.x): 38)
        tIn <- data.frame(dates.x = as.numeric(dts)) #turn into data frame
        
        #prediction from fitted model
        extFit <- predict(i_fit$model, tIn,interval = "confidence", level = 0.95)
        
        y <- exp(extFit) # put on natural scale
        
        #drg <- c(((max(dtr$date))-max(tIn$dates.x)):(max(dtr$date))) # set dates for predicted range
        drg <- c((min(io_2$dates)):(max(dtr$date))) # set dates for predicted range
        
        #get_info(i_fit, "pred")
        
        #create dataframe of predictions
        projs <- data.frame(y, date = as.Date(drg, origin = "1970-01-01"))
        
        
        ### plotly version ###
        
        io_1dta <- as.data.frame(io_1)
        io_2dta <- as.data.frame(io_1a)
        
        p1<-ggplot() +
            geom_bar(data = io_1dta, aes(x = dates, y = counts,
                                         text = paste("Date:",dates,"<br>count:",counts) ), stat = "identity",
                     fill = "#ffb923", color = "white", width = 1) +
            geom_bar(data = tnk, aes(x = dates, y = counts,
                                     text = paste("Date:",dates,"<br>count:",counts)), stat = "identity", 
                     fill = "#999999", color = "white", width = 1) +
            geom_line(aes(x=date, y=fit,group=1,
                          text = paste("Date:",date,"<br>Projected count:",round(fit,2),
                                       "<br>LowerCI:",round(lwr,2),"<br>UpperCI:",round(upr,2))), 
                      projs,linetype="dashed", size = 1, color = "#0a306a") +
            geom_ribbon(data = projs, aes(x = date, ymin = lwr, ymax = upr), 
                        alpha = 0.20,fill = "#0a306a") +
            #xlim(min(dtr$date),max(dtr$date)) +
            coord_cartesian(ylim = c(0, max(io_1$counts)*1.7)) +
            scale_x_date(breaks = pretty(dtr$date, n = 10),date_labels = "%d %b",
                         limits = c(((max_date) %m-% months(input$inSlide)),(max_date+10))) +
            theme_minimal() +
            #theme(plot.caption = element_text(hjust = 0)) +
            labs(title=paste("Epidemic curve by onset date,", global_epi_data()$c_ex),
                 subtitle = "(log-linear model: short term forecast with 95% confidence band)",
                 caption = "*Indicated by gray bars: Illnesses that began during this period may not yet be reported resulting in incomplete data and excluded from model fit.") +
            theme(legend.justification = c(0, 1), 
                  legend.position = c(0, 0.95),
                  axis.title.x=element_blank()) 
        
        ggplotly(p1, tooltip = c("text")) %>%
            layout (title = list(text = paste0('Epidemic curve by onset date,',' ', global_epi_data()$locate,
                                               '<br>',
                                               '<sup>',
                                               '(log-linear model: short term forecast with 95% confidence band)',
                                               '<br>',
                                               '<sup>',
                                               '*Note: Analyses truncated by 7 days (gray bars) due to delay in reporting, resulting in incomplete data',
                                               '</sup>')))
        # annotations = list(x = 18324, y = 19, text = "Truncated", showarrow = F,
        #                    size = 0.5))
        } 
        else{
                
            #Cumulative incidence objects #
            io_1cum <- cumulate(io_1) # cumulative incidence
            #subset and create incidence object on the 10 days before truncation.
            io_2cum <- subset(io_1cum, from = max(io_1$dates-16), to = max(io_1$dates-7))
            # for plotting 7 days truncated.
            tnk_c <- as.data.frame(subset(io_1cum, from = max(io_1cum$dates-6), to = max(io_1cum$dates)))
            #Fit log linear model on the 10 day subset
            i_fit_c <- incidence::fit(io_2cum)
            
                dtr <- data.frame(date = as.Date(min(io_1cum$dates):(max(io_1cum$dates)+10), origin = "1970-01-01"))
                
                dts <- c(min(i_fit_c$model$model$dates.x):27)
                tIn <- data.frame(dates.x = as.numeric(dts)) #turn into data frame
                
                #prediction from fitted model
                extFit <- predict(i_fit_c$model, tIn,interval = "confidence", level = 0.95)
                
                y <- exp(extFit) # put on natural scale
                
                drg <- c(((max(dtr$date))-max(tIn$dates.x)):(max(dtr$date))) # set dates for predicted range
                
                #create dataframe of predictions
                projs <- data.frame(y, date = as.Date(drg, origin = "1970-01-01"))
                
                #`Short term projection with 95% confidence bands` <- "red"
                
                #plot epi curve and prediction window
                io_1cum_df <- as.data.frame(subset(io_1cum, 
                                                   from = min(io_1$dates), 
                                                   to = max(io_1$dates-7)))
                
                t1 <- ggplot() +
                    geom_line(data=io_1cum_df, 
                              aes(x = dates, y = counts,group = 1,
                                  text = paste("Date:",dates,"<br>count:",counts)),
                              size = 1.5, color = "#ffb923") +
                    geom_point(data=io_1cum_df, 
                               aes(x = dates, y = counts,group = 1,
                                   text = paste("Date:",dates,"<br>count:",counts)),
                               size = 1.5, color = "#ffb923") +
                    
                    geom_point(data = tnk_c, 
                               aes(x = dates, y = counts, group = 1,
                                   text = paste("Date:",dates,"<br>count:",counts)), 
                               color = "#999999", size = 1.5) +
                    
                    geom_ribbon(data = projs, 
                                aes(x = date, ymin = lwr, ymax = upr), 
                                alpha = 0.20, fill = "#0a306a")+
                    
                    scale_x_date(breaks = pretty(dtr$date, n = 10), date_labels = "%d %b",
                                 limits = c(((max_date) %m-% months(input$inSlide)),(max_date+10))) +
                    coord_cartesian(ylim = c(0, max(io_1cum_df$counts)*1.7)) +
                    geom_line(aes(date, fit, group = 1,
                                  text = paste("Date:",date,"<br>Projected count:",round(fit,2),
                                               "<br>LowerCI:",round(lwr,2),"<br>UpperCI:",round(upr,2))), 
                              projs,linetype="dashed", size = 0.5, color = "#0a306a") +
                    theme_minimal() +
                    labs(title = "Cumulative daily incidence by onset date, Alaska",
                         subtitle = "(log-linear model: short term forecast with 95% confidence band)",
                         caption = "*Indicated by gray dots: Illnesses that began during this period may not yet be reported resulting in incomplete data and excluded from model fit.") +
                    theme(legend.justification = c(0, 1), 
                          legend.position = c(0, 0.95),
                          axis.title.x=element_blank())
                
                
                ### plotly version
                
                ggplotly(t1, tooltip = c("text")) %>%
                    layout(title = list(text = paste0('Epidemic curve by onset date,',' ', global_epi_data()$locate,
                                                      '<br>',
                                                      '<sup>',
                                                      '(log-linear model: short term forecast with 95% confidence band)',
                                                      '<br>',
                                                      '<sup>',
                                                      '*Note: Analyses truncated by 7 days (gray dots) due to delay in reporting, resulting in incomplete data',
                                                      '</sup>')))
            
        } 
        
    })
    
    output$epi.table <- renderUI({
        
        io_1 <- global_epi_data()$io_1
        i_fit <- global_epi_data()$i_fit
        
        if(input$cumcntCheckbox == FALSE) {
        st1 <- paste0("Estimated number of days to", ' ', names(i_fit$info[4]),':',' ',
                      round(i_fit$info[[4]],2))
        st2 <- paste0("Estimated daily growth rate",':', ' ', round(i_fit$info$r,4)*100,'%')
   
        HTML(paste(st1, st2, sep = '<br/>'))
        } else {
            #Cumulative incidence objects #
            io_1cum <- cumulate(io_1) # cumulative incidence
            #subset and create incidence object on the 10 days before truncation.
            io_2cum <- subset(io_1cum, from = max(io_1$dates-16), to = max(io_1$dates-7))
            # for plotting 7 days truncated.
            tnk_c <- as.data.frame(subset(io_1cum, from = max(io_1cum$dates-6), to = max(io_1cum$dates)))
            #Fit log linear model on the 10 day subset
            i_fit_c <- incidence::fit(io_2cum)
            
            st1 <- paste0("Estimated number of days to", ' ', names(i_fit_c$info[4]),':',' ',
                          round(i_fit_c$info[[4]],2))
            st2 <- paste0("Estimated daily growth rate",':', ' ', round(i_fit_c$info$r,4)*100,'%')
            
            HTML(paste(st1, st2, sep = '<br/>')) 
            
        }
    })
    
    observeEvent(input$Epi_explain, {
      sendSweetAlert(
        session = session,
        title = "Measures of acceleration/deceleration",
        text = HTML("<p>The predicted exponential trajectory can be one of growth (getting bigger) 
                     or decay (getting smaller). If in growth, the doubling time represents the 
                     projected amount of time it will take for the counts to double in size. 
                     If in decline, the halving time represents the 
                     projected amount of time it will take for the counts to reduce in half.<p> 
                    <p>The daily growth rate is the predicted daily percentage change.<p>
                    <p>For technical details on our approach, please review the Methods tab.<p>"
                    
        ),
        html = TRUE,
        type = NULL
      )
    })
    
### R Effective 

    r_data <- reactive({
        
        if(input$inSelectReg1 == "Statewide") {
            dat1 <- subset(dat1, Occurrence == "Y")
        } else {
            dat1 <- dat1 %>% filter(BHR_Name == input$inSelectReg1 & Occurrence == "Y")
        }

        locate <- input$inSelectReg1
        
        if(input$inRes_select == "Resident & non-Resident"){
            dat1s <- dat1
        } else {
            dat1s <- dat1 %>% filter(Resident == "Y")
        }
        
        
        # incidence objects
        io_1a <- incidence(dat1s$OnsetDate1, last_date = max_date)
        ############
        
        #subset and create incidence object on the 21 days before truncation.
        io_2 <- subset(io_1a, from = max(io_1a$dates-27), to = max(io_1a$dates-7))
        
        #Fit log linear model on the 21 day subset
        i_fit <- incidence::fit(io_2)
        #get_info(i_fit, "pred")
        #io_2$counts
        
        ## Use model to predict daily counts for subsetted week.
        
        dtp <- as.Date(max(io_2$dates+1):max(io_2$dates+7),origin = "1970-01-01")
        dtpf <- data.frame(dates.x = (max(i_fit$model$model$dates.x)+1):(max(i_fit$model$model$dates.x)+7))   
        
        extFit <- predict(i_fit$model, dtpf, interval = "confidence", level = 0.95)
        y <- exp(extFit)
        
        projs <- data.frame(y, date = as.Date(dtp, origin = "1970-01-01"))
        
        ## Create dataset with fitted values substituded for the past 7-days.
        io_1b <- incidence(dat1s$OnsetDate1, last_date = max_date-7)
        dts <- c(io_1b$dates, projs$date)
        cnts <- c(io_1b$counts, projs$fit)
        cnts_L <- c(io_1b$counts, projs$lwr)
        cnts_U <- c(io_1b$counts, projs$upr)
        
        #Create dataframes for Reff estimates
        
        TCe <- data.frame(dates = dts,I = cnts)
        TCeL <- data.frame(dates = dts,I = cnts_L)
        TCeU <- data.frame(dates = dts,I = cnts_U)
        
        
        make_data <- function(x) {
            t_start_e <- seq(2, length(x[["dates"]])-13)
            t_end_e   <- t_start_e + 13
            si7 <- estimate_R(x,
                              method="parametric_si",
                              config = make_config(list(
                                  mean_si = 5.011976, 
                                  std_si = 4.10405,
                                  t_start = t_start_e,
                                  t_end = t_end_e)))
            
            mean_e <- round(si7$R$`Mean(R)`,2)
            low_e <- round(si7$R$`Quantile.0.025(R)`,2)
            high_e <- round(si7$R$`Quantile.0.975(R)`,2)
            date_e <- (si7$dates[-c(1:14)])
            
            dfx_e <- data.frame(mean = mean_e, low = low_e, high = high_e, date = as.Date(date_e, origin = "1970-01-01"))
            return(dfx_e)
        }
        
        dx1 <- make_data(TCe)
        dxL <- make_data(TCeL)
        dxU <- make_data(TCeU)
        
        #subset to observed and provisional and plot
        dx1$Reff <- with(dx1, ifelse(date %in% dtp, "Provisional","Estimate"))
        dxLs <- dxL %>% mutate(Reff = ifelse(date %in% dtp, "Provisional","Estimate")) %>%
            filter(Reff == "Provisional")
        dxUs <- dxU %>% mutate(Reff = ifelse(date %in% dtp, "Provisional","Estimate")) %>%
            filter(Reff == "Provisional")
        
        dx2 <- data.frame(dx1,
                          lowlow  = dxL$low,
                          highhigh = dxU$high)
        
        ## set bounds that accounts for fitted value uncert & bounds
        
        dx3 <- dx2 %>% mutate(lower = ifelse(Reff == "Estimate", low, lowlow),
                              upper = ifelse(Reff == "Estimate", high, highhigh))
        
        
        list(dx1 = dx1, dxLs = dxLs, dxUs = dxUs, dx3 = dx3, locate = locate)  
        
    })
  
    output$r.plot <- renderPlotly({
        
        # dx1 <- r_data()$dx1
        # dxLs <- r_data()$dxLs
        # dxUs <- r_data()$dxUs
        dx3 <- r_data()$dx3
        dx3 <- subset(dx3, date >= (max(dx3$date) %m-% months(input$inSlide1)))
        
        a <- ggplot()+
            geom_hline(yintercept = 1, linetype="dotdash", size = 1, color = "red")+
            geom_line(data = dx3, aes(x=date, y = mean, linetype = Reff, group = Reff,
                                  text = paste("Date:",date,"<br>Reff value:",mean,"<br>LowerCI:",lower,
                                               "<br>UpperCI:",upper)), color = "#0a306a", size = 1.5) +
            scale_linetype_manual(values = c("solid","dotted")) +
            geom_ribbon(data = dx3, aes(x = date, ymin = lower, ymax = upper),
                         alpha = 0.20, fill = "#0a306a")+
            ylab("Rt")+
            xlab(" ")+
            theme_minimal()+
            labs(title= paste("Time-varying reproductive number (Rt),",r_data()$locate))  +
            scale_x_date(breaks = pretty( dx3$date, n = 10),date_labels = "%d %b") +
            coord_cartesian(ylim = c(0, 3)) 
            
        ggplotly(a, tooltip = c("text"))
        
        
    })
    
    output$r.table <- renderUI({
        
        #list(dx1 = dx1, dxLs = dxLs, dxUs = dxUs, dx3 = dx3 )  
        
        dx1 <- r_data()$dx1
        dx3 <- r_data()$dx3

        st1 <- paste0("Current Rt estimate as of",' ', last(dx1$date),":",' ', 
                     round(last(dx1$mean),2), ' ', '(',last(dx3$lower), ' ', '-', ' ', last(dx3$upper),')')
        
        mn <- mean(dx1$mean[c((max(nrow(dx1)-14)):(max(nrow(dx1))))])
        st2 <- paste0("Average Rt over past 14 days:", ' ',round(mn,3))
          HTML(paste(st1, st2, sep = '<br/>'))  
    }) 
    
    output$downloadRtData <- downloadHandler(
        
        filename = function() {
            paste(input$inSelectReg1, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(r_data()$dx3[,c(4,1,8,9,5)], file, row.names = FALSE)
        }
    )
    
    observeEvent(input$Rt_explain, {
      sendSweetAlert(
        session = session,
        title = "Understanding Rt",
        text = HTML("<p>The effective reproductive number (R-effective or time-varying Rt),
                    is the avereage number of people, that a single infected person
                    will pass the virus onto and represents the rate at which the virus is spreading.
                    <p>When Rt is greater than 1, COVID-19 will continue to spread, and the higher
                    the value of Rt, the faster an epidemic will progress. If Rt is less than 1,
                    COVID-19 will spread more slowely and cases will decline.<p>
                    <p>For technical details on our approach, please review the Methods tab.<p>"
                    
        ),
        html = TRUE,
        type = NULL
      )
    })
    
    
### Average Daily Rate     
    rate_data <- reactive({
      
    if(input$inSelectRes_rate == "Resident"){  
      
        rsrt.dat <- avr_data
        
        rsrt.dat$daily_average <- round(rsrt.dat$daily_average,2)
        rsrt.dat$alert <- ifelse(rsrt.dat$rate < 5, "Low",
                          ifelse(rsrt.dat$rate > 10,"High","Intermediate"))
        rsrt.dat <- rsrt.dat %>% filter(Region != "Unknown")
    }else{
        rsrt.dat <- avrn_data
      
        rsrt.dat$daily_average <- round(rsrt.dat$daily_average,2)
        rsrt.dat$alert <- ifelse(rsrt.dat$rate < 5, "Low",
                               ifelse(rsrt.dat$rate > 10,"High","Intermediate"))
        rsrt.dat <- rsrt.dat %>% filter(Region != "Unknown")
        }
      
      
      
        if (input$geo == "Statewide"){
            
            temp1a <- rsrt.dat %>% filter(Area == c(input$geo))   
            
        }
        if (input$geo == "Behavioral Health Region"){
            temp1 <- rsrt.dat %>% filter(Area == c(input$geo))
            temp1a <- temp1 %>% filter(window %in% c(input$wind))
            
        }
        if (input$geo == "County") {
            temp1 <- rsrt.dat %>% filter(Area == c(input$geo))
            temp1a <- temp1 %>% filter(window %in% c(input$wind))  
        }
      
        temp2a <- temp1a %>% filter(window == input$wind, Region == input$rgn)
        
        w7 <- with(temp2a, ifelse(input$wind == "14 day window", 13,6))
        
        temp2a <- temp2a[-c(1:w7),]
        
        alert_df <- data.frame(xmin = min(temp2a$dates),
                               xmax = max(temp2a$dates),
                               ymin = c(0,5,10),
                               ymax = c(5,10,max(temp2a$upperCI)+5),
                               Alert_Level = c("Low","Intermediate","High"))
        
        list(alert_df = alert_df, temp2a = temp2a, rsrt.dat = rsrt.dat)
        
    })
    
    output$factor_dropdown <- renderUI({
        Factors <- avr_data %>% filter(Area == input$geo)
        Factors <- names(table(Factors$Region))
        Factors <- as.list(Factors)
        selectInput("rgn", "Select Region:",
                    choices = Factors,
                    selected = Factors[1])
    })
    
    output$rate.plot <- renderPlotly({
        
        
        a <- ggplot() +
            geom_rect(data = rate_data()$alert_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                                       fill = Alert_Level,
                                                       text = paste("Alert Level:",Alert_Level)))+
            scale_fill_manual(values = c("#df212a","#f37b21","#faea5a")) +
            geom_ribbon(data = rate_data()$temp2a, aes(x = dates, ymin = lowerCI, ymax = upperCI),
                        alpha = 0.20, fill = "black") +
            geom_line(data = rate_data()$temp2a, aes(x = dates, y = rate, group = 1,
                                                     text = paste("Date:",dates,"<br>Rate:",rate,
                                                                  "<br>LowerCI:",lowerCI,
                                                                  "<br>UpperCI:",upperCI)), 
                      colour = "black") +
            ylab("rate per 100k residents")+
            xlab("")+
            #ylim(0,max(temp2a$rate)+10)+
            coord_cartesian(ylim = c(0, max(rate_data()$temp2a$upperCI)+5)) +
            scale_x_date(breaks = pretty(rate_data()$temp2a$dates, n = 10), date_labels = "%d %b",
                         limits = c(min(rate_data()$temp2a$dates), max(rate_data()$temp2a$dates))) +
           labs(title= paste0("Average daily case rate",' ','(',input$wind,')', ' ',
                              "per 100,000 population,", ' ',input$rgn)) +
            theme_minimal()
        
        ggplotly(a, tooltip = c("text")) 
        
    })
    
    output$AlertLevel.box <- renderValueBox({
        current.rt <- last(rate_data()$temp2a$rate)
        a_level <- ifelse(current.rt >= 10, "Current Alert Level: High",
                          ifelse(current.rt < 10 & current.rt >= 5, "Current Alert Level: Intermediate",
                                 ifelse(current.rt < 5 ,"Current Alert Level: Low")))
        valueBox(a_level,subtitle = paste0(current.rt," ","per 100,000 population"))
    })
    
    output$downloadRateData <- downloadHandler(
        
        filename = function() {
            paste(input$rgn, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(rate_data()$temp2a, file, row.names = FALSE)
        }
    ) 
    observeEvent(input$RateC_explain, {
      sendSweetAlert(
        session = session,
        title = "Average daily case rate",
        text = HTML("<p> The Alaska DHSS standard for determining alert levels uses in-state resident 
                     cases averaged over a 14-day window. Other options (7-day window and all cases) 
                     are provided for convenience. The per-capita rate uses the census population 
                     estimates for resident cases, and census population + influx estimate for all 
                     cases as respective denominators. All estimates exclude resident out-of-state cases.</p>
                     <p>This estimate can be used to monitor the general trajectory of disease 
                     spread and enables comparison between areas with different population 
                     sizes.</p>
                      <p>To learn more about Alaska COVID-19 Alert Levels please visit:
                      <a href='http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx'>http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx</a>.</p>"

        ),
        html = TRUE,
        type = NULL
      )
    })     
    observeEvent(input$Alert_explain, {
      current.rt <- last(rate_data()$temp2a$rate)
      a_level_I <- ifelse(current.rt >= 10, "High Alert Level (>10 cases per 100,000 residents): Widespread community transmission with many
                          undetected cases and frequent discrete outbreaks is likely occurring ",
                   ifelse(current.rt < 10 & current.rt >= 5, "Intermediate Alert Level (5-10 cases per 100,000 residents): Moderate community
                          transmission with some undetected cases and infrequent discrete outbreaks  is likely occurring ",
                   ifelse(current.rt < 5 ,"Low Alert Level (<5 cases per 100,000 residents): Minimal community transmission  is likely occurring ")))
      
      sendSweetAlert(
        session = session,
        title = "What does this alert level mean?",
        text = HTML(a_level_I, '<br>','<br>',
                    "<p>To learn more about Alaska COVID-19 Alert Levels please visit:
                      <a href='http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx'>http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx</a>.</p>"
                    
        ),
        html = TRUE,
        type = NULL
      )
    })     
}
