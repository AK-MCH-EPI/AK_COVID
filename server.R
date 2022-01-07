library(shiny)

server <- function(input,output,session){

 ## read data and organize for processing ####

source("global.R")
  
### Epi data - projections ####
  
    global_epi_data <- reactive({
        
        if(input$inSelectReg == "Statewide") {
            dat1 <- dat1
        } else {
            dat1 <- dat1 %>% filter(bhr == input$inSelectReg)
        }
        
        
        if(input$inSelectRes == "Resident" & input$inCheckbox == FALSE) {
            e_dat1 <- dat1 %>% filter(resident_y_n == "Y" & occurrence_y_n == "Y")
        }
        if(input$inSelectRes == "Resident & non-Resident" & input$inCheckbox == FALSE) {
            e_dat1 <- dat1 %>% filter(occurrence_y_n == "Y")
        }
        if(input$inSelectRes == "Resident" & input$inCheckbox == TRUE) {
            e_dat1 <- dat1 %>% filter(resident_y_n == "Y")
        }
        if(input$inSelectRes == "Resident & non-Resident" & input$inCheckbox == TRUE) {
            e_dat1 <- dat1
        }
        
        # Code for plot and table
        
        locate <- input$inSelectReg
        c_ex <- nrow(dat1) - nrow(e_dat1)
        
        io_1 <- incidence(e_dat1$OnsetDate_imp, last_date = max(e_dat1$report_date))
        io_1d <- as.data.frame(io_1)
        
        io_2 <- subset(io_1, to = max(io_1$dates-5))
        io_2d <- as.data.frame(io_2)
      
        temp3 <- subset(io_1, from = max(io_1$dates-25), to = max(io_1$dates)-5) # Range for prediction model

        i_fit <- incidence::fit(temp3)  
        
        list(io_1 = io_1, io_1d = io_1d, io_2 = io_2, io_2d = io_2d, i_fit = i_fit, c_ex = c_ex, locate = locate)
        
    })    
    
    output$value_ex <- renderText({
        paste("Number of cases excluded based on case type selection:",global_epi_data()$c_ex)
    })
    
    output$epi.plot <- renderPlotly({
      
        io_1 <- global_epi_data()$io_1
        io_2 <- global_epi_data()$io_2
        io_1d <- global_epi_data()$io_1d
        io_2d <- global_epi_data()$io_2d
        i_fit <- global_epi_data()$i_fit
        
        if(input$cumcntCheckbox == FALSE) {
        
        
          make_data <- function(x) {
            t_start_e <- seq(2, length(x[["dates"]])-9)
            t_end_e   <- t_start_e + 9
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
            date_e <- (si7$dates[-c(1:10)])
            
            dfx_e <- data.frame(mean = mean_e, low = low_e, high = high_e, date = as.Date(date_e, origin = "1970-01-01"))
            return(dfx_e)
          }
          
          TCe <- data.frame(dates = io_2d$dates,I = io_2d$counts)
          dx1 <- make_data(TCe)
          
          dts <- c(min(i_fit$model$model$dates.x): 33)
          tIn <- data.frame(dates.x = as.numeric(dts)) #turn into data frame
          extFit <- predict(i_fit$model, tIn,interval = "confidence", level = 0.95)
          
          y <- exp(extFit)
          
          drg <- as.Date(c((max(io_1d$dates-25)):(max(io_1d$dates)+7)),origin = "1970-01-01") # set dates for predicted range
          
          #create dataframe of predictions
          projs <- data.frame(y, date = as.Date(drg, origin = "1970-01-01"))
        
          #use average Rt over last 
          avgRt10 <- mean(dx1$mean[c((max(nrow(dx1)-9)):(max(nrow(dx1))))]) 
          
          ## Projection estimates
          set.seed(1)
          si <- distcrete("gamma", interval = 1L,
                          shape = 6,
                          #scale = 4.0,
                          w = 0.5)
          proj_1 <- project(subset(io_1, to = max(io_1$dates-5)), 
                            R = avgRt10, si = si, n_days = 5)
          
          proj_2 <- as.data.frame(proj_1)
          proj_2$R.mean <- rowMeans(proj_2[,c(2:101)],na.rm = TRUE)
          proj_3 <- cbind(proj_2,t(apply(proj_2[,-c(1,101)],1,quantile, probs = c(0.025, 0.5, 0.975),
                                         na.rm = TRUE)))
          
          dta <- data.frame(dates = as.Date(c((max(io_1d$dates)-4):(max(io_1d$dates))),origin = "1970-01-01"),
                            Mean = c(proj_3$R.mean),
                            lower = c(proj_3$`2.5%`),
                            upper = c(proj_3$`97.5%`))
          
        ### plotly version ###
        
        p1<-ggplot() +
            geom_bar(data = io_2d, aes(x = dates, y = counts,
                                         text = paste("Date:",dates,"<br>Imputed count:",counts))
                     , stat = "identity",fill = "#ffb923") +
            geom_bar(data = dta, 
                     aes(x = dates, y = Mean,
                                     text = paste("Date:",dates,"<br>Adjusted count:",Mean,
                                           "<br>LowerCI:",round(lower,2),"<br>UpperCI:",round(upper,2))), 
                     stat = "identity",fill = "gray70") +

            geom_line(data = projs,
                      aes(x=date,y=fit,group=1,
                        text = paste("Date:",date,"<br>Model projection:",round(fit,2),
                                     "<br>LowerCI:",round(lwr,2),"<br>UpperCI:",round(upr,2))), 
                   linetype="dashed", size = 1, color = "#0a306a") +

            geom_ribbon(data = projs, 
                      aes(x = date, ymin = lwr,ymax = upr),
                      fill = "blue",alpha = 0.20)+
          
            coord_cartesian(ylim = c(0, max(io_1d$counts)*1.7),
                            xlim = c(((max_date) %m-% months(input$inSlide)),(max_date+10))) +
            scale_x_date(date_breaks = "1 month",date_labels = "%b %Y") +
            theme_minimal() +
            #theme(plot.caption = element_text(hjust = 0)) +
            labs(title=paste("Short term COVID-19 forecast,", global_epi_data()$c_ex),
                 subtitle = "(log-linear model: short term forecast with 95% confidence band)",
                 caption = "*Indicated by gray bars: Illnesses that began during this period may not yet be reported resulting in incomplete data and excluded from model fit.") +
            theme(legend.justification = c(0, 1), 
                  legend.position = c(0, 0.95),
                  axis.title.x=element_blank(),
                  axis.text.x = element_text(angle = 30,hjust = 1)) 
        
        ggplotly(p1, tooltip = c("text")) %>%
            layout (title = list(text = paste0('Estimated COVID-19 Epidemic Curve,',' ', global_epi_data()$locate,
                                               '<br>',
                                               '<sup>',
                                               'Gray bars: preliminary adjusted counts estimated from incomplete data due to reporting lag', 
                                               '<br>',
                                               '<sup>',
                                               '(log-linear model: short term forecast with 95% confidence band)',
                                               '</sup>')))
        # annotations = list(x = 18324, y = 19, text = "Truncated", showarrow = F,
        #                    size = 0.5))
        } 
        else{
                
            #Cumulative incidence objects #
            io_1cum <- cumulate(io_1) # cumulative incidence
            #subset and create incidence object on the 10 days before truncation.
            io_2cum <- subset(io_1cum, from = max(io_1$dates-15), to = max(io_1$dates-5))
            # for plotting 5 days truncated.
            tnk_c <- as.data.frame(subset(io_1cum, from = max(io_1cum$dates-4), to = max(io_1cum$dates)))
            #Fit log linear model on the 10 day subset
            i_fit_c <- incidence::fit(io_2cum)
            
                dtr <- data.frame(date = as.Date(min(io_1cum$dates):(max(io_1cum$dates)+10), origin = "1970-01-01"))
                
                dts <- c(min(i_fit_c$model$model$dates.x):25)
                tIn <- data.frame(dates.x = as.numeric(dts)) #turn into data frame
                
                #prediction from fitted model
                extFit <- predict(i_fit_c$model, tIn,interval = "confidence", level = 0.95)
                
                y <- exp(extFit) # put on natural scale
                
                drg <- c(((max(dtr$date))-max(tIn$dates.x)):(max(dtr$date))) # set dates for predicted range
                
                #create dataframe of predictions
                projs <- data.frame(y, date = as.Date(drg, origin = "1970-01-01"))
                
                ## Projection estimates for truncation window!!!
                
                make_data <- function(x) {
                  t_start_e <- seq(2, length(x[["dates"]])-9)
                  t_end_e   <- t_start_e + 9
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
                  date_e <- (si7$dates[-c(1:10)])
                  
                  dfx_e <- data.frame(mean = mean_e, low = low_e, high = high_e, date = as.Date(date_e, origin = "1970-01-01"))
                  return(dfx_e)
                }
                
                TCe <- data.frame(dates = io_2d$dates,I = io_2d$counts)
                dx1 <- make_data(TCe)
                
                set.seed(1)
                si <- distcrete("gamma", interval = 1L,
                                shape = 6,
                                #scale = 4.0,
                                w = 0.5)
                proj_1 <- project(subset(io_1, to = max(io_1$dates-5)), 
                                  R = last(dx1$mean), si = si, n_days = 5)
                proj_2 <- as.data.frame(proj_1)
                proj_2$R.mean <- rowMeans(proj_2[,c(2:101)],na.rm = TRUE)
                proj_3 <- cbind(proj_2,t(apply(proj_2[,-c(1,101)],1,quantile, probs = c(0.025, 0.5, 0.975),
                                               na.rm = TRUE)))
                dta <- data.frame(dates = as.Date(c((max(io_1$dates)-4):(max(io_1$dates))),origin = "1970-01-01"),
                                  Mean = c(cumsum((proj_3$R.mean))+c(last(io_2cum$counts))),
                                  lower = c(cumsum((proj_3$`2.5%`))+c(last(io_2cum$counts))),
                                  upper = c(cumsum((proj_3$`97.5%`))+c(last(io_2cum$counts))))
                
                #plot epi curve and prediction window
                io_1cum_df <- as.data.frame(subset(io_1cum, 
                                                   from = min(io_1$dates), 
                                                   to = max(io_1$dates)-5))
                
                t1 <- ggplot() +
                    geom_line(data=io_1cum_df, 
                              aes(x = dates, y = counts,group = 1,
                                  text = paste("Date:",dates,"<br>count:",counts)),
                              size = 1.5, color = "#ffb923") +
                    geom_point(data=io_1cum_df, 
                               aes(x = dates, y = counts,group = 1,
                                   text = paste("Date:",dates,"<br>count:",counts)),
                               size = 1.5, color = "#ffb923") +
                    
                    geom_point(data = dta,
                               aes(x = dates, y = Mean, 
                                   text = paste("Date:",dates,"<br>Adjusted count:",Mean,
                                                "<br>LowerCI:",round(lower,2),"<br>UpperCI:",round(upper,2))),
                               color = "gray70", size = 1.5) +
                  
                    geom_ribbon(data = projs, 
                                aes(x = date, ymin = lwr, ymax = upr), 
                                alpha = 0.20, fill = "red")+
                    
                    scale_x_date(breaks = pretty(dtr$date, n = 10), date_labels = "%d %b",
                                 limits = c(((max_date) %m-% months(input$inSlide)),(max_date+10))) +
                    coord_cartesian(ylim = c(0, max(io_1cum_df$counts)*1.7)) +
                    geom_line(aes(date, fit, group = 1,
                                  text = paste("Date:",date,"<br>Projected count:",round(fit,2),
                                               "<br>LowerCI:",round(lwr,2),"<br>UpperCI:",round(upr,2))), 
                              projs,linetype="dashed", size = 0.5, color = "red") +
                    theme_minimal() +
                    labs(title = "COVID-19 Cumulative daily incidence by onset date, Alaska",
                         subtitle = "(log-linear model: short term forecast with 95% confidence band)",
                         caption = "*Indicated by gray dots: Illnesses that began during this period may not yet be reported resulting in incomplete data and excluded from model fit.") +
                    theme(legend.justification = c(0, 1), 
                          legend.position = c(0, 0.95),
                          axis.title.x=element_blank())
                
                
                ### plotly version
                
                ggplotly(t1, tooltip = c("text")) %>%
                    layout(title = list(text = paste0('COVID-19 Cumulative Epidemic Curve,',' ', global_epi_data()$locate,
                                                      '<br>',
                                                      '<sup>',
                                                      'Gray bars: preliminary adjusted counts estimated from incomplete data due to reporting lag', 
                                                      '<br>',
                                                      '<sup>',
                                                      '(log-linear model: short term forecast with 95% confidence band)',
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
          io_2cum <- subset(io_1cum, from = max(io_1$dates-14), to = max(io_1$dates-4))
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


### R Effective ####
    
    r_data <- reactive({
      
      if(input$inSelectReg1 == "Statewide") {
        dat1 <- subset(dat1, occurrence_y_n == "Y")
      } else {
        dat1 <- dat1 %>% filter(bhr == input$inSelectReg1 & occurrence_y_n == "Y")
      }
      
      locate <- input$inSelectReg1
      
      if(input$inRes_select == "Resident & non-Resident"){
        dat1s <- dat1
      } else {
        dat1s <- dat1 %>% filter(resident_y_n == "Y")
      }
      
      
      # incidence objects
      io_1a <- incidence(dat1s$OnsetDate_imp, last_date = max(dat1s$report_date))
      ############
      
      #subset and create incidence object on the 14 days before truncation.
      io_2 <- subset(io_1a, from = max(io_1a$dates-18), to = max(io_1a$dates-5))
      
      #Fit log linear model on the 14 day subset
      i_fit <- incidence::fit(io_2)
      
      ## Use model to predict daily counts for subsetted week.
      
      dtp <- as.Date(max(io_2$dates+1):max(io_2$dates+5),origin = "1970-01-01")
      dtpf <- data.frame(dates.x = (max(i_fit$model$model$dates.x)+1):(max(i_fit$model$model$dates.x)+5))   
      
      extFit <- predict(i_fit$model, dtpf, interval = "confidence", level = 0.95)
      y <- exp(extFit)
      
      projs <- data.frame(y, date = as.Date(dtp, origin = "1970-01-01"))
      
      ## Create dataset with fitted values substituted for the past 5-days.
      io_1b <- incidence(dat1s$OnsetDate_imp, last_date = max_date-5)
      dts <- c(io_1b$dates, projs$date)
      cnts <- c(io_1b$counts, projs$fit)
      cnts_L <- c(io_1b$counts, projs$lwr)
      cnts_U <- c(io_1b$counts, projs$upr)
      
      #Create dataframes for Reff estimates
      
      TCe <- data.frame(dates = dts,I = cnts)
      TCeL <- data.frame(dates = dts,I = cnts_L)
      TCeU <- data.frame(dates = dts,I = cnts_U)
      
      
      make_data <- function(x) {
        t_start_e <- seq(2, length(x[["dates"]])-9)
        t_end_e   <- t_start_e + 9
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
        date_e <- (si7$dates[-c(1:10)])
        
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
        labs(title= paste("COVID-19 Time-varying reproductive number (Rt),",r_data()$locate))  +
        scale_x_date(date_breaks = "1 month",date_labels = "%b %Y") +
        coord_cartesian(ylim = c(0, 3)) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      ggplotly(a, tooltip = c("text"))
      
      
    })
    
    output$r.table <- renderUI({
      
      #list(dx1 = dx1, dxLs = dxLs, dxUs = dxUs, dx3 = dx3 )  
      
      dx1 <- r_data()$dx1
      dx3 <- r_data()$dx3
      
      st1 <- paste0("Current Rt estimate as of",' ', last(dx1$date),":",' ', 
                    round(last(dx1$mean),2), ' ', '(',last(dx3$lower), ' ', '-', ' ', last(dx3$upper),')')
      
      mn <- mean(dx1$mean[c((max(nrow(dx1)-9)):(max(nrow(dx1))))])
      st2 <- paste0("Average Rt over past 10 days:", ' ',round(mn,3))
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
   
### 7-Day rate Alert level ####
    observe({
      if(input$geo2 == "Statewide") {
        updateSelectInput(session, "rgn2", "Select Region:",
                          choices = "Statewide") 
      } else {
      Factors =  c("None Selected",cnty_list)
      updateSelectInput(session, "rgn2", "Select Region:",
                        choices = c(Factors))
      }
    })     
    
  svn_day_data <- reactive({
      
      if(input$geo2 == "Statewide") { 
        
        temp2a <- statewide_7d_rate(dat1)
        vlu <- as.name(input$geo2)
        
      } else {
        
        if(input$rgn2 == "None Selected"){
          temp2a <- (NULL)
          
        } else {

        tmp_dat <- b_c_crosswalk %>% filter(County_Name %in% c(input$rgn2))  
        tdat1 <- dat1 %>% filter(borough_code %in% c(tmp_dat$County_Code)) 
        temp2a <- borough_7d_rate(tdat1)
        
        vlu <- as.name(input$rgn2)
        }
      }
    
      list(temp2a = temp2a, vlu = vlu)
    
      }) 
    
  output$rate.plot2 <- renderPlotly({
      
      temp2a <- svn_day_data()$temp2a
      
      alert_df <- data.frame(xmin = min(temp2a$dates),
                             xmax = max(temp2a$dates),
                             ymin = c(0,10,50,100),
                             ymax = c(9.99,49.99,99.99,max(temp2a$upperCI)+5),
                             Alert_Level = as.factor(c("Low","Moderate","Substantial","High")))
      alert_df$Alert_Level <- factor(alert_df$Alert_Level, levels = c("High","Substantial","Moderate","Low"))
      
      a <- ggplot() +
        geom_rect(data = alert_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                       fill = Alert_Level,
                                       text = paste("Alert Level:",Alert_Level)))+
        scale_fill_manual(values = c("#ff0707","#ff753a","#fff715","#238dff")) +
        geom_ribbon(data = temp2a, aes(x = dates, ymin = lowerCI, ymax = upperCI),
                    alpha = 0.20, fill = "black") +
        geom_line(data = temp2a, aes(x = dates, y = rate, group = 1,
                                     text = paste("Date:",dates,"<br>Rate:",rate,
                                                  "<br>LowerCI:",lowerCI,
                                                  "<br>UpperCI:",upperCI)),
                  colour = "black") +
        ylab("rate per 100k residents")+
        xlab("")+
        #ylim(0,max(temp2a$rate)+10)+
        coord_cartesian(ylim = c(0, max(temp2a$upperCI)+5)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                     limits = c(min(temp2a$dates), max(temp2a$dates))) +
        labs(title = paste0("7-Day COVID-19 case rate - ",svn_day_data()$vlu)) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 30,hjust = 1)) 


      ggplotly(a, tooltip = c("text"))
      
  })
  
  output$downloadRateData2 <- downloadHandler(
    
    filename = function() {
      
      if(input$rgn2 == "None Selected") {
        NULL
      } else {
        paste(input$rgn2, ".csv", sep = "")
      }
    },
    content = function(file) {
      if(input$rgn2 == "None Selected") {
        NULL
      } else {
        write.csv(svn_day_data()$temp2a, file, row.names = FALSE)
      }
    }
  ) 
  
  output$AlertLevel.box2 <- renderValueBox({
    if(input$rgn2 == "None Selected") {
      valueBox(NULL,subtitle = "Please select a region")
    } else {
      
      current.rt <- last(svn_day_data()$temp2a$rate)
      a_level <- ifelse(current.rt >= 100, "Current Alert Level: High",
                        ifelse(current.rt < 100 & current.rt >= 50, "Current Alert Level: Substantial",
                               ifelse(current.rt < 50 & current.rt >= 10, "Current Alert Level: Intermediate",       
                                      ifelse(current.rt < 5 ,"Current Alert Level: Low"))))
      valueBox(a_level,subtitle = paste0(current.rt," ","per 100,000 population"))
    }
  })
  observeEvent(input$Alert_explain2, {
    if(input$rgn2 == "None Selected") {
      sendSweetAlert(
        session = session,
        title = "What does this alert level mean?",
        text = HTML("<p>Select a Region</p>", '<br>','<br>',
                    "<p>To learn more about the NEW Alaska COVID-19 Alert Levels please visit:
                      <a href='http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx'>http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx</a>.</p>"
                    
        ),
        html = TRUE,
        type = NULL
      )
    } else {
      current.rt <- last(svn_day_data()$temp2a$rate)
      a_level_I <- ifelse(current.rt >= 100, "High Alert Level (>100 cases per 100,000 residents over past 7-days)",
                          ifelse(current.rt < 100 & current.rt >= 50, "Substantial Alert Level (50-99 cases per 100,000 residents)",
                                 ifelse(current.rt < 50 & current.rt >= 10, "Moderate Alert Level (10-49 cases per 100,000 residents)",
                                        ifelse(current.rt < 10 ,"Low Alert Level (<10 cases per 100,000 residents)"))))
      
      sendSweetAlert(
        session = session,
        title = "What does this alert level mean?",
        text = HTML(a_level_I, '<br>','<br>',
                    "<p>To learn more about the NEW Alaska COVID-19 Alert Levels please visit:
                      <a href='http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx'>http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx</a>.</p>"
                    
        ),
        html = TRUE,
        type = NULL
      )
    }
  }) 
  
  observeEvent(input$RateC_explain2, {
    sendSweetAlert(
      session = session,
      title = "7-Day COVID-19 case rate",
      text = HTML("<p>In August 2021, DHSS switched from a three-level system to a four-level 
                     system to better align with CDC's levels. The per-capita rate uses the census population 
                     estimates during 2020 for resident cases.</p>
                     <p>This estimate can be used to monitor the general trajectory of disease 
                     spread and enables comparison between areas with different population 
                     sizes.</p>
                      <p>To learn more about the NEW Alaska COVID-19 Alert Levels please visit:
                      <a href='http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx'>http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx</a>.</p>"
                  
      ),
      html = TRUE,
      type = NULL
    )
  })    
    
}
