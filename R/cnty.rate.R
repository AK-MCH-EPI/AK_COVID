### Organize census area county (cnty) data for rates

cnty_av_rates <- function(dataset, res = T){
  
  dat1_res <- dataset
  
  ar_data <- merge(dat1_res, b_c_crosswalk[,c(2,7)],by.x = "Borough_Code", by.y = "County_Code", all.x = T)
  ar_data <- ar_data %>% filter(Borough != "Unknown")
  
   icr1 <- incidence(as.Date(ar_data$Report_Date), last_date = max(ar_data$Report_Date), groups = ar_data$Borough)
   icr2 <- as.data.frame (icr1)
   icr3 <- pivot_longer(icr2, -dates, names_to = "Region", values_to = "Cases")
  
   #7 day window
   c2 <- icr3 %>%
     group_by(Region) %>%
     mutate(roll_mean = rollmean(Cases, 7, na.pad = T, align = "right"),
            window = "7 day window")
   c2$roll_mean[is.na(c2$roll_mean)] <- 0
  
   #population totals for rates
   popCensus <- b_c_crosswalk %>%
     group_by(County_Name) %>%
     summarise(count = sum(county_pop))
   
   b_c_crosswalk$tot <- b_c_crosswalk$county_pop + b_c_crosswalk$influx_pop
   
   if(res == T){
     popBHR <- b_c_crosswalk %>%
       group_by(County_Name) %>%
       summarise(count = last(county_pop))
   }else{
     popBHR <- b_c_crosswalk %>%
       group_by(County_Name) %>%
       summarise(count = sum(tot, na.rm =T))
   }
   
   
   c3 <- merge(c2,popCensus, by.x = "Region", by.y = "County_Name", all.x = T)
  
   c4 <- c3 %>%
     arrange(Region,dates)
  
   c4$rate <- round(((pois.daly(c4$roll_mean, c4$count)$rate)*100000),2)
   c4$lowerCI <- round(((pois.daly(c4$roll_mean, c4$count)$lower)*100000),2)
   c4$upperCI <- round(((pois.daly(c4$roll_mean, c4$count)$upper)*100000),2)
  
   #14 day window
   c5 <- icr3 %>%
     group_by(Region) %>%
     mutate(roll_mean = rollmean(Cases, 14, na.pad = T, align = "right"),
            window = "14 day window")
   c5$roll_mean[is.na(c5$roll_mean)] <- 0
  
   #population totals for rates
  
   c6 <- merge(c5,popCensus, by.x = "Region", by.y = "County_Name", all.x = T)
  
   c7 <- c6 %>%
     arrange(Region,dates)
  
   c7$rate <- round(((pois.daly(c7$roll_mean, c7$count)$rate)*100000),2)
   c7$lowerCI <- round(((pois.daly(c7$roll_mean, c7$count)$lower)*100000),2)
   c7$upperCI <- round(((pois.daly(c7$roll_mean, c7$count)$upper)*100000),2)
  
   c8 <- rbind(c4,c7)
  
   Borough <- data.frame(dates = c8$dates, daily_average = c8$roll_mean, window = c8$window,
                        rate = c8$rate, lowerCI = c8$lowerCI, upperCI = c8$upperCI,
                        Area = "County", Region = c8$Region, population = c8$count)
  return(Borough)

}
