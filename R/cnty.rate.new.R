### Function for calculating 7 - day case rate by census area ####

#### Function Code ####
borough_7d_rate <- function(dataset){
  
  dat1 <- dataset
  
  #dat1 <- dat2
  dat1_res <- dat1 %>% filter(resident_y_n == "Y")
  
  ar_data <- merge(dat1_res, b_c_crosswalk[,c(2,7)],by.x = "borough_code", by.y = "County_Code", all.x = T)
  ar_data <- ar_data %>% filter(borough != "Unknown") # sometimes coding errors are missed
  
  
  
  #Create incidence object  
  ibr1 <- incidence(as.Date(ar_data$report_date), last_date = max(ar_data$report_date), groups = ar_data$borough_code)
  ibr2 <- as.data.frame (ibr1)
  ibr3 <- pivot_longer(ibr2, -dates, names_to = "Region", values_to = "Cases")
  
  #Denominator
  
  b_c_crosswalk_s <- b_c_crosswalk %>% filter(County_Name != "Unknown" & County_Name != "Out of State")
  
  popboro <- b_c_crosswalk %>%
    group_by(County_Code) %>%
    summarise(count = last(county_pop))
  
  
  #rate last 7 days
  d2 <- ibr3 %>%
    arrange(Region,dates) %>%
    group_by(Region) %>%
    mutate(SevenDayCnt = stats::filter(Cases,rep(1,7), sides = 1))
  
  d2$SevenDayCnt[is.na(d2$SevenDayCnt)] <- 0
  
  
  d3 <- merge(d2,popboro, by.x = "Region", by.y = "County_Code", all.x = T)
  
  d4 <- d3 %>%
    arrange(Region,dates)
  
#calculate rates and CI
  d4$rate <- round(((pois.daly(d4$SevenDayCnt, d4$count)$rate)*100000),1)
  d4$lowerCI <- round(((pois.daly(d4$SevenDayCnt, d4$count)$lower)*100000),1)
  d4$upperCI <- round(((pois.daly(d4$SevenDayCnt, d4$count)$upper)*100000),1)
  
  d4 <- merge(d4, b_c_crosswalk_s[,c(1,2)], by.x = "Region", 
              by.y = "County_Code", all.x = TRUE)

  #set dataframe
  boro <- data.frame(dates = d4$dates, seven_day_count = d4$SevenDayCnt, 
                    rate = d4$rate, lowerCI = d4$lowerCI, upperCI = d4$upperCI,
                    Area = "Census Area", Region = d4$County_Name, population = d4$count)
  
  
  return(boro)
}

