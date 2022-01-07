### Function for calculating 7 - day case rate statewide ####

#### Function Code ####

statewide_7d_rate <- function(dataset){
  
  dat1 <- dataset

  dat1_res <- dat1 %>% filter(resident_y_n == "Y")
  
  #Create incidence object  
  io_1j <- incidence(dat1_res$report_date, last_date = max_date)
  i2 <- as.data.frame (io_1j)
  
  #Denominator
  
  b_c_crosswalk_s <- b_c_crosswalk %>% filter(County_Name != "Unknown" & County_Name != "Out of State")
  
    sumpop <- sum(b_c_crosswalk_s$county_pop, na.rm = TRUE)

  
  #rate last 7 days
  i3 <- i2 %>%
    arrange(dates) %>%
    mutate(SevenDayCnt = stats::filter(counts,rep(1,7), sides = 1))

  i3$SevenDayCnt[is.na(i3$SevenDayCnt)] <- 0
  #calculate rates and CI
  i3$rate <- round(((pois.daly(i3$SevenDayCnt, sumpop)$rate)*100000),1)
  i3$lowerCI <- round(((pois.daly(i3$SevenDayCnt, sumpop)$lower)*100000),1)
  i3$upperCI <- round(((pois.daly(i3$SevenDayCnt, sumpop)$upper)*100000),1)
  
  #create filter
  i4 <- data.frame(i3, Area = "Statewide",
                   Region = "Statewide", population = sumpop)
  i4 <- i4[,c(1,3:9)]
  #set dataframe
  statewide <- data.frame(dates = i4$dates, seven_day_count = i4$SevenDayCnt,
                          rate = i4$rate, lowerCI = i4$lowerCI, upperCI = i4$upperCI,
                          area = i4$Area, region = i4$Region, population = i4$population)
  
  return(statewide)
}


