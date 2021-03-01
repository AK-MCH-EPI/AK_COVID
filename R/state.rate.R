### Organize Statewide data for rates

statewide_av_rates <- function(dataset, res = T){

dat1_res <- dataset
  
#Create incidence object  
io_1j <- incidence(dat1_res$ReportDate, last_date = max_date)
i2 <- as.data.frame (io_1j)

#Denominator

b_c_crosswalk <- b_c_crosswalk %>% filter(BHR_Name != "Unknown")

if(res == T){
sumpop <- sum(b_c_crosswalk$county_pop, na.rm = TRUE)
}else{
sumpop <- (sum(b_c_crosswalk$county_pop, na.rm = TRUE)) + (sum(b_c_crosswalk$influx_pop, na.rm = TRUE))
}

#average daily counts 7 day average
i3 <- i2 %>%
  arrange(dates) %>%
  mutate(daily_average = rollmean(counts, 7, na.pad = T, align = "right"),
         window = "7 day window")
i3$daily_average[is.na(i3$daily_average)] <- 0
#calculate rates and CI
i3$rate <- round(((pois.daly(i3$daily_average, sumpop)$rate)*100000),2)
i3$lowerCI <- round(((pois.daly(i3$daily_average, sumpop)$lower)*100000),2)
i3$upperCI <- round(((pois.daly(i3$daily_average, sumpop)$upper)*100000),2)
#average daily counts 14 day average
i4 <- i2 %>%
  arrange(dates) %>%
  mutate(daily_average = rollmean(counts, 14, na.pad = T, align = "right"),
         window = "14 day window")
i4$daily_average[is.na(i4$daily_average)] <- 0
#calculate rates and CI
i4$rate <- round(((pois.daly(i4$daily_average, sumpop)$rate)*100000),2)
i4$lowerCI <- round(((pois.daly(i4$daily_average, sumpop)$lower)*100000),2)
i4$upperCI <- round(((pois.daly(i4$daily_average, sumpop)$upper)*100000),2)  
#combine data
i5 <- rbind(i3,i4)
#crate filter
i6 <- data.frame(i5, Area = "Statewide",
                 Region = "Statewide", population = sumpop)
i6 <- i6[,c(1,3:10)]
#set dataframe
statewide <- data.frame(dates = i6$dates, daily_average = i6$daily_average, window = i6$window,
                        rate = i6$rate, lowerCI = i6$lowerCI, upperCI = i6$upperCI,
                        Area = "Statewide", Region = i6$Region, population = i6$population)

return(statewide)
}
