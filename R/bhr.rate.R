### Organize Behavioral Health Region (BHR) data for rates

bhr_av_rates <- function(dataset, res = T){
  
  dat1_res <- dataset
  
ar_data <- merge(dat1_res, b_c_crosswalk[,c(2,7)],by.x = "Borough_Code", by.y = "County_Code", all.x = T)
ar_data <- ar_data %>% filter(BHR != "Unknown") # sometimes coding errors are missed

ibr1 <- incidence(as.Date(ar_data$Report_Date), last_date = max(ar_data$Report_Date), groups = ar_data$BHR)
ibr2 <- as.data.frame (ibr1)
ibr3 <- pivot_longer(ibr2, -dates, names_to = "Region", values_to = "Cases")

#7 day window
d2 <- ibr3 %>%
  group_by(Region) %>%
  mutate(roll_mean = rollmean(Cases, 7, na.pad = T, align = "right"),
         window = "7 day window")
d2$roll_mean[is.na(d2$roll_mean)] <- 0

#population totals for rates
# popBHR <- b_c_crosswalk %>%
#   group_by(BHR_Name) %>%
#   summarise(count = last(bhr_pop))

b_c_crosswalk$tot <- b_c_crosswalk$county_pop + b_c_crosswalk$influx_pop

if(res == T){
  popBHR <- b_c_crosswalk %>%
    group_by(BHR_Name) %>%
    summarise(count = last(bhr_pop))
}else{
  popBHR <- b_c_crosswalk %>%
    group_by(BHR_Name) %>%
    summarise(count = sum(tot, na.rm =T))
}

d3 <- merge(d2,popBHR, by.x = "Region", by.y = "BHR_Name", all.x = T)

d4 <- d3 %>%
  arrange(Region,dates)

d4$rate <- round(((pois.daly(d4$roll_mean, d4$count)$rate)*100000),2)
d4$lowerCI <- round(((pois.daly(d4$roll_mean, d4$count)$lower)*100000),2)
d4$upperCI <- round(((pois.daly(d4$roll_mean, d4$count)$upper)*100000),2)

#14 day window
d5 <- ibr3 %>%
  group_by(Region) %>%
  mutate(roll_mean = rollmean(Cases, 14, na.pad = T, align = "right"),
         window = "14 day window")
d5$roll_mean[is.na(d5$roll_mean)] <- 0

#population totals for rates
d6 <- merge(d5,popBHR, by.x = "Region", by.y = "BHR_Name", all.x = T)

d7 <- d6 %>%
  arrange(Region,dates)

d7$rate <- round(((pois.daly(d7$roll_mean, d7$count)$rate)*100000),2)
d7$lowerCI <- round(((pois.daly(d7$roll_mean, d7$count)$lower)*100000),2)
d7$upperCI <- round(((pois.daly(d7$roll_mean, d7$count)$upper)*100000),2)


d8 <- rbind(d4,d7)

BHR <- data.frame(dates = d8$dates, daily_average = d8$roll_mean, window = d8$window,
                  rate = d8$rate, lowerCI = d8$lowerCI, upperCI = d8$upperCI,
                  Area = "Behavioral Health Region", Region = d8$Region, population = d8$count)

return(BHR)
}