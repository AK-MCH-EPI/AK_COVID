# Function for reading in and manipulating the Onset date data.

setdata <- function(url){

### Modified the code as a temp fix due to the delay in the geojson file
# da <- jsonlite::fromJSON(url)
# base::class(da)
# #da %>% str()
# 
# # set data imported data as an object update to attributes 
# dat1 <- tibble::as_tibble(da$features$properties) 
# 
# dat1$OnsetDate <- as.Date(substr(dat1$OnsetDate,1,10))

### Temp code ### 
  
## connect to Alaksa data hub through the API: Onset Date Table
  
  url <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-03-01%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-06-30%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  url1 <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-07-01%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-07-15%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  url2 <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-07-16%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-07-31%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  url3 <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-08-01%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-08-15%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  url4 <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-08-16%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-08-31%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  url5 <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-09-01%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-09-15%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  url6 <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-09-16%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-09-30%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  url7 <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-10-01%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-10-15%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  #url8 <- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Onset_Date/FeatureServer/0/query?where=OnsetDate%20%3E%3D%20TIMESTAMP%20'2020-10-16%2000%3A00%3A00'%20AND%20OnsetDate%20%3C%3D%20TIMESTAMP%20'2020-10-31%2000%3A00%3A00'&outFields=*&outSR=4326&f=json"
  
  da <- jsonlite::fromJSON(url)
  da1 <- jsonlite::fromJSON(url1)
  da2 <- jsonlite::fromJSON(url2)
  da3 <- jsonlite::fromJSON(url3)
  da4 <- jsonlite::fromJSON(url4)
  da5 <- jsonlite::fromJSON(url5)
  da6 <- jsonlite::fromJSON(url6)
  da7 <- jsonlite::fromJSON(url7)
  da8 <- jsonlite::fromJSON(url8)
  
  #base::class(da)
  #da %>% str()
  
  # set data imported data as an object update to attributes 
  d1 <- tibble::as_tibble((da$features$attributes))
  d2 <- tibble::as_tibble((da1$features$attributes))
  d3 <- tibble::as_tibble((da2$features$attributes))
  d4 <- tibble::as_tibble((da3$features$attributes))
  d5 <- tibble::as_tibble((da4$features$attributes))
  d6 <- tibble::as_tibble((da5$features$attributes))
  d7 <- tibble::as_tibble((da6$features$attributes))
  d8 <- tibble::as_tibble((da7$features$attributes))
  d9 <- tibble::as_tibble((da8$features$attributes))
  dat1 <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
  
  dat1$OnsetDate <- as.Date(as.POSIXct(dat1$OnsetDate/1000, origin = "1970-01-01", tz = "America/Anchorage"))
  
### End Temp code  
  
### Onset date imputation  
# calcuate duration in days.
dat1$ReportDate <- as.Date(with(dat1, ifelse(OnsetDateReplaced == 2, dat1$OnsetDate,
                                             NA)), origin = "1970-01-01")
dat1$duration <- dat1$ReportDate - dat1$OnsetDate


## set distribution for median among most recent 14 days duration for Gamma dist
## had to modify and hard code this due to change in data structure, and assume it is constant.
set.seed(2020)
#dist <- round(rgamma(1000, shape =  sts$Y[3])) 
#use a truncated gamma distribution to ensure that the selected durations are consistant
# with observed data and in a 2-week range and modify shape from 7 to 6.

dist <- round(rtgamma(50000, shape =  6, truncation = 14)) 

#zro <- table(dat1$duration<=0)[2] # isolate duration = 0 cases and impute values
zro <- table(dat1$duration)
set.seed(19) 
smp1 <- sample(dist, zro)
dat1$duration_imp <- ifelse(dat1$duration == 0, smp1, dat1$duration)

dat1$OnsetDate1 <- as.Date(ifelse(is.na(dat1$duration), dat1$OnsetDate,
                                  dat1$OnsetDate - dat1$duration_imp),
                           origin = "1970-01-01")

# type of travel
dat1$type <- with(dat1, ifelse(SpecificAcquisition %in% c("Community","Secondary"), "local",
                               ifelse(SpecificAcquisition == "Travel", "imported",
                                      ifelse(SpecificAcquisition == "Could Not Be Determined", 
                                             "Unknown","Under Investigation"))))
return(dat1)
}