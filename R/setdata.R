# Function for reading in and manipulating the Onset date data.

setdata <- function(url){

### Modified the code as a temp fix due to the delay in the geojson file
da <- jsonlite::fromJSON(url)
base::class(da)

# set data imported data as an object update to attributes
dat1 <- tibble::as_tibble(da$features$properties)

dat1$OnsetDate <- as.Date(substr(dat1$OnsetDate,1,10))
  
#dat1$OnsetDate <- as.Date(as.POSIXct(dat1$OnsetDate/1000, origin = "1970-01-01", tz = "America/Anchorage"))
  
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