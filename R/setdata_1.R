# Function for reading in and manipulating the Onset date data.

setdata_1 <- function(ds2){

  dat1 <- ds2
  
  dat1$ReportDate <- as.Date(as.POSIXct(dat$ReportDate/1000, 
                                        origin = "1970-01-01", 
                                        tz = "America/Anchorage"), tz = "America/Anchorage")
  
#Simple simulation for estimating onset date
  set.seed(2020)
  dist <- round(rtgamma(100000, shape =  6, truncation = 14))
  
  set.seed(19)
  
  dat1$duration_imp1 <- sample(dist, nrow(dat1))
  
  dat1$OnsetDate_imp <- as.Date(dat1$ReportDate - dat1$duration_imp1, origin = "1970-01-01")
  
return(dat1)
}