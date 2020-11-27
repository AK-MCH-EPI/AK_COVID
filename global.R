###################################################################################
# "We can lift ourselves out of ignorance, we can find ourselves as creatures
# of excellence and intelligence and skill.”
#                                  ― Richard Bach, Jonathan Livingston Seagull
###################################################################################

# Author: Jared Parrish
# Copyright (C) 2020, State of Alaska, Division of Public Health
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this application and associated documentation files to use 
# without restriction, including without limitation
# the rights to use, copy, modify, merge, and publish with the above restriction:
#   
# The above copyright notice and this permission notice shall be included in
# all copies used, and if modified cited as follows:
#
# Original work Copyright (c) 2020, State of Alaska, Division of Public Health
# Modified work Copyright yyyy, (Organization or name)
# 
# THE APPLICATION IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
#
# Release Date: 2020-08-07

############ Begin Code ############################################################

## Load libraries and set options ####
options(stringsAsFactors = FALSE)
options(scipen=999) # remove scientific notation

library(httr)
library(jsonlite)
library(RCurl)
library(ggplot2)
library(lubridate)
library(incidence)
library(tidyr)
library(dplyr)
library(plotly)
library(EpiEstim)
library(projections)
library(distcrete)
library(heavy)
library(zoo)
library(epitools)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)

## read data and organize for processing ####
sapply(list.files("R", full.names = T), source)

## connect to Alaksa data hub through the API: Onset Date Table this API is having issues
# 
# url <- "https://opendata.arcgis.com/datasets/c1b6c31d09b44c33962570950456feea_0.geojson"
# dat1 <- setdata(url)


### Process using the geoservice: requires 2000k batch loading

# Step 1: set up lists and objects
batch <<- list()
ctpt1 <<- 0
status <<- TRUE

url_t <<- "https://services1.arcgis.com/WzFsmainVTuD5KML/arcgis/rest/services/Report_Date/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson&resultOffset="

while(status == TRUE) {
   if(ctpt1 == 0){
      urlt <<- paste0(url_t,ctpt1)
      t_dat <<- jsonlite::fromJSON(urlt)
      batch <<- append(batch, list(t_dat))
      ctpt1 <<- ctpt1 + nrow(t_dat$features$properties)
      status <<- !is.null(t_dat$properties$exceededTransferLimit)
   } 
   if(ctpt1 > 0){
      urlt <<- paste0(url_t,ctpt1)
      t_dat <<- jsonlite::fromJSON(urlt)
      ctpt1 <<- ctpt1 + nrow(t_dat$features$properties)
      batch <<- append(batch, list(t_dat))
      status <<- !is.null(t_dat$properties$exceededTransferLimit)
   }
}

# Step 2: extract data and bind together

batch1 <<- lapply(batch, function(x) {x$features$properties})
dat <<- (do.call(rbind, batch1))

# dat$ReportDate <<- as.Date(as.POSIXct(dat$ReportDate/1000, 
#                                      origin = "1970-01-01", 
#                                      tz = "America/Anchorage"), tz = '')

# Step 3: Simulate onset date from Report date distribution
dat1 <<- setdata_1(dat)

# set.seed(2020)
# dist <- round(rtgamma(100000, shape =  6, truncation = 14))
# 
# set.seed(19)
# 
# dat$duration_imp1 <- sample(dist, nrow(dat))
# 
# dat$OnsetDate_imp <- as.Date(dat$ReportDate - dat$duration_imp1, origin = "1970-01-01")
# 
# dat1 <- dat

### Set date range
dt <- as.Date(as.Date(min(dat1$ReportDate)):as.Date(max(dat1$ReportDate)),origin = "1970-01-01")

### Set Max date 
max_date <-  max(dat1$ReportDate)

### Set number of months of outbrek
mnths_dsp <- interval((min(dt)), (max_date+10)) %/% months(1)


#### elements ####

# factor drop for BHR #
BHR_list <- as.list(names(table(dat1$BHR_Name)))
BHR_list <- BHR_list[-11]

# factor drop for County #
# cnty_list <- as.list(names(table(dat1$County_Name)))
# cnty_list <- cnty_list[-23]

### Average rate ####

# #read in population data for rate calculations
# 
 load("Borough_County_Crosswalk.Rda")
# 
# #base datasets to work with
# 
 dat1_res <- dat1 %>% filter(Resident == "Y", Occurrence == "Y")
 dat1_resnon <- dat1 %>% filter(Occurrence == "Y")
# 
# ### Organize statewide estimates 
# 
 statewide_R <- statewide_av_rates(dat1_res, res=T)
 statewide_RN <- statewide_av_rates(dat1_resnon,res=F)
# 
# #### Organize BHR data ####
 BHR_R <- bhr_av_rates(dat1_res, res=T)
 BHR_RN <- bhr_av_rates(dat1_resnon,res=F)
# 
# # #### Organize Census data ####
 # CNTY_R <- cnty_av_rates(dat1_res,res=T)
 # CNTY_RN <- cnty_av_rates(dat1_resnon,res=F)
# 
# #### Bind data together ####
# ## avr_data <- rbind(statewide_R, BHR_R, CNTY_R)
# ## avrn_data <- rbind(statewide_RN, BHR_RN, CNTY_RN)
# 
avr_data <- rbind(statewide_R,BHR_R)
avrn_data <- rbind(statewide_RN,BHR_RN)
# 

