###################################################################################
# "We can lift ourselves out of ignorance, we can find ourselves as creatures
# of excellence and intelligence and skill.”
#                                  ― Richard Bach, Jonathan Livingston Seagull
###################################################################################

# Author: Jared Parrish
#
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
library(heavy)
library(zoo)
library(epitools)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

## read data and organize for processing ####
sapply(list.files("R", full.names = T), source)

## connect to Alaksa data hub through the API: Onset Date Table
url <- "https://opendata.arcgis.com/datasets/c1b6c31d09b44c33962570950456feea_0.geojson"

da <- jsonlite::fromJSON(url)
base::class(da)
da %>% str()

# set data imported data as an object update to attributes 
dat1 <- tibble::as_tibble(da$features$properties) 

# Fix dates 
# dat1$OnsetDate <- as.Date(as.POSIXct(dat1$OnsetDate/1000, origin = "1970-01-01", tz = "America/Anchorage"))

dat1$OnsetDate <- as.Date(substr(dat1$OnsetDate,1,10))


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

### Set date range
dt <- as.Date(as.Date(min(dat1$OnsetDate)):as.Date(max(dat1$OnsetDate)),origin = "1970-01-01")

### Set Max date
max_date <- max(dat1$OnsetDate)


#### elements ####

# factor drop for BHR #
BHR_list <- as.list(names(table(dat1$BHR_Name)))
BHR_list <- BHR_list[-11]

# factor drop for County #
cnty_list <- as.list(names(table(dat1$County_Name)))
cnty_list <- cnty_list[-23]

### Average rate ####

#read in population data for rate calculations

load("Borough_County_Crosswalk.Rda")

#base datasets to work with

dat1_res <- dat1 %>% filter(Resident == "Y", Occurrence == "Y")
dat1_resnon <- dat1 %>% filter(Occurrence == "Y")

### Organize statewide estimates 

statewide_R <- statewide_av_rates(dat1_res, res=T)
statewide_RN <- statewide_av_rates(dat1_resnon,res=F)

#### Organize BHR data ####
BHR_R <- bhr_av_rates(dat1_res, res=T)
BHR_RN <- bhr_av_rates(dat1_resnon,res=F)

# #### Organize Census data ####
## CNTY_R <- cnty_av_rates(dat1_res,res=T)
## CNTY_RN <- cnty_av_rates(dat1_resnon,res=F)

#### Bind data together ####
## avr_data <- rbind(statewide_R, BHR_R, CNTY_R)
## avrn_data <- rbind(statewide_RN, BHR_RN, CNTY_RN)

avr_data <- rbind(statewide_R,BHR_R)
avrn_data <- rbind(statewide_RN,BHR_RN)

