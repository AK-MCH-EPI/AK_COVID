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
library(heavy)
library(zoo)
library(epitools)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybusy)

## read data and organize for processing ####
sapply(list.files("R", full.names = T), source)

## connect to Alaksa data hub through the API: Onset Date Table

url <- "https://opendata.arcgis.com/datasets/c1b6c31d09b44c33962570950456feea_0.geojson"
dat1 <- setdata(url)


#Check for updates on the hub every 5min
getdata <- function(url){
   
   dat1 <- reactivePoll(300000,
                        checkFunc = function(){
                          da <- jsonlite::fromJSON(url)
                          nrow(da$features$properties)
                        },
                        valueFunc = function(){
                          setdata(url)
                        })
   output$dataTable < renderTable({dat1()})
   
}
 

### Set date range
dt <- as.Date(as.Date(min(dat1$OnsetDate)):as.Date(max(dat1$OnsetDate)),origin = "1970-01-01")

### Set Max date 
max_date <- max(dat1$OnsetDate)

### Set number of months of outbrek
mnths_dsp <- interval((min(dt)), (max_date+10)) %/% months(1)

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

