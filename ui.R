#
# Alaska Rshiny application 2.1
#
# Developed by Jared Parrish, PhD - Alaska Division of Public Health
# 
# Copyright (C) 2020, State of Alaska, Division of Public Health

#ui<- function(req) {

ui <- fluidPage(
    add_busy_spinner(spin = "circle",color = "springgreen", margins = c(300,600),
                     position = "top-left", height = "75px", width = "75px"),
    ##Google Analytics
    #tags$head(includeHTML(("GAnalytics.html"))),
    
    navbarPage(
        " ",
        
        tabPanel("Projected Epidemic Curve",
          sidebarPanel(
                 selectInput("inSelectReg",
                             label = "Select Region",
                             choices = c("Statewide",BHR_list),
                             selected = "Statewide"),
                 helpText(HTML('<p><b>CAUTION:</b>
                                Areas with a small number of cases should interpret
                                the projections with caution due to the model having
                                a limted number of data points to estimate from.</p>')),
                 selectInput("inSelectRes", 
                             label = "Select case type",
                             choices = c("Resident", "Resident & non-Resident"),
                             selected = "Resident"),
                 checkboxInput("inCheckbox",
                               "Include resident cases occuring out of state",
                               TRUE),
                 textOutput("value_ex"),
                 br(),
                 sliderInput("inSlide", "Number of prior months to display:",
                             min = 1, max = mnths_dsp,
                             value = 9
                 ),
                 checkboxInput("cumcntCheckbox",
                               "Change to cumulative daily case count",
                               FALSE),
                 br(),
                  helpText(HTML('<p>Last Updated: 2022-01-07</p>')),
                   width=3),
        mainPanel(
            tabsetPanel(
                tabPanel("Plots", 
                         br(),
                         plotlyOutput("epi.plot"),  
                         br(),
                         h4("Projection Statistics"),
                         htmlOutput("epi.table"),
                         br(),
                         actionButton("Epi_explain", "What do these numbers mean?"),
                ),
                tabPanel("Interpretation",
                         helpText("This graph represents the daily COVID-19 case count in Alaska (yellow).",
                                  "Simulated date of symptom onset was used. Gray bars represent data from the most recent",
                                  "5 days, which are not included in the analysis due to incomplete data/delay in reporting.",
                                  "The blue dotted line represents the predicted daily case trajectory (assumed exponential),",
                                  "with the gray band representing the 95% Confidence Interval (estimate range) of the projection.",
                                  "For a complete description of the methods please view the Methods tab.",
                                  br(),
                                  br(),
                                  "Projection statistics:",
                                  br(),
                                  "The modeled exponential trajectory can be one of growth (getting bigger) or decay (getting smaller).",
                                  "If in growth, the doubling time represents the projected amount of time it will take for the counts to",
                                  "double in size. This assumes a constant growth rate (r) from the observed data under an exponential trajectory.",
                                  "If in decline, the halving time represents the projected amount of time it will take for the counts to reduce",
                                  "in half. This also assumes a constant halving rate (r) from the observed data under an exponential trajectory.",
                                  "The growth rate, is the projected percent increase/decrease in daily cases.",
                                  br(),
                                  br(),
                                  "Confidence intervals (CI) provide an indication of precision of an estimated parameter. Specifically, a 95% CI",
                                  "means that if we sampled the same population an infinite number of times, the generated CI's for each sample",
                                  "would contain the true population parameter in 95% of the cases, assuming no systematic (bias) error.", 
                                  "Generally speaking however, we can interpret the CI as a range around a point estimate",
                                  "within which the true value is likely to lie with a specified degree of probability, assuming there is no",
                                  "systematic error (bias or confounding).", 
                                  "If the sample size is small and subject to more random error, then the estimate will not be as precise, and the",
                                  "confidence interval will be wide, indicating a greater amount of random error. In contrast, with a large",
                                  "sample size, the width of the confidence interval is narrower, indicating less random error and greater",
                                  "precision. One can, therefore, use the width of confidence intervals to indicate the amount of random error",
                                  "in an estimate.")),
                tabPanel("Methods",
                         includeMarkdown("methods.Rmd")
                )
             ))),
        
        tabPanel("Reproductive number",
                 sidebarPanel(
                     selectInput("inSelectReg1",
                                 label = "Select Region",
                                 choices = c("Statewide",BHR_list),
                                 selected = "Statewide"),
                     helpText(HTML('<p><b>CAUTION:</b>
                                   Areas with only a few number of cases have extreme
                                       variability, resulting in high fluctuations that are
                                       heavily influenced by clustered outbreaks.</p>')),
                     radioButtons("inRes_select", "Select case type",
                                  c("Resident","Resident & non-Resident")),
                     helpText(HTML('<p> <i>Resident cases occurring outside Alaska excluded.</i> </p>')),
                     sliderInput("inSlide1", "Number of prior months to display:",
                                 min = 1, max = mnths_dsp,
                                 value = 9
                     ),
                     actionButton("Rt_explain", "Click to learn about Rt"),
                     br(),
                     br(),
                     helpText(HTML('<p> <b>Reference:</b> </p>
                                   <p> Cori, A. et al. A new framework and software to estimate
                                       time-varying reproduction numbers during epidemics (AJE 2013)</p>')),
                     br(),
                     downloadButton("downloadRtData", "Download Data"), 
                     br(),
                     br(),
                     helpText(HTML('<p>Last Updated: 2022-01-07</p>')),
                     width=3),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Plot of Rt", 
                                  br(),
                                  plotlyOutput("r.plot"),
                                  br(),
                                  h4("Current Estimates"),
                                  htmlOutput("r.table")
                         ),
                         tabPanel("Methods",
                                  includeMarkdown("methodsEffectiveR.Rmd")
                         )))),
        
        tabPanel("7-Day Alert Level Trend",
                 sidebarPanel(
                     radioButtons("geo2", "Select Area:",
                                  c("Statewide","Borough")),
                     selectInput("rgn2", "Select Region:",
                                 choices = "", selected = ""),
                     helpText(HTML('<p><b>CAUTION:</b>
                                   Areas with a few number of cases have extreme
                                   variability resulting in unstable rates over time as represented 
                                   by the large confidence bands. In small areas a few cases can
                                   result in large changes.</p>')),
                     actionButton("RateC_explain2", "Learn about this measure"),
                     br(),
                     br(),
                     downloadButton("downloadRateData2", "Download Data"),
                     br(),
                     br(),
                     helpText(HTML('<p><b>Note:</b>
                        This alert level calculation is based on the per capita incidence in the past 7 days among 
			               Alaska RESIDENTS ONLY using date of report.
                        </p>')),
                     width = 3),
                 
                 mainPanel(
                     tags$style(type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }"
                     ),
                     br(),
                     plotlyOutput("rate.plot2"),
                     br(),
                     valueBoxOutput("AlertLevel.box2", width = 20),
                     
                     actionButton("Alert_explain2", "What does this mean?"),
                     br(),
                     br(),
                     
                     HTML("<p> <b>The Alaska DHSS Alert levels are consistent with those developed by the Centers for
				                  Disease Control and Prevention.</b>
                                  To learn more about the NEW Alaska COVID-19 Alert Levels please visit:
                                 <a href='http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx'>http://dhss.alaska.gov/dph/Epi/id/Pages/COVID-19/alertlevels.aspx</a>.</p>"
                     )
                 ))
        
    )
)

            