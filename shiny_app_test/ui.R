#set up
# Load R packages
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)

library(tidyverse)
library(tidymodels)
library(baguette)
library(recipes)

library(hrbrthemes)
library(scales)
library(leaflet)
library(sf)

#https://towardsdatascience.com/build-your-first-shiny-web-app-in-r-72f9538f9868
#https://shiny.rstudio.com/tutorial/

style_desc_distinct <- read_csv("shiny_app/style_desc_distinct.csv")
grade_desc_distinct <- read_csv("shiny_app/grade_desc_distinct.csv")
condition_desc_distinct <- read_csv("shiny_app/condition_desc_distinct.csv")

ui <- fluidPage(theme = shinytheme("cerulean"),
                title = "Allegheny County Home Sale Price Estimator",
                
                titlePanel(title = "Allegheny County Home Sale Price Estimator"),
                
                actionButton("panel_button", "Toggle Input Panel"),
                
                actionButton("map_button", "Toggle Map"),
                
                fluidPage(
                  
                  conditionalPanel(condition = "input.panel_button % 2 == 0",
                                   column(width = 2,
                                          fluidRow(#"Input row 1",
                                            selectInput(inputId = "style_desc_choice", 
                                                        label = "Style",
                                                        choices = pull(style_desc_distinct, style_desc),
                                                        selectize = TRUE,
                                                        multiple = FALSE),
                                            selectInput(inputId = "grade_desc_choice", 
                                                        label = "Grade",
                                                        choices = pull(grade_desc_distinct, grade_desc),
                                                        multiple = FALSE,
                                                        selected = "Average"),
                                            selectInput(inputId = "condition_desc_choice", 
                                                        label = "Condition",
                                                        choices = pull(condition_desc_distinct, condition_desc),
                                                        multiple = FALSE,
                                                        selected = "Average")),
                                          fluidRow(#"Input row 2",
                                            sliderInput(inputId = "bedrooms_choice",
                                                        label = "Bedrooms",
                                                        min = 1,
                                                        max = 6,
                                                        value = 1),
                                            sliderInput(inputId = "full_baths_choice",
                                                        label = "Full bathrooms",
                                                        min = 1,
                                                        max = 4,
                                                        value = 1),
                                            sliderInput(inputId = "half_baths_choice",
                                                        label = "Half bathrooms",
                                                        min = 0,
                                                        max = 4,
                                                        value = 0)),
                                          fluidRow(#"Input row 3",
                                            sliderInput(inputId = "lot_area_choice",
                                                        label = "Lot Area (sq. ft)",
                                                        #min = pull(lot_area_range_min, lot_area),
                                                        #max = pull(lot_area_range_max, lot_area),
                                                        min = 0,
                                                        max = 40000,
                                                        value = 5000,
                                                        step = 500),
                                            sliderInput(inputId = "finished_living_area_choice",
                                                        label = "Finished Living Area (sq. ft)",
                                                        #min = pull(finished_living_area_min, finished_living_area),
                                                        #max = pull(finished_living_area_max, finished_living_area),
                                                        min = 0,
                                                        max = 10000,
                                                        value = 2000,
                                                        step = 100),
                                            sliderInput(inputId = "year_built_choice",
                                                        label = "Year house was built",
                                                        min = 1870,
                                                        max = 2020,
                                                        value = 1948,
                                                        sep = "")),
                                          fluidRow(#"Input row 4",
                                            selectInput(inputId = "heat_type_choice",
                                                        label = "Heat Source",
                                                        choices = c("Central Heat", "Other", "None"),
                                                        multiple = FALSE),
                                            selectInput(inputId = "ac_flag_choice",
                                                        label = "Air Conditioning",
                                                        choices = c(TRUE, FALSE),
                                                        multiple = FALSE),
                                            selectInput(inputId = "sale_month_choice",
                                                        label = "Month of Sale",
                                                        choices = month.abb,
                                                        selected = lubridate::month(Sys.Date(), label = T),
                                                        multiple = FALSE))
                                   ),
                  ),
                  
                  column(#"main_graph_area", 
                    width = 10,
                    
                    conditionalPanel(condition = "input.map_button % 2 == 0",
                                     fluidRow(#"leaflet map and house styles",
                                       textOutput("leaflet_title"), #investigate height and width arguments
                                       leafletOutput("geo_id_map", height = 350)),
                    ),
                    mainPanel(
                      
                      # Output: Tabset w/ plot, summary, and table ----
                      tabsetPanel(type = "tabs",
                                  tabPanel("Prediction", plotOutput("model_output_graph", width = "800px")),
                                  tabPanel("Summary", verbatimTextOutput("txtout")),
                                  tabPanel("Top house styles", plotOutput("style_desc_bar_graph", width = "800px")),
                                  tabPanel("Grade and Condition", plotOutput("grade_condition_graph", width = "800px" ))
                      )
                    )
                  )
                )
)
