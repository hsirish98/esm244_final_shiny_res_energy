library(shiny)
library(tidyverse)
library(palmerpenguins)
library(bslib)
data(state)

my_theme <- bs_theme(
  bg = "#B2D3C2",
  fg = "black",
  primary = "#028A0F",
  base_font = font_google("Crimson Pro")
)

ui <- fluidPage(
  theme=my_theme,
  navbarPage("Energy Modeling",
             tabPanel("Dashboard: Total Residential Energy Consumption (1997-2020)",
                      sidebarLayout(
                        sidebarPanel("Choose End Use (or Total Energy Consumption)",
                                     selectInput(inputId = "year",
                                                 label = "Choose Consumption End Use:",
                                                 choices = c("Total Consumption", "Space Heating", 
                                                             "Air Conditioning", "Water Heating", 
                                                             "Refrigeration", "Other Appliances and Lighting")),
                                     radioButtons(inputId = "pick_region",
                                                  label = "Choose Census Region:",
                                                  choices = c("Entire U.S.", "Northeast", 
                                                              "Midwest", "South", "West", "Four Most Populated Sates"))
                                     # end selectInput
                        ), # end sidebarPanel
                        
                        
                        mainPanel("Here will be graph (consumption 1997-2020)")
                      )
             ), 
             tabPanel("Electricity Grid by End Use by Region",
                      sidebarLayout(
                        sidebarPanel("Different Regions of the U.S. use Different Combinations of Fuel Types.",
                                     radioButtons(inputId = "pick_fuel",
                                                        label = "Choose Fuel Type:",
                                                        choices = c("Natural Gas", "Electricity", 
                                                                    "Propane", "Fuel Oil/Kerosene")
                                     ) , "This Varies by End Use",
                                     radioButtons(inputId = "pick_use",
                                                  label = "Choose End Use:",
                                                  choices = c("Space Heating", "Water Heating",
                                                              "Air Conditioning", "Refridgeration", "Other and Lighting") )
                          # end radioButtons
                        ), # end sidebarPanel
            
                        
                        mainPanel("Here will go regions of the US filtered by Fuel Use")
                        )
                        
                      ),
                        
             tabPanel("Energy Use By Appliance",
                      sidebarLayout(
                        sidebarPanel("Choose Appliance Type",
                                     checkboxGroupInput(inputId = "pick_appliance",
                                                        label = "Choose Appliance(s):",
                                                        choices = c("Space Heating", "Air Conditioning",
                                                                    "Water Heating", "Clothes Wasing",
                                                                    "Clothes Drying", "Lighting", "Refrigeration",
                                                                    "Cooking", "Dishwasher", "TVs", "Pool Pumps", 
                                                                    "Hot Tub Pumps and Heaters"
                                                                    ))
                                     ), # end checkboxGroupInput
                      
                         # end sidebarPanel
                        
                        
                        mainPanel("Here will go the graphs of the relative uses of energy by each appliance for each census region!")
                      )
                        
                      ),
             tabPanel("Road Trip Simulator",
                      sidebarLayout(
                        sidebarPanel("Take a  Road Trip and Track Your Energy Use and CO2 Emissions",
                                     radioButtons(inputId = "pick_car",
                                                  label = "Choose your Vehicle:",
                                                  choices = c("Battery Electric Vehicle", "Traditional Hybrid", "Plug-in Hybrid", 
                                                              "2 Door ICV", "4 Door ICV", "Pickup Truck")
                                     ),
                                     selectInput(inputId = "state",
                                                 label = "Choose State",
                                                 choices = state.name),
                                     sliderInput("integer", "Number of Miles:",
                                                 min = 0, max = 500, value = 500)
                                     ),
                        mainPanel("CO2 Emissions and Energy Use Will be Calculated here!"),
                                     )
             
             )
  )
)
  
 ##end UI

server <- function(input, output) {}

shinyApp(ui = ui, server = server)