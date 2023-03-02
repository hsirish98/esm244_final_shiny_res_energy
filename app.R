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

source("tab2_data_wrangle.R")

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
                        
                        
                        mainPanel("Here will go regions of the US filtered by Fuel Use")
                      )
             ), 
             tabPanel("Electricity Grid by End Use by Region",
                      sidebarLayout(
                        sidebarPanel("Different Regions of the U.S. use Different Combinations of Fuel Types.",
                                     radioButtons(inputId = "pick_place",
                                                  label = "Choose Region:",
                                                  choices = c("All",unique(fuel_use_tidy$census_region)),
                                     ) , "This Varies by End Use",
                                     radioButtons(inputId = "pick_use",
                                                  label = "Choose End Use:",
                                                  choices = unique(fuel_use$end_use))
                                     # end radioButtons
                        ), # end sidebarPanel
                        
                        
                        mainPanel(plotOutput("tab2_plot"))
                      
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
             tabPanel("Carpool to UCSB?",
                      sidebarLayout(
                        sidebarPanel("Take a  Road Trip and Track Your Energy Use and CO2 Emissions",
                                     radioButtons(inputId = "pick_car",
                                                  label = "Choose your Vehicle:",
                                                  choices = c("Battery Electric Vehicle", "Traditional Hybrid", "Plug-in Hybrid", 
                                                              "2 Door ICV", "4 Door ICV", "Pickup Truck", "I Walk!", "I Bike!")
                                     ),
                                     selectInput(inputId = "state",
                                                 label = "Choose State",
                                                 choices = state.name),
                                     sliderInput("integer", "Number of Miles you live from School:",
                                                 min = 0, max = 20, value = 20)
                        ),
                        mainPanel("CO2 Emissions and Energy Use Will be Calculated here!"),
                      )
                      
             )
  )
)

##end UI

server <- function(input, output) {
  
  
  tab2_reactive <- reactive({
    if(input$pick_place=="All"){
      fuel_use_tot %>%
        filter(end_use %in% input$pick_use)
    } else{
    
    fuel_use_tidy %>%
      filter(census_region %in% input$pick_place)%>%
      filter(end_use %in% input$pick_use)
    }
  })
  
  output$tab2_plot <- renderPlot(
    if(input$pick_place=="All"){
      ggplot(data = tab2_reactive(), aes(x=fuel, y=btu, fill=census_region))+
        geom_col()+
        scale_fill_brewer(palette = "Set1")+
        labs(x=input$pick_use, y="Fuel (Btu)", title=input$pick_place, fill="Sub Region") +
        ylim(0,800)+
        theme_minimal()
    } else{
    
    ggplot(data = tab2_reactive(), aes(x=fuel, y=btu, fill=sub_region))+
      geom_col()+
      scale_fill_brewer(palette = "Set2")+
      labs(x=input$pick_use, y="Fuel (Btu)", title=input$pick_place, fill="Sub Region") +
      ylim(0,800)+
      theme_minimal()

      }
  )


}

shinyApp(ui = ui, server = server)













