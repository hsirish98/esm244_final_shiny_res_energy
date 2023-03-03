library(shiny)
library(tidyverse)
library(palmerpenguins)
library(bslib)


my_theme <- bs_theme(
  bg = "#B2D3C2",
  fg = "black",
  primary = "#028A0F",
  base_font = font_google("Crimson Pro")
)

source("wrangle_files/tab2_data_wrangle.R")
source("wrangle_files/map_wrangle.R")

ui <- fluidPage(
  theme=my_theme,
  navbarPage("Energy Modeling",
             tabPanel("Overview",
                        mainPanel(strong(p("Welcome to my shiny app!")),
                                    p("The purpose of this shiny app is to take a look at current
                                      trends in residential/personal energy consumption, and 
                                      get a feel for the task it will take to electrify the residential
                                      sector")
                      
                        )
             ),
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
                        
                  
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Overview", plotOutput("fe_plot")),
                            tabPanel("Total by Region",plotOutput("fe_totals"))
                          )
  
                        )
                        
                      )
                      
             ),
             
             tabPanel("Electrification of the States",
                      sidebarLayout(
                        sidebarPanel("See the Percent of Homes in States that are totally Electrified",
                                     sliderInput(inputId = "choose_pct",
                                                  label = "See How many States are totally Electrified",
                                                  min= (min(states_contig_sf$pct_e,na.rm=TRUE)),
                                                 max=(max(states_contig_sf$pct_e,na.rm=TRUE)), 
                                                 value=7)
                                     ),
                                  
                      
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Map",plotOutput("pct_state")),
                            tabPanel("Analysis",textOutput("elec_summary"))
                            
                            
                          )
                        )
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
                      
             )
  )
)

##end UI

server <- function(input, output) {
  
## Tab 1: Overview


  
## Tab 3: Region + End Use + Fuel
  ## Taking Inputs
  fe_reactive <- reactive({
    ## if "ALL" is selected
    if(input$pick_place=="All"){
      fuel_use_tot %>%
        filter(end_use %in% input$pick_use)
    } else{
    
    ## if a census region is selected
    fuel_use_tidy %>%
      filter(census_region %in% input$pick_place)%>%
      filter(end_use %in% input$pick_use)
    }
  })
  

   ##input for bottom graph
  fe_reactive_tot <- reactive({
    ## if "ALL is selected"
    if(input$pick_place=="All"){
      fuel_use_coll_all %>%
        filter(end_use %in% input$pick_use) 
    } else{
      ## if census region is selected
      fuel_use_tot %>%
        filter(census_region %in% input$pick_place)%>%
        filter(end_use %in% input$pick_use) %>%
        group_by(sub_region)%>%
        summarize(sum=sum(btu))
    }
  })
  
  ## plots for Output
  
  ##top graph (by region)
  output$fe_plot <- renderPlot(
    ##if "ALL" is selected
    if(input$pick_place=="All"){
      ggplot(data = fe_reactive(), aes(x=fuel, y=btu, fill=census_region))+
        geom_col(position="dodge")+
        scale_fill_manual(values=c("darkslategray", "darkslategray4", "darkslategray3", "slategray"))+
        labs(x=input$pick_use, y="Fuel (Trillion Btu)", title=input$pick_place, fill="Census Region") +
        ylim(0,600)+
        theme_minimal()
    } else{
    ## if census region is selected
    ggplot(data = fe_reactive(), aes(x=fuel, y=btu, fill=sub_region))+
      geom_col(position="dodge")+
      scale_fill_manual(values=c("darkslategray", "darkslategray4", "darkslategray3", "slategray"))+
      labs(x=input$pick_use, y="Fuel (Trillion Btu)", title=input$pick_place, fill="Sub Region") +
      ylim(0,600)+
      theme_minimal()

      }
  )
  
 ##bottom graph (totals)
  output$fe_totals <- renderPlot(
    ##if census region is selected
    if(input$pick_place!="All"){
      ggplot(data = fe_reactive_tot(), aes(x=sub_region, y=sum, fill=sub_region))+
        geom_col()+
        scale_fill_manual(values=c("darkgreen", "seagreen3", "seagreen1", "darkseagreen2"))+
        labs(x=input$pick_use, y="Fuel (Trillion Btu)", title="Total Btu Used by Each Subregion (Trillions") +
        ylim(0,1000)+
        theme_minimal()
    } 
    else{
      ##if "ALL" is selected
      ggplot(data = fe_reactive_tot(), aes(x=census_region, y=btu, fill=census_region))+
        geom_col()+
        scale_fill_manual(values=c("darkgreen", "seagreen3", "seagreen1", "darkseagreen2"))+
        labs(x=input$pick_use, y="Fuel (Trillion Btu)", title="Total Btu Used by Each Region (Trillions)") +
        ylim(0,1000)+
        theme_minimal()
    }
  )
  
  
  pct_e_reactive <- reactive({
   
      states_contig_sf %>%
        mutate(color_plot = ifelse(pct_e=="NA","NA","not"))%>%
            mutate(color_plot = ifelse(pct_e>=(input$choose_pct),"electrified", color_plot)) 
  })
  
  alaska_reactive <- reactive({
    
    alaska_sf %>%
      mutate(color_plot = ifelse(pct_e>=(input$choose_pct),"electrified","not"))
  })
  
  hawaii_reactive <- reactive({
    
    hawaii_sf %>%
      mutate(color_plot = ifelse(pct_e>=(input$choose_pct),"electrified","not"))
  })

  
  output$pct_state <- renderPlot(
    ggplot()+
      geom_sf(data=pct_e_reactive(), aes(fill=as.factor(color_plot)))+
      scale_fill_manual(values=c("green","gray","gray4"))+
      labs(title = paste("At least",as.character(input$choose_pct),"% of homes"))+
      theme_void() 
  )
  
  output$elec_summary <- renderText({
    "In order to reach nationwide carbon neutrality goals, 
    we will have to electrify energy services wherever possible. 
    The map which shows (by 2020) the percent of each states' homes that are 
    totally electrified shows that we have a lot of progress to make 
    to reach this goal. As can be seen from  the tab before about different 
    regions on different fuel mixes, we currently have a legacy of relying on
    gas and other fuels to provide much needed services like space heating.
    77% is the max percent of homes electrified in any given state, in Colorado."
  })



}

shinyApp(ui = ui, server = server)













