library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)


my_theme <- bs_theme(
  bg = "#B2D3C2",
  fg = "black",
  primary = "#028A0F",
  base_font = font_google("Crimson Pro")
)

source("wrangle_files/fuel_use_region.R")
source("wrangle_files/map_wrangle.R")
source("wrangle_files/dashboard_wrang.R")
source("wrangle_files/text.R")

ui <- fluidPage(
  theme=my_theme,
  navbarPage("Electrifying Residential Energy",
             tabPanel("Getting Started",
                      mainPanel(
                        tabsetPanel(
                          tabPanel(strong("Overview"), tags$br(), main_ov, tags$br(), tags$br(),
                                   main_2p, tags$br(), tags$br(), data_source, tags$br(), tags$br(),
                                   "The data is found here: "),
                          tabPanel(strong("About the RECS"))
                      )
                      )
             ),
             tabPanel("Dashboard: Total Residential Energy Consumption by Fuel Type",
                      sidebarLayout(
                        sidebarPanel(
                
                          
                                     checkboxGroupInput(inputId = "fuel_dash",
                                                  label = "Choose Fuel Types to Add to the Graph to Compare Over Time",
                                              
                                                  choices = c("Total", "Electricity", "Natural Gas", "LPG", "Fuel Oil/Kerosene"),
                                                  selected="Total")
                            
              
                                     # end selectInput
                        ), # end sidebarPanel
                        
                        
                        mainPanel(plotOutput("dash_plot"))
                      )
             ), 
             
             tabPanel("Electricty vs. Natural Gas Historically",
                      sidebarLayout(
                        sidebarPanel("Total Energy Use vs. Use per Household",
                                     selectInput("en_nat",
                                                 label= "",
                                                 choices = c("Total Use"=1,"Per Household"=2))
                                     
                      ),
                      mainPanel(plotOutput("ng_plot"))
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
                        sidebarPanel("Choose the Housing Characteristic:",
                                     
                                     switchInput(
                                       inputId = "pick_mode",
                                       label = NULL,
                                       value = TRUE,
                                       onLabel = "Fully Electric",
                                       offLabel = "Uses Natural Gas (Any Use)",
                                       onStatus = NULL,
                                       offStatus = NULL,
                                       size = "default",
                                       labelWidth = "auto",
                                       handleWidth = "auto",
                                       disabled = FALSE,
                                       inline = FALSE,
                                       width = NULL
                                     )
                                     
                        ),
                        
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Analysis",textOutput("elec_summary")),
                            tabPanel("Map",plotOutput("pct_state"))
                            
                            
                          )
                        )
                      )
                      
             ),
             
             tabPanel("Energy Insecurity",
                      sidebarLayout(
                        sidebarPanel(
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
  
  ## INPUTS TAB 1
  
  dash_reactive <- reactive({
    top_4_tot %>%
      filter(fuel %in% input$fuel_dash)
  })

  ##OUTPUTS TAB 1
  library(RColorBrewer)
  
  my_colors <- c("Electricity" = "darkgreen", "Natural Gas" = "blue", "Fuel Oil/Kerosene" = "purple", "LPG" = "darkorange", "Total" = "black")
  
  
  output$dash_plot <- renderPlot( {
    ggplot(data=dash_reactive(),(aes(x=as.factor(year), y=MJ, group=fuel, color=fuel))) +
             geom_line(size=2, aes(color=fuel)) +
             geom_point(size=1,aes(color=fuel))+
              ylim(0,12000) +
              labs(y="Billion MJ", x="", title="Total U.S. Fuel Use by Type, 1997-2015", color="Fuel Type")+
              scale_color_manual(values=my_colors)+
             theme_minimal()
  }, bg="transparent")
  
  ##INPUTS TAB 2
  
  elec_ng_reactive <- reactive({
    if(input$en_nat == 1){
      top_2
    } else{
      top_2 %>%
        filter(year%in%1990:2015)
    }
    
  })
  
  ##OUTPUTS TAB 2
  
  output$ng_plot <- renderPlot({
    if(input$en_nat==1){
    ggplot(data=elec_ng_reactive(),(aes(x=year, y=MJ, group=fuel))) +
      geom_line(size=2, aes(color=fuel)) +
      geom_point(size=1,aes(color=fuel))+
      labs(y="MJ", x="", title="Total U.S. Fuel Use by Type, 1997-2015", color="Fuel Type")+
      scale_color_manual(values=c( "green1", "darkgreen"))+
      theme_minimal()
    }
    else{
      ggplot(data=elec_ng_reactive(),(aes(x=year, y=MJ_hh, group=fuel))) +
        geom_line(size=2, aes(color=fuel)) +
        geom_point(size=1,aes(color=fuel))+
        labs(y="MJ", x="", title="Average (Billion) MJ Used per Household Using Fuel Type", color="Fuel Type")+
        scale_color_manual(values=c( "green1", "darkgreen"))+
        theme_minimal()
    }
  }, bg="transparent")
  

  
  
  ## Tab 3: Region + End Use + Fuel
  
  ## INPUTS
  
  ## end use graph inputs
  fe_reactive <- reactive({
    ## if "ALL" is selected
    if(input$pick_place=="All"){
      fuel_use_tidy_test <- fuel_use_tidy %>%
        filter(end_use %in% input$pick_use) %>%
        group_by(census_region, fuel) %>%
        summarize(MJ = sum(MJ, na.rm=TRUE))
      
    } else{
      
      ## if a census region is selected
      fuel_use_tidy_test <- fuel_use_tidy %>%
        filter(end_use %in% input$pick_use, census_region %in% input$pick_place) %>%
        group_by(sub_region, fuel) %>%
        summarize(MJ = sum(MJ, na.rm=TRUE))
      
    }
  })
  
  
  ## totaling graph inputs
  fe_reactive_tot <- reactive({
    ## if "ALL is selected"
    if(input$pick_place=="All"){
      fuel_use_tidy %>%
        filter(end_use %in% input$pick_use) %>%
        group_by(census_region) %>%
        summarize(MJ = sum(MJ, na.rm=TRUE))
      
    } else{
      ## if census region is selected
      fuel_use_tidy %>%
        filter(end_use %in% input$pick_use, census_region %in% input$pick_place) %>%
        group_by(fuel) %>%
        summarize(MJ = sum(MJ, na.rm=TRUE))
    }
  })
  
  ## plots for Output
  
  ##left graph (by region)
  output$fe_plot <- renderPlot( {
    ##if "ALL" is selected
    if(input$pick_place=="All"){
      ggplot(data = fe_reactive(), aes(x=fuel, y=MJ, fill=census_region))+
        geom_col(position="dodge")+
        scale_fill_manual(values=c("darkslategray", "darkslategray4", "darkslategray3", "slategray"))+
        labs(x=input$pick_use, y="Fuel (Million MJ)", title=input$pick_place, fill="Census Region") +
        ylim(0,1000)+
        #geom_text(aes(label=MJ),position = position_dodge(width = 1))+
        theme_minimal() +
        theme(axis.text.y = element_text(size=12, 
                                         color="black"),
              axis.text.x = element_text(size=12, 
                                         color="black"),
              axis.title.x = element_text(size=16, 
                                          color="darkslategray",
                                          face="bold"),
              axis.title.y = element_text(size=16, 
                                          color="darkslategray",
                                          face="bold"))
    } else{
      ## if census region is selected
      ggplot(data = fe_reactive(), aes(x=fuel, y=MJ, fill=sub_region))+
        geom_col(position="dodge")+
        scale_fill_manual(values=c("darkslategray", "darkslategray4", "darkslategray3", "slategray"))+
        labs(x=input$pick_use, y="Fuel (Million MJ)", title=input$pick_place, fill="Sub Region") +
        ylim(0,1000)+
        #geom_text(aes(label=MJ),position = position_dodge(width = 1))+
        theme_minimal() +
        theme(axis.text.y = element_text(size=12, 
                                         color="black"),
              axis.text.x = element_text(size=12, 
                                         color="black"),
              axis.title.x = element_text(size=16, 
                                          color="darkslategray",
                                          face="bold"),
              axis.title.y = element_text(size=16, 
                                          color="darkslategray",
                                          face="bold"))
      
    }
  }, bg="transparent"
  )
  
  ##right graph (totals)
  output$fe_totals <- renderPlot( 
    {
      ##if census region is selected
      if(input$pick_place!="All"){
        ggplot(data = fe_reactive_tot(), aes(x=fuel, y=MJ, fill=fuel))+
          geom_col()+
          scale_fill_manual(values=c("darkgreen", "seagreen3", "seagreen1", "darkseagreen2"))+
          labs(x=input$pick_use, fill="Sub Region",y="Fuel (Million MJ)", title="Total Billion MJ of Each Fuel Used") +
          ylim(0,1500)+
          geom_text(aes(label=MJ), vjust=-1)+
          theme_minimal() +
          theme(axis.text.y = element_text(size=12, 
                                           color="black"),
                axis.text.x = element_text(size=12, 
                                           color="black"),
                axis.title.x = element_text(size=16, 
                                            color="darkslategray",
                                            face="bold"),
                axis.title.y = element_text(size=16, 
                                            color="darkslategray",
                                            face="bold"))
      } 
      else{
        ##if "ALL" is selected
        ggplot(data = fe_reactive_tot(), aes(x=census_region, y=MJ, fill=census_region))+
          geom_col()+
          scale_fill_manual(values=c("darkgreen", "seagreen3", "seagreen1", "darkseagreen2"))+
          labs(x=input$pick_use, fill= "Census Region",y="Fuel (Million MJ)", title="Total Billion MJ Used by Each Region (All Fuel Types)") +
          ylim(0,2000)+
          geom_text(aes(label=MJ, vjust=-1))+
          theme_minimal()+
          theme(axis.text.y = element_text(size=12, 
                                           color="black"),
                axis.text.x = element_text(size=12, 
                                           color="black"),
                axis.title.x = element_text(size=16, 
                                            color="darkslategray",
                                            face="bold"),
                axis.title.y = element_text(size=16, 
                                            color="darkslategray",
                                            face="bold")
                
          ) 
      }
    }, bg="transparent"
    
  )
  
  
  ###TAB 4: ELECTRIFICATION MAP 
  
  pct_e_reactive <- reactive({
    states_contig_sf 
  })
  
  
  output$pct_state <- renderPlot({
    if(input$pick_mode=="TRUE") {
      ggplot()+
        geom_sf(data=pct_e_reactive(), size=0.2,color="black", aes(fill=pct_e))+
        scale_fill_gradient(low="darkseagreen1", high="green4")+
        labs(fill="Percent of Homes in State")+
        labs(title = "Percent of Homes Fully Electrified")+
        theme_void() 
    } else(
      ggplot()+
        geom_sf(data=pct_e_reactive(), size=0.2,color="black", aes(fill=pct_i))+
        scale_fill_gradient(low="lightblue", high="blue")+
        labs(fill="Percent of Homes in State")+
        labs(title = "Percent of Homes Using Natural Gas for Any Reason")+
        theme_void() 
    )
  }, bg= "transparent"
  
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
  
  
  ### TAB 5
  
  ## INPUTS
  
  ## OUTPUTS
 
  output$ins_map <- renderPlot({
      ggplot()+
        geom_sf(data=pct_e_reactive(), size=0.2,color="black", aes(fill=pct_e))+
        scale_fill_gradient(low="darkseagreen1", high="green4")+
        labs(fill="Percent of Homes in State")+
        labs(title = "Percent of Homes Fully Electrified")+
        theme_void()
  }, bg = "transparent"
  )
   
}

shinyApp(ui = ui, server = server)




