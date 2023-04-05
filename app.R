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
  navbarPage("Electrifying Residential Energy", ##title
             ## Tab 1
             ## start of tab
             tabPanel("Getting Started",
                     ##start of layout
                       sidebarLayout(
                        ##start of sidebar
                        sidebarPanel("A Shiny App created by Hannah Irish
                                     for ESM 244 Winter 2023", tags$hr(), 
                                     p("The tabs are meant to be explored in order
                                       upon first visit to the app.")), ##end of sidebar
                       ##start of main panel             
                      mainPanel(
                        ##main panel start tab format:
                        tabsetPanel(
                          ##start first tab
                          tabPanel(strong("Overview"), ##title
                                   tags$p(main_ov), tags$p(main_2p),
                                   tags$p(data_source), tags$p("The data is accessible ", tags$a(href="https://www.eia.gov/consumption/residential/data/2020/", "here"))
                                   ), ##end first tab
                          ##start second tab
                          tabPanel(strong("About the RECS"), ##title
                                   p(RECS),
                                   p("See more information", tags$a(href="https://www.eia.gov/consumption/residential/about.php", "here"))
                                   ), ##end second tab
                          ##start third tab
                          tabPanel(strong("Acknowledgements"), ##title
                                   strong("Data Source:"), p("U.S. Energy Information Administration. (2023). Residential Energy Consumption Survey
                                      (1979-2020). EIA. https://www.eia.gov/consumption/residential/data/2020/"),
                                   p("Thank you to Casey O'Hara and Nathan Grimes for instruction and assistance"),
                                   p("Thank you to Eric Masanet for consultation and for the idea to use this data")
                                   ) ##end third tab
                             ) ##end main panel tabs
                           ) ## end main panel
                      ) ## end panel layout
             ), ## end tab 1
             
             ##start tab 2
             tabPanel("Dashboard: Total Residential Energy Consumption by Fuel Type", ##title
                      ##start panel layout
                      sidebarLayout(
                        ##start sidebar
                        sidebarPanel(
                
                          ##checkbox inputs for the type of fuel to compare on the dashboard graph
                                     checkboxGroupInput(inputId = "fuel_dash",
                                                  label = "Choose Fuel Types to Add to the Graph to Compare Over Time",
                                                  choices = c("Total", "Electricity", "Natural Gas", "Propane", "Fuel Oil/Kerosene"),
                                                  selected="Total"), ## initialize Total to be selected
                                     tags$hr(),
                                     dashboard,
                                     p("***Wood also is used in residential buildings comparable
                                       to lower used fuel types but it is excluded from this analysis
                                       due to lack of consistency of data collection across survey
                                       years.")
                            
              
                                     # end checkbox
                        ), # end sidebar
                       
                        
                        ##main Panel 
                        mainPanel(plotOutput("dash_plot")
                                  ) ##end main panel
                      ) ## end panel layout
             ), ## end tab
             
             ##start tab 3
             tabPanel("Electricty vs. Natural Gas Historically", ##title
                     ## start panel layout
                       sidebarLayout(
                         ##start sidebar
                        sidebarPanel("Total Energy Use vs. Use per Household",
                                     ##select input for the energy vs natural gas graph
                                     ##total use versus per household
                                     selectInput("en_nat",
                                                 label= "",
                                                 choices = c("Total Use"=1,"Per Household"=2)),
                                     tabsetPanel(tabPanel("Total Use", 
                                                          "Natural Gas and Electricity are the fuel types that are the most consumed
                                                          by residential buildings in the United States. Switching appliances to electricity
                                                          (on an increasingly lower fossil fuel grid) will help the U.S. move towards Net Zero
                                                          goals. Historical trends show natural gas consumption decreasing with time while 
                                                          electricity increases."),
                                                 tabPanel("Per Household",
                                                          "We also must investigate the fuel consumed by household (thousand MJ rather than billion MJ).
                                                          Since natural gas is decreasing
                                                          with time much more than electricity changes, we guess that at least some homes are switching
                                                          from using natural gas to electricity with time. Note: Here, we are only looking at 1990
                                                          to 2015 as that is when this data started being consistently reported in the RECS.")
                                                 
                                                 )
                                     
                      ), ## end sidebar
                      
                      ##start main panel
                      mainPanel(plotOutput("ng_plot"),
                                
                                ) ##end main panel
                      ) ##end panel layout
             ), ##end tab
                      
             
    
             
             
            ##start tab 5
             tabPanel("Electrification of the States", ##title
                      ##start panel layout
                      sidebarLayout(
                        ##start sidebar
                        sidebarPanel(strong("Some Homes are Fully Electrified"), ##title
                                     ##toggle explanation text
                                     p("But many use natural gas"),
                                     tags$hr(),
                                     p("Here we can actually look at the percentage of homes in each 
                                     state that are currently electrified as of 2020. Darker green indicates more homes
                                     have been fully electrified (use electricity only for any end use. However, toggle the switch 
                                     to see the percentage of homes that use at least some natural gas, for any end use."),
                                     p("Gray values indicate that there was no data reported for that state.")
                                    
                        ), ## end sidebar
                        
                        ##start main panel
                        mainPanel(
                              ##start switch input
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
                            ), ##end switch input
                            plotOutput("pct_state")
                            ) ##end main panel
                       
                        
                      ) ##end panel layout
             ), ##end tab
             
            ##Start tab 4
            tabPanel("Electricity Grid by End Use by Region", ##title
                     ##start panel layout
                     sidebarLayout(
                       ##start sidebar
                       sidebarPanel(
                         tabsetPanel(
                           tabPanel("Fuel Use By Region",
                                    "Some appliances will be easier to electrify than others. Use the 'interactive' tab
                                     to toggle by region and by end use to see how different uses and regions are powered and 
                                     which end uses are the most energy intensive. 
                                     'Other' includes lighting, clothes washers, other appliances, etc. Again, wood is excluded
                                     as it was not totaled by the survey similar to the other fuel types. Data is from 2015 as 
                                     2020 data has not yet been published."
                           ),
                           tabPanel("Interactive", 
                                    "Different Regions of the U.S. use Different Combinations of Fuel Types.", ##title
                                    ## radio buttons to click "all" vs specific census region
                                    radioButtons(inputId = "pick_place",
                                                 label = "Choose Region:",
                                                 choices = c("All",unique(fuel_use_tidy$census_region)),
                                    ) , ## end radio button 1
                                    "This Varies by End Use", ## second title
                                    ## radio buttons to click the end use
                                    radioButtons(inputId = "pick_use",
                                                 label = "Choose End Use:",
                                                 choices = unique(fuel_use$end_use)
                                    ) ##end radio button 2
                                    # end radioButtons overall
                           ),
                         )
                       ), # end sidebar
                       
                       ## start main panel
                       mainPanel(
                         ##separate into tabs on main panel
                         tabsetPanel(
                           tabPanel("Overview", plotOutput("fe_plot")), ##panel that has graph by region/subregion
                           tabPanel("Total by Region",plotOutput("fe_totals")) ##panel that has totaled graph
                         ) ## end tab segmenting
                       ) ## end main panel
                     ) ##end panel layout
            ), ##end tab
            
            ## Tab 6
             tabPanel("Energy Insecurity", ##title
                      ## start panel layout
                      sidebarLayout(
                        ## start sidebar
                        sidebarPanel(strong("Energy Insecurity in the US (2020)"),
                                     "As we electrify the grid and U.S. households, we must remember
                                     that as climate change worsens, so does residential energy insecurity.
                                     The map on this page shows the percent of homes in each state reporting
                                     any energy insecurity.",
                                     p("The 'breakdown' tab will show the percent of homes using each fuel
                                     type as their main fuel type that report each indicator of energy
                                     insecurity. We saw that space heating is the main source of energy
                                     consumption in U.S. households, and the energy insecurity section of 
                                     the survey shows that homes using mainly electricity for heating are 
                                     reporting a higher percentage of homes energy insecure."),
                                     p("This does not necessarily mean electricity is less reliable, but
                                       as we move forward with any low carbon, climate change solution, 
                                       we must always consider who will face the highest impacts from 
                                       both climate change as well as policy changes to fight it.")
                                     
                        
                                     ), ##end sidebar
                        ##start main panel
                        mainPanel(
                          ##start main panel segmentation
                          tabsetPanel( 
                            tabPanel("Map",plotOutput("ins_map")), ##tab with map of insecurity
                            tabPanel("Breakdown",
                                     selectInput("ins_ind",
                                                 label= "Choose an Energy Insecurity Indicator:",
                                                 choices = c("Any Insecurity" = "any_ins",
                                                             "Reducing Food or Medicine due to Energy Costs" = "reduce_food_med",
                                                             "Leaving Home at Unhealthy Temperature" = "unhealthy_temp",
                                                            "Receiving Disconnect/Stop Delivery Notice" ="disconnect",
                                                            "Unable to use Heating Equipment" = "unable_heat"
                                                             )
                                                 ),
                                     plotOutput("insecure")
                                     ) ##tab of more specific data collection
                          ) ##end panel segmentation
                        ) ##end main panel
                      ) ##end panel layout
                      
             ) ##end tab 6
            
            
  ) ## end Navbar
)##end UI


## start server
server <- function(input, output) {
  
  ## INPUTS TAB 1
  dash_reactive <- reactive({ 
    top_4_tot %>% ## 4 fuel types + the total 
      filter(fuel %in% input$fuel_dash) ##add fuels from checkbox input
  })

  ##OUTPUTS TAB 1
  library(RColorBrewer)
  
  ##set color by fuel type
  my_colors <- c("Electricity" = "darkgreen", "Natural Gas" = "purple", "Fuel Oil/Kerosene" = "blue", "Propane" = "darkorange", "Total" = "black")
  
  ## create dashboard output
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
    if(input$en_nat == 1){ ##if total is selected, choose the full df that has elec vs natural gas options
      top_2
    } else{
      top_2 %>% ##otherwise choose by household use (only available starting 1990)
        filter(year%in%1990:2015)
    }
  })
  
  ##OUTPUTS TAB 2
  
  ##if the total is selected, plot top2
  output$ng_plot <- renderPlot({
    if(input$en_nat==1){
    ggplot(data=elec_ng_reactive(),(aes(x=year, y=MJ, group=fuel))) +
      geom_line(size=2, aes(color=fuel)) +
      geom_point(size=1,aes(color=fuel))+
      labs(y="Billion MJ", x="", title="Total U.S. Fuel Use by Type, 1997-2015", color="Fuel Type")+
      scale_color_manual(values=c( "olivedrab", "darkgreen"))+
      theme_minimal()
    }
    else{ ##otherwise plot the by household number
      ggplot(data=elec_ng_reactive(),(aes(x=year, y=MJ_hh, group=fuel))) +
        geom_line(size=2, aes(color=fuel)) +
        geom_point(size=1,aes(color=fuel))+
        labs(y="Thousand MJ", x="", title="Average (Thousand) MJ Used per Household Using That Fuel Type", color="Fuel Type")+
        scale_color_manual(values=c( "olivedrab", "darkgreen"))+
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
  
  ## tab3 Output
  
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
  
  ##make the reactive the sf
  pct_e_reactive <- reactive({
    states_contig_sf 
  })
  
  ##if electricity:
  output$pct_state <- renderPlot({
    if(input$pick_mode=="TRUE") {
      ggplot()+
        geom_sf(data=pct_e_reactive(), size=0.2,color="black", aes(fill=pct_e))+
        scale_fill_gradient(low="darkseagreen1", high="green4")+
        labs(fill="Percent of Homes in State")+
        labs(title = "Percent of Homes Fully Electrified")+
        theme_void() 
    } else( ##if natural gas:
      ggplot()+
        geom_sf(data=pct_e_reactive(), size=0.2,color="black", aes(fill=pct_i))+
        scale_fill_gradient(low="lavender", high="darkmagenta")+
        labs(fill="Percent of Homes in State")+
        labs(title = "Percent of Homes Using Natural Gas for Any Reason")+
        theme_void() 
    )
  }, bg= "transparent"
  
  )
  
  
  output$elec_summary <- renderText({
    "Make this in the main panel instead!"
  })
  
  
  ### TAB 5
  
  ## INPUTS
  
  ##select the sf
  pct_i_reactive <- reactive({
    states_contig_sf 
  })
  
  ## OUTPUTS
 
  ##show map of energy insecurity
  output$ins_map <- renderPlot({
      ggplot()+
        geom_sf(data=pct_i_reactive(), size=0.2,color="black", aes(fill=pct_i))+
        scale_fill_gradient(low="lavenderblush2", high="red")+
        labs(fill="Percent of Homes Energy Insecure")+
        labs(title = "Percent of Homes Energy Insecure")+
        theme_void()
  }, bg = "transparent"
  )
  
  ###INSECURITY TAB
  
  insecurity_reactive <- reactive({
    ins_tidy %>% filter(indicator == input$ins_ind)
  })
  
  output$insecure <- renderPlot({
    ggplot(data=insecurity_reactive(), aes(x= fct_reorder(fuel, percent, desc), y=percent, fill=percent))+
             geom_col(aes(fill=percent))+
             scale_fill_gradient(high="red", low="lavender")+
      ylim(0,0.5)+
              labs(x="Main Heating Fuel", y="Percent of Homes Reporting Insecurity", fill="Percent of Homes" 
                   )+
             theme_minimal()
  },  bg = "transparent"
  )
   
} ## end ui

##create app
shinyApp(ui = ui, server = server)




