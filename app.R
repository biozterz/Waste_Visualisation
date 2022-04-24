library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(shinythemes)
library(DT)
library(hrbrthemes)
library(viridis)
library(gghighlight)
library(plotly)

country_waste_data <- read.csv("country_level_data.csv")
municpal_waste_data <- read.csv("municipal_waste.csv")

waste_type2 <- c("organic_waste", "glass_waste", "metal_waste", "other_waste", "paper_cardboard_waste", "rubber_leather_waste", "wood_waste", "green_waste")
waste_type <- c("total_municipal_waste", "agricultural_waste", "construction_&_demolition_waste", "e_waste", "hazardous_waste", "industrial_waste", "medical_waste")
country <- c(country_waste_data$country_name)
OECD_country <- c("None", municpal_waste_data$Country)
corr_type <- c("gdp", "population")
donut_type <- c("region_waste", "income_waste")
alarm_type <- c("total municipal waste in the world", "total hazardous waste", "total industrial waste", "total electronic waste")

# Define UI for application.
ui <- fluidPage(
                
          titlePanel(
            h1("Oblivious: A Perspective On Waste.",align = "center", style = "color: darkgreen", ),
            ),

          br(),
          
          HTML('<center><img src="landfill_child.jpeg"></center>'),
          
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          fluidRow(
            column(6, offset = 3, align = "centre", h1('"Kean fills her sack, piece of plastic by piece of plastic.', style = "font-family: monospace; color: green"),
                   h1('She stands at the bottom of one of the deep holes on the garbage dump.', style = "font-family: monospace; color: green"),
                   h1('Suddenly she hears a roaring motor and the rustling noise that can only mean one thing: falling trash!', style = "font-family: monospace; color: green"),
                   h1('Kean pulls herself up and narrowly manages to escape the avalanche of trash started by the tractor, that fills the hole where she was standing."',
                      style = "font-family: monospace; color: green"),
                  )
          ),
        
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          fluidRow(
            column(4, h2("Alarming Figures!"),
                   fluidRow(
                     column(8, selectInput(inputId = "alarmType", label = "Choose your alarming statistic", alarm_type)),
                   )),
            column(8,
                   fluidRow(
                     column(4, h2(textOutput("alarmText"), style = "color: darkred"), h2("tonnes")), 
                     column(8, h2("Equivalent to"), h2(textOutput("equivalentText"), style = "color: darkblue"), align = "left"),
                     fluidRow(
                       column(12, h2(textOutput("explanationText"), style = "font-family:monospace; color:green")),
                              )
                     )
                   ),
            ),
          
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          # Compare total of different types of waste.
          fluidRow(
            # column(4, selectInput(inputId = "countryCompare", label = "Select your country"))
            column(4, h1("How Much Waste?"),
                   fluidRow(
                     column(12, selectInput(inputId = "wasteCompare", selected = c("total_municipal_waste", "medical_waste", "agricultural_waste"), label = "Select types of waste to compare", multiple = T, waste_type)),
                   ),
            ),
            
            column(8, plotlyOutput("wasteComparePlot"))
          ),
          
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          # OECD Treatment compare
          fluidRow(
            column(12, h1("How are the waste treated?", align = 'center'),
                   fluidRow(
                     column(12, 
                            selectizeInput(
                              inputId = "countryCompare",
                              label = "Select your country",
                              choices = NULL,
                              multiple = T,
                              selected = "Australia",
                              options = list(create = F, maxOptions = 100000L)
                            ),
                            fluidRow(
                              column(12, 
                                     selectizeInput(
                                       inputId = "countryHighlight",
                                       choices = NULL,
                                       selected = "None",
                                       label = "Select country to highlight",
                                       options = list(create = F, maxOptions = 100000L)
                                     ),
                                     fluidRow(
                                       column(12, plotlyOutput("treatmentPlot"))  
                                     )
                              )
                            ))
                   ),
            ),
            
            
          ),
          
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          fluidRow(
            column(4, h1("Waste Domination"),
                   fluidRow(
                     column(12, p("Top 5 countries with the most waste (specific waste)",
                            fluidRow(
                              column(12, selectInput(inputId = "wasteType", label = "Select type of waste", waste_type)),
                            )
                          ),
                        )
                   )),
              
              column(8, plotOutput("wastePlot")),
                    ),
          
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          fluidRow(
            column(12, tableOutput("test"))
          ),
          
          fluidRow(
            column(4, h1("How's your country reacting to it?"),
                   fluidRow(
                     column(12, selectInput(inputId = "collectionType", label = "Select type of waste", country)),         
                   )),
            column(6, plotOutput("collectionPlot")),
          ),
            
)


# Define server logic
server <- function(input, output, session) {
  
  # Rename waste columns
  colnames(country_waste_data)[6] <- "organic_waste"
  colnames(country_waste_data)[7] <- "glass_waste"
  colnames(country_waste_data)[8] <- "metal_waste"
  colnames(country_waste_data)[9] <- "other_waste"
  colnames(country_waste_data)[10] <- "paper_cardboard_waste"
  colnames(country_waste_data)[11] <- "plastic_waste"
  colnames(country_waste_data)[12] <- "rubber_leather_waste"
  colnames(country_waste_data)[13] <- "wood_waste"
  colnames(country_waste_data)[14] <- "green_waste"
  colnames(country_waste_data)[20] <- "population"
  colnames(country_waste_data)[21] <- "agricultural_waste"
  colnames(country_waste_data)[22] <- "construction_&_demolition_waste"
  colnames(country_waste_data)[23] <- "e_waste"
  colnames(country_waste_data)[24] <- "hazardous_waste"
  colnames(country_waste_data)[25] <- "industrial_waste"
  colnames(country_waste_data)[26] <- "medical_waste"
  colnames(country_waste_data)[27] <- "total_municipal_waste"
    
  # Alarming Figures
  
  output$alarmText <- renderText({
    if(input$alarmType == "total municipal waste in the world"){
      Out <- sum(country_waste_data$total_municipal_waste, na.rm = T)
    }
    
    else if(input$alarmType == "total hazardous waste"){
      Out <- sum(country_waste_data$hazardous_waste, na.rm = T) 
    }
    
    else if(input$alarmType == "total industrial waste"){
      Out <- sum(country_waste_data$industrial_waste, na.rm = T)
    }
    
    else if(input$alarmType == "total electronic waste"){
      Out <- sum(country_waste_data$e_waste, na.rm = T)
    }
    # Out
    paste(paste(Out))
  })
  
  output$equivalentText <- renderText({
    if(input$alarmType == "total municipal waste in the world"){
      Out <- "20 million Blue Whales"
    }
    
    else if(input$alarmType == "total hazardous waste"){
      Out <- "7.8 million Blue Whales"
    }
    
    else if(input$alarmType == "total industrial waste"){
      Out <- "120 million Blue Whales"
    }
    
    else if(input$alarmType == "total electronic waste"){
      Out <- "4.2 million Blue Whales"
    }
    # Out
    paste(paste(Out))
  })
  
  output$explanationText <- renderText({
    if(input$alarmType == "total municipal waste in the world"){
      Out <- '"Waste type consisting of everyday items discorded by the public. The waste composition depends on the municipality, affected by factors such as whether the municipality have a well developed recycling system and government policies on products like plastic."'
    }
    
    else if(input$alarmType == "total hazardous waste"){
      Out <- '"In one example, 4,000 tons of toxic waste was shipped to Nigeria and simply dumped. The waste  included 150 tons of highly toxic polychlorinated biphenyls, or PCBs, that are known to cause hormone deficiencies and reduced cognitive development in children.s"'
    }
    
    else if(input$alarmType == "total industrial waste"){
      Out <- '"Oil, solvents, weed grass are some examples. It may be hazardous or non-hazardous and can be ignitable, corrosive or reactable."'
    }
    
    else if(input$alarmType == "total electronic waste"){
      Out <- '"Electronic waste is highly toxic if not treated properly. And sadly, that is often the case when dumped in poor countries. Heavy metals (eg. Arsenic,lead, chromium) and toxic chemicals pollute surrounding communities and poison the local population including children - and not least the people working at the dumpsites, often with no protection."'
    }
    # Out
    paste(paste(Out))
  })
  
  # Alarming plot
  
  wt <- sum(country_waste_data$total_municipal_waste, na.rm = T)
  ht <- sum(country_waste_data$hazardous_waste, na.rm = T)
  it <- sum(country_waste_data$industrial_waste, na.rm = T)
  
  total_alarm <- data.frame(total_waste_in_the_world = c(wt), total_hazardous_waste = c(ht), total_industrial_waste = c(it))
  
  output$totalPlot <- renderPlot({
    ggplot(total_alarm, aes(x = input$alarmType, y = .data[[input$alarmType]]), scientific = F) +
      geom_col() + 
      geom_bar(stat="identity", fill = "darkred") +
      xlab(input$alarmType) + ylab("waste in million tonnes")
  })
  
  # Create comparison bar chart
  
  compare_Selected <- reactive({
    req(input$wasteCompare)
    country_waste_data <- country_waste_data %>% select(input$wasteCompare)
  })
  
  total_compare <- reactive(
    compare_Selected() %>% colSums(na.rm = T),
  )
  
  comparedf <- reactive(
    data.frame(waste = c(input$wasteCompare), values = c(total_compare()))  
  )
  
  output$wasteComparePlot <- renderPlotly({
    
    p <- ggplot(comparedf(), aes(x = waste, y = values/1000000)) +
        geom_bar(stat = "identity", fill = "darkgreen") +
        ylab("million tonnes") +
        coord_flip()
    
    options(scipen=999)
    ggplotly(p)
  })
    
    # GDP per capita bubble plot.
    
    # OECD Waste Treatment line plot comparison
    
    updateSelectizeInput(
      session = session,
      inputId = "countryCompare",
      choices = OECD_country,
      selected = "Australia",
      server = TRUE)

    updateSelectizeInput(
      session,
      inputId = "countryHighlight",
      choices = OECD_country,
      server = T)
    
    tCompare <- reactive({
      municpal_waste_data %>% filter(Variable == "Municipal waste treated") %>% filter(Country %in% input$countryCompare)
    })
    
    output$treatmentPlot <- renderPlotly({
      
      if(input$countryHighlight == "None"){
        ggplotly(
          ggplot(tCompare(), aes(x = Year, y = Value, group = Country, color = Country), scientific = F) +
            geom_line() +
            geom_point() +
            scale_color_viridis(discrete = TRUE) +
            ylab("waste in tonnes") 
        )
      }
      else {
        ggplotly(
          ggplot(tCompare(), aes(x = Year, y = Value, group = Country, color = Country), scientific = F) +
            geom_line() +
            geom_point() +
            gghighlight(Country == input$countryHighlight) +
            scale_color_viridis(discrete = TRUE) +
            ylab("waste in tonnes") 
        )
      }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
