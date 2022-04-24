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

country_waste_data <- read.csv("country_level_data.csv")
municpal_waste_data <- read.csv("municipal_waste.csv")

waste_type <- c("organic_waste", "glass_waste", "metal_waste", "other_waste", "paper_cardboard_waste", "rubber_leather_waste", "wood_waste", "green_waste")
country <- c(country_waste_data$country_name)
OECD_country <- c(municpal_waste_data$Country, "None")
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
          
          # OECD Treatment compare
          fluidRow(
            column(4, h1("How are the waste treated?"),
                   fluidRow(
                     # selectizeInput(inputId = "couontryCompare", choices = NULL, label = "Select your country", multiple = T, selected = "Australia"
                     column(4, selectInput(inputId = "countryCompare", label = "Select your country", multiple = T, selected = "Australia", OECD_country),
                            fluidRow(
                              column(12, selectizeInput(inputId = "countryHighlight",selected = "None", options = NULL, label = "Select country to highlight", OECD_country))
                            ))
                   ),
            ),
            
            column(8, plotOutput("treatmentPlot"))
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
            # column(4, selectInput(inputId = "countryCompare", label = "Select your country"))
            column(4, h1("How Much Waste?"),
                   fluidRow(
                     column(12, selectInput(inputId = "wasteCompare", selected = "organic_waste", label = "Select types of waste to compare", multiple = T, waste_type)),
                   ),
                  ),
            
            column(8, plotOutput("wasteComparePlot"))
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
          
          # fluidRow(
          #     column(4, h1("How's the world reacting to it"),
          #            fluidRow(
          #              column(12, selectInput(inputId = "countryCollection", label = "Select type of waste", country)),         
          #            )),
          #     column(6, plotOutput("collectionPlot")),
          #           ),
            
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          
          # fluidRow(
          #     column(4, selectInput(inputId = "corrType", label = "Select type of correlation", corr_type)),
          #     column(8, plotOutput("scatterPlot")),
          #           ),
          
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
server <- function(input, output) {
  
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
  colnames(country_waste_data)[22] <- "construction_waste"
  colnames(country_waste_data)[23] <- "e-waste"
  colnames(country_waste_data)[24] <- "hazardous_waste"
  colnames(country_waste_data)[25] <- "industrial_waste"
  colnames(country_waste_data)[26] <- "medical waste"
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
      Out <- sum(country_waste_data$e-waste, na.rm = T)
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
  
  output$wasteComparePlot <- renderPlot({
    ggplot(comparedf(), aes(x = waste, y = values)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      xlab("Waste Type") + ylab("million tonnes")
  })
  
  # Donut plot
  a <- c(sum(country_waste_data$total_msw_total_msw_generated_tons_year, na.rm = T))
  b <- c("total waste in millions per tonne")
  donut <- data.frame(a, b)
  donut$labelPosition <- (donut$a + 0) / 2
  donut$label <- paste0(donut$b,":", donut$a)
  
  
  LCN <- country_waste_data %>% filter(region_id == "LCN")
  SAS <- country_waste_data %>% filter(region_id == "SAS")
  SSF <- country_waste_data %>% filter(region_id == "SSF")
  ECS <- country_waste_data %>% filter(region_id == "ECS")
  EAS <- country_waste_data %>% filter(region_id == "EAS")
  MEA <- country_waste_data %>% filter(region_id == "MEA")
  NAC <- country_waste_data %>% filter(region_id == "NAC")
  
  total_LCN <- sum(LCN$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_SAS <- sum(SAS$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_SSF <- sum(SSF$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_ECS <- sum(ECS$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_EAS <- sum(EAS$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_MEA <- sum(MEA$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_NAC <- sum(NAC$total_msw_total_msw_generated_tons_year, na.rm = T)
  
  region <- c("LCN", "SAS", "SSF", "ECS", "EAS", "MEA", "NAC")
  waste <- c(total_LCN, total_SAS, total_SSF, total_ECS, total_EAS, total_MEA, total_NAC)
  
  total_waste <- data.frame(region, waste)
  
  total_waste$fraction <- total_waste$waste / sum(total_waste$waste)
  
  total_waste$ymax <- cumsum(total_waste$fraction)
  total_waste$ymin <- c(0, head(total_waste$ymax, n=-1))
  
  total_waste$labelPosition <- (total_waste$waste + 0) / 2
  total_waste$label <- paste0(total_waste$region, "\n value: ", total_waste$region)
  
  hic <- country_waste_data %>% filter(income_id == "HIC")
  umc <- country_waste_data %>% filter(income_id == "UMC")
  lmc <- country_waste_data %>% filter(income_id == "LMC")
  lic <- country_waste_data %>% filter(income_id == "LIC")
  
  total_HIC <- sum(hic$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_UMC <- sum(umc$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_LMC <- sum(lmc$total_msw_total_msw_generated_tons_year, na.rm = T)
  total_LIC <- sum(lic$total_msw_total_msw_generated_tons_year, na.rm = T)
  
  income <- c("HIC", "UMC", "LMC", "LIC")
  income_total_waste <- c(total_HIC, total_UMC, total_LMC, total_LIC)
  
  income_waste <- data.frame(income, income_total_waste)
  
  income_waste
  income_waste$fraction <- income_waste$income_total_waste / sum(income_waste$income_total_waste)
  
  income_waste$ymax <- cumsum(income_waste$fraction)
  income_waste$ymin <- c(0, head(income_waste$ymax, n=-1))
  
  income_waste$labelPosition <- (income_waste$income_total_waste + 0) / 2
  income_waste$label <- paste0(income_waste$income, "\n value: ", income_waste$income)
  
  output$donutPlot <- renderPlot({
    if(input$donutType == "region_waste") {
      ggplot(total_waste, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=region), scientific = FALSE) +
        geom_rect() +
        coord_polar(theta="y") +
        xlim(c(2, 4))
    }

    else if(input$donutType == "income_waste") {
      ggplot(income_waste, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=income), scientific = FALSE) +
        geom_rect() +
        # geom_text( x=3.0, aes(y=labelPosition, label=label), size=3) +
        coord_polar(theta="y") + 
        xlim(c(2, 4))  
    }
  })
  
  # Top 5 waste Country plot
    selected <- reactive({
      req(input$wasteType)
      arrange_at(country_waste_data, .vars = input$wasteType, funs(desc)) 
      })
    
    selected_10 <- reactive(head(selected(), 5))
  
    output$wastePlot <- renderPlot({
      ggplot(selected_10(), aes(reorder(x = country_name, -.data[[input$wasteType]]), y = .data[[input$wasteType]])) +
        geom_bar(stat = "identity", fill = "Darkred") +
        xlab("Country") + ylab("million tonnes")
    })
    
    # Scatter Plot
    corr_selected <- reactive({
      req(input$corrType)
    })
    
    output$scatterPlot <- renderPlot({
      
      country_waste_data$total_waste_per_kg <- country_waste_data$total_msw_total_msw_generated_tons_year*1000
      country_waste_data$waste_per_capita <- country_waste_data$total_waste_per_kg/country_waste_data$population
      
      cdat <- country_waste_data %>% mutate(plotname = as.character(country_name))

      countrylist <- c("Sierra Leone", "Argentina", "Canada", "Ireland", "United Kingdom", "United States",
                        "New Zealand", "Iceland", "Japan", "Luxembourg", "Netherlands", "Switzerland", "Qatar", "South Africa", "Rwanda")
      cdat <- cdat %>%
         mutate(plotname = ifelse(plotname %in% countrylist, plotname, ""))
      cdat %>%
         select(country_name, plotname)
      
      ggplot(cdat, aes(x = .data[[input$corrType]], y = waste_per_capita), scientific = FALSE) +
        geom_point() +
        xlab(input$corrType) +
        ylab("waste per capita (kg/person/year)") +
        geom_text_repel(aes(label = plotname), size = 4)
      
    })
    
    # Pie Chart
    # Global waste treatment and Disposal
    # Waste Collection
    collection <- reactive({
      country_waste_data %>% filter(country == input$collectionType) 
    })
    
    countryCollection <- reactive({
      collection()[, 28:39]
    })
    
    collectionOutput <- reactive({
      data.frame(collection_method = c(colnames(countryCollection())), percentage = collection()[2,])
    })
    
    output$test <- renderTable({
      # collection()
      collectionOutput()
    })
    
    output$collectionPlot <- renderPlot({
      ggplot(collectionOutput(), aes(x = input$collectionType, y =  percentage)) +
        geom_bar(stat = "identity")
    })
    
    
    # Global Waste Treatment & Disposal
    output$treatmentPlot <- renderPlot({
      
    })
    
    # GDP per caipta bubble plot.
    
    # OECD Waste Treatment line plot comparison
    # updateSelectizeInput(session, 'foo', choices = OECD_country, server = T, multiple = T,selected = "Australia", options = NULL, label = "Select your country")
    # updateSelectizeInput(session, inputId = "couontryCompare", choices = paste(OECD_country), server = T)
    
    tCompare <- reactive({
      municpal_waste_data %>% filter(Variable == "Municipal waste treated") %>% filter(Country %in% input$countryCompare)
    })
    
    output$treatmentPlot <- renderPlot({
      
      if(input$countryHighlight == "None"){
        ggplot(tCompare(), aes(x = Year, y = Value, group = Country, color = Country), scientific = F) +
          geom_line() +
          geom_point() +
          scale_color_viridis(discrete = TRUE) +
          ylab("waste in tonnes")   
      }
      else {
        ggplot(tCompare(), aes(x = Year, y = Value, group = Country, color = Country), scientific = F) +
          geom_line() +
          geom_point() +
          gghighlight(Country == input$countryHighlight) +
          scale_color_viridis(discrete = TRUE) +
          ylab("waste in tonnes")           
      }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
