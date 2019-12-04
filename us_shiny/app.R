
library(tigris)
library(shiny)
library(highcharter)
library(flexdashboard)
library(tidyverse)
library(viridis)
library(leaflet)
library(plotly)
library(geojsonio)



cdc_maternal_death_data = read.delim("./data/maternal_deaths_1999_2017.txt") %>%
  janitor::clean_names() %>%
  na.omit() %>%
  subset(notes != "Total") %>%
  select(year, state, deaths) 

cdc_birth_data = read.csv("./data/us_states_data.csv") %>%
  janitor::clean_names() %>%
  na.omit() %>%
  select(-total_population) %>%
  rename("state" = "state_of_residence") 


cdc_birth_data = cdc_birth_data[-52, ]



cdc_merged_data = merge(x = cdc_maternal_death_data, 
                        y = cdc_birth_data,
                        by = "state",
                        all = TRUE) %>%
  na.omit() %>%
  mutate(mortality_per_100000_births = deaths/births*100000)

# Downloading the shapefiles for states at the lowest resolution

states = states(cb=T)

popup_state = paste0("Deaths Per 100,000 Births: ", as.character(cdc_merged_data$mortality_per_100000_births))

n <- 4
colstops <- data.frame(
  q = 0:n/n,
  c = substring(viridis(n + 1), 0, 7)) %>%
  list_parse2()


ui = fluidPage(
  
  titlePanel("Maternal Mortality By State and Year"),
  
  # Sidebar with a slider input for year, numeric input for population 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("yearInput",
                  "Year",
                  min = 1999,
                  max = 2017,
                  step = 1,
                  sep = "",
                  value = 1999),
      
      selectInput("stateInput", "State",
                  choices = c("Alabama", "Arizona", "Arkansas", "California", "Colorado",
                              "Connecticut", "Florida", "Georgia", "Hawaii", "Idaho",
                              "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                              "Louisiana", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                              "Mississipi", "Missouri", "Montana", "Nevada", "New Jersey",
                              "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma",
                              "Oregon", "Pennsylvania", "South Carolina", "Tennessee", "Texas",
                              "Utah", "Virginia", "Washington", "Wisconsin"))
    ),
    
    # Show the map and table
    mainPanel(
      highchartOutput("map"),
      plotOutput("plot")
      
      
    )
  )


)



server = function(input, output) {
  
  
  #bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  #pal = colorNumeric(
   
  # palette = "GnBu", domain = year_df$mortality_per_100000_births)
  
  # Output plot
  output$map = renderHighchart({
    
    
    
    year_df = cdc_merged_data %>%
      filter(year == input$yearInput)
    data("usgeojson")
    
    highchart() %>%
    hc_add_series_map(usgeojson, year_df, name = "maternal mortality per 100,000 births",
                      value = "mortality_per_100000_births", joinBy =  c("woename", "state"),
                      dataLabels = list(enabled = TRUE,
                                        format = '{point.properties.postalcode}')) %>%
    hc_colorAxis(stops = colstops) %>%
    hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
    hc_mapNavigation(enabled = TRUE) 
  })
  
  output$plot = renderPlot({
    filtered =
      cdc_merged_data %>%
      filter(state == input$stateInput)
    ggplot(filtered, aes(x = year, y = mortality_per_100000_births)) +
      geom_bar(stat = "identity", fill = "#ffb6c1") +
      labs(x = "Year", y = "Maternal Mortality Per 100,000 births")
  })
  

 
  
}



shinyApp(ui = ui, server = server)