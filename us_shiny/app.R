
library(tigris)
library(shiny)
library(highcharter)
library(flexdashboard)
library(tidyverse)
library(viridis)
library(leaflet)
library(plotly)
library(geojsonio)

cesarean_rate_df = read.csv("./data/US_cesarean_rates.csv") %>%
  janitor::clean_names()  %>%
  select(-total_births) %>%
  rename("state" = "state_of_residence") 

mother_cost_df = read.csv("./data/US_cost_to_mother.csv", fileEncoding = "latin1") %>%
  janitor::clean_names()

csr_cost_df = merge(x = cesarean_rate_df, 
                    y = mother_cost_df,
                    by = "state",
                    all = TRUE)

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
 mutate(mortality_per_100000_births = deaths/births*100000) %>%
 select(-deaths, -births)

final_merged_data = merge(x = csr_cost_df, 
                          y = cdc_merged_data,
                          by = "state",
                          all = TRUE) %>%
  mutate(cost = gsub(",", "", cost)) %>%
  mutate(cost = gsub("\\$", "", cost)) %>%
  mutate(cost = as.numeric(str_squish(cost)))

# Downloading the shapefiles for states at the lowest resolution
states = states(cb=T)

popup_state = paste0("Deaths Per 100,000 Births: ", as.character(final_merged_data$mortality_per_100000_births))

n <- 4
colstops <- data.frame(
  q = 0:n/n,
  c = substring(viridis(n + 1), 0, 7)) %>%
  list_parse2()


ui = fluidPage(
  
  titlePanel("US Maternal Health Statistics By State and Year"),
  
  # Sidebar with a slider input for year, numeric input for population 
  sidebarLayout(
    sidebarPanel(
      radioButtons("typeInput", "Measure of Interest",
                   choices = c("Maternal Mortality Rate",
                               "Cesarean Rate",
                               "Cost to Mother of Giving Birth"),
                   selected = "Maternal Mortality Rate"),
      sliderInput("yearInput",
                  "Year",
                  min = 1999,
                  max = 2017,
                  step = 1,
                  sep = "",
                  value = 1999),
      p("Year selection only works for the 'Maternal Mortality Rate' Measure of Interest."),      
      selectInput("stateInput", "State",
                  choices = c("Alabama", "Arizona", "Arkansas", "California", "Colorado",
                              "Connecticut", "Florida", "Georgia", "Hawaii", "Idaho",
                              "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                              "Louisiana", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                              "Mississipi", "Missouri", "Montana", "Nevada", "New Jersey",
                              "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma",
                              "Oregon", "Pennsylvania", "South Carolina", "Tennessee", "Texas",
                              "Utah", "Virginia", "Washington", "Wisconsin")),
      selectInput("deliveryInput", "Type of Delivery",
                  choices = c("Vaginal Birth",
                              "C-Section")),
      selectInput("insuranceInput", "Insurance Coverage",
                  choices = c("With Insurance",
                              "Without Insurace")),
      p("Type of Delivery and Insurance Coverage selections only work for the 'Cost to Mother' Measure of Interest.")
     
    ),
    
    # Show the map and table
    mainPanel(
      
      highchartOutput("map"),
      p("Unfortunately CDC does not have complete information for every state and every year. 
         However, we can still see that overall the maternal mortality rate declines from 1999 to 2017."),
      plotlyOutput("plot")
      
      
    )
  )


)



server = function(input, output, session) {
  

  
  
  # Output plot
  output$map = renderHighchart({
    
    if(input$typeInput == "Maternal Mortality Rate") {
    
    year_df = final_merged_data %>%
      filter(year == input$yearInput)
    data("usgeojson")
    
    
    highchart() %>%
    hc_add_series_map(usgeojson, year_df, name = "maternal mortality per 100,000 births",
                      value = "mortality_per_100000_births", joinBy =  c("woename", "state"),
                      dataLabels = list(enabled = TRUE,
                                        format = '{point.properties.postalcode}')) %>%
    hc_colorAxis(stops = colstops) %>%
    hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_title(text = "Maternal Mortality Trend Across States In a Given Year",
             margin = 20, align = "center",
             style = list(color = "#013220",  fontWeight = "bold")) %>%
    hc_subtitle(text = "Data Source: CDC Wonder",
                  align = "left") 
    } else if(input$typeInput == "Cesarean Rate") {
      highchart() %>%
        hc_add_series_map(usgeojson, final_merged_data, name = "cesarean rate",
                          value = "c_rate", joinBy =  c("woename", "state"),
                          dataLabels = list(enabled = TRUE,
                                            format = '{point.properties.postalcode}')) %>%
        hc_colorAxis(stops = colstops) %>%
        hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_title(text = "Cesarean Rate Across States",
                 margin = 20, align = "center",
                 style = list(color = "#013220",  fontWeight = "bold")) %>%
        hc_subtitle(text = "Data Source: CDC Wonder",
                    align = "left") 
    } else {
      
      deliveryType = ifelse(input$deliveryInput == "Vaginal Birth", "Vaginal_birth", "C-section")
      insuranceType = ifelse(input$insuranceInput == "With Insurance", "with_ins", "without_ins")
      
      cost_df = final_merged_data %>%
        filter(type == deliveryType) %>%
        filter(insurance == insuranceType) 
      
      highchart() %>%
        hc_add_series_map(usgeojson, cost_df, name = "Cost to Mothers in Dollars($)",
                          value = "cost", joinBy =  c("woename", "state"),
                          dataLabels = list(enabled = TRUE,
                                            format = '{point.properties.postalcode}')) %>%
        hc_colorAxis(stops = colstops) %>%
        
        hc_mapNavigation(enabled = TRUE) %>%
        hc_title(text = "Cost to Mothers of Giving Birth Across States",
                 margin = 20, align = "center",
                 style = list(color = "#013220",  fontWeight = "bold")) %>%
        hc_subtitle(text = "Data Source: CDC Wonder",
                    align = "left") 
    }
  })
  
  output$plot = renderPlotly({
    
    if(input$typeInput == "Maternal Mortality Rate"){
    filtered =
      final_merged_data %>%
      filter(state == input$stateInput) 
    
   plot_ly(filtered, x = ~year, y = ~mortality_per_100000_births,
           color = ~mortality_per_100000_births, type = "bar",
           text = ~paste("Clarity: ", mortality_per_100000_births))
    
   
    } else if(input$typeInput == "Cesarean Rate") {
      
      final_merged_data %>%
        select(state, c_rate) %>%
        distinct() %>%
        mutate(state = fct_reorder(state, c_rate)) %>%
      plot_ly(
              x = ~state, y= ~c_rate, color = ~state, type = "bar")
    } else{
      NULL
    }
 
  })
  

 
  
}



shinyApp(ui = ui, server = server)