library(shiny)
library(googleway)
library(tidyverse)
library(leaflet)
library(rgeos)

source("functions.R")

ui <- navbarPage('Easy Place Finder',
                 tabPanel('Map',
                          
                      div(class="outer",
                          
                          tags$head(
                            # Include our custom CSS
                            includeCSS("style.css")
                          ),
                              
                          leafletOutput('main_map', height = '100%', width = '100%')),
                        
                      # drop down arrow showing year
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 330, height = "auto",
                                    textInput("location_term", "Enter Address or City and State:", 
                                              placeholder = 'Durham, NC'),
                                    textInput("search_term", "Enter Search Term", placeholder = 'hospital'),
                                    actionButton("submit", "Submit"),
                                    actionButton("clear_map", "Clear Map")
                      )),
                 tabPanel('Data',
                      tags$h4('Search data:'),
                      downloadButton("download_data", "Download"),
                      actionButton("clear_data", "Clear Data"),
                      tableOutput('search_table')))

server <- function(input, output, session) {
  
  # create dataset of search results when search term changes
  search_results <- eventReactive(input$submit, {
    
    # require the location and search term be entered
    req(input$location_term)
    req(input$search_term)
    
    # use progress bar when searching for places
    withProgress(message = 'Finding places...', value = 0, {
      
      incProgress(0/1)
      
      map_center <- geocode_address(input$location_term)

      addresses <- get_google_places(map_center, input$search_term)
      
      # create seperate objects for address lat / long and place addresses, so we can
      # address lat / long to leaflet to center map
      list(map_center = map_center,
           addresses = addresses)
      
    })
  
  })
  

  # create base map
  output$main_map <- renderLeaflet(

    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      # centered on middle of US with wide zoom
      setView(lat = 38.8283, lng = -98.5795, zoom = 5)
  )
  
  # plot search results
  observeEvent(search_results(), {
    
    
    leafletProxy("main_map", session) %>%
      # reset view to address entered in location search box and zoom in
      setView(lat = search_results()$map_center[1], lng = search_results()$map_center[2], zoom = 10) %>%
        # add locations of places that come up in search
        # this will change interactively
        addMarkers(search_results()$addresses$lng, search_results()$addresses$lat,
                   popup = as.character(search_results()$addresses$name),
                   label = as.character(search_results()$addresses$name))
    
  })
  
  # clear points on map when user clicks 'Clear Map' button
  observeEvent(input$clear_map, {
    
    leafletProxy("main_map", session) %>%
      # remove places already on map
      clearMarkers()
    
  })
  
  # table of datasets ---------------------
  
  # initialize reactive to store search results dataset
  full_results <- reactiveValues(results = data.frame())
  
  observeEvent(search_results(), {
    
    full_results$results <- full_results$results %>%
      bind_rows(., search_results()$addresses)
    
  })
  
  # output table to store data
  output$search_table <- renderTable(full_results$results)
  
  # clear table of data when clear data action button is pressed
  observeEvent(input$clear_data, {
    
    full_results$results <- data.frame()
    
  })
  
  # download dataset
  # Downloadable csv of selected dataset ----
  output$download_data <- downloadHandler(
    
    filename = function() {
      "google-places-search.csv"
    },
    
    content = function(file) {
      write.csv(full_results$results, file, row.names = FALSE)
    }
  )
  
}
# Run the application 
shinyApp(ui = ui, server = server)
