source("/remote/complex/home/fpetit/spot_the_eez/R/convert_dms_to_decimal.R")
source("/remote/complex/home/fpetit/spot_the_eez/R/detect_zee.R")

server <- function(input, output) {
  
  # Reactive values to store data and processed data
  data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  
  # Load CSV data
  observeEvent(input$file, {
    req(input$file)
    data(readxl::read_excel(input$file$datapath, skip = 1) |> janitor::clean_names() |> 
           select(longitude_start, latitude_start) |> 
           rename("longitude" = longitude_start,
                  "latitude" = latitude_start) |> 
           na.omit())
  })
  
  # Process data when the button is clicked
  # Process data when the button is clicked
  observeEvent(input$process_button, {
    if (!is.null(data())) {
      # Apply your custom processing function to latitude and longitude columns
      data_processed <- data() %>%
        rowwise() |> 
        mutate(
          longitude = convert_dms_to_decimal(longitude),  # Modify 'convert_dms' based on your function
          latitude = convert_dms_to_decimal(latitude)    # Modify 'convert_dms' based on your function
        )
      
      data_processed_with_zee <- detect_zee(data_processed, "/remote/complex/home/fpetit/spot_the_eez/Data/World_EEZ_v12_20231025")
      processed_data(data_processed_with_zee)
    }
  })
  
  worldmap <- rnaturalearth::ne_countries(scale = 'medium', type = 'map_units',
                                          returnclass = 'sf')
  
  med_cropped <- st_crop(worldmap, xmin = -5, xmax = 25,
                         ymin = 35, ymax = 48)
  
  

  # Render ggplot map
  output$ggplot_map <- renderPlot({
    if (!is.null(processed_data())) {
      ggplot(processed_data()) +
        geom_sf(data = med_cropped)+
        geom_point(aes(x = longitude, y = latitude, fill = zee), shape = 21, colour = "black", size = 2)+
        scale_fill_brewer(palette = "Pastel1")+
        theme_bw()+
        ggtitle("Map of samples")
    }
  })
  
  #Download map
  
  # Download map as PNG
  output$download_map <- downloadHandler(
    filename = function() {
      paste("map-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, output$ggplot_map())
    }
  )
}