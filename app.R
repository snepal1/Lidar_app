library(shiny)
library(lidR)
library(raster)
library(plotly)
library(sf)
library(shinycssloaders)  # Load shinycssloaders package

# Define UI for application that draws a LiDAR plot
ui <- fluidPage(
  # Application title
  titlePanel("LiDAR Data Analysis for Small Area"),
  
  fileInput("upload", NULL, accept = c(".las", ".laz")),
  actionButton("processData", "Process Data"),
  actionButton("clearData", "Clear Data"),
  
  shinycssloaders::withSpinner(
    tags$div(id = "processingMessage", style = "display: none;", "Processing data..."),
    type = 4, color = "#0dc5c1"  # Specify the type and color of the spinner
  ),
  
  verbatimTextOutput("progressOutput"),

  fluidRow(
    column(6, plotlyOutput("lidar3DPlot", height = "800px")),
    column(6, plotlyOutput("normalPlot", height = "800px"))
  ),
  tags$div(class = "vertical-gap"),
  
  fluidRow(
    column(6, plotOutput("CHM", height = "800px")),
    column(6, plotOutput("CHM_shape", height = "800px"))
  ),
    tags$div(class = "vertical-gap")
  
)

options(shiny.maxRequestSize = 1000 * 1024^2)

# Define server logic required to draw LiDAR data
server <- function(input, output, session) {
  # Reactive values to store LiDAR data
  lidar_data <- reactiveVal(NULL)
  las_data <- reactiveVal(NULL)
  chm_data<-reactiveVal(NULL)
  shape_data<-reactiveVal(NULL)
  
  # Progress indicator
  progress <- reactiveVal(0)
  
  observe({
    progress_val <- progress()
    
    if (progress_val < 100) {
      shinyjs::enable("processingMessage")
    } else {
      shinyjs::disable("processingMessage")
    }
  })
  
  observe({
    # Check if the "Clear Data" button is clicked
    if (input$clearData > 0) {
      # Clear the reactive values
      lidar_data(NULL)
      las_data(NULL)
      chm_data(NULL)
      shape_data<-(NULL)
      progress(0)
    }
  })
  
  observeEvent(input$processData, {
    # Check if a LiDAR file is uploaded
    if (!is.null(input$upload) && !is.null(input$upload$datapath)) {
      # Show processing message
      shinyjs::enable("processingMessage")
      
      # Clear previous LiDAR data
      lidar_data(NULL)
      las_data(NULL)
      chm_data<-(NULL)
      shape_data<-(NULL)
      
      # Read the LiDAR file using lidR
      las <- readLAS(input$upload$datapath, filter= "-keep_first -drop_z_above 2000")
      las1 <- classify_ground(las, csf())
      lidar <- normalize_height(las, tin())
      chm<-rasterize_canopy(lidar,0.5, pitfree(subcircle=0.2))
      ttops<-locate_trees(lidar, lmf(ws=5))
      algo <- dalponte2016(chm, ttops)
      las2 <- segment_trees(lidar, algo)
      crowns <- crown_metrics(las2, func = .stdmetrics, geom = "convex")
      # Extract coordinates and attributes for non-normalized data
      las_df <- data.frame(
        X = las@data$X,
        Y = las@data$Y,
        Z = las@data$Z
      )
      
      # Extract coordinates and attributes
      lidar_df <- data.frame(
        X = lidar@data$X,
        Y = lidar@data$Y,
        Z = lidar@data$Z
      )
      
      # Store LiDAR data in reactive value
      lidar_data(lidar_df)
      # Store LiDAR data in reactive value
      las_data(las_df)
      chm_data(chm)
      shape_data(crowns)
      # Set progress to 100 to indicate completion
      progress(100)
    }
  })
  
  # Render LiDAR 3D plot
  output$lidar3DPlot <- renderPlotly({
    # Get LiDAR data from reactive value
    las <- las_data()
    
    if (!is.null(las)) {
      # Plot LiDAR data using plot_ly from plotly
      plot_ly(
        data = las, 
        x = ~X, 
        y = ~Y, 
        z = ~Z, 
        type = "scatter3d", 
        mode = "markers",
        color = ~Z,  # Color based on height (Z)
        colors = colorRamp(c("blue", "green", "yellow", "red")),  # Gradient colors
        marker = list(
          size = 1  # Adjust the size as needed for visibility
        )
      ) %>%
        layout(
          scene = list(
            xaxis = list(title = "UTM X"),
            yaxis = list(title = "UTM Y"),
            zaxis = list(title = "Height (m)")
          )
        )
    } else {
      # If no LiDAR data is available, display a message
      cat("No LiDAR data available.")
    }
  })
  
  # Render LiDAR 3D plot
  output$normalPlot <- renderPlotly({
    # Get LiDAR data from reactive value
    lidar <- lidar_data()
    
    if (!is.null(lidar)) {
      # Plot LiDAR data using plot_ly from plotly
      plot_ly(
        data = lidar, 
        x = ~X, 
        y = ~Y, 
        z = ~Z, 
        type = "scatter3d", 
        mode = "markers",
        color = ~Z,  # Color based on height (Z)
        colors = colorRamp(c("blue", "green", "yellow", "red")),  # Gradient colors
        marker = list(
          size = 1  # Adjust the size as needed for visibility
        )
      ) %>%
        layout(
          scene = list(
            xaxis = list(title = "UTM X"),
            yaxis = list(title = "UTM Y"),
            zaxis = list(title = "Height (m)")
          )
        )
    } else {
      # If no LiDAR data is available, display a message
      cat("No LiDAR data available.")
    }
  })
  
  output$CHM <- renderPlot({
    # Get data from reactive value
    chm <- chm_data()
    #ttops<-ttops_data()
    if (!is.null(chm)) {
    # Plot convex hull area
    plot(chm,col=height.colors(50))
    } else {
      print("No CHM data available.")
    }
  })
  
  output$CHM_shape <- renderPlot({
    # Get data from reactive value
    crown <- shape_data()
    #ttops<-ttops_data()
    if (!is.null(crown)) {
      # Plot convex hull area
      plot(sf:: st_geometry(crown),main= "Crown area")
    } else {
      print("No shape data available.")
    }
  })
  
  # Render progress indicator
  output$progressOutput <- renderText({
    paste("Processing progress: ", progress(), "%")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
