library(shiny)

renderInputs <- function(prefix) {
 
  wellPanel(
    
    
    titlePanel("L1GHTFIRE-ML"),
    column(width = 4, offset = 0, 
           img(src='Google_2015_logo.svg.png', align = "right", height="100%", width="100%")),
    column(width = 4, offset = 0, 
           img(src='Logo_of_the_United_States_Forest_Service.svg.png', align = "right", height="50%", width="50%")),
    fluidRow(
      
      selectInput("levvar", "Level variable", x_axis_vars, width ="50%"),
      selectInput("xvar", "X-axis variable", x_axis_vars, width = "50%"),
      selectInput("yvar", "Y-axis variable", y_axis_vars, width = "50%"),
      
      column(width = 6, offset = 0,
             sliderInput("bed_slope_angle", "Bed Slope Angle (in Degrees):", min = 0, max = 30, value = 15),
             sliderInput("bed_width", "Bed width (meters) :", min = 1, max = 50, value = 25, step = 1),
             sliderInput( "fuel_depth", "Fuel Depth (meters)", min = 0.05, max = 1, value = 0.5, step = 0.05),
             sliderInput("fuel_loading", "Fuel Loading (kg/m^3)", min = 0.050, max = 3.0, value = 1.0, step = 0.01)
             
      ),
      column(width = 6, offset =  0,
             sliderInput("ignition_depth", "Ignition Depth (meters)", min = 0.1, max = 4.0, value = 1.0, step = 0.01),
             sliderInput("particle_diameter", "Particle Diameter (meters):", min = 0.001, max = .005, value = .0035, step = 0.0005),
             sliderInput("particle_moisture", "Particle Moisture (%)", min = 2, max = 35, value = 2, step = 0.5),
             
             sliderInput("wind_mean", "Mean Wind Speed (m/s)", min = 1, max = 10, value = 3.0, step = 0.1),
             
      )
      
             
     
      
    ),
   
           
    uiOutput("bla1_ui"),  # here just for defining your ui
    uiOutput("bla2_ui")
    
  )
  
  
  
}

# Define UI for application that plots random distributions
fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),
          
          # wellPanel(
          #   selectInput("xvar", "X-axis variable", x_axis_vars, selected = "degrees"),
          #   selectInput("yvar", "Y-axis variable", y_axis_vars, selected = "ROS")
          # ),
          # 
          
          fluidRow(
            column(6, renderInputs("a"))          ),
          fluidRow(
            column(6,
                   plotOutput("a_distPlot", height = "600px")
            )
          )
          
)