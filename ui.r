
library(shiny)
# 
# 
# paramNames <- c( 'bed_slope_angle',
#                  'bed_width',
#                  'fuel_clump_size',
#                  'fuel_depth',
#                  'fuel_gap_size',
#                  'fuel_loading',
#                  'ignition_depth',
#                  'particle_diameter',
#                  'particle_moisture',
#                  'wind_mean'
# )


renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(4,
             sliderInput(bed_slope_angle, "Bed Slope Angle (degrees):", min = 0.0, max = 30.0, value = 0.0),
             sliderInput(bed_width, "Bed Width (meters)", min = 1.0, max = 50.0,  value = 50.0, step = 1.0),
             sliderInput(fuel_depth, "Fuel Depth (meters)", min = 0.05, max = 1.0, value = 0.5, step = 0.1),
             sliderInput(fuel_loading, "Fuel Loading (kg/m^3)", min = 0.05, max = 3.0, value = 1.0, step = 0.05)
             
      ),
      column(4,
             sliderInput(ignition_depth, "Ignition Depth (meters)", min = 0.1, max = 4.0, value = 1.0, step = 0.1),
             sliderInput(particle_diameter, "Particle Diameter (meters)", min = 0.001, max = .005, value = 0.0035, step = 0.001),
             sliderInput(particle_moisture, "Particle Moisture (%)", min = 2.0, max = 35.0, value = 2.0, step =0.1),
             sliderInput(wind_mean, "Average Wind Speed (m/s)", min = 0.0, max = 10.0, value = 3.0, step = 0.1)
             
      )
    )
  )
}



# Define UI for application that plots random distributions
fluidPage(
          fluidRow(
            column(1, tags$h3("Scenario A"))
          ),
          fluidRow(
            column(1, renderInputs("a"))
          ),
          fluidRow(
            column(1,
                   plotOutput("a_distPlot", height = "600px")
            )
            
          )
)