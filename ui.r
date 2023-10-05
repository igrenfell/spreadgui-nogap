library(shiny)

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(4,
             sliderInput("bed_slope_angle", "Bed Slope Angle (in Degrees):", min = 0, max = 30, value = 15),
             sliderInput("bed_width", "Bed width (meters) :", min = 1, max = 50, value = 25, step = 1),
             sliderInput( "fuel_depth", "Fuel Depth (meters)", min = 0.05, max = 1, value = 0.5, step = 0.05),
             sliderInput("fuel_loading", "Fuel Loading (kg/m^3)", min = 0.050, max = 3.0, value = 1.0, step = 0.01)
             
      ),
      column(4,
             sliderInput("ignition_depth", "Ignition Depth (meters)", min = 0.1, max = 4.0, value = 1.0, step = 0.01),
             sliderInput("particle_diameter", "Particle Diameter (meters:", min = 0.001, max = .005, value = .0035, step = 0.0005),
             sliderInput("particle_moisture", "Particle Moisture (%)", min = 2, max = 35, value = 2, step = 0.5),
             sliderInput("wind_mean", "Mean Wind Speed (m/s)", min = 1, max = 10, value = 3.0, step = 0.1),
             
      ),
      selectInput("xvar", "X-axis variable", x_axis_vars),
      selectInput("yvar", "Y-axis variable", y_axis_vars)
      
    )#,
    # p(actionButton(paste0(prefix, "_", "recalc"),
    #                "Re-run simulation", icon("random")
    #)
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
            column(6, tags$h3("Scenario A"))
          ),
          fluidRow(
            column(6, renderInputs("a"))          ),
          fluidRow(
            column(6,
                   plotOutput("a_distPlot", height = "600px")
            )
          )
          
)