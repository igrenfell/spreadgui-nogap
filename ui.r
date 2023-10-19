library(shiny)
library(bslib)

ui<-fluidPage(
    theme = bs_theme(version = 4, bootswatch = "united"),
    titlePanel("L1GHTFIRE-ML"),
    br(),
    fluidRow(
    column(width = 4, offset = 0, 
           print("The L1HTFire model (Linear 1-Dimensional Heat Transfer) is a physics-based wildland fire spread model created by the USFS Missoula Fire Sciences Laboratory. 
                 This is a Machine Learning (ML) Surrogate of L1HTFire and displays 3 output variables: fire spread rate, flame length, and flame zone depth. 
                 Google Research created the ML version using 575 million training runs of the physical model. The ML surrogate model is hosted here for demonstration purposes only in order to illustrate 
                 how wildland fire behavior results from the complex relationships between environmental and fire variables.")),
    column(width = 1, offset = 1, 
           img(src='Logo_of_the_United_States_Forest_Service.svg.png', align = "center", height="90%", width="30%")),

    column(width = 1, offset = 0, 
           img(src='Google_2015_logo.svg.png', align = "left", height="60%", width="60%")),
    ),
    br(),
    br(),
    br(),
    br(),
    fluidRow(
    column(5,
      wellPanel(
          selectInput("xvar", "X-axis variable", x_axis_vars, width = "50%",  selected = "bed_slope_angle"),
          selectInput("levvar", "Level variable", x_axis_vars, width ="50%",  selected = "wind_mean"),
          selectInput("yvar", "Y-axis variable", y_axis_vars, width = "50%",  selected = "ros"),
          
          fluidRow(
          column(width = 5, offset = 0,
                 sliderInput("bed_slope_angle", "Bed Slope Angle (in Degrees):", min = 0, max = 30, value = 15),
                 sliderInput("bed_width", "Bed width (meters) :", min = 1, max = 50, value = 25, step = 1),
                 sliderInput( "fuel_depth", "Fuel Depth (meters)", min = 0.05, max = 1, value = 0.5, step = 0.05),
                 sliderInput("fuel_loading", "Fuel Loading (kg/m^3)", min = 0.050, max = 3.0, value = 1.0, step = 0.01)),
          column(width = 6, offset =  0,
                 sliderInput("ignition_depth", "Ignition Depth (meters)", min = 0.1, max = 4.0, value = 1.0, step = 0.01),
                 sliderInput("particle_diameter", "Particle Diameter (meters):", min = 0.001, max = .005, value = .0035, step = 0.0005),
                 sliderInput("particle_moisture", "Particle Moisture (%)", min = 2, max = 35, value = 2, step = 0.5),
                 sliderInput("wind_mean", "Mean Wind Speed (m/s)", min = 1, max = 10, value = 3.0, step = 0.1))
        )
        ),
#        uiOutput("bla1_ui"),  # here just for defining your ui
#        uiOutput("bla2_ui")
      ),
    column(6,
        plotOutput("a_distPlot", height = "700px")
    )
    )
)

