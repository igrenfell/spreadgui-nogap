library(shiny)

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             sliderInput(paste0(prefix, "_", "degrees"), "Bed Slope Angle (in Degrees):", min = 0, max = 30, value = 15),
             sliderInput(paste0(prefix, "_", "bed_width"), "Bed width (meters) :", min = 1, max = 50, value = 25, step = 1),
             sliderInput(paste0(prefix, "_", "fuel_depth"), "Fuel Depth (meters)", min = 0.5, max = 1, value = 0.5, step = 0.05),
             sliderInput(paste0(prefix, "_", "fuel_loading"), "Fuel Loading (kg/m^3)", min = 0.050, max = 3.0, value = 1.0, step = 0.01)
             
             ),
      column(6,
             sliderInput(paste0(prefix, "_", "ignition_depth"), "Fuel Loading (meters)", min = 0.1, max = 4.0, value = 1.0, step = 0.01),
             sliderInput(paste0(prefix, "_", "particle_diameter"), "Particle Diameter (meters:", min = 0.001, max = .005, value = .0035, step = 0.0005),
             sliderInput(paste0(prefix, "_", "particle_moisture"), "Particle Moisture (%)", min = 2, max = 35, value = 2, step = 0.5),
             sliderInput(paste0(prefix, "_", "wind_mean"), "Mean Wind Speed (m/s)", min = 1, max = 10, value = 3.0, step = 0.1),
           
      )
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
          
          # Application title
          # tags$h2("Retirement: simulating wealth with random returns, inflation and withdrawals"),
          # p("An adaptation of the",
          #   tags$a(href="https://systematicinvestor.wordpress.com/2013/04/06/retirement-simulating-wealth-with-random-returns-inflation-and-withdrawals-shiny-web-application/", "retirement app"),
          #   "from",
          #   tags$a(href="http://systematicinvestor.wordpress.com/", "Systematic Investor"),
          #   "to demonstrate the use of Shiny's new grid options."),
          # hr(),
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