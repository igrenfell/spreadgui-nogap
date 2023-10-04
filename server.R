# paramNames <- c("start_capital", "annual_mean_return", "annual_ret_std_dev",
#                 "annual_inflation", "annual_inf_std_dev", "monthly_withdrawals", "n_obs",
#                 "n_sim")

paramNames <- c("degrees", "bed_width", "fuel_depth",
                "fuel_loading", "ignition_depth", "particle_diameter", "particle_moisture",
                "wind_mean")
use_virtualenv("G:\\tensorflow\\venv")
setwd("G:\\tensorflow\\modelProtocolBuffers")
new_model <- load_model_tf('no_gap')

nmc <- compile(new_model)

predict_spread <- function(degrees = 0, bed_width = 50,
                           fuel_depth = 0.5, fuel_loading = 1.0,
                           ignition_depth = 1.0, particle_diameter = 0.0035, particle_moisture = 2.0, wind_mean = 3.0)
  {
  #-------------------------------------
  # Inputs
  #-------------------------------------
  
  #Normalizing slope 
  numerator <- degrees - 0
  denominator <- 30 - 0
  degrees <- numerator / denominator
  
  #Normalizing bed_width 
  numerator <- bed_width - 1
  denominator <- 50 - 1
  bed_width <- numerator / denominator
  #Normalizing fuel_depth 
  numerator <- fuel_depth - 0.05
  denominator <- 1 - 0.05
  fuel_depth <- numerator / denominator
  #Normalizing fuel_loading 
  numerator <- fuel_loading - 0.05
  denominator <- 3 - 0.05
  fuel_loading <- numerator / denominator
  #Normalizing ignition_depth 
  numerator <- ignition_depth - 0.1
  denominator <- 4 - 0.1
  ignition_depth <- numerator / denominator
  #Normalizing particle_diameter 
  numerator <- particle_diameter - 0.001
  denominator <- 0.005 - 0.001
  particle_diameter <- numerator / denominator
  #Normalizing particle_moisture 
  numerator <- particle_moisture - 2
  denominator <- 35 - 2
  particle_moisture <- numerator / denominator
  #Normalizing wind_mean 
  numerator <- wind_mean - 1
  denominator <- 10 - 1
  wind_mean <- numerator / denominator

  #-------------------------------------
  # Prediction
  #-------------------------------------
  
  predvec <- c(degrees,
               bed_width,
               fuel_depth,
               fuel_loading,
               ignition_depth,
               particle_diameter,
               particle_moisture,
               wind_mean)
  
 
  x <- t(as.matrix(cbind(predvec)))
  pred.output <- nmc %>% predict(x)
  pred.output
  
  ###Normalize output
  numerator <- pred.output[1] -  0.0
  denominator <- 50.710646000000004 -  0.0
  flamelength <- numerator / denominator
  numerator <- pred.output[2] -  0.0
  denominator <- 67.77206 -  0.0
  fzd <- numerator / denominator
  numerator <- pred.output[3] -  0.0
  denominator <- 783.45548 -  0.0
  ros <- numerator / denominator
  
  outvec <- c(flamelength, fzd, ros)
    return(outvec)
}

plot_nav <- function(nav) {
  
  layout(matrix(c(1,1)))
  
  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))
  
  # plot all scenarios
  barplot(nav, names.arg= c("FL (m)", "FZD (m)", "ROS (m/min)"))
  
  grid()
  
}

# Define server logic required to generate and plot a random distribution
#
# Idea and original code by Pierre Chretien
# Small updates by Michael Kapler
#
function(input, output, session) {
  
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params
  }
  
  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  navA <- reactive(do.call(predict_spread, getParams("a")))

  # Expression that plot NAV paths. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  output$a_distPlot <- renderPlot({
    plot_nav(navA())})
    # 
    # output$b_distPlot <- renderPlot({
    #   plot_nav(navB())
}
