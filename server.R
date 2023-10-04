library(reticulate)
library(tensorflow)
library(keras)
library(shiny)

#use_virtualenv("r-tensorflow")

setwd("I:\\workspace\\spreadgui")

# new_model <- load_model_tf('gap')
# new_model <- compile(new_model)
# ###For Gap case
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
#                  )
# 
# simulate_spread<- function( bed_slope_angle=0.0,
#                             bed_width=50.0,
#                             fuel_clump_size=1.0,
#                             fuel_depth=0.5,
#                             fuel_gap_size=0.15,
#                             fuel_loading=1.0,
#                             ignition_depth=1.0,
#                             particle_diameter=0.0035,
#                             particle_moisture=2.0,
#                             wind_mean=3.0
#     
#   ) 
# {
#   #-------------------------------------
#   # Rescaling Inputs
#   #-------------------------------------
#   
#   min_bed_slope = 0.0
#   max_bed_slope = 30.0
#   bed_slope_scale = (bed_slope_angle - min_bed_slope) / max_bed_slope
#   
#   min_bed_width = 0.0
#   max_bed_width = 30.0
#   bed_width_scale = (bed_width - min_bed_width) / max_bed_width
#   
#   min_fuel_clump_size = 0.0
#   max_fuel_clump_size = 30.0
#   fuel_clump_size_scale = (fuel_clump_size - min_fuel_clump_size) / max_fuel_clump_size
#   
#   min_fuel_depth = 0.0
#   max_fuel_depth= 30.0
#   fuel_depth_scale = (fuel_depth - min_fuel_depth) / max_fuel_depth
#   
#   min_fuel_gap_size = 0.0
#   max_fuel_gap_size= 30.0
#   fuel_gap_size_scale = (fuel_gap_size - min_fuel_gap_size) / max_fuel_gap_size
#   
#   min_fuel_loading = 0.0
#   max_fuel_loading = 30.0
#   fuel_loading_scale = (fuel_loading - min_fuel_loading) / max_fuel_loading
#   
#   min_ignition_depth = 0.0
#   max_ignition_depth = 30.0
#   ignition_depth_scale = (ignition_depth - min_ignition_depth) / max_ignition_depth
#   
#   min_particle_diameter = 0.0
#   max_particle_diameter = 30.0
#   particle_diameter_scale = (particle_diameter - min_particle_diameter) / max_particle_diameter
#   
#   min_particle_moisture = 0.0
#   max_particle_moisture = 30.0
#   particle_moisture_scale = (particle_moisture - min_particle_moisture) / max_particle_moisture
#   
#   min_wind_mean = 0.0
#   max_wind_mean = 30.0
#   wind_mean_scale = (wind_mean - min_wind_mean) / max_wind_mean
#   
#   
#   #-------------------------------------
#   # Simulation
#   #-------------------------------------
#   
#   ##Single input for now, set up for multiple inputs
#   
#   fuel_bed_data_frame <- as.matrix(cbind(bed_slope_scale,
#                                          bed_width_scale,
#                                          fuel_clump_size_scale,
#                                          fuel_depth_scale,
#                                          fuel_gap_size_scale,
#                                          fuel_loading_scale,
#                                          ignition_depth_scale,
#                                          particle_diameter_scale,
#                                          particle_moisture_scale,
#                                          wind_mean_scale
#                                          
#   ))
#   
#   
#   
#   spread_predict <- predict(new_model, fuel_bed_data_frame)
#   
#   return(spread_predict)
# }

###For No Gap case

new_model <- load_model_tf('no_gap')
new_model <- compile(new_model)
paramNames <- c( 'bed_slope_angle',
                 'bed_width',
                 #'fuel_clump_size',
                 'fuel_depth',
                # 'fuel_gap_size',
                 'fuel_loading',
                 'ignition_depth',
                 'particle_diameter',
                 'particle_moisture',
                 'wind_mean'
                 )

simulate_spread<- function( bed_slope_angle=0.0,
                            bed_width=50.0,
                            #fuel_clump_size=1.0,
                            fuel_depth=0.5,
                            #fuel_gap_size=0.15,
                            fuel_loading=1.0,
                            ignition_depth=1.0,
                            particle_diameter=0.0035,
                            particle_moisture=2.0,
                            wind_mean=3.0

  )

{
  #-------------------------------------
  # Rescaling Inputs
  #-------------------------------------
  
  min_bed_slope = 0.0
  max_bed_slope = 30.0
  bed_slope_scale = (bed_slope_angle - min_bed_slope) / max_bed_slope
  
  min_bed_width = 0.0
  max_bed_width = 30.0
  bed_width_scale = (bed_width - min_bed_width) / max_bed_width
  
  # min_fuel_clump_size = 0.0
  # max_fuel_clump_size = 30.0
  # fuel_clump_size_scale = (fuel_clump_size - min_fuel_clump_size) / max_fuel_clump_size
  
  min_fuel_depth = 0.0
  max_fuel_depth= 30.0
  fuel_depth_scale = (fuel_depth - min_fuel_depth) / max_fuel_depth
  
  # min_fuel_gap_size = 0.0
  # max_fuel_gap_size= 30.0
  # fuel_gap_size_scale = (fuel_gap_size - min_fuel_gap_size) / max_fuel_gap_size
  
  min_fuel_loading = 0.0
  max_fuel_loading = 30.0
  fuel_loading_scale = (fuel_loading - min_fuel_loading) / max_fuel_loading
  
  min_ignition_depth = 0.0
  max_ignition_depth = 30.0
  ignition_depth_scale = (ignition_depth - min_ignition_depth) / max_ignition_depth
  
  min_particle_diameter = 0.0
  max_particle_diameter = 30.0
  particle_diameter_scale = (particle_diameter - min_particle_diameter) / max_particle_diameter
  
  min_particle_moisture = 0.0
  max_particle_moisture = 30.0
  particle_moisture_scale = (particle_moisture - min_particle_moisture) / max_particle_moisture
  
  min_wind_mean = 0.0
  max_wind_mean = 30.0
  wind_mean_scale = (wind_mean - min_wind_mean) / max_wind_mean
  
  
  #-------------------------------------
  # Simulation
  #-------------------------------------
  
  ##Single input for now, set up for multiple inputs
  
  fuel_bed_data_frame <- as.matrix(cbind(bed_slope_scale,
                                             bed_width_scale,
                                             #fuel_clump_size_scale,
                                             fuel_depth_scale,
                                            # fuel_gap_size_scale,
                                             fuel_loading_scale,
                                             ignition_depth_scale,
                                             particle_diameter_scale,
                                             particle_moisture_scale,
                                             wind_mean_scale
                                             
  ))
  
  
  
  spread_predict <- predict(new_model, fuel_bed_data_frame)
  
  return(spread_predict)
}

plot_spread <- function(spread_predict) {
  
  layout(matrix(c(1,1)))
  
  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))
  
  # plot all scenarios
  
  barplot(spread_predict)
}

function(input, output, session) {
  
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params
  }
  
  spreadA <- reactive(do.call(simulate_spread, getParams("a")))
  output$a_distPlot <- renderPlot({
    plot_spread(spreadA())
  })
  
  
}