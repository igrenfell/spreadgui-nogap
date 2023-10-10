library(here)
library(tensorflow)
library(keras)

setwd(here())
# paramNames <- c("start_capital", "annual_mean_return", "annual_ret_std_dev",
#                 "annual_inflation", "annual_inf_std_dev", "monthly_withdrawals", "n_obs",
#                 "n_sim")

#use_virtualenv("G:\\tensorflow\\venv")
#setwd("G:\\tensorflow\\modelProtocolBuffers")
new_model <- load_model_tf('no_gap')

nmc <- compile(new_model)
ycolnames <- c("fzd", "flength", "ros")

paramNames <- c("bed_slope_angle", "bed_width", "fuel_depth",
                "fuel_loading", "ignition_depth", "particle_diameter", "particle_moisture",
                "wind_mean", "xvar", "yvar")

predict_spread <- function(bed_slope_angle = 0, bed_width = 50,
                           fuel_depth = 0.5, fuel_loading = 1.0,
                           ignition_depth = 1.0, particle_diameter = 0.0035, particle_moisture = 2.0, wind_mean = 3.0, xvar, yvar, levvar)
{
 
  xcolnum <- which(xvar == paramNames)
  ycolnum <- which(yvar == ycolnames)
  levcolum <- which(levvar == paramNames)
  nlevs = 5
  #-------------------------------------
  # Inputs
  #-------------------------------------
  print(bed_slope_angle)
  
  #Normalizing slope 
  min_degrees <- 0
  max_degrees <- 30
  numerator <- bed_slope_angle - min_degrees
  denominator <- max_degrees - min_degrees
  degrees.scale <- numerator / denominator
  
  #Normalizing bed_width 
  min_bed_width <-  1
  max_bed_width <- 50
  numerator <- bed_width - min_bed_width
  denominator <- max_bed_width - min_bed_width
  bed_width.scale <- numerator / denominator
  #Normalizing fuel_depth 
  min_fuel_depth <- 0.05
  max_fuel_depth <-  1 
  numerator <- fuel_depth - min_fuel_depth
  denominator <- max_fuel_depth - min_fuel_depth
  fuel_depth.scale <- numerator / denominator
  #Normalizing fuel_loading 
  min_fuel_loading <- 0.05
  max_fuel_loading <- 3
  numerator <- fuel_loading -min_fuel_loading
  denominator <- 3 - min_fuel_loading
  fuel_loading.scale <- numerator / denominator
  #Normalizing ignition_depth 
  min_ignition_depth <- 0.1
  max_ignition_depth  <- 4
  numerator <- ignition_depth - min_ignition_depth
  denominator <- max_ignition_depth - min_ignition_depth
  ignition_depth.scale <- numerator / denominator
  #Normalizing particle_diameter 
  min_particle_diameter <- 0.001
  max_particle_diameter <- 0.005
  numerator <- particle_diameter - min_particle_diameter
  denominator <- max_particle_diameter - min_particle_diameter
  particle_diameter.scale <- numerator / denominator
  #Normalizing particle_moisture 
  min_particle_moisture <- 2
  max_particle_moisture<- 35
  numerator <- particle_moisture - min_particle_moisture
  denominator <- max_particle_moisture - min_particle_moisture
  particle_moisture.scale <- numerator / denominator
  #Normalizing wind_mean 
  min_wind_speed <- 1
  max_wind_speed <- 10
  numerator <- wind_mean - min_wind_speed
  denominator <- max_wind_speed - min_wind_speed
  wind_mean.scale <- numerator / denominator
  
  min_x_vec <- c(min_degrees, 
                 min_bed_width, 
                 min_fuel_depth, 
                 min_fuel_loading, 
                 min_ignition_depth, 
                 min_particle_moisture, 
                 min_particle_moisture, 
                 min_wind_speed
  )
  
  
  max_x_vec <- c(max_degrees, 
                 max_bed_width, 
                 max_fuel_depth, 
                 max_fuel_loading, 
                 max_ignition_depth, 
                 max_particle_moisture, 
                 max_particle_moisture, 
                 max_wind_speed
  )
  
  levseq <- seq(0, 1, length = 5)
  
  #-------------------------------------
  # Prediction
  #-------------------------------------
  
  predvec <- c(degrees.scale,
               bed_width.scale,
               fuel_depth.scale,
               fuel_loading.scale,
               ignition_depth.scale,
               particle_diameter.scale,
               particle_moisture.scale,
               wind_mean.scale)
  
  
  x <- t(as.matrix(cbind(predvec)))
  xrep <- rbind(rep(x, 10))
  xrep <- matrix(xrep, nrow = 10, byrow = TRUE)
  #colnames(xrep) <- x_axis_vars
  xparamnames <- (x_axis_vars)
  xcolnum <- which(xparamnames == xvar)
  # xcolnum <- 1
  min_degrees <- 0
  max_degrees <- 30
  numerator <- bed_slope_angle - min_degrees
  denominator <- max_degrees - min_degrees
  degrees.scale <- numerator / denominator
  
  tempxvals <- seq(min_x_vec[xcolnum], max_x_vec[xcolnum], length = 10)
  numerator <- tempxvals - min_x_vec[xcolnum]
  denominator <- max_x_vec[xcolnum] - min_x_vec[xcolnum]
  temp.pred.x <- numerator / denominator
  xrep[,xcolnum] <- temp.pred.x
  nreps <- length(levseq)
  xrep.list <- vector("list", nreps)
  for(i in 1:nreps)
  {
    tempmat <- xrep
    tempmat[,levcolum] <- rep(levseq[i], dim(xrep)[1])
    xrep.list[[i]] <- tempmat
    
  }
  xrep.all <- do.call("rbind", xrep.list)
  
  #xrep[,xcolnum] <- seq(0, 30, length = 10)
  
  #print(xrep[,xcolnum])
  pred.output <- nmc %>% predict(xrep.all)
  #pred.output <- nmc %>% predict(x)
  
  pred.output
  
  ###Normalize output
  numerator <- pred.output[,1] -  0.0
  denominator <- 50.710646000000004 -  0.0
  flamelength <-  pred.output[,1] * 50.710646000000004
  numerator <- pred.output[,2] -  0.0
  denominator <- 67.77206 -  0.0
  fzd <- pred.output[,2] *67.77206
  numerator <- pred.output[,3] -  0.0
  denominator <- 783.45548 -  0.0
  ros <-pred.output[,3] * 783.45548
  
  outmat<- cbind(flamelength, fzd, ros)
  ycolnum <- which(ycolnames == yvar)
  ycollist <- vector("list", nreps)
  startseq <- seq(1,41, by = 10)
  endseq <- seq(10, 50,by=10)
  
  
  for(i in 1:nreps)
  {
    ycollist[[i]] <- outmat[,ycolnum][startseq[i]:endseq[i]]
    
  }
  
  ycolmat <- do.call("cbind", ycollist)
  
  colnames(outmat) <- ycolnames
  outlist <- vector("list", 5)
  outlist[[1]] <- xrep[,xcolnum]
#  outlist[[2]] <- outmat[,ycolnum]
  outlist[[2]] <- ycolmat
  outlist[[3]] <- xcolnum
  outlist[[4]] <- ycolnum
  outlist[[5]] <- tempxvals
  
  return(outlist)
}

plot_nav <- function(nav) {
  

  layout(matrix(c(1,1)))
  
  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))
  startseq <- seq(1,41, by = 10)
  endseq <- seq(10, 50,by=10)
  
  tempx <- nav[[1]]
  xcoltemp <- nav[[3]]
  ycoltemp <- nav[[4]]
  orig.x  <-  nav[[5]]
  
  ymattemp <-  nav[[2]]
  #ymmattemp <-  outmat[,ycolnum]
 # ymattemp <- matrix(ymattemp, nrow = 10, byrow = TRUE)
  matplot(orig.x, ymattemp, type=  "n", xlab = paramNames[xcoltemp], ylab = ycolnames[ycoltemp])
  matlines(orig.x, ymattemp, col = rainbow(5), xlab = paramNames[xcoltemp], ylab = ycolnames[ycoltemp])
  
  grid()

}

function(input, output, session) {
  navA <- reactive({predict_spread(bed_slope_angle = input$bed_slope_angle,
                                   bed_width =  input$bed_width,
                                   fuel_depth=  input$fuel_depth, 
                                   fuel_loading=  input$fuel_loading, 
                                   ignition_depth=    input$ignition_depth, 
                                   particle_diameter=  input$particle_diameter, 
                                   particle_moisture=  input$particle_moisture, 
                                   wind_mean=input$wind_mean, 
                                   xvar=  input$xvar, 
                                   yvar= input$yvar,
                                   levvar = input$levvar
  )
    
    
  })
  output$a_distPlot <- renderPlot({
    
    plot_nav(navA())  }
  )
}