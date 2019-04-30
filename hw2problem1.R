#' Almond Yield Anomaly function for timerange between 1988-2010
#'
#' A function to compute deviation in almond harvest from the average based on temperature and precipitation.

#' @param temp the average annual temperature, aggregated by montly data
#' @param temp_warm average annual temperature reflecting an average annual increase of 2 degrees celcius 
#' @param precip the average precipitation per year, aggregated by monthly data
#' @param precip_high a parameter reflecting an annual water year of 150% of the average
#' @param precip_low a parameter reflecting a water year with precipitation 50% of the average
#' @author Paige FitzGibbon
#' @return annual almond yield anomoly

hw2problem1 = function(temp, precip,
                       climate_change = 0, sensitivity_p = 0, sensitivity_t = 0,
                    a=-0.015, b=-0.0046, j=-0.07, k=0.0043, c=0.28) {
  if (precip < 0) return(NA)
  
  output = data.frame(year = 1989:2010, yield=NA)
 
   for (i in 1:nrow(output)) {
    
    if((sensitivity_t != 0)&(sensitivity_p != 0)){
      yield = a*(temp[i]+ climate_change + sensitivity_t) - b*(temp[i] + climate_change + sensitivity_t)^2 - j*(precip[i] + sensitivity_precip) + k*(precip[i] + sensitivity_p)^2 + c
      
      output$yield[i] = yield
    }
     else if(sensitivity_tmin != 0){
       yield_calculation = a*(temp[i]+climate_change + sensitivity_t) - b*(temp[i]+climate_change + sensitivity_t)^2 - j*precip[i] + k*precip[i]^2 + c
       
       output$yield[i] = yield
       
     } else if(sensitivity_precip != 0){
       yield_calculation = a*(temp[i]+climate_change) - b*(temp[i]+climate_change)^2 - j * (precip[i]*sensitivity_p) + k * (precip[i]*sensitivity_p)^2 + c
       
       output$yield[i] = yield 
       
     } else {
       
       yield_calculation = a*(temp[i]+climate_change) - b*(temp[i]+climate_change)^2 - j*precip[i] + k*precip[i]^2 + c
       
       output$yield[i] = yield
     }
     
   }
  return(result)
}
