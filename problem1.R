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

problem1 = function(year,temp, precip) {
  if (precip < 0) return(NA)
  result = c(year,-0.015*temp-0.0046*(temp^2)-0.07*precip+0.0043*(precip^2) + 0.28) 
  return(result)
}
