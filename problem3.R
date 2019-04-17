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


problemthree = function(year,temp, precip) {
  if (precip < 0) return(NA)
  
  output <-  mapply( function(year,temp,precip) {
    problem1(year,temp,precip)
  }, clim$year ,clim$temp,clim$precip )
  as.data.frame(output)
  df <-      t(output)
  colnames(df) <- c("Year","Anomoly")
  return(ggplot(df, aes(x=Year,y=Anomoly)))
}