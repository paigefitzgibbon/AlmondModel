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




problemfive = function(year,temp, precip_low,precip_high) {
  if (precip < 0) return(NA)
  
  output1 <-  mapply( function(year,temp,precip_low) {
    problem1(year,temp,precip_low)
  }, clim_sens$year ,clim_sens$temp,clim_sens$precip_low )
  as.data.frame(output1)
  df <-      t(output1)
  as.data.frame(df)
  colnames(df) <- c("Year","Anomoly")
  
  output2 <-  mapply( function(year,temp,precip_high) {
    problem1(year,temp,precip_high)
  }, clim_sens$year ,clim_sens$temp,clim_sens$precip_high )
  as.data.frame(output2)
  df1 <- t(output2)
  as.data.frame(df1)
  colnames(df1) <- c("Year","Anomoly")
df2<- data.frame(df$Year, df$Anomoly, df1$Anomoly)
colnames(df2) <- c("Year", "Lower Bound", "Upper Bound")
  
  
  return(ggplot()
         +geom_line(df2, aes(Year, "Lower Bound"))
         +geom_line(df2, aes(Year, "Upper Bound")))
}