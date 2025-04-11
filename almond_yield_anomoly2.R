#' Almond Yield Anomaly (from Daily Data)
#'
#' Computes almond yield anomalies (in tons/acre) given daily data for temperature (in degrees Celsius), precipitation (in mm), month, and water year as a dataframe. 
#'
#' @param data dataframe that contains the columns: month, year, wy (water year), tmin_c (minimum temperature), precip (precipitation)
#' @param month_temp month of temperature data (integer)
#' @param month_precip month of precipitation data (integer)
#'
#' @return a summary dataframe with average minimum temperature and total precipitation by year and list of minimum, mean, and maximum yield anomalies
#'
#' @examples
#' almond_yield_anomaly_from_daily(data, month_temp = 2, month_precip = 1)
#' 

almond_yield_anomaly_from_daily <- function(data, month_temp = 2, month_precip = 1) {

  # Compute yearly average minimum temperature (avg_min_temp_c)
  min_temp_summary <- data %>%
    # Select month of temperature variable
    filter(month == month_temp) %>% 
    # Group by water year
    group_by(wy) %>% 
    # Take average
    summarise(avg_min_temp_c = mean(tmin_c, na.rm = TRUE), .groups = "drop") 
  
  # Compute yearly precipitation totals (total_precip_mm)
  total_precip_summary <- data %>%
    # Select month of precipitation variable
    filter(month == month_precip) %>%
    # Group by water year
    group_by(wy) %>%
    # Calculate totals
    summarise(total_precip_mm = sum(precip, na.rm = TRUE), .groups = "drop") 
  
  # Combine the data
  climate_summary <- inner_join(min_temp_summary, total_precip_summary, by = "wy") %>%
    rename(water_year = wy)
  
  # Return dataframe
  print(climate_summary)
  
  # Calculate the yield anomaly
  climate_summary$yield_anomaly <- -0.015 * climate_summary$avg_min_temp_c - 0.0046 * climate_summary$avg_min_temp_c^2 - 0.07 * climate_summary$total_precip_mm + 0.0043 * climate_summary$total_precip_mm^2 + 0.28
  
  # Output
  yield_anomalies <- setNames(climate_summary$yield_anomaly, climate_summary$water_year)
  
  return(list(
    yield_anomalies = yield_anomalies,
    mean_yield_anomaly = mean(yield_anomalies),
    max_yield_anomaly = max(yield_anomalies),
    min_yield_anomaly = min(yield_anomalies)
  ))
}
