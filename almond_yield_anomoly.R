#' Almond Yield Anomaly from Daily Data
#'
#' @param data dataframe that contains the columns: day, month, year, wy, tmin_c, precip
#'
#' @return output of function - list with yield anomalies and summary stats
#' @export
#'
#' @examples
#' almond_yield_anomaly_from_daily(data)
#' 

almond_yield_anomaly_from_daily <- function(data) {
  library(dplyr)

  # Compute February Tmin averages (Tn2)
  feb_tempt_min <- data %>%
    filter(month == 2) %>% # select February
    group_by(wy) %>% #group by year
    summarise(Tn2 = mean(tmin_c, na.rm = TRUE), .groups = "drop") #take average of min temperatures
  
  # Compute January precipitation totals (P1)
  jan_precip <- data %>%
    filter(month == 1) %>% # select january
    group_by(wy) %>% # group by year
    summarise(P1 = sum(precip, na.rm = TRUE), .groups = "drop") # add precipitations
  
  # combine the data
  climate_summary <- inner_join(feb_tempt_min, jan_precip, by = "wy")
  print(climate_summary)
  
  # calculate the yield anomaly
  climate_summary$yield_anomaly <- -0.015 * climate_summary$Tn2 - 0.0046 * climate_summary$Tn2^2 - 0.07 * climate_summary$P1 + 0.0043 * climate_summary$P1^2 + 0.28
  

  # Output
  yield_anomalies <- setNames(climate_summary$yield_anomaly, climate_summary$wy)
  
  return(list(
    yield_anomalies = yield_anomalies,
    mean_yield_anomaly = mean(yield_anomalies),
    max_yield_anomaly = max(yield_anomalies),
    min_yield_anomaly = min(yield_anomalies)
  ))
}


