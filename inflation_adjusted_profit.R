
##' Inflation-Adjusted Almond Profits from Almond Yield
#'
#' @param almond A list from `almond_yield_anomaly_from_daily()` function
#' @param inflation_file Path to CSV with columns: year, us_inflation_rate
#' @param price_per_ton Almond price per ton in USD (default = 6000)
#' @param base_year The year to adjust profits to (default = 2010)
#' @param year_1 The year to adjust profits to (default = 1988)
#'
#' @return A named list with min, mean, and max inflation-adjusted profits

inflation_adjusted_profit <- function(yield,
                                      inflation_file = "macrotrends_us_inflation.csv",
                                      price_per_ton = 5000,
                                      base_year = 2010,
                                      year_1= 1988) {
  library(dplyr)
  library(readr)
  
  # Calculate nominal profits from yield anomalies
  yield_min <- yield$min_yield_anomaly
  yield_max <- yield$max_yield_anomaly
  yield_avg <-yield$mean_yield_anomaly
  
  nominal_profit_min <- yield_min * price_per_ton
  nominal_profit_max <- yield_max * price_per_ton
  nominal_profit_avg <- yield_avg * price_per_ton
  
  #load data
  inflation_df <- read_csv(inflation_file)
  
  # Calculate inflation rates
  inflation_rates<- inflation_df%>%
  filter(year >= year_1 & year <= base_year)%>% # filter data for selected years
  mutate(us_inflation_rate = us_inflation_rate / 100)%>% # covert %rate to decimal
  pull(us_inflation_rate)
  
  #Calculate the cumulative inflation adjustment factor from year_1 to the base year
  inflation_adjustment <- prod(1 + inflation_rates)

  # Apply the cumulative inflation adjustment to nominal profits
  min_inflation_adjusted_profit <- nominal_profit_min * inflation_adjustment
  max_inflation_adjusted_profit <- nominal_profit_max * inflation_adjustment
  mean_inflation_adjusted_profit <- nominal_profit_avg * inflation_adjustment
  
  # Return summary of inflation-adjusted profits
  return(list(
    min_inflation_adjusted_profit = min_inflation_adjusted_profit,
    mean_inflation_adjusted_profit = mean_inflation_adjusted_profit, 
    max_inflation_adjusted_profit = max_inflation_adjusted_profit
  ))
}
