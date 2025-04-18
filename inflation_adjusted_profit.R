
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
  
  # Create a data frame for almond yields
  yield <- data.frame(
    year = as.numeric(names(almond$yield_anomalies)),  # Extract years
    almond_yield_anomalies = unname(almond$yield_anomalies)  # Get the values without the names
  )    
  
  # Add yearly profit to yield
  yield <- yield %>%
    mutate(profit_before_inflation = price_per_ton * almond_yield_anomalies)
  
  #load data
  inflation_df <- read_csv(inflation_file, show_col_types = FALSE)
  
  # Filter inflation data for the selected years and convert to decimal
  inflation_rates <- inflation_df %>%
    filter(year >= year_1 & year <= base_year) %>% # Filter data for selected years
    mutate(us_inflation_rate = us_inflation_rate / 100) %>% # Convert percentage to decimal
    pull(us_inflation_rate)
  
  # Calculate NPV
  yield <- yield %>%
    left_join(inflation_df %>% select(year, us_inflation_rate), by = "year")%>%
    mutate(NPV = profit_before_inflation * (1 + us_inflation_rate))
  
  # Calculate statics 
  total_inflation_adjusted_profit <- sum(yield$NPV)
  min_inflation_adjusted_profit <- min(yield$NPV)
  max_inflation_adjusted_profit <- max(yield$NPV)
  mean_inflation_adjusted_profit <- mean(yield$NPV)
  
  # Return summary of inflation-adjusted profits
  return(list(
    #yield = yield,
    total_inflation_adjusted_profit = total_inflation_adjusted_profit,
    min_inflation_adjusted_profit = min_inflation_adjusted_profit,
    mean_inflation_adjusted_profit = mean_inflation_adjusted_profit, 
    max_inflation_adjusted_profit = max_inflation_adjusted_profit
  ))
}
