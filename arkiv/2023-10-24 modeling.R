
# # Packages
# library(forecast)
# library(xts)
# library(tidyverse)
# source("~/BudgetForecast/appendix.R")
# 
# # Test data
# df2 <- df1 |> filter( str_detect(cat, "samlet 50"))
# 
# ts_data <- df2 |> pull(value) |> ts( start = c(2021, 1), frequency = 12 )
# 
# fun_hw <- function( ts_train, start = c(2023, 10), h){
#     
#     # Execute the model
#     model_hw  <- ets( ts_train, model = "AAA")    
#     # Return list with info
#     list( model    = model_hw,
#           obj.fc   = forecast(model_hw, h = h),
#           fc       = fun_fc_tibble_convert(  fc = forecast(model_hw, h = h) ) |> 
#               mutate( date = as.character(date) ),
#           name.model = "holt Winter"
#     )
# }
# 
# 
# 
# 
# fun_hw( ts_train = ts_data, h = 1  )$fc 


