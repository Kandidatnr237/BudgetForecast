# 
# 
# df <- nav_data$df1 |> 
#     filter( sex == "samlet", age == "20-29")
# 
# 
# # Last date
# last_date <- as.Date("2023-07-01")
# 
# 
# 
# # 3, 6, 9, 12 month compared to last year
# 
# # Helper function
# fn_average <- function(x) {
#     map( x, function(x) {
#         # Data
#         df |>
#             filter( date %in% x) |> 
#             summarise( value = mean(value))
#     } )  
# } 
# 
# 
# # Creating table to see the development of the data 
# tbl <- tibble( `this year` = c(3,6,9,12),
#         `last year` = `this year`
#         ) |> 
#     # Periods
#     mutate( `this year` = map(`this year`, function(x) seq.Date(from = max(df$date)-months(x-1), by = "month", length.out = x) ),
#             `last year` = map(`last year`, function(x) seq.Date(from = max(df$date)-months(12) -months(x-1), by = "month", length.out = x) ),
#             ) |>   
#     ## Keep only the last 3,6,9 or 12 monhts
#     mutate(  across( .cols = everything() ,
#                      .fns = function(x) fn_average(x),
#                      .names = "{fn}_{col}"
#                      ),
#              )
# 
# 
# #
# fn_date_point <- function( vec_date, .first = T, .last = F, nr = NULL){ vec_date,  }
# 
# # Calculate the values
# tbl |> 
#     rename( `average this year` = 3, `average last year`  = 4) |>
#     unnest( cols =  c(`average this year`, `average last year`), names_sep = " ") |> 
#     mutate( fra = map(`this year`, \(x)) ) 
#     
# transmuate( 
#         periods = paste0("Based on data from periods ",`this year`[1] |> month(),  )
#         
#         `develop. per cent` = `average this year value`/`average last year value`)
