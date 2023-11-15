

## The Augmentet Dick Fuller test
library(urca)

df2 <- df1 |> filter( str_detect(cat, "samlet 50"))

ts_data <- df2 |> pull(value) |> ts( start = c(2021, 1), frequency = 12 )

#
ur.df( y = ts_data,
       type = "none"
       ) |> 
    summary()

#
fun_print_ur_df( y = ts_data, type = "none", name = "none")
fun_print_ur_df( y = ts_data, type = "drift", name = "none")
fun_print_ur_df( y = ts_data, type = "trend", name = "none")

test <- list( ts_data,ts_data,ts_data,diff(ts_data),diff(ts_data),diff(ts_data) )

df_adf_test <- tibble( type = rep( x = c("none", "drift", "trend"),2),
                       name = c( paste0("Model y ",1:3),
                                 paste0("Model diff(y) ",1:3)
                       ),
                       data = test
)

df_adf_test |> 
    # Execute the ADF-test for all data-set 
    mutate(
        adf = pmap(
            # List of input, and the function
            list(t =type, n = name, y = data ), function(t,n,y){
                # The function that creates the results
                fun_print_ur_df(name= n, y = y, lags = 12, type = t )}
        )
    ) |>
    select(-c(data, type, name), name1 = name ) |>
    unnest(adf) |>
    mutate( name1 = str_remove(name1, "Model "),
            name  = str_replace(name, "tau(1|2|3)", "tau") |> 
                str_replace( "phi(1|2)", "phi")
    ) |> 
    pivot_wider( names_from = name1, values_from = model) |>
    distinct() |> 
    unnest( cols = everything()
    )


#| echo: false
#| warning: false
#| message: false

#' This function do the ADF-test and return the result in a data.frame.
#' This makes it easy to do several test, and print it in a data.frame 
#'
#' @param name # Name of the test for id  
#' @param ...  # This is the argument for the urca::ur.df-function
#'
#' @return # A data.frame with the result
#' @export
#'
#' @examples fun_print_ur_df( name = "none", tsaap)
fun_print_ur_df <- function(name, ...){
    
    # Do the UR-test
    urdf <- ur.df(...)
    
    # The test statistics
    teststat <- as_tibble( urdf@teststat) |>
        pivot_longer( everything(), names_to = "name", values_to = "model" ) |> 
        mutate( model = round(model, digits = 2) #,
                #name = str_remove(name, "\\d")
        ) |> 
        mutate_all( as.character )
    
    # critcal values
    critcal_vales <- as_tibble( urdf@cval) |>
        mutate( stat = attributes(urdf@cval)$dimnames[[1]]) |> 
        pivot_longer( -stat,
                      names_to = "name",
                      values_to = "model" 
        ) |> 
        mutate( model = round(model, digits = 2)) |>
        #mutate( stat = str_remove(stat, "\\d")) |> 
        unite( "name", stat:name, sep = "-", remove = T  ) |> 
        mutate_all( as.character )
    
    # Create the table
    tibble( desc. = name,
            `numb. lags` =  urdf@lags,
            type = urdf@model
    ) |>  
        mutate_all( as.character ) |> 
        pivot_longer(everything(), names_to = "name", values_to = "model") |> 
        bind_rows( teststat ) |> 
        bind_rows( critcal_vales)
}
