

## 
library(R6)


# Data --------------------------------------------------------------------

datasamler <- R6::R6class("data",
                          public = list(
                              initialize = function(df){
                                  private$df <- df
                                  # Test
                              },
                              #
                              ts = function( freq = 12){
                                  ts( y = df$value,
                                      start = c( min(df$date) |> year(),min(df$date) |> month()),
                                      frequency = freq
                                      ) },
                              
                              print = function(...){cat("print")}
                          ),
                          private = list(
                              df = NULL
                            )
                          )




# Tabeller ----------------------------------------------------------------

tabeller <- R6::R6Class("tabeller",
            #
            public = list(
                initialize = function(df){
                private$df <- df
                private$mndTabell   <- fn_monthly_table( tbl = private$df )
            },
            #
            giHovedTabell = function(f){
                fn_desc_binded(df = private$df, f = f)
                
                #private$hovedTabell
                
            }, 
            
            # lagMndTabell = function( ){ 
            #     private$mndTabell <- fn_monthly_table( tbl = private$df )
            # },
            
            giMndTabell = function( ){ return(private$mndTabell) },
            
            print = function(...){cat("Tabeller")}
            ),
            private = list(
                df = NULL,
                mndTabell = NULL
                )
            )

test <- tabeller$new( df = df1)

test$giMndTabell()
test$giHovedTabell( f = mean)


