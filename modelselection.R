

# 1)
# Function data split
fn_ts_split <- function(ts, start_train = 0, end_train = 0.8) {
    
    # Split training data
    start <- ifelse(start_train == 0, 1, length(ts)*start_train) 
    end   <- length(ts)*end_train
     
    
    # Train and test data
    train <- window(ts, start = index(ts)[start] , end = index(ts)[end])
    test  <- window(ts, start = index(ts)[end+1] , end = index(ts)[length(ts)] )
    
    # Return trian and test data
    list( train = train, test = test)
    
}


# 2)
# Return forecast, same lenght as the test set 
fn_train_model <- function( train, test){
    
    # Arima
    arima_train <- auto.arima( y = train)
    
    # Holt Winter
    hw <- forecast::ets(y = train, model = "ZZZ" )
    
    # Exponentail smoothing    
    
    # List returned
    list( 
        arima = arima_train,
        `Holt winter` =  hw
        ) |>
        map( \(x) forecast(x, h = length(test)) ) 
    
}


# 3) RMSE
# Table
fn_rmse <- function(meanfc, test){  sum( ((meanfc - test)^2)^0.5)  }

#fn_rmse( a$`Holt winter`, train_test_ts[[2]] )

fn_rmse_table <- function(models, ts_test){
    map( models,
         #
         \(x) fn_rmse(x, ts_test ) 
         ) |> 
        as_tibble() |> 
        pivot_longer( everything(),
                      names_to =  "model",
                      values_to = "RMSE"
        )    
}