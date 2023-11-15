

## Data storing

dataStore <- R6::R6Class(
    "data",
            
    public = list(
    
    # Start    
    initialize = function(df, date, value){
        private$df = df
        private$ts = ts( data = df$value, start = c( year(min(date)), month(min(date)) ), frequency = 12  )
        },

        # Metode
    # Return data as ts
    tsReturn = function( ){  private$ts },
    
    # Return data as df
    dfReturn = function( ){  private$df },
    
    # # Split data into train and test part
    tsSplit  = function(start_train, end_train) {

        # Split training data
        start <- ifelse(start_train == 0, 1, length(private$ts)*start_train)
        end   <- length(private$ts)*end_train


        # Train and test data
        private$ts_train <- window(private$ts, start = index(private$ts)[start] , end = index(private$ts)[end])
        private$ts_test  <- window(private$ts, start = index(private$ts)[end+1] , end = index(private$ts)[length(private$ts)])

        # Return train and test data in list
        list( train = private$ts_train,
              test = private$ts_test
              )

    },
    
    tsTrainReturn = function( ){ if( !is.null(private$ts_train) ) return(private$ts_train) },
    
    tsTestReturn = function( ){ private$ts_test},
    
    tsTrainTestReturn = function() { 
        
        list( train = private$ts_train,
              test = private$ts_test
              )
    },
 
    # Method
    print = function(...){
        cat("print")}
    ),
    
    private = list(
        df = NULL,
        ts = NULL,
        ts_train = NULL,
        ts_test = NULL
        )
)




