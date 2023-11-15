
# Return list 
fun_return_list <- function(model, h, name) {
    
    list( model    = model,
          obj.fc   = forecast(model, h = h),
          fc       = fun_fc_tibble_convert(  fc = forecast(model, h = h) ) |>
              mutate( date = as.character(date)) ,
          name.model = name
    )
    
}

# Holt winter
fun_hw <- function( ts, h ){
    
    # Execute the model
    # Return list with info
    fun_return_list( model = ets( ts ), h, name = "holt winter")
}


# Auto.arima
fun_auto.arima <- function( ts,  h ){
    
    # Execute the model
    model_arima  <- auto.arima(ts)    
    # Return list with info
    fun_return_list( model = model_arima, h = h, name = "arima" )
}


Sys.setlocale("LC_CTYPE")
tsModeling <- R6::R6Class(
    "tsModeling",
    
    public = list(
        
        # Start    
        initialize = function( ts, h ){
            private$ts = ts
            private$h = h
        },
        
        # Arima
        # ArimaReturn = function(  ){  private$arima },
        # 
        # HwReturn = function(){ private$hw },
        
        doModeling = function( type ){
            
            if(type == "arima"){ private$model =  self$doArima() }
            else if( type == "hw") {private$model =  self$doHw()}
            
            return( private$model)
            
        },
        
        doArima = function( ){ fun_auto.arima( ts = private$ts, h = private$h )},
        
        # Holt Winter
        doHw = function(  ){  fun_hw(ts = private$ts, h = as.integer(private$h) ) },
        
        # Normality test
        doShapiroWilkTest = function( r = "p_value"){
            
            if(is.null(private$model)){return(NULL)}else{
                test <- shapiro.test( private$model$model$residuals )
                if( r == "p_value"){test$p.value}
                }
            },
    
        doAll =  function( dfReturn = T ){ 
            
            # Models
            arima <- self$doArima()
            HW <- self$doHW()
            
            map( list(arima = arima, hw = HW), "fc" ) |> bind_rows( .id = "model" )
            },
        
        giveModel = function(){ private$model},
        
        print = function(...){
            cat("print")}
    ),
    
    private = list(
        ts = NULL,
        h = NULL,
        model = NULL
        # arima = NULL,
        # hw = NULL
    )
)
