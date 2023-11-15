


## Input forecast object, return tibble dates and point, with 80,90,95% CI
fun_fc_tibble_convert <- function(fc){
    tibble( date = index( fc$mean) |> zoo::as.yearmon() |> my(),
            point =  fc$mean,
            low80 = fc$lower[,1],
            low95 = fc$lower[,2],
            high80 = fc$upper[,1],
            high95 = fc$upper[,2]
    )
}


fun_return_list <- function(model, h, name) {
    
    list( model    = model,
          obj.fc   = forecast(model, h = h),
          fc       = fun_fc_tibble_convert(  fc = forecast(model, h = h) ) |>
              mutate( date = as.character(date)) ,
          name.model = name
    )
    
}

fun_hw <- function( ts_train, start = c(2023, 10), model_type = "ZZZ", h){
    
    # Execute the model
    model_hw  <- ets( ts_train, model = model_type )    
    # Return list with info
    fun_return_list( model = model_hw, h, name = "holt winter")
}


# Auto.arima
fun_auto.arima <- function( ts,  h ){
    
    # Execute the model
    model_arima  <- auto.arima(ts)    
    # Return list with info
    fun_return_list( model = model_arima, h = h, name = "arima" )
}



fn_model <- function( type , ts, start, h  ){
    
    if(type == "arima"){  fun_auto.arima( ts = ts, h = h)
        }else if( type == "hw" ){  fun_hw( ts = ts, h = h,  model_type = "ZZZ") }
    else if( type == "all"){ 
        list(
            arima = fun_auto.arima( ts = ts, h = h),
            hw = fun_hw( ts = ts, h = h,  model_type = "AAA")
    )}
}


# Functions for building tables -------------------------------------------
# Table monthly development. Percent change from same period last

# 3,6,9,12 month average

# 1) Helper: Formating date to text
fn_date_label <- function(d) { paste0( year(d) |> str_sub(start = 3, end = 4),"-",month(d, label = T) ) }


# Function
fn_monthly_table <- function( tbl ){
    tibble( this = c(3,6,9,12), last = this + 12 ) |> 
        mutate( `this year`       = map( this, function(x) tbl |> tail(x)),
                `last year`       = map( this, function(x) tbl |> tail(x+12) |> head(x)  ),
                `value this year` = map(`this year`, \(x) mean(x$value) ),
                `value last year` = map(`last year`, \(x) mean(x$value) ),
        ) |> 
        unnest( cols = c(`value this year`, `value last year`)
        ) |> 
        mutate( `Prosent vekst` = ((`value this year`/`value last year`)-1)*100,
                period  = pmap( list( this = `this year`, last = `last year`), function(this,last) {
                    paste0( "Fra og med (",
                            fn_date_label(min(last$date) ), "-", fn_date_label(max(last$date) ),
                            ") til og med (",
                            fn_date_label(min(this$date) ), "-", fn_date_label(max(this$date) )
                    ,")")
                } ) 
        ) |> 
        unnest( period) |> 
        select( period, `value this year`,`value last year`,`Prosent vekst`)
}


# Summerise by year and from jan. to ---------------------------------------------------------

fn_desc <- function(df, f, yearly_complete = T){ 
    
    #
    df |>
        group_by( year = year(date)) |> 
        summarise( f= f(value), ant = n() ) |> 
        # If yearly complete = TRUE, then only year with 12 month are kept 
        filter( ant   >= ifelse(yearly_complete == T, 12, 1),
                year  >= (max(year)-4 ) 
        ) |> 
        select(year, f ) |> 
        mutate( 
            `change` = f-lag(f),
            `percent change` = (f/lag(f)-1)*100 
        )
} |> 
    # Changing the name of the variable to the funtion
    rename( !!deparse(substitute(f)) := f )

#
# fn_desc( df1 |> filter(str_detect(cat, "Kvinner 18")), sum, yearly_complete = T)



# Monthly data ------------------------------------------------------------

fn_desc_month <- function(df, f){
    
    df |>  # Data
        filter(  # Keeping data for this year and last year, same number of month 
            year(date)  %in% seq( from = max(year(date))-2, to = max(year(date)), by = 1 ),
            month(date) %in% 1:month(max(date)) 
        ) |> 
        #  Function summarise
        fn_desc( f, yearly_complete = F) |> 
        mutate( year = paste0(year, "-", max( df$date[year(df$date) == max(year(df$date)) ]) |> month( label = T)  )
        ) |> 
        # Changing the name of the variable to the funtion
        rename( !!deparse(substitute(f)) := f ) |> 
        filter( !is.na(change))
}


# fn_desc_month(df = a, f = mean)

fn_desc_binded <- function(df, f){
    # The first function
    fn_desc(df = df, f = f, yearly_complete = T) |> 
        mutate( year = as.character(year)) |> 
        # The second function
        bind_rows(fn_desc_month(df = df, f = f) ) |> 
        rename( !!deparse(substitute(f)) := f )
}


