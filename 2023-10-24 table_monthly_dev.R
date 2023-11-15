


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
        mutate( `percent development` = (`value this year`/`value last year`)-1,
                period  = pmap( list( this = `this year`, last = `last year`), function(this,last) {
                            paste0( "from ",
                                    fn_date_label(min(last$date) ), "-", fn_date_label(max(last$date) ),
                                    " to ",
                                    fn_date_label(min(this$date) ), "-", fn_date_label(max(this$date) )
                            )
                    } ) 
        ) |> 
        unnest( period) |> 
        select( period, `value this year`,`value last year`,`percent development`)
}


# Summerise by year and from jan. to ---------------------------------------------------------

fn_desc <- function(df, f, yearly_complete = T){ 
    
    library(tidyverse)
    
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
            ) |> 
    # Changing the name of the variable to the funtion 
    rename( !!deparse(substitute(f)) := f )
    
        } 
        

#
fn_desc( df1 |> filter(str_detect(cat, "Kvinner 18")), mean, yearly_complete = T)



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


#fn_desc_binded( df = a, f = mean )


# Bind historical and forecast data ---------------------------------------

fn_hist_and_forecast <- function( df, df_forecast ){
    
    bind_rows( df, df_forecast) |> 
        summarise( mean = mean(value)) |> 
        mutate( `percent change` = mean/lag(mean))
}



