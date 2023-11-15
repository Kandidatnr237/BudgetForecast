

# Documentation of the application.
# The comment in the R-scirpt is writn in english, while the application is in norwegian.

# Bibliotek som trengs for aa kjore appen 
library(shiny)
library(tidyverse)
library(forecast)
library(xts)
library(gt)
library(shinythemes) 
library(openxlsx) # For importing data from the web
library(httr) # 


# Function used in the app ------------------------------------------------

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
    }else if( type == "hw" ){  fun_hw( ts = ts, h = h,  model_type = "AAA") }
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





# Import test data from the web -------------------------------------------

## Importing test data from Nav.no
# 1) Creating new enviroment for organizing data

nav_data <- rlang::new_environment()


# Data at nav.no is given as excel-data. Saving the excel-link at the web and downloading the data
# directly to R
nav_data$url1 <- "https://www.nav.no/_/attachment/download/a83b738d-711c-40b7-a5c0-c0c49ea9d953:89d3a5cbf42beadfd19654dbe2f237b2596977a1/PST311%20Nye%20Mottakere%20av%20uf%C3%B8retrygd.%20Alder.%20Kj%C3%B8nn_2023_08"
nav_data$url2 <- "https://www.nav.no/_/attachment/download/edc9253b-ee64-4f97-9ecc-10b96b7d2976:9bcb65d662e43cd867e31c8cd98911954de6acd7/PST311_Nye_Mottakere_av_uf%C3%B8retrygd._Alder._Kj%C3%B8nn._2022_12"
nav_data$url3 <- "https://www.nav.no/_/attachment/download/10f21377-9951-4741-8f0e-ac4de460300a:dbcc8da57bf68539a38ef280ccf1cd0206b25fa8/PST311_Nye_Mottakere_av_uf%C3%B8retrygd._Alder._Kj%C3%B8nn._2021_12"
nav_data$url4 <- "https://www.nav.no/_/attachment/download/5af5e3b4-edf9-44c8-ae2d-9969af72255b:f476f0747cd494b2b2c93b99b941a38081705530/PST311_Nye_Mottakere_av_uf%C3%B8retrygd._Alder._Kj%C3%B8nn._%C3%85r._M%C3%A5ned._ny_alder_202012"
nav_data$url5 <-"https://www.nav.no/_/attachment/download/6c5bf0e7-3e22-451e-82cb-ef8a6f0a700d:5f29acbb5e6a9166d187469c949634274bec0b07/PST311_Nye_Mottakere_av_uf%C3%B8retrygd._Alder._Kj%C3%B8nn._201912"

# Reading all the data 
nav_data$data_list <- lapply( 
    list( nav_data$url1, nav_data$url2, nav_data$url3,nav_data$url4, nav_data$url5 ),
    function(x) openxlsx::read.xlsx( paste0(x, ".xlsx"), sheet = 2, startRow = 8)) |>
    set_names( c(2023, 2022, 2021, 2020, 2019)
    )
# 

Sys.setlocale("LC_CTYPE")
nav_data$fun_wrangle <- function(df){
    Sys.setlocale("LC_CTYPE")
    df |> 
        rename( kjonn = X1, alder = X2) |> 
        fill( kjonn, .direction = "down") |> 
        mutate( kjonn = ifelse( is.na(kjonn), "samlet", kjonn),
                alder = str_remove(str_to_lower(alder), "??r") |> str_trim(side = "both")
        ) |> 
        filter( !is.na(alder), !str_detect(alder, "alt") ) |>
        pivot_longer( -c(kjonn,alder)) 
}


## Data data in long format
nav_data$df <- map( nav_data$data_list, function(x) nav_data$fun_wrangle(x)) |> 
    bind_rows( .id = "id") 

# Give english name to month, based on Norwegian names
nav_data$df_month <- 
    tibble( name = unique(nav_data$df$name),
            month = 1:12,
            mnd = month(month, label = T)
    )


# Last cleaning process
nav_data$df1 <- nav_data$df |> 
    left_join(nav_data$df_month, join_by(name) ) |> 
    mutate( date = paste0(id,"-",mnd, "-01") |> ymd(),
            value = as.numeric(value)
    ) |> 
    select( date, sex = kjonn, age = alder, value) |> 
    arrange( age, sex, date)



# The Class dataStore -----------------------------------------------------

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


# the class tsModeling and three helper-functions ---------------------------------------

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


# Helper function for the panel 5. Modelvalg ------------------------------


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


# The App part 1 ------------------------------------------------------------------

# UTF
Sys.setlocale("LC_CTYPE")

# Test data
df1 <- nav_data$df1 |> 
    mutate( cat = str_c(sex, " ", age) 
    ) |> 
    filter( ! str_detect(str_to_lower(age), "uopp") 
    ) |> 
    arrange( sex, age)


# Defining range for input selection in the app 
cat <- setNames(unique(df1$cat), unique(df1$cat) ) ## category for the data
h <- c(1:24) # Length for data



# The App part 2 (UI) -----------------------------------------------------

# user interface
ui <- fluidPage(
    theme = shinytheme("cosmo"), 
    tags$head(
        tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
      }
    ") )
    ),
    titlePanel("Shiny budsjettmaskin"),
    #
    navlistPanel(
        widths = c(3,9),
        id = "Info",
        "Innhold",
        # Tab 1.
        tabPanel( "1.Velg data",
                  fluidRow( column(10, helpText( HTML("<span style='color:red;'>NB: Velg importer eller integert data</span>") ) )), 
                  fluidRow( column(10, helpText("Ved valg 'integrert data' kan data velges pa side '2.Spesifiser' data")) ) ,
                  fluidRow( column(6, radioButtons("data_source", "Velg datakilde:",
                                                   choices = c("Importer data", "Integrert data"),
                                                   selected = "Imported Data") ),
                            column(6, fileInput("file", "Upload your data:") )
                  ),
                  fluidRow( column(10, helpText("Importert data ma vaere strukturert slik eksempelet under:"))),
                  fluidRow( column(6,
                                   mainPanel(
                                       tableOutput("display_table")
                                   ))
                  ),
                  fluidRow(
                      column(12,h4( HTML("<strong>Forklaring til de ulike sidene i applikasjonen</strong>")) ),
                  ),
                  hr(),
                  fluidRow(
                      #                      column(6,helpText("1.Velg data:")),
                      column(12,helpText( HTML("<strong>1.Velg data:</strong> Her velger man enten importert eller data som er integrert i applikasjonen. Importert data ma ha en lik struktur som eksempelet over.") )),
                  ),
                  hr(),
                  fluidRow(
                      #                      column(6,helpText("1.Velg data:")),
                      column(12,helpText( HTML("<strong>3. Forecasting:</strong> En av applikasjonens hovedfunksjoner. Her kan man fremskrive observasjoner, basert valgt modell ARIMA eller Holt Winter.
                                               Arima er framkommet fra forecast::auto.arima(data), mens Holt Winter forecast::etc(data).
                                               Man kan velge antall perioder (argumentet h i forecast::forecast(modell, h= antall perioder ). Resultatet kan inspiseres i en graf og to tabeller. 
                                               ") )),
                  ),
                  hr(),
                  fluidRow(
                      #                      column(6,helpText("1.Velg data:")),
                      column(12,helpText( HTML("<strong>4. Modellevaluering:</strong>Modellen ma oppfylle visse kriterier (restleddet skal ha forventningsverdi lik 0, og ikke vaere seriekorrelerte).
                                               Pa side 4 evalueres valgt modell (fra side 3), med et histogram (her ser man hvordan fordelingen til residualene) og et ACF-plot (acf(model$residual)).
                                               I tillegg rapporteres P-verdi fra to formelle tester: Om residualene er normalfordelte og om det er indikasjoner pa seriekorrelasjon.   
                                               ") )),
                  ),
                  hr(),
                  fluidRow(
                      #                      column(6,helpText("1.Velg data:")),
                      column(12,helpText( HTML("<strong>5. Modellvalg:</strong> Hvilken modell man skal velge er ikke opplagt. Side 5 er satt av til aa begrunne hvilken modell man foretrekker.
                                               Dette gjores ved a dele dataene i trenings- og test- sett. Modellen velges basert pa treningssettet ('beste modell' fra auto.arima og etc),
                                               for testes ved a maale hvor godt modellen predikerer test-settet.
                                               Det er en lagt til en funksjonalitet for valg av treningsett (forste og siste observasjon),
                                               og hvor stor andel trening/test-settet skal vaere. 
                                               Treningssettet vil alltid bestaa av observasjoner fremfor testsettet. 
                                               Overste ramme er utprint av trenings- og test-data. I midten er en figur som viser hele tidsserien, treningssettet markert rodt,
                                               mens predikering av testsdata er i stiplet linje. ARIMA er morkeblaa mens Holt Winter er rod linje.
                                               Nederst er RMSE for predikeringen av test-settes (for begge modellene)
                                               ") )),
                  ),
                  hr(),
                  fluidRow(
                      column(12,helpText( HTML("<strong>6. Eksportere data:</strong> Her kan man eksportere tabeller i applikasjonen, til Excel.") )
                      )
                  )
        ),
        tabPanel( "2. Spesifiser data",
                  fluidRow(
                      # Choose which data to look at
                      column(5, selectInput( "cat", "Velg data (fungerer kun med integrerte data)", choices = cat, width = "60%" )  ),
                      column(4, sliderInput("periods", "Lengden til dataene (start 2015)", min =  ymd("2015-01-01"),
                                            # This needs to be updated 
                                            max = max(df1$date),
                                            value =  c( min(df1$date),max(df1$date)), ticks = T) 
                      ),
                      column(3, selectInput( "calc", "Statistikk i tabell 1", choices = c("mean", "sum"), width = "60%") )
                  ),
                  # Choose which periods we are looking at
                  fluidRow(
                      column(8, h4("Table 1: Aarlig utvikling", class = "custom-title"), 
                             tags$head(
                                 tags$style(HTML(
                                     "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                                 )
                                 ) 
                             )
                      ),
                      column(8, gt_output("regnskapTabell") )
                      #column(8, tableOutput("regnskapTabell") )
                  ),
                  fluidRow(
                      column(8, h4("Table 2: Maanedlig utvikling", class = "custom-title") ),
                      column(12, tableOutput("mndTabell") )
                  ),
                  # Look at the data with a graph
                  fluidRow(
                      column(10, plotOutput("tsplot") )
                  )
        ),
        
        
        # UI tab 3: Forecasting ---------------------------------------------------
        
        tabPanel( "3. Forecasting",
                  fluidRow(
                      # Choose which data to look at
                      column( 8, selectInput( "len", "Velg forecast lengde", choices = h)  ),
                      #column(4, selectInput("season", "Include seasonal effect in model", choices = c("additive", "multiplicative"), width = "80%" )),
                      column( 4, selectInput( "model_type", "Modellvalg", choices = c("arima", "hw") ) )
                  ),
                  # tabPanel( "",
                  #           fluidRow(
                  #               # Tabell av AIC mm.
                  #           )),
                  fluidRow(
                      # Choose which data to look at
                      column(8, plotOutput("forecastPlot") )
                  ),
                  # Choose which periods we are looking at
                  fluidRow(
                      column(7, h4("Table 3: Forecast values", class = "custom-title") ),
                      column(5, h4("Table 4: Aarlig utvikling", class = "custom-title"))
                  ),
                  fluidRow(
                      column(7, tableOutput("forecastTable") ),
                      column(5, tableOutput("yearlyChange"))
                  )
        ),
        
        
        # Tab  4: Modell-evaluering -----------------------------------------------
        
        tabPanel("4. Modellevaluering",
                 # Choose which periods we are looking at
                 fluidPage(
                     fluidRow(
                         column(4, selectInput( "model_type_eval", "Modell valg", choices = c("arima", "hw"), width = "80%") ),
                         # Ikke lagt inn enda
                         #column(4, sliderInput( "ts_length", "ts data length", min =  0,  max = 1, value =  c(0,1), step = 0.1,  ticks = T) )
                     ),
                     #     column(4, sliderInput("acf_lags",
                     #                           "Nr lags in ACF:",
                     #                           min = 4,
                     #                           max = 50,
                     #                           value = 10) )
                     # ),
                     fluidRow(
                         column(6, plotOutput("residualHistogram")),
                         column(4, sliderInput("hist_bins",
                                               "Antall bins i histogram:",
                                               min = 1,
                                               max = 50,
                                               value = 30) ),
                         column(6, 
                                br(),
                                p("The residuals' mean should be near zero.
                                  A normal distribution should be centered at zero.
                                  The Shapiro-Wilk test assesses the normality assumption.\n",
                                  "The null hypothesis assumes normality in errors.\n",
                                  "A low p-value raises doubts about normality."),
                                br(),
                                verbatimTextOutput("histogramText")
                         )
                     ),
                     fluidRow(
                         column(6, plotOutput("acf") ),
                         column(4, sliderInput("acf_lags",
                                               "Nr lags in ACF:",
                                               min = 4,
                                               max = 50,
                                               value = 10) ),
                         column(6, 
                                br(),
                                p("The residuals (e) should not be series correlated.
                                In the ACF-plot, this corresponds to no columns crossing the blue horizontal line,
                                indicates that the residuals are white noise (iid).
                                The Breusch-Godfrey (Lagrange Multiplier) test () cbe used as a formal test. 
                                A small p-value (for instance, p-value < .05) indicates there is significant autocorrelation remaining in the residuals. 
                                  "),
                                br() ),
                         column(6, verbatimTextOutput("acfText") )
                     ),
                     fluidRow(
                         wellPanel( h3("Regresjonsresultatet"),
                                    p("Utprint av regresjonsresulatet.")),
                         column(10, verbatimTextOutput("modelsummary"))
                     )
                 )
        ),
        
        # UI:Tab 5 (model selection) ----------------------------------------------
        
        tabPanel("5. Modellvalg",
                 fluidRow(
                     column(5, selectInput( "cat2", "Velg data (fungerer kun med integrerte data)", choices = cat, width = "60%" )  ),
                     column(6, sliderInput("train_range", 
                                           label = "Velg hvor stor andel som skal vaere treningsdata",
                                           min = 0, max = 0.95, value = c(0, 0.8) )
                     ) 
                 ),
                 fluidRow(
                     column(6,h4("Utskrift av trening- og testdata", class = "custom-title"), 
                            tags$head(
                                tags$style(HTML(
                                    "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                                )
                                )
                            )),
                     column(10, verbatimTextOutput("testText") )
                 ),
                 fluidRow( 
                     column(10, plotOutput("testPlot") )
                 ),
                 fluidRow( 
                     column(10, tableOutput("testTable") )
                 )
        ),
        # Exporting data
        tabPanel("6. Skriv ut tabeller",
                 fluidRow(
                     column(6,h4("Table 1: Aarlig utvikling", class = "custom-title"), 
                            tags$head(
                                tags$style(HTML(
                                    "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                                )
                                )
                            )),
                     column(6, downloadButton(outputId = "downloadData1",label =  "Download") )
                 ),
                 fluidRow(
                     column(6,h4("Table 2: Maanedlig endring", class = "custom-title"), 
                            tags$head(
                                tags$style(HTML(
                                    "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                                )
                                )
                            )),
                     column(6, downloadButton(outputId = "downloadData2",label =  "Download") )
                 ),
                 fluidRow(
                     column(6,h4("Table 3: Forecast h-perioder", class = "custom-title"), 
                            tags$head(
                                tags$style(HTML(
                                    "
                                 .custom-title {
                                 font-size: 14px; /* Adjust the font size as needed */
                                 color: gray; /* Adjust the text color as needed */
                                 /* Add other styles as needed */
                                 }
                                 "
                                )
                                )
                            )),
                     column(6, downloadButton(outputId = "downloadData3",label =  "Download") )
                 )
        )
    )
)

# The App part 4: the server ------------------------------------------------------------------

server <- function(input, output) {


    
    
    # Tab 1: Import data ------------------------------------------------------
    
    # Data
    imported_data <- reactive({
        if (input$data_source == "Imported Data") {
            req(input$file)
            dat <- readxl::read_excel(input$file$datapath, sheet = 1) |> 
                filter(between(date, min(input$periods), max(input$periods)))
        } else {
            # If using test data, set it to NULL or some default value
            dat <- NULL
        }
        
        dat
    })
    
    # Data
    data <- reactive({
        dat <- imported_data()
        
        if (!is.null(dat)) {
            dataStore$new(df = dat,
                          value = dat$value,
                          date = dat$date
            )
        } else {

            dat <- df1 |>
                filter( cat == input$cat,
                        between(date, min(input$periods), max(input$periods) )
                )
            
            dataStore$new( df = dat,
                           value = dat$value,
                           date = dat$date
            )
        }
    })
    
    # Data for model selection
    data_to_model_selection <- reactive({
        dat <- imported_data()
        
        if (!is.null(dat)) {
            
            dataStore$new(df = dat,
                          value = dat$value,
                          date = dat$date
            )
        } else {
            # Filter
            dat <- df1 |> filter( cat == input$cat2 )
            
            dataStore$new( df = dat,
                           value = dat$value,
                           date = dat$date
            )

        }
    })
    

    # Models
    models <-  reactive({ tsModeling$new( ts = data()$tsReturn(), h = input$len )$doModeling(type = input$model_type) })       # For Tab-1: Forecast
    modeleval <-  reactive({ tsModeling$new( ts = data()$tsReturn(), h = input$len )$doModeling(type = input$model_type_eval) })  # For Tab-3: Evaluation of model
    
    
    # Model Selection ---------------------------------------------------------
    
    
    #
    listTrainTest <- reactive({ 
        
        data_to_model_selection()$tsSplit( start_train = input$train_range[1],
                                           end_train = input$train_range[2] )
        
    })
    
    
    
    # Tab 2: Data description ---------------------------------------------------------------
    
    # Example data displayed first page:
    # For illustration of how the data should be structured
    output$display_table <- renderTable({
        tibble::tibble( date = seq.Date(from = ymd("2019-01-01"), length.out = 5, by = "month") |> as.character(),
                        value = sample( x = c(100:1000), size = 5, replace = T )
        ) 
    })
    
    
    
    
    # Table 1: Yearly numbers
    output$regnskapTabell <- renderTable(
        { data()$dfReturn() |>
                fn_desc_binded( f = eval( parse( text = input$calc) ) ) |>
                rename( !! input$calc := 2)
        },
        res = 96
    )
    
    
    # Download (Tab6) 1
    output$downloadData1 <- downloadHandler(
        filename = function() {
            paste0( lubridate::today(),"_tabell1.xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(x = data()$dfReturn() |>
                                    fn_desc_binded( f = eval( parse( text = input$calc) ) ) |>
                                    rename( !! input$calc := 2),
                                path = file)
        }
    )
    
    
    
    # Table 2: Monthly development data 
    output$mndTabell <- renderTable( { data()$dfReturn() |> fn_monthly_table() }, res = 96 )
    
    # Download (Tab6) 2
    output$downloadData2 <- downloadHandler(
        filename = function() {
            paste0( lubridate::today(),"_tabell2.xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(x = data()$dfReturn() |> fn_monthly_table(),
                                path = file)
        }
    )
    
    
    
    
    output$tsplot <- renderPlot(
        {ggplot( data = data()$dfReturn(),
                 aes(x = date, y = value)
        ) +
                geom_line(color = "steelblue") +
                theme_minimal(base_size = 14)
        }, res = 96
    )
    
    
    
    
    
    # Tab 3: Forecast ---------------------------------------------------------
    
    # 1)
    # Result from forecast Holt Winter model in table
    output$forecastTable <- renderTable(
        { 
            models()$fc |> mutate( date = str_sub(date, 1, 7) )
        },
        res = 96
    )
    
    # Download (Tab6) 3
    output$downloadData3 <- downloadHandler(
        filename = function() {
            paste0( lubridate::today(),"_tabell3.xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(x = models()$fc |> mutate( date = str_sub(date, 1, 7) ),
                                path = file)
        }
    )
    
    
    # 2) 
    # Yearly change
    output$yearlyChange <- renderTable(
        { 
            data()$dfReturn() |> 
                select(date, value) |> 
                bind_rows( models()$fc |> 
                               mutate( date = ymd(date), value = point)
                ) |>
                mutate( year = year(date) ) |> 
                summarise( mean = mean(value), .by = year) |> 
                mutate( `pst vekst` = (mean/lag(mean)-1),
                        year = as.integer(year)
                )    
        },
        res = 96
    )
    
    # 3)  The Plot
    output$forecastPlot <- renderPlot( models()$obj.fc |>
                                           forecast::autoplot() + 
                                           theme_minimal(base_size = 10) +
                                           theme( plot.title = element_text( size = 10)),
                                       res = 96
    )
    
    
    
    # Tab 4: Model evaluation --------------------------------------------------
    
    # model summary
    output$modelsummary <- renderPrint({
        summary(modeleval()$model |> summary() )
    })
    
    # Histogram/Normality test 
    output$histogramText <- renderPrint({ cat("P-value (Shapiro-Wilk): ", round( stats::shapiro.test( modeleval()$model$residuals)$p.value, 2)*100, "%" ) })
    
    output$acfText <- renderPrint({
        paste0("P-value (LM-test): ",lmtest::bgtest( lm( modeleval()$model$residuals ~1))$p.value |> round(2)*100, " %" )
    })    
    
    # Model evaluation
    output$residualHistogram <- renderPlot({
        
        x <- modeleval()$model$residual    
        bins <- seq(min(x), max(x), length.out = input$hist_bins + 1 )
        
        hist( x, 
              main = paste0("Model type: ",str_to_upper( modeleval()$name.model ) ),
              xlab = "residuals",
              breaks = bins
        )    },
        res = 96
    )
    
    # Model evaluation
    output$acf <- renderPlot( 
        # First convert data to integers
        coredata( modeleval()$model$residual ) |>
            # Plot the acf
            acf( 
                # Main title
                main = paste0("Model type: ",str_to_upper( modeleval()$name.model ) ),
                # Input for numbers of lags 
                lag.max = input$acf_lags
            ),
        res = 96
    )
    
    
    # Tab 5: Model selections  ---------------------------------------------------------
    
    output$testTable <- renderTable({
        fn_rmse_table(
            models = fn_train_model(
                train = listTrainTest()$train,
                test  = listTrainTest()$test ) |> map("mean"),
            ts_test = listTrainTest()$test
        )
    })
    
    output$testText <- renderPrint({
        
        listTrainTest()
    }
    )
    
    
    output$testPlot <- renderPlot(
        {
            autoplot(
                data_to_model_selection()$tsReturn(),
                linetype = 1,
                alpha = 0.2
            ) +
                autolayer( listTrainTest()$train ) +
                autolayer(
                    map(
                        fn_train_model( train = listTrainTest()$train,
                                        test = listTrainTest()$test ),
                        \(x) x$mean )$arima,
                    alpha = 0.7,
                    linetype = 2,
                    color = "darkblue"
                ) +
                autolayer(
                    map(
                        fn_train_model( train = listTrainTest()$train,
                                        test = listTrainTest()$test ),
                        \(x) x$mean )$`Holt winter`,
                    alpha = 0.7,
                    linetype = 2,
                    color = "darkred"
                ) +
                labs( subtitle = HTML("Hele tidsserien i sort farge. Treningsdata i rodt,ARIMA i stiplet blaa, HW i stiplet rod."),
                      y = "value") +
                theme_light( base_size = 14) +
                theme( legend.position = "none")
            
        }
    )
    # 
}


# Run the app -------------------------------------------------------------



shinyApp(ui, server)
