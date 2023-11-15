
# Packages and test data
library(shiny)
library(tidyverse)
library(forecast)
library(xts)
library(gt)
library(shinythemes)

# Source
Sys.setlocale("LC_CTYPE")
source(here::here("2023-10-23 importing_example_data.R")) # Data
source(here::here("appendix.R")) # Appendix with functions
source(here::here("dataStore.R"))
source(here::here("tsModeling.R"))
source(here::here("modelselection.R"))

# UTF
Sys.setlocale("LC_CTYPE")

# Test data
df1 <- nav_data$df1 |> 
    mutate( cat = str_c("tilgang ",sex, " ", age) 
            ) |> 
    filter( ! str_detect(str_to_lower(age), "uopp") 
            ) |> 
    arrange( sex, age)


# Defining range for input selection in the app 
cat <- setNames(unique(df1$cat), unique(df1$cat) ) ## category for the data
h <- c(1:24) # Length for data



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

# Server
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
            # Handle the case where dat is NULL (using test data)
            # You might want to provide some default values or handle it differently
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
            
            # store$tsSplit(
            #     start_train = input$partUsedToTrain[1],
            #     end_train = input$partUsedToTrain[2]
            # )
        }
    })
        

    # Models
       models <-  reactive({ tsModeling$new( ts = data()$tsReturn(), h = input$len )$doModeling(type = input$model_type) })       # For Tab-1: Forecast
    modeleval <-  reactive({ tsModeling$new( ts = data()$tsReturn(), h = input$len )$doModeling(type = input$model_type_eval) })  # For Tab-3: Evaluation of model
    

# Model Selection ---------------------------------------------------------
    
    # # Data for model selection
    # data_to_model_selection <- reactive({ 
    #     
    #     dat <- df1 |>
    #         filter( cat == input$cat2)
    #     # }
    #     
    #     dataStore$new(
    #         df = dat,
    #         value = dat$value,
    #         date = dat$date
    #     )
    #     
    # })
    
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


# 
shinyApp(ui, server)






