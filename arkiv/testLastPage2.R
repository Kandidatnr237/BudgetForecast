# # 
# # 
# # 
# # Packages and test data
# library(shiny)
# library(tidyverse)
# library(forecast)
# library(xts)
# library(gt)
# 
# source(here::here("2023-10-23 importing_example_data.R"), encoding = "UTF-8")  # Data
# source(here::here("dataStore.R"))
# source(here::here("modelselection.R"))
# 
# 
# # Test data
# Sys.setlocale("LC_CTYPE")
# df1 <- nav_data$df1 |>
#     mutate( age = str_remove( str_to_lower(age), "år") |> str_trim( side = "both"),
#             cat = str_c(sex, " ", age)
#     ) |>
#     filter( ! str_detect(str_to_lower(age), "uopp")
#     ) |>
#     arrange( sex, age)
# 
# 
# # Defining range for input selection in the app
# cat <- setNames(unique(df1$cat), unique(df1$cat) ) ## category for the data
# h <- c(1:24) # Length for data
# 
# 
# 
# ui <- fluidPage(
# 
#     fluidRow(
#         column(5, selectInput( "cat", "Choose data", choices = cat, width = "60%" )  ),
#         #column(6, helpText("Dette er hjelpetekst______________.")),
#         column(6, sliderInput("train_range",
#                               label = "Selecting part to training data",
#                               min = 0, max = 0.95, value = c(0, 0.8) )
#         )
#     ),
#     fluidRow(
#         column(10, verbatimTextOutput("testText") )
#     ),
#     fluidRow(
#         column(10, plotOutput("testPlot") )
#     ),
#     fluidRow(
#         column(10, tableOutput("testTable") )
#     )
# 
# )
# 
# server <- function(input, output, session) {
# 
#     # Data for model selection
#     data_to_model_selection <- reactive({
# 
#         dat <- df1 |>
#             filter( cat == input$cat)
#         # }
# 
#         dataStore$new(
#             df = dat,
#             value = dat$value,
#             date = dat$date
#         )
# 
#     })
# 
#     #
#     listTrainTest <- reactive({
# 
#         data_to_model_selection()$tsSplit( start_train = input$train_range[1],
#                                            end_train = input$train_range[2] )
# 
#     })
# 
# 
#     output$testTable <- renderTable({
#         fn_rmse_table(
#             models = fn_train_model(
#                 train = listTrainTest()$train,
#                 test  = listTrainTest()$test ) |> map("mean"),
#             ts_test = listTrainTest()$test
#         )
#     })
# 
#     output$testText <- renderPrint({
# 
#         listTrainTest()
#     }
#     )
# 
# 
#     output$testPlot <- renderPlot(
#         {
#             autoplot(
#                 data_to_model_selection()$tsReturn(),
#                 linetype = 2,
#                 alpha = 0.5
#             ) +
#                 autolayer( listTrainTest()$train ) +
#                 autolayer(
#                     map(
#                         fn_train_model( train = listTrainTest()$train,
#                                         test = listTrainTest()$test ),
#                         \(x) x$mean )$arima,
#                     alpha = 0.7,
#                     linetype = 2,
#                     color = "darkblue"
#                 ) +
#                 autolayer(
#                     map(
#                         fn_train_model( train = listTrainTest()$train,
#                                         test = listTrainTest()$test ),
#                         \(x) x$mean )$`Holt winter`,
#                     alpha = 0.7,
#                     linetype = 2,
#                     color = "darkred"
#                 ) +
#                 labs( title = "Tittle", y = "value")
# 
#         }
#     )
# }
# 
# shinyApp(ui, server)
# # 
# # # # Test side
# # test <- df1 |> filter( str_detect(cat, "samlet 50"))
# # 
# # obj_store <- dataStore$new( df = test,
# #                             value = test$value,
# #                             date = test$date
# # )
# # 
# # list_train <- obj_store$tsSplit( 0, 0.8)
# # 
# # # 
# # # 
# # # list_train_test <- obj_store$tsSplit( start_train = 0, end_train = 0.5)
# # 
# # 
# # fn_rmse_table(
# #     models = fn_train_model(
# #         train = list_train$train,
# #         test = list_train$test
# #         ) |> map("mean"),
# #     ts_test = list_train$test
# # )
# # 
# # fn_train_model(
# #     train = list_train$train,
# #     test = list_train$test
# #     )$arima |>
# #     map(\(x) x$mean) 
# # 
# # map(
# #     fn_train_model( train = list_train$train,
# #                     test = list_train$test ),
# #     \(x) x$mean )$`Holt winter`
# # 
# # # 
# # # # Tabell
# # # # 3) RMSE
# # # # Table
# # fn_rmse <- function(meanfc, test){  sum( ((meanfc - test)^2)^0.5)  }
# # # 
# # #fn_rmse( a$`Holt winter`, train_test_ts[[2]] )
# # 
# # fn_rmse_table <- function(models, ts_test){
# #     map( models,
# #          #
# #          \(x) fn_rmse(x, ts_test )
# #     ) |>
# #         as_tibble() |>
# #         pivot_longer( everything(),
# #                       names_to =  "model",
# #                       values_to = "RMSE"
# #         )   
# # }
# # 
# # 
# # fn_rmse_table(
# #     models = fn_train_model( train =  list_train_test$train,
# #                              test = list_train_test$test ) |> map("mean"),
# #     ts_test = list_train_test$test
# # )
# # # 
# # # 
# # # 
# # # 
# # # 
