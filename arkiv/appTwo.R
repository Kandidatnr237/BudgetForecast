

library(shiny)

ui <- fluidPage(
    # Define the UI structure
    navbarPage(
        "Navs Forecast Machine", # App title
        # Define the navigation menu with tabs using navlistPanel
        tabPanel("1. Data spesification",
                 sidebarPanel(
                     width = c(4,10),
                     # Define input controls for the sidebar
                     selectInput( "cat", "Choose test data", choices = cat, width = "80%"),
                     helpText("Dette er hjelpetekst
                              med linjebryting."),
                     sliderInput("periods", "lenght of data", min =  min(df1$date),  max = max(df1$date), value =  c(min(df1$date),max(df1$date)), ticks = T),
                     helpText("Dette er hjelpetekst\nmed linjebryting."),
                     selectInput( "calc", "choose statistics", choices = c("mean", "sum"), width = "80%"),
                     helpText("Mer hjelpetekst."),
                     
                     mainPanel( width = 6)
                     
                        )
                ),
        tabPanel("2. Modeling",
                 # Add a sidebarLayout in Tab 2
                 sidebarLayout(
                     sidebarPanel(
                         width = c(4,10),
                         # Define input controls for the sidebar
                         selectInput("select_variable", "Select a variable", choices = c("Option 1", "Option 2")),
                         sliderInput("slider_range", "Select a range", min = 0, max = 100, value = c(0, 50))
                     ),
                     mainPanel(
                         # Display the main content based on user inputs
                         plotOutput("plot")
                     )
                )
            )
    )
)

server <- function(input, output) {
    # Define server logic here
    # You can use input$select_variable and input$slider_range to react to user inputs
    
    # Example: Render a plot based on user inputs
    output$plot <- renderPlot({
        # Create a plot based on the selected variable and range
        data <- data.frame(
            x = rnorm(100),
            y = rnorm(100)
        )
        
        plot(data$x, data$y, main = paste("Selected Variable: ", input$select_variable))
    })
}

shinyApp(ui = ui, server = server)

