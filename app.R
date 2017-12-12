library(shiny)
library(shinydashboard)
library(heatmaply)
data(mtcars)

ui <- dashboardPage(
        dashboardHeader(title = "Multivariable_Regression"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("CorChart", tabName = "Correlation", icon = icon("bar-chart-o")),
                        menuItem("Data info", tabName = "dat", icon = icon("th")),
                        menuItem("Regression output", tabName = "lm", icon = icon("th")),
                        selectInput("vars", label = "View variable data", names(mtcars), multiple = TRUE),
                        selectInput("dep", "Dependent variable", names(mtcars)),
                        checkboxGroupInput("indep", "Independent variables", names(mtcars)),
                        actionButton("go", "output")
                        
                )
        ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "Correlation",
                                fluidRow(
                                        box(plotlyOutput("heatmap"),  width = "100%"))
                                ),
                        tabItem(tabName = "dat",
                                fluidRow(
                                        box(verbatimTextOutput("dfStr"), width = "100%"),
                                        box(verbatimTextOutput("dfSum"), width = "100%")
                                )),
                        tabItem(tabName = "lm",
                                fluidRow(
                                        box(verbatimTextOutput("LM"), width = "100%")
                                ))
                        
                
)
) 
)


server <- function(input, output) { 
        
        Df <- reactive({
                mtcars[, input$vars]
        })
        
        regFormular <- eventReactive(input$go, {
                pred <- paste(input$indep, collapse = "+")
                as.formula(paste(input$dep, '~' , pred))
        })
        model <- eventReactive(input$go, {
                lm(regFormular(), data = mtcars)
        })
        
        
        
        output$heatmap <- renderPlotly({
                heatmaply(cor(mtcars), margins = c(40, 40),
                          k_col = 2, k_row = 2,
                          limits = c(-1,1))
        })
        
        output$dfStr <- renderPrint({
                str(Df())
        })
        output$dfSum <- renderPrint({
                summary(Df())
        })                
        
        output$LM <- renderPrint({
                summary(model())
        })
        
        
}
        

shinyApp(ui, server)
