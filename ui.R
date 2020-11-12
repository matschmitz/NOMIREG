library(shiny)

fluidPage(theme = "myCSS.css",
          headerPanel(title = "", windowTitle = "Régression multiple nominale"),
          h2("Régression multiple nominale"),
          
          sidebarLayout(
            sidebarPanel(width = 4,
                         matrixInput("codes", class = "numeric", 
                                     value = matrix(c(-1, +1, 0,
                                                      1, 1, -2),
                                                    dimnames = list(c("feed_NvsP", "feed_AvsNP"),
                                                                    c("N", "P", "A")),
                                                    nrow = 2, byrow = TRUE),
                                     rows = list(names = TRUE),
                                     cols = list(names = TRUE)),
                         checkboxInput('projectX1', 'note~feed_NvsP'),
                         checkboxInput('projectX2', 'note~feed_AvsNP'),
                         verbatimTextOutput("descriptives"),
                         verbatimTextOutput("mdlSummary")
            ),
            
            mainPanel(width = 5,
                      plotlyOutput('mainPlot', height = "600px")
            )
          )
          
)