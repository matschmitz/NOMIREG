library(shiny)

function(input, output) {
  
  getMdl <- reactive({
    C <- input$codes
    
    # Contrast codes
    DT[, feed_NvsP := case_when(feedback == "N" ~ C[1, 1],
                                feedback == "P" ~ C[1, 2],
                                feedback == "A" ~ C[1, 3])]
    DT[, feed_AvsNP := case_when(feedback == "N" ~ C[2, 1],
                                 feedback == "P" ~ C[2, 2],
                                 feedback == "A" ~ C[2, 3])]
    mdl <- DT[, lm(note ~ feed_NvsP + feed_AvsNP)]
    DT[, noteHat := predict(mdl)]
    return(mdl)
  })
  
  output$mainPlot <- renderPlotly({
    mdl <- getMdl()
    B <- coef(mdl)
    b0 <- round(B[1], 2)
    b1 <- round(B[2], 2)
    b2 <- round(B[3], 2)
    b3 <- 2
    
    max.x1.x2 <- max(DT[, .(feed_NvsP, feed_AvsNP)]) + 1
    min.x1.x2 <- min(DT[, .(feed_NvsP, feed_AvsNP)]) - 1
    
    GG <- expand.grid(
      x1 = seq(-10, 10, length.out = 40),
      x2 = seq(-10, 10, length.out = 40)
    ) %>% data.table()
    
    GG[, y := cbind(1, as.matrix(GG)) %*% B]
    
    z <- spread(GG, key = x2, value = y) %>% .[, 2:ncol(.)] %>% as.matrix %>% t
    
    
    # Plot model and data
    fig <- plot_ly()
    
    fig <- fig %>% add_surface(
      x = unique(GG$x1), y = unique(GG$x2), z = z,
      colors    = c("#006400", "#458B00"),
      showscale = FALSE,
      opacity   = .5,
      hoverinfo = "skip",
      contours = list(
        x = list(show           = FALSE,
                 highlight      = input$projectX2, # 
                 highlightcolor = "red",
                 highlightwidth = 5,
                 color          = "azure"),
        y = list(show           = FALSE,
                 highlight      = input$projectX1, # 
                 highlightcolor = "red",
                 highlightwidth = 5,
                 color          = "azure"),
        z = list(show           = FALSE,
                 highlight      = FALSE)
      ))
    
    fig <- fig %>%  add_markers(
      x = DT$feed_NvsP, y = DT$feed_AvsNP, z = DT$note,
      hoverinfo = "skip",
      marker = list(size      = 5,
                    color     = "#1874CD",
                    opacity   = .5))
    
    fig <- fig %>%  add_markers(
      x = DT$feed_NvsP, y = DT$feed_AvsNP, z = DT$noteHat,
      hoverinfo = "skip",
      marker = list(size      = 5,
                    color     = "green",
                    opacity   = .7))
    
    # fig <- fig %>% add_trace(x = 1:3, y = 1:3, z = 0,
    #                          type = "scatter3d",
    #                          mode = "lines",
    #                          hoverinfo = "skip",
    #                          line = list(color = "black", width = 5))
    
    fig <- fig %>% layout(
      title = sprintf(
        "note = %s %s %sfeed_NvsP %s %sfeed_AvsNP",
        b0, ifelse(b1>=0, "+", "-"), abs(b1), ifelse(b2>=0, "+", "-"), abs(b2)),
      scene = list(
        xaxis = list(title      = "feed_NvsP",
                     titlefont  = list(color = "rgb(153, 0, 0)"),
                     tickfont   = list(color = "grey"),
                     showspikes = FALSE,
                     range = c(min.x1.x2, max.x1.x2)),
        yaxis = list(title      = "feed_AvsNP",
                     titlefont  = list(color = "rgb(153, 0, 0)"),
                     tickfont   = list(color = "grey"),
                     showspikes = FALSE,
                     range = c(min.x1.x2, max.x1.x2)),
        zaxis = list(title      = "note",
                     titlefont  = list(color = "rgb(153, 0, 0)"),
                     tickfont   = list(color = "grey"),
                     showspikes = FALSE,
                     range = c(7, 20)),
        camera = list(eye = list(x = .5, y = -2, z = 1.25))),
      showlegend = FALSE)
    
    
    fig
  })
  
  output$mdlSummary <- renderPrint({
    mdl <- getMdl()
    summary(mdl)
  })
  
  output$descriptives <- renderPrint({
    print(DT[, .(M  = round(mean(note), 2),
                 SD = round(sd(note), 2)), .(feedback)])
  })
  
  
}