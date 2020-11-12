# Setup --------------------------------------------------------------------------------------------
library(tidyr)
library(plotly)
library(ggplot2)
library(magrittr)
library(data.table)
library(schmitz)
library(shiny)

# Data ---------------------------------------------------------------------------------------------
DT <- numerify(fread("NOTES.csv"))

mdl <- DT[, lm(note ~ feed_NvsP + feed_AvsNP)]

DT[, noteHat := predict(mdl)]

GG <- expand.grid(
  x1 = seq(-3, 3, length.out = 20),
  x2 = seq(-3, 3, length.out = 20)
) %>% data.table()

GG[, y := cbind(1, as.matrix(GG)) %*% coef(mdl)]

z <- spread(GG, key = x2, value = y) %>% .[, 2:ncol(.)] %>% as.matrix %>% t


# Plot model and data
fig <- plot_ly()

fig <- fig %>% add_surface(
  x = unique(GG$x1), y = unique(GG$x2), z = z,
  # colors    = c("dodgerblue4", 'dodgerblue3'),
  colors    = c("#006400", "#458B00"),
  showscale = FALSE,
  opacity   = .3,
  hoverinfo = "skip",
  contours = list(
    x = list(show           = FALSE,
             highlight      = FALSE, # input$projectX2
             highlightcolor = "white",
             highlightwidth = 5,
             color          = "azure"),
    y = list(show           = FALSE,
             highlight      = FALSE, # input$projectX1
             highlightcolor = "white",
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
  # title = paste0(withMathJax(sprintf(
  #   "$$note = %s %s %s \\textit{X}_{1} %s %s\\textit{X}_{2} %s %s\\textit{X}_{1}\\textit{X}_{2}$$)",
  #   b0, ifelse(b1>=0, "+", ""), b1, ifelse(b2>=0, "+", ""), b2))),
  scene = list(
    xaxis = list(title      = "feed_NvsP",
                 titlefont  = list(color = "rgb(153, 0, 0)"),
                 tickfont   = list(color = "grey"),
                 autoarante = "reversed",
                 showspikes = FALSE),
    yaxis = list(title      = "feed_AvsNP",
                 titlefont  = list(color = "rgb(153, 0, 0)"),
                 tickfont   = list(color = "grey"),
                 showspikes = FALSE),
    zaxis = list(title      = "notes",
                 titlefont  = list(color = "rgb(153, 0, 0)"),
                 tickfont   = list(color = "grey"),
                 showspikes = FALSE),
    camera = list(eye = list(x = .5, y = -2, z = 1.25))),
  showlegend = FALSE)


fig



plot_ly(z = ~volcano) %>%
  add_surface() %>%
  layout(
    scene = list(
      camera=list(eye = list(x = .5, y = -2, z = 1.25))
    )
  )

