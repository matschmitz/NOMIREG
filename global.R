if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, ggplot2, data.table, plotly, tidyr, magrittr, shinyMatrix, dplyr)

# APA theme (for plots)
apatheme <- theme_bw() + theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border     = element_blank(),
                               axis.line        = element_line(),
                               text             = element_text(size = 17, family = "serif"))

# Colors
blue <- "#4472C4"
green <- "#548235"
red <- "#C55A11"
redblood <- "#C00000"
grey <- "#BEBEBE"

# Data
DT <- data.table(etudiant = c("Jane", "Sam", "Tim", "Kate", "Claire",
                              "Kim", "Ann", "Tess", "Jill", "Shae"),
                 note = c(10, 14, 8, 9, 19, 16, 18, 17, 14, 15),
                 feedback = c("N", "N", "N", "N", "P", "P", "P", "A", "A", "A"))


