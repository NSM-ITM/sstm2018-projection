# SSTM Visitor Predictive Analysis Prototype
# 2019-01-09 - Ping Charoenwet, NSM Thailand (SSTM-NSM Staff Exchange)

# warning: global variables are initiated in-situ
# dirty-spaghetti code ahead
#   - model parametes are hard-coded -> see server.R
#   - UI data ranges are hard-coded -> see ui.R

library(shiny)

source("ui.R")
source("server.R")

# ====================================================================

# Initialize App
shinyApp(ui = ui, server = server)
