source("global.R")
source("ui.R")
source("server.R")
log_event(Sys.getenv("R_CONFIG_ACTIVE"))

shinyApp(ui, server)
