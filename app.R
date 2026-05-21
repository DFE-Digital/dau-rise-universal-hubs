print(Sys.getenv("R_CONFIG_ACTIVE"))
source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)
