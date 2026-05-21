.local_log <- character()

assignInNamespace(
  "log_event",
  function(msg) {
    .local_log <<- c(.local_log, msg)
    invisible(NULL)
  },
  ns = "dauPortalTools"
)

# ---- Source app R files (correct path resolution) ----
app_r_files <- list.files(
  here::here("R"),
  pattern = "\\.R$",
  full.names = TRUE
)

purrr::walk(app_r_files, source, local = FALSE)
