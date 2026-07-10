# global.R

source_all <- function(path = "R") {
  files <- list.files(
    path,
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )
  invisible(lapply(files, source))
}

# Load app code
source_all("R")
source("libraries.R")

# Load static configuration
conf <- dauPortalTools::get_config()
friendly_names <- conf$friendly_names
required_fields <- conf$required_fields

reverse_friendly_names <- setNames(
  names(friendly_names),
  unlist(friendly_names)
)

dup_labels <- names(reverse_friendly_names)[duplicated(names(
  reverse_friendly_names
))]
if (length(dup_labels) > 0) {
  warning(
    "Duplicate friendly labels found in config.yml: ",
    paste(dup_labels, collapse = ", ")
  )
}

dauPortalTools::log_event("RISE Universal Portal configuration loaded")
