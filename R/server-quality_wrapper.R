server_quality_wrapper <- function(id, app_id) {
  moduleServer(id, function(input, output, session) {
    df <- reactiveVal(NULL)

    observeEvent(
      TRUE,
      {
        raw_ui <- dauPortalTools::quality_render_live(app_id = app_id)

        df(attr(raw_ui, "data"))
      },
      once = TRUE
    )

    filtered <- reactive({
      req(df())
      x <- df()

      if (input$region_filter != "All") {
        x <- x[x$Region == input$region_filter, ]
      }

      if (input$with_rcs_filter != "All") {
        x <- x[x$`With RCS?` == as.numeric(input$with_rcs_filter), ]
      }

      x
    })

    output$filtered_table <- DT::renderDT({
      req(filtered())
      datatable(
        filtered(),
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 15)
      )
    })
  })
}
