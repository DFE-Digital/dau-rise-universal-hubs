#' Master Event Types Catalog Search Server
#' @export
server_event_catalog_page <- function(
  id,
  global_selected_event_type_id,
  main_navbar_session
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh_trigger <- reactiveVal(0)

    event_types_data <- reactive({
      refresh_trigger()
      df <- dauPortalTools::db_ru_get_event_types()

      if (nzchar(input$filter_evt_name %||% "")) {
        df <- df |>
          dplyr::filter(grepl(
            input$filter_evt_name,
            ruevt_name,
            ignore.case = TRUE
          ))
      }
      df
    })

    output$event_types_table <- DT::renderDT({
      req(event_types_data())
      df <- event_types_data()
      if (nrow(df) == 0) {
        return(data.frame("Status" = "No event categories configured."))
      }

      DT::datatable(
        df |> dplyr::select(ruevt_id, ruevt_name, ruevt_description),
        colnames = c(
          "Event Type ID",
          "Interaction Method Title",
          "Blueprint Description Scope"
        ),
        rownames = FALSE,
        selection = "single",
        options = list(pageLength = 15, dom = "tp"),
        callback = DT::JS(paste0(
          "
          table.on('dblclick', 'tr', function() {
            var data = table.row(this).data();
            if (data) {
              Shiny.setInputValue('",
          ns("evt_row_dblclicked"),
          "', data[0], {priority: 'event'});
            }
          });
          "
        ))
      )
    })

    observeEvent(input$new_event_type_btn, {
      showModal(modalDialog(
        title = "Define Pristine New Event Category Structure",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_new_type"),
            "Commit Event Definition",
            class = "btn-success"
          )
        ),
        tagList(
          textInput(
            ns("modal_name"),
            "Event Method Variant Name:",
            placeholder = "e.g., Intensive Diagnostic Audit"
          ),
          textAreaInput(
            ns("modal_desc"),
            "Methodological Directives & Scope Guidelines:",
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$save_new_type, {
      req(input$modal_name)
      removeModal()

      dauPortalTools::db_ru_add_event_type(
        name = input$modal_name,
        description = input$modal_desc,
        user_id = dauPortalTools::get_user(session)
      )

      showNotification(
        "New interactive event category catalog entry saved.",
        type = "message"
      )
      refresh_trigger(refresh_trigger() + 1)
    })

    observeEvent(input$evt_row_dblclicked, {
      id_val <- suppressWarnings(as.integer(input$evt_row_dblclicked))
      req(!is.na(id_val))

      global_selected_event_type_id(id_val)
      updateNavbarPage(
        main_navbar_session,
        "main_navbar",
        selected = "event_overview"
      )
    })
  })
}
