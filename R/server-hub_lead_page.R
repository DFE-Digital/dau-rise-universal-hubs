#' Polymorphic Hub Lead Designation Tracking Page Module Server
#'
#' Manages the master editing forms and status flags for institutional nodes designated
#' as operational management lead hubs.
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param selected_lead_id ReactiveVal containing the integer primary key ([ruhl_id]).
#' @param active_target ReactiveValues object tracking current contextual entity target state.
#' @export
server_hub_lead_page <- function(id, selected_lead_id, active_target) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    hubs_lookup <- reactive({
      dauPortalTools::db_ruh_get_hubs()
    })

    observeEvent(hubs_lookup(), {
      updateSelectInput(
        session,
        "hub_id",
        choices = setNames(hubs_lookup()$ruhb_id, hubs_lookup()$ruhb_name)
      )
    })

    observeEvent(selected_lead_id(), {
      req(selected_lead_id())

      rec <- dauPortalTools::db_ruh_get_lead_schools(
        ruhl_id = selected_lead_id()
      )
      if (nrow(rec) > 0) {
        updateSelectInput(session, "hub_id", selected = rec$ruhb_id)
        updateTextAreaInput(session, "comment", value = rec$ruhl_comment)

        if (!is.null(rec$ruhl_dateactive) && !is.na(rec$ruhl_dateactive)) {
          d_act <- as.Date(rec$ruhl_dateactive)
          updateSelectInput(
            session,
            "date_active_day",
            selected = format(d_act, "%d")
          )
          updateSelectInput(
            session,
            "date_active_month",
            selected = format(d_act, "%b")
          )
          updateSelectInput(
            session,
            "date_active_year",
            selected = format(d_act, "%Y")
          )
        }

        if (!is.null(rec$ruhl_dateended) && !is.na(rec$ruhl_dateended)) {
          d_end <- as.Date(rec$ruhl_dateended)
          updateSelectInput(
            session,
            "date_ended_day",
            selected = format(d_end, "%d")
          )
          updateSelectInput(
            session,
            "date_ended_month",
            selected = format(d_end, "%b")
          )
          updateSelectInput(
            session,
            "date_ended_year",
            selected = format(d_end, "%Y")
          )
        } else {
          updateSelectInput(session, "date_ended_day", selected = "")
          updateSelectInput(session, "date_ended_month", selected = "")
          updateSelectInput(session, "date_ended_year", selected = "")
        }
      }
    })

    observeEvent(input$save_lead_status, {
      req(selected_lead_id(), input$hub_id, active_target$type)
      req(
        input$date_active_day,
        input$date_active_month,
        input$date_active_year
      )

      if (!identical(active_target$type, "School")) {
        showNotification(
          "Lead designation edits are exclusively permitted against individual School nodes.",
          type = "error"
        )
        return()
      }

      clean_start <- input_to_date("date_active", input)
      clean_ended <- input_to_date("date_ended", input)

      if (is.na(clean_start)) {
        showNotification(
          "A valid active commencement date calculation target is required.",
          type = "error"
        )
        return()
      }

      dauPortalTools::db_ruh_update_lead_school(
        ruhl_id = selected_lead_id(),
        hub_id = as.integer(input$hub_id),
        date_active = format(clean_start, "%Y-%m-%d"),
        date_ended = if (is.na(clean_ended)) {
          NULL
        } else {
          format(clean_ended, "%Y-%m-%d")
        },
        is_active = if (is.na(clean_ended)) 1 else 0,
        comment = input$comment,
        user_id = dauPortalTools::get_user(session)
      )

      showNotification(
        "Lead Hub Designation records modified successfully.",
        type = "message"
      )
    })

    return(list(
      go_back = reactive({
        input$back_to_school
      })
    ))
  })
}
