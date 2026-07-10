#' Polymorphic Event Instance Allocation & Action Response Submodule Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param selected_event_id ReactiveVal tracking the unique integer parent event [ruev_id].
#' @param active_target ReactiveValues object tracking current session context id and type.
#' @export
server_event_instance_page <- function(id, selected_event_id, active_target) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_user <- dauPortalTools::get_user(session)

    refresh_event_master <- reactiveVal(0)
    refresh_sub_actions <- reactiveVal(0)

    active_event_data <- reactive({
      req(selected_event_id())
      refresh_event_master()

      df <- dauPortalTools::db_ru_get_events(ruev_id = selected_event_id())
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      as.list(df[1, ])
    })

    assigned_lead_provider <- reactive({
      req(selected_event_id())
      refresh_event_master()

      df <- dauPortalTools::db_ruh_get_lead_support_records(
        event_id = selected_event_id()
      )
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      as.list(df[1, ])
    })

    output$read_only_event_meta <- renderUI({
      ev <- active_event_data()
      req(ev)

      tagList(
        fluidRow(
          column(
            6,
            tags$strong("Primary Interaction Method:"),
            p(ev$event_type_name)
          ),
          column(
            6,
            tags$strong("Cohort Focus Sub-Variety:"),
            p(ev$event_sub_variety_name)
          )
        ),
        br(),
        fluidRow(
          column(
            6,
            tags$strong("Interaction Event Date:"),
            p(as.character(ev$ruev_date))
          ),
          column(
            6,
            tags$strong("Transaction Status Frame:"),
            p(
              if (ev$ruev_completed == 1) {
                "Completed Log"
              } else {
                "Pending Pipeline Track"
              }
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            tags$strong("Top-Level Event Container Summary Notes:"),
            p(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; border: 1px solid #dee2e6;",
              if (
                is.null(ev$ruev_summary_notes) || !nzchar(ev$ruev_summary_notes)
              ) {
                "No detailed narrative parameters recorded."
              } else {
                ev$ruev_summary_notes
              }
            )
          )
        )
      )
    })

    output$lead_provider_status_banner <- renderUI({
      lead <- assigned_lead_provider()

      if (is.null(lead)) {
        div(
          class = "alert alert-warning d-flex justify-content-between align-items-center m-0",
          tags$span(
            icon("exclamation-triangle"),
            " No explicit Lead Provider supporting entity linked directly to this event sequence yet."
          ),
          actionButton(
            ns("btn_assign_lead_provider"),
            "Link Supporting Lead",
            class = "btn btn-primary btn-sm",
            icon = icon("link")
          )
        )
      } else {
        div(
          class = "alert alert-info d-flex justify-content-between align-items-center m-0",
          tags$span(
            icon("award"),
            tags$strong(glue::glue(
              " Managed By Lead {lead$lead_entity_type}: "
            )),
            glue::glue("{lead$lead_entity_id} (Track ID: {lead$ruhls_id})")
          ),
          actionButton(
            ns("btn_assign_lead_provider"),
            "Change Lead Provider",
            class = "btn btn-dark btn-sm",
            icon = icon("exchange-alt")
          )
        )
      }
    })

    observeEvent(input$btn_assign_lead_provider, {
      ev <- active_event_data()
      req(ev)

      showModal(modalDialog(
        title = "Link Operational Lead Supporting Node to Event Window",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel Linkage Track"),
          actionButton(
            ns("submit_lead_provider_binding"),
            "Save Assignment Link",
            class = "btn-success"
          )
        ),
        tagList(
          p(em(glue::glue(
            "Select an active operational institutional lead configuration context to manage this specific event instance."
          ))),
          hr(),
          DT::DTOutput(ns("available_leads_picker_table"))
        )
      ))
    })

    output$available_leads_picker_table <- DT::renderDT(
      {
        ev <- active_event_data()
        req(ev)

        all_leads <- dauPortalTools::db_ruh_get_lead_support_records()

        if (is.null(all_leads) || nrow(all_leads) == 0) {
          return(data.frame(
            "Status" = "No registered institutional providers exist in the master system profiles ledger."
          ))
        }

        current_lead <- assigned_lead_provider()
        exclude_lead_ids <- if (is.null(current_lead)) {
          integer(0)
        } else {
          current_lead$ruhls_id
        }

        filtered_pool <- all_leads[
          is.na(all_leads$ruev_id) | all_leads$ruhls_id == exclude_lead_ids,
        ]

        filtered_pool |>
          dplyr::select(
            ruhls_id,
            lead_entity_type,
            lead_entity_id,
            ruhl_dateactive,
            ruhl_active
          ) |>
          dplyr::rename(
            "Master Track ID" = ruhls_id,
            "Provider Type Index" = lead_entity_type,
            "Provider Core ID/URN" = lead_entity_id,
            "Assignment Date Registered" = ruhl_dateactive,
            "Active Flag State" = ruhl_active
          )
      },
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 5, dom = "tp")
    )

    observeEvent(input$submit_lead_provider_binding, {
      req(selected_event_id())
      selected_row_idx <- input$available_leads_picker_table_rows_selected

      if (length(selected_row_idx) == 0) {
        showNotification(
          "You must select an institutional row item to save the context binding.",
          type = "warning"
        )
        return()
      }

      ev <- active_event_data()
      all_leads <- dauPortalTools::db_ruh_get_lead_support_records()
      current_lead <- assigned_lead_provider()
      exclude_lead_ids <- if (is.null(current_lead)) {
        integer(0)
      } else {
        current_lead$ruhls_id
      }
      filtered_pool <- all_leads[
        is.na(all_leads$ruev_id) | all_leads$ruhls_id == exclude_lead_ids,
      ]

      target_ruhls_id <- filtered_pool$ruhls_id[selected_row_idx]

      removeModal()

      if (!is.null(current_lead)) {
        dauPortalTools::db_ruh_update_lead_support(
          ruhls_id = current_lead$ruhls_id,
          lead_type = current_lead$lead_entity_type,
          lead_id = current_lead$lead_entity_id,
          hub_id = current_lead$ruhb_id,
          event_id = NULL,
          date_active = current_lead$ruhl_dateactive,
          is_active = current_lead$ruhl_active,
          comment = current_lead$ruhl_comment,
          user_id = current_user
        )
      }

      selected_lead_meta <- all_leads[all_leads$ruhls_id == target_ruhls_id, ]
      dauPortalTools::db_ruh_update_lead_support(
        ruhls_id = target_ruhls_id,
        lead_type = selected_lead_meta$lead_entity_type,
        lead_id = selected_lead_meta$lead_entity_id,
        hub_id = selected_lead_meta$ruhb_id,
        event_id = as.integer(selected_event_id()),
        date_active = selected_lead_meta$ruhl_dateactive,
        is_active = selected_lead_meta$ruhl_active,
        comment = selected_lead_meta$ruhl_comment,
        user_id = current_user
      )

      showNotification(
        "Lead Provider support structure linked cleanly to this point-in-time execution context.",
        type = "message"
      )
      refresh_event_master(refresh_event_master() + 1)
    })

    output$sub_actions_executions_table <- DT::renderDT(
      {
        req(selected_event_id())
        refresh_sub_actions()

        df <- dauPortalTools::db_ru_get_event_action_responses(
          event_id = selected_event_id()
        )
        if (is.null(df) || nrow(df) == 0) {
          return(data.frame(
            "Form Data Metrics" = "No structured action metrics entries are compiled for this entity timeline layout frame yet."
          ))
        }

        df |>
          dplyr::select(
            ruevar_id,
            rueva_name,
            ruevar_value,
            rueva_rule_type,
            rueva_required
          ) |>
          dplyr::rename(
            "Response ID" = ruevar_id,
            "Required Action Parameter" = rueva_name,
            "Logged Value String" = ruevar_value,
            "Data Format Type" = rueva_rule_type,
            "Mandatory?" = rueva_required
          )
      },
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tp")
    )

    return(list(
      go_back = reactive({
        input$back_to_profile
      })
    ))
  })
}
