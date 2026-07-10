#' Polymorphic Hub Lead & Provider Contract Management Page Module Server
#'
#' @param id Character scalar. Shiny namespace identifier.
#' @param selected_lead_id ReactiveVal containing the integer master ledger primary key ([ruhls_id]).
#' @param active_target ReactiveValues object tracking current contextual entity target state.
#' @export
server_hub_lead_page <- function(id, selected_lead_id, active_target) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_user <- dauPortalTools::get_user(session)

    refresh_cohorts <- reactiveVal(0)
    refresh_records <- reactiveVal(0)
    refresh_master <- reactiveVal(0)

    master_record <- reactive({
      req(selected_lead_id())
      refresh_master()

      df <- dauPortalTools::db_ruh_get_lead_support_records(
        ruhls_id = selected_lead_id()
      )
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }
      as.list(df[1, ])
    })

    output$read_only_profile <- renderUI({
      rec <- master_record()
      req(rec)

      context_label <- if (!is.null(rec$hub_name) && nzchar(rec$hub_name)) {
        paste("Hub Framework:", rec$hub_name)
      } else if (!is.null(rec$event_summary) && nzchar(rec$event_summary)) {
        paste("Event Timeline Context:", rec$event_summary)
      } else {
        "Global / Open Context Assignment"
      }

      tagList(
        fluidRow(
          column(
            4,
            tags$strong("Provider Entity:"),
            p(glue::glue("{rec$lead_entity_type} (ID: {rec$lead_entity_id})"))
          ),
          column(
            4,
            tags$strong("Operational Context Scope:"),
            p(context_label)
          ),
          column(
            4,
            tags$strong("Current Status:"),
            p(if (rec$ruhl_active == 1) "Active" else "Concluded")
          )
        ),
        fluidRow(
          column(
            4,
            tags$strong("Commencement Date:"),
            p(as.character(rec$ruhl_dateactive))
          ),
          column(
            4,
            tags$strong("Conclusion Date:"),
            p(
              if (is.na(rec$ruhl_dateended) || is.null(rec$ruhl_dateended)) {
                "Ongoing"
              } else {
                as.character(rec$ruhl_dateended)
              }
            )
          ),
          column(
            4,
            tags$strong("Notes / Directives:"),
            p(
              if (is.null(rec$ruhl_comment) || is.na(rec$ruhl_comment)) {
                "No custom narrative logs filed."
              } else {
                rec$ruhl_comment
              }
            )
          )
        )
      )
    })

    output$summary_stats_container <- renderUI({
      req(selected_lead_id())
      refresh_records()

      assigned_df <- dauPortalTools::db_ruh_get_assigned_support_records(
        ruhls_id = selected_lead_id()
      )
      total_count <- if (is.null(assigned_df)) 0 else nrow(assigned_df)

      bslib::value_box(
        title = "Entities Supported",
        value = total_count,
        showcase = icon("users"),
        theme = "primary",
        p(
          "Total localized individual contract instances currently allocated directly under this provider assignment window."
        )
      )
    })

    output$cohorts_table <- DT::renderDT(
      {
        req(selected_lead_id())
        refresh_cohorts()

        df <- dauPortalTools::db_ruh_get_lead_cohorts(
          ruhls_id = selected_lead_id()
        )
        if (is.null(df) || nrow(df) == 0) {
          return(data.frame(
            "Cohorts" = "This assignment is bound globally at the operational hub line level."
          ))
        }

        df |>
          dplyr::select(ruhlc_id, cohort_id) |>
          dplyr::rename(
            "Assignment Map ID" = ruhlc_id,
            "Allocated Cohort Number" = cohort_id
          )
      },
      selection = "none",
      rownames = FALSE,
      options = list(dom = "t")
    )

    observeEvent(input$btn_add_cohort, {
      rec <- master_record()
      req(rec$ruhb_id)

      cohorts_df <- dauPortalTools::db_ruh_get_support_types(
        hub_id = as.integer(rec$ruhb_id)
      )

      cohort_choices <- if (is.null(cohorts_df) || nrow(cohorts_df) == 0) {
        c("No categories configured (Default Global)" = 0)
      } else {
        setNames(cohorts_df$ruht_id, cohorts_df$ruht_name)
      }

      showModal(modalDialog(
        title = "Assign Cohort/Support Type Target to Provider",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("submit_new_cohort"),
            "Commit Cohort Block",
            class = "btn-success"
          )
        ),
        selectInput(
          ns("modal_cohort_id"),
          "Select Target Cohort Scope Framework:",
          choices = cohort_choices
        )
      ))
    })

    observeEvent(input$submit_new_cohort, {
      req(input$modal_cohort_id, selected_lead_id())
      removeModal()

      current_cohorts <- dauPortalTools::db_ruh_get_lead_cohorts(
        ruhls_id = selected_lead_id()
      )$cohort_id
      updated_cohorts <- unique(c(
        current_cohorts,
        as.integer(input$modal_cohort_id)
      ))

      dauPortalTools::db_ruh_set_lead_cohorts(
        ruhls_id = selected_lead_id(),
        cohort_ids = updated_cohorts
      )
      showNotification(
        "Cohort catalog sync updated successfully.",
        type = "message"
      )
      refresh_cohorts(refresh_cohorts() + 1)
    })

    output$assigned_entities_table <- DT::renderDT(
      {
        req(selected_lead_id())
        refresh_records()

        df <- dauPortalTools::db_ruh_get_assigned_support_records(
          ruhls_id = selected_lead_id()
        )
        if (is.null(df) || nrow(df) == 0) {
          return(data.frame(
            "Status" = "No individual receiving contract tracks mapped under this provider identity frame."
          ))
        }

        df |>
          dplyr::select(
            ruhlasr_id,
            ruhsr_id,
            ruhsr_entity_type,
            ruhsr_entity_id,
            support_type_name
          ) |>
          dplyr::rename(
            "Link Primary Key" = ruhlasr_id,
            "Support Record Contract ID" = ruhsr_id,
            "Target Entity Node" = ruhsr_entity_type,
            "Target Identity URN/ID" = ruhsr_entity_id,
            "Framework Sub-Category Name" = support_type_name
          )
      },
      selection = "none",
      rownames = FALSE,
      options = list(pageLength = 5, dom = "tp")
    )

    observeEvent(input$btn_add_entity_link, {
      rec <- master_record()
      req(rec)

      showModal(modalDialog(
        title = "Link Active Downstream Support Contracts",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close Dialog"),
          actionButton(
            ns("submit_selected_record_links"),
            "Link Selection Arrays",
            class = "btn-success"
          )
        ),
        tagList(
          p(em(
            "The tracking table below dynamically filters active global system support records matching the operational parent Hub/Event scope assigned above."
          )),
          hr(),
          DT::DTOutput(ns("available_records_picker_table"))
        )
      ))
    })

    output$available_records_picker_table <- DT::renderDT(
      {
        rec <- master_record()
        req(rec)

        all_records <- dauPortalTools::db_ruh_get_support_records(
          hub_id = rec$ruhb_id
        )

        already_linked <- dauPortalTools::db_ruh_get_assigned_support_records(
          ruhls_id = selected_lead_id()
        )$ruhsr_id
        available_records <- all_records[
          !all_records$ruhsr_id %in% already_linked,
        ]

        if (is.null(available_records) || nrow(available_records) == 0) {
          return(data.frame(
            "Status Notification" = "No isolated active tracking records are unallocated within this master framework domain context."
          ))
        }

        available_records |>
          dplyr::select(
            ruhsr_id,
            ruhsr_entity_type,
            ruhsr_entity_id,
            support_type_name,
            ruhsr_dateactive
          ) |>
          dplyr::rename(
            "Record ID" = ruhsr_id,
            "Node Level" = ruhsr_entity_type,
            "Receiver Identity Code/URN" = ruhsr_entity_id,
            "Framework Definition Sub-Type" = support_type_name,
            "Date Active Commenced" = ruhsr_dateactive
          )
      },
      selection = "multiple",
      rownames = FALSE,
      options = list(pageLength = 5, dom = "tp")
    )

    observeEvent(input$submit_selected_record_links, {
      req(selected_lead_id())

      selected_row_indices <- input$available_records_picker_table_rows_selected
      if (length(selected_row_indices) == 0) {
        showNotification(
          "No entity rows were chosen for allocation link mapping.",
          type = "warning"
        )
        return()
      }

      rec <- master_record()
      all_records <- dauPortalTools::db_ruh_get_support_records(
        hub_id = rec$ruhb_id
      )
      already_linked <- dauPortalTools::db_ruh_get_assigned_support_records(
        ruhls_id = selected_lead_id()
      )$ruhsr_id
      available_records <- all_records[
        !all_records$ruhsr_id %in% already_linked,
      ]

      target_ruhsr_ids <- available_records$ruhsr_id[selected_row_indices]
      combined_ledger_set <- unique(c(already_linked, target_ruhsr_ids))

      removeModal()

      dauPortalTools::db_ruh_set_assigned_support_records(
        ruhls_id = selected_lead_id(),
        ruhsr_ids = combined_ledger_set
      )
      showNotification(
        "Entity support tracks linked to provider timeline ledger safely.",
        type = "message"
      )
      refresh_records(refresh_records() + 1)
    })

    observeEvent(input$btn_open_edit_modal, {
      rec <- master_record()
      req(rec)

      hubs_df <- dauPortalTools::db_ruh_get_hubs()
      hubs_choices <- setNames(hubs_df$ruhb_id, hubs_df$ruhb_name)

      showModal(modalDialog(
        title = "Edit Provider Profile Assignment Framework Specifications",
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel Modifications"),
          actionButton(
            ns("submit_master_ledger_update"),
            "Commit Modifications",
            class = "btn-success"
          )
        ),
        tagList(
          selectInput(
            ns("modal_edit_hub_id"),
            "Re-Target Linked Hub Framework Context:",
            choices = hubs_choices,
            selected = rec$ruhb_id
          ),
          selectInput(
            ns("modal_edit_status"),
            "Provider Configuration Allocation State Active Status:",
            choices = list(
              "Active Provisioning" = 1,
              "Concluded Assignment Track" = 0
            ),
            selected = rec$ruhl_active
          ),
          ui_date_input(
            ns("modal_edit_date_start"),
            "Provider Track Initiation Active Start Date:",
            value = as.Date(rec$ruhl_dateactive)
          ),
          textAreaInput(
            ns("modal_edit_comment"),
            "Narrative Operational Framework Comments / Directives:",
            value = rec$ruhl_comment,
            rows = 3
          )
        )
      ))
    })

    observeEvent(input$submit_master_ledger_update, {
      req(selected_lead_id())
      req(
        input$modal_edit_date_start_day,
        input$modal_edit_date_start_month,
        input$modal_edit_date_start_year
      )

      clean_date <- input_to_date("modal_edit_date_start", input)
      if (is.na(clean_date)) {
        showNotification(
          "Invalid Date configuration criteria entered. Re-check parameters.",
          type = "error"
        )
        return()
      }

      rec <- master_record()
      removeModal()

      dauPortalTools::db_ruh_update_lead_support(
        ruhls_id = selected_lead_id(),
        lead_type = rec$lead_entity_type,
        lead_id = rec$lead_entity_id,
        hub_id = as.integer(input$modal_edit_hub_id),
        event_id = NULL,
        date_active = format(clean_date, "%Y-%m-%d"),
        date_ended = if (as.integer(input$modal_edit_status) == 0) {
          format(Sys.Date(), "%Y-%m-%d")
        } else {
          NULL
        },
        is_active = as.integer(input$modal_edit_status),
        comment = input$modal_edit_comment,
        user_id = current_user
      )

      showNotification(
        "Master provider specifications updated successfully.",
        type = "message"
      )
      refresh_master(refresh_master() + 1)
      refresh_cohorts(refresh_cohorts() + 1)
      refresh_records(refresh_records() + 1)
    })

    return(list(
      go_back = reactive({
        input$back_to_profile
      })
    ))
  })
}
