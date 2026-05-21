#' Render the About Page
#'
#' Returns the UI for the application's About and Version History page.
ui_about_page <- function() {
  tagList(
    h1(class = "govuk-heading-l", "About This Application"),

    p(
      class = "govuk-body",
      "To support Regions Group, we are pleased to provide access to this portal as a modern, 
      high-performance replacement for KIM. This dashboard is specifically engineered to 
      manage and report on Significant Change applications with greater speed and reliability."
    ),

    hr(style = "border-top: 1px solid #b1b4b6; margin: 30px 0;"),

    # --- Policy and Support Section ---
    fluidRow(
      column(
        width = 6,
        h2(class = "govuk-heading-m", "Policy & Guidance"),
        div(
          style = "padding: 15px; border-left: 5px solid #1d70b8; background: #f3f2f1; height: 100%;",
          p(
            class = "govuk-body",
            "Official guidance on Significant Changes can be found on the ",
            tags$a(
              href = "https://educationgovuk.sharepoint.com/sites/RegionsGroup2/Shared%20Documents/Forms/AllItems.aspx?id=%2Fsites%2FRegionsGroup2%2FShared%20Documents%2FStrategic%20conversations%2F2023%2D24%20Guidance",
              "Regions Group SharePoint site."
            ),
            " Ensure you are familiar with the latest criteria before submitting changes."
          )
        )
      ),
      column(
        width = 6,
        h2(class = "govuk-heading-m", "Support"),
        div(
          style = "padding: 15px; border-left: 5px solid #00703c; background: #f3f2f1; height: 100%;",
          p(
            class = "govuk-body",
            tags$strong("Technical Support: "),
            tags$a(
              href = "mailto:ben7.smith@education.gov.uk?subject=Significant Change Portal Support",
              "Email Ben Smith",
              icon("envelope")
            ),
            br(),
            tags$strong("User Manual: "),
            tags$a(
              href = "https://educationgovuk.sharepoint.com/sites/lvewp00030/SitePages/2-Intervention/Academy%20Letters%20&%20Warning%20Notices/Academy%20Letters%20&%20Warning%20Notices.aspx",
              "View Guide",
              icon("book")
            )
          )
        )
      )
    ),

    # --- Live Announcements ---
    h2(
      class = "govuk-heading-l",
      style = "margin-top: 40px;",
      "Current System Alerts"
    ),
    ui_show_news(),

    # --- Version History ---
    h2(
      class = "govuk-heading-l",
      style = "margin-top: 40px;",
      "Version History"
    ),

    # 2026.5.0 : The Sound of Silence
    tags$details(
      class = "govuk-details",
      open = "open",
      tags$summary(
        class = "govuk-details__summary",
        tags$span(
          class = "govuk-details__summary-text",
          "2026.5.0 : The Sound of Silence"
        )
      ),
      div(
        class = "govuk-details__text",
        p(
          class = "govuk-body",
          tags$em(
            "\"Hello stability, my old friend... we’ve come to fix the leaks again.\""
          )
        ),
        p(
          class = "govuk-body",
          "This update represents a massive infrastructure overhaul. We’ve reached into the darkness of the 
          global scope to ensure that critical info is no longer just 'written on the tenement blocks' 
          of our code."
        ),
        tags$ul(
          class = "govuk-list govuk-list--bullet",
          tags$li(
            tags$strong("Battle-Tested:"),
            " Over 70 automated stability tests passed to ensure module reliability."
          ),
          tags$li(
            tags$strong("Connection Governance:"),
            " Refactored SQL lifecycles to kill stale links. Please watch for the Blue success toast on save."
          ),
          tags$li(
            tags$strong("The Pill Picker:"),
            " Replaced chunky calendar pop-ups with a sleek, manual date input."
          ),
          tags$li(
            tags$strong("Neon Light of News:"),
            " Introduced the live System News & Alerts card."
          ),
          tags$li(
            tags$strong("Purging the Shame:"),
            " Replaced the last remaining PowerApp-era HTML with native Shiny components."
          )
        )
      )
    ),

    # 2026.3.0 : Smooth Portal-ator
    tags$details(
      class = "govuk-details",
      tags$summary(
        class = "govuk-details__summary",
        tags$span(
          class = "govuk-details__summary-text",
          "2026.3.0 : Smooth Portal‑ator!"
        )
      ),
      div(
        class = "govuk-details__text",
        p(
          class = "govuk-body",
          "Smoothed out rough edges and reorganized key pages to make Significant Change admin feel calmer and clearer."
        ),
        tags$ul(
          class = "govuk-list govuk-list--bullet",
          tags$li(
            "Major workflow improvements for creating, editing, and withdrawing records."
          ),
          tags$li("Restructured data-entry forms for better intuition."),
          tags$li("New user assignment system for tracking case ownership."),
          tags$li("Enhanced school information layout.")
        )
      )
    ),

    # 2025.8.0 : I'm so R Shiny
    tags$details(
      class = "govuk-details",
      tags$summary(
        class = "govuk-details__summary",
        tags$span(
          class = "govuk-details__summary-text",
          "2025.8.0 : I'm so R Shiny!"
        )
      ),
      div(
        class = "govuk-details__text",
        p(
          class = "govuk-body",
          "The entire engine was ripped out and replaced to solve the reliability issues seen with PowerApps."
        ),
        tags$ul(
          class = "govuk-list govuk-list--bullet",
          tags$li("Full migration from PowerApps to R Shiny framework."),
          tags$li("Improved global search functionality.")
        )
      )
    ),

    # 2025.4.0 : Preparing for a better process
    tags$details(
      class = "govuk-details",
      tags$summary(
        class = "govuk-details__summary",
        tags$span(
          class = "govuk-details__summary-text",
          "2025.4.0 : Preparing for a better SigChange process"
        )
      ),
      div(
        class = "govuk-details__text",
        tags$ul(
          class = "govuk-list govuk-list--bullet",
          tags$li(
            "Laying the groundwork for improved reporting and data quality."
          ),
          tags$li(
            "Added ability to link directly to school significant change lists."
          ),
          tags$li("Integrated initial welcome messaging and support links.")
        )
      )
    ),

    # --- App Metadata Footer ---
    div(
      style = "margin-top: 50px; padding: 20px; background-color: #f3f2f1; border-radius: 4px; border: 1px solid #b1b4b6;",
      fluidRow(
        column(
          width = 3,
          tags$b("Developed By:"),
          br(),
          "School System Status & Reform"
        ),
        column(width = 3, tags$b("Version:"), br(), "2026.5.0"),
        column(
          width = 3,
          tags$b("Status:"),
          br(),
          span(style = "color: #00703c; font-weight: bold;", "Pre-Prod Beta")
        ),
        column(
          width = 3,
          tags$b("Target Live Date:"),
          br(),
          "Wednesday 13th May"
        )
      )
    ),
    br(),
    br()
  )
}
