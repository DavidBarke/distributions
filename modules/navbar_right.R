navbar_right_ui <- function(id) {
  ns <- shiny::NS(id)

  # Fake dropdowns
  htmltools::tagList(
    dropdown_menu(
      icon = shiny::icon("download"),
      dropdown_download_item(
        outputId = ns("save_rds"),
        label = "Save as RDS"
      )#,
      # dropdown_download_item(
      #   outputId = ns("save_json"),
      #   label = "Save as JSON"
      # )
    ),
    dropdown_menu(
      icon = shiny::icon("upload"),
      dropdown_item(
        inputId = ns("load_rds"),
        label = "Load RDS",
        icon = shiny::icon("upload")
      )#,
      # dropdown_item(
      #   inputId = ns("load_json"),
      #   label = "Load JSON",
      #   icon = shiny::icon("upload")
      # )
    )
  )
}

navbar_right_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      content_r <- shiny::reactive({
        list(
          active = .values$distribution_manager$active_distributions_r(),
          inactive = .values$distribution_manager$inactive_distributions_r()
        )
      })

      output$save_rds <- shiny::downloadHandler(
        filename = filename_factory(extension = "rds"),
        content = function(file) {
          readr::write_rds(content_r(), file)
        },
        contentType = "application/rds"
      )

      shiny::observeEvent(input$load_rds, {
        shiny::showModal(modal_upload_ui(
          id = ns("modal_upload")
        ))
      })

      modal_upload_server(
        id = "modal_upload",
        .values = .values
      )
    }
  )
}

filename_factory <- function(extension = c("rds", "json")) {
  extension <- match.arg(extension)

  function() {
    date <- stringr::str_replace_all(Sys.time(), "\\s", "_") %>%
      stringr::str_replace_all(":", "-")
    glue::glue("distributions_{date}.{extension}")
  }
}
