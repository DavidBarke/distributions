navbar_right_ui <- function(id) {
  ns <- shiny::NS(id)

  # Fake dropdowns
  htmltools::tagList(
    htmltools::tags$li(
      class = "nav-item dropdown",
      shiny::actionLink(
        inputId = ns("intro"),
        label = NULL,
        icon = shiny::icon("question-circle"),
        class = "nav-link"
      )
    ),
    dropdown_menu(
      icon = shiny::icon("download"),
      dropdown_download_item(
        outputId = ns("save_rds"),
        label = "Save as RDS"
      ),
      intro_box_factory = function(ui) {
        rintrojs::introBox(
          ui,
          data.step = 4,
          data.intro = htmltools::tagList(
            htmltools::p(
              "All present distributions can be saved in the .rds file format,
              so that you can either import them in your R session or load them
              at a later point in time. This enables you to prepare a set of
              distributions once and reuse and enhance it anytime in the
              future."
            )
          )
        )
      }
    ),
    dropdown_menu(
      icon = shiny::icon("upload"),
      dropdown_item(
        inputId = ns("load_rds"),
        label = "Load RDS",
        icon = shiny::icon("upload")
      ),
      intro_box_factory = function(ui) {
        rintrojs::introBox(
          ui,
          data.step = 5,
          data.intro = htmltools::tagList(
            htmltools::p(
              "Alternatively you can prepare distributions in your R sessions
              and upload them to this application. For more
              details read the help text in the upload dialog."
            )
          )
        )
      }
    )
  )
}

navbar_right_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$intro, {
        glouton::add_cookie("intro", "true")
        rintrojs::introjs(
          session,
          options = list(
            showStepNumbers = FALSE
          )
        )
      })

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
