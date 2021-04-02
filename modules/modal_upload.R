modal_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::modalDialog(
    title = "Upload distributions from an RDS file",
    easyClose = TRUE,
    shiny::fileInput(
      inputId = ns("file"),
      label = "Select an RDS file",
      accept = ".rds",
      width = "100%"
    ),
    shiny::uiOutput(
      outputId = ns("settings")
    ),
    footer = htmltools::tagList(
      shiny::uiOutput(
        outputId = ns("confirm_upload")
      ),
      shiny::modalButton(
        label = "Dismiss",
        icon = shiny::icon("times")
      )
    )
  )
}

modal_upload_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      file_r <- shiny::reactive({
        shiny::req(input$file)
      })

      ext_r <- shiny::reactive({
        tools::file_ext(file_r()$datapath)
      })

      wrong_ext_r <- shiny::reactive({
        ext_r() != "rds"
      })

      content_r <- shiny::reactive({
        readr::read_rds(file_r()$datapath)
      })

      wrong_content_r <- shiny::reactive({
        !is_valid_content(content_r())
      })

      content_type_r <- shiny::reactive({
        get_content_type(content_r())
      })

      formatted_content_r <- shiny::reactive({
        format_content(content_r())
      })

      output$settings <- shiny::renderUI({
        shiny::validate(
          shiny::need(!wrong_ext_r(), "Please upload an RDS file"),
          shiny::need(!wrong_content_r(), "The content of the uploaded file does not have the appropriate format")
        )

        modal_upload_settings_ui(
          id = ns("modal_upload_settings")
        )
      })

      error_r <- shiny::reactive({
        wrong_ext_r() ||
          wrong_content_r()
      })

      output$confirm_upload <- shiny::renderUI({
        if (!error_r()) {
          shiny::actionButton(
            inputId = ns("confirm_upload"),
            label = "Load distributions",
            icon = shiny::icon("check")
          )
        }
      })

      shiny::observeEvent(input$confirm_upload, {
        shiny::removeModal()

        if (content_type_r() == "distribution_vec") {
          .values$distribution_manager$load_rv(
            list(
              list(
                distributions = formatted_content_r(),
                override = settings_return$override_r(),
                to = settings_return$to_r()
              )
            )
          )
        }

        if (content_type_r() == "distribution_list") {
          if (settings_return$include_active_r()) {
            .values$distribution_manager$load_rv(
              list(
                list(
                  distributions = unname(formatted_content_r()$active),
                  override = settings_return$override_r(),
                  to = "active"
                )
              )
            )
          }

          if (settings_return$include_inactive_r()) {
            # override is FALSE unless active is not included, then override
            # is taken from settings. This is to prevent overriding when
            # after active distributions were loaded
            override <- !settings_return$include_active_r() &&
              settings_return$override_r()

            .values$distribution_manager$load_rv(
              c(
                .values$distribution_manager$load_rv(),
                list(
                  list(
                    distributions = unname(formatted_content_r()$inactive),
                    override = override,
                    to = "inactive"
                  )
                )
              )
            )
          }
        }
      })

      settings_return <- modal_upload_settings_server(
        id = "modal_upload_settings",
        .values = .values,
        content_r = formatted_content_r
      )
    }
  )
}

is_valid_content <- function(x) {
  if (distributional::is_distribution(x)) return(TRUE)

  if (!hasName(x, "active") || !hasName(x, "inactive")) return(FALSE)

  if (
    !distributional::is_distribution(x$active) ||
    !distributional::is_distribution(x$inactive)
  ) return(FALSE)

  return(TRUE)
}

# format_content and get_content_type don't need to check that x is valid
# content. They are only called after the check has been done by is_valid_content
format_content <- function(x) {
  if (distributional::is_distribution(x)) return(x)

  x[c("active", "inactive")]
}

get_content_type <- function(x) {
  if (distributional::is_distribution(x)) return("distribution_vec")

  "distribution_list"
}
