modal_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::modalDialog(
    title = "Upload distributions from an RDS file",
    easyClose = TRUE,
    bs4Dash::tabBox(
      id = ns("tabs"),
      width = NULL,
      collapsible = FALSE,
      shiny::tabPanel(
        title = "Upload",
        shiny::selectInput(
          inputId = ns("source"),
          label = "Source",
          choices = c("From .rds" = "user", "Predefined" = "predefined")
        ),
        shiny::conditionalPanel(
          "input.source === 'user'",
          shiny::fileInput(
            inputId = ns("file"),
            label = "Select an RDS file",
            accept = ".rds",
            width = "100%"
          ),
          ns = ns
        ),
        shiny::conditionalPanel(
          "input.source === 'predefined'",
          shiny::selectInput(
            inputId = ns("predefined"),
            label = "Select a predefined set of distributions",
            choices = c(
              "5 Normal Distributions" = "normal_5.rds",
              "100 Normal Distributions" = "normal_100.rds",
              "1 Distribution of Every Type" = "all_types.rds",
              "Approximations of the Normal Distribution" = "normal_approximations.rds"
            )
          ),
          ns = ns
        ),
        shiny::uiOutput(
          outputId = ns("settings")
        )
      ),
      shiny::tabPanel(
        title = "Help",
        htmltools::includeMarkdown(
          "md/upload_help.md"
        )
      )
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
        if (input$source == "predefined") return(FALSE)

        ext_r() != "rds"
      })

      content_r <- shiny::reactive({
        if (input$source == "user") {
          readr::read_rds(file_r()$datapath)
        } else {
          readr::read_rds(file.path("examples", input$predefined))
        }
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
          if (!is.null(formatted_content_r()$min)) {
            .values$set_plot_min(formatted_content_r()$min[[1]])
          }

          if (!is.null(formatted_content_r()$max)) {
            .values$set_plot_max(formatted_content_r()$max[[1]])
          }

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

            .values$distribution_manager$load_rv(
              c(
                .values$distribution_manager$load_rv(),
                list(
                  list(
                    distributions = unname(formatted_content_r()$inactive),
                    override = settings_return$override_r(),
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
    !is_distribution_or_null(x$active) ||
    !is_distribution_or_null(x$inactive)
  ) return(FALSE)

  return(TRUE)
}

# format_content and get_content_type don't need to check that x is valid
# content. They are only called after the check has been done by is_valid_content
format_content <- function(x) {
  if (distributional::is_distribution(x)) return(x)

  x[c("active", "inactive", "min", "max")]
}

get_content_type <- function(x) {
  if (distributional::is_distribution(x)) return("distribution_vec")

  "distribution_list"
}
