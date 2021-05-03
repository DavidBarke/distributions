statistics_ui <- function(id) {
  ns <- shiny::NS(id)

  choices <- c("Mean", "Variance", "Skewness", "Kurtosis")

  htmltools::tagList(
    shiny::selectInput(
      inputId = ns("statistics"),
      label = htmltools::tagList(
        "Distribution Statistics",
        popover_2(
          tag = shiny::actionLink(
            inputId = ns("type_info"),
            label = NULL,
            icon = shiny::icon("info-circle")
          ),
          title = "Distribution Statistics",
          content = htmltools::tagList(
            dist_func_row(
              htmltools::tags$b("Name"),
              htmltools::tags$b("Definition")
            ),
            dist_func_row(
              "Mean",
              "$$\\mu := E(X)$$"
            ),
            dist_func_row(
              "Variance",
              "$$\\sigma^2 := E\\bigg( (X - \\mu)^2 \\bigg)$$"
            ),
            dist_func_row(
              "Skewness",
              "$$E\\Bigg[ \\bigg( \\frac{X - \\mu}{\\sigma} \\bigg)^3 \\Bigg]$$"
            ),
            dist_func_row(
              "Kurtosis",
              "$$E\\Bigg[ \\bigg( \\frac{X - \\mu}{\\sigma} \\bigg)^4 \\Bigg]$$"
            )
          ),
          `data-template` = '
          <div class="popover wide-popover" role="tooltip">
            <div class="arrow"></div>
            <h3 class="popover-header"></h3>
            <div class="popover-body"></div>
          </div>'
        )
      ),
      choices = choices,
      selected = choices,
      multiple = TRUE
    ),
    DT::dataTableOutput(
      outputId = ns("statistics_tbl")
    )
  )
}

statistics_server <- function(id, .values, distributions_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$type_info, {
        # Typeset MathJax for popover
        session$sendCustomMessage("mathjax-typeset", TRUE)
      })

      statistics_r <- shiny::reactive({
        if (is.null(distributions_r())) return(tibble::tibble(
          Name = character(),
          Mean = numeric(),
          Variance = numeric(),
          Skewness = numeric(),
          Kurtosis = numeric()
        ))

        tbl <- distribution_helper$dists_to_statistics_tbl(distributions_r()) %>%
          dplyr::select(
            Name = name,
            Mean = mean,
            Variance = variance,
            Skewness = skewness,
            Kurtosis = kurtosis,
            color
          ) %>%
          dplyr::mutate(
            text_color = text_color(color),
            background_image = glue::glue("linear-gradient(to right, white, {bg} 80%, {bg})", bg = color)
          )
      })

      output$statistics_tbl <- DT::renderDataTable({
        if (!nrow(statistics_r())) return(DT::datatable(statistics_r()))

        DT::datatable(
          statistics_r(),
          rownames = statistics_r()$Name,
          options = list(
            columnDefs = list(
              list(
                targets = which(
                  !names(statistics_r()) %in% c(input$statistics, "Name")
                ) - 1,
                visible = FALSE
              )
            )
          )
        ) %>% DT::formatStyle(
          columns = which(names(statistics_r()) == "background_image"),
          target = "row",
          backgroundImage = DT::styleValue()
        ) %>% DT::formatStyle(
          columns = which(names(statistics_r()) == "text_color"),
          target = "row",
          color = DT::styleValue()
        ) %>% DT::formatStyle(
          columns = c(
            which(names(statistics_r()) %in% input$statistics)[1],
            which(names(statistics_r()) == "Name")
          ),
          color = DT::JS("'black'")
        )
      })
    }
  )
}
