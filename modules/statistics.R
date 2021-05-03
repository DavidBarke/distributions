statistics_ui <- function(id) {
  ns <- shiny::NS(id)

  choices <- c("Mean", "Variance", "Skewness", "Kurtosis")

  htmltools::tagList(
    shiny::selectInput(
      inputId = ns("statistics"),
      label = "Distribution Statistics",
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
                  !names(statistics_r()) %in% input$statistics
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
          columns = which(names(statistics_r()) %in% input$statistics)[1],
          color = DT::JS("'black'")
        )
      })
    }
  )
}
