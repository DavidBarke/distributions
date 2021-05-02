statistics_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    DT::dataTableOutput(
      outputId = ns("statistics")
    )
  )
}

statistics_server <- function(id, .values, distributions_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      statistics_r <- shiny::reactive({
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
            text_color = text_color(color)
          )
      })

      output$statistics <- DT::renderDataTable({
        DT::datatable(
          statistics_r(),
          rownames = statistics_r()$Name,
          options = list(
            columnDefs = list(
              list(
                targets = c(0, 5, 6),
                visible = FALSE
              )
            )
          )
        ) %>% DT::formatStyle(
          columns = which(names(statistics_r()) == "color"),
          target = "row",
          backgroundColor = DT::styleValue()
        ) %>% DT::formatStyle(
          columns = which(names(statistics_r()) == "text_color"),
          target = "row",
          color = DT::styleValue()
        )
      })
    }
  )
}
