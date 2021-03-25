distribution_input <- function(inputId, value) {
  distribution_id <- distribution_helper$dist_to_id(value)
  distribution_name <- distribution_helper$id_to_name(distribution_id)
  distribution_params <- distribution_helper$dist_to_params(value)
  distribution_param_names <- distribution_helper$get_params(distribution_id)

  param_bages <- purrr::map2(
    distribution_param_names, distribution_params,
    function(name, value) {
      htmltools::span(
        class = "badge distribution-param",
        htmltools::span(
          class = "distribution-param-name",
          htmltools::HTML(name)
        ),
        ":",
        htmltools::span(
          class = "distribution-param-value",
          value
        )
      )
    }
  )

  htmltools::tagList(
    htmltools::singleton(
      htmltools::tags$script(
        src = "js/distribution-input.js"
      )
    ),
    htmltools::div(
      id = inputId,
      class = "flex distribution-input",
      htmltools::span(
        class = "badge distribution-name",
        distribution_name
      ),
      htmltools::div(
        class = "flex distribution-params",
        param_bages
      )
    )
  )
}

update_distribution_input <- function(inputId,
                                      value,
                                      session = shiny::getDefaultReactiveDomain()
) {
  msg <- distribution_helper$dist_to_msg(value)

  session$sendInputMessage(inputId, msg)
}
