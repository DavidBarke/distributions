distribution_input <- function(inputId, value) {
  distribution_id <- distribution_helper$dist_to_id(value)
  distribution_name <- distribution_helper$dist_to_name(value)
  distribution_params <- distribution_helper$dist_to_params(value)
  distribution_param_names <- distribution_helper$get_param_names(distribution_id)

  param_badges <- purrr::map2(
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
      `data-distribution` = distribution_id,
      `data-params` = paste(distribution_params, collapse = ","),
      class = "flex distribution-input",
      htmltools::span(
        class = "badge badge-lg distribution-name",
        distribution_name
      ),
      htmltools::div(
        class = "flex distribution-params",
        param_badges
      )
    )
  )
}

update_distribution_input <- function(inputId,
                                      value,
                                      session = shiny::getDefaultReactiveDomain()
) {
  msg <- distribution_helper$dist_to_msg(value)

  msg <- c(
    action = "update",
    msg
  )

  session$sendInputMessage(inputId, msg)
}

remove_distribution_input <- function(inputId,
                                      session = shiny::getDefaultReactiveDomain()

) {
  msg <- list(
    action = "remove"
  )

  session$sendInputMessage(inputId, msg)
}
