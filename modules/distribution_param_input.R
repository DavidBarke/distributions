distribution_param_input <- function(inputId, name, value) {
  # paste would break rendering of greek letters
  text <- htmltools::tagList(name, ":", value)

  htmltools::span(
    class = "distribution-param",
    badge_input(inputId, text)
  )
}

dist_param_input <- function(inputId, distribution) {

  dist_id <- distribution_helper$dist_to_id(distribution)

  dist_param_vals <- distribution_helper$dist_to_params(distribution)

  dist_param_names <- distribution_helper$get_params(dist_id)
  dist_param_badges <- purrr::map2(
    names(dist_param_names), dist_param_vals, function(name, value) {
      badge(HTML(paste(name, ":", value)))
    }
  )

  htmltools::div(
    id = inputId,
    class = "distribution-params flex",
    `data-distribution` = dist_id,
    dist_param_badges
  )
}
