update_sortable_factory <- function(selector,
                                    inputId,
                                    session = shiny::getDefaultReactiveDomain()
) {
  function() {
    session$sendCustomMessage(
      type = "distributions.update-sortable",
      message = list(
        selector = selector,
        # add type identifier for input handler
        inputId = paste0(inputId, ":sortablejs.rank_list")
      )
    )
  }
}
