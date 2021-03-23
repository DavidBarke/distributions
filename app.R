library(shiny)
library(sortable)

source("init/source_directory.R")
source_directory("modules")

sass::sass(
    sass::sass_file("www/scss/styles.scss"),
    output = "www/css/styles.css",
    options = sass::sass_options(
        output_style = "compressed"
    ),
    cache = FALSE
)

ui <- htmltools::tagList(
    htmltools::includeCSS("www/css/styles.css"),
    bs4Dash::bs4DashPage(
        header = bs4Dash::bs4DashNavbar(
            title = "Distributor"
        ),
        sidebar = bs4Dash::bs4DashSidebar(
            disable = TRUE
        ),
        body = bs4Dash::bs4DashBody(
            shiny::fluidRow(
                shiny::column(
                    width = 3,
                    drop_zone(
                        id = "drag_from",
                        label = "Inactive Distributions",
                        purrr::map(1:10, ~ {
                            distribution_box_ui(
                                id = "test_box"
                            )
                        })
                    )
                ),
                shiny::column(
                    width = 3,
                    drop_zone(
                        id = "drag_to",
                        label = "Active Distributions"
                    )
                ),
                shiny::column(
                    width = 6
                )
            )
        )
    ),
    sortable::sortable_js(
        css_id = "drag_from",
        options = sortable::sortable_options(
            group = list(
                name = "group",
                pull = TRUE,
                put = TRUE
            )
        )
    ),
    sortable::sortable_js(
        css_id = "drag_to",
        options = sortable::sortable_options(
            group = list(
                name = "group",
                pull = TRUE,
                put = TRUE
            )
        )
    )
)

server <- function(input, output) {

    .values <- new.env()

    distribution_box_server(
        id = "test_box",
        .values = .values
    )

}

shinyApp(ui = ui, server = server)
