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

size <- 100

color_scale <- scales::col_numeric(palette(), c(1, size))

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
                        purrr::map2(seq_len(size), color_scale(seq_len(size)), ~ {
                            distribution_box_ui(
                                id = paste0("test_box_", .x),
                                color = .y
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

    purrr::walk(seq_len(size), ~ {
        distribution_box_server(
            id = paste0("test_box_", .),
            .values = .values,
            distribution_modifier_return = distribution_modifier_return,
            distribution_modifier_ui_proxy = distribution_modifier_ui_proxy
        )
    })

    distribution_modifier_return <- distribution_modifier_server(
        id = "distribution_modifier",
        .values = .values
    )

}

distribution_modifier_ui_proxy <- function(current_distribution) {
    distribution_modifier_ui(
        id = "distribution_modifier",
        current_distribution = current_distribution
    )
}

shinyApp(ui = ui, server = server)
