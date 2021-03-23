library(shiny)
library(sortable)

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
                    htmltools::div(
                        id = "drag_from",
                        class = "outline panel-body",
                        purrr::map(LETTERS[1:5], ~ {
                            htmltools::div(
                                shiny::icon("home"),
                                .
                            )
                        })
                    )
                ),
                shiny::column(
                    width = 3,
                    htmltools::div(
                        id = "drag_to",
                        class = "outline panel-body"
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

}

shinyApp(ui = ui, server = server)
