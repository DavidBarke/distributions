library(shiny)
library(sortable)
library(magrittr)

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

distribution_helper <- Distribution$new()

ui <- htmltools::tagList(
    htmltools::includeCSS("www/css/styles.css"),
    bs4Dash::bs4DashPage(
        header = bs4Dash::bs4DashNavbar(
            title = bs4Dash::bs4DashBrand(
                title = "Distributions"
            )
        ),
        sidebar = bs4Dash::bs4DashSidebar(
            disable = TRUE
        ),
        body = bs4Dash::bs4DashBody(
            body_ui(
                id = "body"
            )
        )
    )
)

server <- function(input, output) {

    .values <- new.env()

    body_server(
        id = "body",
        .values = .values
    )
}

shinyApp(ui = ui, server = server)
