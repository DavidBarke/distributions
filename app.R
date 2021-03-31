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

distribution_helper <- Distribution$new()

ui <- htmltools::tagList(
    htmltools::includeScript("www/js/distribution-input.js"),
    htmltools::includeScript("www/js/color-input.js"),
    htmltools::includeScript("www/js/remove-icon.js"),
    htmltools::includeCSS("www/css/styles.css"),
    bs4Dash::bs4DashPage(
        header = bs4Dash::bs4DashNavbar(
            title = bs4Dash::bs4DashBrand(
                title = "Distributions"
            ),
            status = "teal"
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
