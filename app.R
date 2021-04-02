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
    htmltools::includeScript("www/js/bin-drop-zone.js"),
    htmltools::includeScript("www/js/update-sortable-handler.js"),
    htmltools::includeCSS("www/css/styles.css"),
    bs4Dash::bs4DashPage(
        header = bs4Dash::bs4DashNavbar(
            title = bs4Dash::bs4DashBrand(
                title = "Distributions"
            ),
            status = "primary",
            rightUi = navbar_right_ui(
                id = "navbar_right"
            )
        ),
        sidebar = bs4Dash::bs4DashSidebar(
            disable = TRUE
        ),
        body = bs4Dash::bs4DashBody(
            body_ui(
                id = "body"
            )
        ),
        footer = bs4Dash::bs4DashFooter(
            left = bin_drop_zone_ui(
                id = "bin_drop_zone"
            ),
            fixed = TRUE
        ),
        freshTheme = fresh::create_theme(
            fresh::bs4dash_vars(
                link_color = "white",
                main_footer_padding = 0
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

    navbar_right_server(
        id = "navbar_right",
        .values = .values
    )
}

shinyApp(ui = ui, server = server)
