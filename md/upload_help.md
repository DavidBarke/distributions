### Overview

You may upload your own set of distributions by following these steps:

-   Install the
    [{distributional}](https://github.com/mitchelloharawild/distributional)
    package
-   Create a data structure of distributions (see below) in your R
    session
-   Store this data structure in the `.rds` file format
-   Upload the `.rds` file inside this application

### Data Structures

    library(distributional)
    library(readr)

#### Vector of distributions

    x <- dist_normal(mu = 0, sigma = 1:5)
    write_rds(x, "dist_vector.rds")

#### List of distributions

If you need the flexibilty to differentiate between active and inactive
distributions, you may store a list of distributions.

    x <- list(
      active = dist_normal(mu = 0, sigma = 1:5),
      inactive = dist_normal(mu = -2:2, sigma = 1)
    )
    write_rds(x, "dist_list.rds")

### Add color information

If you want to control the color of a distribution, you can add a color
field to a distribution on the same level as the distribution
parameters.

    x <- dist_normal(mu = 0, sigma = 1:5)

    # Second distribution shall be red
    x[[2]]$color <- "#ff0000"

    # Apply color scale
    library(scales)
    color_scale <- col_numeric(palette(), 1:5)
    for (i in 1:5) x[[i]]$color <- color_scale(i)
