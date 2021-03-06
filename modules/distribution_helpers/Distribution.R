Distribution <- R6::R6Class(
  classname = "Distribution",
  public = list(
    initialize = function() {
      private$plotter <- DistributionPlotter$new(self)

      # Must be set here, otherwise unicode is not handled properly
      private$func_choices <- c("d", "p", "q", "s", "h", "ch")
      names(private$func_choices) <- c(
        "Probability Density Function f(x)",
        "Cumulative Distribution Function F(x)",
        "Quantile Function Q(p)",
        "Survival Function S(x)",
        "Hazard Function \u03BB(x)",
        "Cumulative Hazard Function \u039B(x)"
      )

      # Must be set here, otherwise private is not available in functions
      private$distribution_funcs <- list(
        # Probability density function
        d = list(
          default = stats::density
        ),
        # Cumulative distribution function
        p = list(
          default = distributional::cdf
        ),
        q = list(
          default = stats::quantile
        ),
        # Survival function
        s = list(
          default = function(d, x) {
            1 - distributional::cdf(d, x)
          }
        ),
        # Hazard function
        h = list(
          default = function(d, x) {
            stats::density(d, x) / private$distribution_funcs$s$default(d, x)
          },
          exponential = function(d, x) rep(d$rate, times = length(x))
        ),
        # Cumulative hazard function
        ch = list(
          default = function(d, x) {
            -log(private$distribution_funcs$s$default(d, x))
          },
          exponential = function(d, x) d$rate * x
        )
      )
    },

    dist_to_id = function(distribution) {
      dist_cls <- class(distribution)[1]

      # Extracts everything after the first _
      stringr::str_extract(dist_cls, "(?<=_).*$")
    },

    dist_to_msg = function(distribution) {
      list(
        distribution_id = self$dist_to_id(distribution),
        distribution_param_values = unname(self$dist_to_params(distribution))
      )
    },

    dist_to_name = function(distribution) {
      id <- self$dist_to_id(distribution)
      names(private$choices[private$choices == id])
    },

    dist_to_params = function(distribution) {
      distribution["color"] <- NULL
      x <- as.numeric(distribution)
      names(x) <- names(distribution)
      x
    },

    dists_to_statistics_tbl = function(distributions) {
      skewness.dist_student_t <- function(x, ...) NA_real_
      kurtosis.dist_student_t <- function(x, ...) NA_real_

      tbl <- tibble::tibble(distribution = distributions) %>%
        dplyr::mutate(
          name = distribution %>% purrr::map(self$dist_to_trace_name),
          mean = mean(distribution),
          variance = distributional::variance(distribution),
          skewness = distributional::skewness(distribution),
          kurtosis = distributional::kurtosis(distribution),
          color = distribution %>% purrr::map("color"),
          across(mean:kurtosis, signif),
          across(mean:kurtosis, replace_na)
        )
    },

    dist_to_support = function(distribution) {
      id <- self$dist_to_id(distribution)
      support_type <- private$support_type[[id]]
      private$support_fun[[support_type]](distribution)
    },

    dist_to_trace_name = function(distribution) {
      id <- self$dist_to_id(distribution)

      name <- private$short_name[id]

      param_names <- self$get_param_names(id)
      param_values <- self$dist_to_params(distribution)

      paste0(
        name, "(", paste0(param_names, ": ", param_values, collapse = ", "), ")"
      )
    },

    get_choices = function() {
      private$choices
    },

    get_distribution_func = function(id,
                                     type = c("d", "p", "q", "s", "h", "ch")
    ) {
      type <- match.arg(type)

      funcs <- private$distribution_funcs[[type]]

      funcs[[id]] %||% funcs$default
    },

    get_func = function(id) {
      private$funcs[[id]]
    },

    get_func_choices = function() {
      private$func_choices
    },

    get_params = function(id) {
      private$params[[id]]
    },

    get_param_ids = function(id) {
      names(private$params[[id]])
    },

    get_param_names = function(id) {
      unname(as.character(private$params[[id]]))
    },

    get_random_distribution = function() {
      func <- sample(private$funcs, 1)

      param_ids <- self$get_param_ids(names(func))
      param_vals <- purrr::map_dbl(param_ids, self$get_random_param_value)
      if (names(func) == "uniform") param_vals <- sort(param_vals)
      names(param_vals) <- param_ids

      do.call(
        func[[1]],
        as.list(param_vals)
      )[[1]]
    },

    get_random_param_value = function(param_id) {
      restrictions <- private$param_restrictions[[param_id]]

      if (!length(restrictions)) restrictions <- "no"

      switch (
        restrictions,
        "no" = round(runif(1, -5, 5), digits = 2),
        "nn" =,
        "pos" =,
        "pos_int" = sample(1:20, 1),
        "prob" = sample(seq(0, 1, length.out = 11), 1)
      )
    },

    # returns logical
    is_valid_param_value = function(value, param_id, distribution_id) {
      # distribution_id is currently unused

      if (is.na(value)) return(FALSE)

      restrictions <- c(
        private$param_restrictions[[param_id]]
      )

      is_valid <- purrr::map_lgl(private$check_funs[restrictions], ~ .(value))

      all(is_valid)
    },

    is_discrete = function(distribution) {
      self$dist_to_id(distribution) %in% private$discrete
    },

    plot_dists = function(distributions,
                          type = c("d", "p", "q", "s", "h", "ch"),
                          limits = c(0, 10),
                          n = 100
    ) {
      private$plotter$plot(
        distributions = distributions,
        type = type,
        limits = limits,
        n = n
      )
    },

    # calls shiny::validate for UI
    validate_param_value = function(value, param_id, distribution_id) {
      # distribution_id is currently unused

      restrictions <- c(
        "not_na",
        private$param_restrictions[[param_id]]
      )

      # Evaluate need functions
      needs <- purrr::map(private$need_funs[restrictions], ~ .(value))

      do.call(
        shiny::validate,
        needs
      )
    }
  ),
  private = list(
    plotter = NULL, # DistributionPlotter

    choices = c(
      "Bernoulli" = "bernoulli",
      "Beta" = "beta",
      "Binomial" = "binomial",
      "Cauchy" = "cauchy",
      "Chi-Square" = "chisq",
      "Degenerate" = "degenerate",
      "Exponential" = "exponential",
      "F" = "f",
      "Gamma" = "gamma",
      "Geometric" = "geometric",
      "Gumbel" = "gumbel",
      "Hypergeometric" = "hypergeometric",
      "Logistic" = "logistic",
      "Negative Binomial" = "negbin",
      "Normal" = "normal",
      "Poisson" = "poisson",
      "Student's t" = "student_t",
      "Uniform" = "uniform",
      "Weibull" = "weibull"
    ),

    short_name = c(
      "bernoulli" = "Brnl",
      "beta" = "Beta",
      "binomial" = "B",
      "cauchy" = "Cauchy",
      "chisq" = "\u03C7\u00B2",
      "degenerate" = "Deg",
      "exponential" = "Exp",
      "f" = "F",
      "gamma" = "\u0393",
      "geometric" = "Geom",
      "gumbel" = "Gumb",
      "hypergeometric" = "Hyp",
      "logistic" = "Logi",
      "negbin" = "NB",
      "normal" = "N",
      "poisson" = "P",
      "student_t" = "t",
      "uniform" = "U",
      "weibull" = "W"
    ),

    func_choices = NULL,

    discrete = c(
      "bernoulli",
      "binomial",
      "geometric",
      "hypergeometric",
      "negbin",
      "poisson"
    ),

    support_type = list(
      bernoulli = "zeroone",
      beta = "zeroone",
      binomial = "binomial",
      cauchy = "all",
      chisq = "nn",
      degenerate = "degenerate",
      exponential = "nn",
      f = "nn",
      gamma = "nn",
      geometric = "nn",
      gumbel = "all",
      hypergeometric = "hypergeometric",
      logarithmic = "oneinf",
      logistic = "all",
      negbin = "nn",
      normal = "all",
      poisson = "nn",
      student_t = "all",
      uniform = "uniform",
      weibull = "nn"
    ),

    support_fun = list(
      all = function(distribution) c(-Inf, Inf),
      binomial = function(distribution) c(0, distribution$n),
      degenerate = function(distribution) c(distribution$x, distribution$x),
      hypergeometric = function(distribution) {
        c(
          max(0, distribution$k - distribution$n),
          min(distribution$k, distribution$m)
        )
      },
      nn = function(distribution) c(0, Inf),
      oneinf = function(distribution) c(1, Inf),
      uniform = function(distribution) {
        c(distribution$l, distribution$u)
      },
      zeroone = function(distribution) c(0, 1)
    ),

    funcs = list(
      bernoulli = distributional::dist_bernoulli,
      beta = distributional::dist_beta,
      binomial = distributional::dist_binomial,
      cauchy = distributional::dist_cauchy,
      chisq = distributional::dist_chisq,
      degenerate = distributional::dist_degenerate,
      exponential = distributional::dist_exponential,
      f = distributional::dist_f,
      gamma = distributional::dist_gamma,
      geometric = distributional::dist_geometric,
      gumbel = distributional::dist_gumbel,
      hypergeometric = distributional::dist_hypergeometric,
      logistic = distributional::dist_logistic,
      negbin = distributional::dist_negative_binomial,
      normal = distributional::dist_normal,
      poisson = distributional::dist_poisson,
      student_t = distributional::dist_student_t,
      uniform = distributional::dist_uniform,
      weibull = distributional::dist_weibull
    ),

    distribution_funcs = NULL,

    params = list(
      bernoulli = list("prob" = "p"),
      beta = list(
        "shape1" = "Shape 1",
        "shape2" = "Shape 2"
      ),
      binomial = list(
        "size" = "n",
        "prob" = "p"
      ),
      cauchy = list(
        "location" = "\u03BC",
        "scale" = "\u03C3"
      ),
      chisq = list(
        "df" = "df",
        "ncp" = "ncp"
      ),
      degenerate = list(
        "x" = "x"
      ),
      exponential = list(
        "rate" = "\u03BB"
      ),
      f = list(
        "df1" = "df1",
        "df2" = "df2",
        "ncp" = "ncp"
      ),
      gamma = list(
        "shape" = "k",
        "rate" = "\u03B2"
      ),
      geometric = list(
        "prob" = "p"
      ),
      gumbel = list(
        "alpha" = "\u03BC",
        "scale" = "\u03C3"
      ),
      hypergeometric = list(
        "m" = "m",
        "n" = "n",
        "k" = "k"
      ),
      logistic = list(
        "location" = "\u03BC",
        "scale" = "\u03C3"
      ),
      negbin = list(
        "size" = "n",
        "prob" = "p"
      ),
      normal = list(
        "mu" = "\u03BC",
        "sigma" = "\u03C3"
      ),
      poisson = list(
        "lambda" = "\u03BB"
      ),
      student_t = list(
        "df" = "df",
        "mu" = "\u03BC",
        "sigma" = "\u03C3",
        "ncp" = "ncp"
      ),
      uniform = list(
        "min" = "a",
        "max" = "b"
      ),
      weibull = list(
        "shape" = "\u03BB",
        "scale" = "k"
      )
    ),

    param_restrictions = list(
      alpha = character(),
      df = "nn",
      df1 = "nn",
      df2 = "nn",
      lambda = "nn",
      location = character(),
      k = "pos_int",
      m = "pos_int",
      max = character(),
      mean = "pos",
      min = character(),
      mu = character(),
      n = "pos_int",
      ncp = "nn",
      prob = "prob",
      rate = "pos",
      scale = "pos",
      shape = "nn",
      shape1 = "nn",
      shape2 = "nn",
      sigma = "pos",
      size = "pos_int",
      x = character()
    ),

    check_funs = list(
      not_na = purrr::compose(`!`, is.na),
      nn = function(x) x >= 0,
      pos = function(x) x > 0,
      pos_int = function(x) x > 0 && as.integer(x) == x,
      prob = function(x) x >= 0 && x <= 1
    ),

    need_funs = list(
      not_na = function(value) {
        shiny::need(!is.na(value), "Parameter must be a number.")
      },
      nn = function(value) {
        shiny::need(value >= 0, "Parameter must be non-negative.")
      },
      pos = function(value) {
        shiny::need(value > 0, "Parameter must be positive.")
      },
      pos_int = function(value, param_id, distribution_id) {
        shiny::need(
          value > 0 && as.integer(value) == value,
          "Parameter must be a positive integer."
        )
      },
      prob = function(value) {
        shiny::need(
          value >= 0 && value <= 1,
          "Parameter must be between 0 and 1."
        )
      }
    )
  )
)
