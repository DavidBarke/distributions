Distribution <- R6::R6Class(
  classname = "Distribution",
  public = list(
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
      self$id_to_name(
        self$dist_to_id(
          distribution
        )
      )
    },

    dist_to_params = function(distribution) {
      x <- as.numeric(distribution)
      names(x) <- names(distribution)
      x
    },

    dist_to_trace = function(distribution,
                             type = c("d", "p", "q"),
                             limits = c(0, 10)
    ) {
      type <- match.arg(type)

      discrete <- self$is_discrete(distribution)

      # Get S3 generic
      to_trace_fun <- if (discrete) {
        dist_to_trace_discrete
      } else {
        dist_to_trace_continuous
      }

      # Dispatch
      class(distribution) <- c(class(distribution), type)

      to_trace_fun(
        distribution = distribution,
        type = type,
        limits = sort(limits)
      )
    },

    get_choices = function() {
      private$choices
    },

    get_func = function(id) {
      private$funcs[[id]]
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

    id_to_name = function(id) {
      names(private$choices[private$choices == id])
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
      "Inverse Exponential" = "inverse_exponential",
      "Inverse Gamma" = "inverse_gamma",
      "Inverse Gaussian" = "inverse_gaussian",
      "Logarithmic" = "logarithmic",
      "Logistic" = "logistic",
      "Negative Binomial" = "negbin",
      "Normal" = "normal",
      "Pareto" = "pareto",
      "Poisson" = "poisson",
      "Student's t" = "student_t",
      "Uniform" = "uniform",
      "Weibull" = "weibull"
    ),

    discrete = c(
      "bernoulli",
      "binomial",
      "geometric",
      "hypergeometric",
      "logarithmic",
      "negbin",
      "poisson"
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
      inverse_exponential = distributional::dist_inverse_exponential,
      inverse_gamma = distributional::dist_inverse_gamma,
      inverse_gaussian = distributional::dist_inverse_gaussian,
      logarithmic = distributional::dist_logarithmic,
      logistic = distributional::dist_logistic,
      negbin = distributional::dist_negative_binomial,
      normal = distributional::dist_normal,
      pareto = distributional::dist_pareto,
      poisson = distributional::dist_poisson,
      student_t = distributional::dist_student_t,
      uniform = distributional::dist_uniform,
      weibull = distributional::dist_weibull
    ),

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
        "location" = "&#x3BC",
        "scale" = "&#x3C3"
      ),
      chisq = list(
        "df" = "df",
        "ncp" = "ncp"
      ),
      degenerate = list(
        "x" = "x"
      ),
      exponential = list(
        "rate" = "&#x3BB"
      ),
      f = list(
        "df1" = "df1",
        "df2" = "df2",
        "ncp" = "ncp"
      ),
      gamma = list(
        "shape" = "k",
        "rate" = "&#x3B2"
      ),
      geometric = list(
        "prob" = "p"
      ),
      gumbel = list(
        "alpha" = "&#x3BC",
        "scale" = "&#x3C3"
      ),
      hypergeometric = list(
        "m" = "m",
        "n" = "n",
        "k" = "k"
      ),
      inverse_exponential = list(
        "rate" = "&#x3BB"
      ),
      inverse_gamma = list(
        "shape" = "k",
        "scale" = "&#x3B2"
      ),
      inverse_gaussian = list(
        "mean" = "&#x3BC",
        "shape" = "&#x3BB"
      ),
      logarithmic = list(
        "prob" = "p"
      ),
      logistic = list(
        "location" = "&#x3BC",
        "scale" = "&#x3C3"
      ),
      negbin = list(
        "size" = "n",
        "prob" = "p"
      ),
      normal = list(
        "mu" = "&#x3BC",
        "sigma" = "&#x3C3"
      ),
      pareto = list(
        "shape" = "x",
        "scale" = "&#x3B1"
      ),
      poisson = list(
        "lambda" = "&#x3BB"
      ),
      student_t = list(
        "df" = "df",
        "mu" = "&#x3BC",
        "sigma" = "&#x3C3",
        "ncp" = "ncp"
      ),
      uniform = list(
        "min" = "a",
        "max" = "b"
      ),
      weibull = list(
        "shape" = "&#x3BB",
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
