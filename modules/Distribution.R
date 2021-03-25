Distribution <- R6::R6Class(
  classname = "Distribution",
  public = list(
    dist_to_id = function(distribution) {
      dist_cls <- class(distribution[[1]])[1]

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
      x <- as.numeric(distribution[[1]])
      names(x) <- names(distribution[[1]])
      x
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

    id_to_name = function(id) {
      names(private$choices[private$choices == id])
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
      bernoulli = list("p" = "prob"),
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
        "df2" = "df2"
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
    )
  )
)
