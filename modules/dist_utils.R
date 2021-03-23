distributions <- new.env()

distributions$choices <- c(
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
  "Negative Binomial" = "negative_binomial",
  "Normal" = "normal",
  "Pareto" = "pareto",
  "Poisson" = "poisson",
  "Student's t" = "student_t",
  "Uniform" = "uniform",
  "Weibull" = "weibull"
)

distributions$func <- list(
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
  negative_binomial = distributional::dist_negative_binomial,
  normal = distributional::dist_normal,
  pareto = distributional::dist_pareto,
  poisson = distributional::dist_poisson,
  student_t = distributional::dist_student_t,
  uniform = distributional::dist_uniform,
  weibull = distributional::dist_weibull
)

distributions$params <- list(
  bernoulli = c("p" = "prob"),
  beta = c(
    "Shape 1" = "shape1",
    "Shape 2" = "shape2"
  ),
  binomial = c(
    "n" = "size",
    "p" = "prob"
  ),
  cauchy = c(
    "&#x3BC" = "location",
    "&#x3C3" = "scale"
  ),
  chisq = c(
    "df" = "df"
  ),
  degenerate = c(
    "x" = "x"
  ),
  exponential = c(
    "&#x3BB" = "rate"
  ),
  f = c(
    "df1" = "df1",
    "df2" = "df2"
  ),
  gamma = c(
    "k" = "shape",
    "&#x3B2" = "rate"
  ),
  geometric = c(
    "p" = "prob"
  ),
  gumbel = c(
    "&#x3BC" = "alpha",
    "&#x3C3" = "scale"
  ),
  hypergeometric = c(
    "m" = "m",
    "n" = "n",
    "k" = "k"
  ),
  inverse_exponential = c(
    "&#x3BB" = "rate"
  ),
  inverse_gamma = c(
    "k" = "shape",
    "&#x3B2" = "scale"
  ),
  inverse_gaussian = c(
    "&#x3BC" = "mean",
    "&#x3BB" = "shape"
  ),
  logarithmic = c(
    "p" = "prob"
  ),
  logistic = c(
    "&#x3BC" = "location",
    "&#x3C3" = "scale"
  ),
  negative_binomial = c(
    "n" = "size",
    "p" = "prob"
  ),
  normal = c(
    "&#x3BC" = "mu",
    "&#x3C3" = "sigma"
  ),
  pareto = c(
    "x" = "shape",
    "&#x3B1" = "scale"
  ),
  poisson = c(
    "&#x3BB" = "lambda"
  ),
  student_t = c(
    "df" = "df",
    "&#x3BC" = "mu",
    "&#x3C3" = "sigma"
  ),
  uniform = c(
    "a" = "min",
    "b" = "max"
  ),
  weibull = c(
    "&#x3BB" = "shape",
    "k" = "scale"
  )
)


