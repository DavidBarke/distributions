var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".distribution-input");
  },

  initialize: function(el) {
    $(el).data("distribution-id", "normal");
    $(el).data("distribution-param-values", [0, 1]);
  },

  getValue: function(el) {
    return {
      distribution_id: $(el).data("distribution-id"),
      distribution_param_values: $(el).data("distribution-param-values")
    };
  },

  setValue: function(el, data) {
    // Update data values
    $(el).data("distribution-id", data.distribution_id);
    var paramValues = [].concat(data.distribution_param_values);
    $(el).data("distribution-param-values", paramValues);

    var distributionName = this.getDistributionName(data.distribution_id);
    $(el).find(".distribution-name").text(distributionName);

    var paramNames = this.getParamNames(data.distribution_id);
    $(el).find(".distribution-params").empty();
    var that = this;
    paramValues.forEach(function(paramVal, i) {
      console.log(paramVal);
      var paramUI = that.paramUI(
        paramNames[i],
        paramVal
      );
      console.log(paramUI);
      $(el).find(".distribution-params").append(paramUI);
    });


    $(el).trigger("update");
  },

  receiveMessage: function(el, data) {
    this.setValue(el, data);
  },

  subscribe: function(el, callback) {
    $(el).on("update.distribution-input", function(e) {
      callback();
    });

    $(el).on("click.distribution-input", function(e) {
      Shiny.setInputValue($(el).attr("id") + "_click", Math.random());
    });
  },

  unsubscribe: function(el, callback) {
    $(el).off(".distribution-input");
  },

  getDistributionName: function(distributionId) {
    distributions = {
      "bernoulli": "Bernoulli",
      "beta": "Beta",
      "binomial": "Binomial",
      "cauchy": "Cauchy",
      "chisq": "Chi-Square",
      "degenerate": "Degenerate",
      "exponential": "Exponential",
      "f": "F",
      "gamma": "Gamma",
      "geometric": "Geometric",
      "gumbel": "Gumbel",
      "hypergeometric": "Hypergeometric",
      "inverse_exponential": "Inverse Exponential",
      "inverse_gamma": "Inverse Gamma",
      "inverse_gaussian": "Inverse Gaussian",
      "logarithmic": "Logarithmic",
      "logistic": "Logistic",
      "negbin": "Negative Binomial",
      "normal": "Normal",
      "pareto": "Pareto",
      "poisson": "Poisson",
      "student_t": "Student's t",
      "uniform": "Uniform",
      "weibull": "Weibull"
    };

    return distributions[distributionId];
  },

  getParamNames: function(distributionId) {
    params = {
      "bernoulli": ["p"],
      "beta": ["Shape 1", "Shape 2"],
      "binomial": ["n", "p"],
      "cauchy": ["&#x3BC", "&#x3C3"],
      "chisq": ["df", "ncp"],
      "degenerate": ["x"],
      "exponential": ["&#x3BB"],
      "f": ["df1", "df2"],
      "gamma": ["k", "&#x3B2"],
      "geometric": ["p"],
      "gumbel": ["&#x3BC","&#x3C3"],
      "hypergeometric": ["m", "n", "k"],
      "inverse_exponential": ["&#x3BB"],
      "inverse_gamma": ["k", "&#x3B2"],
      "inverse_gaussian": ["&#x3BC", "&#x3BB"],
      "logarithmic": ["p"],
      "logistic": ["&#x3BC", "&#x3C3"],
      "negbin": ["n", "p"],
      "normal": ["&#x3BC", "&#x3C3"],
      "pareto": ["x", "&#x3B1"],
      "poisson": ["&#x3BB"],
      "student_t": ["df", "&#x3BC", "&#x3C3", "ncp"],
      "uniform": ["a", "b"],
      "weibull": ["&#x3BB", "k"]
    };

    return params[distributionId];
  },

  paramUI: function(name, value) {
    return $('<span class="badge distribution-param">'
    + '<span class="distribution-param-name">'
    + name
    + '</span>:<span class="distribution-param-value">'
    + value
    + '</span>'
    + '</span>');
  }

});

Shiny.inputBindings.register(binding, "distribution-input");
