var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".badge-input");
  },

  initialize: function(el) {
    $(el).attr("data-count", 0);
  },

  getValue: function(el) {
    return parseInt($(el).attr("data-count"));
  },

  subscribe: function(el, callback) {
    $(el).on("click.badge-input", function(e) {
      var count = parseInt($(this).attr("data-count"));
      $(this).attr("data-count", count + 1);

      callback();
    });
  },

  unsubscribe: function(el, callback) {
    $(el).off(".badge-input");
  }

});

Shiny.inputBindings.register(binding, "badge");



var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".badge-input > div");
  },

  initialize: function(el) {
    $(el).attr("data-count", 0);
  },

  getValue: function(el) {
    return $(el).text();
  },

  setValue: function(el, data) {
    $(el).text(data.text);
    $(el).trigger("text");
  },

  receiveMessage: function(el, data) {
    this.setValue(el, data);
  },

  subscribe: function(el, callback) {
    $(el).on("text.badge-input", function(e) {
      callback();
    });
  },

  unsubscribe: function(el, callback) {
    $(el).off(".badge-input");
  }

});

Shiny.inputBindings.register(binding, "badge-text");
