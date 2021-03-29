var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".remove-icon");
  },

  initialize: function(el) {
    $(el).data("val", 0);
  },

  getValue: function(el) {
    var val = $(el).data("val");
    if (val === 0) return undefined;
    return val;
  },

  subscribe: function(el, callback) {
    $(el).on("click.remove-icon", function(e) {
      $(el).data("val", $(el).data("val") + 1);
      callback();
    });
  },

  unsubscribe: function(el, callback) {
    $(el).off(".remove-icon");
  }

});

Shiny.inputBindings.register(binding, "remove-icon");
