var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".color-input").find("input");
  },

  getValue: function(el) {
    return $(el).val();
  },

  subscribe: function(el, callback) {
    $(el).on("change.color-input", function(e) {
      console.log("callback");
      callback();
    });
  },

  unsubscribe: function(el, callback) {
    $(el).off(".color-input");
  }

});

Shiny.inputBindings.register(binding, "color-input");
