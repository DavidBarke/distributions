var binding = new Shiny.InputBinding();

$.extend(binding, {

  find: function(scope) {
    return $(scope).find(".color-input").find("input");
  },

  initialize: function(el) {
    var bg = $(el).data("bg");
    if (typeof bg !== "undefined") {
      $(el).closest(bg).css(
        "background-image",
        "linear-gradient(to right, white, " +
        $(el).val() + " 80%, " + $(el).val() +
        ")"
      );
      //$(el).closest(bg).css("background-color", $(el).val());
    }
  },

  getValue: function(el) {
    return $(el).val();
  },

  subscribe: function(el, callback) {
    $(el).on("change.color-input", function(e) {
      callback();
    });

    $(el).on("input.color-input", function(e) {
      var bg = $(el).data("bg");
      if (typeof bg !== "undefined") {
        $(el).closest(bg).css("background-image", "linear-gradient(to right, white, " + $(el).val() + ")");
        //$(el).closest(bg).css("background-color", $(el).val());
      }
    });
  },

  unsubscribe: function(el, callback) {
    $(el).off(".color-input");
  }

});

Shiny.inputBindings.register(binding, "color-input");
